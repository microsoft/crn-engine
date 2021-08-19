module Microsoft.Research.ClassicDSDServer.DSD

open System.Net.WebSockets
open Messages
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI
open Microsoft.Research
open Microsoft.Research.CRNEngineServerLib.Serialisation
open Microsoft.Research.CRNEngineServerLib.Messages
open Microsoft.Research.DNA

let mutable currentCode:DNA.JSAPI.ParseObject option = None
let mutable currentResult:DNA.JSAPI.ParseResult option = None

let runParseAndCache code oldSyntax =
    currentCode <- None
    currentResult <- None
    currentResult <- (if oldSyntax then DNA.JSAPI.user_parse_oldsyntax else DNA.JSAPI.user_parse) code |> Some
    currentCode <- Some code
    currentResult.Value

let processCompileDSDRequest code oldSyntax (webSocket:WebSocket) = 
    let sendObject o = sendObject webSocket o
    try
      let result = runParseAndCache code oldSyntax
      let isJIT = DNA.JSAPI.is_jit result.bundle
      let ig = if isJIT then result.unexpanded else { task = result.unexpanded.task; nodes = Map.empty |> Map.add "" (DNA.JSAPI.user_expand result.bundle); edges = Map.empty; expanded = false }
      let ecode = JSAPI.user_get_export ig "" "code"
      let code = match ecode.content with Some [|code|] -> code | _ -> failwith ""
      let options = Dsd.get_options result.bundle
      sendObject { Response_IsJIT.mtype = "dsd.isjit"
                   Response_IsJIT.isJIT = false }
      sendObject { Response_ParseResult.mtype = "dsd.parseresult"
                   Response_ParseResult.result = { model = ig; settings = options; code = code } }
    with e -> match e with
              | :? Parser.Exception as e -> sendObject { mtype = "error"
                                                         error = { message = e.Message; positions = Some e.Errors } }
              | _ -> sendObject { mtype = "error"
                                  error = { message = e.Message; positions = None } }

let isCurrent (code:DNA.JSAPI.ParseObject) = currentCode.IsSome && code.code = currentCode.Value.code && code.specificities = currentCode.Value.specificities && code.toeholds = currentCode.Value.toeholds

let processExpandDSDRequest code oldSyntax (userIG:GuiIG) (options:Options.t) (webSocket:WebSocket) = 
    let sendObject o = sendObject webSocket o
    try
      let currResult = if isCurrent code = false || currentResult.IsNone then runParseAndCache code oldSyntax else currentResult.Value
      // Replace the DSD settings with the ones provided by the front-end, except for the rules program.
      let settings = { options with rulesProgram = currResult.settings.rulesProgram }
      let result = { currResult with bundle = Dsd.set_options currResult.bundle settings; settings = settings }
      currentResult <- Some result
      let isJIT = DNA.JSAPI.is_jit result.bundle
      let ig = if isJIT then result.unexpanded else { task = userIG.task; nodes = Map.empty |> Map.add "" (DNA.JSAPI.user_expand result.bundle); edges = Map.empty ; expanded = false}
      let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
      // Replace the settings with the ones from the original model. This won't work with multi-CRN models.
      // If the user-modified plots are unmodified or empty, then I should use the plots from the result. Otherwise, I should use the user-modified plots.
      let userModel = userIG.nodes |> Map.toSeq |> Seq.head |> snd
      let unexpandedModel = result.unexpanded.nodes |> Map.toSeq |> Seq.head |> snd
      let userPlots = userModel.top.settings.simulation.plots
      let unexpandedPlots = unexpandedModel.top.settings.simulation.plots
      let modified = (userPlots <> unexpandedPlots) && userPlots <> []
      let map_simulations o_sim sim = { o_sim with Simulation_settings.plots = if modified then o_sim.plots else sim.plots }
      let model = { model with
                     top = { model.top with
                              settings = { userModel.top.settings with
                                            simulation = map_simulations userModel.top.settings.simulation model.top.settings.simulation
                                            simulations = List.mapi (fun (i:int) sim -> map_simulations userModel.top.settings.simulations.[i] sim) model.top.settings.simulations } } }
      let ig = { task = userIG.task; nodes = Map.empty |> Map.add "" model; edges = Map.empty; expanded = false }
      sendObject { Response_IsJIT.mtype = "dsd.isjit"
                   Response_IsJIT.isJIT = isJIT }
      let ecode = JSAPI.user_get_export ig "" "code"
      let code = match ecode.content with Some [|code|] -> code | _ -> failwith ""
      let options = Dsd.get_options result.bundle
      sendObject { Response_ParseResult.mtype = "dsd.parseresult"
                   Response_ParseResult.result = { model = ig; settings = options; code = code } }
    with e -> match e with
              | :? Parser.Exception as e -> sendObject { mtype = "error"
                                                         error = { message = e.Message; positions = Some e.Errors } }
              | _ -> sendObject { mtype = "error"
                                  error = { message = e.Message; positions = None } }

let processParseDSDRequest parseObject oldSyntax (webSocket:WebSocket) = 
    let sendObject o = sendObject webSocket o
    try
      let result = runParseAndCache parseObject oldSyntax
      let ig = result.unexpanded
      let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
      let options = Dsd.get_options result.bundle
      let ecode = JSAPI.user_get_export ig "" "code"
      let code = match ecode.content with Some [|code|] -> code | _ -> failwith ""
      sendObject { Response_IsJIT.mtype = "dsd.isjit"
                   Response_IsJIT.isJIT = DNA.JSAPI.is_jit currentResult.Value.bundle }
      sendObject { Response_ParseResult.mtype = "dsd.parseresult"
                   Response_ParseResult.result = { model = ig; settings = options; code = code } }
    with e -> match e with
              | :? Parser.Exception as e -> sendObject { mtype = "error"
                                                         error = { message = e.Message; positions = Some e.Errors } }
              | _ -> sendObject { mtype = "error"
                                  error = { message = e.Message; positions = None } }
