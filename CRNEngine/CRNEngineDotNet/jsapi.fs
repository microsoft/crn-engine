// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScriptExport>]
module Microsoft.Research.CRNEngine.JSAPI
open Microsoft.Research.CRNEngine.InferenceSiteGraph
open Microsoft.Research.CRNEngine.Graph
open Microsoft.Research.CRNEngine
open Microsoft.Research.Filzbach

type export_def =
    {
      ///MIME type. The GUI may use this to decide how to present this export. Supported: image/svg+xml, text/plain, text/html, application/x-tex.
      content_type : string
      ///An identifier for this export. This can be used to request a specific export. The GUI may rely on this to identify a specific export for special purposes.
      id : string
      ///A string that can be user as a header for this export.
      display_name : string
      ///A node identifier, in case this export refers to a specific node. If None, the export refers to the whole graph.
      node_id : string option
      ///An instance identifier, in case the model is producing multiple exports for this export ID. If None, the export refers to the whole model.
      instance : string option
      ///The export content. If None, the export is available for lazy generation.
      content : string[] option
      ///The export content, when saved to disk. If None, is assumed to be the same as the content. If blank, saving is not allowed.
      save_content : string option
    }

type SynthesisDispersionResult = 
    {
        markersX: float[];
        markersY: float[];
        plotX: float[];
        plotY: float[];
        xMin: float;
        xMax: float;
        yMin: float;
        yMax: float;
    }

type SynthesisValue =
    {
        value: float;
        lowerBound: float option;
        upperBound: float option;
    }

type SynthesisEquations =
    {
        rateEquations: string;
        jacobian: string;
        diffusion: string;
        csts: string;
    }

type SynthesisResult =
    {
        message: string;
        values: Map<string,SynthesisValue>;
        equations: SynthesisEquations;
        dispersion: SynthesisDispersionResult option;
        jacobian: Graph option;
        crn: Gui option;
        code: string option;
    }

type BistabilityPlot =
    {
        speciesX: string;
        speciesY: string;
        state1x: float;
        state1y: float;
        state2x: float;
        state2y: float;
        initialsX: float list;
        initialsY: float list;
        state1simsX: float list list;
        state1simsY: float list list;
        state2simsX: float list list;
        state2simsY: float list list;
    }

type ValueType = 
    | [<WebSharper.Constant "Float">] Float
    | [<WebSharper.Constant "MeanStdev">] MeanStdev
    | [<WebSharper.Constant "MeanStdevProbabilities">] MeanStdevProbabilities
    | [<WebSharper.Constant "Spatial1D">] Spatial1D
    | [<WebSharper.Constant "Spatial2D">] Spatial2D
    | [<WebSharper.Constant "MeanStdevTable">] MeanStdevTable

let valueTypeCaseString = 
    function 
    | Float -> "Float"
    | MeanStdev -> "MeanStdev"
    | MeanStdevProbabilities -> "MeanStdevProbabilities"
    | Spatial1D -> "Spatial1D"
    | Spatial2D -> "Spatial2D"
    | MeanStdevTable -> "MeanStdevTable"

type sim_run = 
    { value_type : ValueType
      instances : Instance<string> list }

let parse_code (s : string) : IGraph =
    let inferencegraph = match Parser.from_string_first_error_with_msg InferenceSiteGraph.parse s with 
                         | Choice1Of2 m -> m
                         | Choice2Of2 err -> raise (Parser.Exception(err.text, [|err|]))
    // Expand and lift. Is this necessary?
    //let inferencegraph = InferenceSiteGraph.expandAndLift inferencegraph
    let initialise_model (model:Model) : Model = { top = model.top.initialise(); systems = List.map (fun (crn:Crn) -> crn.initialise()) model.systems }
    let initialised = { inferencegraph with nodes = inferencegraph.nodes |> Map.map (fun _ model -> initialise_model model) }
    initialised

let prepare_Model (gui:GuiIG) (model_id:string) : Model =
  let ig = gui.to_ig()
  let ig = InferenceSiteGraph.expandAndLift ig
  let model = Map.find model_id ig.nodes
  let model = model.merge_shared_settings()
  model

let prepare_CRN_for_sim (gui : GuiIG) (model_id:string) (instance : Instance<string>) =
    // Convert from the gui form.
    let model = prepare_Model gui model_id
    // Find the CRN for this instance.
    let crn = match instance.model with
              | "" -> model.top
              | m -> match List.tryFind (fun (s:Crn) -> s.name = m) model.systems with
                     | Some crn -> crn
                     | None -> model.top
    let settings = instance.settings.map (Functional2.from_string_plot >> Expression.map (Crn.species_to_rates crn.settings.rates))
    // Insert the settings and environment in the crn.
    let crn = {crn with settings = crn.settings.update_simulation settings}
    let crn = crn.substitute instance.environment
    crn

// I can't ship a Populations directly. I need to render it to SVG first. But I can't render a Populations directly, either (the renderer runs on Initials[]). So I need to convert it to Initials, then render it to SVG, and then ship it.
let pop_to_svg (crn:Crn) (x:Populations<Species,float>) =
    let pop_to_init (p:Population<Species,float>) = Initial.create(Expression.Float p.value, p.species, None)
    let initials = x.index_to_species |> Array.map pop_to_init |> List.ofArray
    let crnx = { crn with initials = initials }
    let svg = (try Svg.to_string Crn.default_svg_style <| Crn.initials_to_svg crnx with e -> e.Message)
    svg

let get_instances (gui:GuiIG) (model_id:string) : Instance<string> list =
    let model = prepare_Model gui model_id
    let crninstances (crn:Crn) = 
      let instances = crn.get_instances()
      let map_instance instance = Instance.create(instance.model, instance.sweep, instance.assignment, instance.environment, instance.settings.map Functional2.to_string_plot, instance.name)
      let instances = List.map map_instance instances
      instances
    let instances =
      match model.systems with
      [] -> crninstances model.top
      | _ -> model.systems |> List.collect crninstances
    instances

let make_instance_name (instance:Instance<string>) =
    if instance.name <> "" then instance.name else
    let name = instance.model
    let name = if instance.sweep = "" then name else name + "." + instance.sweep
    name

let simulateFloat (ig : GuiIG) (node_id:string) (instance : Instance<string>) (output : Row<float> -> unit) (output_export : export_def->unit) (cancel:bool ref) = 
    let crn = prepare_CRN_for_sim ig node_id instance
    let instance_name = make_instance_name instance
    let output_finals (x:Populations<Species,float>) =
        let svg = pop_to_svg crn x
        let svg = [|svg|]
        let exportDef = { content_type = "image/svg+xml"; id = "finals"; display_name = "Finals"; node_id = Some node_id; instance = (match instance_name with "" -> None | name -> Some name); content = Some svg; save_content = None }
        output_export exportDef
    match crn.settings.simulator with
    | Simulator.Oslo -> crn.to_oslo().simulate_callback cancel output output_finals |> ignore
    | Simulator.SSA -> crn.to_ssa().simulate_callback cancel output output_finals (Some (fun _ _ _ -> ())) |> ignore // Note: if I use None here, W# compilation produces invalid code. Unable to create a simple repro.
    | Simulator.Sundials ->
        // Note: this will return all results at once when computation is done. In order to have progressive update, we need a Sundials simulator that takes a callback.
        let result = crn.to_sundials().simulate()
        Table.to_rows result |> List.iter output
    | _ -> failwithf "The %s simulator cannot be used here" crn.settings.simulator.to_string    

type jit<'s> when 's : equality and 's:comparison =
  { jit: Jit.t<'s>
    calculus: Calculus<'s> }

let simulateMeanStdev (ig : GuiIG) (model_id:string) (instance : Instance<string>) (output : Row<Point> -> unit) (output_export:export_def->unit) (cancel:bool ref) = 
    let crn = prepare_CRN_for_sim ig model_id instance
    match crn.settings.simulator with
    | Simulator.LNA -> crn.to_lna().simulate_callback cancel output |> ignore
    | Simulator.CME -> crn.simulate_cme_callback cancel ignore output |> ignore    // Note: Calling simulate_cme_callback will always regenerate the state-space. It would be better to only regenerate if the state-space has changed.
    | Simulator.MC -> failwith "MC simulator cannot be invoked this way"
    | _ -> failwith "simulator does not return mean+stdev"

let simulateSpatial1D (ig : GuiIG) (model_id:string) (instance : Instance<string>) (output : Row<float []> -> unit) (output_export:export_def->unit) (cancel:bool ref) = 
    let crn = prepare_CRN_for_sim ig model_id instance
    match crn.settings.simulator with
    | Simulator.PDE -> 
        match crn.settings.spatial.dimensions with
        | 1 -> crn.to_pde1d() |> Pde<float []>.simulate_1d_callback cancel output
        | _ -> failwith "wrong number of dimensions"
    | _ -> failwith "simulator does not return 1d spatial"

let simulateSpatial2D (ig : GuiIG) (model_id:string) (instance : Instance<string>) (output : Row<float [] []> -> unit) (output_export:export_def->unit) (cancel:bool ref) = 
    let crn = prepare_CRN_for_sim ig model_id instance
    match crn.settings.simulator with
    | Simulator.PDE -> 
        match crn.settings.spatial.dimensions with
        | 2 -> crn.to_pde2d() |> Pde<float [][]>.simulate_2d_callback cancel output
        | _ -> failwith "wrong number of dimensions"
    | _ -> failwith "simulator does not return 2d spatial"

let simulateMeanStdevTable (ig : GuiIG) (model_id:string) (instance : Instance<string>) (output : Table<Point> -> unit) (output_export:export_def->unit) (cancel:bool ref) =
    let crn = prepare_CRN_for_sim ig model_id instance
    if crn.settings.stochastic.trajectories > 1 then
        crn.to_ssa().simulate_trajectories_callback cancel output
    else
        failwith "simulator does not return mean+stdev table"

type probabilityMap = 
    { times : float list
      values : int list
      probabilities : float [] list }


type transition = 
    { target : int // this is the index in the state_space structure.
      propensity : string }

type state = 
    { species : int Stringmap.t // this goes from species name to population count
      transitions : transition [] }

type state_space = 
    { states : state []
      start_index : int
      attributes : Attributes [] }

let convert_state_space (ctmc_result : ctmc_result<'t>) (namer:'t->Attributes) : state_space = 
    // I'll maintain a map from each state to its index in the array, so I can find them quickly while doing transitions.
    let idmap = Dictionary.empty()
    
    // I've rewritten this function into iterative form as the recursive one was overflowing stacks when transpiled into JavaScript
    let originalStates = 
        ctmc_result.ctmc.graph
        |> Dictionary.toSeq
        |> Array.ofSeq

    let attributes = new System.Collections.Generic.List<Attributes>()
    let namer sp =
      let attrib = namer sp
      attributes.Add attrib
      attrib.name
    
    //First we form the states without transitions so they get their IDs
    let statesToTransmitWithoutTransitions = 
        originalStates |> Array.mapi (fun i (k, v) -> 
                              let state = k
                              Dictionary.add idmap state i //keep for fast lookups
                              let species = 
                                  Seq.map 
                                      (fun (kv : System.Collections.Generic.KeyValuePair<int, int>) -> 
                                      (ctmc_result.to_species.[kv.Key] |> namer, kv.Value)) state
                              { species = 
                                    species
                                    |> List.ofSeq
                                    |> Stringmap.of_list
                                transitions = [||] })
    
    //Then we patch in the transitions to IDs
    let statesToTransmitWithTransitions = 
        (originalStates, statesToTransmitWithoutTransitions) 
        ||> Array.mapi2 (fun i (k, v) withoutTransitions -> 
                let transitions = v
                
                let mapTransition (transition : Transition) = 
                    { target = Dictionary.find idmap transition.target
                      // I'm converting the propensity to a string, so that the UI doesn't have to bother stringifying it.
                      propensity = Expression.to_string (fun s -> s) transition.propensity }
                
                let mappedTransitions = 
                    transitions
                    |> List.map mapTransition
                    |> List.toArray
                
                { withoutTransitions with transitions = mappedTransitions })
    
    { states = statesToTransmitWithTransitions
      start_index = Dictionary.find idmap ctmc_result.ctmc.initial_state
      attributes = attributes.ToArray() }

let simulateMeanStdevProbabilities (ig : GuiIG) (model_id:string) (instance : Instance<string>) (ctmc_output:state_space -> unit) (output : Row<Point> -> unit) (output_export:export_def->unit) (cancel:bool ref) : Probabilities = 
    let crn = prepare_CRN_for_sim ig model_id instance
    let attributes (sp:Species) = Stringmap.find sp.name crn.attributes
    let ctmc_output ctmc = convert_state_space ctmc attributes |> ctmc_output
    let cme = 
        match crn.settings.simulator with
        | Simulator.CME -> crn.simulate_cme_callback cancel ctmc_output output
        | Simulator.CMESundials -> let (cme,_) = crn.simulate_cmesundials_callback cancel ctmc_output output in cme
        | _ -> failwith "simulator does not return mean+stdev and probabilities"
    let idx = new System.Collections.Generic.Dictionary<string, Species>()
    Array.iter (fun (p : Population<Species, float>) -> idx.Add(p.species.name, p.species)) 
        cme.simulator.populations.index_to_species
    cme.probabilities

let getProbabilityMap (p : Probabilities) (speciesName : string) (lowerBound: float): probabilityMap = 
    let times = Cme.get_times p
    let (_, max, min) = Probabilities.get_bounds p speciesName
    let values = List.init (max - min + 1) (fun i -> i + min)
    let probabilities = Probabilities.probability_map (Some lowerBound) p speciesName
    { times = times
      values = values
      probabilities = probabilities }

let simulateFloatJIT (gui:GuiModel) (jit:jit<'a>) (output : Row<float> -> unit) (output_plottable:Jit.newplottable->unit) (output_export:export_def->unit) (output_program:GuiIG->unit) (cancel:bool ref)=
    Jit.simulate_callback cancel output output_plottable jit.jit jit.calculus
    let crn = gui.top.to_crn()
    let final_crn = Jit.to_crn jit.jit crn
    // Workaround for https://github.com/dotnet-websharper/core/issues/1091
    let mutable svg = ""
    try svg <- Svg.to_string Crn.default_svg_style <| Crn.initials_to_svg final_crn
    with e -> svg <- e.Message
    let svg = [|svg|]
    let exp = { content_type = "image/svg+xml" ; id = "finals" ; display_name = "Finals" ; node_id = Some gui.top.name ; instance = None ; content = Some svg; save_content = None }
    output_export exp
    let mutable svg = ""
    try svg <- Svg.to_string Crn.default_svg_style <| Crn.reactions_to_svg final_crn
    with e -> svg <- e.Message
    let svg = [|svg|]
    let exp = { content_type = "image/svg+xml" ; id = "reactions" ; display_name = "Reactions" ; node_id = Some gui.top.name ; instance = None ; content = Some svg; save_content = None }
    output_export exp
    let mutable code = ""
    try code <- final_crn.to_string()
    with e -> code <- e.Message
    let code = [|code|]
    let exp = { content_type = "text/plain" ; id = "code" ; display_name = "Code" ; node_id = Some gui.top.name ; instance = None ; content = Some code; save_content = None }
    output_export exp
    let final_crn = { final_crn with initials = crn.initials }
    let final_crn = final_crn.saturate_initials()
    let gui = Gui.from_crn final_crn
    let gui = { top = gui; systems = [] }
    let gui = { task = None; nodes = [("",gui)] |> Map.ofList; edges = Map.empty; expanded = false }
    output_program gui

let simulator_to_value_type (settings:Crn_settings<Functional>) =
    match settings.simulator with
    | Simulator.LNA -> MeanStdev
    | Simulator.CME | Simulator.CMESundials -> MeanStdevProbabilities
    | Simulator.Oslo -> Float
    | Simulator.SSA ->
        if settings.stochastic.trajectories > 1 then MeanStdevTable else Float
    | Simulator.Sundials -> Float
    | Simulator.PDE -> 
        match settings.spatial.dimensions with
        | 1 -> Spatial1D
        | 2 -> Spatial2D
        | _ -> failwith "unsupported number of dimensions"
    | Simulator.MC -> MeanStdev

let user_get_sim_runs (guiig: GuiIG) (model_id:string) : sim_run = 
    let ig = guiig.to_ig()
    let igEx = InferenceSiteGraph.expandAndLift ig
    let model = Map.find model_id igEx.nodes
    // Verify that all systems use the same simulator. Systems with different simulators can work in theory, but would require changing the way sim runs are declared and processed.
    let base_settings = match model.systems with [] -> model.top.settings | first::_ -> first.settings
    model.systems |> List.iter (fun crn -> if crn.settings.simulator <> base_settings.simulator then failwith "Systems with different simulators are not supported")
    let value_type = simulator_to_value_type base_settings
    let instances = get_instances guiig model_id
    { value_type = value_type
      //jit = false // TODO: set jit appropriately
      //AP//instances = crn.get_instances() }
      instances = instances }

//This is what happens when the user clicks Parse when the CRN -> Code tab is selected
let user_parse_code (code : string) = 
    let igraph = (parse_code code)
    let gui = GuiIG.from_ig igraph
    gui

let model_to_single_export (ig:IGraph) (nodeId:string) (id:string) : export_def =
    let node = Map.find nodeId ig.nodes
    let model = node.saturate_initials()
    match id with
    | "code" ->
        // Workaround for https://github.com/dotnet-websharper/core/issues/1091
        let mutable content = ""
        try
            if nodeId = "" then
                content <- { IGraph.task=ig.task; IGraph.nodes=Map.ofList["",model]; IGraph.edges=Map.empty; IGraph.expanded=false }.to_string()
            else
                content <- model.string()
        with e -> content <- e.Message
        let content = [|content|]
        { content_type = "text/plain"; id = id; display_name = "Code"; node_id = Some nodeId; instance = None ; content = Some content; save_content = None }
    | "sbml" ->
        let mutable content = ""
        try content <- Sbml.to_xml (model.to_sbml())
        with e -> content <- e.Message
        let content = [|content|]
        { content_type = "text/plain"; id = id; display_name = "SBML"; node_id = Some nodeId;instance = None ; content = Some content; save_content = None }
    | "initials" ->
        let mutable content = ""
        try content <- Svg.to_string Crn.default_svg_style <| Model.initials_to_svg model
        with e -> content <- e.Message
        let content = [|content|]
        { content_type = "image/svg+xml"; id = id; display_name = "Initials"; node_id = Some nodeId;instance = None ; content = Some content; save_content = None }
    | "finals" ->
        let content = "Finals not available for single export"
        let content = [|content|]
        { content_type = "image/svg+xml"; id = id; display_name = "Finals"; node_id = Some nodeId;instance = None ; content = Some content; save_content = None }
    | "reactions" ->
        let mutable content = [||]
        try content <- Model.reactions_to_svgs model |> List.map (Svg.to_string Crn.default_svg_style) |> List.toArray
        with e -> content <- [|e.Message|]
        let mutable scontent = ""
        try scontent <- Svg.to_string Crn.default_svg_style <| Model.reactions_to_svg model
        with e -> scontent <- e.Message
        { content_type = "image/svg+xml"; id = id; display_name = "Reactions"; node_id = Some nodeId;instance = None ; content = Some content; save_content = Some scontent }
    | "matlab" ->
        let mutable content = ""
        try content <- model.to_matlab()
        with e -> content <- e.Message
        let content = [|content|]
        { content_type = "text/plain"; id = id; display_name = "Matlab"; node_id = Some nodeId;instance = None ; content = Some content; save_content = None }
    | _ ->
        let content = "Unknown export ID " + id
        let content = [|content|]
        { content_type = "text/plain"; id = id; display_name = id; node_id = Some nodeId;instance = None ; content = Some content; save_content = None }

let model_to_export final (ig:IGraph) (nodeId:string) : export_def[] = 
    let node = Map.find nodeId ig.nodes
    let model = node.saturate_initials()
    let initials = try if final then None else Svg.to_string Crn.default_svg_style <| Model.initials_to_svg model |> Some with e -> Some e.Message
    let initials = match initials with Some initials -> Some [|initials|] | None -> None
    let finals = try if final then Svg.to_string Crn.default_svg_style <| Model.initials_to_svg model |> Some else Some "" with e -> Some e.Message
    let finals = match finals with Some finals -> Some [|finals|] | None -> None
    [| { content_type = "text/plain"; id = "code"; display_name = "Code"; node_id = Some nodeId; instance = None ;content = None; save_content = None }
       { content_type = "text/plain"; id = "sbml"; display_name = "SBML"; node_id = Some nodeId; instance = None ;content = None; save_content = None }
       { content_type = "image/svg+xml" ; id = "initials" ; display_name = "Initials" ; node_id = Some nodeId; instance = None ;content = initials; save_content = None }
       { content_type = "image/svg+xml" ; id = "finals" ; display_name = "Finals" ; node_id = Some nodeId; instance = None ;content = finals; save_content = None }
       { content_type = "image/svg+xml"; id = "reactions"; display_name = "Reactions"; node_id = Some nodeId; instance = None ;content = None; save_content = None }
       { content_type = "text/plain"; id = "matlab"; display_name = "Matlab"; node_id = Some nodeId; instance = None ;content = None; save_content = None } |]

//This is what happens when the user clicks Parse when one of the CRN -> Directives, Parameters, Species or Reactions tab is selected (currently, all this does is generate the exports). The "final" flag indicates whether the population should be considered a final population (and therefore go in the "finals" export); this concept should be reworked.
let user_get_exports final (gui : GuiIG) (nodeId: string) =
    let ig = gui.to_ig()
    let exports = model_to_export final ig nodeId
    exports

let user_get_export (gui:GuiIG) (nodeId: string) (id:string) =
    let ig = gui.to_ig()
    let export = model_to_single_export ig nodeId id
    export

type inference_evaluated_values = 
    { values : System.Collections.Generic.Dictionary<string, float>
      lglk : float }

let convert_evaluated_values (ev : Microsoft.Research.Filzbach.Parameters.EvaluatedValues) = 
    let convert_associative_array (aa : Microsoft.Research.Filzbach.DataStructures.AssociativeArray<'t>) = 
      let ret = new System.Collections.Generic.Dictionary<string, 't>()
      Array.iter (fun name -> ret.Add(name, aa.Item(name))) aa.Names
      ret
    { values = convert_associative_array ev.values
      lglk = ev.logLikelihood }

type inference_burnin = 
    { space : System.Collections.Generic.Dictionary<string, Microsoft.Research.Filzbach.Parameters.ParameterRange> //contains only non-fixed parameters information
      state : inference_evaluated_values //contains all parameters (non-fixed followed by fixed)
      stats : Microsoft.Research.Filzbach.Parameters.ParameterStatistics
      mle : inference_evaluated_values
      priors : Microsoft.Research.Filzbach.Parameters.PriorValue option array
      indexes : int array }

type inference_sampling = 
    { space : System.Collections.Generic.Dictionary<string, Microsoft.Research.Filzbach.Parameters.ParameterRange> //contains only non-fixed parameters information
      state : inference_evaluated_values //contains all parameters (non-fixed followed by fixed)
      thinningSkippedCount : int //how many steps are passed after last save of state to the chain
      chain : inference_evaluated_values list
      mle : inference_evaluated_values
      priors : Microsoft.Research.Filzbach.Parameters.PriorValue option array
      indexes : int array }

[<WebSharper.NamedUnionCases>]
type inference_phase = 
    | BurninPhase of BurninPhase : inference_burnin
    | SamplingPhase of SamplingPhase : inference_sampling

type inference_result = 
    { nodeId : string
      iteration : int
      lkincreased : bool
      state : inference_phase
      mlesims : Microsoft.Research.CRNEngine.Result<float> list option
      summary : string }

let convert_inference_result nodeId (res : Inference.mcmc_intermediate_result) (lkincreased:bool) : inference_result = 
    let convert_associative_array (aa : Microsoft.Research.Filzbach.DataStructures.AssociativeArray<'t>) = 
      let ret = new System.Collections.Generic.Dictionary<string, 't>()
      Array.iter (fun name -> ret.Add(name, aa.Item(name))) aa.Names
      ret
    let convert_phase (p : Filzbach.RunPhase) : inference_phase = 
        match p with
        | Filzbach.BurninPhase p -> 
            BurninPhase { inference_burnin.space = convert_associative_array p.space
                          inference_burnin.state = convert_evaluated_values p.state
                          inference_burnin.stats = p.stats
                          inference_burnin.mle = convert_evaluated_values p.mle
                          inference_burnin.priors = p.priors
                          inference_burnin.indexes = p.indexes }
        | Filzbach.SamplingPhase p -> 
            SamplingPhase { inference_sampling.space = convert_associative_array p.space
                            inference_sampling.state = convert_evaluated_values p.state
                            inference_sampling.thinningSkippedCount = p.thinningSkippedCount
                            inference_sampling.chain = List.map convert_evaluated_values p.chain
                            inference_sampling.mle = convert_evaluated_values p.mle
                            inference_sampling.priors = p.priors
                            inference_sampling.indexes = p.indexes }
    { nodeId = nodeId
      iteration = res.iteration
      lkincreased = lkincreased
      state = convert_phase res.state
      mlesims = if lkincreased then Some res.mlesims else None
      summary = res.summary }

type InferenceParameterType = | [<WebSharper.Constant "Real">] Real
                              | [<WebSharper.Constant "Log">] Log
                              | [<WebSharper.Constant "Fixed">] Fixed
type InferenceParameterRange =
    { pType: InferenceParameterType
      lb: float
      ub:float }
type InferenceParameter =
    { name : string
      range : InferenceParameterRange
      initValue: float option }
type InferenceParameters =
    { nodeId : string
      parameters : InferenceParameter list }

let combine_dynchar_exports (exports:(string*Plotting.html_content) list) : export_def =
    match exports with
    | [(_,export)] ->
      let html_string = Plotting.structured_tabbed_embedded "top_tabs" export
      let html_string = [|html_string|]
      let html_page = Plotting.structured_tabbed "" "top_tabs" export
      {content_type = "text/html"; id = "dynamicCharacterization"; display_name = "Dynamic Characterization"; node_id = None; instance = None ;content = Some html_string; save_content = Some html_page }
    | _ ->
      let embedded = List.mapi (fun i (name,content) -> name,(Plotting.structured_tabbed_embedded (sprintf "group%d" i) content)) exports |> Seq.toList
      let html_string = Plotting.arbitrary_tabbed_content "top_tabs" embedded |> Plotting.make_embedded
      let html_string = [|html_string|]
      let html_page = Plotting.arbitrary_tabbed "Dynamic Characterisation" embedded
      {content_type = "text/html"; id = "dynamicCharacterization"; display_name = "Dynamic Characterization"; node_id = None; instance = None ;content = Some html_string; save_content = Some html_page }

// Produces a stream of inference results, in GUI-friendly format.
let user_infer_gui (gui : GuiIG) (output_export : export_def -> unit) (output_parameter_definitions : InferenceParameters -> unit) (output_inference : inference_result -> unit) (cancel:bool ref) =
    // Prepare the inference graph.
    let ig = gui.to_ig()
    let ig = InferenceSiteGraph.expandAndLift ig
    let dynchar_exports = new System.Collections.Generic.List<string*Plotting.html_content>()
    // This is the function that runs inference on a single model. It will report its progress through the callbacks.
    let infer_model (model:Model) =
      // Maintain the last unconverted result, which will be required to perform final export operations.
      let mutable final_result : Inference.mcmc_result option = None
      let results, parameters = model.infer_seq (fun result -> final_result <- Some result)
      // Convert the Filzbach-style parameters into GUI-style parameters.
      let convert_parameter (filzpar:Parameters.Parameter) : InferenceParameter =
        { name = filzpar.name
          range = { pType = match filzpar.range.pType with Parameters.ParameterType.Real -> Real
                                                         | Parameters.ParameterType.Log -> Log
                                                         | Parameters.ParameterType.Fixed -> Fixed
                    lb = filzpar.range.lb
                    ub = filzpar.range.ub }
          initValue = filzpar.initValue }
      let parameters = List.map convert_parameter parameters
      // Convert the stream and output accessory informations.
      let results = results |> Seq.choose (fun (a,_,increased) -> match a with None -> None | Some a -> convert_inference_result model.top.name a increased |> Some)
      output_parameter_definitions { nodeId = model.top.name; parameters = parameters }
      // Output the stream.
      Seq.iter output_inference results
      // Look at the final result. If inference terminated correctly, it should be a SamplingPhase.
      match final_result with
      | Some result ->
            // Generate the final exports.
            if result.posterior <> [] then
              let exp = Html.mcmc_to_results model.top.settings model result |> Html.results_to_html_content
              dynchar_exports.Add(model.top.name,exp);
      | None -> ()
      final_result.Value.to_summary()
    // Run inference on the graph.
    InferenceSiteGraph.inferWith infer_model ig |> ignore
    // Combine and output the dynamic characterisation exports
    let export = dynchar_exports |> List.ofSeq |> combine_dynchar_exports 
    output_export export
    
let user_state_space (gui : GuiIG) = 
    let model = gui.nodes |> Map.toSeq |> Seq.head |> snd
    let crn = model.top.to_crn()
    let ctmc_result = crn.to_ctmc()
    let attributes (sp:Species) = Stringmap.find sp.name crn.attributes
    convert_state_space ctmc_result attributes

let user_state_space_jit (jit:jit<'a>) =
    let (ss,attributes) = Jit.to_ctmc jit.jit jit.calculus
    convert_state_space ss attributes

let expression_to_string (exp : Expression.t<string>) = exp |> Expression.to_string id

let mcplot_to_string (mcplot : Moment_closure_settings.monomial) = Moment_closure_settings.to_string_monomial mcplot

type column_array<'v> = 
    { name : string
      values : 'v [] }

type table_array<'v> = 
    { times : float []
      columns : column_array<'v> [] }

let test_crn_deterministic (crn : string) : table_array<float> = 
    let table = (Parser.from_string Crn.parse crn).to_oslo().simulate()
    { //table 
      //Expando lists are awkward to work with in JavaScript, turn them into arrays
      times = table.times |> Array.ofList
      columns = 
          (table.columns |> List.map (fun column -> 
                                { name = column.name
                                  values = column.values |> Array.ofList }))
          |> Array.ofList }

let test_crn_sundials (crn : string) : table_array<float> = 
    let table = (Parser.from_string Crn.parse crn).to_sundials().simulate()
    { //table 
      //Expando lists are awkward to work with in JavaScript, turn them into arrays
      times = table.times |> Array.ofList
      columns = 
          (table.columns |> List.map (fun column -> 
                                { name = column.name
                                  values = column.values |> Array.ofList }))
          |> Array.ofList }

let prepare_for_cme (crnStr:string) =
  let crn  = Parser.from_string Crn.parse crnStr
  let ctmc = crn.to_ctmc()
  let cme = crn.to_cme ctmc
  let env = Parameters.to_env crn.settings.parameters
  let rates = Crn.to_inlined_rates crn.settings
  cme, env, rates

let test_crn_cme_sundials (crn : string) : table_array<Point> = 

    let cme, env, rates = prepare_for_cme crn
    let cme, table = cme.simulate_sundials env rates
    {
      times = table.times |> Array.ofList
      columns = 
          (table.columns |> List.map (fun column -> 
                                { name = column.name
                                  values = column.values |> Array.ofList }))
          |> Array.ofList }