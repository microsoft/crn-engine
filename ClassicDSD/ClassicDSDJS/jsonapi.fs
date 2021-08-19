[<JavaScriptExport>]
module Microsoft.Research.DNA.JSONAPI
open WebSharper
open Microsoft.Research.DNA.Dsd
open Microsoft.Research.DNA.JSAPI
open Microsoft.Research.CRNEngine
open Microsoft.Research.DNA

type ParseResult =
  { bundle     : bundle
  ; settings   : obj
  ; unexpanded : obj }
  
let set_options (result:ParseResult) (settings:obj) : ParseResult =
  let settings:Options.t = WebSharper.Json.Decode settings in
  let result:JSAPI.ParseResult = { bundle = result.bundle; settings = WebSharper.Json.Decode result.settings; unexpanded = WebSharper.Json.Decode result.unexpanded }
  let result = JSAPI.set_options result settings
  { bundle = result.bundle ; settings = WebSharper.Json.Encode result.settings; unexpanded = WebSharper.Json.Encode result.unexpanded }

let user_parse (code:obj) : ParseResult =
  let code:ParseObject = WebSharper.Json.Decode code in
  let pr = JSAPI.user_parse code in
  let unexpanded = WebSharper.Json.Encode pr.unexpanded in
  let settings = WebSharper.Json.Encode pr.settings in
  { bundle = pr.bundle ; settings = settings ; unexpanded = unexpanded }

let user_expand (bundle:bundle) : obj =
  let gui = JSAPI.user_expand bundle in
  let ret = WebSharper.Json.Encode gui in
  ret

let user_compile (code:obj) : obj =
  let code:ParseObject = WebSharper.Json.Decode code in
  let gui = JSAPI.user_compile code in
  let ret = WebSharper.Json.Encode gui in
  ret

let user_parse_oldsyntax (code:ParseObject) : ParseResult =
  let code:ParseObject = WebSharper.Json.Decode code in
  let pr = JSAPI.user_parse_oldsyntax code in
  let unexpanded = WebSharper.Json.Encode pr.unexpanded in
  let foo: Map<(string * int), Set<RulesDSD.Syntax.Clause<Microsoft.Research.DNA.LogicDSD.SiteT>>> option = None
  let settings = WebSharper.Json.Encode pr.settings in
  { bundle = pr.bundle ; settings = settings ; unexpanded = unexpanded }

let user_compile_oldsyntax (code:ParseObject) : obj =
  let code:ParseObject = WebSharper.Json.Decode code in
  let gui = JSAPI.user_compile_oldsyntax code in
  let ret = WebSharper.Json.Encode gui in
  ret

(*let convert_bundle_after_jit (bundle:bundle) (jit:Microsoft.Research.CRNEngine.JSAPI.jit<Microsoft.Research.DNA.Species.t>) : obj =
  let gui = JSAPI.convert_bundle_after_jit bundle jit in
  let ret = WebSharper.Json.Encode gui in
  ret*)

let is_jit (bundle:bundle) : bool = JSAPI.is_jit bundle

(*let jit_ctmc (bundle:bundle) (gui:obj) : obj =
  let gui = JSONAPI.decodeGui gui
  let ret = JSAPI.jit_ctmc bundle gui
  let ret = WebSharper.Json.Encode ret
  ret*)

// We're handling this by <WebSharperDeadCodeElimination>False</WebSharperDeadCodeElimination>
// We may get a fuller solution at some point: https://github.com/intellifactory/websharper/issues/683

(*[<SPAEntryPoint>]
let Main () =
    //Compel WebSharper to export these.
    //Had to start doing this in the 4.x branch

    Microsoft.Research.CRNEngine.JSONAPI.Main() |> ignore

    user_parse |> ignore
    user_expand |> ignore
    user_compile |> ignore

    user_parse_oldsyntax |> ignore
    user_compile_oldsyntax |> ignore

    printfn("Starting ClassicDSD fake Entry Point..")*)
