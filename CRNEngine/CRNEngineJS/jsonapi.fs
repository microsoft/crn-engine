// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScriptExport>]
module Microsoft.Research.CRNEngine.JSONAPI
open Microsoft.Research.CRNEngine.Hashtable
open WebSharper
open Microsoft.Research.CRNEngine.JSAPI

// I'm putting the call to Decode in a separate function to prevent W# from generating the serialization code multiple times;
// this is especially annoying for JSAPI.t because it's both large and frequently used.
let decodeIG gui = WebSharper.Json.Decode<GuiIG> gui
let encodeIG gui = WebSharper.Json.Encode<GuiIG> gui
let decodeModel gui = WebSharper.Json.Decode<GuiModel> gui
let encodeModel gui = WebSharper.Json.Encode<GuiModel> gui

let user_parse_code (code:string) =
  let ret = JSAPI.user_parse_code code
  let ret = WebSharper.Json.Encode ret
  ret

let user_get_exports final (gui:obj) (nodeId:string) = 
  let gui:GuiIG = decodeIG gui
  let ret = JSAPI.user_get_exports final gui nodeId
  let ret = WebSharper.Json.Encode ret
  ret

let user_get_export (gui:obj) (nodeId:string) (id:string) = 
  let gui:GuiIG = decodeIG gui
  let ret = JSAPI.user_get_export gui nodeId id
  let ret = WebSharper.Json.Encode ret
  ret

let user_infer_gui (gui:obj) (output_export:obj -> unit) (output_parameter_definitions:obj -> unit ) (output_inference:obj -> unit) (cancel:bool ref) : unit=
  let gui = decodeIG gui
  let output_export (x:JSAPI.export_def) = WebSharper.Json.Encode x |> output_export
  let output_parameter_definitions (x:JSAPI.InferenceParameters) = WebSharper.Json.Encode x |> output_parameter_definitions
  let output_inference (x:JSAPI.inference_result) = WebSharper.Json.Encode x |> output_inference
  JSAPI.user_infer_gui gui output_export output_parameter_definitions output_inference cancel

let user_state_space (gui:obj) : obj =
  let gui = decodeIG gui
  let ret = JSAPI.user_state_space gui
  let ret = WebSharper.Json.Encode ret
  ret

let user_state_space_jit (jit:JSAPI.jit<'s>) : obj =
  let ret = JSAPI.user_state_space_jit jit
  let ret = WebSharper.Json.Encode ret
  ret

let user_get_sim_runs (gui:obj) (model_id:string) : obj =
  let gui = decodeIG gui
  let ret = JSAPI.user_get_sim_runs gui model_id
  let ret = WebSharper.Json.Encode ret
  ret
  
let simulateFloat (gui:obj) (model_id:string) (env:obj) (output:obj -> unit) (output_export:obj->unit) (cancel:bool ref) : unit =
  let gui = decodeIG gui
  let env = WebSharper.Json.Decode env
  let output (x:Row<float>) = WebSharper.Json.Encode x |> output
  let output_export (x:export_def) = WebSharper.Json.Encode x |> output_export
  JSAPI.simulateFloat gui model_id env output output_export cancel

let simulateFloatJIT (gui:obj) (jit:JSAPI.jit<'s>) (output:obj->unit) (outputplottable:obj->unit) (outputexport:obj->unit) (outputprogram:obj->unit) (cancel:bool ref) : unit =
  let gui = decodeModel gui
  let output (x:Row<float>) = WebSharper.Json.Encode x |> output
  let outputplottable (x:Jit.newplottable) = WebSharper.Json.Encode x |> outputplottable
  let outputprogram (x:GuiIG) = WebSharper.Json.Encode x |> outputprogram
  let outputexport (x:export_def) = WebSharper.Json.Encode x |> outputexport
  JSAPI.simulateFloatJIT gui jit output outputplottable outputexport outputprogram cancel

let simulateMeanStdev (gui:obj) (model_id:string) (env:obj) (output:obj -> unit) (output_export:obj->unit) (cancel:bool ref) : unit =
  let gui = decodeIG gui
  let env = WebSharper.Json.Decode env
  let output (x:Row<Point>) = WebSharper.Json.Encode x |> output
  let outputexport (x:export_def) = WebSharper.Json.Encode x |> output_export
  JSAPI.simulateMeanStdev gui model_id env output outputexport cancel

let simulateMeanStdevProbabilities (gui:obj) (model_id:string) (env:obj) (ctmc_output:obj -> unit) (output:obj -> unit) (output_export:obj->unit) (cancel:bool ref) : obj =
  let gui = decodeIG gui
  let env = WebSharper.Json.Decode env
  let output (x:Row<Point>) = WebSharper.Json.Encode x |> output
  let ctmc_output (x:JSAPI.state_space) = WebSharper.Json.Encode x |> ctmc_output
  let outputexport (x:export_def) = WebSharper.Json.Encode x |> output_export
  let ret:Probabilities = JSAPI.simulateMeanStdevProbabilities gui model_id env ctmc_output output outputexport cancel
  let ret = WebSharper.Json.Encode ret
  ret

let getProbabilityMap (p:Probabilities) (species:string) (lowerBound:float): obj =
  let p = WebSharper.Json.Decode p
  let ret = JSAPI.getProbabilityMap p species lowerBound
  let ret = WebSharper.Json.Encode ret
  ret

let simulateSpatial1D (gui:obj) (model_id:string) (env:obj) (output:obj -> unit) (output_export:obj->unit) (cancel:bool ref) : unit =
  let gui = decodeIG gui
  let env = WebSharper.Json.Decode env
  let output (x:Row<float[]>) = WebSharper.Json.Encode x |> output
  let outputexport (x:export_def) = WebSharper.Json.Encode x |> output_export
  JSAPI.simulateSpatial1D gui model_id env output outputexport cancel

let simulateSpatial2D (gui:obj) (model_id:string) (env:obj) (output:obj -> unit) (output_export:obj->unit) (cancel:bool ref) : unit =
  let gui = decodeIG gui
  let env = WebSharper.Json.Decode env
  let output (x:Row<float[][]>) = WebSharper.Json.Encode x |> output
  let outputexport (x:export_def) = WebSharper.Json.Encode x |> output_export
  JSAPI.simulateSpatial2D gui model_id env output outputexport cancel

let simulateMeanStdevTable (gui:obj) (model_id:string) (env:obj) (output:obj -> unit) (output_export:obj->unit) (cancel:bool ref) : unit =
  let gui = decodeIG gui
  let env = WebSharper.Json.Decode env
  let output (x:Table<Point>) = WebSharper.Json.Encode x |> output
  let outputexport (x:export_def) = WebSharper.Json.Encode x |> output_export
  JSAPI.simulateMeanStdevTable gui model_id env output outputexport cancel

let expression_to_string (exp:obj) =
  let exp = WebSharper.Json.Decode exp
  let ret = JSAPI.expression_to_string exp
  ret

let mcplot_to_string (exp:obj) =
  let exp = WebSharper.Json.Decode exp
  let ret = JSAPI.mcplot_to_string exp
  ret

// We're handling this by <WebSharperDeadCodeElimination>False</WebSharperDeadCodeElimination>
// We may get a fuller solution at some point: https://github.com/intellifactory/websharper/issues/683

(*
[<SPAEntryPoint>]
let Main () =
    //Compel WebSharper to export these.
    //Had to start doing this in the 4.x branch

    user_parse_code |> ignore
    user_parse_gui |> ignore
    user_infer_gui |> ignore
    user_state_space |> ignore
    user_get_sim_runs |> ignore
    simulateFloat |> ignore
    simulateMeanStdev |> ignore
    simulateMeanStdevProbabilities |> ignore
    getProbabilityMap |> ignore
    simulateSpatial1D |> ignore
    simulateSpatial2D |> ignore
    expression_to_string |> ignore

    printfn("Running WebSharper fake Entry Point..")*)