// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineServerLib.Inference

open Microsoft.Research.CRNEngineServerLib.Serialisation
open System.Net.WebSockets
open Messages
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI
open Microsoft.Research.Filzbach
open Microsoft.Research.CRNEngineCloudLib
open Microsoft.Research.CliLibrary

let processInferGuiRequest (ig:GuiIG) (pool:string) (webSocket:WebSocket) (cancel_flag:bool ref) = 
    match ig.task with Some t -> (match t.task_type with Some TaskType.Parse -> failwith "this model is designed for parsing only" | _ -> ()) | _ -> ()
    let sendObject o = sendObject webSocket o
    //This has been written to match JavaScript Web Worker backend, see Worker.ts
    let mutable exc = None
    use res = new System.Collections.Concurrent.BlockingCollection<obj>()
    async {
      try
        // Send simulation instance definitions.
        let output_instances (model:GuiModel) =
          let runs = JSAPI.user_get_sim_runs ig model.top.name
          let definitions = 
              runs.instances
              |> List.mapi (fun i e -> 
                      { id = i
                        instance = e })
              |> List.toArray
          res.Add { mtype = "instancedefinitions"
                    nodeId = model.top.name
                    definitions = definitions }
        Map.iter (fun k model -> output_instances model) ig.nodes
        // Prepare export callback.
        let output_export export = res.Add { mtype = "export"; export = export }
        // Prepare parameter definitions callback.
        let output_parameter_definitions (parameters:InferenceParameters) = res.Add { mtype = "parameterdefinitions"; nodeId = parameters.nodeId;parameters = parameters.parameters |> List.toArray }
        // Prepare results callback.
        let output_inference (result:inference_result) =
          res.Add { mtype = "summary" ; nodeId = result.nodeId; summary = result.summary }
          res.Add { mtype = "inferenceprogress" ; nodeId=result.nodeId; progress = result.iteration }
          match result.state with
          | JSAPI.SamplingPhase p -> 
              if p.thinningSkippedCount = 0 then 
                  match p.chain with
                  | [] -> ()
                  | update :: _ -> 
                      res.Add { mtype = "inferencechainupdate"
                                nodeId = result.nodeId
                                update = update }
          | _ -> ()
          if result.lkincreased then
            (let state = 
               match result.state with
               | JSAPI.BurninPhase p -> p.mle
               | JSAPI.SamplingPhase p -> p.mle
             res.Add { mtype = "parameterresult"
                       nodeId = result.nodeId
                       values = state })
          // sMlesims will only be present if the likelihood has increased.
          match result.mlesims with
          | Some mlesims ->
              for i, sim in mlesims |> Seq.indexed do
                res.Add { mtype = "inferenceresult"
                          nodeId = result.nodeId
                          result = { result = sim
                                     sim_id = i } }
          | _ -> ()
        // Run inference.
        if Azure.enabled && pool <> "" then
            let results = AzureJobs.inferOnAzureGUI pool ig
            ignore results
        else
            JSAPI.user_infer_gui ig output_export output_parameter_definitions output_inference cancel_flag
      with e ->
        exc <- Some e
      try res.CompleteAdding() with _ -> ()
    } |> Async.Start
    for msg in res.GetConsumingEnumerable() do
        match msg with
        | :? Response_InferenceChainUpdate as res -> sendObject res
        | :? Response_InferenceProgress as res -> sendObject res
        | :? Response_InstanceDefinitions as res -> sendObject res
        | :? Response_ParameterDefinitions as res -> sendObject res
        | :? Response_Export as res -> sendObject res
        | :? Response_InferenceSummary as res -> sendObject res
        | :? Response_ParameterResult as res -> sendObject res
        | :? Response_InferenceResult as res -> sendObject res
        | _ -> failwith (sprintf "unmatched type %s" (msg.GetType().Name))
    match exc with Some exc -> sendObject {mtype="error";error={message=exc.Message;positions=None}}
                 | None -> ()

let processInferCodeRequest code datasets (pool:string) (webSocket:WebSocket) (cancel_flag:bool ref) =
    let ig = JSAPI.parse_code code
    let datasetsList = List.ofArray datasets
    let mapcrn (crn:Crn) = { crn with settings = { crn.settings with data = datasetsList } }
    let mapmodel (model:Model) = { model with top = mapcrn model.top; systems = List.map mapcrn model.systems }
    let ig = { ig with nodes = ig.nodes |> Map.map (fun k v -> mapmodel v) }
    let gui = GuiIG.from_ig ig
    processInferGuiRequest gui pool webSocket cancel_flag
