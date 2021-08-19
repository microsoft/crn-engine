module Microsoft.Research.CRNEngineServerLib.Simulation

open Microsoft.Research.CRNEngineServerLib.Serialisation
open System.Net.WebSockets
open Messages
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI
open Microsoft.Research.Filzbach
open Microsoft.Research.CRNEngineCloudLib

let runSimulateGui (isJIT:GuiModel->bool) (getJIT:GuiModel->Instance<string>->JSAPI.jit<'s>) (ig:GuiIG) (nodeId:string) (pool:string) (webSocket:WebSocket) (cancel_flag:bool ref) = 
    let sendObject o = sendObject webSocket o
    match ig.task with Some t -> (match t.task_type with Some TaskType.Parse -> failwith "this model is designed for parsing only" | _ -> ()) | _ -> ()
    let model = ig.nodes |> Map.find nodeId
    sendObject { mtype = "node"
                 node = model }
    let runs = JSAPI.user_get_sim_runs ig nodeId
    
    sendObject { mtype = "simtype"
                 simtype = JSAPI.valueTypeCaseString runs.value_type }
    let definitions = 
        runs.instances
        |> List.mapi (fun i e -> 
               { id = i
                 instance = { e with settings = { e.settings with plots = if model.top.settings.simulator = Simulator.MC then (model.top.settings.moment_closure.plots |> List.map JSAPI.mcplot_to_string) else e.settings.plots } } })
        |> List.toArray
    sendObject { mtype = "instancedefinitions"
                 nodeId = model.top.name
                 definitions = definitions }
    if Azure.enabled && pool <> "" then
        AzureJobs.simulateOnAzureGUI pool ig |> ignore
    else
        for id, instance in runs.instances |> Seq.indexed do
            //TODO: try to reduce duplication between the simulator types, the intermediate store is polymorphic
            let mutable exc = None
            use res = new System.Collections.Concurrent.BlockingCollection<obj>()
            match runs.value_type with
            | ValueType.Float -> 
                let addExport exp = res.Add { Response_Export.mtype = "export"
                                              Response_Export.export = exp }
                let addRow (row:Row<float>) = res.Add { mtype = "simresult"
                                                        row = { instance = id
                                                                time = row.time
                                                                values = row.values } }
                let addNewPlottable (plottable:Jit.newplottable) = res.Add { mtype = "newplottable"
                                                                             plottable = { instance = id
                                                                                           plottable = plottable } }
                async {
                  try
                    if (isJIT model) then
                      let jit = getJIT model instance
                      let output_program gui =  res.Add { Response_Program.mtype = "model"
                                                          Response_Program.model = gui }
                      simulateFloatJIT model jit addRow addNewPlottable addExport output_program cancel_flag
                    else
                      JSAPI.simulateFloat ig model.top.name instance addRow addExport cancel_flag
                  with e ->
                    exc <- Some e
                  try res.CompleteAdding() with _ -> ()
                }
                |> Async.Start
            | ValueType.MeanStdev -> 
                let addRow (row:Row<Point>) = res.Add { mtype = "simresult"
                                                        row = { instance = id
                                                                time = row.time
                                                                values = row.values } }
                let addExport (exp:export_def) = res.Add { mtype = "export"
                                                           export = exp }
                async { 
                  try
                    if model.top.settings.simulator = Simulator.MC then
                      let crn = prepare_CRN_for_sim ig model.top.name instance
                      let moments = Microsoft.Research.CRNEngine.Moments.generateMoments crn
                      let closure = Microsoft.Research.CRNEngine.Moments.generateClosure moments
                      Microsoft.Research.CRNEngine.Moments.simulate_callback cancel_flag addRow closure |> ignore
                    else
                      JSAPI.simulateMeanStdev ig model.top.name instance addRow addExport cancel_flag
                  with e ->
                    exc <- Some e
                  try res.CompleteAdding() with _ -> ()
                }
                |> Async.Start
            | ValueType.MeanStdevProbabilities -> 
                let addRow (row:Row<Point>) = res.Add { mtype = "simresult"
                                                        row = { instance = id
                                                                time = row.time
                                                                values = row.values } }
                let addExport (exp:export_def) = res.Add { mtype = "export"
                                                           export = exp }
                let addSS (ss:state_space) = res.Add { mtype = "statespace"
                                                       statespace = ss }
                async { 
                  try
                      let probabilities = JSAPI.simulateMeanStdevProbabilities ig model.top.name instance addSS addRow addExport cancel_flag
                      res.Add { mtype = "probabilities"
                                probabilities = { instance = instance
                                                  probabilities = probabilities } }
                  with e ->
                    exc <- Some e
                  try res.CompleteAdding() with _ -> ()
                }
                |> Async.Start
            | ValueType.Spatial1D -> 
                let addRow (row:Row<float[]>) = res.Add { mtype = "simresult"
                                                          row = { instance = id
                                                                  time = row.time
                                                                  values = row.values } }
                let addExport (exp:export_def) = res.Add { mtype = "export"
                                                           export = exp }
                async { 
                  try
                    JSAPI.simulateSpatial1D ig model.top.name instance addRow addExport cancel_flag
                  with e ->
                    exc <- Some e
                  try res.CompleteAdding() with _ -> ()
                }
                |> Async.Start
            | ValueType.Spatial2D -> 
                let addRow (row:Row<float[][]>) = res.Add { mtype = "simresult"
                                                            row = { instance = id
                                                                    time = row.time
                                                                    values = row.values } }
                let addExport (exp:export_def) = res.Add { mtype = "export"
                                                           export = exp }
                async { 
                  try
                    JSAPI.simulateSpatial2D ig model.top.name instance addRow addExport cancel_flag
                  with e ->
                    exc <- Some e
                  try res.CompleteAdding() with _ -> ()
                }
                |> Async.Start
            | ValueType.MeanStdevTable ->
                let addTable (table:Table<Point>) = res.Add { mtype = "simtable"
                                                              table = { instance = id
                                                                        time = Array.ofList table.times
                                                                        values = table.columns |> List.map (fun c -> Array.ofList c.values) |> Array.ofList } }
                let addExport (exp:export_def) = res.Add { mtype = "export"
                                                           export = exp }
                async { 
                  try
                    JSAPI.simulateMeanStdevTable ig model.top.name instance addTable addExport cancel_flag
                  with e ->
                    exc <- Some e
                  try res.CompleteAdding() with _ -> ()
                }
                |> Async.Start
            for msg in res.GetConsumingEnumerable() do
                match msg with
                | :? Response_SimResult as res ->
                    // Perform basic validation.
                    let validatepoint (point:Point) =
                        if System.Double.IsNaN point.mean then
                            failwith (sprintf "Mean is NaN at time %f in instance %d" res.row.time res.row.instance)
                        else if System.Double.IsNaN point.stdev then
                            failwith (sprintf "Stdev is NaN at time %f in instance %d" res.row.time res.row.instance)
                    match res.row.values with
                    | :? (Point[]) as points ->
                        Array.iter validatepoint points
                    | _ -> ()
                    sendObject res
                | :? Response_NewPlottable as res -> sendObject res
                | :? Response_Export as res -> sendObject res
                | :? Response_Program as res -> sendObject res
                | :? Response_StateSpace as res -> sendObject res
                | :? Response_SimTable as res -> sendObject res
                | :? Response_Probabilities as res -> sendObject res
                | _ -> failwith (sprintf "unmatched type %s" (msg.GetType().Name))
            match exc with None -> () | Some e -> sendObject { mtype = "error"
                                                               error = { message = e.Message; positions = None } }
