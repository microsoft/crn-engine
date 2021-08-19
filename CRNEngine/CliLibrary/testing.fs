module Microsoft.Research.CliLibrary.Testing
open Microsoft.Research.CRNEngine
open Microsoft.Research.CliLibrary
open Microsoft.Research.CRNEngine.JSAPI
open System
open System.IO
open System.Threading
open Argu

type Result = Skipped of string | Unchecked of string | Success of string | Failure of string

let printResult result =
    printf " - "
    Console.ForegroundColor <- match result with Skipped _ | Unchecked _ -> ConsoleColor.Yellow | Success _ -> ConsoleColor.Green | Failure _ -> ConsoleColor.Red
    printf (match result with Skipped _ -> "SKIPPED" | Unchecked _ -> "NOT CHECKED" | Success _ -> "SUCCESS" | Failure _ -> "FAILURE")
    Console.ResetColor()
    printfn "%s" (match result with Skipped msg | Unchecked msg | Success msg | Failure msg -> sprintf " (%s)" msg)

type CliArguments =
| Models of string
| Observations of string
| Results of string
| Timeout of int
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Models _       -> "Specify the models folder."
      | Observations _ -> "Specify the observations folder (for inference)."
      | Results _      -> "Specify the results folder (for validating results)."
      | Timeout _      -> "Specify a timeout (in seconds)."

/// The name of the model that's currently being run. I maintain this here so the watchdog thread can get at it if needed.
let mutable currentModel = ""

let simulate (model:Model) =
    let crns = if List.isEmpty model.systems then [model.top] else model.systems
    let sim_crn (crn:Crn) = 
       let runs,_ = crn.to_simulation_runs()
       let sim_instance (i:Instance<Functional>) = 
         let crn = {crn with settings = crn.settings.update_simulation i.settings}
         let crn = crn.substitute i.environment
         let result = Simulation.simulate_crn_callback false crn
         Simulation.simresult_to_floats result
       let tables = List.map sim_instance runs
       tables
    let alltables = List.collect sim_crn crns
    alltables

/// True if a and b are significantly different.
let compareEpsilon epsilon a b = a > b + epsilon || a < b - epsilon

let compareResults valueType (myResult:Table<float>) (goodResults:Table<float>) =
    /// Margin for floating point comparison.
    let compareEpsilon = compareEpsilon 10e-12
    /// Compares two series.
    let compareColumns name (my:float list) (good:float list) =
        if my.Length <> good.Length then failwith (sprintf "Length of column %s does not match. Results: %d, check: %d" name my.Length good.Length)
        List.iteri2 (fun i my good -> if compareEpsilon my good then failwith (sprintf "At position %d in column %s: result = %f, check = %f" i name my good)) my good
    /// Compares time series for this couple of results.
    let compareTimes () =
        let myTimes = myResult.times
        let goodTimes = goodResults.times
        compareColumns "Time" myTimes goodTimes
    /// Default comparison procedure for this couple of results (for regular float types of results).
    let defaultCompare () =
        if goodResults.columns.Length <> myResult.columns.Length then failwith (sprintf "Results have %d columns, known results have %d columns." myResult.columns.Length goodResults.columns.Length)
        List.iter2 (fun (my:Column<float>) (good:Column<float>) -> compareColumns my.name my.values good.values) myResult.columns goodResults.columns
    // Determine how to compare results. For some simulator types, the CSV form may not be the same as the output.
    match valueType with
    | MeanStdev
    | MeanStdevProbabilities ->
        compareTimes()
        // Determine if the good results are in the form of (value,low,high). This is the form that is produced by the GUI tools for the table viewer. It is acceptable for known-good results to be in this form, but they'll need to be reshaped. If they are not in this form, then they need to be in the same form as the output.
        if goodResults.columns.Length = myResult.columns.Length / 2 * 3 then
            // Results are in (value,low,high) form. They can still be compared, but need some reshaping.
            let compareMeanStDev idx =
                let meanCol = myResult.columns.[idx]
                let stDevCol = myResult.columns.[idx+(myResult.columns.Length/2)]
                let goodMeanCol = goodResults.columns.[idx*3]
                compareColumns meanCol.name meanCol.values goodMeanCol.values
                let goodLowCol = goodResults.columns.[idx*3+1]
                let goodStDevValues = List.map2 (fun m l -> m-l) goodMeanCol.values goodLowCol.values
                compareColumns stDevCol.name stDevCol.values goodStDevValues
                let goodHighCol = goodResults.columns.[idx*3+2]
                let goodStDevValues = List.map2 (fun m h -> h-m) goodMeanCol.values goodHighCol.values
                compareColumns stDevCol.name stDevCol.values goodStDevValues
            [0..(myResult.columns.Length/2-1)] |> List.iter compareMeanStDev
        else
            defaultCompare()
    | Spatial2D ->
        // Determine if the good results contain the final state only. This is the form that is produced by the GUI tools for the table viewer. The know-good table will contain the Y coordinates in the times series, and the X coordinates in the column names. The results table, on the other hand, will contain a number of columns equal to the cartesian product of the X and Y sets of coordinates. If they are not in this form, then they need to be in the same form as the output.
        if myResult.columns.Length = goodResults.columns.Length * goodResults.times.Length then
            let xlen = goodResults.columns.Length
            let ylen = goodResults.times.Length
            let myColsA = List.toArray myResult.columns
            let goodColsA = List.toArray goodResults.columns
            for x = 0 to xlen-1 do
                let goodCol = goodColsA.[x]
                for y = 0 to ylen-1 do
                    let my = List.last myColsA.[x*ylen+y].values
                    let good = goodCol.values.[y]
                    if compareEpsilon my good then failwith (sprintf "At position (%d,%d): result = %f, check = %f" x y my good)
                done
            done
        else
            defaultCompare()
    | _ ->
        defaultCompare()

let checkSimResult resultsFolder fileName valueType (tables:Table<float> list) =
    let check myResult baseName =
        // Search for the known results file. Might be CSV or TSV.
        let resultsFileName = Path.Combine (resultsFolder, baseName + ".csv")
        if File.Exists resultsFileName then
            let resultsTextCsv = File.ReadAllText resultsFileName
            let goodResult = Table<_>.parse_csv resultsTextCsv
            compareResults valueType myResult goodResult
            Success "sim results checked against CSV"
        else
            let resultsFileName = Path.Combine (resultsFolder, baseName + ".tsv")
            if File.Exists resultsFileName then
                let resultsTextTsv = File.ReadAllText resultsFileName
                let goodResult = Table<_>.parse_tsv resultsTextTsv
                compareResults valueType myResult goodResult
                Success "sim results checked against TSV"
            else
                Unchecked "no results file found"
    let baseName = Path.GetFileNameWithoutExtension fileName
    match tables with
    | [myResult] ->
        let result = check myResult baseName
        printResult result
    | results ->
        // Generate a single meaningful row of text as output.
        let results = List.mapi (fun i r -> let name = sprintf "%s.%d" baseName i in check r name,name) results
        if not (List.exists (fun (r,n) -> match r with Success _ -> false | _ -> true) results) then
            Success "sim results checked against multiple files" |> printResult
        else
            match List.tryFind (fun (r,_) -> match r with Failure _ -> true | _ -> false) results with
            | Some (Failure r,n) -> Failure (sprintf "%s: %s" n r) |> printResult
            | _ ->
                match List.tryFind (fun (r,_) -> match r with Unchecked _ -> true | _ -> false) results with
                | Some (Unchecked _,n) ->
                    if not (List.exists (fun (r,n) -> match r with Unchecked _ -> false | _ -> true) results) then
                        Unchecked "no results file found" |> printResult
                    else
                        Unchecked (sprintf "no results file found for %s" n) |> printResult
                | _ -> ()

let simExternal resultsFolder (fileName:string) (simulate:unit->Table<float> list) =
    let tables = simulate ()
    checkSimResult resultsFolder fileName JSAPI.ValueType.Float tables

let simIG resultsFolder (fileName:string) (ig:InferenceSiteGraph.IGraph) =
    let simIgNode model =
        let crns = if List.isEmpty model.systems then [model.top] else model.systems
        let simCrn (crn:Crn) = 
            let runs,_ = crn.to_simulation_runs()
            let sim_instance (i:Instance<Functional>) = 
                let crn = {crn with settings = crn.settings.update_simulation i.settings}
                let crn = crn.substitute i.environment
                let result = Simulation.simulate_crn_callback false crn
                Simulation.simresult_to_floats result
            let tables = List.map sim_instance runs
            let valueType = simulator_to_value_type crn.settings
            checkSimResult resultsFolder fileName valueType tables
        List.iter simCrn crns
    ig.nodes |> Map.toSeq |> Seq.map (fun (_,v) -> v) |> Seq.iter simIgNode

let inferIG (observationsFolders:string[]) resultsFolder (fileName:string) (ig:InferenceSiteGraph.IGraph) =
    /// Margin for floating point comparison.
    let compareEpsilon = compareEpsilon 10e-12
    let addData (model:Model) = 
        let loadData settings =
            let tryLoadData settings folder =
                try
                    Io.load_data folder settings |> Some
                with _ -> None
            match Array.tryPick (tryLoadData settings) observationsFolders with Some settings -> settings | None -> failwith "Unable to find required observation files"
        { model with
            systems = model.systems |> List.map (fun crn -> { crn with settings = loadData crn.settings});
            top = {model.top with settings = loadData model.top.settings}  }
    let ig = InferenceSiteGraph.mapNodes (fun _ m -> addData m) ig
    let inferModel (model:Model) =
        // Note: I'm using infer_seq instead of infer because they don't give the same results. This is a bug that needs to be addressed. After that is done, this should use infer, or just use InferenceSiteGraph.infer instead of InferenceSiteGraph.inferWith.
        let mutable result = None
        let results,_ = model.infer_seq (fun f -> result <- Some f)
        List.ofSeq results |> ignore
        let summary = result.Value.to_summary()
        summary
    let summaries = InferenceSiteGraph.inferWith inferModel ig |> List.map (fun (_,(_,s)) -> s)
    match summaries with
    | [summary] ->
        let parseResults (delim:char) (s:string) =
            let str = s.Split('\r','\n') |> Array.filter (fun s -> s <> "")
            if str.Length <= 1 then failwith "wrong format of inferred parameters file"
            let parseRow i (row:string) =
                let cells = row.Split(delim)
                if cells.Length = 6 then
                    (cells.[0], float cells.[3])
                else if cells.Length = 2 then
                    (cells.[0], float cells.[1])
                else
                    failwith (sprintf "wrong format of inferred parameters file at row %d" i)
            str.[1..] |> Array.mapi parseRow |> Map.ofArray
        let compareResults (summary:Inference.Summary) goodParameters =
            let checkParameter name value =
                let myParameter = match summary.parameters.TryFind name with Some parameter -> parameter | None -> failwith (sprintf "could not find parameter %s in results" name)
                if compareEpsilon myParameter.mle value then failwith (sprintf "value for parameter %s: result = %f, check = %f" name myParameter.mle value)
            goodParameters |> Map.iter checkParameter
        let parametersFileName = Path.Combine (resultsFolder, (Path.GetFileNameWithoutExtension fileName) + ".csv")
        if File.Exists parametersFileName then
            let parametersCsv = File.ReadAllText parametersFileName
            let goodParameters = parseResults ',' parametersCsv
            compareResults summary goodParameters
            Success "inferred parameters checked against CSV" |> printResult
        else
            let resultsFileName = Path.Combine (resultsFolder, (Path.GetFileNameWithoutExtension fileName) + ".tsv")
            if File.Exists resultsFileName then
                let parametersTsv = File.ReadAllText resultsFileName
                let goodParameters = parseResults '\t' parametersTsv
                compareResults summary goodParameters
                Success "inferred parameters checked against TSV" |> printResult
            else
                Unchecked "no inferred parameters file found" |> printResult
    | _ -> Unchecked "cannot compare inference results for multi-result models" |> printResult

/// Returns true in case of failure.
let runModel (getExternalSimulation:string->(unit->Table<float> list) option) (parser:string->InferenceSiteGraph.IGraph) (modelsFolder:string) (observationsFolders:string[]) resultsFolder fileName : bool =
    currentModel <- fileName
    let relativeFileName = fileName.Substring(modelsFolder.Length+1)
    printf "Running: \"%s\"" relativeFileName
    let resultsFolder = System.IO.Path.Combine(resultsFolder,System.IO.Path.GetDirectoryName(relativeFileName))
    try
        let text = File.ReadAllText fileName
        if text.Contains "no_auto_test" then
            Skipped "marked with no_auto_test" |> printResult
        else
            let externalSimulation = getExternalSimulation text
            if externalSimulation.IsSome then
                simExternal resultsFolder fileName externalSimulation.Value
            else
                let ig = parser text
                if ig.task.IsSome && ig.task.Value.task_type = Some TaskType.Parse then
                    Skipped "parse only" |> printResult
                else
                    // Make it quiet, we don't want to produce much output here. Also, ensure the random seed is set.
                    let tweak_simulation simulation = match simulation.seed with None -> { simulation with seed = Some 0 } | _ -> simulation
                    let tweak_settings settings = { settings with quiet = true
                                                                  inference = { settings.inference with print_console = false; print_summary = false }
                                                                  simulation = tweak_simulation settings.simulation
                                                                  simulations = List.map tweak_simulation settings.simulations }
                    let ig = InferenceSiteGraph.mapNodes (fun _ model -> model.map_crns (fun crn -> { crn with settings = tweak_settings crn.settings })) ig
                    let ig = InferenceSiteGraph.expandAndLift ig
                    if text.Contains "directive inference" then
                        inferIG observationsFolders resultsFolder fileName ig
                    else
                        simIG resultsFolder fileName ig
        false
    with e ->
        Failure e.Message |> printResult
        true

/// Returns the number of failed models.
let run getExternalSimulation parser modelsFolder observationsFolders resultsFolder timeout =
    // Create a thread that will throw an exception if the program does not finish within the timeout.
    let finished = new AutoResetEvent false
    let watchDog () =
        Thread.CurrentThread.IsBackground <- true
        if not (finished.WaitOne (timeout*1000)) then failwith (sprintf "Timeout exceeded while running %s" currentModel)
    (new Thread(new ThreadStart(watchDog))).Start()
    
    let allModels = Directory.EnumerateFiles (modelsFolder, "*.*", SearchOption.AllDirectories)
    let failures = Seq.filter (fun f -> runModel getExternalSimulation parser modelsFolder observationsFolders resultsFolder f) allModels |> Seq.length
    
    // Stop the watchdog thread.
    finished.Set() |> ignore
    
    failures

let testValidate getExternalSimulation parser args = 
    let argParser = ArgumentParser.Create<CliArguments>()
    let parserResults = argParser.Parse(args)
    let modelsFolder = match parserResults.TryGetResult Models with Some models -> models | None -> "."
    let observationsFolder = match parserResults.TryGetResult Observations with Some observations -> observations | None -> "."
    let observationsFolders = observationsFolder.Split(',')
    let resultsFolder = match parserResults.TryGetResult Results with Some results -> results | None -> "."
    let timeout = match parserResults.TryGetResult CliArguments.Timeout with Some timeout -> timeout | None -> 3600
    let modelsFolder = Path.GetFullPath(modelsFolder);
    let observationsFolder = Path.GetFullPath(observationsFolder);
    let resultsFolder = Path.GetFullPath(resultsFolder);
    printfn "Models folder: %s" modelsFolder
    printfn "Observations folder(s): %s" observationsFolder
    printfn "Results folder: %s" resultsFolder
    let failures = run getExternalSimulation parser modelsFolder observationsFolders resultsFolder timeout
    failures