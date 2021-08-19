module Microsoft.Research.CliLibrary.Program

open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngineCloudLib
open System.IO
open Argu

type key = string

let string_of_file s = 
  Io.println ("Loading file: " + s)
  Io.read_file s

let crn_of_file s =
  let s = string_of_file s
  let crn = Parser.from_string Crn.parse s
  crn
  // TODO: Apply settings updates
  //let settings = Lib.fold_right ParseResult.apply_settings_update settings_updates crn.settings in
  //{ crn with settings = settings }

(*let Gui_of_file settings_updates s =
  let g = Gui.empty |> Gui.setWeakTypingOption true |> Gui.setLBSProgramText (string_of_file s) |> Gui.solve (ref false) in
  let settings = Lib.fold_right ParseResult.apply_settings_update settings_updates (Gui.getSimSettings g) in
  Gui.setSimSettings settings g*)

type exports =
  { mutable text   : bool
  ; mutable dot    : bool
  ; mutable crn    : bool
  ; mutable sbml   : bool
  ; mutable states : bool }

(* Command line interface logic. *)
type argspec = Bool of bool ref
type spec = (string * argspec * string) list

type CliArguments = 
  | [<MainCommand; Last>] File of file:string
  | Step of float
  | Multicore
  | States
  | Profile
  | Synthesize
  | Inference of string
  | Simulate
  | SimulateUncertainty of int
  | Simulations of int
  | Seed of int
  | Simulator of string
  | Moments of int*float
  | Horizontal
  | Summary
  | Progress
  | Text
  | Dot
  | Sbml
  | Pool of string
  | Jobs
  | Stop of string
  | Delete of string
  | Results of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | File _        -> "Specify the file to use."
      | Step _        -> "Set reporting step for simulator (every n%)."
      | Multicore     -> "Process different simulations using multiple cores (if possible)."
      | States        -> "Compute state space (CTMC)."
      | Profile       -> "Enable profiling output."
      | Synthesize    -> "Run synthesis method."
      | Inference _   -> "Infer parameters of CRN(s)."
      | Simulate      -> "Do a simulation."
      | SimulateUncertainty _ -> "Do multiple simulations, sampling parameter values from their prior distributions."
      | Simulations _ -> "Set the number of simulation runs to perform."
      | Seed _        -> "Set the seed for stochastic simulation or parameter inference."
      | Simulator _   -> "Select a simulator mode - 'Stochastic', 'Deterministic', 'DeterministicStiff', 'JIT', 'Sundials', 'SundialsStiff', 'Cme', 'CmeSundials' or 'CmeSundialsStiff'."
      | Moments _     -> "Moment closure approximation with specified order and initial minimum."
      | Horizontal    -> "Write simulation output horizontally."
      | Summary       -> "Write output files with summary statistics (mean, std) instead of blocks of instances."
      | Progress      -> "Print progress to console"
      | Text          -> "Produce a text representation of the program."
      | Dot           -> "Produce a DOT representation of the program."
      | Sbml          -> "Produce a SBML representation of the program."
      | Pool _        -> "Name of the cloud pool to use."
      | Jobs          -> "Lists existing cloud jobs."
      | Stop _        -> "Stops the cloud job with the given ID."
      | Delete _      -> "Deletes the cloud job with the given ID."
      | Results _     -> "Retrieves results for the cloud job with the given ID."

let printJobs () =
    Io.println "Retrieving jobs..."
    let jobs = AzureJobsManagement.getJobs true false false
    Io.println ("-------------------------------------------------------------------------")
    Io.println (sprintf "%-30s %-20s %-10s %-10s" "Job ID" "Started" "Type" "State")
    Io.println ("-------------------------------------------------------------------------")
    let printJob (job:DataStructures.JobDescriptor) =
      Io.println (sprintf "%-30s %-20s %-10s %-10s" job.id (job.start.ToString("dd/MM/yy HH.mm.ss")) job.verb (job.state.ToString()))
    List.iter printJob jobs
    Io.println ("-------------------------------------------------------------------------")

let resultsJob id =
    Io.println (sprintf "Retrieving results of job %s..." id)
    try
      let stream = AzureJobsManagement.getFile true id "results.zip"
      match stream with 
      | None -> Io.println "The specified job is not completed."
      | Some stream ->
        let fname = sprintf "%s.results.zip" id
        let fullname = System.IO.Path.GetFullPath fname
        Io.println (sprintf "Downloading %d bytes to %s" stream.Length fullname)
        use file = new FileStream(fullname, FileMode.Create)
        stream.CopyToAsync(file).Wait()
        file.Flush()
    with e ->
      Io.println e.Message
      Io.println "The specified job could not be accessed, or does not exist."

let hasSimulate (parser_results:ParseResults<CliArguments>) =
  parser_results.Contains Simulate || parser_results.Contains SimulateUncertainty

let getFileOptions (programName:string) (parser_results:ParseResults<CliArguments>) : bool * Simulation.outputFileOptions =
    let jsonMode = programName.EndsWith(".json")
    let options : Simulation.outputFileOptions = 
      { name = programName
      ; horizontal = parser_results.Contains Horizontal
      ; summary = parser_results.Contains Summary
      ; progress = parser_results.Contains Progress
      ; json = jsonMode
      ; copy = None
      }
    jsonMode, options

let run parser (programName:string) (parser_results:ParseResults<CliArguments>) =
    let profile = parser_results.Contains Profile
    let simulate = hasSimulate parser_results
    let uncertainty = parser_results.GetResult(SimulateUncertainty, defaultValue = 0)
    let synthesize = parser_results.Contains Synthesize
    let simulations = parser_results.TryGetResult Simulations
    let seed = parser_results.TryGetResult Seed
    let infer = parser_results.Contains Inference
    let multicore = parser_results.Contains Multicore
    let pool = parser_results.TryGetResult Pool
    let text = parser_results.Contains Text
    let jsonMode, options = getFileOptions programName parser_results
   
    // Compilation
    let programCode = string_of_file programName
    (*let possible_model, errors = Parser.from_string_find_errors Model.parse programCode

    if possible_model.IsNone then
        let errors = errors |> Seq.map (fun error -> sprintf "Line %i, column %i: %s" error.row error.column error.text) |> (String.concat System.Environment.NewLine)
        failwithf "Failed to parse: %s%s%s" programName System.Environment.NewLine errors
      
    let model = possible_model.Value*)
      
    let ig     = if jsonMode then
                    (WebSharper.Json.Deserialize<GuiIG> programCode).to_ig()
                 else
                    parser programCode
    let igraph = InferenceSiteGraph.expandAndLift ig
      
    let infer = match igraph.task with None -> infer | Some task -> task.task_type = (Some TaskType.Infer)
    let simulate = match igraph.task with None -> simulate | Some task -> task.task_type = (Some TaskType.Simulate)
    let copies = match igraph.task with None -> 1 | Some task -> task.copies
    let copy_id = match igraph.task with None -> 1 | Some task -> task.copy_id
    let uses_task = igraph.task.IsSome
      
    for c = copy_id to (copies+copy_id-1) do
        if uses_task then Io.println ("Copy " + (string c))
        let options = if uses_task then { options with copy = Some c } else options
        // Profile
        if profile 
        then 
          igraph.nodes |> Map.iter (fun name model ->
            Io.println ("Model: " + name);
            model.top::model.systems |> List.iter (fun (crn) ->
              Io.println ("System: " + crn.name);
              Io.println ("Total number of species: " + (string (crn.initials |> List.length)));
              Io.println ("Total number of reactions: " + (string (crn.reactions |> List.length)))
            )
          )
      
        // Update settings in model
        let updated_igraph = 
            igraph
            |> InferenceSiteGraph.mapNodes (fun _ model ->
                let settings = model.top.settings
                let stochastic = { settings.stochastic with trajectories = match simulations with Some s -> s | None -> settings.stochastic.trajectories }
                let simulation = match seed with Some s -> { settings.simulation with seed = Some s } | None -> settings.simulation
                let updated_settings = 
                    { model.top.settings with stochastic = stochastic; simulation = simulation }
                    |> fun s -> 
                        match parser_results.TryGetResult Simulator with 
                        | Some sim -> { s with simulator = Parser.from_string Simulator.parse sim }
                        | None -> s
                    |> fun s -> 
                        if multicore 
                        then { settings with simulation = { settings.simulation with multicore = true }} 
                        else settings
                { model with top = {model.top with settings = updated_settings } }
            )
      
        // Exports (TODO: revive)
        (*let file = Path.GetFileNameWithoutExtension programName
        if parser_results.Contains Dot then
          model.systems |> List.iter (fun crn -> Export.export_reaction_graph_dot crn (sprintf "%s_%s" file crn.name));
        //if parser_results.Contains  Sbml @> then Export.export_sbml term id file !profile;
        if parser_results.Contains States then
          model.systems |> List.iter (fun crn -> Export.export_states_dot crn (sprintf "%s_%s" file crn.name));*)
      
        // Do moment closure?
        (*updated_igraph
        |> InferenceSiteGraph.iterNodes (fun _ model ->
            let do_momentclosure, (mc_settings : Moment_closure_settings.t<Moment_closure_settings.monomial>) = 
                match parser_results.TryGetResult Moments with
                | Some (order,init_min) -> true, {order = order; initial_minimum = init_min; log_evaluation = true; plots = []}
                | None -> model.top.settings.simulator = MC, model.top.settings.moment_closure
            if do_momentclosure
            then       
                (model.top :: model.systems)
                |> List.iter (fun crn -> 
                  let crn = crn.update_settings {crn.settings with moment_closure = mc_settings}
                  let fname = if model.systems.Length > 1 then sprintf "%s_%s" programName crn.name else programName
                  let moments = Moments.generateMoments crn
                  moments |> MC_Utils.to_string |> Io.write_file (sprintf "%s_all_moments_%d.txt" fname mc_settings.order)
                  let closure = moments |> Moments.generateClosure 
                  closure |> MC_Utils.to_string |> Io.write_file (sprintf "%s_closure_approximation_%d.txt" fname mc_settings.order)
      
                  // Currently don't support sweeps
                  if simulate 
                  then 
                    let programName = Path.GetFileNameWithoutExtension programName
                    let programDir = Path.GetDirectoryName programName
                    let outDir = Path.Combine(programDir, programName + "_simulation")
                    Directory.CreateDirectory outDir |> ignore
                    Moments.simulate closure 
                    |> snd 
                    |> Table.map (fun p -> p.mean)
                    |> Table<float>.to_string "\t" string 
                    |> Io.write_file (Path.Combine (outDir, sprintf "momentClosure_order%d.tsv" mc_settings.order))
                )
        )*)
      
        // Simulation
        if simulate
        then
          if Azure.enabled && pool.IsSome then
            printfn "Starting cloud simulation..."
            let (_,_,jobid) = AzureJobs.startSimulateOnAzure pool.Value updated_igraph
            printfn "Cloud simulation started with job ID = %s" jobid
          else
            printfn "Simulating..."
            updated_igraph 
            |> InferenceSiteGraph.iterNodes (fun _ model ->
              let sw = System.Diagnostics.Stopwatch()
              sw.Start()
      
              if uncertainty > 0
              then 
                let outDir = Simulation.prepareOutputDirectory programName (if uses_task then Some c else None)
                Simulation.run_list_multi uncertainty options model
                |> Simulation.write_multi outDir options model.top.settings
              else
                Simulation.run_list options model
      
              sw.Stop()
              printfn "Completed: %f seconds" ((float sw.ElapsedMilliseconds)/1000.0)
              printfn "---------------------------------"
            )

        // Inference
        if infer
        then
          let settings_igraph = updated_igraph |> InferenceSiteGraph.mapNodes (fun _ model ->
            //TODO: parse simulator settings
            //let inf_settings = match parser_results.TryGetResult Simulator with Some sim -> { inf_settings with simulator = Crn.simulator_of_string sim } | None -> settings
            let inf_settings = match parser_results.TryGetResult Seed with Some s -> { model.top.settings.inference with seed = uint32 s } | None -> model.top.settings.inference
            let global_settings = {model.top.settings with inference = inf_settings}
  
            if jsonMode then
                model
            else
                let dataDir = parser_results.GetResult Inference
                // Load the datasets that have been specified
                { model with
                      systems = model.systems |> List.map (fun crn -> { crn with settings = Io.load_data dataDir crn.settings});
                      top = {model.top with settings = Io.load_data dataDir global_settings} 
                }
          )
  
          if Azure.enabled && pool.IsSome then
            printfn "Starting cloud inference..."
            let (_,_,jobid) = AzureJobs.startInferOnAzure pool.Value settings_igraph
            printfn "Cloud inference started with job ID = %s" jobid
          else
            // Assign new Model.t and run inference
            let inf_options : Inference.options = { algorithm = Inference.MCMC; writeHtml = true; directoryMode = Inference.Seed }
            settings_igraph
            |> InferenceSiteGraph.inferWith (fun model ->
                let copy = if uses_task then Some c else None
                let baseName = Path.GetFileNameWithoutExtension programName
                let baseName = match copy with None -> baseName | Some copy -> (baseName+"_"+(string copy))
                let results = Inference.do_inference inf_options model (Path.Combine [|baseName + "_inference" ;model.top.name|]) copy programCode
                results.to_summary()
            ) |> ignore
        
        // Synthesis
        if synthesize
        then
          updated_igraph 
          |> InferenceSiteGraph.iterNodes (fun _ model ->
            Synthesis.run model
          )
        
        // Output CRN (useful when the CRN comes from DSD for example)
        if text
          then printf "%s" (ig.to_string ())
    done


let parse_args args =
  let arg_parser = ArgumentParser.Create<CliArguments>()
  // Ignoring unrecognized arguments here enables calling programs to extend the CRN parser with additional arguments.
  let parser_results = arg_parser.Parse(args, ignoreUnrecognized=true)
  arg_parser, parser_results 

let args_results = parse_args >> snd
let get_program_name (x:ParseResults<CliArguments>) = x.TryGetResult(File)

// Generic CLI program called by CRN & DSD (TODO: Point GEC here too)
let main parser args =
    Io.println ("Commit number "+Microsoft.Research.CRNEngine.Lib.commit_number)

    let arg_parser, parser_results = parse_args args 
    let jobCommand = parser_results.Contains Jobs || parser_results.Contains Stop || parser_results.Contains Delete || parser_results.Contains Results
    if (not (parser_results.Contains File)) && (not jobCommand) then
      let s = arg_parser.PrintCommandLineSyntax()
      Io.println "ERROR: no <file> specified."
      Io.println s
    else
      let programName = get_program_name parser_results
    
      if Azure.enabled then
        if parser_results.Contains Jobs then printJobs()
        match parser_results.TryGetResult Stop with Some j -> AzureJobsManagement.stopJob j | None -> ()
        match parser_results.TryGetResult Delete with Some j -> AzureJobsManagement.deleteJob j | None -> ()
        match parser_results.TryGetResult Results with Some j -> resultsJob j | None -> ()
    
      match programName with
      | None -> ()
      | Some programName ->
        run parser programName parser_results

    0