// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CliLibrary.Simulation

open System.IO
open Microsoft.Research.CRNEngine
open Microsoft.Research.Filzbach

type prediction = { model : string; sweep : string; results : Result<float> list }
type outputFileOptions = { name: string; horizontal: bool; summary: bool; progress: bool; json: bool; copy: int option }

let build_environment param n =
  let pn = List.find (fun (p:Parameters.Parameter) -> p.name = n) param
  pn.initValue

let simulate_ssa_env (e:Environment.t) (crn:Crn) = (crn.substitute e).to_ssa().simulate()
let simulate_sweeps (crn:Crn) = crn.to_ssa().simulate()

let env_to_string (e:Environment.t) = Lib.string_of_list (fun (k, v) -> k + " = " + (Lib.display_float v)) "_" (Map.toList e)

type simresult =
| Float of Table<float>
| Point of Table<Point>
| FloatArray of Table<float[]>*(float list)
| FloatArray2 of Table<float[][]>*(float list)*(float list)
  with
    member this.exportSimJson() =
        match this with
        | Float t -> Io.exportSimJson t
        | Point t -> Io.exportSimJson t
        | FloatArray (t,xs) -> Io.exportSimJson t
        | FloatArray2 (t,xs,ys) -> Io.exportSimJson t
  end

let simresult_to_floats sr =
    match sr with
    | Float t -> t
    | Point t -> Table<Point>.point_to_float t
    | FloatArray (t,xs) -> Table<float[]>.floatarray_to_float xs t
    | FloatArray2 (t,xs,ys) -> Table<float[][]>.floatarrayarray_to_float xs ys t

let simulate_crn_callback progress (crn:Crn) : simresult =
  let tfinal = crn.settings.simulation.final
  let console_frequency = 1.0 / (float crn.settings.simulation.points)
  let console_dt = tfinal * console_frequency
  let next_console_time = ref console_dt
  let console_function t = 
    if progress && (t > !next_console_time)
    then 
      printfn "  t = %1.3g" !next_console_time; 
      next_console_time := !next_console_time + console_dt
  let console_function_max t maxs = 
    if progress && (t > !next_console_time)
    then 
      printfn "  t = %1.3g, maxs = [%s]" !next_console_time (maxs |> Array.map (sprintf "%1.3g") |> String.concat "; ");
      next_console_time := !next_console_time + console_dt

  let cancel = ref false
  //let map_point result = { instance = result.instance; table = Table<float>.point_to_float result.table }
  match crn.settings.simulator with 
  | Simulator.SSA -> 
    let ssa = crn.to_ssa()
    ssa.simulate_action console_function |> Float
  | Simulator.Oslo ->       
    let result = ref []
    let output (row:Row<float>) = console_function row.time; result := row::!result
    let ode = crn.to_oslo()
    ode.simulate_callback cancel output ignore |> ignore
    ode.process_simulation !result |> Float
  | Simulator.Sundials -> 
    #if JavaScript 
    failwith "Sundials not supported in JavaScript"
    #else
    try 
      let ode = crn.to_ode()
      Ode.simulate_sundials ode |> Float // TODO: Make a callback function so that we can pass in cancel and output
    with | :? Errors.SimulatorErrorException -> crn.settings.parameters |> Parameters.to_string_inline |> failwithf "Sundials error: %s"
    #endif
  | Simulator.LNA -> 
    let result = ref []
    let output (row:Row<Point>) = console_function row.time; result := row::!result
    let lna = crn.to_lna()
    lna.simulate_callback cancel output |> ignore
    lna.process_simulation !result |> Point
  | Simulator.CME | Simulator.CMESundials -> 
    let result = ref []
    let output (row:Row<Point>) = console_function row.time; result := row::!result
    let ctmc = crn.to_ctmc ()
    if not crn.settings.quiet then printfn "Number of states: %d" (Ctmc.num_states ctmc.ctmc)
    if not crn.settings.quiet then printfn "Number of transitions: %d" (Ctmc.num_transitions ctmc.ctmc)
    let cme = crn.to_cme ctmc
    let paramEnv = Parameters.to_env crn.settings.parameters
    let ratesEnv = Crn.to_inlined_rates crn.settings
    if crn.settings.simulator = Simulator.CME
    then 
      Cme.simulate_callback paramEnv ratesEnv cancel output (not crn.settings.quiet) cme |> ignore
      cme.process_simulation (List.rev !result) |> Point
    else
      let _, result = cme.simulate_sundials_callback cancel paramEnv ratesEnv output 
      result |> Point
  | Simulator.PDE -> 
    match crn.settings.spatial.dimensions with 
    | 1 -> 
      let result = ref []
      let output (row:Row<float[]>) = 
        console_function_max row.time (row.values |> Array.map Array.max); 
        result := row::!result
      let pde = crn.to_pde1d()
      Pde<float[]>.simulate_1d_callback cancel output pde;
      let plots = pde.sim_settings.plots |> List.map Functional2.to_string_plot
      let xs = List.init pde.settings.nx (fun i -> (float i) * pde.settings.xmax / (float (pde.settings.nx-1)))
      let t = !result |> Table.from_rows_reverse plots
      FloatArray (t,xs)
    | 2 -> 
      let result = ref []
      let output (row:Row<float[][]>) = 
        console_function_max row.time (row.values |> Array.map (Array.map Array.max >> Array.max))
        result := row::!result
      let pde = crn.to_pde2d()
      Pde<float[][]>.simulate_2d_callback cancel output pde;
      let plots = pde.sim_settings.plots |> List.map Functional2.to_string_plot
      let xs = List.init pde.settings.nx (fun i -> (float i) * pde.settings.xmax / (float (pde.settings.nx-1)))
      let t = !result |> Table.from_rows_reverse plots 
      FloatArray2 (t,xs,xs)
    | _ -> failwith "Unknown number of dimensions"
  | Simulator.MC  -> 
      let moments = Moments.generateMoments crn
      let closure = moments |> Moments.generateClosure 
      Moments.simulate closure 
      |> snd 
      |> Point
 

let prepareOutputDirectory key copy =
  let programName = Path.GetFileNameWithoutExtension key
  let programDir = Path.GetDirectoryName key
  let baseDir = programName + "_simulation"
  let baseDir = match copy with None -> baseDir | Some copy -> baseDir + "_" + (string copy)
  let outDir = Path.Combine(programDir, baseDir)
  Directory.CreateDirectory outDir |> ignore
  outDir

let prepareSimulationDirectory outDir modelname sweep = 
  let simDir = 
    if modelname = "" 
    then if sweep = "" then outDir else Path.Combine (outDir,sweep)
    else if sweep = "" then Path.Combine (outDir, modelname) else Path.Combine (outDir, modelname + "_" + sweep)
  Directory.CreateDirectory simDir |> ignore
  simDir

let prepareFilenames settings instances = 
  let simulatorStr = 
    match settings.simulator with 
    | Simulator.PDE -> sprintf "pde%dd" settings.spatial.dimensions
    | _             -> settings.simulator.to_string
  
  if (List.length instances) = 1
  then [ simulatorStr + ".tsv" ]
  else 
    instances 
    |> List.mapi (fun i instance -> 
      if instance.environment.Count > 2
      then sprintf "instance_%d.tsv" i
      else env_to_string instance.environment + ".tsv"
    )

let write_multi outDir fileOptions settings (predictions:prediction list) = 
  let to_file = if fileOptions.horizontal then Io.to_file_horizontal else Io.to_file_vertical
  predictions
  |> List.iter (fun pred -> 
    let simDir = prepareSimulationDirectory outDir pred.model pred.sweep
    let filenames = 
      prepareFilenames settings (pred.results |> List.map (fun r -> r.instance))
      //pred.results |> List.map (fun r -> r.instance.name)
      |> Seq.map (fun f -> Path.Combine (simDir, f))
    let save fname r =
      to_file fname "\t" string r.table
      if fileOptions.json then Io.exportSimJson r.table
    pred.results |> Seq.iter2 save filenames
  ) 

/// Simulate all CRNs in the model then write to file
let run_list fileOptions (model:Model) = 
  let to_file = if fileOptions.horizontal then Io.to_file_horizontal else Io.to_file_vertical
  let outDir = prepareOutputDirectory fileOptions.name fileOptions.copy
  let crns = if List.isEmpty model.systems then [model.top] else model.systems
  crns
  |> List.iter (fun crn -> 
    let runs,_ = crn.to_simulation_runs()
    let filenames = prepareFilenames crn.settings runs
    let f (i:Instance<Functional>) fname = 
      let simDir = prepareSimulationDirectory outDir i.model i.sweep
      let crn = {crn with settings = crn.settings.update_simulation i.settings}
      let result = simulate_crn_callback fileOptions.progress (crn.substitute i.environment)
      to_file (Path.Combine (simDir, fname)) "\t" string (simresult_to_floats result)
      if fileOptions.json then result.exportSimJson()
    List.iter2 f runs filenames
  )

/// Simulate all CRNs in the model multiple times, sampling parameter values from the specified priors
let run_list_multi nsamples fileOptions (model:Model) = 
  let crns = if List.isEmpty model.systems then [model.top] else model.systems
  // Fix the times so that simulations can be aggregated
  let crns_fixed_times = 
    crns 
    |> List.map (fun crn -> 
      let sim_settings = crn.settings.simulation
      let times = 
        let starttime = sim_settings.initial
        let printinterval = sim_settings.get_print_interval()
        // ND: Make the time vector one larger than requested, to ensure the final time is included.
        List.init (sim_settings.points+1) (fun index -> starttime + float index * printinterval)
      { crn with settings = { crn.settings with simulation = crn.settings.simulation.update_times times } }
    )
      
  let rng = Rng.Random()  
  printfn "CRN: Sampling from the prior"
  let parameters = model.top.settings.parameters
  let groups_simulations = 
    List.init nsamples (fun i -> 
      printf "."
      let new_parameters = parameters |> Parameters.sample_from_prior rng
      let result = 
        crns_fixed_times
        |> List.collect (fun crn -> 
          { crn with settings = { crn.settings with parameters = new_parameters } }.simulate_async (fun crn -> simulate_crn_callback fileOptions.progress crn |> simresult_to_floats))
        |> List.groupBy (fun r -> (r.instance.model, r.instance.sweep))
        |> List.unzip
      if (i+1) % 100 = 0 then printfn "Completed %d samples" (i+1)
      result
    )
  printfn ""

  let groups, first_simulations = groups_simulations.Head
  let all_simulations = groups_simulations |> List.map snd 
  groups
  |> List.mapi (fun i (model,sweep) ->
    { model = model
    ; sweep = sweep
    ; results = 
      // For each instance
      List.init first_simulations.Head.Length (fun j ->
        if fileOptions.summary
        then
          all_simulations |> List.map (fun sims -> sims.[i].[j]) |> Result<float>.qsummary
        else
          all_simulations |> List.map (fun sims -> sims.[i].[j]) |> Result<float>.concat
      )
    }
  )
  
