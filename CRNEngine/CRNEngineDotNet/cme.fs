// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Oslo
open Microsoft.FSharp.NativeInterop
open Microsoft.Research.CRNEngine.Sundials.external_sundials

#nowarn "9" //Native calls

[<JavaScript>]
type Cme = 
  {
    name: string;
    simulator: Simulation<Functional>;
    settings: Deterministic_settings;
    statespace: Ctmc ref;
    probabilities: Probabilities;
  }
  static member empty = {
    name = "None"
    simulator = Simulation.empty ();
    settings = Deterministic_settings.defaults; 
    statespace = { Ctmc.graph = Dictionary.empty(); Ctmc.initial_state = Map.empty} |> ref;
    probabilities = Probabilities.empty
  }
  static member get_times (cme:Probabilities) : float list = cme.times
  static member create 
    (populations:Populations<Species, float>) 
    (events:Event<Species, float, float> list) 
    (s:Deterministic_settings) 
    (sim_settings:Simulation_settings<Functional>) 
    (scale:float) =
    { Cme.empty with simulator = Simulation.create populations events sim_settings scale; settings = s }
  #if JavaScript
  static member consoleWrite (s:string) = WebSharper.JavaScript.Console.Log s
  #else
  static member consoleWrite (s:string) = System.Console.WriteLine s
  #endif
  static member simulate_callback 
    (env           : Environment.t)
    (ratesEnv      : Map<string,Expression.t<Inlined<Species>>>)
    (cancel_flag   : bool ref) 
    (output        : Row<Point> -> unit) 
    (console_print : bool) 
    (cme:Cme) =
    let simulator = cme.simulator 
    let speciesToPopIndex = simulator.populations.find_index
    let keyInliner = Key.inline_keys env ratesEnv >> Expression.map (Inlined.map speciesToPopIndex)
    let printspecies : Expression.t<Inlined<int>> [] =
      simulator.settings.plots
      |> Array.ofList
      |> Array.map (Expression.expand keyInliner >> Expression.simplify)
    let events = simulator.events 
    if events |> List.exists (fun ev -> match ev.target with Target.OutputPoint _ -> false | _ -> true) 
    then failwith "Events not supported for CME integration";  
    if console_print then Cme.consoleWrite("Converting state-space to sparse matrix for CME integration");
    //TODO: (perf) This causes the full matrix to manifest which is not really needed. Adjust this and the below datapoint() function CG
    let (transitionMatrix,stoichiometry,P0) = 
      (!cme.statespace).statespace_for_CME_integration env (simulator.populations.index_to_species |> Array.length) 
    let stoichiometry_element_squared =
      Matrix.ofRows (Array.init (stoichiometry |> Matrix.numRows) 
       (fun i -> 
         Array.init (stoichiometry |> Matrix.numCols) (fun j -> System.Math.Pow (stoichiometry.[i,j], 2.0)) |> Vector.ofArray)) 
    (* Set up ODE solver enumerator *)
    if console_print then Cme.consoleWrite("Integrating CME for probabilities");
    let f (_:float) = Vector.toArray >> transitionMatrix.timesRight >> Vector.ofArray // TODO: avoid the copying
    (* Output function *)
    let times = ref []
    let probs = ref []
    let datapoint (time:float) (pr:Vector) = 
      times := time :: !times
      probs := pr :: !probs
      let get_mean i =
        let species_col = Matrix.copyCol stoichiometry i 
        Vector.dot pr species_col 
      let get_stdev mean i = 
        let species_col = Matrix.copyCol stoichiometry_element_squared i 
        let var = Vector.dot pr species_col - mean*mean // TODO: check row vs column
        // Protecting against numerical error creating negative variance (especially problematic when distribution has stabilised)
        if var < 0.0 then 0.0 else System.Math.Sqrt var      
      printspecies
      |> Array.map
          (fun spec -> 
            match spec with
            | Expression.Key (Inlined.Species x) ->
              let mean = get_mean x
              { Point.mean = mean; Point.stdev = get_stdev mean x }
            | _ -> failwith "Species functions are not supported in plots (use single species only)."
          ) 
    let integrator = Oslo_integrator.create cme.name cme.simulator datapoint Point.interpolate cme.settings P0 f
    let updated_integrator = Oslo_integrator.simulate_callback cancel_flag output integrator
    { cme with 
        simulator = updated_integrator.simulator
        probabilities = 
        { 
          times = !times |> List.rev
          stateprobabilities = !probs |> Lib.rev_map Vector.toArray
          stoichiometry = Matrix.toArray stoichiometry
          species = simulator.populations.species_to_index |> Hashtable.map_key (fun s -> s.name) 
        }
    }
  member cme.process_simulation sim_data = 
    let times:float list = cme.simulator.settings.times 
    let plots:string list = List.map Functional2.to_string_plot cme.simulator.settings.plots 
    if times = [] 
    then Table.from_rows plots sim_data
    else 
      let int_data:Row<Point> list = Row.interpolate_reverse Point.interpolate sim_data times 
      Table.from_rows plots int_data
  static member simulate 
    (env            : Environment.t) 
    (ratesEnv       : Map<string,Expression.t<Inlined<Species>>>)
    (console_print  : bool) 
    (cme            : Cme) =
    let cancel = ref false 
    let result = ref [] 
    let output row = result := row::!result 
    let final_sim = Cme.simulate_callback env ratesEnv cancel output console_print cme 
    final_sim, cme.process_simulation (List.rev !result)
  
  /// Simulate with Sundials integrator.
  member s.simulate_sundials_callback (cancel_flag:bool ref) env ratesEnv (output:Row<Point>->unit) =   
    //We could have a separate create method as in the Oslo ode.
    let simulator = s.simulator
    let sim_settings = simulator.settings
    let times_list = 
      if sim_settings.times <> [] 
      then sim_settings.times
      else 
        let starttime = sim_settings.initial
        let printinterval = sim_settings.get_print_interval()
        // ND: Make the time vector one larger than requested, to ensure the final time is included.
        List.init (sim_settings.points+1) (fun index -> starttime + float index * printinterval)

    // Prevent events
    let events = simulator.events 
    if events |> List.exists (fun ev -> match ev.target with Target.OutputPoint _ -> false | _ -> true) 
    then failwith "Events not supported for CME integration";  

    // Define problem matrices
    let ((propensities, targets, sources, diagonals), stoichiometry, P0) = 
      (!s.statespace).statespace_for_CME_sundials env (simulator.populations.index_to_species |> Array.length) 
    let stoichiometry_element_squared =
      Matrix.ofRows (Array.init (stoichiometry |> Matrix.numRows) 
       (fun i -> 
         Array.init (stoichiometry |> Matrix.numCols) (fun j -> System.Math.Pow (stoichiometry.[i,j], 2.0)) |> Vector.ofArray)) 

    let times:float[] = Array.ofList times_list      
    let num_states = Array.length P0

    (* Prepare output channel *)
    let numtimes = times_list.Length      
    let outer_solution = System.Collections.Generic.List<Vector>(numtimes)
#if JavaScript
    let sundialsOutput (time:float) (outputdata:float array) =
#else
    let sundialsOutput (time:float) (trans_prob:nativeptr<float>) =
      let outputdata = Array.init num_states (NativePtr.get trans_prob)
#endif
      let v = Oslo.Vector.ofArray(outputdata)
      outer_solution.Add(v);
      () 

    let sundialsOutputDelegate = new SundialsOutput(sundialsOutput)      
    let abstol = s.settings.abstolerance
    let reltol = s.settings.reltolerance
    let stiff = s.settings.stiff

    // During this function, output() will be called back into concs will be changed inside here
    let num_transitions = Array.length propensities 
#if JavaScript
    // we need explicit return of "prob_mass" here
    let sundialsOutputCode, P0  =
#else
    let sundialsOutputCode =
#endif
        fnSundialsCMESolver(num_states, num_transitions, propensities, sources, targets, diagonals, P0, times, numtimes, sundialsOutputDelegate, stiff, abstol, reltol)     
    if sundialsOutputCode <> 0 then failwithf "Sundials solver failed with code: %i" sundialsOutputCode;
    let probs = List.ofSeq outer_solution

    (* Output function *)
    let speciesToPopIndex = simulator.populations.find_index
    let keyInliner = Key.inline_keys env ratesEnv >> Expression.map (Inlined.map speciesToPopIndex)
    let printspecies : Expression.t<Inlined<int>> [] =
      simulator.settings.plots
      |> Array.ofList
      |> Array.map (Expression.expand keyInliner >> Expression.simplify)
    
    let datapoint t (pr:Vector) = 
      let get_mean i =
        let species_col = Matrix.copyCol stoichiometry i 
        Vector.dot pr species_col 
      let get_stdev mean i = 
        let species_col = Matrix.copyCol stoichiometry_element_squared i 
        let var = Vector.dot pr species_col - mean*mean // TODO: check row vs column
        // Protecting against numerical error creating negative variance (especially problematic when distribution has stabilised)
        if var < 0.0 then 0.0 else System.Math.Sqrt var      
      { time = t
      ; values = 
        printspecies
        |> Array.map
            (fun spec -> 
              match spec with
              | Expression.Key (Inlined.Species x) ->
                let mean = get_mean x
                { Point.mean = mean; Point.stdev = get_stdev mean x }
              | _ -> failwith "Species functions are not supported in plots (use single species only)."
            ) 
      }
    let probabilities = 
        { 
          times = times_list
          stateprobabilities = probs |> List.map Vector.toArray
          stoichiometry = Matrix.toArray stoichiometry
          species = simulator.populations.species_to_index |> Hashtable.map_key (fun s -> s.name) 
        }
    let meanstd = List.map2 datapoint times_list probs
    List.iter output meanstd
    
    { s with probabilities = probabilities }, s.process_simulation meanstd  
  member s.simulate_sundials env ratesEnv =
    let cancel = ref false
    let output = fun row -> ()
    s.simulate_sundials_callback cancel env ratesEnv output