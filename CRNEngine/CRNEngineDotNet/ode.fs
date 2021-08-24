// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open System.Diagnostics
open Microsoft.Research.Filzbach
[<JavaScript>]
[<DebuggerDisplay("{this.name}")>] // displays CRNs as strings in VS debugger
type Ode = 
  { 
  name: string;
  settings: Crn_settings<Functional>;
  reactions: Reaction<Species,Value,Functional> list;
  initials: Initial<Species,Value> list;
  }
  static member create name reactions initials settings = { 
    name = name
    settings = settings 
    reactions = reactions
    initials = initials 
  }
  static member empty = { 
    name = ""
    settings = Crn_settings<Functional>.defaults
    reactions = []
    initials = [] 
  }
  member ode.substitute (e:Environment.t) = {ode with settings = ode.settings.substitute e }
  static member update_times (times:float list) (ode:Ode) = {ode with settings = ode.settings.update_times times }
  static member get_matrices (initials:Initial<Species,Value> list) (reactions: Reaction<Species,'a,Functional> list) =   
    let species = initials |> List.distinctBy (fun i -> i.species) |> List.toArray // find species in initials (ignore duplicates)
    let species_ids = species |> Array.mapi (fun i s -> s.species, i) |> Map.ofArray // assign each species an index
    let numReactions = List.length reactions
    let numSpecies = Array.length species
    let stoich = Array.init numSpecies (fun _ -> Array.zeroCreate numReactions) // Create the stoichiometry matrix
    for s in 0..(numSpecies-1) do
      reactions |> List.iteri (fun ri r -> stoich.[s].[ri] <- r.getStoich species.[s].constant species.[s].species)
    done;
    let powers = reactions |> List.map (fun r -> Mset.collectm (fun entry -> (species_ids.[entry.element], (float) entry.multiplicity)) r.reactants) |> Array.ofList in // Create the powers array 
    species_ids, stoich, powers
  static member get_rates (species_ids:Map<Species,int>) (reactions: Reaction<Species,Value,Functional> list) convert_to_stochastic= 
    let fe = Expression.map (Key.map (fun s ->
      match Map.tryFind s species_ids with
      | Some x -> x
      | None -> failwithf "Missing species: %A in rate" s.name))
    reactions
    |> List.map (fun r -> 
      let fv = if convert_to_stochastic then r.deterministic_to_stochastic else id
      r.rate.map fv fe)
    |> Array.ofList // Create the rates array //NB check this. 
  member ode.to_oslo_abstract () =
    let species_ids, stoich, powers = Ode.get_matrices ode.initials ode.reactions  
    let rates = Ode.get_rates species_ids ode.reactions (ode.settings.simulation.kinetics=Kinetics.Stochastic)
    OsloAbstract.create ode.name ode.settings stoich powers rates species_ids ode.initials  
  member ode.to_oslo () = ode.to_oslo_abstract().evaluate()
  member ode.to_sundials_abstract () = // Updates the ode parameters with environment env
    let species = ode.initials |> Seq.distinctBy (fun i -> i.species) |> Seq.toArray // find species in initials (ignore duplicates)
    let species_ids = species |> Array.mapi (fun i s -> s.species, i) |> Map.ofArray // assign each species an index
    SundialsAbstract.create species_ids ode.reactions ode.initials ode.settings
  member ode.to_sundials () = ode.to_sundials_abstract().evaluate()  
  static member simulate_sundials (ode:Ode) =     
    try ode.to_sundials().simulate()
    with 
    | :? Errors.SimulatorErrorException as ex -> 
      let para_string = ode.settings.parameters |> Parameters.to_string_inline
      //printfn "Error in %s: %s" ode.name para_string
      raise (Errors.ode_solver_error (para_string |> sprintf "%s\n\nParameters: %s" ex.Message))
  member ode.to_lna scale = // Updates the ode parameters with environment env
    let species_ids, stoich, powers = Ode.get_matrices ode.initials ode.reactions  
    let rateDs = Ode.get_rates species_ids ode.reactions (ode.settings.simulation.kinetics<>Kinetics.Deterministic)
    let oslo = OsloAbstract.create ode.name ode.settings stoich powers rateDs species_ids ode.initials
    let e = Parameters.to_env ode.settings.parameters
    let initials = ode.initials |> Array.ofList |> Array.map (fun initial -> Expression.eval (Environment.find e) initial.value)
    Lna.initialise (oslo.evaluate ()) oslo.matrix initials scale
  member ode.to_simulation_runs () = ode.settings.to_simulation_runs ode.name 
  member ode.simulate_instance (simulate:Ode -> Table<'v>) (i:Instance<Functional>) = 
    //let ode = {ode with settings = ode.settings.update_simulation i.settings}
    try Result<_>.create i (simulate (ode.substitute i.environment))
    with 
    #if JavaScript 
    | e -> failwith (e.ToString())
    #else
    | :? Errors.SimulatorErrorException as ex -> raise (Errors.ode_solver_error (sprintf "%s\n\nInstance: %s" ex.Message (Environment.to_string i.environment)))
    #endif  
  member ode.simulate_instances (simulate:Ode -> Table<'v>) (instances:(Instance<Functional>) list) =
    List.map (ode.simulate_instance simulate) instances
  member ode.simulate (simulate:Ode -> Table<'v>) = ode.simulate_instances simulate (fst (ode.to_simulation_runs()))
  static member list_to_instances (odes:Ode list) = 
    odes 
    |> List.collect (fun (ode:Ode) -> 
      let instances, _ = ode.to_simulation_runs()
      instances |> List.map (fun i -> i, ode)
    )
    |> List.unzip
  static member list_simulate_instances (multicore:bool) (simulate:Ode -> Table<'v>) (instances:(Instance<Functional>) list) (odes:Ode list) =
    if multicore 
    then
      #if JavaScript
      failwith "Can't use Async in JavaScript (directive simulation {multicore=True})"
      #else
      let simulate_async (simulate:Ode -> Table<'v>) i (ode:Ode) = async { return (ode.simulate_instance simulate i)}
      List.map2 (simulate_async simulate) instances odes 
      |> Async.Parallel |> Async.RunSynchronously |> List.ofSeq
      #endif
    else List.map2 (fun i (ode:Ode) -> ode.simulate_instance simulate i) instances odes
  static member list_simulate (simulate:Ode -> Table<'v>) (global_settings:Crn_settings<Functional>) (odes:Ode list) = 
    let odes:Ode list = 
      let f (ode:Ode) = { ode with settings = { ode.settings with parameters = global_settings.parameters }}
      List.map f odes
    let instances, repl_odes = Ode.list_to_instances odes    
    Ode.list_simulate_instances global_settings.simulation.multicore simulate instances repl_odes  
  ///This function is a bit convoluted. Could be streamlined further, with care.
  ///Evaluate the sweeps and create corresponding instances
  static member generate_instances e (crn:string) (sweeps,simulations) =
      let l:(string * string * Environment.t * string) list = Sweeps.eval_label e sweeps
      let n_data = List.length simulations
      let n_instances = List.length l
      if (n_data <> n_instances) 
      then 
        let name,_,_,_ = l.Head
        raise (new Errors.EngineException (sprintf "Number of sweep items in %s (%d) not compatible with number of data columns (%d)" name n_instances n_data))
      else 
      let f (sw_name,assignment,e,iname) (s:Simulation_settings<Functional>) =
        Instance.create(crn,sw_name,assignment,e,s,iname)
      List.map2 f l simulations
  static member list_to_inference_method (global_settings:Crn_settings<Functional>) (odes:Ode list) =
    // Get the settings from the odes
    let l:Crn_settings<Functional> list = List.map (fun (o:Ode) -> o.settings) odes
    // Inline Multiple parameters and simulation times
    let parameters,tables_list,sweeps_list,simulations_list,_ = global_settings.to_inference_runs l
    // Inline constant parameters in sweeps
    let fixed_env = parameters |> List.partition Parameter.variable |> snd |> Parameters.to_env
    let inlined_sweeps = sweeps_list |> List.map (fun sweeps -> sweeps |> List.map (fun sw -> sw.inline_env fixed_env))
    // Get simulation parameters
    let inference_parameters = parameters |> Parameters.remove (List.collect Sweeps.get_variables inlined_sweeps) //Remove sweep parameters
    // Combine sweeps list and simulation settings list for further processing
    let l:(Sweep list * Simulation_settings<Functional> list) list = List.zip inlined_sweeps simulations_list
    let plottables = simulations_list |> List.collect (fun sims -> sims |> List.map (fun sim -> sim.plots))
    // Add global parameters to odes, not just inference parameters. This may contain new Multiple parameters
    let odes = 
      let f (ode:Ode) = { ode with settings = { ode.settings with parameters = parameters }}
      List.map f odes
    // Generate instances
    let initial_env = Parameters.to_env inference_parameters
    let instances_list = List.map2 (fun ode li -> Ode.generate_instances initial_env ode.name li) odes l
    let replicated_odes = 
      (odes, instances_list) 
      ||> List.map2 (fun ode is -> is |> List.map (fun i -> { ode with settings = ode.settings.update_simulation i.settings })) 
      |> List.concat
    let instances = List.concat instances_list
    let sweeps = List.concat sweeps_list
    // Prune if requested
    let pruned_odes = 
      if global_settings.simulation.prune
      then 
        let f (s:Sweep) = (s.merge).values |> List.map (fun vs -> List.zip s.merge.variables vs |> Map.ofList)
        let sweep_environments = List.collect f sweeps        
        let fixed_env_value = fixed_env |> Map.map (fun k v -> Value.Float v)
        (replicated_odes, sweep_environments)
        ||> List.map2 (fun ode env -> 
            let uinst = Lib.update_map fixed_env_value env
            ode.prune uinst
        )
      else replicated_odes
    plottables, (List.concat tables_list), inference_parameters, pruned_odes, instances, sweeps
  /// Define an inference forward evaluation without dependencies, simulating all traces in every iteration.
  static member list_inference_full global_settings plottables tables simulate (odes:Ode list) instances inference_parameters sweeps =
    let simfn with_times (e:Environment.t) = 
      let updated_instances = 
        Sweeps.eval e sweeps 
        |> List.map snd 
        |> List.map2 (fun (i:Instance<Functional>) e -> i.update_environment e) instances 
      let updated_odes = odes |> List.map (fun ode -> ode.substitute e)
      Ode.list_simulate_instances global_settings.simulation.multicore (simulate with_times) updated_instances updated_odes
    Inference.create global_settings.inference simfn plottables tables inference_parameters
  /// Define an inference forward evaluation with dependencies, determining which data-points need to be re-evaluated when parameters change.
  static member list_inference_dependencies settings plottables tables simulate (odes:Ode list) instances (inference_parameters:Parameter list) sweeps =
    let simfn with_times e subset = 
      let updated_instances = 
        Sweeps.eval e sweeps 
        |> List.map snd 
        |> List.map2 (fun (i:Instance<Functional>) e -> i.update_environment e) instances
      let updated_odes = odes |> List.map (fun ode -> ode.substitute e)
      let indexed = List.indexed (List.zip updated_instances updated_odes)
      let selected_instances, selected_odes = 
        match subset with 
        | Parameters.Subset ids -> indexed |> List.filter (fun (index,_) -> List.contains index ids) |> List.map snd |> List.unzip
        | Parameters.All -> updated_instances, updated_odes
      Ode.list_simulate_instances settings.simulation.multicore (simulate with_times) selected_instances selected_odes
    // Now compute the dependencies
    let indexed_instance_variables = sweeps |> Sweeps.get_variable_dependencies |> List.indexed
    let dependencies = 
      // TODO: Establish when there are non-sweep parameters that influence the instance dynamics
      inference_parameters 
      |> List.map (fun p -> 
        match indexed_instance_variables |> List.filter (fun (_,inst) -> List.contains p.name inst ) with
        | []  -> Parameters.All
        | ids -> ids |> List.map fst |> List.sort |> Parameters.Subset )
    Inference.create_with_dependencies settings.inference simfn plottables tables inference_parameters dependencies
  (*
  static member list_to_inference_sundials (global_settings:Crn_settings<Functional>) (odes:Ode list)  = 
    Ode.list_to_inference_method Ode.simulate_sundials global_settings odes
  *)
  (*static member list_to_inference_oslo (global_settings:Crn_settings<Functional>) (odes:Ode list)  = 
    Ode.list_to_inference_method (fun ode -> ode.to_oslo().simulate()) global_settings odes*)
  static member list_to_inference (settings:Crn_settings<Functional>) (odes:Ode list) =
    let simulate with_times ode = 
      let ode' = if with_times then ode else Ode.update_times [] ode
      match settings.simulator with 
      | Simulator.Oslo -> ode'.to_oslo().simulate()
      | Simulator.Sundials -> Ode.simulate_sundials ode'
      | _ -> failwithf "Invalid simulator type for inference: %A" settings.simulator
    let plottables, tables, inference_parameters, odes, instances, sweeps = Ode.list_to_inference_method settings odes
    if settings.inference.partial_evaluation
    then Ode.list_inference_dependencies settings plottables tables simulate odes instances inference_parameters sweeps
    else Ode.list_inference_full settings plottables tables simulate odes instances inference_parameters sweeps
  static member list_infer (global_settings:Crn_settings<Functional>) (odes:Ode list) =
    Inference.run_mcmc <| Ode.list_to_inference global_settings odes
  static member prior_prediction num_samples (settings:Crn_settings<Functional>) odes =
    let simulate with_times ode = 
      let ode' = if with_times then ode else Ode.update_times [] ode
      match settings.simulator with 
      | Simulator.Oslo -> ode'.to_oslo().simulate()
      | Simulator.Sundials -> Ode.simulate_sundials ode'
      | _ -> failwithf "Invalid simulator type for inference: %A" settings.simulator
    let plottables, tables, inference_parameters, odes, instances, sweeps = Ode.list_to_inference_method settings odes    
    let simfn with_times (e:Environment.t) = 
      let updated_instances = 
        Sweeps.eval e sweeps 
        |> List.map snd 
        |> List.map2 (fun (i:Instance<Functional>) e -> i.update_environment e) instances 
      let updated_odes = odes |> List.map (fun ode -> ode.substitute e)
      Ode.list_simulate_instances settings.simulation.multicore (simulate with_times) updated_instances updated_odes
    Inference.prior_prediction num_samples settings.inference simfn plottables tables inference_parameters
  member ode.to_inference () = Ode.list_to_inference ode.settings [ode]
  member ode.infer () = Inference.run_mcmc <| Ode.list_to_inference ode.settings [ode]

  member ode.prune env : Ode =
    let possibly_positive p =
      match p |> Expression.substitute env |> Expression.simplify with
      | Value.Float 0.0 -> false
      | _ -> true
    let positive_initials = ode.initials |> List.filter (fun i -> i.value |> possibly_positive) 
    let positive_species = positive_initials |> List.map (fun i -> i.species)
    let reaction_enabled species r = 
      r.reactants |> Mset.elements |> List.forall (fun s -> List.contains s species)
    let rec reachable_reactions (acc_r, acc_u) species =
      let reachable, unreachable = acc_u |> List.partition (reaction_enabled species)
      match reachable with
      | [] -> acc_r, species
      | _ ->
        let products = reachable |> List.collect (fun r -> r.products |> Mset.elements)
        let new_species = products |> List.filter (fun s -> not (List.contains s species))
        reachable_reactions (reachable@acc_r, unreachable) (new_species@species |> List.distinct)
    let all_reactions =
      ode.reactions
      |> List.collect (fun r -> r.normalise () |> function (r, Some r_rev) -> [r; r_rev] | (r, None) -> [r])
    // Determine if there are additional species referenced in rate expressions. N.B. If there are, and they aren't "positive", then they will remain at zero!
    (*let all_species = 
      ode.settings.simulation.plots 
      |> List.collect (Functional2.all_species ode.settings.rates)
      |> List.append positive_species
      |> List.distinct*)
    let reactions, species = reachable_reactions ([], all_reactions) positive_species
    let rateMention = Expression.mentions >> List.choose (fun key -> match key with Key.Rate r -> Some r | _ -> None)
    let rates = ode.settings.all_used_rates rateMention reactions
    
    // Simplify rate expressions
    let species_in_rates = rates |> Map.toList |> List.collect (snd >> Functional2.all_species rates)
    let zeros = (Set.ofList species_in_rates) - (Set.ofList species) |> Set.toList
    let zero_map = zeros |> List.map (fun s -> Key.Species s, Expression.Float 0.0) |> Map.ofList
    let simplified_rates = rates |> Map.map (fun _ v -> Expression.substitute zero_map v)

    // Simplify plot expressions
    let species_in_plots = ode.settings.simulation.plots |> List.collect (Functional2.all_species rates)
    let zeros = (Set.ofList species_in_plots) - (Set.ofList species) |> Set.toList
    let zero_map = zeros |> List.map (fun s -> Key.Species s, Expression.Float 0.0) |> Map.ofList
    let simplified_plots = ode.settings.simulation.plots |> List.map (Expression.substitute zero_map)

    let updated_settings = { ode.settings with rates = simplified_rates; simulation = { ode.settings.simulation with plots = simplified_plots}}
    
    // Re-saturate initials
    let initial_species = positive_initials |> List.map (fun i -> i.species) 
    let add_missing_species (initials:Initial<Species,Value> list) (s:Species) = 
      if List.exists ((=) s) initial_species
      then initials
      else (Initial.create(false,(Expression.Float 0.0),s,None,None))::initials
    let initials = List.rev <| Lib.fold_left add_missing_species (List.rev positive_initials) species
    { ode with reactions = reactions; initials = initials; settings = updated_settings }

  // ********************
  // EXPORTS
  // ********************
  
  /// Generate Matlab code that numerically integrates then plots the system described by ode
  static member to_matlab (ode:Ode) =
    let newline = Lib.newline
    let tabtab = "\t\t"
    let lookup = Parameters.to_env ode.settings.parameters |> Environment.find
    //Constants for various names used in the syntax.
    let outerFunctionName,innerFunctionName,timeVar,popsVar,innerReturnVar = "crn_export","odes","t","x__","dxdt"
    let initialTimeVar,finalTimeVar,speciesVar,odeSolverName,solutionVar,lengthVar,loopVar = "tstart","tfinal","species","ode15s","sol","n","i"
    let plotsVar = "plots"
    let sb = Stringbuilder.empty ()
    let append = Stringbuilder.append sb
    let havePlots = not (List.isEmpty ode.settings.simulation.plots)
    // Code for the outer function.
    append ("function " + solutionVar + " = " + outerFunctionName + "()" + newline + newline)
    append (initialTimeVar + " = " + (string ode.settings.simulation.initial) + "; % Initial time" + newline)
    append (finalTimeVar + " = " + (string ode.settings.simulation.final) + "; % Final time" + newline)
    append (speciesVar + " = {" + (ode.initials |> Lib.string_of_list (fun initial -> "\'" + initial.species.name + "\'") ",") + "}; % The list of all species" + newline)
    if havePlots 
    then
      append (plotsVar + " = {" + (ode.settings.simulation.plots |> Lib.string_of_list (fun plot -> "\'" + (Expression.to_string (Key.to_matlab Species.to_string "time") plot) + "\'") ",") + "};" + newline) 
    append (lengthVar + " = length(" + speciesVar + ");" + newline + newline)
    // parameters are prepended 'p.' 
    append ("% Write out the parameters")
    ode.settings.parameters |> List.iter( fun p -> append (newline + "p." + p.name + " = " + string p.value + ";"))
    append (newline + newline)
    // Assign initial conditions 
    let initials_evaluated = ode.initials |> List.map (Initial.eval (Expression.eval lookup))
    append ("% Assign initial conditions" + newline)
    append ("x0 = zeros(" + lengthVar + ",1);")
    initials_evaluated
    |> List.iteri (fun i initial -> 
      if initial.value <> 0.0 then append (newline + "x0(" + string (i+1) + ") = " + string initial.value + ";" + tabtab + "% " + initial.species.name) )
    append (newline + newline)
    // Solver the ODEs
    let paramsVarStr = if List.isEmpty ode.settings.parameters then "" else ",p"
    append ("% Solve the ODEs" + newline)
    append ("[" + timeVar + "," + popsVar + "] = " + odeSolverName + "(@" + innerFunctionName + ",[" + initialTimeVar + " " + finalTimeVar + "],x0,[]" + paramsVarStr + ");" + newline + newline)
    append ("% Write out a solution structure to be returned by the function" + newline)
    append ("for " + loopVar + " = 1:" + lengthVar + newline)
    append ("  " + solutionVar + ".(" + speciesVar + "{" + loopVar + "}) = " + popsVar + "(:," + loopVar + ");" + newline)
    append ("end" + newline + newline)
    if havePlots 
    then
      // Apply plot expressions 
      append ("% Apply plot expressions" + newline)
      append ("plotsValues = zeros(length(" + popsVar + ")," + string (List.length ode.settings.simulation.plots) + ");")
      let speciesPrefixed species = "sol.(\'" + (species |> Species.to_string) + "\')"
      ode.settings.simulation.plots
      |> List.iteri(fun i plot ->
          //MATLAB's elementwise operators require dot prefixes, we could generalise the Expression.to_string, just patch locally for now
          let asMatlab =
              (Expression.to_string (Key.to_matlab speciesPrefixed "time") plot)
                  .Replace("*", ".*")
                  .Replace("/", "./")
                  .Replace("^", ".^")                
          append (newline + "plotsValues(:," + string (i + 1) + ") = " + asMatlab + ";"))
      append (newline + newline)
    // Produce a plot 
    append ("% Produce a plot" + newline)
    append ("figure;" + newline)
    let plotsString, legendsString =
      if havePlots
      then "plotsValues", plotsVar
      else popsVar, speciesVar   
    append ("plot(" + timeVar + ", " + plotsString + ")" + newline)
    append ("xlabel('Time')" + newline)
    append ("ylabel('Concentration')" + newline)
    append ("box off" + newline)
    append ("legend(" + legendsString + ",'box','off')")
    append (newline + newline + "return" + newline + newline + "%%%" + newline + newline)
    // Code for the inner function.
    append ("function " + innerReturnVar + " = " + innerFunctionName + "(" + timeVar + "," + popsVar + paramsVarStr + ")" + newline + newline)
    // Write out the parameters 
    if not ode.settings.parameters.IsEmpty 
    then
      append ("% Write out the parameters")
      ode.settings.parameters |> List.iter( fun p -> append (newline + p.name + " = p." + p.name + ";"))
      append (newline + newline)
    // Assign states 
    append ("% Assign states")
    initials_evaluated
    |> List.iteri (fun i initial -> append (newline + initial.species.name + " = " + popsVar + "(" + string (i+1) + ")" + ";"))
    append (newline + newline)
    // Define rate expressions 
    if not ode.settings.rates.IsEmpty 
    then
      append ("% Define rate expressions")
      ode.settings.rates |> Map.iter (fun name expr -> append (newline + name + " = " + Expression.to_string (Key.to_matlab Species.to_string timeVar) expr + ";"))
      append (newline + newline)
    // Define reaction propensities 
    let matlab_reaction_to_string (r:Reaction<Species,Value,Functional>) : string =
      let single_reactants = List.rev (Mset.elements r.reactants)
      let single_reactants_names = List.map (fun sr -> (sr, Species.to_string sr)) single_reactants
      match r.rate with
      | Rate.MassAction(rate) ->
        let exp =
          List.fold
            (fun acc (k, x) ->               
                let coeff = Expression.Float (float (r.numInReactants k))
                let pow = Expression.power (Expression.from_string Parser.name x) coeff
                Expression.mul acc pow )
            rate
            single_reactants_names
        Expression.to_string id exp
      | Rate.Function(e) -> Expression.to_string (Key.to_matlab Species.to_string timeVar) e
    append ("% Define reaction propensities");
    ode.reactions
    |> List.iteri (fun i reaction -> append (newline + "r_" + string i + " = " + matlab_reaction_to_string reaction + ";"))
    append (newline + newline)
    append ("% Assign derivatives");
    let d species = "d" + species
    let deriv_string (species_key:int) (initials: Initial<Species,float> list) (all_sim_reactions: (int * Reaction<Species,Value,Functional>) list) =
      let str =
        Lib.fold_left
            (fun (acc,first) (i,r:Reaction<Species,Value,Functional>) ->
              let initial = List.item species_key initials
              let constant = initial.constant
              let stoich = r.getStoich constant initial.species
              match stoich with
              | 0.0 -> (acc,first)
              | f ->
                let opStr = if f>0.0 then (if first then " " else " + ") else (if first then " -" else " - ")
                let coeffStr = if (System.Math.Abs f) = 1.0 then "" else (string (System.Math.Abs f)) + "*"
                let acc = acc + opStr + coeffStr + "r_" + (string i) in
                (acc,false))
            ("",true)
            all_sim_reactions
        |> fst       
      if str = "" then "0.0" else str // Work correctly in case where population never changes
    ode.initials
    |> List.iteri (fun i initial -> append (newline + (d initial.species.name) + " = " + deriv_string i initials_evaluated (List.indexed ode.reactions) + ";"))
    append (newline + newline)
    // Postamble which closes the inner function.
    append (innerReturnVar + " = [" + (ode.initials |> Lib.string_of_list (fun init -> d init.species.name) ";") + "];" + newline + newline)
    append ("return")
    sb.ToString()
