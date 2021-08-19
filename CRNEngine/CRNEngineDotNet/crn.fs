namespace Microsoft.Research.CRNEngine
open System.Diagnostics
open Operators
[<JavaScript>]
[<DebuggerDisplay("{this.to_string()}")>] // displays CRNs as strings in VS debugger
type Crn = 
  { 
    name:string
    settings: Crn_settings<Functional>
    reactions: Reaction<Species,Value,Functional> list
    initials: Initial<Species,Value> list
    attributes: Stringmap.t<Attributes>
  }
  static member empty = { 
    name      = ""
    settings  = Crn_settings.defaults
    reactions = []
    initials  = []
    attributes = Stringmap.empty
  }
  member crn.map (f:Species -> Species) = {
    name      = crn.name
    settings  = crn.settings.map (Functional2.map f) 
    reactions = List.map (Reaction.map f id id) crn.reactions
    initials  = List.map (Initial.mapSpecies f) crn.initials
    attributes = crn.attributes
  }
  member crn.update_settings (settings:Crn_settings<Functional>) = {crn with settings = settings}
  member crn.substitute (e:Environment.t) = 
    crn.update_settings {crn.settings with parameters = Parameters.substitute e crn.settings.parameters}
  member crn.update_times (times:float list) = 
    crn.update_settings {crn.settings with simulation = crn.settings.simulation.update_times times }
  member crn.all_species () = 
    (crn.initials 
     |> List.map (fun i -> i.species)
     |> List.distinctBy (fun i -> i.name)) @ 
    (crn.reactions |> List.collect (fun r -> r.allSpecies)) 
    |> List.distinct
  ///Ensures all species mentioned in reactions or events are in the population also
  ///Adds species to the population as necessary, as initial and input
  member crn.saturate_initials () = 
    let reaction_species:Species list = 
      let f (r:Reaction<Species,Value,Functional>) =
        Mset.union_all (=) [r.reactants; r.products; r.catalysts]
      Mset.elements (Mset.union_all (=) (List.map f crn.reactions))
    let rate_species:Species list = 
      crn.settings.rates 
      |> Map.toList 
      |> List.collect (snd >> Expression.mentions)
      |> List.distinct
      |> List.fold (fun acc f -> match f with Key.Species k -> k :: acc | _ -> acc) []
    let extra_species = List.append reaction_species rate_species |> List.distinct
    let initial_species:Species list = List.map (fun (i:Initial<Species,Value>) -> i.species) crn.initials 
    let add_missing_species (initials:Initial<Species,Value> list) (s:Species) = 
      if List.exists ((=) s) initial_species
      then initials
      else (Initial.create(false,(Expression.Float 0.0),s,None,None))::initials
    let initials:Initial<Species,Value> list = 
      List.rev <| Lib.fold_left add_missing_species (List.rev crn.initials) extra_species
    {crn with initials = initials}
  member crn.saturate_plots () =
    let crn = 
      if crn.settings.simulation.plots.IsEmpty
      then 
        match crn.settings.simulator with
        | Simulator.MC ->
          let mc_settings = 
            Moment_closure_settings.saturate_plots (crn.all_species()) crn.settings.moment_closure  
          { crn with settings = { crn.settings with moment_closure = mc_settings } }
        | _ -> 
          let plots:Functional list = crn.all_species () |> List.map (fun sp -> Expression.Key (Key.Species sp))
          { crn with settings = { crn.settings with simulation = { crn.settings.simulation with plots = plots } } }
      else crn
    let saturate (sim_settings:Simulation_settings<Functional>) =
      if sim_settings.plots.IsEmpty
      then
        match crn.settings.simulator with
        | Simulator.MC -> sim_settings
        | _ ->
          let plots:Functional list = 
            List.map (fun (i:Initial<Species,Value>) -> Expression.Key (Key.Species (i.species))) crn.initials
          { sim_settings with plots = plots }
      else
        sim_settings
    let crn = { crn with settings = { crn.settings with simulation = saturate crn.settings.simulation; simulations = List.map saturate crn.settings.simulations } }
    crn
  member crn.scale () = 
    let scale = crn.settings.stochastic.scale
    if scale = 1.0
    then crn
    else 
      let scale_rate (scale:float) (power:int) (v:Value) = 
        match power with
        | 0 -> Expression.Times [v; Expression.Float scale]
        | 1 -> v
        | 2 -> Expression.Divide{div1 = v; div2 = Expression.Float scale}
        | _ -> Expression.Times [v; Expression.Float (scale ** (1.0 - (float) power))]
      let scale_functional (f:Functional) = Expression.Times [Expression.expand (Key.scale scale) f; Expression.Float scale]
      let scale_reaction (r:Reaction<Species,Value,Functional>) = r.scale scale_rate scale scale_functional
      let scale_population (v:Value) = Expression.Times [v; Value.Float scale]
      let scale_initial (i:Initial<Species,Value>) = i.scale scale_population
      { crn with 
          settings = { crn.settings with stochastic = {crn.settings.stochastic with scale = 1.0} } 
          reactions = List.map scale_reaction crn.reactions
          initials  = List.map scale_initial crn.initials
      }
  static member create (n:string) (s:Crn_settings<Functional>) (r:Reaction<Species,Value,Functional> list) 
    (i:Initial<Species,Value> list) (a:Stringmap.t<Attributes>) (saturatePlots:bool) =
    let crn:Crn = { name = n; settings = s; reactions = r; initials = i; attributes = a }
    let crn = crn.saturate_initials();
    if saturatePlots then crn.saturate_plots() else crn
  member crn.to_ode () =
    let initials = crn.initials |> Seq.toList
    let rs = crn.reactions |> Reaction.normalise_list in // handle reversible reactions
    Ode.create crn.name rs initials crn.settings
  member crn.to_oslo () = crn.to_ode().to_oslo()
  member crn.to_lna () =
    let crn = crn.scale()
    let ode = crn.to_ode ()
    ode.to_lna crn.settings.stochastic.scale
  member crn.initialise_ctmc () =
    let crn = crn.scale()
    let reactions = 
      crn.reactions 
      |> Reaction.normalise_list 
      |> List.map (fun r -> if crn.settings.simulation.kinetics = Kinetics.Deterministic then { r with rate = r.rate.map r.stochastic_to_deterministic id } else r)
    let react (_:Species list) (_:Species) = reactions
    let env = crn.settings.parameters |> Parameters.to_env
    let species = crn.initials |> List.map (fun i -> i.species, i.value |> Expression.eval (Environment.find env) |> int)
    let calc : Calculus<Species> = { react = react }
    let initial_state, applicable_reactions, adapter = calc.initialise_ctmc species
    calc, initial_state, applicable_reactions, adapter
  member crn.to_ctmc () =    
    let calc, initial_state, applicable_reactions, adapter = crn.initialise_ctmc ()
    calc.to_ctmc (Hashtable.ofMap crn.settings.rates) 1.0 initial_state applicable_reactions adapter
  member crn.to_cme ctmc =
    let crn = crn.scale()
    let env = Parameters.to_env crn.settings.parameters
    let populations, events = 
      crn.initials |> Initial<Species,Value>.to_initialpops_events env crn.settings.simulation.initial
    let cme = Cme.create populations events crn.settings.deterministic crn.settings.simulation 1.0
    { cme with statespace = ref ctmc.ctmc }
  member crn.to_pde rng (populations:Populations<Species,'b>) =
    let rs = crn.reactions |> Reaction.normalise_list in // handle reversible reactions
    let species_ids, stoich, powers = Ode.get_matrices crn.initials rs
    let rates = Ode.get_rates species_ids rs (crn.settings.simulation.kinetics=Kinetics.Stochastic)
    let settings = { crn.settings.spatial with parameters = crn.settings.parameters }
    Pde<'b>.create crn.name populations crn.settings.simulation settings stoich powers rates crn.settings.rates rng
  member crn.to_pde1d () = 
    let env = Parameters.to_env crn.settings.parameters
    let nx = crn.settings.spatial.nx
    let rng = 
      match crn.settings.simulation.seed with 
      | Some s -> new Rng.Random(s)
      | None -> new Rng.Random()
    Initial<Species,Value>.to_initialpops_events_poly env crn.settings.simulation.initial crn.initials (Pde<float []>.accumulator_1d rng nx)
    |> fst 
    |> crn.to_pde rng
  member crn.to_pde2d () =  
    let env = Parameters.to_env crn.settings.parameters
    let nx = crn.settings.spatial.nx
    let rng =
      match crn.settings.simulation.seed with 
      | Some s -> new Rng.Random(s)
      | None -> new Rng.Random()
    Initial<Species,Value>.to_initialpops_events_poly env crn.settings.simulation.initial crn.initials (Pde<float [][]>.accumulator_2d rng nx)
    |> fst 
    |> crn.to_pde rng
  member crn.to_ssa () = 
    let crn = crn.scale()
    let e:Environment.t = Parameters.to_env crn.settings.parameters
    let ratesEnv = Key.inline_rates_env e crn.settings.rates 
    let populations,events = Initial<Species,Value>.to_initialpops_events e crn.settings.simulation.initial crn.initials
    let scale = crn.settings.stochastic.scale
    let simulator = Simulation.create populations events crn.settings.simulation scale
    let reactions = 
      if crn.settings.simulation.kinetics = Kinetics.Deterministic 
      then crn.reactions |> List.map (fun r -> { r with rate=r.rate.map r.stochastic_to_deterministic id})
      else crn.reactions
    let simreactions_hash, _, totalprop = Ssa.make_sim_reactions_pops e ratesEnv scale reactions simulator.populations
    Ssa.create simulator crn.settings.stochastic simreactions_hash totalprop crn.settings.parameters ratesEnv
  // ND: It looks like this method enables simulation and subsequent comparison with data (but doesn't appear to be used yet)
  member crn.to_simulation_runs () = crn.settings.to_simulation_runs crn.name  
  member crn.get_instances () = fst (crn.to_simulation_runs ())  
  member crn.simulate_callback cancel output (simulate:bool ref -> (Row<'a> -> unit) -> Crn -> unit) = 
    let instances,tables = crn.to_simulation_runs()
    let f (i:Instance<Functional>) = 
      let crn = {crn with settings = crn.settings.update_simulation i.settings}
      simulate cancel output (crn.substitute i.environment)
    List.iter f instances
  // Simulates all sweep instances
  member crn.simulate (simulate:Crn -> Table<'a>) = 
    let runs,tables = crn.to_simulation_runs()
    let f (i:Instance<Functional>) = 
      let crn = {crn with settings = crn.settings.update_simulation i.settings}
      Result<_>.create i (simulate (crn.substitute i.environment))
    List.map f runs
  member crn.simulate_async (simulate:Crn -> Table<'a>) = 
    #if JavaScript
    failwith "Can't use Async in JavaScript (directive simulation {multicore=True})"
    #else
    let runs,tables = crn.to_simulation_runs()
    let f (i:Instance<Functional>) = 
      let crn = {crn with settings = crn.settings.update_simulation i.settings}
      async { return Result<_>.create i (simulate (crn.substitute i.environment)) }
    List.map f runs |> Async.Parallel |> Async.RunSynchronously |> List.ofArray
    #endif
  //TODO: move simulation methods for cme and pde into their respective types.
  //member crn.simulate_pde_1d () = crn.to_pde1d() |> Pde<float []>.simulate_1d
  //member crn.simulate_pde_2d () = crn.to_pde2d() |> Pde<float [][]>.simulate_2d
  //member crn.simulate_pde_1d_callback cancel output = crn.to_pde1d() |> Pde<float []>.simulate_1d_callback cancel output
  //member crn.simulate_pde_2d_callback cancel output = crn.to_pde2d() |> Pde<float [][]>.simulate_2d_callback cancel output  
  member crn.simulate_cme_state () =
    let env = Parameters.to_env crn.settings.parameters
    let ratesEnv = Crn.to_inlined_rates crn.settings
    let cme = crn.to_cme (crn.to_ctmc())
    Cme.simulate env ratesEnv false cme
  member crn.simulate_cme () = snd <| crn.simulate_cme_state ()
  member crn.simulate_cme_callback (cancel:bool ref) (ctmc_output:ctmc_result<Species>->unit) (output:Row<Point> -> unit) =
    let paramEnv = Parameters.to_env crn.settings.parameters
    let ratesEnv = Crn.to_inlined_rates crn.settings
    let ctmc = crn.to_ctmc()
    let cme = crn.to_cme ctmc
    ctmc_output ctmc;
    Cme.simulate_callback paramEnv ratesEnv cancel output false cme
  member crn.simulate_cmesundials_callback (cancel:bool ref) (ctmc_output:ctmc_result<Species>->unit) (output:Row<Point> -> unit) =
    let paramEnv = Parameters.to_env crn.settings.parameters
    let ratesEnv = Crn.to_inlined_rates crn.settings
    let ctmc = crn.to_ctmc()
    let cme = crn.to_cme ctmc
    ctmc_output ctmc;
    cme.simulate_sundials_callback cancel paramEnv ratesEnv output
  member crn.infer () = crn.to_ode().infer()
  //member crn.infer_oslo () = crn.to_ode().infer_oslo()
  member crn.to_sundials () = crn.to_ode().to_sundials()
  //member crn.infer_sundials () = crn.to_ode().infer_sundials()  
  member crn.simulate_case () : Result<float> list = 
    let map_point result = { instance = result.instance; table = Table<float>.point_to_float result.table }
    match crn.settings.simulator with 
    | Simulator.SSA -> crn.simulate (fun crn -> crn.to_ssa().simulate())
    | Simulator.Oslo -> crn.simulate (fun crn -> crn.to_oslo().simulate())
    | Simulator.Sundials -> 
      #if JavaScript 
      failwith "Sundials not supported in JavaScript"
      #else
      crn.simulate (fun crn -> crn.to_sundials().simulate())
      #endif
    | Simulator.LNA -> List.map map_point (crn.simulate (fun crn -> crn.to_lna().simulate()))
    | Simulator.CME | Simulator.CMESundials -> 
      let runs,tables = crn.to_simulation_runs()
      let initial_state_map = 
        runs 
        |> List.map (fun i -> i, (crn.substitute i.environment).initialise_ctmc ()) 
        |> List.groupBy (fun (_,(_,is,_,_)) -> is)
        |> List.map (fun (_,is) -> snd is.Head, is |> List.map fst)
      initial_state_map
      |> List.map (fun ((calc,initial_state,applicable_reactions,adapter),is) -> 
        let ctmc = calc.to_ctmc (Hashtable.ofMap crn.settings.rates) 1.0 initial_state applicable_reactions adapter
        let cme = crn.to_cme ctmc
        is
        |> List.map (fun i -> 
          let ratesEnv = Crn.to_inlined_rates crn.settings
          match crn.settings.simulator with
          | Simulator.CME -> Cme.simulate i.environment ratesEnv false cme
          | Simulator.CMESundials -> cme.simulate_sundials i.environment ratesEnv
          | _ -> failwith "Should be unreachable with a non-CME simulator"
          |> snd 
          |> Result<_>.create i
        )
      )
      |> List.concat
      |> List.map map_point
    | Simulator.PDE -> crn.simulate (fun crn -> 
        match crn.settings.spatial.dimensions with 
        | 1 -> 
          let pde = crn.to_pde1d() 
          let xs, table = Pde<float []>.simulate_1d pde
          Table<float[]>.floatarray_to_float xs table
        | 2 -> 
          let pde = crn.to_pde2d()           
          let xs, table = pde |> Pde<float [][]>.simulate_2d
          Table<float[][]>.floatarrayarray_to_float xs xs table
        | _ -> failwith "Unknown number of dimensions"
      )
    | Simulator.MC  -> failwith "Can't simulate MC with generic simulate method, because CRNEngine does not have access to MomentClosure"  
  member crn.update_empty_plot_labels () = {crn with settings = crn.settings.update_empty_plot_labels ()}
  member crn.create_blank_attributes () =
    let allInitials = crn.saturate_initials().initials
    let f m (i:Initial<Species,Value>) =
      if (Stringmap.tryFind i.species.name m).IsNone then
        Stringmap.add i.species.name {name = i.species.name; svg = ""; structure = "" } m
      else m
    let m = allInitials |> List.fold f crn.attributes
    { crn with attributes = m }
  member crn.initialise () = crn.create_blank_attributes().update_empty_plot_labels()
  static member to_inlined_rates (settings:Crn_settings<Functional>)  =
    let parameterEnv:Environment.t = Parameters.to_env settings.parameters
    Key.inline_rates_env parameterEnv settings.rates  
  ///Attempt to replace species if they are defined as rates
  ///TODO: we should get rid of the Rate union case altogether and just inline Species definitions
  static member species_to_rates (rates:Map<string,Functional>) (k:Key<Species>) =
    match k with
    | Key.Species s ->
      if rates.ContainsKey s.name
        then Key.Rate s.name
        else Key.Species s
    | Key.Time -> Key.Time
    | Key.Parameter p -> Key.Parameter p
    | Key.Rate r-> Key.Rate r
  member this.disambiguate_species_to_rates () =
    { this with settings  = this.settings.map (Expression.map (Crn.species_to_rates this.settings.rates))
                reactions = this.reactions 
                            |> List.map (Reaction.map id id (Expression.map (Crn.species_to_rates this.settings.rates)))  }
  static member create_from_instructions (settings:Crn_settings<Functional>) 
    (instructions: ((string * string list) * Instruction list) list * Instruction list) = 
    let rec checkNameClashes = function
    | []
    | [_] -> ()
    | (className1, elems1)::rest ->
        List.iter (fun (className2, elems2) ->
          let clashes = Set.intersect elems1 elems2 
          if not (Set.isEmpty clashes)
          then sprintf "Naming conflict: %s and %s \"%s\" share the same name" className1 className2 (Set.toList clashes).Head
                  |> failwith
            ) rest
        checkNameClashes rest
    let reactions,initials = Instruction.convert_instructions instructions
    (* CS: Currently, the string "[X]" can be parsed either as a Species "X" or a rate expression "X".
    The parser cannot disambiguate this element until the whole CRN is parsed.
    When parsing is finished and all initials and rates defined in the CRN are known, such elements can be disambiguated.
    If an element [X] is defined both in the rates and in the initials, an exception is thrown. *)
    let pars  = settings.parameters |> List.map (fun p -> p.name) |> Set.ofList
    let inits = initials            |> List.map (fun i -> i.species.name) |> Set.ofList
    let rates = settings.rates      |> Map.toList |> List.map fst |> Set.ofList  
    checkNameClashes [("parameter", pars); ("initial", inits); ("rates", rates)]
    let nameConflicts = 
      initials 
      |> List.map (fun i -> i.species.name)
      |> List.filter (settings.rates.ContainsKey)
    if nameConflicts.IsEmpty
    then 
      //TODO: we may need to refine this so that the simulation_settings are not applied.
      let cSettings = settings.map (Expression.map (Crn.species_to_rates settings.rates))
      let cReactions = reactions |> List.map (Reaction.map id id (Expression.map (Crn.species_to_rates settings.rates)))
      Crn.create "" cSettings cReactions initials Stringmap.empty true
    else failwith <| "Name conflict: an initial species and a rate share the name \""+ nameConflicts.Head + "\""

  member crn.to_string_with initials_filter value_filter () = // do not print an initial if its concentration satisfies some predicate
    let env:Environment.t = Parameters.to_env crn.settings.parameters
    let settings_directives_string = crn.settings.to_string Functional2.to_string Functional2.to_string_plot
    let initial_as_expression = Expression.Float crn.settings.simulation.initial
    let inits:string list =
      crn.initials
      |> List.filter(fun initial -> initials_filter initial.value)
      |> List.map (Initial.to_string Species.to_string (Expression.to_string id) value_filter >> sprintf "| %s")
    let reacts:string list =
      crn.reactions
      |> List.map (Reaction.to_string Species.to_string (Expression.to_string id) Functional2.to_string >> sprintf "| %s")
    settings_directives_string + 
    (if inits = [] then "" else "\n" + String.concat ("\n") inits) +
    (if reacts = [] then "" else "\n" + String.concat ("\n") reacts)
  
  member crn.to_string_zero_initials () =
    crn.to_string_with (fun _ -> true) (fun _ -> false) ()
    
  
  member crn.to_string () =
    let not_initial value = Expression.simplify value <> Expression.Float crn.settings.simulation.initial 
    crn.to_string_with Expression.is_not_empty not_initial ()
  
  member crn.to_string_structure () =
    let not_initial value = Expression.simplify value <> Expression.Float crn.settings.simulation.initial 
    let settings_directives_string = crn.settings.to_string Functional2.to_string Functional2.to_string_plot
    let to_structural (x:Species) : string = (Stringmap.find x.name crn.attributes).structure
    let inits:string list =
      crn.initials
      |> List.filter(fun initial -> Expression.is_not_empty initial.value)
      |> List.map (Initial.to_string to_structural (Expression.to_string id) not_initial >> sprintf "| %s")
    let reacts:string list =
      crn.reactions
      |> List.map (Reaction.to_string to_structural (Expression.to_string id) Functional2.to_string >> sprintf "| %s")
    settings_directives_string + 
    (if inits = [] then "" else "\n" + String.concat ("\n") inits) +
    (if reacts = [] then "" else "\n" + String.concat ("\n") reacts)

  member crn.to_matlab () =
    let saturated:Crn = crn.saturate_initials().create_blank_attributes()
    match crn.settings.simulator with
    | Simulator.Oslo
    | Simulator.Sundials -> saturated.to_ode() |> Ode.to_matlab
    | x -> x.ToString() + " simulator does not support MATLAB export, try Oslo or Sundials"
  
  static member parse_instructions_modules (ps:Parser.t<Species>) modules (settings:Crn_settings<Functional>) = 
    Instruction.parse ps settings 
    |>> fun (newModules, is) -> 
          let allModules = modules @ newModules
          Crn.create_from_instructions settings (allModules, is), allModules // return module defs too
  
  static member parse_instructions (ps:Parser.t<Species>) (settings:Crn_settings<Functional>) = 
    Crn.parse_instructions_modules ps [] settings
  
  static member parse : Parser.t<Crn> = 
    Crn_settings.parse Functional2.parse Functional2.parse_plot .>> Parser.spaces 
      >>= Crn.parse_instructions Species.parse |>> fst

  static member parse_with_modules moduleDefs defaults =
    let psettings = Crn_settings.parse_defaults  Functional2.parse Functional2.parse_plot defaults
    psettings >>= Crn.parse_instructions_modules Species.parse moduleDefs
  
  static member parse_with_modules_defaultSettings moduleDefs defaults = 
    let psettings = Crn_settings.parse_directives_defaults defaults Functional2.parse Functional2.parse_plot |>> defaults.from_default_directive_list
    psettings >>= Crn.parse_instructions_modules Species.parse moduleDefs
    

  static member from_string (s:string) = (Parser.from_string Crn.parse s).create_blank_attributes()
  
(*AP: TODO: do we still need this?
  member crn.simulate_2d_test_callback (cancel:bool ref) (output:Row<float[][]> -> unit) = 
    let output2d (d:Row<float[]>) = 
      let values = d.values
      let time = d.time
      let dd = [|values;(Array.map (fun v -> Array.map (fun w -> w / 2.0) v) values);(Array.map (fun v -> Array.map (fun w -> w * 2.0) v) values)|] in
      output { time = d.time; values = dd }
    crn.simulate_pde_1d_callback cancel output2d
*)

///////////////////////////////////////////////////////////////////////////////////////////////////
// Simpify the functions below
///////////////////////////////////////////////////////////////////////////////////////////////////
  (* TODO: implement the following
  member crn.to_ssa () 
    let seed_generator = Rng.Random() in
    let do_simulation i = 
        let seed = match crn.settings.ssa.seed with
                    | None   -> seed_generator.Next()
                    | Some s -> s+i 
        in
        printfn "  Simulation %d: seed %d" i seed;
        let crn = { crn with settings = { crn.settings with ssa = { crn.settings.ssa with seed = Some seed } } } in
        let ssa, sim_data = Crn.simulate_ssa_env env crn in
        let filename = Io.prepare_files_directories s sweep_key env (sprintf "%s_%d" filename seed) in
        Io.write_file filename (Io.string_of_table_unordered "\t" sim_data)
    in
    let simulations = crn.settings.ssa.trajectories in
    if multicore
    then
        let job = Async.Parallel(List.init simulations (fun i -> async { return do_simulation i })) in
        Async.RunSynchronously job |> ignore
    else 
        for i in 0..simulations-1 do
          do_simulation i
  *)



  (*let to_sundials_t (crn:t) = 

      (*Sundials*)
    let reaction_maker environment =
      let populations,events = Initial.to_pops_events environment crn.initials in
      let simulator = Simulation.create populations events crn.settings.simulation 1.0 in
      let sreactions = List.map Lib.fst3 (Reaction.get_sim_reactions_products 1.0 simulator.populations environment crn.reactions) in
      sreactions
    ()*)
  
  ///TODO: move this function into calculus.fs  
  ///creates a new CRN by expanding the whole reaction network of the input initials according to the input calculus.
  static member from_calculus_translated 
        (to_engine_species      : 's -> Species) 
        (get_species_attributes : 's -> Attributes)
        (equalsSpecies          : 's -> 's -> bool)  // equalsSpecies s1 s2 == "is species s1 equivalent to s2?"
        (matchesPlot            : 's -> 's -> bool)  // matchesPlot s1 s2   == "does s2 match plot s1?"
        (isSpecies              : 's -> bool)        // is the input a species, or something else? (e.g. a plot pattern, or DNA strand with a free name in some domain)
        (name                   : string) 
        (settings               : Crn_settings<Expression.t<Key<'s>>>) 
        (calculus               : Calculus<'s>) 
        (initials               : Initial<'s,Value> list) 
        (initialReactions       : Reaction<'s,Value,Expression.t<Key<'s>>>  list)
        (jit                    : bool)
        (saturatePlots          : bool)
        (enumerateAllSpecies    : bool) // if true, reaction enumeration picks new species not only from the products of new reactions (as in DSD), but any species occurring in a new reaction. This is used in Logic GEC, when a program ends with a CRN rather than a single species. Classic DSD and Logic DSD do not use this feature.
        =
    /// find all possible reactions that "calculus" can generate from the CRN
    let rec loop (reactions_acc: _ list) species_acc to_add =
      let is_new sps s =
        match List.tryFind (equalsSpecies s) sps with
        | None -> true
        | Some _ -> false
      in
      match to_add with
      | []    -> reactions_acc, species_acc |> Seq.distinct |> Seq.toList
      | i::is ->
        if not (is_new species_acc i)
          then loop reactions_acc species_acc is
          else 
            let new_reactions = calculus.react species_acc i
            let new_species = 
              new_reactions
              |> List.collect (fun r -> 
                if enumerateAllSpecies       
                  then r.allSpecies @ (match r.rate with 
                                       | MassAction _ -> [] 
                                       | Function fr  -> fr |> Expression.mentions |> List.collect (fun x -> match x with Key.Species s -> [s] | _ -> []))
                  else r.products |> Mset.elements)
              |> List.distinct
              |> List.filter (is_new (species_acc@is) )
            loop (new_reactions@reactions_acc) (i::species_acc) (is @ new_species)

    let declaredSpecies = List.map (fun (i:Initial<'s,Value>) -> i.species) initials
    let zero : Value = Expression.Float 0.0
    let tzero = None //Expression.Float settings.simulation.initial
    let plotSpecies = 
      settings.simulation.plots
      // filter out plots containing any free names; such as "_" or any unbound domain "K"
      |> List.collect Expression.mentions 
      |> List.choose (fun k -> match k with 
                               | Key.Species sp -> if (isSpecies sp) 
                                                    then Some sp  
                                                    else None
                               | _ -> None)
      |> List.filter (fun x -> not (List.contains x declaredSpecies))
      |> Seq.distinct |> Seq.toList
    let plotInitials = plotSpecies 
                       |> List.map (fun sp -> Initial.create(false, zero, to_engine_species sp, tzero, None))
    // Assign attributes to all initial species.
    let m = (declaredSpecies @ plotSpecies) |> List.map get_species_attributes |> List.fold (fun mm s -> Stringmap.add s.name s mm) Stringmap.empty in

    // Extend initial species with species in initial reactions.
    let reactionSpecies = initialReactions |> List.collect (fun r -> r.products |> Mset.elements)
    let initialSpecies = 
      declaredSpecies @ reactionSpecies
      |> List.rev
      |> List.fold (fun acc sp -> 
        if List.exists (equalsSpecies sp) acc
        then acc
        else acc @ [sp]) [] 

    // Assign attributes to all species in plots.
    let get_expression_structural exp =
      let get_structural (sp:'s) =
        let n = (to_engine_species sp).name
        match Map.tryFind n m with None -> (get_species_attributes sp).structure
                                 | Some attr -> attr.structure
      let map_key = Key.to_string get_structural
      Expression.to_string map_key exp
    let m = List.fold (fun mm exp -> let exp_s = Expression.to_string_plot (Key.to_string_plot (fun sp -> (to_engine_species sp).name)) exp
                                     if Stringmap.inDomain mm exp_s then mm
                                     else Stringmap.add exp_s { name = exp_s; structure = get_expression_structural exp; svg = "" } mm) m settings.simulation.plots    // add user-defined reactions only if they are not already generated by the calculus

    // Generate all species.
    let reactions', species = loop [] [] initialSpecies

    // CS: this seems to be the behaviour in Visual DSD
    let reactions = initialReactions @ (List.filter (fun (x:Reaction<'s,Value,Expression.t<Key<'s>>>) -> not (List.exists ((=) x) initialReactions)) reactions') 
    //let species   = species'    |> Seq.distinct |> Seq.toList
    let build_initial_mapping s =
      let engine_species = s |> to_engine_species
      ( s
      , match initials |> List.tryFind (fun i -> i.species = s) with
        | Some i -> Initial.create(i.constant, i.value, engine_species, i.time, i.spatial)
        | None -> Initial.create(false, zero, engine_species, tzero, None)
      )
    in
    let initials_mappings = species |> Lib.rev_map build_initial_mapping in
    let initials_map = initials_mappings |> Map.ofList in
    
    // create initials for species mentioned in the reactions which are *not* present in the initials or plots
    let reactionInitials = 
        species 
        |> List.filter (fun sp -> initials |> List.forall (fun i -> i.species <> sp))
        |> List.filter (fun sp -> plotSpecies |> List.forall (fun i -> i <> sp))
        |> List.map    (fun sp -> Initial.create(false, zero, to_engine_species sp, tzero, None))
    // transform the species in the input initials to engine_species
    let engineInitials = initials |> List.map (Initial.mapSpecies to_engine_species)
    let new_initials = engineInitials @ reactionInitials @ plotInitials
    
    // maps species to engine_species in reactions
    let map_species s = 
      match Map.tryFind s initials_map with
      | Some initial -> initial.species
      | None -> 
          let equivalentKey = Map.findKey (fun k _ -> equalsSpecies s k) initials_map
          (Map.find equivalentKey initials_map).species
    let translatePlot s = List.filter (matchesPlot s) species 
                          |> Seq.distinct 
                          |> Seq.toList
    // Expand sum plot patterns, unless running JIT. In JIT mode, sum patterns are dynamic.
    let filteredSettings =
      if not jit
        then settings.collect_plots (Expression.collect (Key.collect translatePlot))
        else settings
    let new_settings = filteredSettings.map (Expression.map (Key.map to_engine_species))
    let new_reactions = reactions |> Lib.rev_map (Reaction.map map_species (Expression.map id) (Expression.map (Key.map map_species))) in
    // Assign attributes to all remaining species.
    let m = species |> List.map get_species_attributes |> List.fold (fun mm s -> Stringmap.add s.name s mm) m in
    
    Crn.create name new_settings new_reactions new_initials m saturatePlots

  static member compute_reversibles eq (t:Crn) =
    let rec cmp_list c l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x::xs, y::ys ->
        let r = c x y in
        if r = 0 then cmp_list c xs ys else r
    let species_map = Hashtable.empty () in
    List.iteri (fun i s -> Hashtable.add species_map s i) (t.initials |> List.map (fun i -> i.species) |> Set.ofList |> Set.toList);
    let get_index s =
      match Hashtable.tryFind species_map s with
      | Some i -> i
      | None ->
        let i = Hashtable.count species_map in
        Hashtable.add species_map s i;
        i in
    let cmp s1 s2 = s1 - s2 in
    let cmp_m (entry1: Mset.entry<int>) (entry2: Mset.entry<int>) =
      let r = cmp entry1.element entry2.element in
      if r = 0 then entry1.multiplicity - entry2.multiplicity else r in
    let reaction_rep (r: Reaction<Species,Value,Functional>) =
      let reactants_mset = Mset.map get_index (Mset.union eq r.catalysts r.reactants) in
      let reactants_rep = List.sortWith cmp_m reactants_mset in
      let products_mset = Mset.map get_index (Mset.union eq r.catalysts r.products) in
      let products_rep = List.sortWith cmp_m products_mset in
      if cmp_list cmp_m reactants_rep products_rep > 0
      then (products_rep, reactants_rep, true)
      else (reactants_rep, products_rep, false) in
    let reactions_map = Hashtable.empty () in
    let order = ref [] in
    let add_reaction reac =
      let (r, p, flipped) = reaction_rep reac in
      match Hashtable.tryFind reactions_map (r, p) with
      | Some l -> Hashtable.add reactions_map (r, p) ((reac, flipped)::l)
      | None ->
          order := (r, p)::!order;
          Hashtable.add reactions_map (r, p) [reac, flipped] in
    List.iter add_reaction (List.rev t.reactions);
    let add_rate_def (rd1: Rate<Value,Functional>) (rd2: Rate<Value,Functional>) : Rate<Value,Functional> =
      match (rd1, rd2) with
      | Rate.MassAction ma1, Rate.MassAction ma2 -> Rate.MassAction (Expression.Plus [ma1;ma2])
      | _ -> failwith "Not implemented" 
    in
    let add_rate_def_reverse (rd1: Rate<Value,Functional> option) (rd2: Rate<Value,Functional> option) =
      match (rd1, rd2) with
      | None, None -> None
      | None, Some rd | Some rd, None -> Some rd
      | Some rd1, Some rd2 -> Some (add_rate_def rd1 rd2) 
    in
    let add_rate_def_mixed (rd1: Rate<Value,Functional>) (rd2: Rate<Value,Functional> option) =
      match (rd1, rd2) with
      | rd, None -> rd
      | rd1, Some rd2 -> add_rate_def rd1 rd2 
    in
    let merge_reactions rs =
      let merge (r1: Reaction<Species,Value,Functional>, f1: bool) (r2: Reaction<Species,Value,Functional>, f2: bool) =
        if f1 = f2 then 
          (* straight merge *)
            let rate = add_rate_def r1.rate r2.rate
            let reverse = add_rate_def_reverse r1.reverse r2.reverse
            Reaction.create(r1.catalysts, r1.reactants, reverse, rate, r1.products), f1
        else
          (* flip r2 *)
          let rate = add_rate_def_mixed r1.rate r2.reverse
          let reverse = Some (add_rate_def_mixed r2.rate r1.reverse)
          Reaction.create(r1.catalysts,r1.reactants, reverse, rate, r1.products), f1
      let mergeable (r: Reaction<Species,Value,Functional>, _:bool) =
        match r.rate, r.reverse with
        | Rate.MassAction _, None
        | Rate.MassAction _, Some (Rate.MassAction _) -> true
        | _ -> false 
      in
      let mrs, urs = List.partition mergeable rs
      let rf = match mrs with
                  | [] -> []
                  | [r,f] -> [r,f]
                  | r::rs -> [Lib.fold_left merge r rs] 
      urs@rf 
    !order
    |> List.collect (Hashtable.find reactions_map >> merge_reactions)
    |> List.map fst

  static member name_eq (s1:Species) (s2:Species) = s1.name = s2.name
  static member group_reactions (t:Crn) =
    let grouped_reactions = Crn.compute_reversibles Crn.name_eq t in
    { t with reactions = grouped_reactions }

  /// collects all distinct parameters occurring anywhere in the initials, reactions, plots, rates or sweeps
  // TODO: add parameters from spatial_settings
  member this.all_used_parameters () : string list =
    let ofFunctional = Expression.mentions >> List.choose (fun key -> match key with Key.Parameter p -> Some p | _ -> None)
    let initial_vars = 
        this.initials 
        |> List.collect (fun i -> 
            let x = match i.time with Some t -> Expression.mentions t | None -> []
            let y = i.value |> Expression.mentions
            x @ y
        )
    let reaction_vars = 
        this.reactions
        |> List.collect (fun r ->
            let fw = match r.rate with
                     | Rate.MassAction v -> v |> Expression.mentions
                     | Rate.Function   e -> e |> ofFunctional
            let bw = match r.reverse with
                     | None -> []
                     | Some (Rate.MassAction v) -> v |> Expression.mentions
                     | Some (Rate.Function   e) -> e |> ofFunctional 
            fw @ bw
        )
    let simulation_vars = 
        this.settings.simulation.plots
        @ List.concat (this.settings.simulations |> List.map (fun i -> i.plots))
        |> List.collect ofFunctional
    let rate_vars = this.settings.rates |> Map.toList |> List.map snd |> List.collect ofFunctional
    
    let sim_sweeps = this.settings.simulations |> List.collect (fun sim -> sim.sweeps)
    let sweeps = 
      if List.isEmpty sim_sweeps
      then this.settings.sweeps
      else this.settings.sweeps |> List.filter (fun sw -> List.contains sw.name sim_sweeps)
    let sweep_vars = 
      sweeps
      |> List.collect (fun sweep -> 
        sweep.assignments
        |> List.collect (fun assignment ->
          assignment.variables @ (assignment.values |> List.concat |> List.collect Expression.mentions)
        )
      )
    
    // Collect and find distinct variables
    initial_vars @ reaction_vars @ simulation_vars @ rate_vars @ sweep_vars |> List.distinct
    
  member this.all_used_rates () = 
    let rateMention = Expression.mentions >> List.choose (fun key -> match key with Key.Rate r -> Some r | _ -> None)
    this.settings.all_used_rates rateMention this.reactions

  ///TODO: move parts of these functions in reaction.fs, initial.fs, etc.
  ////////////////////////////////////////////////////////////////////////////////////////
  // SBML export
  ////////////////////////////////////////////////////////////////////////////////////////

  member crn.to_sbml () =
    // Do something here to convert reaction rates from deterministic to stochastic?

    let env = Parameters.to_env crn.settings.parameters |> Environment.find

    // Note: need to convert from old units format to the new one.
    let time_exp = 0
    let time_mult = 1.0
    let conc_exp = match crn.settings.units.concentration with Molar n -> n
    let scale = crn.settings.stochastic.scale

    (* A counter for generating new, unique SId strings. *)
    let counter = ref 0 in
    let get_ctr () = (let c = !counter in counter := c+1; c) in
    let new_SId s = Sbml.mkSId (s + get_ctr().ToString()) in
    let species_SId () = new_SId "s_id" in
    let reaction_SId () = new_SId "r_id" in

    (* Collect the species together by 'key, species represented as strings, along with population and constancy information
       and their associated SBML SId strings. *)
    let all_species = List.map (fun (initial:Initial<Species,Value>) -> (initial.species.name, Sbml.mkString initial.species.name, initial.value, initial.constant, species_SId())) crn.initials
    in
  
    (* Search the species for the SId associated with a particular key. *)
    // FP: W# breaks this by using all_species directly as the inner loop variable, which means that all_species gets corrupted at first usage. I tried to make a simple repro for this, but in simpler cases it seems to work properly. The following code overwrites getSId with something that works.
    let getSId (x:string) =
      let rec inner = function
          [] -> failwith ("Error - getSId - key " + x + " not found.")
        | (name : string, _ : Sbml.sbmlString, _ : Value , _ : bool, sid : Sbml.sId)::zs -> if x=name then sid else inner zs
      in
      inner all_species
    in
  
    // FP: replaced the getSId function with one using a map; should be faster and not confuse W#
    let sidmap = all_species |> List.map (fun (name,str,v,c,sid) -> (name,(name,str,v,c,sid)) ) |> Map.ofList
    let getSId (x:string) = match Map.tryFind x sidmap with | None -> failwith ("Error - getSId - key " + x + " not found.")
                                                            | Some (name,str,v,c,sid) -> sid

    (*** Units which will be added to the model once created ***)  
    let unitHash = Hashtable.empty () in
    (* Create units for parameters *)
    let create_unit nr_reactants =
      let uid = Sbml.mkUnitSId ("k_unit_" + nr_reactants.ToString()) in
      let u_t = Sbml.create_sbmlUnit Sbml.Second |> Sbml.u_set_exponent -1 |> Sbml.u_set_scale time_exp |> Sbml.u_set_multiplier time_mult in
      let u_s =
        if nr_reactants <> 1 then
          [ Sbml.create_sbmlUnit Sbml.Mole |> Sbml.u_set_exponent (1-nr_reactants) |> Sbml.u_set_scale conc_exp
          ; Sbml.create_sbmlUnit Sbml.Litre |> Sbml.u_set_exponent (nr_reactants-1)]
        else [] in
      let u_def = Sbml.create_unitDefinition uid (u_t::u_s) in
      (uid, u_def) in

    let get_unit nr_reactants =
      match Hashtable.tryFind unitHash nr_reactants with
      | Some (uid, _) -> uid
      | None ->
          let (uid, u_def) = create_unit nr_reactants in
            Hashtable.add unitHash nr_reactants (uid, u_def); uid in
    (* Create units for time, substance, volume and parameters *)
    let uid_time = Sbml.mkUnitSId "time" in
    let u_time = Sbml.create_sbmlUnit Sbml.Second |> Sbml.u_set_scale time_exp |> Sbml.u_set_multiplier time_mult in
    let u_def_time = Sbml.create_unitDefinition uid_time [u_time] in
    Hashtable.add unitHash -1 (uid_time, u_def_time);
    let uid_substance = Sbml.mkUnitSId "substance" in
    let u_substance = Sbml.create_sbmlUnit Sbml.Mole |> Sbml.u_set_scale conc_exp in
    let u_def_substance = Sbml.create_unitDefinition uid_substance [u_substance] in
    Hashtable.add unitHash -2 (uid_substance, u_def_substance);
    let uid_volume = Sbml.mkUnitSId "volume" in
    let u_volume = Sbml.create_sbmlUnit Sbml.Litre in
    let u_def_volume = Sbml.create_unitDefinition uid_volume [u_volume] in
    Hashtable.add unitHash -3 (uid_volume, u_def_volume);
    let uid_dimless = Sbml.mkUnitSId "dimless" in
    let u_dimless = Sbml.create_sbmlUnit Sbml.Dimensionless in
    let u_def_dimless = Sbml.create_unitDefinition uid_dimless [u_dimless] in
    Hashtable.add unitHash -4 (uid_dimless, u_def_dimless);
    let uid_concentration = Sbml.mkUnitSId "concentration" in
    let u_per_volume = Sbml.create_sbmlUnit Sbml.Litre |> Sbml.u_set_exponent -1 in
    let u_def_concentration = Sbml.create_unitDefinition uid_concentration [u_substance; u_per_volume] in
    Hashtable.add unitHash -5 (uid_concentration, u_def_concentration);
    (* Function to actually add the units to the model *)
    let add_unitDefinitions = Hashtable.fold (fun _ (_, u_def) -> Sbml.model_add_unitDefinition u_def) unitHash in
    (*** Create a "default" compartment for everything to be in. ***)
    let cmt_id = Sbml.mkSId "c" in
    let cmt_size_id = Sbml.mkSId "c_size" in
    let avogadro = 6.0221415 in
    let volume = (scale / avogadro) * 10.0**(-23.0-float(conc_exp)) in
    let cmt_size = volume in
    let cmt_size_parm = Sbml.create_parameter cmt_size_id |> Sbml.p_set_value cmt_size |> Sbml.p_set_units uid_volume in
    let cmt = Sbml.create_compartment cmt_id |> Sbml.cmt_set_size cmt_size in
    (* ************************************* *)
    (* *** Create any global parameters? *** *)
    (* ************************************* *)
    let paramHash = Hashtable.empty () in
    let get_param (name, value) uid =
      match Hashtable.tryFind paramHash (name, value) with (* this might actualy allow scoped parameters *)
      | Some (pid, p) -> pid, p
      | None ->
         let pid = Sbml.mkSId name in
         let p = Sbml.create_parameter pid |> Sbml.p_set_value value |> Sbml.p_set_units uid in
         Hashtable.add paramHash (name, value) (pid, p);
         pid, p in
    (* Function to actually add the parameters to the model *)
    let add_parameterDefinitions = Hashtable.fold (fun _ (_, p) -> Sbml.model_add_parameter p) paramHash in
    (* Initial assignments which require computation *)
    let initsHash = Hashtable.empty () in
    let add_init = Hashtable.add initsHash in
    let add_ia sid mexp =
      let init_mexp = Sbml.Nary (Sbml.Times, [Sbml.Const (Sbml.Int 1, Some uid_concentration); mexp]) in
      Sbml.model_add_initialAssignment (Sbml.create_initialAssignment sid init_mexp) in
    let add_initialAssignments = Hashtable.fold (add_ia) initsHash in
    let get_init_id s =
      let (sid, _) = get_param (s, Expression.eval env (Expression.Key s)) uid_dimless in
      sid in
    (*---------------------------*)
    (* Floats found in the model *)
    (* According to the SBML specification, these should be defined as parameters and given units *)
    (*---------------------------*)
    let floatHash = Hashtable.empty () in
    let float_counter = ref 0 in
    let get_fid () =
      let fid = "float_" + (!float_counter).ToString() in
      float_counter := !float_counter + 1;
      fid in
    let get_float f = match Hashtable.tryFind floatHash f with
                          | Some fid -> fid
                          | None ->
                            let fid, _ = get_param (get_fid (), f) uid_dimless in
                            Hashtable.add floatHash f fid;
                            fid in
    (*------------------------------------------*)
    (* Create the species elements of the SBML. *)
    (*------------------------------------------*)
    let mkSpecies (_:string,s,pop,c,sid) =
      match pop with
      | Expression.Float popv ->
          Sbml.create_species sid cmt_id |>
          Sbml.sp_set_name s |>
          Sbml.sp_set_initialConcentration popv |>
          Sbml.sp_set_constant c |>
          (* Must set boundaryCondition to false if constant is false.
           * Otherwise, the species can't appear as a reactant or product. *)
          fun sp -> if c then (Sbml.sp_set_boundaryCondition true sp) else sp 
      | _ ->
          add_init sid (Expression.to_mathML get_init_id pop); (* (Value.Prod (pop, Value.Float volume))); Seems this expression is a concentration, so do not multiply by volume *)
          Sbml.create_species sid cmt_id |>
          Sbml.sp_set_name s |>
          Sbml.sp_set_constant c |>
          (* Must set boundaryCondition to false if constant is false.
           * Otherwise, the species can't appear as a reactant or product. *)
          fun sp -> if c then (Sbml.sp_set_boundaryCondition true sp) else sp
    in
    (* Create species references to the SIds indicated by the given keys. *)
    let mkSpeciesReferences xs =
      Mset.collectm (fun el -> Sbml.create_speciesReference (getSId el.element) |> Sbml.sr_set_stoichiometry (float el.multiplicity)) xs
    in
    let parM = crn.settings.parameters |> List.map (fun p -> p.name, Expression.Float p.value) |> Map.ofList
    (* *** Translate a engine reaction into its SBML representation. *)
    let mkSBMLReaction (r:Reaction<Species,Value,Functional>) : Sbml.reaction =
      let r = Reaction.map (fun (sp:Species)->sp.name) id id r
      (* Create kinetic law; this depends on whether mass-action or functional rates are used. *)
      let kl =       
        match r.rate with
        | Rate.MassAction v ->
            let rMassAction = Expression.eval env v in
            (* Obtain suitable unit *)
            let uid = get_unit (Mset.size r.reactants) in
            let rate_sid, rate_param, local = (match v with
                                                   | Expression.Key name ->
                                                      let rate_sid, rate_param = get_param (name, rMassAction) uid in
                                                      rate_sid, rate_param, false
                                                   | _ -> (* Expressions will just be evaluated to constants *)
                                                     (* Create a rate parameter so that the reactions are recognised as mass action in COPASI. *)
                                                     (* Always use "k" as the rate parameter name, so we can avoid passing another function in! *)
                                                     let rate_parameter_name = "k" in
                                                     let rate_sid = Sbml.mkSId rate_parameter_name in
                                                     let rate_param = Sbml.create_parameter rate_sid |> Sbml.p_set_value rMassAction |> Sbml.p_set_units uid in
                                                     rate_sid, rate_param, true
                                                    ) in
            let power_sbml s n =
              if n = 1 then s
              else
                let n_const = Sbml.Const (Sbml.Int n, Some uid_dimless) in
                Sbml.Binary (Sbml.Power, s, n_const) in
            let divide_sbml s n =
              if n = 1 then s
              else
                let n_const = Sbml.Const (Sbml.Int n, Some uid_dimless) in
                Sbml.Binary (Sbml.Divide, s, n_const) in
            (* Produce the MathML representation of the reaction rate. *)
            let rate_math = Sbml.Nary(Sbml.Times,                (* RATE IS PRODUCT OF: *)
                              Sbml.Identifier (cmt_size_id) ::               (* 1. Volume of compartment (computed from scale factor). *)
                              Sbml.Identifier (rate_sid) ::                  (* 2. Rate of reaction. *)
                              (Mset.collectm (fun el ->
                                                divide_sbml (power_sbml (Sbml.Identifier (getSId el.element)) el.multiplicity) (Lib.fac el.multiplicity)) r.reactants))                  (* 3. Population of reactants. *)
            in
            let kl = Sbml.create_kineticLaw rate_math |> Sbml.kl_add_parameter cmt_size_parm in
            let kl = if local then Sbml.kl_add_parameter rate_param kl else kl in (* only add local parameters *)
            kl
        | Rate.Function(exp) ->
            let exp = Expression.map (Key.to_matlab Species.to_string "time") exp
            let exp = Expression.substitute parM exp
            (* Get a mathML expression from the modelling engine expression, and then create a kineticLaw from this. *)   
            let mathMLExp = Expression.to_mathML getSId exp in
            Sbml.create_kineticLaw mathMLExp
      in

      (* Actually build the "reaction" data structure. *)
      Sbml.create_reaction (reaction_SId()) |>
      Sbml.r_set_reactants (mkSpeciesReferences r.reactants) |>
      Sbml.r_set_products (mkSpeciesReferences r.products) |>
      Sbml.r_set_reversible false |> (* Always irreversible, as inverses aren't collated in the SBML output. *)
      Sbml.r_set_kineticLaw kl
    in
    (* BUILD UP THE FINAL MODEL:
       0. reset scale factor to 1.
       1. Start with an empty model.
       2. Add the default compartment.
       3. Populate with species from the lists.
       4. Add the reactions. *)
    let m = Sbml.empty_model |>
            Sbml.model_add_compartment cmt |>
            Sbml.model_set_species (List.map mkSpecies all_species) |>
            Sbml.model_set_reactions (List.map mkSBMLReaction (Reaction.normalise_list crn.reactions)) |>
            add_unitDefinitions |>      (* has to be done after reactions have been processed *)
            add_parameterDefinitions |> (* has to be done after reactions have been processed *)
            add_initialAssignments      (* has to be done after species have been processed *)
    in
    m
  
  ////////////////////////////////////////////////////////////////////////////////////////
  // SVG export
  ////////////////////////////////////////////////////////////////////////////////////////
  static member try_grab field def (s:string) =
    let pattern = field |> sprintf "%s=\""
    let offset = pattern.Length
    if s.Contains pattern
    then
      let i = s.IndexOf pattern
      let p_str = s.Substring (i + offset)
      if p_str.Contains "\""
      then
        let j = p_str.IndexOf "\""
        p_str.Substring(0, j) |> float
      else def
    else def

  static member svg_raw_box_sized (svg:string) width height =
    { Svg.box_group =
       { name = ""
       ; content = Svg.Raw svg
       ; anchors = []
       ; debug_anchors = false
       ; sub_groups = []
       ; offset = None }
    ; Svg.box_dim = { x = width; y = height } }
  static member svg_raw_box (svg:string) =
    let width = Crn.try_grab "width" 150.0 svg
    let height = Crn.try_grab "height" 60.0 svg
    Crn.svg_raw_box_sized svg width height
  static member svg_label_box (lclass:string option) (svg:string) =
    let b = Svg.label_box lclass Svg.default_letter_width svg
    { b with box_dim = { b.box_dim with y = 22. } }

  static member get_svg (crn:Crn) (sp:string) (embed:bool) =
    let attrs = Stringmap.tryFind sp crn.attributes
    let svg = match attrs with None -> "" | Some attrs -> attrs.svg
    match svg with
    | "" ->
      Crn.svg_label_box (Some "crnsvgvalue crnsvgspeciestext") sp
    | svg ->
      let svg = if (embed && svg.StartsWith("<svg")) then ("<g" + svg.Remove(0,4).Remove(svg.Length-10,6) + "</g>") else svg
      Crn.svg_raw_box svg

  static member svgListMargin = 20.

  static member initials_to_svg (crn:Crn) =
    let inset = { Svg.x = 30.0; Svg.y = 10.0 }
    let nonZero = crn.initials |> List.filter (fun i -> i.value <> Expression.Float 0.0)
    let rows = nonZero |> List.map (fun i -> 
                                          [ i.value |> Expression.to_string id |> (Crn.svg_label_box (Some "crnsvgvalue"))
                                          ; Crn.get_svg crn i.species.name true
                                          (*; i.time |> Expression.to_string id |> sprintf "@ %s" |> Svg.label_box*) ])
    let rowW (r:Svg.box list) = r.Head.box_dim.x in
    let (max:float) = match rows with [] -> 0.0 | _ -> rows |> List.maxBy rowW |> rowW
    let rows = rows |> List.map (fun l -> match l with [] -> [] | i::g -> { i with
                                                                              box_dim = { i.box_dim with x = 0. } }::g )
    rows |> List.map (fun i -> let b = i |> Svg.stackhc { inset with x = max + 10.0 } Crn.svgListMargin in { b with box_group = { b.box_group with offset = Some {x=max;y=0.}}})
         |> Svg.stackv inset 10.0
       
  static member rate_box_svg (rate:Rate<Value,Functional>) (reverseRate:Rate<Value,Functional> option) =
    let charWidth = 13
    let rateToString:(Rate<Value,Functional> -> string) = fun ra -> ra.to_string_bare (Expression.to_string id) Functional2.to_string
    let rate = rateToString rate
    let reverseRate = match reverseRate with None -> None
                                           | Some reverseRate -> rateToString reverseRate |> Some
    let rateWidth = float (match reverseRate with None -> charWidth * String.length rate | Some reverseRate -> charWidth * max (String.length rate) (String.length reverseRate))
    let rateWidth = max rateWidth 50.
    let content = match reverseRate with
                  | None ->
                    Svg.Paths [ { Svg.path_class = Some "crnsvgrate"
                                ; Svg.commands = [ Svg.MoveTo {x=0.;y=20.}
                                                 ; Svg.LineTo {x=rateWidth;y=20.}
                                                 ; Svg.LineTo {x=rateWidth-10.;y=10.} ]
                                ; Svg.path_label = None }
                              ; { Svg.path_class = None
                                ; Svg.commands = []
                                ; Svg.path_label = { Svg.label_text = rate
                                                   ; Svg.label_anchor = {x=rateWidth/2.;y=17.}
                                                   ; Svg.label_class = Some "crnsvgrate"
                                                   ; Svg.letter_width = Svg.default_letter_width
                                                   ; Svg.label_dir = {x=rateWidth;y=17.} } |> Some } ]
                  | Some reverseRate ->
                    Svg.Paths [ { Svg.path_class = Some "crnsvgrate"
                                ; Svg.commands = [ Svg.MoveTo {x=0.;y=20.}
                                                 ; Svg.LineTo {x=rateWidth;y=20.}
                                                 ; Svg.LineTo {x=rateWidth-10.;y=10.}
                                                 ; Svg.MoveTo {x=rateWidth;y=25.}
                                                 ; Svg.LineTo {x=0.;y=25.}
                                                 ; Svg.LineTo {x=10.;y=35.} ]
                                ; Svg.path_label = None }
                              ; { Svg.path_class = None
                                ; Svg.commands = []
                                ; Svg.path_label = { Svg.label_text = rate
                                                   ; Svg.label_anchor = {x=rateWidth/2.;y=17.}
                                                   ; Svg.label_class = Some "crnsvgrate"
                                                   ; Svg.letter_width = Svg.default_letter_width
                                                   ; Svg.label_dir = {x=rateWidth;y=17.} } |> Some }
                              ; { Svg.path_class = None
                                ; Svg.commands = []
                                ; Svg.path_label = { Svg.label_text = reverseRate
                                                   ; Svg.label_anchor = {x=rateWidth/2.;y=38.}
                                                   ; Svg.label_class = Some "crnsvgrate"
                                                   ; Svg.letter_width = Svg.default_letter_width
                                                   ; Svg.label_dir = {x=rateWidth;y=38.} } |> Some } ]
    { Svg.box_group = Svg.content_group content
    ; Svg.box_dim = {x=rateWidth;y=40.} }

  static member reaction_to_svg (crn:Crn) (r:Reaction<Species,Value,Functional>) =
    let noMargin = { Svg.x = 0.0; Svg.y = 0.0 }
    let map_mset_entry (m:Species Mset.entry) =
      match m.multiplicity with
      | 1 -> [Crn.get_svg crn m.element.name true]
      | x -> [ m.multiplicity.ToString() |> (Crn.svg_label_box (Some "crnsvgvalue"))
             ; Crn.get_svg crn m.element.name true]
    let map_mset (m:Species Mset.t) =
      m |> List.map map_mset_entry |> List.mapi (fun i b -> if i = 0 then b else Crn.svg_label_box (Some "crnsvgvalue crnsvgplus") "+"::b) |> List.collect id |> Svg.stackhc { x = 0.; y = 0. } 10.
    let reactants = if Mset.is_empty r.reactants then None else map_mset r.reactants |> Some
    let catalysts = if Mset.is_empty r.catalysts then None else map_mset r.catalysts |> Some
    let products = if Mset.is_empty r.products then None else map_mset r.products |> Some
    let left = match catalysts with
               | None -> reactants
               | Some catalysts -> match reactants with
                                   | None -> Some catalysts
                                   | Some reactants -> Svg.stackhc noMargin 20. [reactants;Crn.svg_label_box (Some "crnsvgvalue") "~";catalysts] |> Some
    let rate = Crn.rate_box_svg r.rate r.reverse
    let boxes = match left with
                | None -> match products with
                          | None -> failwith "invalid reaction"
                          | Some products -> [rate;products]
                | Some left -> match products with
                               | None -> [left;rate]
                               | Some products -> [left;rate;products]
    let box = Svg.stackhc noMargin 20. boxes
    let box = { box with box_dim = { box.box_dim with x = box.box_dim.x * 1.05 } }
    box

  static member reactions_to_svg (crn:Crn) =
    let noMargin = { Svg.x = 0.0; Svg.y = 0.0 }
    match crn.reactions with
    | [] -> Crn.svg_raw_box ""
    | reactions ->
        let reactions = reactions |> List.map (Crn.reaction_to_svg crn)
        Svg.stackv noMargin Crn.svgListMargin reactions
  
  member crn.to_svg () =
    let initials = Crn.initials_to_svg crn
    let reactions = Crn.reactions_to_svg crn
    Svg.stackv { Svg.x = 0.0; Svg.y = 0.0 } Crn.svgListMargin [initials;reactions]
  
  // This is the style that should be used for reaction lists and population lists.
  static member default_svg_style = "
  .crnsvgvalue {
      text-anchor: end;
      stroke: black;
      fill: black;
      stroke-width: 0;
      font-family: Verdana, Arial, sans-serif;
      font-size: 15px;
  }
  .crnsvgrate {
      text-anchor: middle;
      stroke: black;
      fill: black;
      stroke-width: 0;
      font-family: Verdana, Arial, sans-serif;
      font-size: 15px;
  }
  path.crnsvgrate { stroke-width: 2; fill: none; stroke-linejoin: round }
  .crnsvgplus { text-anchor: start }
  .crnsvgspeciestext { text-anchor: start }
  "