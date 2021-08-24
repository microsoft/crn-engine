// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

[<JavaScript>] 
type reaction_info = { 
  reaction: Reaction<int,float,Lambda<Inlined<int>>>;
  propensity: float;
  dependencies: int list; (* ids of reactions whose reactants are mentioned anywhere in this one (see calculate_reaction_deps) *)
}

[<JavaScript>] 
type Ssa = 
  { 
    simulator: Simulation<Functional>;
    plots: Expression.t<Inlined<int>> array
    settings: Stochastic_settings;
    reactions: Hashtable.t<int, reaction_info>;
    total_propensity: float; 
  }
  static member empty () = {
    simulator = Simulation.empty ();
    plots = Array.empty
    settings = Stochastic_settings.defaults; 
    reactions = Hashtable.empty();
    total_propensity = 0.0;
  }
  static member create (simulator:Simulation<Functional>) (s:Stochastic_settings) (rs:Hashtable.t<int, reaction_info>) total_prop (ps:Parameter list) ratesEnv = 
    let indexedRatesEnv = Map.map (fun _ -> Expression.map (Inlined.map (simulator.populations.find_index))) ratesEnv
    { Ssa.empty () with 
        simulator = simulator;
        plots = 
          let paramEnv = Parameters.to_env ps
          simulator.settings.plots 
          |> List.map (Expression.expand 
                            (Key.map simulator.populations.find_index 
                            >> Key.inline_keys paramEnv indexedRatesEnv)
                        >> Expression.simplify) 
          |> Array.ofList;
        settings = s; 
        reactions = rs; 
        total_propensity = total_prop;
    }
  (* Compute the time until the next reaction - assume an exponential distribution. *)
  //let calculate_time_to_reaction_exponential (prop:float) = log(1.0/(Randomise.float 1.0)) /prop
  //AP//let calculate_time_to_reaction_exponential (prop:float) (random:Rng.Random) = log(1.0 / random.NextDouble()) / prop

  // TODO: remove
  //let mutable debug = ref []  
  (* Increase the population of a species. *)
  static member private incPop (pops:Populations<Species,float> ) (entry:Mset.entry<int>) =
    if not (pops.is_constant entry.element) then
      pops.set_population entry.element ((pops.get_population entry.element) + (float entry.multiplicity))
  (* Execute the given reaction by updating the populations. *)
  static member private executeReaction (pops:Populations<Species,float> ) (r:Reaction<int,float,Lambda<Inlined<int>>>) =
    (* Increase the population of a list of species. *)
    let incPops (ss:int Mset.t) (pops:Populations<Species,float> ) = Mset.iterm (Ssa.incPop pops) ss
    (* Decrease the population of a species. *)
    let decPop (pops:Populations<Species,float> ) (entry:Mset.entry<int>) =
      if not (pops.is_constant entry.element) then
        pops.set_population entry.element ((pops.get_population entry.element) - (float entry.multiplicity))
    (* Decrease the population of a list of strands. *)
    let decPops (ss:int Mset.t) (pops:Populations<Species,float> ) = Mset.iterm (decPop pops) ss
    decPops r.reactants pops; incPops r.products pops

  (***********************************************************************)
  (* %%%%%% Gillespie stochastic simulation algorithm - Direct method. *)
  (* This is the type of functions which "output" simulation results. *)
  //type ('species,'key) sim_outputter when 'species:equality and 'key:equality = float -> 'species settings -> (string * float) list -> unit
  //type output = | NonSpatial of (string * float) | Spatial1D of (float * float) | Spatial2D of (float * float * float)

  (* Randomly pick the next reaction to execute, with probability proportional to the propensities. *)
  static member private pickReactionRandom (total_propensity:float) (reactions:reaction_info array) (random:Rng.Random) =
    let rand = random.NextDouble() * total_propensity 
    let numreactions = reactions.Length 
    let rec pick_reaction (acc:float) (index:int) =
      let r = reactions.[index] 
      let currProp = acc + r.propensity 
      if currProp > rand 
      then (r.reaction, r.dependencies) 
      else 
        if ((index+1) >= numreactions)
        then (* Detect whether this is a propensity problem or not *)
          if currProp <> total_propensity 
          then failwith "Incorrect total propensity used!" 
          else failwith "Index outside range"
        else pick_reaction currProp (index+1) 
    pick_reaction 0.0 0
  static member private calculate_reaction_prop scale (pops:Populations<Species,float> ) (r:Reaction<int,float,Lambda<Inlined<int>>>) =
    let getpop a = pops.get_population a 
    let get_unscaled_pop a = 
      match a with 
      | Inlined.Species i   -> (getpop i) / scale 
      | Inlined.Time        -> failwith "Time dependent rates not supported in SSA."
      //| Inlined.IRate r      -> ratesEnv.[r]
    match r.rate with
      | Rate.MassAction rate -> 
        (match Mset.to_mlist r.reactants with
          | [] -> rate
          | [{multiplicity=1;element=a}] -> rate * (getpop a)
          | [{multiplicity=2;element=a}] -> 
             (* Special case - two members of the same species *)
             let pop = getpop a 
             rate * pop * (pop - 1.0) * 0.5
          | [{multiplicity=3;element=a}] -> 
             let pop = getpop a 
             rate * pop * (pop - 1.0) * (pop - 2.0) / 6.0
          | [{multiplicity=1;element=a};{multiplicity=1;element=b}] ->
             (* Simple case - two different species *)
             rate * (getpop a) * (getpop b)
          (* Use binomial coefficients for the general case! *)
          | aa -> aa |> Lib.fold_left (fun acc entry -> acc * (float) (Lib.binom ((int32) (getpop entry.element)) (entry.multiplicity))) rate )
       | Rate.Function f -> 
  //       let print_pop p = string (getpop p) in
  //       let exp_str = Expression.to_string print_pop exp in (* for debug *)
         (f.key get_unscaled_pop) * scale
  (* Update propensities for given reactions as a side effect, for the Direct method. Return the new total propensity. *)
  static member private update_props_direct 
    scale (totalprop:float) 
    (reactions:reaction_info array) 
    (is:int list) 
    (pops:Populations<Species,float> ) : float =
    (* Start by calculating the sum of the deltas, before adding to the total propensity, for numerical stability. *)
    let deltas = 
      Lib.fold_left (fun acc i -> 
        let r = reactions.[i]
        let new_prop = Ssa.calculate_reaction_prop scale pops r.reaction 
        reactions.[i] <- { r with propensity = new_prop};
        acc + (new_prop - r.propensity)) 0.0 is
    let new_totalprop = totalprop + deltas 
    if new_totalprop < 0.0 then 0.0 else new_totalprop
  static member private calculate_reaction_deps (r:Reaction<int,float,Lambda<Inlined<int>>>) (reactions) =
    let changed_species = Mset.nonzero_elements (Mset.difference (=) r.products r.reactants) 
    (* Dependence can have two sources: stoichiometry and functional rates *)
    let independent_from (sr:Reaction<int,float,Lambda<Inlined<int>>>) (rate:Rate<float, Expression.t<Inlined<int>>>) =
      let in_reactants = Mset.nonzero_elements sr.reactants
      let in_rates =
        match rate with
        | Rate.Function e -> 
            let rec depsInKey k = 
              match k with 
              | Inlined.Species i -> [i]
              | Inlined.Time -> []
              //| Key.IRate r       -> Expression.mentions ratesEnv.[r] |> List.collect depsInKey
            e |> Expression.mentions
              |> List.collect depsInKey                               
        | Rate.MassAction _ -> [] in
      let influencees = Lib.union (=) in_reactants in_rates in
        Lib.disjoint (=) influencees changed_species in
    let f (is,i) (sr,rate) = if (independent_from sr rate) then (is,(i+1)) else (i::is, (i+1))
    List.rev (fst(Lib.fold_left f ([],0) reactions))  
  static member make_sim_reactions_pops env ratesEnv scale reactions (pops:Populations<Species,float> ) : Hashtable.t<int, reaction_info> * int * float =
    let rs = Reaction<int,float,Lambda<Inlined<int>> >.get_sim_reactions_products scale pops env ratesEnv reactions 
    let rinfoOld =
      rs 
      |> List.map
        (fun (sr,p_species,e) ->
          let prop = Ssa.calculate_reaction_prop scale pops sr 
          (sr, prop, e))
    let rinfo = 
      rinfoOld 
      |> 
        let arg = rinfoOld |> List.map (fun (x,y,z) -> (x,z))
        List.map (fun (sr,prop,e) -> (sr,prop,Ssa.calculate_reaction_deps sr arg))
    let ht = Hashtable.empty () 
    let next, totalprop =
      Lib.fold_left
        (fun (i,totalprop) (r,prop,deps) ->
          let _ = Hashtable.add ht i { reaction = r; propensity = prop; dependencies = deps} 
          (i+1, totalprop + prop))
        (0, 0.0) rinfo
    (ht, next, totalprop)
  (* Main Gillespie function - Direct Method. With hashtables *)
  member ssa.simulate_callback (cancel:bool ref) (output:Row<float> -> unit) (output_finals:Populations<Species,float>->unit) (update_stationary: Option<float -> float -> Populations<Species,float> -> unit>) =
    //let simulator = ssa.simulator in 
    let settings = ssa.settings in
    let random = match ssa.simulator.settings.seed with None -> new Rng.Random() | Some s -> new Rng.Random(s)
    (* Raise an exception if the populations aren't integer (or constant) *)
    if Populations<Species,float>.test_integer_or_constant ssa.simulator.populations = false
    then failwith "Can't use non-integer initial conditions without specifying the species as constant"
    else ();
    let final_step = match settings.steps with None -> None | Some n -> Some(ssa.simulator.stepsdone + n) 
    let simreactions_hash = ssa.reactions 
    let totalprop = ssa.total_propensity 
    let simreactions:reaction_info array = Array.zeroCreate (Hashtable.count simreactions_hash) 
    Hashtable.iter (fun i sr -> simreactions.[i] <- sr) simreactions_hash;
    let scale = settings.scale 
    let endtime = ssa.simulator.settings.final 
    let printinterval = ssa.simulator.settings.get_print_interval()
    let grab_data time (pops:Populations<Species,float> ) =
      Array.map (Expression.eval (fun k -> 
        match k with 
        | Inlined.Time       -> time
        | Inlined.Species sp -> pops.get_population sp
        )) 
        ssa.plots 
    (* Inner loop for testing whether a step has passed an event or not, and processes all such events for the given step. *)
    let rec enact_events lu1 (ssa:Ssa) = 
      let dt = lu1 / ssa.total_propensity 
      function
      | [] -> (dt, [], ssa)
      | (next_event:Event<Species, float, float>) :: events ->
        if next_event.time < ssa.simulator.currenttime + dt 
        then
          (* Output the point at ssa.current time _and_ infinitesimally close to the actual event *)
          let new_data = grab_data ssa.simulator.currenttime ssa.simulator.populations 
          let rr = 0.99999999 
          let output_time = rr * next_event.time + (1.0 - rr) * ssa.simulator.currenttime 
          output {time=ssa.simulator.currenttime; values=new_data};
          output {time=output_time; values=new_data};
          (* Run simulation until the event, and keep track of how much of the random number has been used up. *)
          let dt1 = next_event.time - ssa.simulator.currenttime 
          let old_prop = ssa.total_propensity 
          (* Enact the event *)
          (match next_event.target with
          | Target.Species ev_pop ->
              let enact (pi:Population<Species,float> ) = 
                let si = ssa.simulator.populations.find_index pi.species 
                Ssa.incPop ssa.simulator.populations {element=si; multiplicity=(int32) (pi.value * ssa.settings.scale)} 
              List.iter enact ev_pop.get_pop_info_list
          //| Event.Parameter _ -> ()
          | Target.OutputPoint -> ()
          ); 
          (* Compute the new total propensity - side effect is to update propensities of reactions as necessary. *)
          Array.iteri
            (fun i (sr:reaction_info) ->
              let prop = Ssa.calculate_reaction_prop scale ssa.simulator.populations sr.reaction 
              Array.set simreactions i { sr with propensity = prop })           
            simreactions;
          let totalprop = Array.fold (fun p (sr:reaction_info) -> p + sr.propensity) 0.0 simreactions 
          (* Run for the remaining portion of the random number *)
          let new_lu1 = lu1 - dt1 * old_prop 
          let new_current_time = next_event.time 
          enact_events new_lu1 {ssa with total_propensity = totalprop; simulator = {ssa.simulator with currenttime = new_current_time}} events
        else (dt, next_event::events, ssa) 
    (* MAIN SIMULATOR LOOP. *)
    let rec loop (first_loop:bool) (ssa:Ssa) (remaining_events:Event<Species,float,float> list) (output:Row<float> -> unit) =
      (* Pick random number first *)
      //let u = Randomise.float 1.0 in
      let u = random.NextDouble() 
      (* Treat a 0.0 as if it were a 1.0 *)
      let lu1 = if u > 0.0 then log(1.0 / u) else 0.0 
      (* Calculate the propensities of all reactions in the system and the time to the next reaction. *)
      let dt, remaining_events, ssa = enact_events lu1 ssa remaining_events 
      let next_currenttime = ssa.simulator.currenttime + dt     
      match remaining_events with
      | [] -> ssa
      | _ ->
        if update_stationary.IsSome then 
          update_stationary.Value ssa.simulator.currenttime dt ssa.simulator.populations
        (* Output the data points, if necessary. *)
        let ssa:Ssa =
          if (Simulator.shouldPrint endtime next_currenttime ssa.simulator.nextprinttime final_step first_loop ssa.simulator.currenttime) 
          then
            let new_row:Row<float> = {time = ssa.simulator.currenttime; values = (grab_data ssa.simulator.currenttime ssa.simulator.populations)} in
            output new_row;
            let next_time = next_currenttime + printinterval
            in {ssa with simulator = {ssa.simulator with nextprinttime = next_time} }
          else
            ssa
        (* Stop if we reach the endpoint or the "cancel flag" is triggered. *)
        if (Simulator.shouldStop endtime ssa.simulator.currenttime ssa.simulator.stepsdone cancel final_step) then
          ssa
        else
          let ssa = { ssa with simulator = {ssa.simulator with currenttime = next_currenttime }} 
          (* Find a randomly chosen reaction and its dependencies. *)
          let (reaction,deps) = Ssa.pickReactionRandom ssa.total_propensity simreactions random 
          (* Modify the populations according to that reaction. *)
          Ssa.executeReaction ssa.simulator.populations reaction;
          (* Compute the new total propensity - side effect is to update propensities of reactions as necessary. *)
  //        let correctTotalProp = simreactions |> Array.fold (fun acc x -> acc + x.propensity) 0.0// TODO: remove
          let totalprop = Ssa.update_props_direct scale ssa.total_propensity simreactions deps ssa.simulator.populations in
  //        let diff = correctTotalProp - totalprop
  //        debug := !debug @ [ ssa.simulator.stepsdone.ToString() + "\t" 
  //                          + totalprop.ToString() + "\t"
  //                          + correctTotalProp.ToString() + "\t"
  //                          + diff.ToString() ]// TODO: remove
          (* Increase the "step counter" by one... *)
          let inc_stepsdone = ssa.simulator.stepsdone + 1 
          let ssa = { ssa with total_propensity = totalprop; simulator = {ssa.simulator with stepsdone = inc_stepsdone}} in
          loop false ssa remaining_events output
    let ssa = loop true ssa ssa.simulator.events output
    output_finals ssa.simulator.populations
    ssa
  member ssa.simulate_state () =
    let cancel = ref false 
    let result = ref [] 
    let output row = result := row::!result 
    let plots:string list = List.map Functional2.to_string_plot ssa.simulator.settings.plots
    let final_sim = ssa.simulate_callback cancel output ignore None
    final_sim, Table.from_rows_reverse plots !result
  member ssa.simulate_trajectories_callback cancel (output:Table<Point>->unit) = 
    let random = match ssa.simulator.settings.seed with None -> new Rng.Random() | Some s -> new Rng.Random(s)
    let dt = (ssa.simulator.settings.final - ssa.simulator.settings.initial) / (float ssa.simulator.settings.points)
    let times = List.init ssa.simulator.settings.points (fun i -> ssa.simulator.settings.initial + dt*(float i))  
    let populations = ssa.simulator.populations
    let plots = ssa.simulator.settings.plots |> List.map Functional2.to_string_plot 
    let zeros : Point[][] = Array.create plots.Length (Array.create ssa.simulator.settings.points {mean=0.0; stdev=0.0})
    let result = Table<Point>.from_array_columns times zeros plots
    let rec loop i (point_result:Table<Point>) = 
      if (i >= ssa.settings.trajectories || !cancel)
      then ()
      else
        let seed = 
          match ssa.simulator.settings.seed with
          | None   -> random.Next()
          | Some s -> s+i 
        let individual_result = ref [] 
        let internal_output row = individual_result := row::!individual_result 
        let ssa_local = { ssa with simulator = { ssa.simulator with populations = populations.clone; settings = { ssa.simulator.settings with seed = Some seed } } } 
        let _ = ssa_local.simulate_callback (ref false) internal_output ignore None
        let rows = Row<float>.interpolate_reverse Row<float>.zero_order_hold (List.rev !individual_result) times
        let plots:string list = List.map Functional2.to_string_plot ssa.simulator.settings.plots
        let trajectory = Table.from_rows_reverse plots rows
        let new_columns : Column<Point> list = 
          (point_result.columns, trajectory.columns) 
          ||> List.map2 (fun point_col new_col -> 
            { name = new_col.name
            ; values = 
              (point_col.values, new_col.values)
              ||> List.map2 (fun point vnew -> 
                let new_mean, new_var = Statistics.online_meanvar point.mean (point.stdev*point.stdev) vnew (i+1)
                { mean = new_mean; stdev = sqrt new_var }
              )
            }
          )
        let new_result = { point_result with columns = new_columns }
        output new_result
        loop (i+1) new_result
    loop 0 result
  member ssa.simulate_trajectories () = 
    let result = ref Table<Point>.empty
    let cancel = ref false
    let output (new_result:Table<Point>) = result := new_result
    ssa.simulate_trajectories_callback cancel output
    !result
  member ssa.simulate_action action =   
    if ssa.settings.trajectories = 1 && List.isEmpty ssa.simulator.settings.times
    then 
      let cancel = ref false 
      let result = ref [] 
      let output (row:Row<float>) = action row.time; result := row::!result 
      let plots:string list = List.map Functional2.to_string_plot ssa.simulator.settings.plots
      let _ = ssa.simulate_callback cancel output ignore None
      Table.from_rows_reverse plots !result
    else
      let populations = ssa.simulator.populations
      let dt = (ssa.simulator.settings.final - ssa.simulator.settings.initial) / (float ssa.simulator.settings.points)
      let times = List.init ssa.simulator.settings.points (fun i -> ssa.simulator.settings.initial + dt*(float i))
      let base_seed = (match ssa.simulator.settings.seed with None -> new Rng.Random() | Some s -> new Rng.Random(s)).Next()
      let do_simulation i = 
        //printfn "  Simulation %d: seed %d" i seed;
        let cancel = ref false 
        let result = ref [] 
        let output row = result := row::!result     // Don't do action when there are multiple trajectories
        let plots:string list = List.map Functional2.to_string_plot ssa.simulator.settings.plots
        let ssa_local = { ssa with simulator = { ssa.simulator with populations = populations.clone; settings = { ssa.simulator.settings with seed = Some (base_seed+i) };  } } 
        let _ = ssa_local.simulate_callback cancel output ignore None
        let rows = Row<float>.interpolate_reverse Row<float>.zero_order_hold (List.rev !result) times
        Table.from_rows_reverse plots rows
      let results = 
        if ssa.simulator.settings.multicore
        then
          #if JavaScript
          failwith "Can't use Async in JavaScript (directive simulation {multicore=True})"
          #else
          let job = Async.Parallel(List.init ssa.settings.trajectories (fun i -> async { return do_simulation i })) in
          Async.RunSynchronously job |> List.ofArray
          #endif
        else 
          List.init ssa.settings.trajectories do_simulation
      Table.concat results
  member ssa.simulate () = ssa.simulate_action (fun _ -> ())
  static member set_number_of_points num_points (ssa:Ssa) = 
      { ssa with simulator = {ssa.simulator with settings = { ssa.simulator.settings with points = num_points }}}
  static member private get_plot_indices (pops: Populations<Species,float>) (plots:Expression.t<Inlined<int>> list) = 
    plots 
    |>  List.map (fun p -> 
      match p with 
      | Expression.Key (Inlined.Species index) -> 
        match pops.index_to_species.[index].max with
          | Some max -> index, max 
          | None -> failwithf "No maximum specified for plot species %s" pops.index_to_species.[index].species.name
      | _ -> failwithf "Currently do not support expressions"
    )
  /// Evaluate the stationary distribution from a single SSA simulation
  static member simulate_with_stationary (ssa:Ssa) : (Ssa * Table<float> * Map<string,double []>) = 
    let cancel = ref false 
    // Prepare for outputting the trajectory
    let result = ref []
    let output row = result := row::!result
    // Prepare for outputting the stationary distribution
    let skip_time = match ssa.settings.stationary_skiptime with Some v -> v | None -> ssa.simulator.settings.initial
    let pop_indices = Ssa.get_plot_indices ssa.simulator.populations (ssa.plots |> Array.toList)
    let species_names:string list = List.map Functional2.to_string_plot ssa.simulator.settings.plots
    let mutable stationary_distribution = pop_indices |> List.toArray |> Array.map (fun (i, m) -> (Array.zeroCreate<double> (m + 1)))
    let mutable total_time = 0.0
    let mutable prev_pops = ssa.simulator.populations
    // Check this works
    let update_distribution t dt (pops:Populations<Species, float>) = 
      if (t > skip_time)
      then
        total_time <- total_time + dt;
        pop_indices |> List.iteri (fun i (index,max) -> 
          let p = prev_pops.index_to_species.[index]
          let v = int p.value
          if v <= max then
            stationary_distribution.[i].[v] <- stationary_distribution.[i].[v] + dt )
      prev_pops <- pops
    let final_sim = ssa.simulate_callback cancel ignore ignore (Some update_distribution)
    let stationary_output =
      species_names
      |> List.mapi (fun i k -> k, stationary_distribution.[i] |> Array.map (fun x -> x/total_time))
      |> Map.ofList
    final_sim, (Table.from_rows_reverse species_names !result), stationary_output
