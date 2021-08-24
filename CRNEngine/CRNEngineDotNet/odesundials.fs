// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open System
open Microsoft.FSharp.NativeInterop
open Microsoft.Research.CRNEngine.Sundials.external_sundials

#nowarn "9" //Native calls

[<JavaScript>]
type OdeSundials = 
  {
    simulator: Simulation<Functional>;
    plots: Expression.t<Inlined<int>> array
    reactions: Reaction<int,float,Lambda<Inlined<int>>> list;
    settings: Deterministic_settings;
    concs: float []
  }
  /// Simulate an evaluated OdeSundials type.
  member s.simulate () = 
    //We could have a separate create method as in the Oslo ode.
    let simulator = s.simulator
    let reactions = s.reactions
    let sim_settings = simulator.settings
    //let times_list = Simulation_settings.get_times sim_settings
    let pops = simulator.populations
    let starttime = sim_settings.initial
    let endtime = sim_settings.final
    let event_matters (ev:Event<Species, float, float>) = // RLP: These things are currently ensured by the simulator
      ev.time >= simulator.currenttime &&
      match ev.target with
      | Target.Species pop -> Array.exists (fun p -> p <> 0.0) (pop.get_pop_array )
      | _ -> true  
    let times_list, events = 
      if sim_settings.times <> [] 
      then 
        let events = 
          simulator.events 
          |> List.filter event_matters (* Events sorted by time, evaluated by the environment *)
          |> List.map (fun ev -> 
            match ev.target with 
            | Target.OutputPoint _ -> { ev with time = List.max sim_settings.times + 1.0 } 
            | _ -> ev)  (* Reset Output time to last requested time-point *)
        sim_settings.times, events
      else 
        let starttime = sim_settings.initial
        let printinterval = sim_settings.get_print_interval()
        // ND: Make the time vector one larger than requested, to ensure the final time is included.
        let times = List.init (sim_settings.points+1) (fun index -> starttime + float index * printinterval)
        let events = simulator.events |> List.filter event_matters (* Events sorted by time, evaluated by the environment *)
        let all_times = List.append times (events |> List.collect (fun ev -> 
          match ev.target with Target.OutputPoint -> [] | _ -> [ev.time-1e-8; ev.time]) )
        List.sort all_times, events
    let output_times:float[] = Array.ofList times_list  
    let numSpecies = pops.get_count 
    let numPrintSpecies = s.plots.Length
    // Partition the reactions into mass action and functional
    let reactionsMassAction = reactions |> List.filter (fun r -> 
      match r.rate with | Rate.MassAction _ -> true | _ -> false)
    let ratesMassAction =
       reactionsMassAction
       |> List.map (fun r ->
            match r.rate with
            | Rate.MassAction(r) -> r
            | _ -> failwith "Unexpected reaction type")
       |> List.toArray
    let numReactionsM = List.length reactionsMassAction
    let stoichInt species (reaction:Reaction<int,float,Lambda<Inlined<int>>>) =
      let constant = pops.is_constant  species
      (int)(reaction.getStoich constant species)
    (* Create the mass action variables needed by the Sundials solver *)
    let speciesIndices = seq {0..(numSpecies-1)}
    let stoichM = reactionsMassAction |> Seq.collect (fun reaction -> 
      Seq.map (fun s -> stoichInt s reaction) speciesIndices) |> Array.ofSeq  
    (* powers with zeros *)
    let powersM = Array.create (numSpecies*numReactionsM) 0
    reactionsMassAction |> List.iteri (fun ri reaction -> 
      reaction.reactants |> Mset.iterm (fun entry -> powersM.[numSpecies*ri + entry.element] <- entry.multiplicity))
    (* deal with functional rates *)
    let reactionsFunctional = 
      reactions |> List.filter (fun r -> match r.rate with | Rate.Function _ -> true | _ -> false)
    let numReactionsF = List.length reactionsFunctional
    let stoichF = 
      reactionsFunctional 
      |> Seq.collect (fun reaction -> Seq.map (fun s -> stoichInt s reaction) speciesIndices) |> Array.ofSeq
    (* Create call backs to run any functional rates *)
    let functionalRateFunctions = Array.zeroCreate numReactionsF
    reactionsFunctional |>
      List.iteri (fun ri (r:Reaction<_,_,_>) ->
        match r.rate with
        | Rate.Function lambda -> functionalRateFunctions.[ri] <- lambda
        | _ -> failwith "Unexpected reaction type"
        )
    //Called back from the native Sundials code to calculate functional rates
#if JavaScript
    let functionRatesApplyer (time:float) (conc:float[]) =
      let fluxesResult = Array.create numReactionsF 1.0
      // compute rate
      for r in 0..(numReactionsF-1) do
        let conc index =
          match index with 
          | Inlined.Species i -> conc.[i]
          | Inlined.Time      -> time
          //| Inlined.IRate    r -> resolvedRatesEnv.[r]
        fluxesResult.[r] <- functionalRateFunctions.[r].key conc
      fluxesResult
 #else
    let functionRatesApplyer (time:float) (fluxes:nativeptr<float>) (nativeconc:nativeptr<float>) =    
      // compute rate
      for r in 0..(numReactionsF-1) do
      let conc index =
        match index with 
        | Inlined.Species i -> NativePtr.get nativeconc i
        | Inlined.Time      -> time
        //| Inlined.IRate    r -> resolvedRatesEnv.[r]
      NativePtr.set fluxes r (functionalRateFunctions.[r].key conc)
 #endif
    let functionRatesApplyerDelegate = new FunctionalRates(functionRatesApplyer)

    //Preallocate the right sized array
    //let storage = ref [||]
    let storage : System.Collections.Generic.List<float>[] = 
      Array.init numPrintSpecies (fun _ -> new System.Collections.Generic.List<float>())
    let printspecieslist = s.plots
    let specieslookup = Array.map Expression.to_lambda printspecieslist
    let simulate_event_worth(currenttime, concs, stepsdone) (ev:Event<Species, float, float>) =  
      let eventtime = ev.time
      match Array.tryFindIndex (fun t -> t >= currenttime) output_times with
      | None -> (currenttime, (concs:double []), stepsdone) (* Last event has stepped out of simulated interval *)
      | Some first_index ->
          let last_index_plus1 = 
            match Array.tryFindIndex (fun t -> t > eventtime) output_times with
            | None      -> Array.length output_times
            | Some(loc) -> loc      
          let inner_n = last_index_plus1 - first_index
          let inner_times = Array.sub output_times first_index inner_n      
          (* If the output_times don't include the starttime already, prepend to the array *)
          let (pre_times, pre_n, added_currenttime) =
            if (inner_n = 0)
            then ([|currenttime|], 1, true)
            else
              if (inner_times.[0] > currenttime)
              then (Array.append [|currenttime|] inner_times, inner_n + 1, true)
              else (inner_times, inner_n, false)      
          (* If the output_times don't include the next event time, append to the array *)
          let (times, numtimes, added_endtime) = 
            if (pre_times.[pre_n-1] < eventtime)
            then (Array.append pre_times [|eventtime|], pre_n + 1, true)
            else (pre_times, pre_n, false)                
          (* we currently extract values as we go which saves intermediate memory but requires repeated lookups
             check this isn't costing us too much, we don't need intermediate results with this solver type *)
          #if JavaScript
          let sundialsOutput (time:float) (conc:float array) =
          #else
          let sundialsOutput (time:float) (conc:nativeptr<float>) =
              let conc_lookup = NativePtr.get conc
          #endif
              (* Outputs have some detailed logic around events - we send the time to Sundials but it musn't appear in the output.
              There may be a better way to go about doing this. Colin *)
              //if (((not added_currenttime) || time <> currenttime) && ((not added_endtime) || time <> eventtime)) then
              if (((not added_currenttime) || time <> currenttime) && ((time = endtime && not added_endtime) || time <> eventtime)) then                  
                  let rec keySolver k = 
                    match k with 
                    #if JavaScript
                    | Inlined.Species i -> conc.[i]
                    #else
                    | Inlined.Species i -> conc_lookup i
                    #endif
                    | Inlined.Time      -> time
                  //Row orientated approach
                  //let outputdata = List.map (fun ip -> ip conc_lookup) specieslookup
                  //outer_solution.Add(time :: outputdata)
                  //Column orientated approach
                  specieslookup 
                    |> Array.iteri (fun index ip -> storage.[index].Add(ip.key keySolver))
              ()          
          let sundialsOutputDelegate = new SundialsOutput(sundialsOutput)      
          // during this function, output() will be called back into          
          #if JavaScript
          // we need explicit return of "conc" here
          let sundialsOutputCode,concs =
          #else
          // concs will be changed inside fnSundialsSolver execution via ptr accessing
          let sundialsOutputCode =
          #endif
            fnSundialsSolver(numSpecies, numReactionsM, stoichM, powersM, ratesMassAction, numReactionsF, stoichF,functionRatesApplyerDelegate, concs, times, numtimes, sundialsOutputDelegate, s.settings.stiff, s.settings.abstolerance, s.settings.reltolerance)          
          if sundialsOutputCode <> 0 then raise (Errors.SimulatorErrorException (sprintf "Sundials solver failed with code: %i" sundialsOutputCode))
          (* Enact the event *)
          let concs =
              match ev.target with
              | Target.Species event_pop ->
                  let pop_map =
                      event_pop.get_pop_info_list 
                      |> List.map (fun pi -> 
                          ( match pops.tryFind_index pi.species with
                          | Some i -> i
                          | None   -> failwith ("Unknown species in event")) // Would be better to also return the string of the species, but need a namer.
                          , pi.value)
                      |> Map.ofList
                  let vals = Array.mapi (fun i x -> match Map.tryFind i pop_map with Some perturbation -> x + perturbation | None -> x) concs
                  vals
              | _ -> concs      
          (eventtime, concs, stepsdone + numtimes)
    (* Inside the event *)
    let (endtime, concs, stepsdone) = List.fold simulate_event_worth (starttime, s.concs, 0) events
    //Switch to preallocation
    let raw_data = storage |> Seq.map Array.ofSeq |> Array.ofSeq
    let plots:string list = List.map Functional2.to_string_plot sim_settings.plots
    Table.from_array_columns times_list raw_data plots

/// The OdeSundials type is parameterized (by values) or instantiated from an environment
[<JavaScript>]
type SundialsAbstract = 
  {
    species: Map<Species,int>
    settings: Crn_settings<Functional>;
    reactions: Reaction<Species,Value,Functional> list;
    initials: Initial<Species,Value> list;
  }
  static member create species_ids reactions initials settings = {
    species = species_ids;
    reactions = reactions;
    initials = initials;
    settings = settings;
  }
  /// Evaluate the environment stored inside settings, producing concrete initial conditions and reactions
  member ode.evaluate () : OdeSundials =
    let env:Environment.t = Parameters.to_env ode.settings.parameters
    let ratesEnv = Key.inline_rates_env env ode.settings.rates
    let populations, events = Initial<Species,Value>.to_initialpops_events env ode.settings.simulation.initial ode.initials
    let simulator = Simulation.create populations events ode.settings.simulation 1.0
    let concs = populations.get_pop_array 
    let sreactions = 
      Reaction<Species,Value,Functional>.get_sim_reactions_products 1.0 populations env ratesEnv ode.reactions 
      |> List.map Lib.fst3
    let indexKey = Inlined.map (fun sp -> match populations.tryFind_index sp with Some k -> k | None -> failwithf "Species %s not found in model" sp.name)
    { 
      simulator = simulator; 
      plots = simulator.settings.plots 
              |> List.map (Key.inline_keys env ratesEnv 
                  |> Expression.expand
                  >> Expression.simplify
                  >> Expression.map indexKey) 
              |> Array.ofList
      settings = ode.settings.deterministic;
      reactions = sreactions;
      concs = concs
      //rates = Hashtable.map (Expression.map indexKey) ratesEnv
    }