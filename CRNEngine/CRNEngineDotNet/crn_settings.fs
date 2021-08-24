// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open System.Diagnostics
open Operators
[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Crn_settings<'e> when 'e:equality = 
  {
    simulation      : Simulation_settings<'e>
    simulations     : Simulation_settings<'e> list
    stochastic      : Stochastic_settings
    deterministic   : Deterministic_settings
    spatial         : Spatial_settings<'e>
    inference       : Inference_settings
    moment_closure  : Moment_closure_settings.t<Moment_closure_settings.monomial>
    synthesis       : Synthesis_settings
    data            : Dataset list
    units           : Units
    simulator       : Simulator
    parameters      : Parameter list
    sweeps          : Sweep list
    rates           : Map<string, 'e>
    plot            : Plot_settings<'e>
    quiet           : bool
  }
  static member defaults = {
    simulation    = Simulation_settings.defaults
    simulations   = []
    stochastic    = Stochastic_settings.defaults
    deterministic = Deterministic_settings.defaults
    spatial       = Spatial_settings.defaults
    inference     = Inference_settings.defaults
    moment_closure= Moment_closure_settings.defaults
    synthesis     = Synthesis_settings.defaults
    data          = []
    units         = Units.defaults
    simulator     = Simulator.defaults
    parameters    = []
    sweeps        = []
    rates         = Map.empty
    plot          = Plot_settings.defaults
    quiet         = false
  }
  member s.map (f:'e -> 'b) = { 
    simulation    = s.simulation.map f 
    simulations   = List.map (fun (s:Simulation_settings<'e>) -> s.map f) s.simulations
    stochastic    = s.stochastic
    deterministic = s.deterministic
    spatial       = Spatial_settings<'e>.map f s.spatial
    inference     = s.inference
    moment_closure= s.moment_closure
    synthesis     = s.synthesis
    data          = s.data
    units         = s.units
    simulator     = s.simulator
    parameters    = s.parameters
    sweeps        = s.sweeps
    rates         = Map.map (fun a b -> f b) s.rates
    plot          = s.plot.map f
    quiet         = s.quiet
  }
  member s.map_plots (f:'e -> 'b) (fplots:'e -> 'b) = { 
    simulation    = s.simulation.map fplots 
    simulations   = List.map (fun (s:Simulation_settings<'e>) -> s.map fplots) s.simulations
    stochastic    = s.stochastic
    deterministic = s.deterministic
    spatial       = Spatial_settings<'e>.map fplots s.spatial
    inference     = s.inference
    moment_closure= s.moment_closure
    synthesis     = s.synthesis
    data          = s.data
    units         = s.units
    simulator     = s.simulator
    parameters    = s.parameters
    sweeps        = s.sweeps
    rates         = Map.map (fun a b -> f b) s.rates
    plot          = s.plot.map fplots
    quiet         = s.quiet
  }
  member s.collect_plots (f:'e -> 'e list) = {
    simulation    = s.simulation.collect_plots f 
    simulations   = List.map (fun (s:Simulation_settings<'e>) -> s.collect_plots f) s.simulations
    stochastic    = s.stochastic
    deterministic = s.deterministic
    spatial       = s.spatial
    inference     = s.inference
    moment_closure= s.moment_closure
    synthesis     = s.synthesis
    data          = s.data
    units         = s.units
    simulator     = s.simulator
    parameters    = s.parameters
    sweeps        = s.sweeps
    rates         = s.rates
    plot          = s.plot
    quiet         = s.quiet
  }
  member s.update_empty_plot_labels () = 
    let (x_label:string,y_label:string) = s.simulator.plot_label s.spatial.dimensions s.units
    {s with plot = s.plot.update_empty_labels x_label y_label}
  member top.to_inference_runs (l:Crn_settings<'e> list) =
    let f1 (s:Crn_settings<'e>) =
      let f2 (simulation:Simulation_settings<'e>) = 
        let data:Dataset list = simulation.filter_data s.data
        let tables:Table<float> list = 
          List.collect (fun (ds:Dataset) ->
            ds.data 
            |> List.map (fun table -> 
              if (simulation.final < List.max table.times)
              then Table<float>.filter_by_tmax simulation.final table
              else table
            )
          ) data
        let sweeps:Sweep list = simulation.filter_sweeps s.sweeps
        match (data.Length, sweeps.Length) with
        | (1,0) -> ()
        | nd, ns -> if nd <> ns then failwithf "Incompatible number of sweeps and data in simulation %s" simulation.name
        let simulations:Simulation_settings<'e> list = 
          List.map (fun (table:Table<float>) -> simulation.update_times table.times) tables
        simulations,tables,sweeps
      let simulations = if s.simulations = [] then [s.simulation] else s.simulations
      let simulations_list,tables_list,sweeps_list = List.unzip3 (List.map f2 simulations)
      List.concat simulations_list, List.concat tables_list, List.concat sweeps_list
    let simulations_list,tables_list,sweeps_list = List.unzip3 (List.map f1 l)
    let sweeps_list = if sweeps_list = [[]] then [[Sweep.create ()]] else sweeps_list
    //let instance_names = sweeps_list |> List.map (fun sweeps -> sweeps |> List.collect (fun sw -> sw.merge.to_string_bindings))
    let sweeps_list, parameters,paramsMap = Parameters.expand_multiples sweeps_list top.parameters
    parameters, tables_list, sweeps_list, simulations_list, paramsMap
  member s.to_simulation_runs (model:string) = 
    let environment:Environment.t = Parameters.to_env s.parameters
    let f (simulation:Simulation_settings<'e>) = 
      let data:Dataset list = simulation.filter_data s.data
      let tables:Table<float> list = (List.collect (fun (ds:Dataset) -> ds.data) data)
      let sweeps:Sweep list = simulation.filter_sweeps s.sweeps
      let f2 (sweep_name,assignment,e,name) = Instance.create(model,sweep_name,assignment,e,simulation,name)
      List.map f2 (Sweeps.eval_label environment sweeps), tables
    let simulations = if s.simulations = [] then [s.simulation] else s.simulations
    let l1,l2 = List.unzip (List.map f simulations)
    List.concat l1, List.concat l2  
  member s.update_times (times:float list) = {s with simulation = s.simulation.update_times times}
  member s.update_simulation (simulation:Simulation_settings<'e>) = {s with simulation = simulation}
  member s.substitute (e:Environment.t) = { s with parameters = Parameters.substitute e s.parameters }
  member s.to_string (fs:'e -> string) (fplots:'e -> string) =
    let list_to_multiline_string f l = sprintf "[\n  %s\n]" (String.concat ";\n  " (List.map f l))
    let rates_to_string rates = 
      //AP//rates |> Map.toList |> List.map (fun (name,expr) -> sprintf "%s = %s" name (fs expr))
      list_to_multiline_string (fun (name,expr) -> sprintf "%s = %s" name (fs expr)) (Map.toList rates)
    let simulations_to_string simulations =
      let f (s:Simulation_settings<'e>) = sprintf "%s = %s" s.name (s.to_string fplots)
      list_to_multiline_string f simulations
      //AP//list_to_multiline_string (fun (s:Simulation_settings<'e>) -> s.to_string fplots) simulations
    let defaults = Crn_settings<'e>.defaults 
    let f (b:bool) (d:string) (s:string) = if b then "" else sprintf "directive %s %s\n" d s in
      f (s.simulation = defaults.simulation) "simulation" (s.simulation.to_string fplots) +
      f (s.simulations = defaults.simulations) "simulations" (simulations_to_string s.simulations) +
      f (s.simulator = defaults.simulator) "simulator" (s.simulator.to_string) + 
      f (s.stochastic = defaults.stochastic) "stochastic" (Stochastic_settings.to_string s.stochastic) + 
      f (s.deterministic = defaults.deterministic) "deterministic" (Deterministic_settings.to_string s.deterministic) +
      f (s.units = defaults.units) "units" (s.units.to_string) + 
      f (s.spatial = defaults.spatial) "spatial" (Spatial_settings.to_string fplots s.spatial) +
      f (s.parameters = defaults.parameters) "parameters" (Parameters.to_string s.parameters) + 
      f (s.inference = defaults.inference) "inference" (Inference_settings.to_string s.inference) +
      f (s.sweeps = defaults.sweeps) "sweeps" (Sweep.list_to_string s.sweeps) + 
      f (s.data = defaults.data) "data" ("[" + String.concat "; " (List.map (fun d -> d.file) s.data) + "]") + 
      f (s.rates = defaults.rates) "rates" (rates_to_string s.rates)
  member settings.all_used_rates rateMention reactions =
    let reaction_rates = 
        reactions
        |> List.collect (fun r ->
            let fw = match r.rate with
                     | Rate.MassAction _ -> []
                     | Rate.Function   e -> e |> rateMention
            let bw = match r.reverse with
                     | None              -> []
                     | Some (Rate.MassAction _) -> []
                     | Some (Rate.Function   e) -> e |> rateMention
            fw @ bw
        )
    let simulation_rates = 
        settings.simulation.plots
        @ List.concat (settings.simulations |> List.map (fun i -> i.plots))
        |> List.collect rateMention
    let rec ref_rates (existing:string list) = 
        let referenced = existing |> List.collect (fun r -> rateMention settings.rates.[r]) |> Set.ofList
        let extra = Set.difference referenced (Set.ofList existing) |> Set.toList
        if List.isEmpty extra
        then existing
        else ref_rates (existing @ extra)
    
    ref_rates (reaction_rates @ simulation_rates)
    |> List.map (fun key -> key, settings.rates.[key])
    |> Map.ofList

  member settings.from_directive (directive:Directive<'e>) =
    match directive with
    | Simulation     s -> {settings with simulation = s}
    | Simulations    s -> {settings with simulations = s}
    | Simulator      s -> {settings with simulator = s}
    | Stochastic     s -> {settings with stochastic = s}
    | Deterministic  s -> {settings with deterministic = s}
    | Inference      s -> {settings with inference = s}
    | Parameters     s -> {settings with parameters = s} 
    | Rates          s -> {settings with rates = s} 
    | Sweeps         s -> {settings with sweeps = s}
    | Data           s -> {settings with data = List.map (fun si -> Dataset.empty si) s}
    | Spatial        s -> {settings with spatial = s}
    | Plot           s -> {settings with plot = s}
    | Units          s -> {settings with units = s}
    | Moment_closure s -> {settings with moment_closure = s}
    | Synthesis      s -> {settings with synthesis = s}

  static member checkSweepParameters settings : Crn_settings<'e> = 
    let p_initialised = settings.parameters |> List.map (fun p -> p.name) |> List.distinct
    settings.sweeps 
    |> List.collect (fun sw -> sw.mentions)
    |> List.iter (fun v -> 
      if not (List.contains v p_initialised)
      then failwithf "Sweep variable %s not initialised" v)
    settings

  member defaults.from_directive_list (ds:Directive<'e> list) = 
    List.fold (fun (s:Crn_settings<'e>) (d:Directive<'e>) -> s.from_directive d) defaults ds
    |> Crn_settings.checkSweepParameters

  static member parse_defaults (parse_rate:Parser.t<'e>) (parse_plot:Parser.t<'e>) (defs:Crn_settings<'e>) = 
    Directive.parse_list parse_rate parse_plot |>> defs.from_directive_list

  static member parse (parse_rate:Parser.t<'e>) (parse_plot:Parser.t<'e>) = 
    Crn_settings.parse_defaults parse_rate parse_plot Crn_settings<'e>.defaults

  static member from_string (parse_rate:Parser.t<'e>) (parse_plot:Parser.t<'e>) (s:string) = 
    Parser.from_string (Crn_settings<'e>.parse parse_rate parse_plot) s 
  

  member default_settings.from__default_directive (directive:Directive<'e>) =
    match directive with
    | Simulation     s -> {default_settings with simulation = s}
    | Simulations    s ->
        let filterSimulation (sim:Simulation_settings<'e>) = List.filter (fun (x:Simulation_settings<'e>) -> (x.name <> sim.name) ) 
        let defaultSimsReduced = s |> List.fold (fun acc sim -> filterSimulation sim acc) default_settings.simulations
        let extendedSims = defaultSimsReduced@s
        {default_settings with simulations = extendedSims}
    | Simulator      s -> {default_settings with simulator = s}
    | Stochastic     s -> {default_settings with stochastic = s}
    | Deterministic  s -> {default_settings with deterministic = s}
    | Inference      s -> {default_settings with inference = s}
    | Parameters     s -> 
        let filterParameters (param:Parameter) = List.filter (fun (x:Parameter) -> x.name <> param.name)
        let defaultParamsReduced = s |> List.fold (fun acc param -> filterParameters param acc) default_settings.parameters
        let extendedParams = defaultParamsReduced@s
        {default_settings with parameters = extendedParams} 
    | Rates          s ->
        let extendedRates = (s |> Map.toList) |> List.fold (fun (map:Map<string,'e>) (x,y) -> map.Add(x,y)) default_settings.rates
        {default_settings with rates = extendedRates} 
    | Sweeps         s -> 
        let filterSweeps (sweep:Sweep) = List.filter (fun (x:Sweep) -> x.name <> sweep.name)
        let defaultSweepsReduced = s |> List.fold (fun acc sweep -> filterSweeps sweep acc) default_settings.sweeps
        let extendedSweeps = defaultSweepsReduced@s
        {default_settings with sweeps = extendedSweeps} 
    | Data           s -> {default_settings with data = List.map (fun si -> Dataset.empty si) s}
    | Spatial        s -> {default_settings with spatial = s}
    | Plot           s -> {default_settings with plot = s}
    | Units          s -> {default_settings with units = s}
    | Moment_closure s -> {default_settings with moment_closure = s}
    | Synthesis      s -> {default_settings with synthesis = s}

  member defaults.from_default_directive_list (ds:Directive<'e> list) = 
    List.fold (fun (s:Crn_settings<'e>) (d:Directive<'e>) -> s.from__default_directive d) defaults ds


  static member parse_directive_defaults (defaults:Crn_settings<'e>) (parse_rate:Parser.t<'e>) (parse_plot:Parser.t<'e>) =
    let safe_map_of_list bindings = 
      bindings 
      |> List.fold (fun m (k,v) -> 
        if Map.containsKey k m 
        then raise (new Errors.EngineException (sprintf "Failed trying to parse duplicate rate expression %s" k)) 
        else Map.add k v m
      ) Map.empty
    let parse_rates:Parser.t<Map<string,'e>> = 
      Parser.list_of (Parser.name .>>. (Parser.skw "=" >>. parse_rate)) >>= (safe_map_of_list >> Parser.preturn) 
    Parser.choice [ 
      Parser.kw "deterministic" >>. Deterministic_settings.parse_defaults defaults.deterministic |>> Deterministic;
      Parser.kw "stochastic"    >>. Stochastic_settings.parse_defaults defaults.stochastic |>> Stochastic;
      Parser.kw "simulations"   >>. Parser.list_of (Simulation_settings.parse_named_defaults defaults.simulation parse_plot) |>> Simulations;
      Parser.kw "simulation"    >>. Simulation_settings.parse_defaults defaults.simulation parse_plot |>> Simulation;
      Parser.kw "simulator"     >>. Simulator.parse |>> Simulator; 
      Parser.kw "parameters"    >>. Parser.list_of Parameter.parse |>> Parameters;
      Parser.kw "sweeps"        >>. Parser.list_of Sweep.parse |>> Sweeps;
      Parser.kw "data"          >>. Parser.list_of Parser.name |>> Data;
      Parser.kw "inference"     >>. Inference_settings.parse_defaults defaults.inference |>> Inference;
      Parser.kw "spatial"       >>. Spatial_settings.parse_defaults defaults.spatial parse_plot |>> Spatial;
      Parser.kw "rates"         >>. parse_rates |>> Rates;
      Parser.kw "plot_settings" >>. Plot_settings.parse_defaults defaults.plot parse_plot |>> Plot;
      Parser.kw "moments"       >>. Moment_closure_settings.parse_defaults defaults.moment_closure |>> Moment_closure
      Parser.kw "units"         >>. Units.parse |>> Units
    ]
  static member parse_directive (parse_rate:Parser.t<'e>) (parse_plot:Parser.t<'e>) = 
    Crn_settings.parse_directive_defaults Crn_settings<'e>.defaults parse_rate parse_plot

  static member parse_directives_defaults (defaults:Crn_settings<'e>) (parse_rate:Parser.t<'e>) (parse_plot:Parser.t<'e>) = 
    let parse_keyword:Parser.t<Directive<'e>> = 
      Parser.kw "directive" >>. Crn_settings<'e>.parse_directive_defaults defaults parse_rate parse_plot
    Parser.sepBy parse_keyword Parser.spaces

  static member parse_directives (parse_rate:Parser.t<'e>) (parse_plot:Parser.t<'e>) = 
    Crn_settings<'e>.parse_directives_defaults Crn_settings<'e>.defaults parse_rate parse_plot