namespace Microsoft.Research.CRNEngine

open Operators
open Parser
open System.Diagnostics

[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Simulation_settings<'e> when 'e:equality = 
  {
    name:string
    initial:float
    final:float
    points:int
    plots:'e list
    plotcolours: string list
    seed: int option;
    kinetics:Kinetics
    times:float list 
    prune:bool
    multicore:bool
    data:string list
    sweeps:string list
  }
  static member defaults = {
    name      = ""
    initial   = 0.0
    final     = 1000.0
    points    = 1000
    plots     = []
    plotcolours = []
    seed      = None
    kinetics  = Contextual
    times     = []
    prune     = true
    multicore = false
    data = []
    sweeps = []
  }
  member s.map (f:'e -> 'e2) = {
    name      = s.name
    initial   = s.initial
    final     = s.final
    points    = s.points
    plots     = List.map f s.plots
    plotcolours = s.plotcolours
    seed      = s.seed
    kinetics  = s.kinetics
    times     = s.times
    prune     = s.prune
    multicore = s.multicore
    data = s.data
    sweeps = s.sweeps
  }
  member s.collect_plots (f:'e -> 'e list) =
    // If one of the plots has an assigned plot colour, then I'm assigning the same plot colours to all of the collected plots.
    let (plots,plotcolours) = s.plots |> List.mapi (fun i plot -> f plot |> List.map (fun p -> (p, if s.plotcolours.Length > i then s.plotcolours.[i] else ""))) |> List.concat |> List.unzip
    { name      = s.name
      initial   = s.initial
      final     = s.final
      points    = s.points
      plots     = plots
      plotcolours = if List.exists (fun s->s<>"") plotcolours then plotcolours else []
      seed      = s.seed
      kinetics  = s.kinetics
      times     = s.times
      prune     = s.prune
      multicore = s.multicore
      data = s.data
      sweeps = s.sweeps
    }
  member s.filter_data (data:Dataset list) = 
    if s.data = [] 
    then data 
    else
      let f (name:string) = 
        try List.find (fun (ds:Dataset) -> ds.file = name) data
        with _ -> failwith ("dataset " + name + " not found in settings"  )
      List.map f s.data
  member s.filter_sweeps (sweeps:Sweep list)= 
    if s.sweeps = []
    then sweeps
    else 
      let f (name:string) = 
        try List.find (fun (sweep:Sweep) -> sweep.name = name) sweeps
        with _ -> failwith ("sweep " + name + " not found in settings"  )
      List.map f s.sweeps
  member s.update_times (times:float list) = {s with times = times}
  member s.to_string_defaults (defaults:Simulation_settings<'e>) (fe:'e -> string) =
    "{" +
    (if s.initial = defaults.initial then "" else sprintf "initial=%s; " (s.initial.ToString())) +
    (if s.final = defaults.final then "" else sprintf "final=%s; " (s.final.ToString())) +
    (if s.points = defaults.points then "" else sprintf "points=%s; " (s.points.ToString())) +
    (if s.plots = defaults.plots then "" else sprintf "plots=[%s]; " (String.concat "; " (List.map fe s.plots))) +
    (if List.exists (fun c -> c <> "") s.plotcolours then sprintf "plotcolours=[%s]; " (String.concat "; " (s.plotcolours |> List.map (fun s -> "\""+s+"\""))) else "") +
    (if s.seed = defaults.seed then "" else sprintf "seed=%d" s.seed.Value) +
    (if s.kinetics = defaults.kinetics then "" else sprintf "kinetics=%s; " s.kinetics.to_string) +
    (if s.times = defaults.times then "" else sprintf "times=[%s]; " (String.concat "; " (List.map (fun f -> f.ToString()) s.times))) +
    (if s.prune = defaults.prune then "" else sprintf "prune=%s; " (s.prune.ToString())) +
    (if s.multicore = defaults.multicore then "" else sprintf "multicore=%s; " (s.multicore.ToString())) +
    (if s.data = defaults.data then "" else sprintf "data=[%s]; " (String.concat "; " s.data)) +
    (if s.sweeps = defaults.sweeps then "" else sprintf "sweeps=[%s]; " (String.concat "; " s.sweeps)) +
    "}"
  member s.to_string (fe:'e -> string) = s.to_string_defaults Simulation_settings<'e>.defaults fe
  member s.get_print_interval () = 
    if (s.points > 0) then (s.final - s.initial) / ((float) s.points) else 0.0  
  static member parse_defaults (defaults:Simulation_settings<'e>) (pe:Parser.t<'e>) =
    Parser.record defaults [ 
      "initial", Parser.pfloat .>> Parser.spaces |>> fun d (s:Simulation_settings<'e>) -> { s with initial = d }
      "final",   Parser.pfloat .>> Parser.spaces |>> fun d (s:Simulation_settings<'e>) -> { s with final = d }
      "points",  Parser.pint32 .>> Parser.spaces |>> fun d (s:Simulation_settings<'e>) -> { s with points = d }
      "plots",   Parser.list_of (pe .>> Parser.spaces) |>> fun d (s:Simulation_settings<'e>) -> { s with plots = d }
      "plotcolours", Parser.list_of (Parser.pcolour .>> Parser.spaces) |>> fun d (s:Simulation_settings<'e>) -> { s with plotcolours = d }
      "seed",    Parser.pint32 .>> Parser.spaces |>> fun d (s:Simulation_settings<'e>) -> { s with seed = Some d }
      "kinetics", Kinetics.parse .>> Parser.spaces |>> fun d (s:Simulation_settings<'e>) -> { s with kinetics = d }
      "prune",   Parser.pbool .>> Parser.spaces |>> fun d (s:Simulation_settings<'e>) -> { s with prune = d }
      "multicore",Parser.pbool .>> Parser.spaces |>> fun d (s:Simulation_settings<'e>) -> { s with multicore = d }
      "data",   Parser.list_of (Parser.name .>> Parser.spaces) |>> fun d (s:Simulation_settings<'e>) -> { s with data = d }
      "sweeps",   Parser.list_of (Parser.name .>> Parser.spaces) |>> fun d (s:Simulation_settings<'e>) -> { s with sweeps = d }
    ] 
  static member parse (pe:Parser.t<'e>) = Simulation_settings<'e>.parse_defaults (Simulation_settings<'e>.defaults) pe
  static member from_string (pe:Parser.t<'e>) (s:string) = Parser.from_string (Simulation_settings.parse pe) s
  static member parse_named_defaults (defaults:Simulation_settings<'e>) (pe:Parser.t<'e>) = 
    (Parser.name .>>. (Parser.skw "=" >>. (Simulation_settings.parse_defaults defaults pe))) 
    |>> fun (n:string,s:Simulation_settings<'e>) -> {s with name = n} 
  static member parse_named  (pe:Parser.t<'e>) = Simulation_settings.parse_named_defaults Simulation_settings<'e>.defaults pe
