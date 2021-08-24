// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Directive<'s> when 's:equality =
  | Simulation     of Simulation_settings<'s>
  | Simulations    of Simulation_settings<'s> list
  | Simulator      of Simulator
  | Stochastic     of Stochastic_settings
  | Deterministic  of Deterministic_settings
  | Inference      of Inference_settings
  | Parameters     of Parameter list
  | Rates          of Map<string,'s>
  | Sweeps         of Sweep list
  | Data           of string list
  | Spatial        of Spatial_settings<'s>
  | Plot           of Plot_settings<'s>
  | Units          of Units
  | Moment_closure of Moment_closure_settings.t<Moment_closure_settings.monomial>
  | Synthesis      of Synthesis_settings
  //TODO: delete the functions below and call the ones in Crn_settings instead.
  static member parse (parse_rate:Parser.t<'s>) (parse_plot:Parser.t<'s>) =
    let safe_map_of_list bindings = 
      bindings 
      |> List.fold (fun m (k,v) -> 
        if Map.containsKey k m 
        then raise (new Errors.EngineException (sprintf "Failed trying to parse duplicate rate expression %s" k)) 
        else Map.add k v m
      ) Map.empty
    let parse_rates:Parser.t<Map<string,'s>> = 
      Parser.list_of (Parser.name .>>. (Parser.skw "=" >>. parse_rate)) >>= (safe_map_of_list >> Parser.preturn) 
    Parser.choice [ 
      Parser.kw "deterministic" >>. Deterministic_settings.parse |>> Deterministic;
      Parser.kw "stochastic"    >>. Stochastic_settings.parse |>> Stochastic;
      Parser.kw "simulations"   >>. Parser.list_of (Simulation_settings.parse_named parse_plot) |>> Simulations;
      Parser.kw "simulation"    >>. Simulation_settings.parse parse_plot |>> Simulation;
      Parser.kw "simulator"     >>. Simulator.parse |>> Simulator; 
      Parser.kw "parameters"    >>. Parser.list_of Parameter.parse |>> Parameters;
      Parser.kw "sweeps"        >>. Parser.list_of Sweep.parse |>> Sweeps;
      Parser.kw "data"          >>. Parser.list_of Parser.name |>> Data;
      Parser.kw "inference"     >>. Inference_settings.parse |>> Inference;
      Parser.kw "spatial"       >>. Spatial_settings.parse parse_plot |>> Spatial;
      Parser.kw "rates"         >>. parse_rates |>> Rates;
      Parser.kw "plot_settings" >>. Plot_settings.parse parse_plot |>> Plot;
      Parser.kw "moments"       >>. Moment_closure_settings.parse |>> Moment_closure
      Parser.kw "synthesis"     >>. Synthesis_settings.parse |>> Synthesis 
      Parser.kw "units"         >>. Units.parse |>> Units
    ]
  static member parse_list (parse_rate:Parser.t<'s>) (parse_plot:Parser.t<'s>) = 
    let parse_keyword:Parser.t<Directive<'s>> = 
      Parser.kw "directive" >>. Directive<'s>.parse parse_rate parse_plot
    Parser.sepBy parse_keyword Parser.spaces
