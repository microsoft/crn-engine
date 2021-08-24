// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Crn_settings 
open WebSharper

type table<'v> = Table.t<'v>
type simulation_settings<'s> when 's:equality = Simulation_settings.t<'s>
type ode_settings<'s> when 's:equality = Ode_settings.t<'s>

type simulator = 
  | [<Constant "Oslo">] Oslo
  | [<Constant "Sundials">] Sundials
  | [<Constant "SSA">] SSA
  | [<Constant "CME">] CME
  | [<Constant "LNA">] LNA
  | [<Constant "PDE">] PDE
  | [<Constant "MC">]  MC

type t<'s> when 's:equality = {
  simulation: simulation_settings<'s>;
  stochastic: Stochastic_settings.t;
  deterministic: Deterministic_settings.t;
  spatial: Spatial_settings.t<'s>;
  inference: Inference_settings.t;
  moment_closure  : Moment_closure_settings.t<Moment_closure_settings.monomial>;
  data: Dataset.t list;
  units: Units.t;
  simulator: simulator;
  parameters: Parameter list;
  sweeps: Sweep list;
  rates: Map<string, Expression.t<'s>>;
  plot_settings: Plot_settings;
}

type syntax<'s> when 's:equality =
  | Simulation of simulation_settings<'s>
  | Simulator of simulator
  | Stochastic of Stochastic_settings.t
  | Deterministic of Deterministic_settings.t
  | Inference of Inference_settings.t
  | Parameters of Parameter list
  | Rates of Map<string, Expression.t<'s>>
  | Sweeps of Sweep list
  | Data of string list
  | Spatial of Spatial_settings.t<'s>
  | Plot_settings of Plot_settings
  | Moment_closure of Moment_closure_settings.t<Moment_closure_settings.monomial>

val from_syntax_list : t<'s> -> syntax<'s> list -> t<'s>

// "map" only affects plots and rates
val map                 : ('a -> 'b) -> t<'a> -> t<'b>
val mapExpressions      : (Expression.t<'a> -> Expression.t<'b>) -> t<'a> -> t<'b>
// map plots and rates differently
val mapPlotsAndRates    : plotFun : (Expression.t<'a> -> Expression.t<'b>) 
                          -> rateFun : (Expression.t<'a> -> Expression.t<'b>)  
                          -> t<'a> -> t<'b>
val collectPlots        : ('a -> 'a list) -> t<'a> -> t<'a>
val substitute          : Environment.t -> t<'s> -> t<'s>
val defaults            : t<'s> when 's:equality
val to_ode_settings     : t<'s> -> ode_settings<'s>
val simulator_to_string : simulator -> string
val to_string           : ('s -> string) -> ('s -> string) -> t<'s> -> string
val update_settings     : syntax<'s> -> t<'s> -> t<'s>
val to_inlined_rates    : t<Key.t<'s>> -> Map<string, Expression.t<Key.inlined_t<'s>>>

val parse_simulator     : Parser.t<simulator>
val parse_defaults      : Parser.t<'s> -> t<Key.t<'s>> -> Parser.t<t<Key.t<'s>>>
val parseCrnSetting     : Parser.t<'s> -> Parser.t<syntax<Key.t<'s>>>
val parse_syntax        : Parser.t<'s> -> Parser.t<syntax<Key.t<'s>>>
val parse               : Parser.t<'s> -> Parser.t<t<Key.t<'s>>>
val from_string         : Parser.t<'s> -> string -> t<Key.t<'s>>
