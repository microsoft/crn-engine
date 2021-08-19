[<JavaScript>]
module Microsoft.Research.CRNEngine.Simulation_settings 
open WebSharper

type kinetics =
  | [<Constant "Contextual">] Contextual
  | [<Constant "Stochastic">] Stochastic
  | [<Constant "Deterministic">] Deterministic

type t<'s> when 's:equality = {
  points: int;
  initial: float;
  final: float;
  plots: Expression.t<'s> list;
  kinetics: kinetics;
  times: float list; 
  prune: bool;
  multicore: bool;
}

val defaults            : t<'a>
// "map" only affects plots
val map                 : ('a -> 'b) -> t<'a> -> t<'b>
val mapExpressions      : (Expression.t<'a> -> Expression.t<'b>) -> (t<'a>) -> t<'b>
val collectPlots        : ('s -> 's list) -> t<'s> -> t<'s>
val get_print_interval  : t<'s> -> float

// ND: Previously was used as a generic generator for times, but only Sundials uses it, and now needs to use it in a different way
//val get_times           : t<'s> -> float list   
val update_times        : float list -> t<'a> -> t<'a>
val get_plots_names     : ('s -> string) -> t<'s> -> string list
val to_string           : ('s -> string) -> singlePlotPrinter:('s -> string) -> t<'s> -> string
val parse               : Parser.t<'s> -> Parser.t<t<Key.t<'s>>>
val from_string         : Parser.t<'s> -> string -> t<Key.t<'s>>