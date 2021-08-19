[<JavaScript>]
module Microsoft.Research.CRNEngine.Stochastic_settings 
open WebSharper

type t = {
  scale: float;
  seed: int option;
  steps: int option;
  trajectories: int;
  stationary_skiptime: float option;
}

val defaults : t
val to_string : t -> string
val parse : Parser.t<t>
val from_string : string -> t