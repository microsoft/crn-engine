[<JavaScript>]
module Microsoft.Research.CRNEngine.Simulation
open WebSharper

type data = float Table.t
type t<'s> when 's:equality = { 
  settings: Simulation_settings.t<'s>;
  populations: Populations.t<Species,float>;
  events: Event.t<Species, float, float> list;
  currenttime: float;
  nextprinttime: float;
  stepsdone: int;
}

val empty : Unit -> t<'s>
val create : Populations.t<Species,float> -> Event.t<Species, float, float> list -> Simulation_settings.t<'s> -> float -> t<'s>
val get_printspecies : t<Key.t<Species>> -> Expression.t<Key.t<int>> list
val shouldStop : float -> float -> int -> bool ref -> int option -> bool
val shouldPrint : float -> float -> float -> int option -> bool -> float -> bool
