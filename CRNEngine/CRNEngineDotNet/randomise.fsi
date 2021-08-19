module Microsoft.Research.CRNEngine.Randomise

val getSeed : unit -> int
(*val manual_init : int -> unit*)
val float : float -> float
val int : int -> int
val get_seeded : int -> Rng.Random
val get_unseeded : unit -> int *  Rng.Random