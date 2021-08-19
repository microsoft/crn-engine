[<JavaScript>]
module Microsoft.Research.CRNEngine.Point

type t = {mean: float; stdev: float}

val create : mean:float -> stdev:float -> t
val interpolate : t:float -> t0:float -> t1:float -> x0:t -> x1:t -> t
