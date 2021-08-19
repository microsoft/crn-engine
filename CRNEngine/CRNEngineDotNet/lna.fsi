[<JavaScript>] 
module Microsoft.Research.CRNEngine.Lna

type row = Row<Point>
type output = row -> unit
type settings = { scale: float }
type lambda = Lambda<Inlined<int>>
type rate = Rate<float,lambda>
type cancel = bool ref

type t = { 
  ode: OdeOslo.evaluated;
  outputconcentrations: bool;
  initials: float array;
  stoich: float [][];
  covariance: Oslo.Matrix;
  times: float list;
  nextprinttime: float;
  jacobian: Oslo.Vector -> Oslo.Matrix;
  drift: Oslo.Vector -> Oslo.Matrix;
  props: float -> Oslo.Vector -> Oslo.Vector;
  stepsdone: int;
  settings: settings;
}

val default_settings : settings
val initialise : OdeOslo.evaluated -> float [] [] -> float array -> settings -> t
val simulate_callback : cancel -> output -> t -> t
val simulate : lna:t -> t * Table<Point>