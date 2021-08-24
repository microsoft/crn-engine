// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.OdeOslo

type simulator = Simulation.t<Key<Species>>
type populations = Populations<Species, float>
type lambda = Lambda.t<Key2<int>>
type event = Event<Species, float, float>
type rate = Rate<float,lambda>

/// The OdeOslo type is parameterized (by values) or instantiated from an environment
type t = {
  name: string
  settings: Ode_settings<Key<Species>>
  matrix: float[][]
  powers: (int*float) list array
  rates: Rate<Value,Expression.t<Key<int>>> array
  species: Map<Species,int>
  initials: Initial<Species,Value> list
}

type evaluated = { 
  name: string
  simulator: simulator
  plots: Expression.t<Key2<int>> array
  settings: Deterministic_settings
  stoich: Oslo.Matrix
  powers: (int*float) list array
  rateDs: Rate<float,lambda> array
  concs: Oslo.Vector
}

type cancel = bool ref
type row = Row<float>
type table = Table<float>
type output = row -> unit

val substitute : Environment.t -> t -> t
val create : string -> Ode_settings<Key<Species>> -> float [] [] -> (int * float) list array -> Rate<Value,Expression.t<Key<int>>> array -> Map<Species,int> -> Initial<Species,Expression.t<string>> list -> t
val get_instances : t -> Instance list

val evaluate : t -> evaluated
val simulate_callback : cancel -> output -> evaluated -> evaluated
val simulate : evaluated -> evaluated * table

//I've exposed these to remove duplication in LNA. Perhaps refactor?
(*type (*internal*) Stepper<'solver> = {
    init    : 'solver
    current : 'solver -> 'solver
    advance : 'solver -> 'solver
    refine : ('solver -> 'solver -> 'solver list) option
    get_time : 'solver -> float
    get_x : 'solver -> Oslo.Vector
}*)
//val internal runSolver : stepper:Stepper<'a> -> duration:'b -> currenttime:float -> nextprinttime:float -> eventtime:float -> concs:Vector -> stepsdone:int -> settings:'c -> grab_data:(Vector -> float list) -> printinterval:float -> cancel_flag:bool ref -> output:output -> float * Vector * int