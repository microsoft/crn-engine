// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Pde
open WebSharper;

type lambda = Lambda<Inlined<int>>
type output<'p> = Row<'p> -> unit
type rate = Rate<Value,Expression.t<Key<int>>>

type t<'a> = { 
  name: string
  species_index: Species -> int
  populations: Populations<Species,'a>
  sim_settings: Simulation_settings<Functional>
  settings: Spatial_settings<Functional>
  stoich: Oslo.Matrix
  powers: (int*float) list array
  rates: rate array
  rateExpressions: Map<string, Functional>
}

val create : string 
          -> Populations<Species,'a> 
          -> Simulation_settings<Functional> 
          -> Spatial_settings<Functional> 
          -> double[][] 
          -> (int * float) list array 
          -> rate array 
          -> Map<string, Functional>
          -> t<'a>

val accumulator_1d : int -> seq<Initial<'s,float>> -> float []
val accumulator_2d : int -> seq<Initial<'s,float>> -> float [][]

val simulate_1d_callback : bool ref -> output<float[]> -> t<float[]> -> unit
val simulate_2d_callback : bool ref -> output<float[][]> -> t<float[][]> -> unit
val simulate : (bool ref -> (Row<'a> -> unit) -> t<'a> -> unit) -> t<'a> -> float list * Table<'a>