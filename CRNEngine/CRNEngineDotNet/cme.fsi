// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Cme

open Oslo

type simulator = Simulation.t<Key<Species>>
type sim_settings = Simulation_settings.t<Key<Species>>
type cancel = bool ref
type output = Row<Point> -> unit
type populations = Populations<Species, float>
type event = Event<Species, float, float>
type value = Expression.t<string>

type probabilities = {
  times: float list;
  stateprobabilities : float[] list;
  stoichiometry : float[][];
  species: Hashtable.t<string,int>;
}

type t = {
  name: string;
  simulator: simulator;
  settings: Deterministic_settings.t;
  statespace: Ctmc.t ref;
  probabilities: probabilities;
}

val empty : t

val create : populations:populations -> events:event list -> s:Deterministic_settings.t -> sim_settings:sim_settings -> scale:float -> t

val integrate_CME_callback  : env                 : Environment.t 
                                -> ratesEnv       : Map<string,Expression.t<Key2<Species>>>
                                -> cancel_flag    : cancel 
                                -> output         : output 
                                -> stiff          : bool 
                                -> console_print  : bool 
                                -> cme            : t 
                                -> t
val integrate_CME           : env                 : Environment.t 
                                -> ratesEnv       : Map<string,Expression.t<Key2<Species>>>
                                -> stiff          : bool 
                                -> console_print  : bool 
                                -> cme            : t 
                                -> t * Table<Point>

val probability_map : probabilities -> string -> float [] list
val get_bounds : probabilities -> string -> float[] * int * int // Column, max, min
val get_times : probabilities -> float list