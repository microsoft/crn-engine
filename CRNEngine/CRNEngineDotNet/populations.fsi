// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>] 
module Microsoft.Research.CRNEngine.Populations

//'s for species, 'v for value
type population<'s,'v> when 's:equality = Population.t<'s,'v>
type t<'s,'v> when 's : equality =
  { index_to_species : population<'s,'v> array
  ; species_to_index : Hashtable.t<'s,int> }

type expression<'s> when 's:equality = Expression.t<'s>

(* CS: making internal functions public for the time being for the Moment Closure project *)
val empty : unit -> t<'s,'v>
val create : population<'s,'v> list -> t<'s,'v>
val contains_species : t<'s,'v> -> 's -> bool
val species_present : ('v -> float) -> t<'s,'v> -> 's -> bool (* contains and pop not 0 *)
val add_pop_info : population<'s,'v> -> t<'s,'v> -> t<'s,'v>

val get_pop_info_list : t<'s,'v> -> population<'s,'v> list
val get_pop_array : t<'s,'v> -> 'v array

val get_initial_species_list : t<'s,'v> -> 's list
val get_input_species_list : t<'s,'v> -> 's list

val get_calculus_species_list : t<'s,'v> -> 's list

val get_pop_info : t<'s,'v> -> int -> population<'s,'v>
val find_index : t<'s,'v> -> 's -> int
val tryFind_index : t<'s,'v> -> 's -> int option
val get_calculus_species : t<'s,'v> -> int -> 's
val get_population : t<'s,'v> -> int -> 'v
val set_population : t<'s,'v> -> int -> 'v -> unit
val is_constant : t<'s,'v> -> int -> bool
val set_constant : t<'s,'v> -> int -> bool -> unit
val set_max : t<'s,'v> -> int -> int option -> unit

val get_count : t<'s,'v> -> int

val eval : ('v -> float) -> t<'s,'v> -> t<'s,float>
val map_population : ('value1->'value2) -> t<'s,'value1> -> t<'s,'value2>
val test_integer_or_constant : t<'s,float> -> bool
val round : t<'s,float> -> t<'s,float>
val map_species : ('species1->'species2) -> t<'species1,'v> -> t<'species2,'v>
val scale : t<'s,float> -> float -> t<'s,float>

val tryFind_species : t<'s,'v> -> 's -> population<'s,'v> option

val clone : t<'s,'v> -> t<'s,'v>

val to_string : t<'s,'v> -> ('s -> string) -> ('v -> string) -> string
val to_string_nonzero : t<'s,'v> -> ('s -> string) -> ('v -> string) -> string
val to_string_float : t<'s,float> -> ('s -> string) -> string
val to_string_ext : t<'s,'v> -> (population<'s,'v> -> string) -> string -> string

val to_key_plottables : t<'s,'v> -> 's Expression.t -> int Expression.t
val to_species_plottables : t<'s,'v> -> int Expression.t -> 's Expression.t

(* val internal  extend : t<'s,float> -> t<'s,float> -> t<'s,float> *)
val merge : ('v -> 'v -> 'v) -> t<'s,'v> -> t<'s,'v> -> t<'s,'v>