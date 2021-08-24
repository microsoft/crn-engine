// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.Process
open Microsoft.Research.DNA

module Stringmap = Microsoft.Research.CRNEngine.Stringmap

type value = Value.t
type value_env = Value.t_env
type domain = Domain.t
type strand = Strand.t
type gate = Gate.t
type origami = Origami.t
type species = Species.t
type species_env = Species.t_env
type parameter = Pattern.t

type prim = Microsoft.Research.CRNEngine.Expression.t<string> // Microsoft.Research.CRNEngine.Crn.value
type expression = Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<species>>
type sreaction = Microsoft.Research.CRNEngine.Reaction<species,prim,expression>
type spatialInitial = Microsoft.Research.CRNEngine.Spatial_initial.t

type dom_data =
  | DNA of string * Types.range
  | Rate of value
  | Colour of string * Types.range
  | Subdomains of (string list * bool) * Types.range
type dom_spec = string * dom_data
type t = 
  | Repeat of value * bool * t * value option * spatialInitial option(* amount, constant, p, time *)
  | Strand of strand * value option
  | Gate of gate * value option (* Should be deprecated for below, once at least solo reactions completed *)
  //| Complex of branch * value
  | Origami of t list * value option (*origami*)
  | Instance of string * Types.range * value list * value option (* module name, ?, module parameters, time *)
  | Parallel of t list
  | New of (string * dom_spec list) * t
  | Chemical of t Dsdreaction.mass_action_reaction
  | LogicDsdProcess of RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT> * value option

type t_bind = Def of string * parameter list * (value list -> bool)  * t
type t_env = t_bind list

type sim_species = species * float * bool

val dom_spec_map : (value -> value) -> dom_spec list -> dom_spec list

val erasePosns : t -> t
val replacePosns : Types.range Stringmap.t -> t -> t

(* Evaluate the process or system to another process or value *)
val eval_system : Options.t -> string -> int -> dom_spec list -> value
val eval : t_env -> value_env -> t -> t

val free_names : t -> (Value.name * Value.range) list

(* Type checking *)
val type_check_system : string -> dom_spec list -> bool -> Types.type_env -> unit
val inferTypes : bool option -> Types.type_env -> t -> unit

val display : t -> string

(* Actually evaluate the process to a set of species *)
val eval_to_species : Options.t -> (string list * int) -> t_env -> value_env -> t -> (string list * int) * species_env * sreaction list

val from_species : Value.t option -> Species.t -> t

(******************************************************************************)
