// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.Strand
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type value = Value.t
type value_env = Value.t_env
type domain = Domain.t
type t = Upper of domain list
       | Lower of domain list

val mirror : t -> t
val reverse : t -> t
val rotate : t -> t
val equal : t -> t -> bool
val compare : t -> t -> int

val display : t -> string
val displays : t list -> string
val to_dot : t -> string
val to_dots : t list -> string

val eval : value_env -> t -> t
val free_names : t -> (Value.name * Value.range) list
val inferType : bool option -> Types.type_env -> t -> unit
val getPosn : t -> Types.range
val erasePosns : t -> t
val replacePosns : Types.range Stringmap.t -> t -> t
val standard_form : t -> t
val matches : t -> t -> bool
val matches_env : Domain.match_env -> t -> t -> Domain.match_env option
val has_wildcard : t -> bool
val domains : t -> domain list
val tethered_domains : t -> domain list
val get_tags : t -> Domain.tag list option
val annotate_tag_sets : t -> t
val unannotate_tag_sets : t -> t
val is_upper : t -> bool
val mk_upper : domain list -> t

val exposed_nontoeholds : t -> domain list
val neighbouring_toeholds : t -> (domain * domain) option
val append : t -> t -> t
val is_reactive : t -> bool

(******************************************************************************)
val universal_counters : t -> t