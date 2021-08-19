module Microsoft.Research.DNA.Domain
open Microsoft.Research.DNA

module Stringmap = Microsoft.Research.CRNEngine.Stringmap
type prim = Microsoft.Research.CRNEngine.Expression.t<string>

type value = Value.t
type value_env = Value.t_env
type match_env = (string * bool) Stringmap.t
type tag = string * float
type tether = (tag list * int) option
type t = 
  | Toe of value*value*bool*tether
  | Normal of value*bool*tether
type sub_map = Microsoft.Research.CRNEngine.Hashtable.t<string list, t>

val complement : t -> t
val star : t -> t
val unstar : t -> t
val get_value : t -> value
val is_complemented : t -> bool
val are_complements : t -> t -> bool
val is_tethered : t -> bool
val tags_in_common_list : t list -> tag list option
val all_tags_list : t list -> tag list option
val all_tethered_tags_list : t list -> tag list option
val get_tags : t -> tag list option
val set_tags : tag list option -> t -> t
val set_tags_list : tag list option -> t list -> t list
val add_tags_list : tag list option -> t list -> t list
val update_tether : t -> tether -> t
val stick : t -> t -> t
val unstick : t -> t*t
val unstick_list : t list -> t list * t list
val unstick_keep_doc : t -> t*t
val unstick_keep_doc_list : t list -> t list * t list
val display_bare_domain : t -> string
val display_picture : t -> bool -> string
val display_length : t -> int
val display : t -> string
val display_sequence : t list -> string
val eval : value_env -> t -> t
val eval_sequence : value_env -> t list -> t list
val bind_rate : t -> prim
val localize_bind_rate : t -> prim -> t
val unbind_rate : t -> prim
val free_names : t -> (Value.name * Value.range) list
val inferType : bool option -> Types.type_env -> t -> unit
val getPosn : t -> Types.range
val erasePosns : t -> t
val replacePosns : Types.range Stringmap.t -> t -> t
val length : int -> int -> t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
val compare_domains : t list -> t list -> int
val matches : t -> t -> bool
val matches_list : t list -> t list -> bool
val matches_list_env : match_env -> t list -> t list -> match_env option
val is_wildcard : t -> bool
val is_toehold : t -> bool
val contains_toehold : t list -> bool
val get_name : t -> string
val get_sequence : t -> string
val to_string : t -> string
val matching_toeholds : sub_map -> t list -> t list -> ((t list * (t * t list option) * t list) * (t list * (t * t list option) * t list)) list
val neighbouring_toeholds : t list -> (t * t) option
val internal_complements : t list -> (t list * t * t list * t * t list) list

(******************************************************************************)
val universal_counters : t -> t
