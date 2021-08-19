module Microsoft.Research.DNA.Segment
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

(* A segment can be a lower toehold strand or a double-strand *)
type value = Value.t
type value_env = Value.t_env
type domain = Domain.t
type strand = Strand.t

type side = Left | Right
type t = 
  ///lower left, upper left, middle, upper right, lower right
  | Double of (domain list * domain list * domain list * domain list * domain list)
  ///lower, upper, middle, loop, side
  | Hairpin of (domain list * domain list * domain list * domain list * side)

val tag_sets : t -> Domain.tag list option * Domain.tag list option
val tethered_tag_sets : t -> Domain.tag list option * Domain.tag list option
val set_tags : t -> Domain.tag list option -> t
val add_tags : t -> Domain.tag list option -> t

val bottom_left_overhang : t -> domain list
val top_left_overhang : t -> domain list
val double_stranded_region : t -> domain list
val top_right_overhang : t -> domain list
val bottom_right_overhang : t -> domain list
val hairpin_region : t -> domain list
val rotate : t -> t
val mirror : t -> t
val reverse : t -> t
val equals : t -> t -> bool
val compare : t -> t -> int

val display : t -> string
val eval : value_env -> t -> t
val free_names : t -> (Value.name * Value.range) list
val domains : t -> domain list
val is_reactive : t -> bool
val tethered_domains : t -> domain list
val inferType : bool option -> Types.type_env -> t -> unit
val getPosn : t -> Types.range 
val erasePosns : t -> t
val replacePosns : Types.range Stringmap.t -> t -> t
val matches : t -> t -> bool
val matches_env : Domain.match_env -> t -> t -> Domain.match_env option
val has_wildcard : t -> bool
val lower_toehold_available : t -> bool
val upper_toehold_available : t -> bool

(** Find exposed long domains. *)
val exposed_nontoeholds : t -> domain list

(** Look for exposed neighbouring toeholds. *)
val neighbouring_toeholds : t -> (domain * domain) option

(** Cover a single exposed toehold to the left/right. *)
val cover_left : t -> (t * domain) option
val cover_right : t -> (t * domain) option

(** Migrate towards the left/right (by less than one segment). *)
val migrate_left : t -> t -> (t * t * domain list) option
val migrate_left_simple : t * t -> t * t
val migrate_right : t -> t -> (t * t * domain list) option
val migrate_right_simple : t * t -> t * t

(** Migrate left/right (by less than one segment) and open a hairpin. **)
val migrate_open_left : t -> t -> (t * domain list) option
val migrate_open_right : t -> t -> (t * domain list) option

(** Displace a single strand to the left/right. *)
val displace_left : t -> t -> (t * strand * domain list) option
val displace_right : t -> t -> (t * strand * domain list) option

(** Various kinds of sticking reaction... *)
val stick_sg : Domain.sub_map -> t -> strand -> (t * t * (domain * bool) * bool) list
val stick_ss : Domain.sub_map -> strand -> strand -> (t * (domain * bool)) list
val stick_gg : Domain.sub_map -> t -> t -> (t * t * t * (domain * bool) * bool) list

(** Pinning via migration reactions *)
val close_lower_left_migrate : t -> (domain list * t * t * domain list) list
val close_lower_left_displace : t -> (domain list * t * Strand.t * domain list) list
val close_lower_left_open : t -> (domain list * t * domain list) list

(** Pinning and unpinning reactions *)
val pin_strand : strand -> (t * (domain * bool)) list
val pin_segment : t -> (t * t * (domain * bool) * bool) list
val unbind_open_up : t -> (strand * domain) option
val unbind_open_down : t -> (strand * domain) option
val unbind_open_gg_lcon : t -> t -> (t * domain) option
val unbind_open_gg_ucon : t -> t -> (t * domain) option

(** Unsticking reactions. *)
val unstick : Domain.sub_map -> t -> (strand * strand * domain) option

(** Leak reactions. *)
val leak_sg : t -> strand -> (t * strand * bool) list
(* val leak_gg : ... *)

(******************************************************************************)
val universal_counters : t -> t

(* FD additions *)
val empty : t