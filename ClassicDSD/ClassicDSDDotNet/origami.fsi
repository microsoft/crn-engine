module Microsoft.Research.DNA.Origami
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type value_env = Value.t_env
type options = Options.t
type domain = Domain.t
type strand = Strand.t
type gate = Gate.t

(* The content of an origami is a tethered domain anchoring a strand or gate.
   The additional domain option indicates that this domain is part of a strand or gate anchored d'.
   (This avoids duplicating gates and strands.)
   Only one of strand, gate, or domain will be Some *)
type tag = Domain.tag

type content = C_strand of strand | C_gate of gate

type t = content list

val display : t -> string
val debug_display : t -> string
val free_names : t -> (Value.name * Value.range) list
val domains : t -> domain list
val eval : value_env -> t -> t
val inferType : Types.type_env -> t -> unit
val erasePosns : t -> t
val replacePosns : Types.range Stringmap.t -> t -> t
val matches : options -> t -> t -> bool
val matches_env : options -> Domain.match_env -> t -> t -> Domain.match_env option
val has_wildcard : t -> bool

val origami_map : (strand -> strand) -> (gate -> gate) -> t -> t

val standard_form : options -> t -> t
val equal : options -> t -> t -> bool

val neighbouring_toeholds : t -> (domain*domain) option list
val exposed_nontoeholds : t -> domain list

val is_reactive : Domain.sub_map -> options -> t -> bool
val rotate : options -> t -> t

val origami_species : t -> gate list * strand list

(** Sticking reactions between gates and strands within an origami, after creating a new gate or strand *)
val sticking_reactions_sg : Domain.sub_map -> options -> t -> strand -> (gate * strand * domain * gate * t) list
val sticking_reactions_gs : Domain.sub_map -> options -> t -> gate -> (gate * strand * domain * gate * t) list
val sticking_reactions_ss : Domain.sub_map -> options -> t -> strand -> (strand * strand * domain * gate * t) list
val sticking_reactions_gg : Domain.sub_map -> options -> t -> gate -> (gate * gate * domain * gate * t) list

(** Sticking reactions that bring a new strand or gate into an origami *)
val entry_reactions_sg : Domain.sub_map -> options -> t -> strand -> (gate * strand * domain * gate * t) list
val entry_reactions_gs : Domain.sub_map -> options -> t -> gate -> (gate * strand * domain * gate * t) list
val entry_reactions_ss : Domain.sub_map -> options -> t -> strand -> (strand * strand * domain * gate * t) list
val entry_reactions_gg : Domain.sub_map -> options -> t -> gate -> (gate * gate * domain * gate * t) list

(** Pinning reactions on gates and strands within an origami *)
val pinning_reactions_s : options -> t -> (strand * domain * gate * t) list
val pinning_reactions_g : options -> t -> (gate * domain * gate * t) list

val close_migration_reactions : options -> t -> (domain list * gate * domain list * gate * t) list
val close_displacing_reactions : options -> t -> (domain list * gate * domain list * gate list * strand list * t) list
val close_open_reactions : options -> t -> (domain list * gate * domain list * gate * t) list

(** Return all unsticking reactions possible on a given gate in an origami, without expulsion *)
val unsticking_reactions : Domain.sub_map -> options ->gate-> t -> (gate * domain * gate list * strand list * t) list
(** Return all unsticking reactions possible on a given gate in an origami, with expulsion *)
val unsticking_reactions_expel : Domain.sub_map -> options ->gate-> t -> (gate * domain * gate list * strand list * t * gate list * strand list) list

(** Return all reactions that open a hairpin after an unbind on a given gate in an origami *)
val unbind_open_reactions: options ->gate-> t -> (gate * domain * gate list * strand list * t) list

(** Right, what are the leaks going to be like in an origami and with the other stuff in the soup **)

(** Return all (single) displacement reactions possible from a given anchored gate in an origami, without expulsion *)
val displacement_reactions : options -> gate -> t -> (gate * domain list * gate list * strand list * t) list
(** Return all (single) displacement reactions possible from a given anchored gate in an origami with expulsion **)
val displacement_reactions_expel : options -> gate -> t -> (gate * domain list * gate list * strand list * t * gate list * strand list) list 

(** Return all migration reactions possible on anchored gates in an origami. *)
val migration_reactions : options -> gate -> t -> (gate * domain list * gate * t) list

(** Return all migration reactions that open a hairpin on gates in an origami. *)
val open_migration_reactions : options -> gate ->  t -> (gate * domain list * gate * t) list

(** Return all cover reactions possible on an anchored gate in an origami. *)
val cover_reactions : options -> gate -> t -> (gate * domain * gate * t) list


(** Get all combined fast reactions on anchored gates in an origami (rate may be tau or infinity depending on semantics). *)
val fast_reactions : Domain.sub_map -> options -> t -> (gate list * strand list * t) list

(** Get all "initial" reactions on anchored gates in an origami. *)
val initial_reactions : Domain.sub_map -> options -> t -> (gate list * strand list * t) list

val universal_counters : t -> t