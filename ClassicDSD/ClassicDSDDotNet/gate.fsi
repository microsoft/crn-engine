module Microsoft.Research.DNA.Gate
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type value = Value.t
type value_env = Value.t_env
type domain = Domain.t
type strand = Strand.t
type options = Options.t
type semantics = Options.semantics
type t = Segment.t list list
type connects = Uppers | Lowers (* Which strands are the complex connections on *)
type t_complex = | Segment of Segment.t
                 | Sequence of connects * t_complex * t_complex

val first_segment : t -> Segment.t
val replace_first : t -> Segment.t -> t
val last_segment : t -> Segment.t
val replace_last : t -> Segment.t -> t
val normalise : t -> t
val normalise_pair : Segment.t*Segment.t -> Segment.t*Segment.t
val cons_strand_to_segments : strand -> Segment.t list -> Segment.t list
val append_strand_to_segments : Segment.t list -> strand -> Segment.t list
val cons_strand_to_gate : strand -> t -> t
val append_strand_to_gate : t -> strand -> t
val join : t -> bool -> t -> t

val display : t -> string
val free_names : t -> (Value.name * Value.range) list
val domains : t -> domain list
val tethered_domains : t -> domain list
val get_tags : t -> Domain.tag list option
val annotate_tag_sets : t -> t
val unannotate_tag_sets : t -> t

val eval : value_env -> t -> t
val inferType : bool option -> Types.type_env -> t -> unit
val getPosn : t -> Types.range
val erasePosns : t -> t
val replacePosns : Types.range Stringmap.t -> t -> t
val compare : t -> t -> int
val equal : options -> t -> t -> bool
val matches : options -> t -> t -> bool
val matches_static : t -> t -> bool (*comparison function that does not rotate either gate*)
val matches_env : options -> Domain.match_env -> t -> t -> Domain.match_env option
val has_wildcard : t -> bool

(** Find exposed long domains. *)
val exposed_nontoeholds : t -> domain list

(** Look for exposed neighbouring toeholds. *)
val neighbouring_toeholds : t -> (domain * domain) option

(** Use branch migration to put molecules into a standard form. *)
val standard_form : options -> t -> t

(* "Physically rotate" a gate. Once it's rotated, put it back into standard form. *)
val rotate : options -> t -> t

(* support function for rotation and standard form in branching structures *)
val migrate_right_all : t -> t

(* Are two gates equal, up to physical rotation? *)
val equal : options -> t -> t -> bool

(* Are two complexes equal, up to physical rotations *)
val equal_complex : options -> t_complex -> t_complex -> bool

(** Sticking reactions. *)
val stick_strand_to_gate : Domain.sub_map -> bool -> strand -> t -> (t * strand * domain * t) list
val stick_strand_to_strand : Domain.sub_map -> bool -> strand -> strand -> (strand * strand * domain * t) list
val stick_gate_to_gate : Domain.sub_map -> options -> t -> t -> (t * t * domain * t) list

(** Pinning reactions *)
val pin_strand : bool -> strand -> (strand * domain * t) list
val pin_gate : bool -> t -> (t * domain * t) list

(** Return all unsticking reactions possible from a given gate. *)
val unsticking_reactions : Domain.sub_map -> options -> t -> (t * domain * t list * strand list) list

(** Return all reactions on a given gate that open a hairpin after an unbind *)
val unbind_open_reactions: options -> t -> (t * domain * t list * strand list) list

(** Return all leak reactions possible between lists of gates and strands. *)
val leak_strand_into_gate : options -> strand -> t -> (t * strand * bool * t list * strand list) list

(** Return all leak reactions possible between two gates. *)
val leak_gate_into_gate : options -> t -> t -> (t * t * bool * t list * strand list) list

(** Return all (single) displacement reactions possible from a given gate. *)
val displacement_reactions : options -> t -> (t * domain list * t list * strand list) list

(** Return all migration (leak) reactions resulting from hairpins closing possible on a given gate. *)
val close_migration_reactions : options -> t -> (domain list * t * domain list * t) list
(** Return all displacing (leak) reactions resulting from hairpins closing possible on a given gate. *)
val close_displacing_reactions : options -> t -> (domain list * t * domain list * t list * strand list) list
(** Return all open (leak) reactions resulting from hairpins closing possible on a given gate. *)
val close_open_reactions : options -> t -> (domain list * t * domain list * t) list

(** Return all migration reactions possible on a given gate. *)
val migration_reactions : t -> (t * domain list * t) list

(** Return all migration reactions that open a hairpin on a given gate. *)
val open_migration_reactions : options ->  t -> (t * domain list * t) list

(** Return all cover reactions possible on a given gate. *)
val cover_reactions : t -> (t * domain * t) list

(** Get all combined fast reactions on a list of gates (rate may be tau or infinity depending on semantics). *)
val fast_reactions : Domain.sub_map -> options -> t list -> (t list * strand list) list

(** Get all "initial" reactions on a gate. *)
val initial_reactions : Domain.sub_map -> options -> t -> (t list * strand list) list

(** Is a gate "reactive"? *)
val is_reactive : Domain.sub_map -> options -> t -> bool

(******************************************************************************)
val universal_counters : t -> t



(* FD Additions: *)
//val getEnergy : t  -> SequenceCalc.energy