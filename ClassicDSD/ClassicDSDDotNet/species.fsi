// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.Species
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

//module Value = Microsoft.Research.CRNEngine.Expression (* Overides DNA.Value *)
type prim = Microsoft.Research.CRNEngine.Expression.t<string>

type options = Options.t
type key = string
type domain = Domain.t
type strand = Strand.t
//type branch = Branch.t
type origami = Origami.t

(* NB: gate should be removed once branching complexes are fully supported *)
type t = STRAND of strand | GATE of Gate.t | (*COMPLEX of branch |*) ORIGAMI of origami | UNKNOWN of string
       | LogicDsdProcess of RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>


type sim_t = Population<t,prim>

(* An environment for mapping species to the names that generate them; including population, constant status, and whether plotted *)
type t_map = (t * prim * bool * bool * prim option * Microsoft.Research.CRNEngine.Spatial_initial.t option)
type t_disp = Names of string Stringmap.t * int
type t_env = (t_map list Stringmap.t * t_disp) 

val is_strand : t -> bool
val is_gate : t -> bool
//val is_complex : t -> bool
val is_origami : t -> bool
val display : t -> string
val display_name : options -> t_env -> bool -> t -> string
val try_find_name : options -> t_env -> bool -> t -> string option
val matches : options -> t -> t -> bool

val is_predicate : t -> bool (* Does this species contain wildcards an is as such realy a species predicate? *)

val domains : t -> domain list

val empty_env : int -> t_env
val union_env : options -> t_env -> t_env -> t_env
val add_env : options -> string -> t_map -> t_env -> t_env
val env_map : (t_map -> t_map) -> t_env -> t_env 
val env_to_species : t_env -> (sim_t * prim option) list
val env_to_plot_species : t_env -> t list

(* Compute the "standard form" of a species. *)
val standard_form : options -> t -> t

(* "Physically rotate" a species. Once it's rotated, put it back into standard form. *)
val rotate : options -> t -> t

(* Are two species equal, up to physical rotation? *)
val equal : options -> t -> t -> bool

(* Check a list of species to see that they meet certain syntactic criteria. *)
val check : t list -> unit

(* Get all "initial" reactions on a gate. *)
val initial_reactions : Domain.sub_map -> options -> t -> t list list

(* Collate a list of gates, a list of strands, and a list of origami into a single list of species. *)
val collate : Gate.t list -> strand list -> origami list -> t list

(* Separate a single list of species into a list of gates, a list of strands, and a list of origami *)
val separate : t list -> Gate.t list * strand list * origami list

(* Separate an association list keyed by species into three association lists, keyed by gates, strands, and origami. *)
val separate_assocs : (t * 'a) list -> (Gate.t * 'a) list * (strand * 'a) list * (origami * 'a) list

(* Is a species "reactive"? A syntactic approximation to reactivity as defined by the reaction rules... *)
val is_reactive : Domain.sub_map -> options -> t -> bool

(**************************************************************************************************************)

(* Type for 'raw' species, as produced by the parser. *)
type raw = t * ((bool * t) list) * Types.range
//type raw_branch = RawBranch of raw * Branch.connection option * (raw_branch * Branch.connection option) list

(* Convert a 'raw' species into a proper one, checking for well-formedness along the way. *)
val convert_raw_gate : raw -> t
//val convert_raw_branch : raw_branch -> t
val convert_raw_origami : t list -> origami

(* return all species' strands (as if the species was melted by increasing the temperature) *)
val melt: t -> t list

val eval : Gate.value_env -> t -> t

val erasePos : t -> t
val universal_counters : t -> t
val free_names : t -> (Value.name * Value.range) list