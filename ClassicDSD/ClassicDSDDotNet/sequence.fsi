module Microsoft.Research.DNA.Sequence
open Microsoft.Research.DNA

(* DNA sequences and random generation thereof. *)
type domain = Domain.t
type dnabase = Adenine | Guanine | Cytosine | Thymine
type t = dnabase list
val complement : t -> t
val complement_dnabase : dnabase -> dnabase
val string_of_dnabase : dnabase -> string
val string_of_sequence : t -> string
type mapping =
  { toeholds: string list
  ; specificities: string list
  ; assigned: (domain * t option) list }

val empty : mapping
val initialise_mapping : Options.t -> mapping
val add_domains : mapping -> domain list -> mapping
val display_mapping : mapping -> string
val get_domain : mapping -> domain -> t option option

//FD additions
val parse_sequence : string -> t