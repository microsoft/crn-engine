// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.Errors
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type typeErrorKind =
  | UnboundVariable of string
  | WrongType of Types.t * (Types.t list) * string
  | ModuleArgs of string * int * int
  | TupleArgs of string * int * int
  | ToeInconsistent of Types.range * bool
  | UnTetheredInOrigami of string
  | TetheredOutside of string
  | RepeatedSpec of string * string
  | NoSeq of string
  | UnknownSpec of string * string
  | UnknownModule of string * string

type DSDException =
  class
    inherit Parser.Exception
    member BeginPos : Types.pos option
    member EndPos : Types.pos option
  end

(* Raise errors. *)
val type_error : Types.range -> typeErrorKind -> 'a
val char_error : Types.pos -> Types.pos -> 'a
val comment_error : Types.pos -> Types.pos -> 'a
val syntax_error : Types.pos -> Types.pos -> string -> 'a
val dna_char_error : char -> 'a
val dna_dup_error : string -> 'a
val dna_toe_error : string -> 'a
val dna_spec_error : string -> 'a
val leak_rates_error : float -> float -> 'a
val domain_lengths_error : int -> int -> 'a
val interacting_nontoehold_error : string -> string -> 'a
val illegal_polymerisation_error : string -> string -> 'a
(*val secondary_structure_error : string -> string -> 'a*)
val neighbouring_toeholds_error : string -> string -> string -> 'a
