// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.Types
open Microsoft.Research.CRNEngine

(* Types for storing location information. Useful for error messages! *)
type pos = int * int //Microsoft.FSharp.Text.Lexing.Position
type range = (pos * pos) option (* THE OPTION LETS US ERASE THE POSITIONS SO STRUCTURAL EQUALITY CAN BE USED IN THE SIMULATOR! *)
(* RLP: An arguably more appropriate approach would be erasing the entire type for the simulator *)

val emprange : range
val mkRange : pos*pos -> range
val rangeBegin : range -> pos
val rangeEnd : range -> pos
val format_range : pos -> pos -> string


(* Type for representing the types assigned to syntax.
   BottomT represents an unknown type
   TiedT represents a set of types, presently unknown, that must be equal 
     The option boolean encodes a triple selection between numbers Some(true), numbers + bool Some(false), and +strings None
   The range is the position where this type was inferred 
   In DomainT, the boolean indicates true for normal and false for toe
   Modules are assumed not to return anything, since species aren't 'values'
   *)
type t =
   | BottomT 
   | TiedT of t ref list * bool option * range list
   | StringT of range
   | IntT of range
   | BoolT of range
   | CharT of range
   | FloatT of range
   | DomainT of bool option * range
   | TupT of t ref list * range 
   | ModuleT of t ref list

type type_bind = (string * t ref)
type type_env = type_bind list

val equal : t ref -> t ref -> bool
val type_to_string : t -> string
val types_to_string : t list -> string
val update_range : t -> range -> t
val is_numeric : t -> bool
val is_num_or_bool : t -> bool

val bindings_to_env : (string * float) list -> type_env

val getBeginPos : t -> range

val unify : range -> t ref -> t ref -> bool
val unify_possibilities : range -> t list -> t -> t list
