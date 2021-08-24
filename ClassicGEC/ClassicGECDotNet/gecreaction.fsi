// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.Gecreaction

open Microsoft.Research.GEC

(* Opaque reaction datatype. *)
type t

(* Functions to create GEC reaction datatypes. *)
val makeNormal : string list list -> string list list -> string list list -> float -> t
val makeTransport : string list -> string list -> float -> string -> Ast.direction -> t

(* Decide whether a reaction is a normal or a transport reaction... *)
val isNormal : t -> (string list list * string list list * string list list * float) option
val isTransport : t -> (string list * string list * float * string * Ast.direction) option

(* Produce a string representation of a GEC reaction. *)
val display : t -> string

(* Are two reactions equal? *)
val equal : t -> t -> bool

(* Get all species names from a GEC reaction. *)
val species : t -> string list list

(* Apply a substitution to a GEC reaction. *)
val applySubst : Subst.t -> t -> t

val parseReaction : Parser.t<t>
