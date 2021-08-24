// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.Cssubst

open Microsoft.Research.GEC

//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

(* ************************************************************************************************************ *)

(* Type for "context-sensitive" substitutions. *)
type t

(* The "empty" context-sensitive substitution. *)
val empty : t

(* Make a context-sensitive substitution. *)
val make : Subst.t -> string list -> string list list -> string list list -> t

(* Produce a string representation of a context-sensitive substitution. *)
val display : t -> string

(* Check that a given element of the "csSubst" type satisfies the 2 criteria for a context-sensitive substitution. *)
val isOK : t -> bool

(* Get the "normal" substitution out of a context-sensitive one. *)
val getSubst : t -> Subst.t

(* ************************************************************************************************************ *)

(* "Merge" two context-sensitive substitutions together, and test to see if the result is
   also a context-sensitive substitution. *)
val merge : t -> t -> t option

(* Compose two lists of context-sensitive substitutions by only retaining those elements of the
   "cartesian product" which satisfy the criteria. *)
val compose : t list -> t list -> t list

(* Erase the "context" data in a context-sensitive substitution. *)
val eraseContext : t -> t

(* Check whether a context-sensitive substitution satisfies a list of arithmetic constraints. *)
val satisfiesConstraints : t -> (Ast.aexp * Ast.op * Ast.aexp) list -> bool

(* Produce a "varAss" (variable assignments) from a context-sensitive substitution.  *)
val mkVarAss : t -> (string * string) list * (string * string) list * (string * string) list
