// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.Subst

open Microsoft.Research.GEC

//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

(* ************************************************************************************************************ *)

(* The target of a "normal" substitution can either produce a species name, a part id or a real number. *)
type target = SPECIES of string list
            | PART of string
            | NUMBER of float
            | ALGEBRAIC_EXPRESSION of Ast.aexp
type t = target Stringmap.t

(* The empty substitution. *)
val empty : t

(* An equality function on substitution targets, which accounts for complexes. *)
val targetEq : target -> target -> bool

(* An equality function on substitutions themselves. *)
val eq : t -> t -> bool

(* Produce a string representation of a substitution. *)
val displayTarget : target -> string
val display : t -> string

(* Compute the union of two substitutions, if possible. *)
val union : t -> t -> t option

(* Compute Dom_S(theta). *)
val speciesDomain : t -> string list

(* Find the result of looking up a variable (NB raises an exception if not found...) *)
val find : string -> t -> target

(* Try to find the result of lookup up a variable (returns an option type). *)
val tryFind : string -> t -> target option

(* Does a string correspond to a floating-point value? *)
val getFloat : string -> float option

(* ************************************************************************************************************ *)

(* "Unify" a string from the program with a "target" from the database. *)
val unify : string -> target -> t option

(* Turn an association list into a substitution, if possible... *)
val multiple : (string * target) list -> t option

(* Apply a substitution to a complex. *)
val applyToComplex : t -> string list -> string list

(* Apply a substitution to a "part type" presented as in trans.fs, using strings... *)
val applyToPartTypeStrings : t -> string * (string list list) -> string * (string list list)

(* Apply a substitution to a directive data structure (for generic plotting). *)
val applyToDirective : t -> Ast.directive -> Ast.directive

(* Apply a substitution to an arithmetic expression. *)
val applyToArithmeticExpression : t -> Ast.aexp -> Ast.aexp

(* ************************************************************************************************************ *)

(* Check whether a substitution satisfies an arithmetic constraint. *)
val satisfiesConstraint : t -> (Ast.aexp * Ast.op * Ast.aexp) -> bool

(* Produce a "varAss" (variable assignments) from a substitution.  *)
val mkVarAss : t -> (string * string) list * (string * string) list * (string * string) list
