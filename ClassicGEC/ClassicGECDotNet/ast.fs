// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

(*
Defines the abstract syntax tree data types for GEC.
Also defines compiler exceptions.

Author: Michael Pedersen.
Copyright � Microsoft Research, 2009.
*)

[<JavaScript>] 
module Microsoft.Research.GEC.Ast
//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

// exceptions for compilation errors.
//type pos = {l1 : int; c1 : int; l2 : int; c2 : int}
//exception CompilerEx of string
//exception CompilerExPos of string * pos

// values can be names or variables (collectively identifiers), floats or wild cards; 
// or more recently, algebraic expressions for functional rate expressions in promoter properties.
type value =
    | IdVal of string
    | FloatVal of float
    | WildCardVal
    | AlgebraicExp of aexp 
    
// in order to parse actual parameter lists without conflicts, we allow "abstract complexes"
// to contain any value, including floats, and rely on a type system to filter out bad uses.
and abstractComplex = value list

// properties:
and prop =  string * abstractComplex list

// arithmetic expressions (very basic for now):
and aexp = //| ValAExp of value 
    | FloatAExp of float
    | IdAExp of string
    | PlusAExp of aexp * aexp
    | MinusAExp of aexp * aexp
    | MulAExp of aexp * aexp
    | DivAExp of aexp * aexp
    | PowAExp of aexp * aexp

// direction for transport reactions (in or out of compartment):
type direction = In | Out
    
// predicate operators for quantitative constraints:
type op = Gt | Lt | Eq    

// programmes:
type prog = 
    | Nil
    | Brick of value * string * prop list
    | Device of string
    | Reac of abstractComplex list * abstractComplex list * abstractComplex list * value * bool
    | Trans of abstractComplex * abstractComplex * string * value * bool * direction
    | TemplateInv of string * abstractComplex list
    | Seq of prog * prog 
    | Par of prog * prog
    | Comp of string * prog
    | New of string * prog
    | TemplateDef of string * string list * prog * prog
    | Constraint  of aexp * op * aexp
    | Rate of value * float
    | InitPop of abstractComplex * float
    | Copy of int * prog * bool * bool // first bool indicates par/seq, second bool indicates simonly

// directives:
type gecSimpleSpecies = CompartmentSpecies of string * abstractComplex
                      | SimpleSpecies of abstractComplex
type gecSpecies = gecSimpleSpecies list
type gecNumPoints = Default | IntPoints of int | AllPoints
type kinetics =
  | Contextual_kinetics
  | Stochastic_kinetics
  | Deterministic_kinetics
let string_of_kinetics = function
  | Contextual_kinetics -> "contextual"
  | Stochastic_kinetics -> "stochastic"
  | Deterministic_kinetics -> "deterministic"
type directive =
    | SAMPLE of float * gecNumPoints
    | TIME of Time
    | CONCENTRATION of Concentration
    | ABSTOLERANCE of float
    | RELTOLERANCE of float
    | SCALE of float
    | PLOT of gecSimpleSpecies Key Expression.t list
    | KINETICS of kinetics

(* ************************************************************************************************************ *)

(* String representation of a species complex. *)
let complexString xs = Lib.string_of_list Lib.id "::" xs

(* String representation of a species in a compartment. *)
let compartmentString c x = c + Lib.brack x

(* String representation of an operator. *)
let stringOfOp = function
  | Gt -> ">"
  | Lt -> "<"
  | Eq -> "="

(* String representation of an expression. *)
let rec stringOfAExp = function
  | FloatAExp f -> Lib.display_float f
  | IdAExp id -> id
  | PlusAExp (e1,e2) -> (stringOfAExp e1) + "+" + (stringOfAExp e2)
  | MinusAExp (e1,e2) -> (stringOfAExp e1) + "-" + (stringOfAExp e2)
  | MulAExp (e1,e2) -> (stringOfAExp e1) + "*" + (stringOfAExp e2)
  | DivAExp (e1,e2) -> (stringOfAExp e1) + "/" + (stringOfAExp e2)
  | PowAExp (e1,e2) -> (stringOfAExp e1) + "^" + (stringOfAExp e2)


(* String representation of an expression. *)
let rec lbsStringOfAExp = function
  | FloatAExp f -> Lib.display_float f
  | IdAExp id -> id
  | PlusAExp (e1,e2) -> "(" + (lbsStringOfAExp e1) + "+" + (lbsStringOfAExp e2) + ")"
  | MinusAExp (e1,e2) -> "(" + (lbsStringOfAExp e1) + "--" + (lbsStringOfAExp e2) + ")"
  | MulAExp (e1,e2) -> (lbsStringOfAExp e1) + "*" + (lbsStringOfAExp e2)
  | DivAExp (e1,e2) -> (lbsStringOfAExp e1) + "/" + (lbsStringOfAExp e2)
  | PowAExp (e1,e2) -> (lbsStringOfAExp e1) + "^" + (lbsStringOfAExp e2)

(* String representation of a value. *)
let stringOfValue = function
  | IdVal x -> x
  | FloatVal f -> Lib.display_float f
  | WildCardVal -> "_"
  | AlgebraicExp aexp -> stringOfAExp aexp

(* String representation of an abstract complex. *)
let abstractComplexString vs = Lib.string_of_list stringOfValue "::" vs

(* Are two complexes equal? *)
let complexesEqual (xs:string list) (ys:string list) = Lib.is_permutation (=) xs ys

(* Produce a string representation of a "gecSpecies". *)
let stringOfGecSimpleSpecies (g:gecSimpleSpecies) : string =
  (* Produce a string representation of a "gecSimpleSpecies". *)
  let stringOfGecSimpleSpecies (g:gecSimpleSpecies) : string =
    match g with
    | SimpleSpecies ac -> abstractComplexString ac
    | CompartmentSpecies (c,ac) -> compartmentString c (abstractComplexString ac)
  stringOfGecSimpleSpecies g

(* Produce the source code representation of a directive. *)
let stringOfDirective (d:directive) : string = 
  match d with
  | SAMPLE(f,io) -> "directive sample " + (Lib.display_float f) + (match io with Default -> "" | IntPoints i -> " " + (string i) | AllPoints -> " all")
  | TIME u -> "directive time " + u.to_string
  | CONCENTRATION u -> "directive concentration " + u.to_string
  | ABSTOLERANCE f -> "directive abstolerance " + (Lib.display_float f)
  | RELTOLERANCE f -> "directive reltolerance " + (Lib.display_float f)
  | SCALE f -> "directive scale " + (Lib.display_float f)
  | PLOT ps -> "directive plot " + (Lib.string_of_list (Expression.to_string (Key.to_string stringOfGecSimpleSpecies)) "; " ps)
  | KINETICS k -> "directive kinetics " + string_of_kinetics k
