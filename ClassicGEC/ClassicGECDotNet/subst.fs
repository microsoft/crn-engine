// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.GEC.Subst
open Microsoft.Research.GEC

//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

module Expressions = Microsoft.Research.CRNEngine.Expression
(* ************************************************************************************************************ *)

(* A "normal" substitution can either produce a species name, a part id or a real number. *)
type target = SPECIES of string list
            | PART of string
            | NUMBER of float
            | ALGEBRAIC_EXPRESSION of Ast.aexp
type t = target Stringmap.t

(* The empty substitution and singleton substitution. *)
let empty : t = Stringmap.empty
let singleton (x:string) (t:target) : t = empty |> Stringmap.add x t

(* Produce a string representation of a substitution. *)
let displayTarget = function
  | SPECIES ys -> Ast.complexString ys
  | PART y -> y
  | NUMBER n -> Lib.display_float n
  | ALGEBRAIC_EXPRESSION(_) -> "(algebraic_expression)"
let display (theta:t) = 
  let body = Stringmap.fold (fun acc x tgt -> acc + "(" + Lib.quote x + ", " + Lib.quote (displayTarget tgt) + ")" + "; ") "" theta in
  "[" + body + "]"

(* Find the result of looking up a variable (NB raises an exception if not found...) *)
let find (x:string) (theta:t) = Stringmap.find x theta

(* Try to find the result of lookup up a variable (returns an option type). *)
let tryFind (x:string) (theta:t) = Stringmap.tryFind x theta

(* An equality function on substitution targets, which accounts for complexes. *)
let targetEq (tgt1:target) (tgt2:target) : bool =
  match tgt1, tgt2 with
  | SPECIES xs1, SPECIES xs2 -> Ast.complexesEqual xs1 xs2
  | PART p1, PART p2 -> p1=p2
  | NUMBER f1, NUMBER f2 -> f1=f2
  | _,_ -> false

(* An equality function on substitutions themselves. *)
let eq (theta1:t) (theta2:t) =
  let dom1 = Stringmap.getKeys theta1 in
  let dom2 = Stringmap.getKeys theta2 in
  if (dom1=dom2) then
    Lib.forall (fun x -> targetEq (find x theta1) (find x theta2)) dom1
  else false

(* Compute the union of two substitutions, if possible. *)
let union (theta1:t) (theta2:t) : t option =
  Stringmap.fold (fun acc x t ->
    match acc with
    | None -> None
    | Some smap ->
        match Stringmap.tryFind x smap with
        | None -> Some(Stringmap.add x t smap)
        | Some t' -> if t=t' then Some smap else None)
    (Some theta1) theta2

(* Compute Dom_S(theta). *)
let speciesDomain (theta:t) = 
  let f snames x tgt : string list =
    match tgt with
    | SPECIES _ -> Lib.maybeappend (=) snames x
    | _ -> snames
  in
  Stringmap.fold f [] theta

(* ************************************************************************************************************ *)

(* Does a string correspond to a meta-variable? (i.e. does it begin with a capital? *)
let isMetaVariable (x:string) : bool =
  System.Char.IsUpper (x.[0])

(* Does a string correspond to a floating-point value? *)
let getFloat (x:string) : float option =
  let ret = ref 0.0
  if System.Double.TryParse(x, ret) then Some !ret else None

(* Implement a rudimentary "occurs check". Assumes that x is a metavariable. *)
let occursCheckPassed (x:string) (tgt:target) : bool =
  match tgt with
  | PART z -> not(x=z)
  | SPECIES zs -> not(Lib.contains x zs)
  | _ -> true

(* "Unify" a string from the program with a "substTarget" from the database. *)
let unify (x:string) (tgt:target) : t option =
  if isMetaVariable x then
    (if occursCheckPassed x tgt then Some(singleton x tgt) else None)
  else
    match tgt with
    | PART z -> if x=z then Some empty else None
    | SPECIES zs -> (if zs=[x] then Some empty else None)
    | NUMBER f -> (match getFloat x with None -> None | Some f' -> if f=f' then Some empty else None)
    | ALGEBRAIC_EXPRESSION a -> None // check this

(* Turn an association list into a substitution, if possible... *)
let multiple (prs:(string * target) list) : t option = 
  let rec loop (theta:t) (prs:(string * target) list) = 
    match prs with
    | [] -> Some theta
    | ((x,t)::prs) ->
        begin match unify x t with
        | None -> None
        | Some theta' ->
            begin match union theta theta' with
            | None -> None
            | Some theta -> loop theta prs
            end
        end
  in
  loop empty prs

(* Apply a substitution to a complex, represented as strings. *)
let applyToComplex (theta:t) (xs:string list) : string list =
  Lib.collect (fun x -> match tryFind x theta with Some(SPECIES zs) -> zs | _ -> [x]) xs

(* Apply a substitution to a "part type" presented as in trans.fs, using strings... *)
let applyToPartTypeStrings (theta:t) (pt:string * (string list list)) : string * (string list list) =
  let (brickType, brickProps) = pt in
  let applySubst x = match tryFind x theta with None -> x | Some tgt -> displayTarget tgt in
  (brickType, List.map (List.map applySubst) brickProps)

(* Apply a substitution to a "gecSpecies" (for generic plotting). *)
let applyToGecSpecies (theta:t) (i:Ast.gecSimpleSpecies) : Ast.gecSimpleSpecies =
  (* Apply a substitution to a value. *)
  let applyToValue (theta:t) (v:Ast.value) : Ast.value list =
    match v with 
    | Ast.IdVal x -> begin match tryFind x theta with
                       | Some (SPECIES(xs)) -> List.map (fun x -> Ast.IdVal(x)) xs
                       | _ -> [v]
                     end
    | _ -> [v]
  in
  (* Apply a substitution to a "simple" GEC species. *)
  let applyToSimpleGecSpecies (theta:t) (i:Ast.gecSimpleSpecies) : Ast.gecSimpleSpecies =
    match i with
    | Ast.SimpleSpecies ac -> Ast.SimpleSpecies(Lib.collect (applyToValue theta) ac)
    | Ast.CompartmentSpecies(c,ac) -> Ast.CompartmentSpecies(c, Lib.collect (applyToValue theta) ac)
  in
  (applyToSimpleGecSpecies theta) i

(* Apply a substitution to a directive data structure (for generic plotting). *)
let applyToDirective (theta:t) (d:Ast.directive) : Ast.directive =
  (* Apply a substitution to a "plottable". *)
  let applyToPlottable (theta:t) (p:Ast.gecSimpleSpecies Key Expression.t) = Expressions.map (Key.map (applyToGecSpecies theta)) p in
  (*
  let rec applyToPlottable (theta:t) (p:Ast.gecSpecies Plottable.t) : Ast.gecSpecies Plottable.t = match p with
    | Expressions.PopulationAExp g -> Expressions.PopulationAExp (applyToGecSpecies theta g)
    //| Plottable.PLOT_STRING s -> Plottable.PLOT_STRING s
    | Expressions.SumAExp ps -> Expressions.SumAExp (List.map (applyToPlottable theta) ps)
  in *)
  match d with
    | Ast.PLOT ps -> Ast.PLOT (List.map (applyToPlottable theta) ps)
    | _ -> d

(* Apply a substitution to an arithmetic expression. *)
let rec applyToArithmeticExpression (theta:t) (a:Ast.aexp) : Ast.aexp =
  match a with
  | Ast.FloatAExp f -> Ast.FloatAExp f
  | Ast.IdAExp x -> begin
                      match tryFind x theta with
                        | None -> Ast.IdAExp x
                        | Some (NUMBER f) -> Ast.FloatAExp f
                        | Some t -> failwith ("applyToArithmeticExpression: illegal substitution because " + x + " maps to " + (displayTarget t))
                    end
  | Ast.PlusAExp (a1,a2) -> Ast.PlusAExp((applyToArithmeticExpression theta a1),(applyToArithmeticExpression theta a2))
  | Ast.MinusAExp (a1,a2) -> Ast.MinusAExp((applyToArithmeticExpression theta a1),(applyToArithmeticExpression theta a2))
  | Ast.MulAExp (a1,a2) -> Ast.MulAExp((applyToArithmeticExpression theta a1),(applyToArithmeticExpression theta a2))
  | Ast.DivAExp (a1,a2) -> Ast.DivAExp((applyToArithmeticExpression theta a1),(applyToArithmeticExpression theta a2))
  | Ast.PowAExp (a1,a2) -> Ast.PowAExp((applyToArithmeticExpression theta a1),(applyToArithmeticExpression theta a2))

(* ************************************************************************************************************ *)

(* Does a ground constraint hold between floating point values? *)
let groundConstraintHolds ((f1,op,f2):float*Ast.op*float) : bool =
  match op with
  | Ast.Lt -> f1<f2
  | Ast.Gt -> f1>f2
  | Ast.Eq -> f1=f2

(* Try to evaluate an algebraic expression to a float, using a given substitution. *)
let rec tryProduceFloat (theta:t) (a:Ast.aexp) : float option =
  match a with
  | Ast.FloatAExp f -> Some f
  | Ast.IdAExp id -> if isMetaVariable id then begin match tryFind id theta with Some(NUMBER f) -> Some f | _ -> None end
                                          else failwith ("Subst.satisfiesConstraint: " + id + " is not a metavariable.")
  | Ast.PlusAExp (a1,a2) -> (match (tryProduceFloat theta a1, tryProduceFloat theta a2) with
                              | Some f1, Some f2 -> Some(f1 + f2)
                              | _,_ -> None)
  | Ast.MinusAExp (a1,a2) -> (match (tryProduceFloat theta a1, tryProduceFloat theta a2) with
                              | Some f1, Some f2 -> Some(f1 - f2)
                              | _,_ -> None)
  | Ast.MulAExp (a1,a2) -> (match (tryProduceFloat theta a1, tryProduceFloat theta a2) with
                              | Some f1, Some f2 -> Some(f1 * f2)
                              | _,_ -> None)
  | Ast.DivAExp (a1,a2) -> (match (tryProduceFloat theta a1, tryProduceFloat theta a2) with
                              | Some f1, Some f2 -> Some(f1 / f2)
                              | _,_ -> None)
  | Ast.PowAExp (a1,a2) -> (match (tryProduceFloat theta a1, tryProduceFloat theta a2) with
                              | Some f1, Some f2 -> Some(f1 * f2)
                              | _,_ -> None)

(* Check whether a substitution satisfies an arithmetic constraint. *)
let satisfiesConstraint (theta:t) ((a1,op,a2):Ast.aexp*Ast.op*Ast.aexp) : bool =
  (* Turn a string into a float, using a given substitution, if possible... *)
  match (tryProduceFloat theta a1),(tryProduceFloat theta a2) with
    | Some f1, Some f2 -> groundConstraintHolds (f1,op,f2)
    | _,_ -> true (* Constraints involving uninstantiated variables can always be satisfied. *)

(* Produce a "varAss" (variable assignments) from a substitution.  *)
let mkVarAss (theta:t) : (string * string) list * (string * string) list * (string * string) list =
  Stringmap.fold 
    (fun (parts,species,rates) x tgt -> 
        match tgt with
        | SPECIES ys -> 
            (parts,(species@[x,(Ast.complexString ys)]),rates)
        | PART y -> 
            ((parts@[x,y]),species,rates)
        | NUMBER n -> 
            (parts,species,(rates@[x,(Lib.display_float n)]))    
        | ALGEBRAIC_EXPRESSION a -> 
            (parts,species,(rates@[x,(Ast.lbsStringOfAExp a)]))
    )
    ([],[],[])  
    theta