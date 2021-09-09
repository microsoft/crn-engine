// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.GEC.Cssubst

open Microsoft.Research.GEC
open Microsoft.Research.CRNEngine

(* ************************************************************************************************************ *)

(* A "context-sensitive" substitution has four parts. *)
type t = { theta:Subst.t;      (* The actual substitution. *)
           rho: string list;   (* The variables on which "theta" must be injective. *)
           sigma: string list list; (* Species names used in the current context. *)
           tau: string list list }  (* Species names excluded for use. *)

(* The "empty" context-sensitive substitution. *)
let empty = { theta=Subst.empty; rho=[]; sigma=[]; tau=[] }

(* Make a context-sensitive substitution. *)
let make (theta:Subst.t) (rho:string list) (sigma:string list list) (tau:string list list) : t =
  { theta=theta; rho=rho; sigma=sigma; tau=tau }

let printListDisplay (f:'a -> string) (xs:'a list) = "[" + (Lib.string_of_list f "; " xs) + "]"
(* Produce a string representation of a context-sensitive substitution. *)
let display (cs:t) =
  "{ theta = " + (Subst.display cs.theta) + ";" + Lib.newline +
  "  rho   = " + (printListDisplay Lib.id cs.rho) + ";" + Lib.newline +
  "  sigma = " + (printListDisplay Ast.complexString cs.sigma) + ";" + Lib.newline +
  "  tau   = " + (printListDisplay Ast.complexString cs.tau) + " }"

(* Check that a given element of the "csSubst" type satisfies the 2 criteria for a context-sensitive substitution. *)
let isOK (cs:t) = 
  let check_ii () = 
    // Assume that cs.rho contains no duplicates...
    match Lib.find_duplicate Subst.targetEq (List.map (fun x -> Subst.find x cs.theta) cs.rho) with
    | None -> true
    | Some _ -> false
  in
  let check_iii () = Lib.disjoint Ast.complexesEqual cs.sigma cs.tau in
  (check_ii ()) && (check_iii ())

(* Get the "normal" substitution out of a context-sensitive one. *)
let getSubst (cs:t) : Subst.t = cs.theta

(* ************************************************************************************************************ *)

(* "Merge" two context-sensitive substitutions together, and test to see if the result is
   also a context-sensitive substitution. *)
let merge (cs1:t) (cs2:t) : t option =
  match (Subst.union cs1.theta cs2.theta) with
    | None -> None
    | Some theta ->
        let cs12 = { theta = theta;
                     rho = Lib.remove_duplicates (=) (cs1.rho @ cs2.rho);
                     sigma = Lib.remove_duplicates Ast.complexesEqual (cs1.sigma @ cs2.sigma);
                     tau = Lib.remove_duplicates Ast.complexesEqual (cs1.tau @ cs2.tau) }
        in
        if isOK cs12 then Some cs12 else None

(* Compose two lists of context-sensitive substitutions by only retaining those elements of the
   "cartesian product" which satisfy the criteria. *)
let compose (css1:t list) (css2:t list) : t list =
  (* NB: we don't form the entire cartesian product - it could get quite big.
     Instead, have two nested loops with an accumulator running through them.
     TO DO: this could be put into CPS to make it tail-recursive... *)
  let rec loop (acc:t list) (css1:t list) =
    match css1 with
    | [] -> acc
    | (cs1::css1) ->
      let rec loop2 (acc:t list) (css2:t list) =
        match css2 with
        | [] -> acc
        | (cs2::css2) -> 
                        match merge cs1 cs2 with
                        | None -> loop2 acc css2
                        | Some cs12 -> loop2 (acc@[cs12]) css2
      in
      let acc = loop2 acc css2 in
      loop acc css1
  in
  loop [] css1

(* Erase the "context" data in a context-sensitive substitution. *)
let eraseContext (cs:t) : t = { theta=cs.theta; rho=[]; sigma=[]; tau=[] }

(* Check whether a context-sensitive substitution satisfies a list of arithmetic constraints. *)
let satisfiesConstraints (cs:t) (ks:(Ast.aexp*Ast.op*Ast.aexp) list) : bool =
  Lib.forall (Subst.satisfiesConstraint cs.theta) ks

(* Produce a "varAss" (variable assignments) from a context-sensitive substitution.  *)
let mkVarAss (cs:t) = Subst.mkVarAss cs.theta
