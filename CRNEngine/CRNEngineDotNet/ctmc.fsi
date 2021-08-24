// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Ctmc
open Microsoft.Research.CRNEngine
open Oslo
open Oslo.SparseMatrix

(* CG - improved from string keys but can we go further to e.g. a packed array? *)
type reaction<'s> when 's : equality = Reaction<'s,Value,Expression.t<Key<'s>>> 

type state = Map<int,int>

type transition = 
    { target_state : state
    ; propensity : Value }

type t =
    { graph : Dictionary.t<state,transition list>
    ; initial_state : state }

type ctmc_result<'s> = {
  ctmc:t;
  to_species:System.Collections.Generic.List<'s>;
 }

val ctmc : calculus:Calculus.t<'a> 
            -> ratesEnv: Hashtable.t<string,Expression.t<Key<'a>>>
            -> scale:float 
            -> ss:('a * int) list -> ctmc_result<'a> when 'a : comparison

val statespace_for_CME_integration : t -> Environment.t -> int -> SparseMatrix * Matrix * Vector
