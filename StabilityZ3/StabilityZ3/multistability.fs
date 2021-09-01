// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.Multistability

module Stability = Microsoft.Research.Biology.StabilityZ3.Stability 

open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.Biology.StabilityZ3

let Or  = Array.reduce (fun a b -> BOr (a,b))

(* Idea: 
    - construct two systems that have the same parameters but different state variables
    - enforce stability for both systems
    - enforce that the stable states are unique
*)
let Bistable (S:Dynamical) =         
    let state_vars = S.State
    let p1 = state_vars |> Array.map(fun x -> x, sprintf "%s_1" x) 
    let p2 = state_vars |> Array.map(fun x -> x, sprintf "%s_2" x)

    let S1 = S.Subst (p1 |> Map.ofArray)
    let S2 = S.Subst (p2 |> Map.ofArray)

    let vars = S1.GetVars + S2.GetVars

    let unique = 
        Array.map2 (fun (_,v) (_,v') -> BNot(BEq(Key v, Key v'))) p1 p2
        |> Or
    
    let property =         
        [| Stability.StableEq Stability.Default S1      //first state is a stable equlibrium
        ;  Stability.StableEq Stability.Default S2      //second state is a stable equilibrium
        ;  [|unique|]                                   //the two states are distinct
        |]
        |> Array.concat
    property, vars

    
let BistableZ3 (z3:Microsoft.Z3.Context) (S:Dynamical) =                     
    let state_vars = Array.append S.State S.consLaws
    let p1 = state_vars |> Array.map(fun x -> x, sprintf "%s_1" x) 
    let p2 = state_vars |> Array.map(fun x -> x, sprintf "%s_2" x)

    let S1 = S.Subst (p1 |> Map.ofArray)
    let S2 = S.Subst (p2 |> Map.ofArray)

    let vars = 
        S1.GetVars + S2.GetVars
        |> Set.toArray
        |> Array.map(fun v ->  
            let v' = z3.MkFreshConst(v, z3.RealSort) :?> Microsoft.Z3.ArithExpr
            v, v')
        |> Map.ofArray 

    let unique = 
        Array.map2 (fun (_,v) (_,v') -> z3.MkNot(z3.MkEq(vars.[v], vars.[v']))) p1 p2
        |> z3.MkOr

    let J1 = S1.J () |> Matrix.encode vars z3
    let J2 = S2.J () |> Matrix.encode vars z3

    let property =         
        [| S1.EqCst |> Array.map (Encoding.BoolExpr2RealRec vars z3) //equilibrium of state 1
         ; S2.EqCst |> Array.map (Encoding.BoolExpr2RealRec vars z3) //equilibrium of state 2
         ; Stability.StableZ3 Stability.Default z3 J1    //stability of state 1
         ; Stability.StableZ3 Stability.Default z3 J2    //stability of state 2
         ; [|unique|]                                        //the two states are distinct
         |]
        |> Array.concat

    property, vars
