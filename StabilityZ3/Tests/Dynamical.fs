// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Tests.Dynamical

open Xunit
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.TuringSymbolic
open Microsoft.Research.CRNEngine.Expression

let turingSettings  =  TuringAnalysisSettings.Default     

let Var k = Microsoft.Research.CRNEngine.Expression.Key k

let Brusselator () = 
    let X = Var "X"
    let Y = Var "Y"
    let A = Var "A"
    let B = Var "B"    
    
    let odes = 
        [| "X", A - (B + 1.0) * X + X*X*Y
          ; "Y", B*X-X*X*Y
        |]
    
    Dynamical.Create(odes, ["X"; "Y"]) 
    |> Dynamical.setAllLB 0.0

[<Fact(DisplayName="Dynamical - Brusselator")>]
let SolveBrusselator () =
    let S = Brusselator ()
    let solver = Solver.solverType.PortfolioTO 240000u
    
    // Make sure we return SAT
    let res1 = S |> Solver.CheckTuring true solver turingSettings
    Assert.Equal("SAT", res1.solution.Status)

    // Make sure we return UNSAT when diffusion rates are equal
    let Seq = {S with diffusion = ["X", Float 1.0; "Y", Float 1.0] |> Map.ofSeq}
    let res2 = Seq |> Solver.CheckTuring true solver turingSettings
    Assert.Equal("UNSAT", res2.solution.Status)    

[<Fact(DisplayName="Dynamical - Print constraints")>]
let PrintBrusselator () =
    let S = {Brusselator () with diffusion = ["X", Float 1.0; "Y", Float 1.0] |> Map.ofSeq}
    S
    |> TuringSymbolic.Turing {turingSettings with prevent_noise_amp = No; prevent_oscillations = false}
    |> fst
    |> Array.map MathNetWrapper.Simplify 
    |> fun x -> sprintf "$$%s$$" (x.ToString())
