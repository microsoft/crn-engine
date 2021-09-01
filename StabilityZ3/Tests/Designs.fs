// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Tests.Designs

open Xunit
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine.Expression

let turingSettings = TuringSymbolic.TuringAnalysisSettings.Default     
    
[<Fact(DisplayName="Designs - Beta", Skip = "Too slow")>]
let beta () =     
    let full = Examples.betaCrn ()
    let reduced, repl = full.ModelReduceKeep ["C6";"C12";"aiiA"]

    let expected = 
        [
          "D6", "(bR6^(2)*aR^(2)*C6^(2)*bD6)/(uR6^(2)*dR^(2)*uD6)"
          "E12", "(bS12^(2)*aS^(2)*C12^(2)*bE12)/(uS12^(2)*dS^(2)*uE12)"
          "GOLasE12", "(cons_0*bGE12*aS^(2)*C12^(2)*bE12*bS12^(2))/((bS12^(2)*bE12*C12^(2)*aS^(2)*bGE12) + (uS12^(2)*uE12*dS^(2)*uGE12))"
          "GOLuxD6", "(cons_1*bGD6*aR^(2)*C6^(2)*bD6*bR6^(2))/((bR6^(2)*bD6*C6^(2)*aR^(2)*bGD6) + (uR6^(2)*uD6*dR^(2)*uGD6))"
          "R6", "(bR6*C6*aR)/(uR6*dR)"
          "S12", "(bS12*C12*aS)/(uS12*dS)"
          "aiiAC12", "(bA*C12*aiiA)/(kA12 + uA)"
          "aiiAC6", "(bA*C6*aiiA)/(kA6 + uA)"
          "lasI", "(((bS12^(2)*bE12*a1*C12^(2)*aS^(2)*bGE12) + (uS12^(2)*uE12*a081*dS^(2)*uGE12))*cons_0)/(((bS12^(2)*bE12*C12^(2)*aS^(2)*bGE12) + (uS12^(2)*uE12*dS^(2)*uGE12))*dI)"
          "lasR", "aS/dS"
          "luxI", "(((bS12^(2)*bE12*a1*C12^(2)*aS^(2)*bGE12) + (uS12^(2)*uE12*a081*dS^(2)*uGE12))*cons_0)/(((bS12^(2)*bE12*C12^(2)*aS^(2)*bGE12) + (uS12^(2)*uE12*dS^(2)*uGE12))*dI)"
          "luxR", "aR/dR"
        ]
        |> Map.ofList 
        |> Map.map (fun k v -> ExpressionFunctions.Parse v)

    repl |> Map.iter (fun k v -> Assert.Equal (MathNetWrapper.SimplifyNum (expected.[k] - v), Float 0.0))

    (*let solver = Solver.solverType.PortfolioTO 10000u
    let out1,time1,unk1 = Solver.CheckTuring true solver turingSettings sys
    Assert.True (out1.solution.IsSome)
    let out2,time2,unk2 = Solver.CheckTuring false solver turingSettings sys
    Assert.True (out2.solution.IsSome)*)

[<Fact(DisplayName="Designs - Gamma")>]
let GammaTest () = 
    let solver = Solver.solverType.PortfolioTO 240000u
    let settings = TuringSymbolic.TuringAnalysisSettings.Default
       
    let Gamma =
        let C6 = Key "C6"
        let C12 = Key "C12"
        let aiiA = Key "aiiA"
        let k6 = Key "k6"
        let k4 = Key "k4"
        let k5 = Key "k5"
        let k3 = Key "k3"
        let k1 = Key "k1"
        let k2 = Key "k2"
        let k8 = Key "k8"
        let k7 = Key "k7"    
    
        let odes = 
            [| "C6", (k4*C12*C12/(C12*C12+k7))-k1*C6*aiiA
             ; "C12", k5*(C12*C12/(C12*C12+k7))-k2*C12*aiiA
             ; "aiiA", k6*(C6*C6/(C6*C6+k8))-k3*aiiA
             |]
    
        //Dynamical.Create(odes, ["C6"; "C12"; "CI"])
        Dynamical.Create(odes, Map.ofList ["C6",Key "Diff6"; "C12",Key "Diff12"]) 
        |> Dynamical.setAllLB 0.0
        //|> Dynamical.setUB "Dy" 1.01

    let res = Gamma |> Solver.CheckTuring true solver settings    
    Assert.Equal("SAT", res.solution.Status)
