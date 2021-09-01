// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Tests.LinearCrn

open Xunit
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.LinearCRN

let ENCODE_TO_Z3 = true
let neutral_type1 = 
    { isomorphisms  = true
    ; oscil         = true
    ; noise_amp     = TuringSymbolic.Neutral
    ; output        = RDNets
    ; types         = Type1
    ; timeout = 60
    } 

[<Fact(DisplayName="LinearCrn - Check 3 species, 6 reactions")>]
let check_s3r6 () = 
    let crn, _ = Encode 3 6 2
    let s = 
        {TuringSymbolic.TuringAnalysisSettings.Default with 
              prevent_oscillations = neutral_type1.oscil
            ; prevent_noise_amp    = neutral_type1.noise_amp
        }
    let S = Solver.CheckTuring ENCODE_TO_Z3 Solver.Portfolio s crn
    let solution = S.solution
    let result = match solution with SAT _ -> true | _ -> false
    Assert.True result

[<Fact(DisplayName="LinearCrn - Enumerate (2,4) neutral")>]
let enumerate_s2r4_neutral () = 
    let results = LinearCRN.Generate neutral_type1 Solver.Portfolio 2 4
    Assert.Equal (1, List.length results)

[<Fact(DisplayName="LinearCrn - Enumerate (3,6) neutral")>]
let enumerate_s3r6_neutral () = 
    let results = Generate neutral_type1 Solver.Portfolio 3 6
    let str = CrnsToString true results
    Assert.Equal (21, List.length results)

[<Fact(DisplayName="LinearCrn - Enumerate (3,6) lessthan")>]
let enumerate_s3r6_lessthan () = 
    let results = Generate  { neutral_type1 with noise_amp = TuringSymbolic.LessThan } Solver.Portfolio 3 6
    Assert.Equal (11, List.length results)

[<Fact(DisplayName="LinearCrn - Noise-amplifying example")>]
let noise_amp () = 
    let S = FromMatrix 2 None [|[|0;0;1|]; [|1;1;1|]; [|0;1;1|]|]
    let result = Solver.CheckTuring ENCODE_TO_Z3 Solver.Portfolio TuringSymbolic.TuringAnalysisSettings.Default S
    Assert.True result.solution.isUNSAT


[<Fact(DisplayName="LinearCrn - 3-6-2 Example")>]
let check_362() = 
    let sys, _ = Encode 3 6 2
    let S = 
        sys 
        |> Dynamical.setUB "D_{X_1}" 1.0
        |> Solver.CheckTuring ENCODE_TO_Z3 Solver.Nlsat TuringSymbolic.TuringAnalysisSettings.Default

    let result = 
        match S.solution with 
        | SAT (_,r) ->  r.time < 1.0            
        | _ -> false

    Assert.True result


[<Fact(DisplayName="LinearCrn - 4-7-2 Example")>]
let check_472() = 
    let sys, _ = Encode 4 7 2
    let S = 
        sys 
        |> Dynamical.setUB "D_{X_1}" 1.0
        |> Solver.CheckTuring ENCODE_TO_Z3 Solver.Nlsat TuringSymbolic.TuringAnalysisSettings.Default

    let result = 
        match S.solution with 
        | SAT (_,r) ->  r.time < 60.0            
        | _ -> false

    Assert.True result
