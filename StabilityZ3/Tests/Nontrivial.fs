// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Tests.Nontrivial

open Microsoft.Research.Biology.StabilityZ3.CRNs.Enumerator
open Xunit

let isCrnNontrivial crn_str = 
    let S, _ = 
        crn_str 
        |> Microsoft.Research.CRNEngine.Crn.from_string 
        |> Microsoft.Research.Biology.StabilityZ3.SymbolicODEs.Stoich
    isNonTrivial false S

[<Fact(DisplayName="Trivial CRN 1")>]
let Trivial1 () = 
    "A->B+C | ->B+C" |> isCrnNontrivial |> Assert.False
    
[<Fact(DisplayName="Trivial CRN 2")>]
let Trivial2 () = 
    "A + B -> | 2B -> A + C | 2A -> B + C | C -> B" |> isCrnNontrivial |> Assert.False
    
[<Fact(DisplayName="Non-trivial CRN 1")>]
let Nontrivial1 () = 
    "A+B -> A+C | 2C -> B+C" |> isCrnNontrivial |> Assert.True
    
[<Fact(DisplayName="Non-trivial CRN 2")>]
let Nontrivial2 () = 
    "A -> B+C | -> B+C | B+C -> A | B+C -> " |> isCrnNontrivial |> Assert.True
    
[<Fact(DisplayName="Non-trivial CRN 3")>]
let Nontrivial3 () = 
    "2A->B+C | A+C->B+D | B+F->D+F | D+E->A+E" |> isCrnNontrivial |> Assert.True

[<Fact(DisplayName="Non-trivial CRN 3 (adversarial ordering)")>]
let Nontrivial3_Hand () = 
    // D+E->A+E | B+F->D+F | A+C->B+D | 2A->B+C
    [| [|0;0;0;0|]; [|0;0;0;0|]; [|0;0;-1;1|]; [|1;0;-1;-2|]; [|-1;1;1;0|]; [|0;-1;1;1|]|] |> isNonTrivial false |> Assert.True

[<Fact(DisplayName="Non-trivial CRN 4")>]
let Nontrivial4 () = 
    "C+E->2A | A+E->B+C | B+F->E+F | C+D->B+D" |> isCrnNontrivial |> Assert.True



let chunk n x = Array.sub x 0 n |> String.concat "\n"
[<Fact(DisplayName="GenCRN (3,3) CRNs")>]
let GenCRN33 () = 
    let nr = 3
    let all_strs = System.IO.File.ReadAllLines "all_3_3.txt" |> Array.chunkBySize (nr+1) |> Array.map (chunk nr)
    (*let nt_strs = System.IO.File.ReadAllLines "nontrivial_3_3.txt" |> Array.chunkBySize (nr+1) |> Array.map (chunk nr)
    let all_results = all_strs |> Array.map (fun l -> Array.contains l nt_strs)
    let debug_str = all_results |> Array.map (fun r -> if r then "1" else "0") |> String.concat ""*)
    let all_results = (System.IO.File.ReadAllText "result_3_3.txt").ToCharArray() |> Array.map (fun c -> c = '1')

    //let all_crns = all_strs |> Array.map Crn.from_string
    (all_strs, all_results) 
    ||> Array.iter2 (fun str nt ->
        let result = isCrnNontrivial str
        Assert.Equal (nt, result)
    )
