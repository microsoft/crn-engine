module Microsoft.Research.CRNEngine.Tests.RNGTests
open Xunit
open FsUnit
open Microsoft.Research.CRNEngine
open Microsoft.Research.Filzbach.Lib

let rec generateRandList (gen:IRng) count =
    let next,rng = gen.NextInt32()
    if count =1 then        
        [next]
    else
        next::(generateRandList rng) (count-1)


let generateRandListWithAdapter (gen:Rng.Random) count =
    List.init count (fun i -> gen.Next())

let seed = 123u

[<Fact(DisplayName="RNG - LCG - reproducibility")>]
let LCG_reproducibility () =    
    let l1 = generateRandList (LCGRng(seed)) 100
    let l2 = generateRandList (LCGRng(seed)) 100
    List.iter2 (fun (first:uint32) second -> Assert.Equal(first,second)) l1 l2

[<Fact(DisplayName="RNG - RngAdpater - reproducibility")>]
let rngAdpater_reproducibility () =    
    let l1 = generateRandListWithAdapter (Rng.Random(int seed)) 100
    let l2 = generateRandListWithAdapter (Rng.Random(int seed)) 100
    List.iter2 (fun (first:int) second -> Assert.Equal(first,second)) l1 l2

[<Fact(DisplayName="RNG - RngAdpater - upper bound constraint")>]
let rngAdpater_upper_bound_constraint () = 
    let count = 1000
    let rng_constraint = 30
    let rng = Rng.Random(int seed)
    let l1 = List.init count (fun i -> rng.Next(rng_constraint))
    List.iter (fun i -> Assert.True(i >= 0 && i < rng_constraint)) l1
    //checking that every element is present in the sequence
    let elems = [ 0..29 ]
    
    let checkResent elem l = 
        let index = List.tryFindIndex (fun i -> i = elem) l
        match index with
        | None -> failwith (sprintf "elemnent %d is not found in the sequnce" elem)
        | Some(i) -> ()
    List.iter (fun i -> checkResent i l1) elems

[<Fact(DisplayName="RNG - RngAdpater - JavaScript/.NET  identical sequnces(Next(max) call)")>]
//      !!! This test can give false positives  !!!
//          It contains hardcoded values, if the RngAdpater was switched to another RNG
//          Or RNG itself is updated, the hardcoded values bellow need to be updated as well
//
//          If you update the hardcoded values bellow please update the same values in
//          corresponding JS test in /ModellingEngine/CRNEngineTSWrapper/Tests/RNGTests.ts
//
let rngAdpater_JavaScript_DotNET_identical_sequnces_Next_max_call () =
   
    let l1 = Rng.emitTestSequence()
    
    let l2 = [|
                2837;
                4351;
                386;
                2208;
                3594;
                5902;
                3612;
                3268;
                797;
                6479;
        |]
    Array.iter2 (fun (first:int) second -> Assert.Equal(first,second)) l1 l2