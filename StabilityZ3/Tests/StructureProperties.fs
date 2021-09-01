// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Tests.StructureProperties

open Microsoft.Research.Biology.StabilityZ3.CRNs.Enumerator
open Xunit

[<Fact(DisplayName="Strongly-Connected CRN 1")>]
let StronglyConnected1 () =         
    //A -> B    
    let S = [|[|-1|]; [|1|]|]
    let Sr = [|[|1|]; [|0|]|]
    IsStronglyConnected false S Sr
    |> Assert.False

[<Fact(DisplayName="Strongly-Connected CRN 2")>]
let StronglyConnected2 () =         
    //A <-> B    
    let S = [|[|-1; 1|];[|1; -1|]|]
    let Sr = [|[|1; 0|];[|0; 1|]|]
    IsStronglyConnected false S Sr
    |> Assert.True

[<Fact(DisplayName="Strongly-Connected CRN 3")>]
let StronglyConnected3 () =         
    //A -> B | B -> C    
    let S = [|[|-1; 0|]; [|1; -1|]; [|0; 1|]|]
    let Sr = [|[|1; 0|]; [|0; 1|]; [|0; 0|]|]
    IsStronglyConnected false S Sr
    |> Assert.False

[<Fact(DisplayName="Strongly-Connected CRN 4")>]
let StronglyConnected4 () =         
    //A -> B | B -> C | C -> A
    let S = [|[|-1; 0; 1|];[|1; -1; 0|]; [|0; 1; -1|]|]
    let Sr = [|[|1; 0; 0|];[|0; 1; 0|];[|0; 0; 1|]|]
    IsStronglyConnected false S Sr
    |> Assert.True

[<Fact(DisplayName="Strongly-Connected CRN 5")>]
let StronglyConnected5 () =         
    //A <-> B | B -> C 
    let S = [|[|-1; 1; 0|];[|1; -1; 0|];[|0; 0; 1|]|]
    let Sr = [|[|1; 0; 0|];[|0; 1; 0|];[|0; 1; 0|]|]
    IsStronglyConnected false S Sr
    |> Assert.False

[<Fact(DisplayName="Strongly-Connected CRN 6")>]
let StronglyConnected6 () =         
    //A <-> B | B -> C  | B + C -> C
    let S = [|[|-1; 1; 0; 0|];[|1; -1; 0; -1|];[|0; 0; 1; 0|]|]
    let Sr = [|[|1; 0; 0; 0|];[|0; 1; 0; 1|];[|0; 1; 0; 1|]|]
    IsStronglyConnected false S Sr
    |> Assert.False

[<Fact(DisplayName="Strongly-Connected CRN 7")>]
let StronglyConnected7 () =         
    //A <-> B | B -> C  | B + C -> B
    let S = [|[|-1; 1; 0; 0|];[|1; -1; 0; 0|];[|0; 0; 1; -1|]|]
    let Sr = [|[|1; 0; 0; 0|];[|0; 1; 0; 1|];[|0; 1; 0; 1|]|]
    IsStronglyConnected false S Sr
    |> Assert.True


