// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.CRNs.Similarity

open System.IO

/// Rotates a list by one place forward.
let rotate lst =
    List.tail lst @ [List.head lst]

/// Gets all rotations of a list.
let getRotations lst =
    let rec getAll lst i = if i = 0 then [] else lst :: (getAll (rotate lst) (i - 1))
    getAll lst (List.length lst)

/// Gets all permutations (without repetition) of specified length from a list.
let rec getPerms n lst = 
    match n, lst with
    | 0, _ -> seq [[]]
    | _, [] -> seq []
    | k, _ -> lst |> getRotations |> Seq.collect (fun r -> Seq.map ((@) [List.head r]) (getPerms (k - 1) (List.tail r)))

let allPerms = getPerms 4 [0;1;2;3]

let Stoich  =
    sprintf "super_S3_stoich.tsv"
    |> File.ReadAllLines
    |> Array.map (fun x -> x.Split('\t') |> Array.map int)
let StoichReactant = 
    sprintf "super_S3_stoichReactant.tsv"
    |> File.ReadAllLines
    |> Array.map (fun x -> x.Split('\t') |> Array.map int)
    
///////////////////////////////////////////////

let transpose (matrix:_ [][]) =
    if matrix.Length = 0 then failwith "Invalid matrix"  
    Array.init matrix.[0].Length (fun i -> 
        Array.init matrix.Length (fun j -> 
            matrix.[j].[i]))
            
let mnorm1 (A:int[][]) =
    A |> Array.map (Array.map abs)|> transpose |> Array.map Array.sum |> Array.max
    
let mnorminf (A:int[][]) =
    A |> Array.map (Array.map abs)|> Array.map Array.sum |> Array.max
    
let mnorm2 (A:int[][]) = 
    A |> Array.map (Array.map (fun x -> x*x)) |> Array.map Array.sum |> Array.sum |> float |> sqrt
    
let matrixMinus (A:int[][]) (B:int[][]) = Array.map2 (fun x y -> Array.map2 (-) x y) A B

//////////////////////////////////////////////// 

let CRNdistance id1 id2 =
    let subStoich1 = Stoich |> Array.map (fun stoich_i -> id1 |> List.map (Array.get stoich_i) |> Array.ofList)
    let subStoich2 = Stoich |> Array.map (fun stoich_i -> id2 |> List.map (Array.get stoich_i) |> Array.ofList)
    let subStoichReactant1 = StoichReactant |> Array.map (fun stoich_i -> id1 |> List.map (Array.get stoich_i) |> Array.ofList)
    let subStoichReactant2 = StoichReactant |> Array.map (fun stoich_i -> id2 |> List.map (Array.get stoich_i) |> Array.ofList)
    let permutedSubStoich1 = allPerms |> Seq.map (fun x-> transpose (Array.permute (fun y -> x.[y]) (transpose subStoich1) ))
    let permutedSubStoichReactant1 = allPerms |> Seq.map (fun x-> transpose (Array.permute (fun y -> x.[y]) (transpose subStoichReactant1) ))
    let distStoich=permutedSubStoich1 |> Seq.map (matrixMinus subStoich2) |> Seq.map mnorm1 |> Seq.min
    let distStoichReactant=permutedSubStoichReactant1 |> Seq.map (matrixMinus subStoichReactant2) |> Seq.map mnorm1 |> Seq.min

    distStoich+distStoichReactant
/////////////////////////

let S3R4Similarity = 
    let results = File.ReadAllLines "allSAT_S3_R4_FilterNoise.csv" |> Array.map (fun s -> s.Split(',') |> Array.map int)|>Array.map (List.ofArray)|>List.ofArray
    let distances = results|>List.map (fun x -> results|>List.map (CRNdistance x)) |>List.map (fun row -> row |> List.map string |> String.concat ",")
    File.WriteAllLines ("AllSatDistances.csv", distances)

