// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.CRNs.Enumerator

open Microsoft.Research.Biology.StabilityZ3
open System.IO

type Dvary = A | B
//type Encoding = Byte | Lookup
type Settings = 
    { numSpecies: int
    ; numReactions: int
    ; numProducts: int
    ; Dvary: Dvary
    ; subsets: int 
    ; chunkSize: int option
    //; encoding: Encoding
    ; verbose: bool
    }

type Filters = 
    { stronglyConnected: bool
    ; conservation: bool
    ; nontrivial: bool
    }

let EverySpeciesDegraded io (subStoichiometry:int[][]) = 
    let res = (subStoichiometry |> Array.map Array.min |> Array.max < 0)    // Ensures every species can be degraded (otherwise no finite equilibrium)
    if io then printfn "- Every species degraded? %b" res
    res

let EverySpeciesProduced io (subStoichiometry:int[][]) = 
    let res = (subStoichiometry |> Array.map Array.max |> Array.min > 0)    // Ensures every species can be produced (otherwise zero equilibrium)
    if io then printfn "- Every species produced? %b" res
    res

// Test for conservation law involving a diffusible species
let ConservationLawDiffusibleExists (subStoichiometry:int[][]) = 
    subStoichiometry 
    |> Microsoft.Research.Biology.StabilityZ3.Invariants.FarkasArray 
    |> Array.exists (fun elem -> (elem.[0] > 0) || (elem.[1] > 0))

// Test for any conservation law
let ConservationLawExists io (subStoichiometry:int[][]) = 
    let res = 
        subStoichiometry 
        |> Invariants.FarkasArray 
        |> Array.isEmpty |> not
    if io then printfn "- No conservation laws? %b" (not res)
    res
    
let IsStronglyConnected io (subStoichiometry:int[][]) (subStoichiometryReactant:int[][]) =  
    let N = subStoichiometry.Length      //number of species
    
    let G = 
        //every reactant is a source edge
        //every species with (negative/positive) net stoich is a target of the edge (-/+) 
        Array.init N (fun i -> 
            let rxns = subStoichiometryReactant.[i] |> Array.indexed |> Array.filter (snd >> (<>) 0) |> Array.map fst //indecies of reactions that consume species i            
            [0..N-1]
            |> Seq.filter(fun j -> Array.exists (fun r -> subStoichiometry.[j].[r]<>0 ) rxns)
            |> Set.ofSeq
            )                        
            
    let res = Microsoft.Research.Biology.Graph.isStronglyConnected G
    if io then printfn "- Is strongly connected? %b" res
    res

let isTrivialQuick (S:float[][]) =            
  // Quick test: check if there is a positive vector in the stoichiometry matrix
  S
  |> Array.exists (fun Si ->
    let mutable allNegative = true
    let mutable allPositive = true
    let mutable allZero = true
    Si |> Array.iter (fun sij ->
      if (sij < 0.0) 
      then
        allPositive <- false
        allZero <- false
      else 
        if (sij > 0.0)
        then
          allNegative <- false
          allZero <- false
    )
    (allPositive || allNegative) && not allZero
  )

(* Application of the Fourier-Motzkin Elimination algorithm to the case (\Gamma^{T}x >= 0) with x \neq 0, where \Gamma is the stoichimetry matrix of the CRN.
     (note: the stoichimoetry matrix \Gamma is not transposed in the code, we just reverse apply FME on it with indices reversed) *)
let FourierMotzkin tol fStoich =
    //for j in 0..speciesCount-1 do
    let rec processRow j (S:float[][]) =
      let speciesCount = S.Length
      let matrixSize = S.[0].Length
      let pos = Array.zeroCreate matrixSize 
      let neg = Array.zeroCreate matrixSize
      if j >= speciesCount 
      then false
      else
        let mutable psize = 0 // size of P
        let mutable nsize = 0 // size of N
        let mutable allCoefficientsAreZero = true
        let mutable allCoefficientsArePositive = true
        let mutable allCoefficientsAreNegative = true
        // optimization: the following two booleans are used to detect if there are inequalities of the form 
        // x_j >= 0 and -x_j >= 0    (i.e. single variable inequalities)
        let mutable someMonoCoefficientIsPositive = false
        let mutable someMonoCoefficientIsNegative = false
        for i in 0..matrixSize-1 do
          if (abs(S.[j].[i]) > tol) 
          then
            allCoefficientsAreZero <- false

            // check if all other coefficients are zero in the row
            let mutable onlyNonZeroCoefficient = true
            let mutable continueRunning = true
            let mutable k = j+1
            while (k < speciesCount && continueRunning) do
              if (abs(S.[k].[i]) > tol) 
              then 
                onlyNonZeroCoefficient <- false 
                continueRunning <- false
              k <- k + 1
          
            // mark down all positive and negative coefficients
            if onlyNonZeroCoefficient 
            then
              if (S.[j].[i] > tol) 
              then
                allCoefficientsAreNegative <- false
                someMonoCoefficientIsPositive <- true
                pos.[psize] <- i
                psize <- psize + 1
            
              else
                allCoefficientsArePositive <- false
                someMonoCoefficientIsNegative <- true
                neg.[nsize] <- i;
                nsize <- nsize + 1
            
            else
              if (S.[j].[i] > tol) 
              then 
                pos.[psize] <- i
                psize <- psize + 1
                allCoefficientsAreNegative <- false 
              else                  
                neg.[nsize] <- i
                nsize <- nsize + 1
                allCoefficientsArePositive <- false
        

        // optimization: if x_j >= 0 and x_j <= 0, x_j must be 0
        if (allCoefficientsAreZero || (someMonoCoefficientIsNegative && someMonoCoefficientIsPositive))
        then processRow (j+1) S // assign 0 to this element of the vector
        else

          // found a positive vector in x_j, by putting x_j to 1 or -1 and all other coefficients x_i with i \neq j to 0
          if (allCoefficientsAreNegative || allCoefficientsArePositive) 
          then true
          else
            if (psize = 0 || nsize = 0)
            then processRow (j+1) S
            else                                                 
              // allocate more space for the new equations
              let mutable newEquationsSize = matrixSize + psize * nsize
              let updatedS = 
                S |> Array.map (fun Sk -> 
                  Array.init newEquationsSize (fun i -> if i < matrixSize then Sk.[i] else 0.0)
                )
          
              // create new inequalities
              for pi in 0..psize-1 do
                for ni in 0..nsize-1 do
                  let newColIndex = matrixSize + pi * nsize + ni;
                  for row in (j+1)..speciesCount-1 do
                    let pelem = updatedS.[row].[pos.[pi]] / abs(updatedS.[j].[pos.[pi]])
                    let nelem = updatedS.[row].[neg.[ni]] / abs(updatedS.[j].[neg.[ni]])
                    let diff = pelem + nelem
                    updatedS.[row].[newColIndex] <- diff
          
              for k in (j+1)..speciesCount-1 do
                for pi in 0..psize-1 do updatedS.[k].[pos.[pi]] <- 0.0
                for ni in 0..nsize-1 do updatedS.[k].[neg.[ni]] <- 0.0
          
              processRow (j+1) updatedS
    
    processRow 0 fStoich

let isNonTrivial io (stoichiometry:int[][]) =
  
  let fStoich = Array.map (Array.map float) stoichiometry
  let res = 
      if isTrivialQuick fStoich
      then false
      else not (FourierMotzkin 1e-6 fStoich)
  if io then printfn "- Is Non-trivial? %b" res
  res
          

let Filter filters io (S,Sr) = 
    // Stability / non-triviality
    if filters.nontrivial
    then 
        isNonTrivial io S
    else 
        EverySpeciesDegraded io S && EverySpeciesProduced io S
    && 
    // Mass-conservation laws (depending on option)
    not (filters.conservation && ConservationLawExists io S)    
    && 
    // Strongly connected interaction graph  
    not (filters.stronglyConnected && not (IsStronglyConnected io S Sr))  


/// Obtain stoichiometry of super CRN
let Stoichiometry numSpecies numProducts : int[][] =     // Important that inner collection is an array, as access is faster than list
    sprintf "super_S%d_P%d_stoich.tsv" numSpecies numProducts
    |> File.ReadAllLines
    |> Array.map (fun x -> x.Split('\t') |> Array.map int)
    //|> List.ofArray

let StoichiometryReactant numSpecies numProducts : int[][] =     // Important that inner collection is an array, as access is faster than list
    sprintf "super_S%d_P%d_stoichReactant.tsv" numSpecies numProducts
    |> File.ReadAllLines
    |> Array.map (fun x -> x.Split('\t') |> Array.map int)
    //|> List.ofArray

let reactions crn_settings = 
    File.ReadAllLines (sprintf "super_S%d_P%d_reactions.csv" crn_settings.numSpecies crn_settings.numProducts) 
    |> Array.map (fun line -> line.Split(',') |> Array.item 1)

let ComplexHash numSpecies complex = ()
    
let ReactionHash reactants products = ()

let getStoichSubset (stoichiometry:int[][]) (stoichReactants:int[][]) index = 
    let subStoichiometry = stoichiometry |> Array.map (fun stoich_i -> index |> List.map (Array.get stoich_i) |> Array.ofList)
    let subStoichiometryReactants = stoichReactants |> Array.map (fun stoich_i -> index |> List.map (Array.get stoich_i) |> Array.ofList)
    (subStoichiometry, subStoichiometryReactants) 

let filetag crn_settings = sprintf "S%d_P%d_R%d" crn_settings.numSpecies crn_settings.numProducts crn_settings.numReactions

let Run filters crn_settings =
    let stoich  = Stoichiometry crn_settings.numSpecies crn_settings.numProducts
    let stoichR = StoichiometryReactant crn_settings.numSpecies crn_settings.numProducts
    let rs = reactions crn_settings
    let max = Array.length rs - 1
    printfn "- Generating subsets for %d reactions" max
    let filter = getStoichSubset stoich stoichR >> Filter filters false

    // Generates subsets iteratively, using previous subset to generate the next, in forward lexicographical order
    let nextCombination subs = 
        match List.tryFindBack (fun (i,xi) -> xi < max+i+1-crn_settings.numReactions) (List.indexed subs) with
        | Some (i,xi) -> 
            if i = 0 
            then printf "."; [xi+1..xi+crn_settings.numReactions] |> Some
            else (List.take i subs) @ (List.init (crn_settings.numReactions-i) (fun j -> xi+1+j)) |> Some
        | None        -> None

    let mutable count = 0
    let mutable chunks = 0
    let fname, chunkSize = 
        match crn_settings.chunkSize with
        | Some size -> (fun ch -> filetag crn_settings + sprintf "_%d.csv" ch), size
        | None   -> (fun _ -> filetag crn_settings + sprintf ".csv"), -1
    let stream = new StreamWriter(fname chunks, false)
    let rec checkNextCombination (stream:StreamWriter) subs = 
        match subs with 
        | Some elems -> 
            let stream = 
                if filter elems 
                then 
                    stream.WriteLine (String.concat "," (List.map string elems))
                    count <- count + 1
                    if count = chunkSize
                    then 
                        chunks <- chunks + 1
                        count <- 0
                        stream.Flush()
                        stream.Close()
                        new StreamWriter(fname chunks, false)
                    else stream
                else stream
            checkNextCombination stream (nextCombination elems)
        | None       -> 
            stream.Flush()
            stream.Close()
            printfn "\n- Found %d CRNs" (count + chunks*chunkSize)

    checkNextCombination stream (Some [0..crn_settings.numReactions-1])
