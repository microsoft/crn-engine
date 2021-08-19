module Microsoft.Research.Biology.StabilityZ3.CRNs.ByteEncoding

open System.IO
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine

type Entry<'a> = Microsoft.Research.CRNEngine.Mset.entry<'a>
type Mset<'a> = Microsoft.Research.CRNEngine.Mset.t<'a>
// Hashing function:
(* 
   Let N be the total number of species, and m be the maximum number of complexes.

   hash(C_1 -> C_2) = hash(C_1)*m + hash(C_2)

   hash(C) = 
      0                       if C = 0 
      1+i                     if C = A_i
      N+1+i                   if C = 2A_i
      2N+1+hash(i,j)          if C = A_i + A_j, i < j
   
   hash(i, j) = (N i - i(i+1)/2) + (j - i - 1)
*)

let entry element multiplicity : Entry<int> = { element = element; multiplicity = multiplicity}
let divRem c m = (float (c - (c % m)) / float m) |> int, c % m

let rec identifyHeterodimer ns i c = 
  if c = 0 
  then failwith "The first heterodimer should have index 1"
  if c < ns
  then (i, i + c)
  else identifyHeterodimer (ns-1) (i+1) (c - ns + 1)

let rec identifyHeterotrimer ns i c = 
  let bound = (ns - 1) * (ns - 2) / 2
  if (c - i) <= bound
  then 
    let j, k = identifyHeterodimer (ns-1) 0 (c-i) 
    [entry i 1; entry (i+j+1) 1; entry (i+k+1) 1]
  else identifyHeterotrimer (ns - 1) (i + 1) (c - bound + 1)

(********* Decoded CRN to CRNEngine.Crn *********)
// Utility function to assign a species name to an index in the byte encoding
let fullAlphabet = [| 'A' .. 'Z' |] |> Array.map (fun x -> System.Char.ToString x)
let namer (ms:Mset<int>) : Mset<Species> = Mset.map (fun i -> Species.create (fullAlphabet.[i])) ms

let decodeReaction (i:int) ((x,y) : Mset<int> * Mset<int>) : Reaction<Species,Value,Functional> = // i = ith reaction in a CRN
  // turn all indices into names
  let rs = namer x
  let ps = namer y
  let rt  = sprintf "k%i" i |> Expression.Key |> Rate.MassAction
  // static member create(catalysts:Mset.t<'s>,reactants:Mset.t<'s>,reverse: Rate<'v,'e> option, rate: Rate<'v,'e>,products:Mset.t<'s>) = 
  {catalysts=Mset.empty;reactants=rs;reverse=None;rate=rt;products=ps}

let decodeCRN (x : (Mset<int> * Mset<int>)[]) : Crn =
  x 
  |> Array.mapi decodeReaction 
  |> Array.fold (fun crn r -> { crn with reactions = r :: crn.reactions} ) Crn.empty
(******************)

let complexToMSet numSpecies molecularity (c:int) : Mset<int> =
  let homomerRange = numSpecies * molecularity
  if c = 0
  then Mset.Empty
  elif c > homomerRange
  then // heteromer
    match molecularity with
    | 2 -> let i, j = identifyHeterodimer numSpecies 0 (c - molecularity*numSpecies) in [entry i 1; entry j 1]
    | 3 -> 
        let heterodimerSize = numSpecies * (numSpecies-1) / 2
        let heterotrimerIndex = homomerRange + 3 * heterodimerSize
        if c > heterotrimerIndex    // Are we hetero-trimer?
        then identifyHeterotrimer numSpecies 0 (c - heterotrimerIndex)
        elif c <= homomerRange + heterodimerSize
        then    // We are (hetero)dimer
          let i, j = identifyHeterodimer numSpecies 0 (c - homomerRange) in [entry i 1; entry j 1]
        elif c <= homomerRange + 2 * heterodimerSize    // Are we dimer-monomer?
        then    // We are dimer-monomer
          let i, j = identifyHeterodimer numSpecies 0 (c - homomerRange - heterodimerSize) in [entry i 2; entry j 1]
        else    // We are monomer-dimer
          let i, j = identifyHeterodimer numSpecies 0 (c - homomerRange - 2 * heterodimerSize) in [entry i 1; entry j 2]
    | _ -> failwithf "Molecularity %d is not supported" molecularity
  else // homomer
    let mul, elem = divRem (c - 1) numSpecies
    [ { element = elem; multiplicity = mul + 1} ]

let decode numSpecies molecularity (reactionHash:int) : Mset<int> * Mset<int> =
    let numComplexes = MathNet.Numerics.Combinatorics.Combinations (numSpecies + molecularity, molecularity) |> int
    let reactantsHash, productsHash = divRem reactionHash numComplexes
    complexToMSet numSpecies molecularity reactantsHash, complexToMSet numSpecies molecularity productsHash


let readByteEncoding (crn_settings:Enumerator.Settings) path : Dynamical [] = 
  printfn "Reading byte encoded CRNs..."
  let reader = new BinaryReader(File.OpenRead(path))
  let inline charToInt c = int c - int '0'
  let numSpecies = reader.ReadChar() |> charToInt
  reader.ReadChar() |> ignore
  reader.ReadChar() |> ignore
  
  let numReactions = reader.ReadChar() |> charToInt
  reader.ReadChar() |> ignore
  reader.ReadChar() |> ignore
  
  let reactionByteSize = if numSpecies <= 4 then 1 else 2
  let crnByteSize = reactionByteSize * numReactions

  let acc = ref []
  
  
  let mutable buffer : byte array = Array.zeroCreate crnByteSize
  let rec f () =
    let payload = reader.Read(buffer,0,crnByteSize)
    if payload = 0 then ()
    elif payload <> crnByteSize then failwithf "Error: the input file contains a CRN with fewer than %i reactions." numReactions
    else 
      let crnEncoding =
        buffer
        |> Array.splitInto numReactions
        |> Array.map (fun x -> 
          let encodedReaction =
            match x.Length with 
            | 1 -> int (x.[0])
            | 2 -> int (x.[0]) + ((int x.[1]) <<< 8)
            | _ -> failwithf "Unsupported reaction byte encoding length %i" reactionByteSize
          encodedReaction)
      let crn = 
        crnEncoding 
        |> Array.map (decode numSpecies 2)
        |> decodeCRN
        |> fun x -> x.saturate_initials()

      let crnName = crnEncoding |> Array.map (string) |> String.concat ","
      acc := (crnName, crn) :: !acc
      f ()
      
  let crns = 
    f ()
    !acc
    |> List.rev
  
  printfn "Read %i systems" crns.Length

  printfn "Filtering..."
  let filteredCrns =
    crns 
    |> List.filter (fun (_, crn) ->
      // exclude reactions that create dimers (e.g. -> 2A or -> A+B)
      let producesDimer =
        crn.reactions
        |> List.exists (fun reaction ->
              reaction.reactants.Length = 0
              && ((reaction.products.Length = 1 && reaction.products.Head.multiplicity <> 1)
                   || reaction.products.Length > 1 )
        )
      
      // exclude homodimer loss reactions (keep 2X -> 0, remove 2X -> X)
      let homodimerLoss =
        crn.reactions
        |> List.exists (fun reaction -> 
            reaction.reactants.Length = 1
            && reaction.reactants.Head.multiplicity = 2 
            && reaction.products.Length = 1 
            && reaction.products.Head.multiplicity = 1 
            && reaction.reactants.Head.element = reaction.products.Head.element
        )
      
      not producesDimer && not homodimerLoss 
    )
  printfn "Remaining %i systems" filteredCrns.Length

  filteredCrns
  |> List.map (fun (crnName, crn) -> Analyser.DynamicalFromByteCodeCrn crn_settings.Dvary false crn crnName)
  |> Array.ofList
