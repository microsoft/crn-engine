module Tests.ByteEncoding

open Xunit
open Microsoft.Research.Biology.StabilityZ3.CRNs.ByteEncoding

let complexes n m = MathNet.Numerics.Combinatorics.Combinations (n + m, m) |> int
let expectedReactions n m = 
  let c = complexes n m
  c * (c - 1)

let distinctReactions n m = 
  let expected = expectedReactions n m
  let decoded = Array.init expected (decode n m)
  expected, decoded |> Array.distinct

let distinctComplexes n m = 
  let expected = complexes n m
  let decoded = Array.init expected (complexToMSet n m)
  expected, decoded |> Array.distinct
  
let orderedCorrectly = 
  Array.iter (fun ms -> 
    ms 
    |> List.map (fun (e:Entry<int>) -> e.element) 
    |> List.pairwise 
    |> List.forall (fun (a,b) -> a < b)
    |> Assert.True
  )

[<Fact(DisplayName="Decode BImolecular complexes")>]
let complexesBimolecular () = 
  let molecularity = 2
  [2..5]
  |> List.iter (fun numSpecies -> 
    let expected, decoded = distinctComplexes numSpecies molecularity
    Assert.Equal (expected, decoded.Length)
    decoded |> orderedCorrectly
  )

[<Fact(DisplayName="Decode TRImolecular complexes")>]
let complexesTrimolecular () = 
  let molecularity = 3
  [2..5]
  |> List.iter (fun numSpecies -> 
    let expected, decoded = distinctComplexes numSpecies molecularity
    Assert.Equal (expected, decoded.Length)
    decoded |> orderedCorrectly
  )
  
[<Fact(DisplayName="Decode 2 species bimolecular")>]
let decode2bimolecular () =
  let numSpecies = 2
  let molecularity = 2
  let expected, decoded = distinctReactions numSpecies molecularity
  Assert.Equal (expected, decoded.Length)

[<Fact(DisplayName="Decode 3 species bimolecular")>]
let decode3bimolecular () = 
  let numSpecies = 3
  let molecularity = 2
  let expected, decoded = distinctReactions numSpecies molecularity
  Assert.Equal (expected, decoded.Length)

[<Fact(DisplayName="Decode 4 species bimolecular")>]
let decode4bimolecular () = 
  let numSpecies = 4
  let molecularity = 2
  let expected, decoded = distinctReactions numSpecies molecularity
  Assert.Equal (expected, decoded.Length)


[<Fact(DisplayName="Decode 2 species trimolecular")>]
let decode2trimolecular () =
  let numSpecies = 2
  let molecularity = 3
  let expected, decoded = distinctReactions numSpecies molecularity
  Assert.Equal (expected, decoded.Length)

[<Fact(DisplayName="Decode 3 species trimolecular")>]
let decode3trimolecular () = 
  let numSpecies = 3
  let molecularity = 3
  let expected, decoded = distinctReactions numSpecies molecularity
  Assert.Equal (expected, decoded.Length)

[<Fact(DisplayName="Decode 4 species trimolecular")>]
let decode4trimolecular () = 
  let numSpecies = 4
  let molecularity = 3
  let expected, decoded = distinctReactions numSpecies molecularity
  Assert.Equal (expected, decoded.Length)


// Compare genCRN results
type Species = Microsoft.Research.CRNEngine.Species
let alphabet = [|"A"; "B"; "C"; "D"|] |> Array.map (fun a -> { name = a } : Species)
let mapMset (set:Mset<int>) : Mset<Species> =
  set |> List.map (fun e -> { element = alphabet.[e.element]; multiplicity = e.multiplicity})

[<Fact(DisplayName="Decode genCRN (4,2) nontrivial")>]
let decode_gencrn () = 
  let bytes = System.IO.File.ReadAllBytes "nontrivial_4_2.dat" |> Array.skip 6 |> Array.chunkBySize 2
  let reactions : (Mset<Species> * Mset<Species>)[][] = 
      bytes 
      |> Array.map (
          Array.map (fun b -> 
              let reactants, products = int b |> decode 4 2
              ( reactants |> mapMset, products |> mapMset )
          )
      )
  
  let expected : (Mset<Species> * Mset<Species>)[][] = 
      System.IO.File.ReadAllLines "nontrivial_4_2.txt"
      |> Array.chunkBySize 3
      |> Array.map (fun lines -> 
          let crn = lines.[0..1] |> String.concat "" |> Microsoft.Research.CRNEngine.Crn.from_string
          crn.reactions |> List.map (fun rs -> rs.reactants, rs.products) |> Array.ofList
      )
  
  (expected, reactions)
  ||> Array.iter2 (
    Array.iter2 (fun (eR,eP) (oR,oP) -> 
      Assert.Equal<Mset<Species>>(eR, oR)
      Assert.Equal<Mset<Species>>(eP, oP)
    )
  ) 
  //Assert.Equal<(Mset<Species> * Mset<Species>)[][]>(expected, reactions)