// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Oslo open Oslo.Vector open Oslo.SparseMatrix

[<JavaScript>]
type Probabilities = 
  {
    times: float list;
    stateprobabilities : float[] list;
    stoichiometry : float[][];
    species: Hashtable.t<string,int>;
  }
  static member empty = { 
    times = List.empty; 
    stateprobabilities = List.empty; 
    stoichiometry = [||]; 
    species = Hashtable.empty();
  }
  static member get_bounds (probabilities:Probabilities) (species_name:string) : float[] * int * int =
    let index = Hashtable.find probabilities.species species_name 
    let Scol = Array.init probabilities.stoichiometry.Length (fun i -> probabilities.stoichiometry.[i].[index]) 
    let sp_max = (int32) (Array.max Scol) 
    let sp_min = (int32) (Array.min Scol) 
    (Scol,sp_max,sp_min)
  //Return the marginal probability distribution for species sp
  static member probability_map lowerbound (probabilities:Probabilities) (species_name:string) : float [] list =
    let (Scol,sp_max,sp_min) = Probabilities.get_bounds probabilities species_name 
    let groups = Scol |> Array.mapi (fun i n -> (i,(int32) n)) |> Array.toSeq |> Seq.groupBy snd 
    let cmap = groups |> Seq.map (fun (g,l) -> (g, Seq.map fst l)) |> Map.ofSeq 
    //Construct a sparse matrix that enables matrix multiplication to pick out specific copy numbers.
    let nstates = Scol.Length 
    let data = 
      Array.map ( fun i -> 
        match Map.tryFind i cmap with
        | None -> ([||], [||], 0)
        | Some l -> let n = Seq.length l in (Array.create n 1.0, Array.ofSeq l, n)
      ) [| sp_min .. sp_max |]
    let items = Array.map Lib.fst3 data 
    let locs = Array.map Lib.snd3 data 
    let count = Array.map Lib.trd3 data 
    let M = SparseMatrix.SparseMatrix(sp_max - sp_min + 1, nstates, items, locs, count) 
    let bound = match lowerbound with Some lb -> Array.map (fun xi -> if xi < lb then 0.0 else xi) | None -> id
    //Map each state-solution vector to an array of summed probabilities for each copy number.
    probabilities.stateprobabilities |> List.map (M.times >> bound)

