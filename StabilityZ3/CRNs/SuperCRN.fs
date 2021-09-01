// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.CRNs.SuperCRN

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine
open System.IO

type reaction = Reaction<Species,Value,Functional>

let stoichCreate (reactants : Species Mset.t) (speciesnamelist : Species list)=
    speciesnamelist
    |> List.map (fun x-> 
        match reactants |> List.tryFind (fun (y:Mset.entry<Species>) -> y.element=x) with 
        | Some multiset -> multiset.multiplicity 
        | None ->0
    )

let reactions_to_string reactions = 
  let value_to_string = Expression.to_string id
  let expr_to_string = Expression.to_string (Key.to_string Species.to_string)  
  reactions |> List.rev |> List.mapi (fun i r -> sprintf "%d,%s" i (Reaction.to_string Species.to_string value_to_string expr_to_string r)) 


let stoichiometricMatrix (reactions:reaction list) (speciesnamelist : Species list) = 
  let stoichmat = ref []
  reactions
  |> List.iter(fun x ->
    let reactantlist = stoichCreate x.reactants speciesnamelist
    let productlist = stoichCreate x.products speciesnamelist
    let stoich=List.zip reactantlist productlist |> List.map (fun x -> (snd x) - (fst x))
    stoichmat := stoich :: !stoichmat
    ) 
  (LinearAlgebra.transpose !stoichmat) |> List.map (fun row -> row |> List.map string |> String.concat "\t")

let stoichiometricReactantMatrix (reactions:reaction list) (speciesnamelist : Species list) = 
  let stoichmat = ref []
  List.iter(fun (x : reaction) ->
    let reactantlist = stoichCreate x.reactants speciesnamelist
    stoichmat := reactantlist :: !stoichmat
    ) reactions
  (LinearAlgebra.transpose !stoichmat) |> List.map (fun row -> row |> List.map string |> String.concat "\t")

let adjacency fname monomers homodimers heterodimers homotrimers heterotrimers =
  // empty compound
  let empty   = Mset.empty
  
  let reactants = [empty] @ monomers @ homodimers @ heterodimers
  let products = [empty] @ monomers @ homodimers @ heterodimers @ homotrimers @ heterotrimers
  let speciesnamelist = monomers |> List.map (fun x -> x.[0].element)
  let (=>) a b = not a || b // "implies" operator (logical implication)
  let matr = 
    Array2D.init reactants.Length products.Length (fun source target -> 
      // No self-reactions
      source <> target  
      // No production of multisets containing entries >1 (include at least homodimers)
      && (reactants.[source] = empty) => (products.[target] |> List.forall (fun ms -> ms.multiplicity <= 1) )
      // Only one autosynthesis (keep X -> 2X, remove X -> 3X)
      && not (List.contains reactants.[source] monomers && List.contains products.[target] homotrimers && reactants.[source].Head.element = products.[source].Head.element)
      // Only one homodimer loss reaction (keep 2X -> 0, remove 2X -> X)
      && not (List.contains reactants.[source] homodimers && List.contains products.[target] monomers && reactants.[source].Head.element = products.[source].Head.element)
      // Disallow transfer of homodimers to any other homo-polymer (covered by reaction converting to a monomer, but with additional copies)
      //&& not (List.contains reactants.[source] (monomers @ homodimers) && List.contains products.[target] (homodimers @ homotrimers) && reactants.[source].Head.element <> products.[target].Head.element)
      // Disallow X -> 3X, X -> 3Y and X + Y -> 3X
      //&& not (List.contains reactants.[source] (monomers @ heterodimers) && List.contains products.[target] homotrimers)
    )
  
  let reactions = ref []
  Array2D.iteri (fun i j isConnected -> 
    if isConnected
    then 
      let rate : Rate<Value,Functional> = Rate.MassAction (Expression.Key <| "k" + i.ToString() + "_" + j.ToString())
      let r = Reaction.create (empty, reactants.[i], None, rate, products.[j])
      reactions := r :: !reactions) matr

  let crn = Crn.create fname Crn_settings.defaults !reactions [] Stringmap.empty false
  // Write files

  let stoich_lines = stoichiometricMatrix !reactions speciesnamelist
  File.WriteAllLines (sprintf "%s_stoich.tsv" fname, stoich_lines)
  let stoichreactant_lines = stoichiometricReactantMatrix !reactions speciesnamelist
  File.WriteAllLines (sprintf "%s_stoichReactant.tsv" fname, stoichreactant_lines)
  let ode_lines = crn |> SymbolicODEs.ofCrn |> SymbolicODEs.toOdeString
  File.WriteAllLines (sprintf "%s_ode.txt" fname, ode_lines)
  let reaction_lines = crn.reactions |> reactions_to_string
  File.WriteAllLines (sprintf "%s_reactions.csv" fname, reaction_lines)
  

let alphabet="ABCDEFGHIJKLMNOPQRSTUVWXYZ"|>Array.ofSeq|>Array.map string

let rec pairs l = 
        [
        match l with
        | h::t -> 
            yield! t |> Seq.map (fun elem -> (h, elem))
            yield! t |> pairs
        | _ -> ()
        ]

let rec getCombsWithRep n lst = 
    match n, lst with
    | 0, _ -> seq [[]]
    | _, [] -> seq []
    | k, (x :: xs) -> Seq.append (Seq.map ((@) [x]) (getCombsWithRep (k - 1) lst)) (getCombsWithRep k xs)

let generate (n: int) numProducts =
    if n > 26 then failwith "Up to 26 species only"
    let alpha = alphabet.[0..(n-1)] |> List.ofArray
    let sp = alpha |> List.map Species.create
    
    let monomers = sp |> List.map (fun x -> Mset.from_list [x])
    let homodimers = sp |> List.map (fun x -> Mset.from_list [x;x])
    let heterodimers = pairs sp |> List.map (fun (x,y) -> Mset.from_list [x;y])
    let homotrimers, heterotrimers = 
        match numProducts with 
        | 2 -> ( []
               , []
               )
        | 3 -> let trimers = getCombsWithRep 3 sp |> Seq.map Mset.from_list |> List.ofSeq
               List.partition (fun x -> (x |> List.distinct |> List.length) = 1) trimers               
        | _ -> failwithf "Can't generate CRNs with %d products" numProducts

    let filename = sprintf "super_S%d_P%d" n numProducts
    adjacency filename monomers homodimers heterodimers homotrimers heterotrimers
