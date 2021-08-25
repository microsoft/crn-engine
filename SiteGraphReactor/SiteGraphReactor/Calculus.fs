// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Calculus

(* Maybe later..
type 's mset = ('s * int) list
let map_mset f (m: 's mset) = m |> List.map (fun (s,n) -> f s, n)
*)

// TODO: Add rate
type 's reaction =
  { reactants : 's list
  ; products  : 's list
  ; rate      : float }

let map_reaction f (r: 's reaction) =
  { reactants = r.reactants |> List.map f
  ; products  = r.products |> List.map f
  ; rate      = r.rate }

type 's t when 's:comparison =
  { mono : 's -> Set<'s reaction>
  ; bin : 's -> 's -> Set<'s reaction>
  ; reactions : Set<'s> -> 's -> Set<'s reaction>
  ; initial_species : 's list }

let from_mono_bin mono bin is =
  let reactions ss s =
    seq {
      yield mono s
      yield bin s s
      yield! ss |> Seq.map (bin s)
    } |> Set.unionMany
  { mono = mono; bin = bin; reactions = reactions; initial_species = is }

let reaction_graph limit (c: 's t) =
  let positivite_propensity (r: 's reaction) = true // TODO: should look at rate of r
  let rec work rs all_ss l =
    match l with
    | [] -> all_ss, rs
    | w::ws ->
      if limit > 0 && (Set.count all_ss > limit) then (w::ws |> Set.ofList |> Set.union all_ss), rs else
      printfn "Species: %d" (Set.count all_ss)
      let new_rs = c.reactions all_ss w - rs |> Set.filter positivite_propensity
      let is_new s = Set.contains s all_ss |> not
      let new_ss = new_rs |> Seq.collect (fun r -> r.products) |> Seq.distinct |> Seq.filter is_new |> Seq.toList
      work (Set.union new_rs rs) (Set.add w all_ss) (ws @ new_ss)
  work Set.empty Set.empty c.initial_species
