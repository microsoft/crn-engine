// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Tiles

// This module builds a calculus of tiled and free species
// based on a calculus of free species and a predicate is_bound

type 's s =
  | Free of 's      // This species should not be tethered
  | Tile of 's list // All species in here are meant to be tethered

let map_reactions rs = rs |> Set.map (Calculus.map_reaction Free)

let leave1out f l =
  let rec loop acc e after =
    match after with
    | [] -> (f e [])::acc |> List.rev
    | a::afters -> loop (f e after::acc) a afters
  match l with
  | [] -> []
  | h::t -> loop [] h t

let lift_mono (c:'s Calculus.t) =
  function
  | Free f -> c.mono f |> map_reactions
  | Tile ss ->
    let react s rest =
      seq {
        yield c.mono s
        yield! rest |> List.map (c.bin s)
      } |> Set.unionMany
        |> Set.map (fun r -> { Calculus.reactants = [r.reactants@rest |> Tile]
                             ; Calculus.products  = [r.products@rest  |> Tile]
                             ; Calculus.rate      = r.rate }) // TODO: Take local concentration into account
    ss |> leave1out react
       |> Set.unionMany

let lift_bin is_bound (c:'s Calculus.t) s1 s2 =
  match s1, s2 with
  | Free f1, Free f2 -> c.bin f1 f2 |> map_reactions
  | Free f, Tile ss
  | Tile ss, Free f ->
    let react s rest =
      let split fs =
        let bound, free = fs |> List.partition is_bound
        (bound |> Tile)::(free |> List.map Free)
      c.bin s f
        |> Set.map (fun r -> { Calculus.reactants = r.reactants@rest |> split
                             ; Calculus.products  = r.products@rest  |> split
                             ; Calculus.rate      = r.rate }) // TODO: Take local concentration into account
    ss |> leave1out react
       |> Set.unionMany
  | Tile _, Tile _ -> Set.empty

let lift_species is_bound (c:'s Calculus.t) =
    c.initial_species
    |> List.map (fun s -> if s |> is_bound then Tile [s] else Free s)

let lift is_bound c = Calculus.from_mono_bin (lift_mono c) (lift_bin is_bound c) (lift_species is_bound c)
