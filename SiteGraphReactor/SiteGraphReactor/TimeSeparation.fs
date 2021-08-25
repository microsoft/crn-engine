// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.TimeSeparation

// This module builds a calculus of merged reactions
// based on a calculus and a notion of slow/fast mono-reactions

let rec cartesian ll =
    match ll with
    | [] -> [[]]
    | (l::ls) ->
        cartesian ls
        |> List.collect (fun path -> l |> List.map (fun x -> x::path))

let expand_choices l =
    l
    |> List.map Set.toList
    |> cartesian
    |> List.map List.concat
    |> Set.ofList

let alter_each f l =
    let rec loop pre = function
    | [] -> [pre]
    | [x] -> [pre@[f x]]
    | x::xs -> (pre@[f x]@xs) :: (loop (pre@[x]) xs)
    loop [] l

let collect_each f (l:'a list) =
    let surround b a (e:'a) = b@[e]@a
    let rec loop pre = function
    | [] -> [pre]
    | [x] -> f x |> List.map (surround pre [])
    | x::xs -> (f x |> List.map (surround pre xs)) @ (loop (pre@[x]) xs)
    loop [] l

let map_context f l =
    let rec loop pre = function
    | [] -> []
    | x::xs -> (f x (pre@xs)) :: (loop (pre@[x]) xs)
    loop [] l

let collect_context f l =
    let rec loop pre = function
    | [] -> []
    | x::xs -> (f x (pre@xs)) @ (loop (pre@[x]) xs)
    loop [] l


(*
  Variant of Kosaraju's algorithm (to work for a partial equivalence relation)
  https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm

  Notes:
   - For a given equivalence class, we are guaranteed to explore the entire equivalence class
*)
let rec get_stable mono_fast (s:'s) =
    let V = Set.empty<'s> |> ref
    let mark_visited i = V := Set.add i !V
    let is_visited i = Set.contains i !V
   
    let L = List.empty<'s> |> ref
    let record i = L := i::!L
   
    let prevs = Map.empty<'s,Set<'s>> |> ref
    let add_prev i p =
        let new_prevs =
            match Map.tryFind i !prevs with
            | None -> p |> Set.singleton
            | Some old_prevs -> Set.add p old_prevs
        prevs := Map.add i new_prevs !prevs
    let add_to_prevs p is =
        is |> Seq.iter (fun i -> add_prev i p)
    let get_prevs i =
        match Map.tryFind i !prevs with
        | None -> Set.empty
        | Some prevs -> prevs
   
    let splits = Map.empty<'s,Set<'s list>> |> ref
    let add_splits i ss =
        let new_splits =
            match Map.tryFind i !splits with
            | None -> ss |> Set.ofList
            | Some old_splits -> ss |> Set.ofList |> Set.union old_splits
        splits := Map.add i new_splits !splits
    let get_splits i = (!splits).[i]

    let rec visit i =
        if i |> is_visited then ()
        else
            i |> mark_visited
            let nexts, splits =
                mono_fast i
                |> Seq.fold (fun (n_acc, s_acc) (r: Calculus.reaction<'s>) ->
                                 match r.products with
                                 | [p] -> (p::n_acc, s_acc)
                                 | ps -> (n_acc, ps::s_acc))
                            ([],[])
            nexts |> add_to_prevs i
            splits |> add_splits i
            nexts |> Seq.iter visit
            record i
    visit s
    // Now [L] is generated in proper order

    let root = Map.empty<'s,'s> |> ref
    let get_root i = (!root).[i]
    let set_root r i = root := Map.add i r !root
    let has_root i = Map.containsKey i !root
   
    let transports = Map.empty<'s,Set<'s>> |> ref
    let add_transport r1 r2 =
        match Map.tryFind r1 !transports with
        | None -> transports := Map.add r1 (Set.singleton r2) !transports
        | Some set -> transports := Map.add r1 (Set.add r2 set) !transports
   
    let rec assign r i =
        if has_root i then
            let ri = get_root i
            if ri <> r then add_transport ri r
        else
            i |> set_root r
            i |> get_prevs |> Set.iter (assign r)
   
    !L |> List.iter (fun i -> if not <| has_root i then assign i i)
    // Now [roots] is populated

    splits :=
        !splits
        |> Map.toSeq
        |> Seq.map (fun (i,ss) -> get_root i, ss)
        |> Map.ofSeq
    // Now [splits] is indexed by roots

    // The function [rep] computes representatives in a manner independent of how the equivalence class was discovered
    // It should then give the same representative if the same class is discovered by different runs
    let get_class i =
        let r = i |> get_root
        !L
        |> List.filter (get_root >> ((=) r)) // This is the entire class
    let rep = get_class >> Seq.min

    (*
    let rec descendants i =
        let finish () = [rep i]
        match Map.tryFind i !transports with
        | None -> finish ()
        | Some ds ->
            if ds |> Set.isEmpty then finish () else
            ds
            |> Set.toList
            |> List.collect (get_root >> descendants)
    *)
    let rec stabilisations i =
        let finish () = [get_class i] |> Set.singleton
        let singles =
            match Map.tryFind i !transports with
            | None -> Set.empty
            | Some ds -> ds
        let multiples =
            match Map.tryFind i !splits with
            | None -> Set.empty
            | Some ds -> ds
        let expand split =
            split
            |> List.map (get_stable mono_fast)
            |> expand_choices
        if singles |> Set.isEmpty && multiples |> Set.isEmpty then finish ()
        else
            [ singles
              |> Set.toList
              |> List.map (get_root >> stabilisations)
              |> Set.unionMany
            ; multiples
              |> Set.toList
              |> List.map expand
              |> Set.unionMany
            ]
            |> Set.unionMany

    s |> get_root |> stabilisations // get_root is unnecessary here but feels safer

let stabilise_reaction mono_fast (r:'s Calculus.reaction) =
    let stable_products =
        r.products
        |> List.map (get_stable mono_fast)
    stable_products
    |> expand_choices
    |> Set.map (fun ps -> { r with products = ps |> List.map Seq.min})

let stabilise_many mono_fast = Set.toSeq >> Seq.map (stabilise_reaction mono_fast) >> Set.unionMany

let lift_mono mono_fast mono_slow (c:'s Calculus.t) s =
(*
    c.mono s
    |> stabilise_many mono_fast
*)
//(*
    s
    |> get_stable mono_fast
    |> Set.toSeq
    |> Seq.collect (collect_context (fun c cs -> c |> List.collect (mono_slow >> List.map (fun (r:'s Calculus.reaction) -> {r with reactants = [s]; products = r.products @ (cs |> List.map Seq.min)}))))
    |> Seq.map (stabilise_reaction mono_fast)
    |> Set.unionMany
//*)

let lift_bin mono_fast (c:'s Calculus.t) s1 s2 =
    //let ch1 = s1 |> get_stable mono_fast
    //let ch2 = s2 |> get_stable mono_fast
    c.bin s1 s2 // TODO: stabilise s1 and s2 first (unstable compounds do not have time to react)
    |> stabilise_many mono_fast

let lift_species mono_fast (c:'s Calculus.t) =
    c.initial_species
    |> List.map (get_stable mono_fast)
    |> List.collect (Set.toList >> (List.collect Seq.min))

let lift mono_fast mono_slow c =
    Calculus.from_mono_bin (lift_mono mono_fast mono_slow c) (lift_bin mono_fast c) (lift_species mono_fast c)
