// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module RulesDSD.ProcessEquality

open RulesDSD.Syntax
open RulesDSD.Substitution
open RulesDSD.Unification

// TODO: turn into unifyHole
let rec patternMatches cle (strand : Hole<'s>) (pattern : Hole<'s>) sol : Substitution<'s, 'a> list =
  match strand,pattern with
  | _, [] -> [sol]  // match found
  | [], _ -> []      // the empty strand never matches any pattern
  
  | h1::hs1, h2::hs2 ->
    let h1' = sol.Apply(h1, cle)
    let h2' = sol.Apply(h2, cle)
    (h1', h2')  
    |> unifyHoleEntry cle 
    |> List.collect (fun theta -> 
        let sol' = Substitution<'s, 'a>.Compose sol theta cle
        patternMatches cle hs1 hs2 sol')

// find all pattern matches in a strand. acc is the set of matches found so far, pos is the pattern's index in the strand currently being matched
let rec fam cle (strand : Hole<'s>) (pattern : Hole<'s>) acc pos =
  match strand, pattern with
  | [], _  -> acc
  | _, []  -> acc
  | _::rest, _   -> 
    if strand.Length < pattern.Length
      then acc 
      else 
        let acc' = 
          patternMatches cle strand pattern Substitution.id
          |> List.map (fun x -> x, pos)
          |> Set.ofList 
          |> Set.union acc
                           
        fam cle rest pattern acc' (pos + 1)

let findAllMatches cle (strand : Hole<'s>) (sites : Hole<'s>) =
  fam cle strand sites Set.empty 0

type ProcessEquality = PE 
  with
  static member FindContexts(Process.Proc sys, patterns: Pattern<'s> list, cle) =
    let contexts = 
      patterns |> List.fold
       (fun acc pattern -> 
        match acc with
        | None                 -> None // skip remaining patterns if a previous pattern failed to match
        | Some (previousContexts: Set<Substitution<'s, 'a> * Pattern<'s> list>) -> 
        let dummyHole = []
        let matches = 
          Map.fold
           (fun acc (strandID:StrandID) (_strand:Strand<'s>) -> 
             let strand = _strand
                          |> List.mapi (fun i x -> x, Location.Loc (strandID, i))
             //let _proc = Term.Proc (Process.ofList [_strand])
             // TODO: optimization: apply substitutions to filter out bad matches early
             let addPositions pos (sites:Hole<'s>) : Hole<'s> =
              sites
                |> List.mapi (fun i s -> (fst s, Location.Loc (strandID, pos+i)))
             match pattern with
             //| Hole h     -> Set.add (Substitution.id, h) acc
             | Nihil  -> Set.add (Substitution.id, Pattern.Nihil) acc
             | Pattern.Inner sites -> 
                let matches = findAllMatches cle strand sites
                if matches.IsEmpty
                  then acc
                  else 
                    let newMatches = 
                      matches 
                      |> Set.map (fun (sub, pos) -> 
                         let hole = Pattern.Inner (addPositions pos sites)
                         (sub, hole))
                    Set.union acc newMatches
             
             | Pattern.Strand sites -> 
                patternMatches cle strand sites Substitution<'s, 'a>.id
                |> List.fold (fun acc sub -> 
                              if sites.Length = strand.Length
                                then 
                                  let hole = Pattern.Strand (addPositions 0 sites)
                                  Set.add (sub, sub.Apply(hole, cle)) acc
                                else acc) acc
             
             | FivePrime  p -> 
                patternMatches cle strand p Substitution.id 
                |> List.fold (fun acc sub ->
                    let hole = Pattern.FivePrime (addPositions 0 p)
                    Set.add (sub, hole) acc) acc 
             
             | ThreePrime p -> 
                let offset = strand.Length - p.Length
                if offset < 0
                  then acc
                  else 
                    let cutStrand = List.skip offset strand
                    patternMatches cle cutStrand p Substitution.id
                    |> List.fold  (fun acc sub -> 
                        let hole = Pattern.ThreePrime (addPositions offset p)
                        Set.add (sub, hole) acc
                    ) acc
             
             | Nicking (threePrime, fivePrime) -> 
                let offset = strand.Length - threePrime.Length
                let threePrimeMatch = 
                  if offset < 0
                    then []
                    else 
                      let cutStrand = List.skip offset strand
                      patternMatches cle cutStrand threePrime Substitution.id
                let fivePrimeMatch = patternMatches cle strand fivePrime Substitution.id
                
                (* create two distinct pattern matches for Nicking, one with the matched 3' end and a dummy range, and one with the dummy range and a 5' matched range. This avoid creating a pattern match on two ends of the same strand (it could create a circular pattern). 
                 After finding all such patterns in a system, dummy ranges will be replaced by 3' or 5' ranges from other strands. This second step is performed outside the fold loops.*)
                let tpHole = addPositions offset threePrime
                let fpHole = addPositions 0 fivePrime
                let tpPattern = Pattern.Nicking (tpHole, dummyHole)
                let fpPattern = Pattern.Nicking (dummyHole, fpHole)
                match threePrimeMatch, fivePrimeMatch with
                | [],  []   -> acc
                | tps, []   -> tps |> List.map (fun tp -> tp, tpPattern) |> Set.ofList |> Set.union acc
                | [],  fps  -> fps |> List.map (fun fp -> fp, fpPattern) |> Set.ofList |> Set.union acc
                | tps, fps  -> let x = tps |> List.map (fun tp -> tp, tpPattern) |> Set.ofList 
                               let y = fps |> List.map (fun fp -> fp, fpPattern) |> Set.ofList
                               Set.union (Set.union acc x) y
           ) Set.empty sys
           |> Set.map (fun (s, h) -> s, s.Apply(h, cle))
        
        // find all valid combinations of nicking patterns and remove all dummy ranges
        let newMatches =
          match pattern with
          | Nicking _ ->
            matches |> Set.fold
              (fun acc nick1 -> 
                Set.remove nick1 matches
                |> Set.fold (fun acc nick2 -> 
                  match nick1, nick2 with
                  | (s1, Nicking (tp1, fp1)), (s2, Nicking (tp2, fp2)) -> 
                    let tp1IsDummy = (tp1 = dummyHole)
                    let tp2IsDummy = (tp2 = dummyHole)
                    // skip if the non dummy ranges are both 3' or 5'
                    if (tp1IsDummy && tp2IsDummy)
                       || (not tp1IsDummy && not tp2IsDummy)
                      then acc
                      else 
                        // skip non-mergeable matches
                        match Substitution.TryMerge s1 s2 cle with
                        | None     -> acc
                        | Some sub -> 
                          let sid (x:Hole<'s>) = match snd x.[0] with 
                                                 | Location.Loc (i, _) -> i
                                                 | _ -> failwith "Unexpected error in context finding"
                          let tp, fp = if tp1IsDummy
                                        then tp2, fp1
                                        else tp1, fp2
                          // skip pattern matches on the same strand
                          if sid tp = sid fp
                            then acc
                            else Set.add (sub, Pattern.Nicking (tp, fp)) acc
                  
                  // this should not happen
                  | _,_ -> failwith "Unexpected patterns in nicking pattern matching"
                  ) acc
              ) Set.empty 
          | _ -> matches
        if newMatches.IsEmpty
          then None
          else 
            let currentContexts = 
              if previousContexts.IsEmpty
                then // initialize the context by placing each hole in a different hole list
                  matches |> Set.map (fun (x, y) -> (x, [y]))
                else
                  (* for each matched contex hole, add it to the previous matches only if they don't overlap *)
                  previousContexts
                  |> Set.fold (fun acc (prevSub, prevHoles) -> 
                    newMatches
                    |> Set.fold (fun acc (newSub, newHole) -> 
                        if prevHoles |> List.forall (newHole.Overlaps >> not)
                          then 
                            match Substitution.TryMerge prevSub newSub cle with
                            | Some mergedSub -> Set.add (mergedSub, prevHoles @ [newHole]) acc
                            | None -> acc
                          else acc
                        ) acc
                    ) Set.empty
            if currentContexts.IsEmpty
              then None
              else Some currentContexts
      ) (Some Set.empty) 
    
    // create a new goal for each matched context
    match contexts with
    | None -> None
    | Some cs ->
      cs
      |> Set.toList
      |> List.map (fun (theta, holes) -> 
          let holes'  = holes |> List.map (fun x -> theta.Apply(x, cle) |> Pat) |> TList
          let p       = Process.Proc sys
          let context = Func ("_ctx", [Term.Proc p; holes'])
          (theta, context))
      |> Some

  static member Flatten (cle:CLE<'s,'a>) (t:Term<'s>) =
    let rec flatten (t:Term<'s>) : Process<'s> option =
      match t with 
      | Term.Proc p   -> Some p
      | Term.TList ps
      | Func("|", ps) -> ps 
                         |>  List.fold (fun acc p ->
                                match acc, flatten p with 
                                | Some p1, Some p2 -> Some (cle.ComposeProcesses p1 p2)
                                | _, _             -> None) (Process.OfList [] |> Some)
      | Func("_ctx", [Func ("_ctx", [Proc (Process.Proc ctxSys); TList ctxHoles]); TList inputHoles])
      | Func("_ctx", [Proc (Process.Proc ctxSys); TList ctxHoles; TList inputHoles]) -> 
        let newHoles = 
          inputHoles
          |> List.map (fun pattern -> 
            match pattern with 
            | Pat p -> p
            | _ -> failwith "Unexpected term in context patterns.")

        let newSys = 
          newHoles 
          |> List.zip ctxHoles
          |> List.fold
            (fun (acc:Map<StrandID,Strand<'s>>) (ctxHole, newHole) ->

              let replaceAt oldStrand (startIdx, endIdx) newSites =
                let start  = List.take startIdx oldStrand
                let ending =  List.skip endIdx oldStrand
                start @ newSites @ ending

              let freshStrandID (p:Map<StrandID, Strand<'s>>) =
                let max = Map.toList p 
                            |> List.map fst 
                            |> List.max
                max + 1
              let getStrandID (x:Hole<'s>)    = match snd x.Head with 
                                                | Location.Loc (i, _) -> i
                                                | _ -> failwith "Unexpected error in context finding"
              let getStrandStart (x:Hole<'s>) = match snd x.Head with 
                                                | Location.Loc (_, i) -> i
                                                | _ -> failwith "Unexpected error in context finding"

              match ctxHole with
              (* whole strand substitutions *)
              | Term.Var _ -> failwith <| sprintf "Insufficiently instantiated variable \"%s\"" (Term.ToString ctxHole)
              | Pat Pattern.Nihil -> 
                match newHole with
                | Pattern.Nihil        -> acc
                | Pattern.Strand sites -> acc.Add(freshStrandID acc, List.map fst sites)
                | _ -> failwith "Mismatching context hole substitution."
            
              | Pat (Pattern.Strand sites) ->
                let strandID = getStrandID sites
                match newHole with
                | Pattern.Nihil        -> 
                  acc.Remove strandID
                | Pattern.Strand sites -> 
                  acc.Add(strandID, List.map fst sites)
                | _ -> failwith "Mismatching context hole substitution."

              (* nick and inner sequences substitutions *)
              | Pat (Pattern.Inner sites) -> 
                let strandID = getStrandID sites
                let threePrimeStart = getStrandStart sites
                let fivePrimeEnd    = threePrimeStart + sites.Length

                match newHole with
                // replace an inner sequence
                | Pattern.Inner newSites ->
                  let oldStrand = acc.[strandID]
                  let innerRange = (threePrimeStart, fivePrimeEnd)
                  let newStrand = replaceAt oldStrand innerRange (List.map fst newSites)
                  acc.Add(strandID, newStrand)

                // introduce a new nick
                | Pattern.Nicking (newThreePrimeEnd, newFivePrimeStart) ->
                  let oldStrand = acc.[strandID]
                  let newStrand1 = List.take threePrimeStart oldStrand 
                                      @ List.map fst newThreePrimeEnd
                  let newStrand2 = List.map fst newFivePrimeStart
                                    @ List.skip fivePrimeEnd oldStrand 
                  let acc' = acc.Add(strandID, newStrand1)
                  acc'.Add(freshStrandID acc', newStrand2)
                    
                | _ -> failwith "Mismatching context hole substitution."

              | Pat (Pattern.Nicking (sites1, sites2)) -> 
                let strandID1 = getStrandID sites1
                let strandID2 = getStrandID sites2
                let tpStart   = getStrandStart sites1
                let fpEnd     = sites2.Length

                match newHole with
                // replace a nick
                | Pattern.Nicking (newThreePrimeEnd, newFivePrimeStart) ->
                  let oldStrand1 = acc.[strandID1]
                  let oldStrand2 = acc.[strandID2]
                  let newStrand1 = List.take tpStart oldStrand1 
                                    @ List.map fst newThreePrimeEnd
                  let newStrand2 = List.map fst newFivePrimeStart 
                                    @ List.take fpEnd oldStrand2
                  acc.Add(strandID1, newStrand1).Add(strandID2, newStrand2)

                // ligate a nick
                | Pattern.Inner newSites ->
                  let oldStrand1 = acc.[strandID1]
                  let oldStrand2 = acc.[strandID2]
                  let newStrand  = List.take tpStart oldStrand1 
                                    @ List.map fst newSites 
                                    @ List.take fpEnd oldStrand2
                  acc.Remove(strandID2).Add(strandID1, newStrand)
              
                | _ -> failwith "Mismatching context hole substitution."

              (* 3' end substitution *)
              | Pat (Pattern.ThreePrime sites) -> 
                let strandID = getStrandID sites
                let tpStart  = getStrandStart sites

                match newHole with
                | Pattern.ThreePrime newSites ->
                  let oldStrand = acc.[strandID]
                  let newStrand = List.take tpStart oldStrand @ List.map fst newSites
                  acc.Add(strandID, newStrand)
              
                | _ -> failwith "Mismatching context hole substitution."

              (* 5' end substitution *)
              | Pat (Pattern.FivePrime sites) -> 
                let strandID = getStrandID sites

                match newHole with
                | Pattern.FivePrime newSites ->
                  let oldStrand = acc.[strandID]
                  let fpEnd = sites.Length
                  let newStrand = List.map fst newSites @ List.skip fpEnd oldStrand
                  acc.Add(strandID, newStrand)
              
                | _ -> failwith "Mismatching context hole substitution."
              | _ -> failwith <| sprintf "Unexpected term \"%s\" in process \"%s\"" (Term.ToString ctxHole) (Process.ToString (Process.Proc ctxSys))
            ) ctxSys
        (* CS: the commented out code below is a well-formedness restriction, that has been lifted up for the time being. 
               By enforcing that dangling bonds are not deleted in a context, and that all other bonds are either 
               deleted or added in pairs, result of applying holes to a context is always well-formed.
               Unfortunately this doesn't let programmers write code that splits complexes and adds bonds to them
               separately. For example:
                  reaction([P1 ; P2], ...) :-
                    P1 = C1 [D], P2 = C2 [D'],
                    compl(D, D'),
                    Q1 = C1 [D!i], Q2 = C2[D'!i]   <----- two dangling bonds
               Maybe the safety check can be implemented in a separate type system for the rules.
               *)
        //let newBondsMap = getBondsMap Pattern.Fold newHoles
        //if newBondsMap.[0].Length > 0
        //  then None
        //  else 
        //    // Dangling pointers check
        //    let rec termFold (f:(Map<Bond,int> -> (SiteT* _) -> Map<Bond,int>)) 
        //                     (acc:Map<Bond,int>) 
        //                     (t:Term) : Map<Bond,int> = 
        //      let g x y = f x (fst y)
        //      match t with
        //      //| Term.Hole h       -> Hole.Fold g acc h
        //      | Term.Pat p        -> Pattern.Fold f acc p
        //      | Term.Var _        -> acc
        //      // these should not happen
        //      | Term.TList ts     -> List.fold (termFold f) acc ts
        //      | Term.TCons (t,t') -> List.fold (termFold f) acc [t;t']
        //      | Term.Float _        -> acc
        //      | Term.Const _      -> acc
        //      | Term.Func _       -> acc // List.fold (termFold f) acc ts
        //      | Term.Proc _       -> acc
              //| Term.Ctx _        -> acc

            //let oldBondsMap = getBondsMap termFold ctxHoles
            //let oldDanglingBonds = oldBondsMap.[1]
            //let newDanglingBonds = newBondsMap.[1]
            //if oldDanglingBonds <> newDanglingBonds
            //  then None 
            //  else 
            //    // TODO: bond variables
            //    Some (Process.Proc newSys) // |> Process.Canonical)
        Some (Process.Proc newSys)
      | _ -> None
    flatten t