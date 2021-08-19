namespace Microsoft.Research.CRNEngine
[<JavaScript>] 
type Calculus<'s> when 's : comparison = 
  { react : 's list -> 's -> Reaction<'s,Value,Expression.t<Key<'s>>> list }
  member c.adapt =
    let ix : Index<'s> = Index<'s>.empty () in
    let react_st is i =
      // convert indices to species
      let ss = is |> List.map (fun i -> ix.to_species.[i]) in
      let s = ix.to_species.[i] in
      // call calculus
      let rs = c.react ss s in
      // convert products to indices (creating new mappings for new species)
      let ps = rs |> Ctmc.get_all_product_species in                 
      let ips = ps |> List.map (Index.index ix) in
      let map_s = Index.index ix in
      rs |> List.map (Reaction.map map_s id (Expression.map (Key.map map_s))) in
    { init     = List.map (Index.add ix)
    ; react_st = react_st 
    ; index    = ix }
  member calculus.initialise_ctmc species =
    // Setup initial list of work items
    let counts = species |> List.map snd
    let adapter = calculus.adapt 
    let indices = species |> List.map fst |> adapter.init
    let initial_state = 
      List.zip indices counts
      |> List.filter (snd >> (<) 0) // Establish invariant
      |> Map.ofList    
    let applicable_reactions,_ = 
      indices
      |> List.fold
            (fun (current_reactions,sp) s -> 
               let new_reactions = adapter.react_st sp s
               ( current_reactions@new_reactions |> List.distinct 
               , s::sp@(new_reactions |> Ctmc.get_all_product_species) |> List.distinct )
            )
            ([],[])
    initial_state, applicable_reactions, adapter
  member calculus.to_ctmc ratesEnv scale initial_state applicable_reactions adapter =
    let mutable graph:Dictionary.t<State,Transition list> = Dictionary.empty()
    let mutable memoized_states:Dictionary.t<State,State> = Dictionary.empty() // Musn't make duplicates or consumes excessive memory
    Dictionary.add graph initial_state []
    Dictionary.add memoized_states initial_state initial_state
    let adaptedRates = ratesEnv |> Hashtable.map (Expression.map (Key.map (fun x -> adapter.index.from_species.[x])))
    let nonzero_reactions =
      applicable_reactions
      |> List.map (fun r -> r, Ctmc.calculate_reaction_prop scale initial_state r adaptedRates)
      |> List.filter Ctmc.nonzero_prop
    let ws =
      nonzero_reactions
      |> List.map (fun (r,p) -> initial_state, (r,p), applicable_reactions)
    let apply (reaction: Reaction<int,Value,Expression.t<Key<int>>>) (st:State) =
      let to_sub = reaction.reactants |> Mset.to_map
      let to_add = reaction.products |> Mset.to_map
      let sub (state:State) key value =
          let start = Map.find key state
          match start - value with
          | 0 -> Map.remove key state
          | pop -> Map.add key pop state
      let add (state:State) key value =
          let start = Map.tryFind key state
          match start with
          | Some pop -> Map.add key (pop + value) state
          | None ->  Map.add key value state
      st
      |> (fun st -> Map.fold sub st to_sub)
      |> (fun st -> Map.fold add st to_add)
    let not_in_state (st:State) s =
      match Map.tryFind s st with
      | None -> true
      | Some n -> n > 0
    let add_link st new_st prop =  
      let was_known = Dictionary.containsKey memoized_states new_st
      let stored_new_st =
          if was_known then
              Dictionary.find memoized_states new_st
          else
              (Dictionary.add graph new_st [];
              Dictionary.add memoized_states new_st new_st;
              new_st)
      let new_transition = {target = stored_new_st; propensity = prop }
      let existing = Dictionary.find graph st
      Dictionary.add graph st (new_transition::existing)
      was_known
    //To get tail call elimination in WebSharper 3.x you need to make the recursive call last in a function hence the outerexplore
    let outerexplore() =
        // Handle work list
        (*
          Each discovered transition results in a work item on the work list
          A work item has three components:
           - source state of the transition
           - reaction making the transition, with propensity
           - list of other reactions applicaple to the source state (all these will be known and passed on to the target state)
          Handling a work item means:
           - compute the target state
           - add transition to graph (with memoization)
           - compute list of reactions applicaple to target state
             These are the ones applicaple to the source state plus the ones arising from new species
           - add work items for each reaction applicaple to target state
        *)
        let rec explore =
          function
          | [] -> ()
          | (st, (reaction,p), current_applicable_reactions)::workingset ->
            let new_state = apply reaction st //What if reaction arrives at existing state?
            let new_state_was_known = add_link st new_state p
            if new_state_was_known 
            then explore workingset
            else
              let species_to_expand = reaction |> Ctmc.get_product_species |> List.filter (not_in_state st) |> Seq.distinct |> List.ofSeq in
              let known_species = st |> Map.toSeq |> Seq.map fst |> List.ofSeq //Is regenerating known species better than carrying?
              let new_applicable_reactions,_ =
                species_to_expand
                |> List.fold
                      (fun (new_reactions_acc, species_acc) s -> 
                          let new_reactions = adapter.react_st species_acc s
                          ( new_reactions_acc@new_reactions
                          , s::species_acc@(new_reactions |> Ctmc.get_all_product_species) |> Seq.distinct |> List.ofSeq)
                      )
                      ([], known_species)
              let applicable_reactions =
                new_applicable_reactions@current_applicable_reactions
                |> Seq.distinct |> List.ofSeq // This should be less time consuming than dealing with the duplicates (but a clever convention might get rid of this)
              let nonzero_reactions =
                applicable_reactions
                |> List.map (fun r -> r, Ctmc.calculate_reaction_prop scale new_state r adaptedRates)
                |> List.filter Ctmc.nonzero_prop
              let new_workingset =
                nonzero_reactions
                |> List.map (fun (r,p) -> new_state, (r,p), applicable_reactions)
              explore (workingset@new_workingset)     
        explore ws  
    outerexplore()
    {ctmc={graph=graph; initial_state=initial_state}; to_species=adapter.index.to_species}