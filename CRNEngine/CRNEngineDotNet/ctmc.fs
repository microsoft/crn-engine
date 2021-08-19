namespace Microsoft.Research.CRNEngine
open Oslo
open Oslo.SparseVector
open Oslo.SparseMatrix

[<JavaScript>] 
type Index<'s> when 's:comparison =
  { 
    to_species : System.Collections.Generic.List<'s>
    from_species : System.Collections.Generic.Dictionary<'s, int> 
  }
  static member empty () = { 
    to_species = new System.Collections.Generic.List<_>()
    from_species = new System.Collections.Generic.Dictionary<_,_>() 
  }
  static member add (i:Index<'s>) s =
    let count = i.to_species.Count
    i.to_species.Add s;
    i.from_species.Add(s,count);
    count
  static member index (i:Index<'s>) e =
    if i.from_species.ContainsKey e 
    then i.from_species.[e]
    else Index.add i e

(* CTMC algorithm *)
[<JavaScript>] 
type State = Map<int,int>  // Invariant is that species mapping to population 0 are not in the map

[<JavaScript>] 
type Transition = { target : State; propensity : Value }

[<JavaScript>] 
type Ctmc =
  { graph:Dictionary.t<State,Transition list>; initial_state:State }
  static member get_states (ctmc:Ctmc) = ctmc.graph |> Dictionary.keys
  static member num_states (ctmc:Ctmc) = ctmc.graph |> Dictionary.count
  static member num_transitions (ctmc:Ctmc) = ctmc.graph |> Dictionary.toSeq |> Seq.fold (fun acc (_,vs) -> acc + vs.Length) 0
  static member nonzero_prop (_: Reaction<int,Value,Expression.t<Key<int>>>, prop:Value) = 
    (Expression.simplify prop) <> Expression.Float 0.0
  static member get_product_species (reaction: Reaction<'s,Value,Expression.t<Key<'s>>> ) = reaction.products |> Mset.elements
  static member get_all_product_species rs = rs |> List.collect Ctmc.get_product_species
  static member calculate_reaction_prop scale (st:State) (r:Reaction<int,Value,Expression.t<Key<int>>> ) (ratesEnv:Hashtable.t<string, Expression.t<Key<int>>>) =
    let getpop (a:int) =
      match Map.tryFind a st with
      | None -> 0.0
      | Some p -> p |> float // TODO : should counts be floats?
    let get_unscaled_pop a = (getpop a) / scale
    match r.rate with
      | Rate.MassAction rate -> 
        (match Mset.to_mlist r.reactants with
          | [] -> rate
          | [{multiplicity=1;element=a}] -> Expression.Times [rate; Expression.Float (getpop a)]
          | [{multiplicity=2;element=a}] -> 
             (* Special case - two members of the same species *)
             let pop = Expression.Float (getpop a)
             Expression.Times [rate; pop; Expression.Minus{sub1=pop;sub2=Expression.Float 1.0} ; Expression.Float 0.5]
          | [{multiplicity=3;element=a}] -> 
             let pop = Expression.Float (getpop a)
             Expression.Divide{div1=Expression.Times[rate; pop; Expression.Minus{sub1=pop;sub2=Expression.Float 1.0}; Expression.Minus{sub1=pop;sub2=Expression.Float 2.0}]; div2=Expression.Float 6.0}
          | [{multiplicity=1;element=a};{multiplicity=1;element=b}] ->
             (* Simple case - two different species *)
             Expression.Times[rate; Expression.Float (getpop a); Expression.Float (getpop b)]
          (* Use binomial coefficients for the general case! *)
          | aa -> aa |> Lib.fold_left (fun acc entry -> Expression.Times [acc; (Expression.Float ((float) (Lib.binom ((int32) (getpop entry.element)) entry.multiplicity)))] ) rate )
       | Rate.Function (e) -> 
           let rec f (k:Key<int>) =
             match k with
             | Key.Parameter p -> Expression.Key p
             | Key.Species   s -> Expression.Float (getpop s)
             | Key.Time        -> failwith "Unexpected [time] in CTMC rate"
             | Key.Rate      r -> ratesEnv.[r] |> Expression.expand f
           Expression.expand f e
  (* Function to create the transition matrix of the state space (using the SparseMatrix class), a Matrix encoding of the state-space and a Vector of the initial state *)
  member ss.statespace_for_CME_integration env num_species : SparseMatrix * Matrix * Vector =
    let num_states = Dictionary.count ss.graph 
   // let num_species = 1 + (ss.graph |> Dictionary.keys |> Seq.collect (Map.toSeq >> Seq.map fst) |> Seq.max) in
    let S = Matrix.zeros num_states num_species 
    let mutable P0 = Vector.zeros(num_states)   
    let state_map = ss.graph |> Dictionary.keys |> Seq.mapi (fun i k -> k, i) |> Map.ofSeq 
    let initial_index = Map.find ss.initial_state state_map 
    let _ = P0.[initial_index] <- 1.0 
    // Species with zero population are not in the state map
    let read_state (s:State) i =
      match Map.tryFind i s with
      | Some p -> p
      | None -> 0 
    let A = new SparseMatrix(num_states, num_states) 
    // Can we use SparseMatrix(num_states, num_states, items, indices, count) instead?
    ss.graph
    |> Dictionary.toSeq
    |> Seq.iteri
         (fun i (state,transitions) ->
           let ordered_items =
             transitions
             |> Seq.map (fun tr -> state_map.[tr.target], tr.propensity |> Expression.eval (Environment.find env))
             |> Seq.sortBy fst 
           let indices = ordered_items |> Seq.map fst |> Seq.distinct |> Seq.toArray 
           let count = indices.Length 
           #if JavaScript // FP: workaround for Array.zeroCreate not working properly in W#; remove this when we move to W# 4.
           let items = Array.init count (fun _ -> 0.0)
           #else
           let items = Array.zeroCreate count 
           #endif
           ordered_items
           |> Seq.iter
                (fun (index,item) ->
                  let pos = Array.findIndex ((=) index) indices 
                  items.[pos] <- items.[pos] + item
                );
           let gain_i = Array.sum items 
           let sv =
              if gain_i = 0.0 
              then SparseVector(items,indices,count)
              else // add diagonal element
                  SparseVector(Array.append [|-gain_i|] items, Array.append [|i|] indices, 1 + count)
           A.Item(i) <- sv;
           for j in 0 .. num_species-1 do
             S.Item(i,j) <- read_state state j |> double
           done
         );
    (A,S,P0)
  (* Function to create the transition matrix of the state space (using the SparseMatrix class), a Matrix encoding of the state-space and a Vector of the initial state *)
  member ss.statespace_for_CME_sundials env num_species : (float[]*int[]*int[]*float[]) * Matrix * float[] =
    let num_states = Dictionary.count ss.graph 
    let S = Matrix.zeros num_states num_species 
    let state_map = ss.graph |> Dictionary.keys |> Seq.mapi (fun i k -> k, i) |> Map.ofSeq 
    let initial_index = Map.find ss.initial_state state_map 
    let P0 = Array.init num_states (fun i -> if i = initial_index then 1.0 else 0.0 )
    
    // Species with zero population are not in the state map
    let read_state (s:State) i =
      match Map.tryFind i s with
      | Some p -> p
      | None -> 0 

    let tar, src, prop = 
      ss.graph
      |> Dictionary.toSeq
      |> Seq.mapi
         (fun i (state,transitions) ->
           let ordered_items =
             transitions
             |> Seq.map (fun tr -> state_map.[tr.target], tr.propensity |> Expression.eval (Environment.find env))
             |> Seq.sortBy fst 
             |> Array.ofSeq
           let targets, propensities = Array.unzip ordered_items
           let sources = Array.create (Array.length targets) i

           // Need to update the stoichiometry
           for j in 0 .. num_species-1 do
             S.Item(i,j) <- read_state state j |> double
           done
           //let S_row = Array.init num_species (read_state state)

           targets, sources, propensities
         )
       |> Array.ofSeq
       |> Array.unzip3    
    let diagonals = prop |> Array.map Array.sum in 
    let cpp_sparse_vector = Array.concat prop, Array.concat tar, Array.concat src, diagonals in 
    cpp_sparse_vector, S, P0    

type ctmc_result<'s> = {
  ctmc:Ctmc;
  to_species:System.Collections.Generic.List<'s>;
 }

  (*
    let fake_env (a:string) = 0.001 in //TODO: simplify the expression and see if it matches Float 0.0
    match (Expression.eval fake_env prop) with 0.0 -> false | _ -> true
  *)