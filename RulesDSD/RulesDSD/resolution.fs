// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module RulesDSD.Resolution

open RulesDSD.Syntax
open RulesDSD.Substitution
open RulesDSD.Unification
open RulesDSD.ProcessEquality

open System.Diagnostics
#if JavaScript
#else
open QuickGraph
#endif

type Mode = SingleAnswer | AllAnswers

type TJVertex = {n : int; index : int; lowlink : int; onStack:bool}

// Tarjan's strongly connected components algorithm, from https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
let tarjan (vertices : TJVertex seq) (edges: (int * int) list) = 
  let index = ref 0
  let S = ref []
  let result = ref []
  let state : TJVertex [] = Array.ofSeq vertices

  let rec strongconnect (v:int) =
    // Set the depth index for v to the smallest unused index
    let updatedNode = {n = state.[v].n; index = !index; lowlink = !index; onStack = true}
    Array.set state v updatedNode

    index := !index + 1
    S := v :: !S
      
    // Consider successors of v
    for (v, w) in edges |> List.filter (fun x -> fst x = v) do
      if state.[w].index = -1
        then
          // Successor w has not yet been visited; recurse on it
          strongconnect(w)
          Array.set state v { state.[v] with lowlink = min state.[v].lowlink state.[w].lowlink }
        elif state.[w].onStack 
          // Successor w is in stack S and hence in the current SCC
          // If w is not on stack, then (v, w) is an edge pointing to an SCC already found and must be ignored
          // Note: The next line may look odd - but is correct.
          // It says w.index not w.lowlink; that is deliberate and from the original paper
          then Array.set state v { state.[v] with lowlink = min state.[v].lowlink  state.[w].index }

      
    // If v is a root node, pop the stack and generate an SCC
    if state.[v].lowlink = state.[v].index then
      // start a new strongly connected component
      let x = ref []
      let c = ref -1
      while !c = -1 || state.[!c] <> state.[v] do
          let sHead, sTail = 
            match !S with 
            | x::y -> x,y 
            | [] -> failwith "Unexpected error in SCC computation."
          S := sTail
          Array.set state sHead { state.[sHead] with onStack = false }
          // add w to current strongly connected component
          c := sHead
          x := !c :: !x
      // output the current strongly connected component
      result := !x :: !result
  
  // traverse graph
  let n = Seq.length vertices
  for v in [0..n-1] do
    if (state.[v]).index = -1
      then strongconnect(v)
  
  // return result
  !result
   
[<DebuggerDisplay("{this.String()}")>]
type Goal<'s, 'a when 's : equality and 'a :equality> = 
  { goals        : Literal<'s> list
    rule         : Clause<'s>
    solution     : Substitution<'s, 'a> }
  with 
  static member Create (s, g, r) = { goals = g; rule = r; solution = s} 
  static member Create (g, r)    = { goals = g; rule = r; solution = Substitution<'s, 'a>.id } 
  static member Create (g, cle)  = 
    let sol = 
      g 
      |> List.map (fvl cle)
      |> Set.unionMany
      |> Set.fold (fun (acc:Substitution<'s, 'a>) v -> 
                    match v with 
                    | Variable.TVar x     -> acc.Add(v, Term.Var x, cle)
                    | Variable.SVar (x,s) -> acc.Add(x, Choice3Of4 s, cle)
                    | Variable.IVar (x,a) -> acc.Add(x, Choice4Of4 a, cle)
                    | Variable.LVar x     -> acc.Add(v, Location.Var x, cle)
                  ) Substitution<'s, 'a>.id
    { goals = g; rule = Clause.Create(Pred("", [])); solution = sol } 
  static member OK : Literal<'s> list = []
  static member ToString(this:Goal<'s, 'a>, cle) = 
    sprintf "{%s} {%s}" (this.goals |> List.map (fun x -> x.String) |> String.concat ", " ) (this.solution.String cle)
  member this.String() = Goal.ToString(this, CLE.empty)

(************************)
(************************)
(************************)

let optionMap2 f x y =
    match x, y with
    | (Some x', Some y') -> Some (f x' y')
    | (None, _)          -> None
    | (_, None)          -> None

let rec eval (cle:CLE<'s,_> ) (t:Term<'s>) = 
  match t with 
  | Term.Var _   -> None 
  | Term.Float i -> Some i
  | Term.Const c -> None 
  // TODO: print context
  | Term.Func (op, ts) -> 
    if Term.IsExpression t
    then 
      let e = Term.ToExpression t
      let err _ = let msg = e |> Microsoft.Research.CRNEngine.Expression.to_string (Term.ToStringWith cle)
                  failwithf "Cannot evaluate expression %s" msg
      e |> Microsoft.Research.CRNEngine.Expression.eval err
        |> Some
    else  
      match op with
      | "length" -> match ts with 
                    | [TList ts] -> Some (float ts.Length)
                    | _          -> None
      | _ ->
        let ts' = ts |> List.map (eval cle)
        let ts'' = ts' |> List.fold (optionMap2 (fun x y -> x@[y])) (Some [])
        match ts'' with
        | None    -> None
        | Some ns ->
            match op with 
            | "+" -> Some <| List.sum ns
            | "*" -> Some <| List.fold (*) 1.0 ns
            | "-" -> match ns with 
                      | [a; b] -> Some (a - b)
                      | _ -> None
            |_    -> None
  | Term.TList _   -> None
  | Term.TCons _   -> None 
  | Term.Pat _     -> None 
  | Term.Proc  _   -> None 
  | Term.TMSet _   -> None 
  | Term.TCRN  _   -> None 
                          
let applyP cle (theta:Substitution<'s, 'a>) (Pred (psym, pargs)) =
  Pred(psym, pargs |> List.map (fun x -> theta.Apply(x, cle)))

let applyL cle (theta:Substitution<'s, 'a>) lit =
  match lit with
  | Pos p -> Pos (applyP cle theta p)
  | Neg p -> Neg (applyP cle theta p)

let applyG cle (theta:Substitution<'s, 'a>) goal =
  match goal with
  | Pos p -> Pos (applyP cle theta p)
  | Neg p -> Neg (applyP cle theta p)

let freshID counter = 
  let n = !counter
  counter := n + 1 
  n

let freshName counter = 
  let n = !counter
  counter := n + 1 
  n

(****************************************************)
(****************************************************)
(* Core LSDNF resolution step: unify a goal 
   predicate with some predicate from the program   *)
(****************************************************)
(****************************************************)
let rec resolveGoal (program:RulesProgram<'s>) (theta:Substitution<'s, 'a>) (cle:CLE<'s,'a>) varCounter g (*(debugMap:Map<string,System.TimeSpan> ref)*) : (Substitution<'s, 'a> *Substitution<'s, 'a> * Literal<'s> list * Clause<'s>) list option = 
 let extendSolutions cle (theta:Substitution<'s,'a>) debug solutions = 
   match solutions with
   | [] -> None
   | sols -> sols |> List.fold (fun acc sol -> 
                         let theta' = Substitution<'s, 'a>.Update theta sol cle
                         (sol, theta', Goal<'s, 'a>.OK, debug) :: acc
                     ) []
                     |> Some
 // let t = System.DateTime.Now // uncomment this line, debugMap and related code below to enable performance profiling
 let r = 
   match cle.resolveGoal (match g with Pred (a,b) -> a,b) (match theta with Sub t -> t) with 
   | Some sols -> Some (sols |> List.map (fun (a,b) -> Sub a, Sub b, Goal<'s,'a>.OK, Clause.Create(Pred("custom", []))))
   | None ->
    match g with 
    (*****************)
    (* Context setup *)
    (*****************)
    | Pred ("is", [Term.Var (x,y); Func("|", procs)]) 
      -> let flatProcs = procs 
                         |> List.choose (ProcessEquality.Flatten cle (*false*))
         if flatProcs.Length <> procs.Length
          then None
          else let flatProc = flatProcs |> List.fold (cle.ComposeProcesses (*false*)) (Process.OfList [])
               let sol     = Substitution<'s, 'a>.Create ((x,y), Term.Proc flatProc)
               let theta'  = Substitution<'s, 'a>.Update theta sol cle
               Some [sol, theta', Goal<'s, 'a>.OK, Clause.Create( Pred("is",[]))]
    // holes must be instantiated in order to find a context
    | Pred ("is", [p; ctx; TCons (x,y)]) -> 
      match TCons (x, y) |> Term.FlattenList with
      | Choice1Of2 flattenedHoles -> let g' = Pred ("is", [p; ctx; TList flattenedHoles])
                                     resolveGoal program theta cle varCounter g' (*debugMap*)
      | _ -> None 
    | Pred ("is", [Term.TList ps; ctx; ts]) ->
        match Term.TList ps |> ProcessEquality.Flatten cle (*false*) with
        | Some p -> let g' = Pred ("is", [Term.Proc p; ctx; ts])
                    resolveGoal program theta cle varCounter g' (*debugMap*)
        | None -> None 
   
    (*******************)
    (* Context finding *)
    (*******************)
    | Pred ("is", [Term.Proc p; Term.Var (ctx,y); ts]) -> 
      match ts |> Term.FlattenList with
      | Choice2Of2 _ -> None
      | Choice1Of2 patternTerms  ->
        let patterns = 
          patternTerms 
          |> List.choose (fun t -> match t with 
                                   | Pat p -> Some p
                                   | _     -> None)
        if patterns.Length <> patternTerms.Length 
          then None // i.e. resolution fails because one of the holes is not a pattern but some other term                                
          else
            match ProcessEquality.FindContexts(p, patterns, cle) with
            | None -> None
            | Some cs ->
              cs
              |> List.distinct
              |> List.map (fun (sub, context) -> 
                  let ctxSub  = Substitution<'s, 'a>.Create ((ctx,y), context)
                  let sol     = Substitution<'s, 'a>.Compose sub ctxSub cle 
                  let theta'  = Substitution<'s, 'a>.Update theta sol cle 
                  (sol, theta', Goal<'s, 'a>.OK, Clause<'s>.Create( Pred("is ctx",[]))))
              |> Some
    
    (************************)
    (* Context substitution *)
    (************************)
    // Replace the holes in a context with the patterns
    | Pred ("is", [sys; Func("_ctx", [Term.Proc (Process.Proc ctxSys); ctxTerm]); ts]) ->
      match ts |> Term.FlattenList with
      | Choice2Of2 _           -> None
      | Choice1Of2 inputHoles  ->
        match ctxTerm with
        | Term.Var (-1, "_") -> 
            Some [Substitution<'s, 'a>.id, theta, Goal<'s, 'a>.OK, Clause.Create( Pred("is ctx",[]))] 
        | Term.Var (x,y) -> // generate a variable for each input hole, in order to postpone holes unification
            let varHoles = inputHoles |> List.map (fun _ -> Var (freshID varCounter, "_X"))
            let sol    = Substitution<'s, 'a>.Create((x,y), TList varHoles) 
            let theta' = theta.Add(TVar (x,y), TList varHoles, cle)
            Some [sol, theta', Goal<'s, 'a>.OK, Clause.Create( Pred("is ctx",[]))] 
        (* // TODO: double check that this is context holes unification
        *)
        | TList ctxHoles ->
          match ProcessEquality.Flatten cle (Func("_ctx", [Proc (Process.Proc ctxSys); TList ctxHoles; TList inputHoles]) ) with
          | None   -> None
          | Some p ->
            match sys with
            | Term.Var (-1, "_") -> 
                let sol    = Substitution<'s, 'a>.id
                Some [sol, theta, Goal<'s, 'a>.OK, Clause.Create( Pred("is ctx",[]) ) ]
            | Term.Var (sysVar, y) -> 
                let sol    = Substitution<'s, 'a>.Create((sysVar, y), Term.Proc p) 
                let theta' : Substitution<'s, 'a> = theta.Add(TVar (sysVar, y), Term.Proc p, cle)
                Some [sol, theta', Goal<'s, 'a>.OK, Clause.Create( Pred("is ctx",[]) ) ]
            | Proc q -> 
              if cle.toCanonicalFormProcess p = cle.toCanonicalFormProcess q
                then Some [Substitution<'s, 'a>.id, theta, Goal<'s, 'a>.OK, Clause.Create( Pred("is ctx",[]) ) ]
                else None
            | _ -> None
        | _ -> None
    
    (**********************)
    (* Strand conversions *)
    (**********************)
    | Pred ("length", [t1; t2]) 
      -> match Term.FlattenList t1 with
         | Choice1Of2 ts -> unify cle [TEq (Float (float ts.Length), t2) ]
                            |> extendSolutions cle theta (Clause.Create(Pred("length",[])))
         | Choice2Of2 _ -> None
    (**********************)
    (* Graph operations *)
    (**********************)
    | Pred ("tscc", [t1; t2])
    | Pred ("scc", [t1; t2]) 
      -> match Term.FlattenList t1 with
         | Choice1Of2 ts1 -> 
            // t1 must be a list of pairs (node n, list of neighbours of n)
           let pairs : ((Process<'s> * Process<'s> list) list) option = 
             ts1 |> List.fold (fun acc x -> 
               match acc with 
               | Some accx -> 
                 match x with 
                 | TList [Term.Proc source; ts2] ->
                   match ts2 with 
                   | TList procs -> 
                     procs |> List.fold (fun accy y -> 
                               match accy with 
                               | None -> None 
                               | Some ps -> match y with 
                                            | Term.Proc py -> Some (py::ps)
                                            | _ -> None) (Some [])
                           |> fun tgs -> match tgs with 
                                         | Some targets -> Some ((source, targets) :: accx)
                                         | None -> None
                   | _ -> None
                 | _ -> None
               | None -> None
              ) (Some [])
           match pairs with 
           | None -> None 
           | Some edges ->
              // manual implementation of Tarjan's Strongly Connected Components algorithm, from https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
              #if JavaScript
              let e = 
                edges 
                |> List.map (fun (source, ts) -> 
                  ts 
                  |> List.map (fun target -> (source, target))) 
                  |> List.concat
              let procs = 
                e 
                |> List.collect (fun (x,y) -> [x;y])
                |> List.distinct
              let iProcs      = procs |> List.indexed
              let procToIndex = iProcs |> List.map (fun (x,y) -> y,x) |> Map.ofList
              let indexToProc = iProcs |> Map.ofList
              
              let prefs = iProcs |> List.map (fun (x, y) -> x, { n = procToIndex.[y]; index = -1; lowlink = -1; onStack = false}) 
              let indexToRef = prefs |> Map.ofList
              let edgess = e |> List.map (fun (x,y) -> procToIndex.[x], procToIndex.[y])
              let vertices = 
                edgess 
                |> List.collect (fun (x, y) -> [x; y])
                |> List.distinct
                |> List.map (fun i -> indexToRef.[i])

              let sccs    = tarjan vertices edgess
              let sccDict = 
                sccs 
                |> List.indexed 
                |> List.collect (fun (i, procIndexes) -> procIndexes |> List.map (fun x -> indexToProc.[x], i))
                |> Map.ofList
              #else
              // NON-JAVASCRIPT PATH
              // create QuickGraph digraph g
              let e = edges |> List.map (fun (source, ts) -> 
                                  ts |> List.map (fun target -> 
                                    new SEdge<Process<'s>>(source, target)
                                  )
                               ) 
                      |> List.concat
              let agraph = e.ToAdjacencyGraph()                
                        
              // compute strongly connected components of g
              let sccAlgo = QuickGraph.Algorithms.ConnectedComponents.StronglyConnectedComponentsAlgorithm agraph 
              sccAlgo.Compute()
              
    
              // convert sccs into Terms
              let sccDict = sccAlgo.Components
              let procs = sccDict.Keys
              #endif
              
              let toTerm = Seq.map (fun (_, cc) -> TList (cc |> Seq.map Term.Proc |> Seq.toList))
                           >> Seq.toList 
                           >> TList
              let scc     = procs |> Seq.groupBy (fun p -> sccDict.[p])
              if g.Name = "tscc"
                then 
                  let componentsMap = 
                    edges 
                    |> List.map (fun (x, ys) -> x, ys |> List.map (fun y -> sccDict.[y]) |> List.distinct)
                    |> Map.ofList
                  let tscc = 
                    scc
                    |> Seq.filter ( fun (i, cc) -> 
                        let targetCcs = 
                          cc
                          |> Seq.map (fun i -> componentsMap.[i])
                          |> Seq.concat 
                          |> Seq.distinct
                        let l = targetCcs |> Seq.length 
                        if l = 1 
                          then targetCcs |> Seq.head = i
                          else l = 0
                    )
                  unify cle [TEq (tscc |> toTerm, t2) ]
                  |> extendSolutions cle theta (Clause.Create(Pred("tscc",[])))
                else 
                  unify cle [TEq (scc |> toTerm, t2) ]
                  |> extendSolutions cle theta (Clause.Create(Pred("scc",[])))
         | Choice2Of2 _ -> None

    (***************)
    (* Arithmetics *)
    (***************)
    | Pred ("is", [Term.Var (x,y); aExpr])
    | Pred ("is", [aExpr; Term.Var (x,y)])  -> 
        (* eval evaluates an expression to a number*)
        match eval cle aExpr with
        | Some n -> 
            let sol = Substitution<'s, 'a>.Create((x,y), Float n)
            let theta' : Substitution<'s, 'a> = theta.Add(TVar (x,y), Float n, cle)
            Some [sol, theta', Goal<'s, 'a>.OK, Clause.Create( Pred("is",[]) ) ]
        | None -> None
    
    (**********************)
    (* Boolean operations *)
    (**********************)
    | Pred ("<", [aExpr; bExpr])  -> 
            match eval cle aExpr with
            | Some n1 -> 
              match eval cle bExpr with
              | Some n2 -> 
                if n1 < n2
                  then Some [Substitution.id, theta, Goal<'s, 'a>.OK, Clause.Create( Pred("<",[]) ) ]
                  else None
              | None -> None
            | None   -> None
    | Pred ("<", _ ) -> failwith <| sprintf "unsupported arithmetic operation \"%s\""  g.String
    | Pred ("<=", [aExpr; bExpr])  -> 
            match eval cle aExpr with
            | Some n1 -> 
              match eval cle bExpr with
              | Some n2 -> 
                if n1 <= n2
                  then Some [Substitution.id, theta, Goal<'s, 'a>.OK, Clause.Create( Pred("<=",[]) ) ]
                  else None
              | None -> None
            | None -> None
    | Pred ("<=", _ ) -> failwith <| sprintf "unsupported arithmetic operation \"%s\""  g.String
    | Pred (">", [aExpr; bExpr])  -> 
        match eval cle aExpr with
        | Some n1 -> 
          match eval cle bExpr with
          | Some n2 -> 
            if n1 > n2
              then Some [Substitution.id, theta, Goal<'s, 'a>.OK, Clause.Create( Pred(">",[]) )]
              else None
          | None -> None
        | None -> None
    | Pred (">", _ ) -> failwith <| sprintf "unsupported arithmetic operation \"%s\""  g.String
    | Pred (">=", [aExpr; bExpr])  -> 
            match eval cle aExpr with
            | Some n1 -> 
              match eval cle bExpr with
              | Some n2 -> 
                if n1 >= n2
                  then Some [Substitution.id, theta, Goal<'s, 'a>.OK, Clause.Create( Pred(">=",[]) ) ]
                  else None
              | None -> None
            | None -> None
    | Pred (">=", _ ) -> failwith <| sprintf "unsupported arithmetic operation \"%s\""  g.String
    // unify two terms
    | Pred ("=", [t1; t2])  -> 
        unify cle [TEq(t1, t2)]
        |> extendSolutions cle theta (Clause.Create( Pred("=",[])))
   
    (*******************)
    (* List operations *)
    (*******************)
    // list membership
    | Pred ("member", [t; t'])  -> 
        match Term.FlattenList t' with
        | Choice1Of2 ts  -> ts
                            |> List.map (fun t' -> 
                              Substitution.id, theta, [Pos (Pred ("=", [t;t']))], Clause.Create (Pred("member",[]))
                              )
                            |> Some
        | _ -> None
    // list containment check (no unification is called)
    | Pred ("contains", [t; t'])  -> 
      match Term.FlattenList t' with
      | Choice1Of2 ts  -> if ts |> List.contains t
                           then Some [Substitution.id, theta, Goal<'s, 'a>.OK, Clause.Create( Pred("contains",[]) )]
                           else None
      | _ -> None
   
    // reverse a list
    | Pred ("reverse", [ts; x])  -> 
        match Term.FlattenList ts with
        |Choice1Of2 t ->
          unify cle [TEq(List.rev t |> TList, x)]
          |> extendSolutions cle theta (Clause.Create( Pred("reverse",[]) ) )
        | Choice2Of2 _ -> None
    
    // append two lists together
    | Pred ("append", [ts1; ts2; x])  -> 
      match Term.FlattenList ts1, Term.FlattenList ts2 with
      |Choice1Of2 t1, Choice1Of2 t2  ->
        let t3 = t1@t2
        unify cle [TEq (t3 |> TList, x)]
        |> extendSolutions cle theta (Clause.Create( Pred("append",[]) ) )
      | _ -> None
  
    // similar to List.concat
    | Pred ("concat", [ts; x])  -> 
      let concat x = 
        x
        |> List.fold (fun acc t -> match t |> Term.FlattenList with 
                                    | Choice1Of2 ts -> (List.rev ts) @ acc
                                    | _  -> failwith <| sprintf "Cannot concat indeterminate list %s " (Term.ToString t)) []
        |> List.rev
      match Term.FlattenList ts with 
      | Choice1Of2 tlist -> 
          unify cle [TEq (x, concat tlist |> TList)]
          |> extendSolutions cle theta (Clause.Create(Pred("concat",[])))
      | Choice2Of2 (tlist, rest) -> 
          unify cle [TEq (x, TCons(concat tlist |> TList, rest))]
          |> extendSolutions cle theta (Clause.Create( Pred("concat",[])))
    // sorts a list; duplicates are maintained
    | Pred ("sort", [ts; x])  -> 
      match Term.FlattenList ts with
      |Choice1Of2 t ->
        unify cle [TEq(List.sort t |> TList, x)]
        |> extendSolutions cle theta (Clause.Create( Pred("reverse",[]) ) )
      | Choice2Of2 _ -> None
    // removes duplicates from a list
    | Pred ("distinct", [ts; x])  -> 
      match Term.FlattenList ts with
      |Choice1Of2 t ->
        unify cle [TEq(t |> List.distinct |> TList, x)]
        |> extendSolutions cle theta (Clause.Create( Pred("reverse",[]) ) )
      | Choice2Of2 _ -> None
    
    | Pred ("difference", [ts1; ts2; x])  -> 
      match Term.FlattenList ts1, Term.FlattenList ts2 with 
      | Choice1Of2 t1, Choice1Of2 t2 -> 
          let d = t1 |> List.filter (fun t -> not (List.contains t t2))
          unify cle [TEq(TList d, x)]
          |> extendSolutions cle theta (Clause.Create( Pred("reverse",[]) ) )
      | _, _ -> None
   
    | Pred ("intersection", [ts1; ts2; x])  -> 
      match Term.FlattenList ts1, Term.FlattenList ts2 with 
      | Choice1Of2 t1, Choice1Of2 t2 -> 
        let terms, theta' = 
          t1 
          |> List.fold (fun (tAcc, sol:Substitution<'s, 'a>) t ->
                          let sol' = t2 
                                      |> List.map (fun x -> sol.Apply(x, cle))
                                      |> List.collect (fun z -> unify cle [TEq(z, t)])
                          match sol' with 
                          | [] -> (tAcc, Substitution.Update sol theta cle)
                          | thetas -> (t::tAcc, Substitution.Update sol theta cle)
                      ) ([], Substitution.id)
          |> fun (ts, sol) -> let ts' = ts |> List.rev |> List.map (fun x-> theta.Apply(x, cle)) |> TList
                              ts', sol
        match unify cle [TEq (x, terms)] with
        | [] -> None
        | thetas ->
          thetas
          |> List.map (fun theta'' -> 
              let sol = Substitution.Compose theta' theta'' cle
              let theta''' = theta 
                              |> fun x -> Substitution.Update x theta'  cle
                              |> fun x -> Substitution.Update x theta'' cle
              sol, theta''', Goal<'s, 'a>.OK, Clause.Create( Pred("intersection",[]) ))
          |> Some
      | _ -> None
    
    (*******************)
    (* meta predicates *)
    (*******************)
    // findAll(+X, +p(a1, ..., X, ... aN), -Xs)
    // returns Xs, the list of all variables X such that satisfy predicate p(a1, ..., X, ... aN)
    | Pred ("findAll", [t; Term.Func(f, args); Term.Var (xs,ys)])  -> 
      let p = Pred (f, args)
      let g = Goal<'s, 'a>.Create ([Pos p], cle)
      match resolveInner [g] cle varCounter program [] AllAnswers (*debugMap*) with 
      | Some gmatches -> 
        let res = gmatches 
                  |> List.map (fun (sub:Substitution<'s, 'a>)-> sub.Apply(t, cle))
                  |> List.distinct
                  |> TList 
        unify cle [TEq(res, Term.Var (xs,ys))]
        |> extendSolutions cle theta (Clause.Create( Pred("reverse",[]) ) )
      | None -> unify cle [TEq(TList [], Term.Var (xs,ys))]
                |> extendSolutions cle theta (Clause.Create( Pred("reverse",[]) ) )
    
    (***********************************)
    (* Unify query with program clause *)
    (***********************************)
    | Pred (gSym, gArgs) -> 
      let key  = gSym, gArgs.Length
      if not <| program.ContainsKey key
        then None
        else
      program.Item key
      |> Set.toList // TODO: replace with Set.filter
      |> List.choose (fun rule -> 
          match rule.head with 
          | Pred (pSym, pArgs) -> 
            if pSym = gSym && pArgs.Length = gArgs.Length
            then // unify goal with a clause in the program
              let idCache : Map<string, int> ref = emptyIdCache () 
              let idProvider = cachedIdProvider varCounter idCache >> fst
              let freshArgs  = pArgs |> List.map (Term.refresh cle idProvider)
              let eqs = List.zip gArgs freshArgs
                        |> List.map TEq
              match unify cle eqs with 
              | []   -> None 
              | sols -> sols
                        |> List.fold (fun acc sol -> 
                          let theta'    = Substitution.Update theta sol cle
                          let freshBody = rule.body |> List.map (Literal<'s>.refresh cle idProvider >> applyG cle sol)
                          (sol, theta', freshBody, rule) :: acc) []
                        |> Some
            else None)
      |> List.concat
      |> fun x -> if x.IsEmpty then None else Some x
 (* // performance profiling
 let dt = System.DateTime.Now - t
 match (!debugMap).TryFind g.Name with 
 | None -> debugMap := (!debugMap).Add(g.Name, dt)
 | Some t' -> debugMap := (!debugMap).Add(g.Name, t' + dt)
 *)
 r       

and resolveInner (slds:Goal<'s, 'a> list) cle varCounter program oldSolutions mode (*debugMap*) =
  match slds with 
  | []  -> None
  | _   -> 
    let expandGoal (sld:Goal<'s, 'a>) : Goal<'s, 'a> list = 
      match sld.goals with
      // SLD resolution
      | Pos atom :: gs ->
        if mode = AllAnswers && atom |> fvp cle |> Set.isEmpty
            then 
                let oneQueryOnly = Goal<'s, 'a>.Create([Pos atom], cle)
                                    |> List.singleton
                match resolveInner oneQueryOnly cle varCounter program [] SingleAnswer (*debugMap*) with
                | Some _ -> (sld.solution, gs, Clause.Create (Pred (sprintf "not(%s)" atom.String,[])))
                                |> Goal<'s, 'a>.Create
                                |> List.singleton
                | None -> []
        else 
        match resolveGoal program sld.solution cle varCounter atom (*debugMap*) with
        // create a new SLD node for each match
        | Some gmatches -> 
            gmatches 
            |> List.map (fun (tmpSol, sol', newGoals, rule) -> 
                let gs' = newGoals @ (gs |> List.map (applyL cle tmpSol) )
                Goal<'s, 'a>.Create(sol', gs', rule.head |> applyP cle tmpSol |> Clause.Create))

        // resolution failed; remove the SLD node from the list
        | None -> [] 
      // negation-as-failure (NF)
      | Neg atom :: gs -> 
              // Floundering check: try to disprove ground atoms only (otherwise SLDNF might be unsound)
              (* TODO: this check could be relaxed by allowing goals to have variables,
                       and fail NF only if all solutions always contain the empty substitution.
                       Some Prolog implementations do not check for floundering at all.
                       (see pg. 74, "Logic, Programming and Prolog" by U. Nilsson, J. Maluszynski *)
              if Set.isEmpty (fvp cle atom)
                then 
                  let counterProof = Goal<'s, 'a>.Create([Pos atom], cle)
                  match resolveInner [counterProof] cle varCounter program [] SingleAnswer (*debugMap*) with
                  | None   -> [sld.solution, gs, Clause<'s>.Create (Pred (sprintf "not(%s)" atom.String,[]))]
                              |> List.map Goal<'s, 'a>.Create
                  | Some _ -> [] // negation failed
                else [] 
      | [] -> []
        
    match mode with 
    | SingleAnswer -> 
      let slds' = slds |> List.collect expandGoal
      match slds' |> List.tryFind (fun s -> s.goals.IsEmpty) with
      | Some sld -> Some [sld.solution]
      | None     -> if slds' = slds 
                      then None 
                      else resolveInner slds' cle varCounter program [] mode (*debugMap*)
    | AllAnswers -> 
      let g, gs = slds.Head, slds.Tail
      let successfulSlds, newSlds = g |> expandGoal |> List.partition (fun s -> s.goals.IsEmpty)
      let newSolutions = successfulSlds |> List.map (fun g -> g.solution)
      let sols = oldSolutions @ newSolutions
      if newSlds.IsEmpty &&  gs.IsEmpty
        then if sols.IsEmpty
              then None
              else Some sols
        else resolveInner (newSlds @ gs) cle varCounter program sols mode (*debugMap*)

let resolve goal (program:RulesProgram<'s>) (cle:CLE<'s, 'a>)  (*debugMap*) = 
  let counter = fvl cle goal |> Set.count |> fun x -> x + 1 |> ref
  resolveInner [Goal<'s, 'a>.Create([goal], cle)] cle counter program [] SingleAnswer (*debugMap*)
    |> Option.map List.head
                     
let resolveAll (goal:Literal<'s>) (program:RulesProgram<'s>) (cle:CLE<'s,'a>) = 
  let counter = fvl cle goal |> Set.count |> fun x -> x + 1 |> ref
  resolveInner [Goal<'s, 'a>.Create([goal], cle)] cle counter program [] AllAnswers