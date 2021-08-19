[<JavaScript>]
module RulesDSD.Substitution

open RulesDSD.Syntax

open System.Diagnostics

let mapUnion (loses: Map<_,_>) (wins: Map<_,_>) = 
        Seq.fold (fun m (KeyValue(k,v)) -> Map.add k v m) loses wins

[<DebuggerDisplay("{this.String}")>]   
type Substitution<'s, 'a when 's :equality and 'a :equality> = Sub of Map<Var, Choice<Term<'s>, Location, 's, 'a>>
  with 
    static member Apply (Sub sub:Substitution<'s, 'a>, s:'s, cle:CLE<'s, 'a>)  : 's = cle.applyAll sub (Choice1Of2 s)
    static member Apply (Sub sub:Substitution<'s, 'a>, a:'a, cle:CLE<'s, 'a>)  : 'a = cle.applySubVar sub a

    (* identity substitution *)
    static member id                         : Substitution<'s, 'a> = Sub Map.empty
    static member Create (x:Var, t:Term<'s>) : Substitution<'s, 'a> = Sub (Map.ofList [x, Choice1Of4 t])
    static member Create (x:Var, l:Location) : Substitution<'s, 'a> = Sub (Map.ofList [x, Choice2Of4 l])
    static member Create (x:Var, s:'s)       : Substitution<'s, 'a> = Sub (Map.ofList [x, Choice3Of4 s])
    static member Create (x:Var, s:'a)       : Substitution<'s, 'a> = Sub (Map.ofList [x, Choice4Of4 s])
    
    (* add a binding *)
    member this.Add(k:Variable<'s, 'a>, t:Term<'s>, cle:CLE<'s, 'a>) = 
      match k with
      | TVar x -> 
        let sub : Substitution<'s, 'a> = Substitution<'s, 'a>.Create(x, t)
        Substitution.Compose this sub cle
      | _      -> failwith <| sprintf "Substitution extension failed: cannot extend %s with %s" (this.String cle) k.String

    member this.Add(k:Variable<'s, 'a>, l:Location, cle:CLE<'s, 'a>) = 
      match k with
      | LVar x -> 
        let sub : Substitution<'s, 'a> = Substitution<'s, 'a>.Create(x, l)
        Substitution.Compose this sub cle
      | _      -> failwith <| sprintf "Substitution extension failed: cannot extend %s with %s" (this.String cle) k.String   
    
    member this.Add(k, x:'a, cle:CLE<'s, 'a>) = 
      let sub = Substitution<'s, 'a>.Create(k, x)
      Substitution.Compose this sub cle

    member this.Add(k, c:Choice<Term<'s>, Location, 's, 'a>, cle:CLE<'s, 'a>) = 
        let sub = match c with 
                  | Choice1Of4(x) -> Substitution<'s, 'a>.Create(k, x)
                  | Choice2Of4(x) -> Substitution<'s, 'a>.Create(k, x)
                  | Choice3Of4(x) -> Substitution<'s, 'a>.Create(k, x)
                  | Choice4Of4(x) -> Substitution<'s, 'a>.Create(k, x)
        Substitution.Compose this sub cle

    (* map values *)
    member this.Map (f : Var -> Term<'s> -> Term<'s>) = 
      let wrapper x y = match y with 
                        | Choice1Of4 z -> Choice1Of4 (f x z)
                        | _            -> y
      match this with Sub m -> Sub (Map.map wrapper m)
    member this.Map (f : Var -> Location -> Location)  = 
      let wrapper x y = match y with 
                        | Choice2Of4 z -> Choice2Of4 (f x z)
                        | _            -> y
      match this with Sub m -> Sub (Map.map wrapper m)

    member this.Map (f : Var -> 's -> 's)  = 
      let wrapper x y = match y with 
                        | Choice3Of4 z -> Choice3Of4 (f x z)
                        | _            -> y
      match this with Sub m -> Sub (Map.map wrapper m)    

    member this.Map (f : Var -> 'a -> 'a)  = 
      let wrapper x y = match y with 
                        | Choice4Of4 z -> Choice4Of4 (f x z)
                        | _            -> y
      match this with Sub m -> Sub (Map.map wrapper m)    

    static member Apply (Sub sub:Substitution<'s, 'a>, loc:Location)  : Location = 
      match loc with
      | Location.Var (-1, "_") -> Location.Var (-1, "_")
      | Location.Var v         -> match sub.TryFind(v) with 
                                  | Some (Choice2Of4 l) -> l
                                  | Some (Choice1Of4 _)
                                  | Some (Choice3Of4 _)
                                  | Some (Choice4Of4 _)
                                  | None                -> loc
      | Location.Loc _   -> loc

    static member Apply (sub, (s:'s, l:Location), cle:CLE<'s, 'a>) = Substitution<'s, 'a>.Apply(sub, s, cle), Substitution<'s, 'a>.Apply(sub, l)
    static member Apply (Sub sub, p:Pattern<'s>, cle:CLE<'s, 'a>)  = p |> Pattern.Map (fun (s, l) -> (cle.applyAll sub (Choice1Of2 s), Substitution<'s, 'a>.Apply(Sub sub, l)))
    static member Apply (Sub s : Substitution<'s, 'a>, t:Term<'s>, cle:CLE<'s, 'a>) : Term<'s> = 
      let sub : Substitution<'s, 'a> = Sub s
      match t with
      | Term.Var (-1, "_") -> t
      | Term.Var (x,y)     -> match s.TryFind((x,y)) with 
                              | Some (Choice1Of4 t') -> t'
                              | Some (Choice2Of4 l)  -> Pat <| Pattern.Inner [cle.underscore(), l]
                              | Some (Choice3Of4 sp) -> Pat <| Pattern.Inner [cle.applyAll s (Choice1Of2 sp), Location.Var (-1,"_")]
                              | Some (Choice4Of4 sp) -> Pat <| Pattern.Inner [cle.applySubVar s sp |> cle.cast, Location.Var (-1,"_")]
                              | None -> t

      | Term.Const _       -> t
      | Term.Float _       -> t
      | Term.Func (f, ts)  -> Term<'s>.Func (f, ts |> List.map (fun t -> sub.Apply(t, cle)))
      | Term.TList ts      -> Term<'s>.TList (ts |> List.map (fun t -> sub.Apply(t, cle)))
      | Term.TCons (t, t') -> Term<'s>.TCons (sub.Apply(t, cle),  sub.Apply(t', cle))
      | Term.Pat p         -> Substitution<'s, 'a>.Apply(sub, p, cle) |> Pat
      | Term.Proc  s       -> s |> Process<'s>.Map (fun (x:'s) -> Substitution<'s, 'a>.Apply(sub, x, cle))  |> Proc
      | Term.TCRN ts       -> ts 
                              |> List.map (fun t -> sub.Apply(t, cle))
                              |> List.distinct
                              |> TCRN 
      | Term.TMSet ts      -> ts 
                              |> List.map (fun (n, t) -> n, sub.Apply(t, cle)) 
                              |> TMSet 

    member this.Apply(term:Term<'s>, cle:CLE<'s, 'a>)               = Substitution<'s, 'a>.Apply(this, term, cle)
    member this.Apply(loc:Location)                                 = Substitution<'s, 'a>.Apply(this, loc)
    member this.Apply(sp:'s, cle:CLE<'s, 'a>)                       = Substitution<'s, 'a>.Apply(this, sp, cle)
    member this.Apply(a:'a, cle:CLE<'s, 'a>)                        = Substitution<'s, 'a>.Apply(this, a, cle)
    member this.Apply(x:Choice<Term<'s>, Location, 's, 'a>, cle:CLE<'s, 'a>) =
      match x with 
      | Choice1Of4 t -> this.Apply(t, cle) |> Choice1Of4
      | Choice2Of4 l -> this.Apply l       |> Choice2Of4
      | Choice3Of4 s -> this.Apply(s, cle) |> Choice3Of4
      | Choice4Of4 a -> this.Apply(a, cle) |> Choice4Of4
    member this.Apply(p:Pattern<'s>, cle:CLE<'s,'a>)                = Substitution<'s, 'a>.Apply(this, p, cle)
    member this.Apply((s:'s, l:Location), cle:CLE<'s, 'a>)          = Substitution<'s, 'a>.Apply(this, s, cle), Substitution<'s, 'a>.Apply(this, l)
    member this.Apply(p:RulesDSD.Syntax.Predicate<'s>, cle:CLE<'s, 'a>) = 
      match p with 
      | RulesDSD.Syntax.Predicate.Pred(x,y) -> RulesDSD.Syntax.Predicate<'s>.Pred(x, (y |> List.map (fun x -> this.Apply(x, cle)))) 
    
    member this.Apply(l:Literal<'s>, cle)  = match l with 
                                             | Neg p -> Neg (this.Apply(p, cle))
                                             | Pos p -> Pos (this.Apply(p, cle))

    member this.Apply(c:Clause<'s>, cle:CLE<'s, 'a>)  = 
      let head = this.Apply(c.head, cle) 
      let body = c.body |> List.map (fun x -> this.Apply(x, cle))
      Clause<'s>.Create(head, body)

    member this.Add(that:Substitution<'s, 'a>) = 
      match this, that with | Sub m1, Sub m2 -> Sub (mapUnion m1 m2)

    member this.Keys () = match this with Sub s -> s |> Map.toList |> List.map fst

    static member Update (Sub x:Substitution<'s, 'a>) (y:Substitution<'s, 'a>) (cle:CLE<'s, 'a>) = 
      x |> Map.map (fun _ i -> y.Apply(i, cle))
        |> Sub
                                
    (* Substitution composition. NB: this operation is NOT commutative; the left operand has precedence over  *)
    static member Compose (Sub x:Substitution<'s, 'a>) (Sub y:Substitution<'s, 'a>) (cle:CLE<'s, 'a>) =
      x
      |> Map.map (fun _ v -> 
          match v with 
          | Choice1Of4 t -> Choice1Of4 ((Sub y).Apply(t, cle))
          | Choice2Of4 l -> Choice2Of4 ((Sub y).Apply(l))
          | Choice3Of4 s -> Choice3Of4 (cle.applySub y s)
          | Choice4Of4 s -> Choice4Of4 (cle.applySubVar y s))
      |> fun x -> (Sub x).Add(Sub y)
    
    (* Composes two substitutions only if they have disjoint domains *)
    static member TryMerge (Sub x:Substitution<'s, 'a>) (Sub y:Substitution<'s, 'a>) (cle:CLE<'s, 'a>) : Substitution<'s, 'a> option =
      y 
      |> Map.fold (fun acc k v -> 
           match acc with 
           | None -> None 
           | Some (Sub x') -> 
               match x'.TryFind(k) with 
               | Some v' -> if v = v' 
                             then Some (Sub x')
                             else None
               | None    -> let sub' = (Sub x').Add(k,v, cle)
                            Some sub'
      ) (Some (Sub x))

    (* display *)
    member this.String (cle:CLE<'s, 'a>) = 
      let mappings = match this with 
                     | Sub s -> s |> Map.toList 
                                  |> List.map (fun (k,v) -> let txt = match v with 
                                                                      | Choice1Of4 t  -> Term.ToStringWith cle t
                                                                      | Choice2Of4 l  -> Location.ToString l 
                                                                      | Choice3Of4 sp -> cle.toString sp
                                                                      | Choice4Of4 sp -> cle.toStringTempVar sp
                                                            sprintf "%s -> %s" (printVar k) txt)
      String.concat "; " mappings

(* free variables *)
let unionFold f = List.fold (fun x y -> Set.union x (f y)) Set.empty

let fvLocation l = 
  match l with
  | Location.Loc _         -> Set.empty
  | Location.Var (-1, "_") -> Set.empty
  | Location.Var x         -> Set.singleton (LVar x)

let rec fvPattern (cle:CLE<'s, 'a>) p = 
  let fv (x, y) = cle.fvs x |> Set.union (fvLocation y)
  match p with
  | Pattern.Inner        a -> unionFold fv a
  | Pattern.Nicking (a, b) -> Set.union (unionFold fv a) (unionFold fv b)
  | Pattern.Strand       a -> unionFold fv a
  | Pattern.ThreePrime   a -> unionFold fv a
  | Pattern.FivePrime    a -> unionFold fv a
  | Pattern.Nihil          -> Set.empty

and fvSystem (cle:CLE<'s, 'a>) (Process.Proc s) = 
  s |> Map.toList 
    |> List.map (snd >> unionFold cle.fvs) 
    |> Set.unionMany

and fvt (cle:CLE<'s, 'a>) t = 
  match t with 
  | Term.Var (-1, "_") -> Set.empty
  | Term.Var (x,y)     -> Set.singleton (TVar (x,y))
  | Term.Const _       -> Set.empty
  | Term.Float _       -> Set.empty
  | Term.Func (_, ts)  -> ts |> unionFold (fvt cle)
  | Term.TList ts      -> ts |> unionFold (fvt cle)
  | Term.TCons (t,t')  -> [t;t'] |> unionFold (fvt cle)
  | Term.Pat p         -> p  |> fvPattern cle
  | Term.Proc s        -> s  |> fvSystem cle
  | Term.TCRN ts       -> ts |> unionFold (fvt cle)
  | Term.TMSet ts      -> ts |> List.map snd |> unionFold (fvt cle)

let fvp (cle:CLE<'s, 'a>) (Pred (_, ts)) = ts |> unionFold (fvt cle)

let rec fvl (cle:CLE<'s, 'a>) lit = 
  match lit with 
  | Pos p   -> fvp cle p
  | Neg p   -> fvp cle p