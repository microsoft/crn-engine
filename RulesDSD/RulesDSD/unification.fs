[<JavaScript>]
module RulesDSD.Unification

open RulesDSD.Syntax
open RulesDSD.Substitution

open System 
open System.Diagnostics

(**************************************)
(**************************************)
(********    Unification    ***********)
(* Unification is the process of finding a substitution \theta such that
   term_1\theta = term_2\theta for any term t_1 and t_2 [*]. 

   If t_1 and t_2 are ground terms (i.e. they have no free variables), 
   unification reduces to checking that t_1 is equal to t_2.
   In this case unification returns the identity substitution, Substitution.id.
   
   
   *: The notation t\theta means "apply substitution \theta to t".
    It's a useful notation to express function composition:
      t\theta_1\theta_2 
    which is parsed (t\theta_1)\theta_2) and means "apply \theta_1 and then \theta_2 to t".
   *)
(**************************************)
(**************************************)
[<DebuggerDisplay("{this.String}")>]
type TermEquation<'s when 's : equality> = TEq of Term<'s> * Term<'s>
  with member this.String = match this with
                            | TEq (t1, t2) -> sprintf "%s = %s" (Term.ToString t1) (Term.ToString t2)



let rec unifyLocation (l1, l2) : Substitution<'s, 'a> option=
  match l1, l2 with
  | Location.Loc(x1,y1), 
    Location.Loc(x2,y2) -> if (x1,y1) = (x2,y2) 
                            then Some (Substitution<'s, 'a>.id)
                            else None
  | Location.Var (-1,"_"), _ 
  | _, Location.Var (-1,"_")   -> Some (Substitution<'s, 'a>.id)
  | Location.Var v, l 
  | l, Location.Var v     -> Some (Substitution<'s, 'a>.Create(v, l))

and unifyHoleEntry (cle:CLE<'s, 'a>) ((s1:'s, l1), (s2:'s, l2)) = 
  match cle.unify (s1, s2) with
  | [] -> []
  | thetas1 ->
    match unifyLocation (l1, l2) with
    | Some theta2 -> thetas1 |> List.map (fun theta1 -> Substitution<'s, 'a>.Compose (Sub theta1) theta2 cle)
    | None        -> []

and unifyInner (theta:Substitution<'s, 'a>) (cle:CLE<'s, 'a>) (sys:TermEquation<'s> list) : Substitution<'s, 'a> list = 
  match sys with
  | []               -> [theta]
  | TEq (t1, t2) :: rest -> 
    match t1, t2 with
    (*** list unification ***)
    | TCons (t, ts), TList (t' :: ts')
    | TList (t' :: ts'), TCons (t, ts) -> unifyInner theta cle (TEq (t, t') :: TEq (ts, TList ts') :: rest)
    
    
    | TCons (t, Term.Var(X,Y)), TCons (t', ts) 
    | TCons (t', ts), TCons (t, Term.Var (X,Y)) 
      -> unifyInner theta cle (TEq (t, t') :: TEq (Term.Var (X,Y), ts) :: rest)
    
    | TCons (t, Term.Var (X,Y)), TList ts 
    | TList ts, TCons (t, Term.Var (X,Y))
     -> match ts with 
        | []        -> [] 
        | t' :: ts' -> unifyInner theta cle (TEq (t, t') :: TEq (Term.Var (X,Y), TList ts') :: rest) 
    
    | TCons (t, ts), TCons (t', ts') 
    | TCons (t', ts'), TCons (t, ts) 
      -> unifyInner theta cle (TEq (t, t') :: TEq (ts, ts') :: rest) 

    | TList ts1, TList ts2 -> 
      if ts1.Length = ts2.Length
        then 
          let newEqs = List.zip ts1 ts2 
                        |> List.map TEq
          unifyInner theta cle (newEqs @ rest) 
        else []
     
     (*** Sets and Multisets unification ***)
     // Unification in this section roughly follows the paper:
     // "Integrating Lists, Multisets, and Sets in a Logic Programming Framework", A. Dovier, A. Policriti, G. Rossi at https://doi.org/10.1007/978-94-009-0349-4_16
     | TCRN crn1, TCRN crn2 ->
       // TODO: otimization: check if the CRN is ground?
       if crn1.Length <> crn2.Length 
         then []  // unification fails
         else
           let rec g t1 crn2 acc crn1 sols = 
             match crn2 with
             | []        -> sols
             | t2::rest2 -> let crn1'   = TCRN crn1
                            let crn2'   = TCRN (rest2 @ acc)
                            let sols' = unifyInner theta cle (TEq (t1, t2) :: TEq (crn1', crn2') :: rest)
                            g t1 rest2 (t2::acc) crn1 (sols @ sols')
           and f crn1 crn2 acc sols = 
             match crn1  with 
             | []        -> match crn2 with
                            | []   -> unifyInner theta cle rest
                            | _::_ -> sols
             | t1::rest1 -> let crn1' = acc @ rest1
                            let sols' = g t1 crn2 [] crn1' sols 
                            f rest1 crn2 (t1::acc) sols'
                               
           f crn1 crn2 [] [] |> List.distinct

       (* set unification algorithm (adapted to the case {t0...tn|X} with X = []):
             t|s = t'|s' iff t=t' and:
             s = s'
             or t|s = s'
             or s = t'|s'
       *)
     | TMSet ts1, TMSet ts2 ->
         if (ts1.Length = 0 && ts2.Length <> 0) 
            || (ts2.Length = 0 && ts1.Length <> 0) // TODO: what if two elements in one mset are unified, and the lenghts become different?
         then []
         else 
           match ts1 with 
           | []        -> unifyInner theta cle rest
           | (n1, t1)::tail1 -> 
             let rec f acc heads2 curr =
               match curr with
               | (n2, t2) :: tail2 -> 
                 match unifyInner theta cle [TEq (t1, t2)] with 
                 | []      -> // unify t1 with another element of the multiset
                     f acc ((n2, t2)::heads2) tail2 
                 | thetas' ->
                   let sols = 
                     thetas' 
                     |> List.collect (fun theta' -> 
                       let t1' = if n1 = n2 
                                   then TMSet tail1
                                   else TMSet ((abs (n1-n2), t1)::tail1)
                                 |> fun x -> theta'.Apply(x, cle)
                       let t2' = TMSet (heads2@tail2)
                                 |> fun x -> theta'.Apply(x, cle)
                       let rest' = rest |> List.map (fun (TEq(t1, t2)) -> TEq (theta'.Apply(t1, cle), theta'.Apply(t2, cle)))
                       let newConstraints = TEq (t1', t2') :: rest'
                       unifyInner theta' cle newConstraints)
                   f (sols @ acc) ((n2, t2)::heads2) tail2
               | []      -> acc
             
             f [] [] ts2
             |> List.collect (fun theta' -> unifyInner theta' cle rest)
             |> List.distinct




    (*** Operations unification ***)
    | Func (f, xs), Func (g, ys) -> 
        if f <> g 
          then []
        elif xs.Length <> ys.Length
          then []
          else 
            let newEqs = List.zip xs ys |> List.map TEq
            unifyInner theta cle (newEqs @ rest) 
    (*** Base terms unification ***)
    | Float n1, Float n2 -> 
        if n1 = n2 
          then unifyInner theta cle rest 
          else []
    | Const c1, Const c2 -> 
        if c1 = c2 
          then unifyInner theta cle rest 
          else []
    
    (*** Variables unification ***)
    | t, Term.Var (-1, "_")
    | Term.Var (-1, "_"), t -> match t with 
                               | Term.Var (-1, "_") -> unifyInner theta cle rest 
                               | Term.Var (X, Y)    -> let theta' : Substitution<'s, 'a> = theta.Add(TVar (X,Y), Term.Var (-1, "_"), cle)  
                                                       rest |> List.map (fun (TEq(t1, t2)) -> TEq (theta'.Apply(t1, cle), theta'.Apply(t2, cle)))
                                                            |> fun x -> unifyInner theta' cle x 
                               | _             -> unifyInner theta cle rest 
    | Term.Var (X,Y), t -> if Term.Var (X,Y) = t
                            then unifyInner theta cle rest // skips "X = X"
                           elif (fvt cle t |> Set.map Variable<_,_>.ToVar).Contains (X,Y)
                            then [] // occurs-check failed
                            else 
                              let theta' = theta.Add(TVar (X,Y), t, cle)
                              rest |> List.map (fun (TEq(t1, t2)) -> TEq (theta'.Apply(t1, cle), theta'.Apply(t2, cle )))
                                   |> unifyInner theta' cle 
    
    (*** Patterns unification ***)
    | (Pat p1, Pat p2)  -> 
        match p1, p2 with
        | Nihil, Nihil -> [Substitution<'s, 'a>.id]
        | Nicking (a1, b1), Nicking (a2, b2) 
           -> if a1.Length <> a2.Length
                 || b1.Length <> b2.Length
                then []
                else let a3 = List.zip a1 a2
                     let b3 = List.zip b1 b2
                     let unifiedPatterns = 
                       (a3 @ b3)
                       |> List.fold (fun subs (s1, s2) -> 
                                       subs 
                                       |> List.collect (fun (sub : Substitution<'s, 'a>)-> unifyHoleEntry cle (sub.Apply(s1, cle), sub.Apply(s2, cle)))
                                       |> List.allPairs subs
                                       |> List.map (fun (sub, sub') -> Substitution.Compose sub sub' cle)) 
                                    [Substitution.id]
                     unifiedPatterns 
                     |> List.choose (fun x -> Substitution.TryMerge theta x cle)
                     |> List.collect (fun (theta' : Substitution<'s, 'a>) -> 
                                        rest 
                                        |> List.map (fun (TEq (x, y)) -> TEq (theta'.Apply(x, cle), theta'.Apply(y, cle)))
                                        |> unifyInner theta' cle)

        | Pattern.Strand s1,      Pattern.Strand s2
        | Pattern.Inner s1,       Pattern.Inner s2 
        | Pattern.ThreePrime s1,  Pattern.ThreePrime s2
        | Pattern.FivePrime s1,   Pattern.FivePrime s2
          ->  if s1.Length <> s2.Length
                then [] 
                else 
                  let unifiedPattern =
                    List.zip s1 s2
                    |> List.fold (fun subs (s1, s2) -> 
                          subs 
                           |> List.collect (fun (sub : Substitution<'s,'a>) -> unifyHoleEntry cle (sub.Apply(s1, cle), sub.Apply(s2, cle)))
                           |> List.allPairs subs
                           |> List.map (fun (sub, sub') -> Substitution.Compose sub sub' cle)) 
                        [Substitution.id]
                  unifiedPattern 
                  |> List.choose (fun x -> Substitution.TryMerge theta x cle)
                  |> List.collect (fun (theta' : Substitution<'s,'a>) ->
                                        rest 
                                        |> List.map (fun (TEq (x, y)) -> TEq (theta'.Apply(x, cle), theta'.Apply(y, cle)))
                                        |> unifyInner theta' cle)
        | _, _ -> [] // sprintf "Patterns mismatch: \"%s\", \"%s\" " (t1.ToString()) (t2.ToString())

    | (Term.Proc p1, Term.Proc p2) -> // TODO: this assumes processes are in canonical form
      if Process.Canonical cle p1 = Process.Canonical cle p2 
        then unifyInner theta cle rest
        else []
      
    // flip var on rhs
    | (t1, Var (X,Y)) -> unifyInner theta cle (TEq (Var (X,Y), t1)::rest)
    | _ -> [] // Choice2Of2 <| sprintf "Failed unification: cannot unify \"%s\" with \"%s\""  (Term.ToString t1) (Term.ToString t2)

and unify cle : TermEquation<'s> list -> Substitution<'s, 'a> list = unifyInner Substitution<'s,'a>.id cle