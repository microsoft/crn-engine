[<JavaScript>]
module RulesDSD.Syntax

open System 
open System.Diagnostics

// Position is an internal index, that pinpoints a domain within a strand graph.
// The first int indexes a strand in a strand graph, the second int indexes a domain within that strand.
// This is for internal use only.
type StrandID  = int
type SiteIndex = int
type BondID    = int
type Var       = int * string
type Pos       = StrandID * SiteIndex
type Range     = StrandID * (SiteIndex * SiteIndex)

let GT = 1
let LT = -1
let EQ = 0

let printVar (x,y) = if x >=0 then sprintf "%s(%i)" y x else y

(* CRN reaction encoding keywords *)
type Keywords = Base of unit
  with
    static member context        = "_ctx"
    static member reaction       = "_rxn"
    static member mset           = "_mset"
    static member massActionRate = "_massActionRate"
    static member functionalRate = "_functionalRate"
    





(* Variables distinguished by type *)
[<DebuggerDisplay("{this.String}"); CustomEquality; CustomComparison>]
type Variable<'s, 'a> = TVar of Var      // terms
                      | LVar of Var      // locations
                      | SVar of Var * 's // species
                      | IVar of Var * 'a // sub-species
 with
  member this.String = 
    match this with
    | TVar x      -> printVar x
    | LVar x      -> printVar x
    | SVar (x, _) -> printVar x 
    | IVar (x, _) -> printVar x
  static member ToVar (this :Variable<'s, 'a>) = 
    match this with | TVar x | SVar (x, _) | LVar x | IVar (x, _) -> x
  override this.Equals that  = 
    match that with
    | :? Variable<'s, 'a> as that' -> Variable<_,_>.ToVar this = Variable<_,_>.ToVar that'
    | _                        -> invalidArg "yobj" "cannot compare values of different types"
  override this.GetHashCode() = Variable<_,_>.ToVar this |> hash
  interface System.IComparable with
      member this.CompareTo that = 
        match that with
        | :? Variable<'s, 'a> as that' -> compare (Variable<_,_>.ToVar this) (Variable<_,_>.ToVar that')
        | _                        -> invalidArg "yobj" "cannot compare values of different types"
                 
(* variable ID provider *)
let emptyIdCache () = ref Map.empty

let cachedIdProvider counter =
  fun (cache:Map<'s, int> ref) name -> 
    if (!cache).ContainsKey name
      then ((!cache).[name], name)
      else 
        let n = !counter
        counter := n + 1
        cache := (!cache).Add(name, n)
        (n, name)

let newIdProvider () = 
  cachedIdProvider (ref 0)

let makeProvider () = 
  newIdProvider () (emptyIdCache ())

let idProvider () : 's -> int * 's = 
  let cache = ref Map.empty
  newIdProvider () cache


[<DebuggerDisplay("{ToString(this)}")>]
type Location = Loc of StrandID * SiteIndex
                | Var of Var
  with
  static member ToString(l:Location) =
    match l with 
    | Location.Loc (i, j) -> sprintf "(%i, %i)" i j
    | Location.Var v      -> printVar v
  static member Compare (l1:Location) (l2:Location) =
    match l1, l2 with
    | Location.Var (_,X), Location.Var (_,Y) -> System.String.Compare(X,Y)
    | Location.Var _, Location.Loc _ -> LT
    | Location.Loc _, Location.Var _ -> GT
    | Location.Loc (x1, y1), Location.Loc(x2, y2) -> 
        if x1 = x2 
          then compare y1 y2
          else compare x1 x2
  static member refresh idProvider loc =
    match loc with 
    | Location.Loc  _        -> loc
    | Location.Var (-1, "_") -> loc
    | Location.Var (x, n)    -> Location.Var (idProvider n, n)
  static member wildcard = Location.Var (-1, "_")

type Hole<'s> = ('s * Location) list
and  Sub<'s, 'a when 's : equality> = Map<Var, Choice<Term<'s>, Location, 's, 'a>>   // this is the inner type of a Substitution; not ideal

(*********************
  Custom Logic Engine 
  
  A Custom Logic Engine (CLE) offers a programmatic interface that to domain specific elements into a general logic programming language with graph grammars.
  For example, Logic DSD is obtained by implementing a CLE for sites (domains + bonds); predicates, contexts and patterns are then obtained "for free". 
  
  The CLE is polymorphic in two parameters 's and 'a. 's stands for the main domain-specific species captured by the language (e.g. a site).
  'a stands for the sub-species that 's might contain (e.g. a domain or a bond). Roughly, 's and 'a correspond to the kind of logic variables that 
  are domain-specific (e.g. variable X in d!X is different from the same variable in N is X + 1).
**************************)
and  CLE<'s , 'a when 's : equality> = 
  { toString               : 's -> string                                // print 's
    toStringTempVar        : 'a -> string                                // print 'a
    compare                : 's -> 's -> int                             // compare two 's, used for sorting; the actual ordering is unimportant, as long as it is a partial order
    cast                   : 'a -> 's                                    // cast a 'a into a 's (e.g. a bond X can be cast into a site _!X); used in substitutions
    toCanonicalForm        : 's -> 's                                    // find the canonical form of 's
    toCanonicalFormProcess : Process<'s> -> Process<'s>                  // find the canonical form of a process. Since species 's might reference each other (e.g. as in bonds), it is necessary to have a Process level canonical form function
    unify                  : 's * 's -> Sub<'s, 'a> list                 // species unification. The core algorithm that find a 's and 'a variable substitution such that two species are equivalent after applying it
    underscore             : unit -> 's                                  // wildcard "_" for 's
    applyAll               : Sub<'s, 'a> -> Choice<'s, 'a> -> 's         // apply a substitution to a species or subspecies
    applySub               : Sub<'s, 'a> -> 's -> 's                     // apply a species substitution
    applySubVar            : Sub<'s, 'a> -> 'a -> 'a                     // apply a substitution to a subspecies
    fvs                    : 's -> Set<Variable<'s, 'a>>                 // free variables in 's
    refresh                : (string -> int) -> 's -> 's                 // provide a copy of 's where variables have been renamed by an ID provider. Used in resolution
    disambiguateVars       : Clause<'s> -> Clause<'s>                    // post-parsing step applied to each parsed clause. In Logic DSD this is used to disambiguate the use of domain variables (e.g. in P = C[D][D!i], D can be cast down to an unbound domain rather than a generic site)
    ComposeProcesses       : Process<'s> -> Process<'s> -> Process<'s>   // compose two processes together (possibly forming a complex in Logic DSD)
    resolveGoal            : (string * Term<'s> list) // a predicate     // species-specific predicates resolution. The core resolution algorithm that executes custom predicates for species (e.g. "compl(D, E)" is a custom predicate in Logic DSD to find the complement E of a domain D)
                              -> Sub<'s, 'a>          // the current unification solution theta
                              -> (Sub<'s, 'a> * Sub<'s, 'a>) list option // a list of expanded solutions theta', together with the solution sigma such that theta' = sigma \circ theta
    domainKeywords         : string list
  }
  with 
  static member empty : CLE<'s, 'a> = 
    { toString               = fun x   -> x.ToString()
      toStringTempVar        = fun x   -> x.ToString()
      cast                   = fun _   -> failwith "Internal error (CLE)" 
      compare                = fun _ _ -> failwith "Internal error (CLE)" 
      toCanonicalForm        = id 
      toCanonicalFormProcess = id 
      unify                  = fun _   -> failwith "Internal error (CLE)"
      underscore             = fun _   -> failwith "Internal error (CLE)"
      applyAll               = fun _ _ -> failwith "Internal error (CLE)"
      applySub               = fun _ _ -> failwith "Internal error (CLE)"
      applySubVar            = fun _ _ -> failwith "Internal error (CLE)"
      fvs                    = fun _   -> failwith "Internal error (CLE)"
      refresh                = fun _   -> failwith "Internal error (CLE)"
      disambiguateVars       = fun _   -> failwith "Internal error (CLE)"
      ComposeProcesses       = fun _ _ -> failwith "Internal error (CLE)"
      resolveGoal            = fun _ _ -> failwith "Internal error (CLE)"
      domainKeywords         = []
    }
and  
  [<DebuggerDisplay("{ToString(this)}")>]
  Pattern<'s when 's : equality> = 
  | Nicking     of Hole<'s> * Hole<'s>
  | Inner       of Hole<'s>
  | Strand      of Hole<'s>
  | ThreePrime  of Hole<'s>
  | FivePrime   of Hole<'s>
  | Nihil

  with
  static member ToStringWith (cle:CLE<'s, 'a>) (p:Pattern<'s>) =
    let printHole (x:Hole<'s>) = x 
                                |> List.map (fun (s, l) -> 
                                    match l with 
                                    | Location.Var (-1,"_") -> cle.toString s
                                    | _                -> sprintf "%s @ %s" (cle.toString s) (Location.ToString l))
                                |> String.concat " "
    match p with 
    | Nihil          -> "0"
    | Inner        s -> printHole s |> sprintf "%s"
    | Nicking (l, r) -> let left  = printHole l
                        let right = printHole r
                        sprintf "%s> | <%s" left right
    | Strand       s -> printHole s |> sprintf "<%s>"
    | ThreePrime   s -> printHole s |> sprintf "%s>"
    | FivePrime    s -> printHole s |> sprintf "<%s"

  static member ToString(p:Pattern<'s>) = Pattern.ToStringWith CLE.empty p
    

  static member Map(f : ('s * Location) -> ('t * Location)) (p:Pattern<'s>) =
     let g s = s |> List.map f
     match p with
     | Nihil            -> Nihil
     | Inner          s -> Inner (g s)
     | Nicking (s1, s2) -> Nicking (g s1, g s2)
     | Strand s         -> Strand (g s)
     | ThreePrime s     -> ThreePrime (g s)
     | FivePrime s      -> FivePrime (g s)
    
  static member Fold (f:'a -> ('s * Location) -> 'a) (acc : 'a) (h:Pattern<'s>) = 
    match h with
    | ThreePrime hole  -> List.fold f acc hole
    | FivePrime hole   -> List.fold f acc hole
    | Nicking (h1, h2) -> List.fold f (List.fold f acc h1) h2
    | Strand hole      -> List.fold f acc hole
    | Inner hole       -> List.fold f acc hole
    | Nihil            -> acc

  member this.Overlaps(that:Pattern<'s>) = 
    let holes (p:Pattern<'s>) =
      match p with
      | ThreePrime hole  -> [hole]
      | FivePrime hole   -> [hole]
      | Nicking (h1, h2) -> [h1; h2]
      | Strand hole      -> [hole]
      | Inner hole       -> [hole]
      | Nihil            -> []
    let toRange p (h:Hole<'s>) : Range = 
      match snd h.Head with
      | Location.Loc (sid, idx) -> (sid, (idx, idx+h.Length-1))
      | Location.Var _ -> failwith <| sprintf "Unexpected uninstantiated pattern %s" (Pattern.ToString p)
    let overlaps ((sid1, (x1, y1)) : Range) ((sid2, (x2, y2)) : Range) =
        sid1 = sid2
        && (x1 <= y2 && y1 >= x2)
      // all ranges are disjoint
    this
      |> holes 
      |> List.map (toRange this)
      |> List.exists (fun r1 -> 
        that
        |> holes 
        |> List.map (toRange that)
        |> List.exists (overlaps r1))

  static member Canonical (cle:CLE<'s, 'a>) (p:Pattern<'s>) = 
    p |> Pattern.Map (fun (x,y) -> (x |> cle.toCanonicalForm, y)) 

  static member refresh (cle:CLE<'s, 'a>) (idProvider : string -> int) (p : Pattern<'s>) =
    let refreshHole (sp, loc)  = 
      let loc' = Location.refresh idProvider  loc
      let sp'  = cle.refresh idProvider sp
      (sp', loc')
    match p with 
    | Pattern.Nihil -> Pattern.Nihil
    | Pattern.Inner hs          -> Pattern.Inner      (hs |> List.map refreshHole)
    | Pattern.ThreePrime hs     -> Pattern.ThreePrime (hs |> List.map refreshHole)
    | Pattern.FivePrime hs      -> Pattern.FivePrime  (hs |> List.map refreshHole)
    | Pattern.Strand hs         -> Pattern.Strand     (hs |> List.map refreshHole) 
    | Pattern.Nicking(hs1, hs2) -> let hs1' = hs1 |> List.map refreshHole
                                   let hs2' = hs2 |> List.map refreshHole
                                   Pattern.Nicking(hs1', hs2')

(* System *)
and Strand<'s> = 's list
and 
 [<DebuggerDisplay("{ToString(this)}")>]
 Process<'s when 's : equality> = Proc of Map<StrandID, Strand<'s>>
  with
    static member ToStringWith (cle:CLE<'s, 'a>) (Proc s:Process<'s>) =
      s |> Map.toList 
        |> List.map (snd 
                    >> List.map (cle.toString)
                    >> String.concat " " 
                    >> sprintf "<%s>")
        |> String.concat " | "
    static member ToString(p:Process<'s>) = Process.ToStringWith CLE.empty p
    static member Map(f:'s -> 't) (Proc s:Process<'s>) =
      Proc (s |> Map.map (fun _ -> List.map f))
    static member Fold(f:'State -> 's -> 'State) (acc:'State) (Proc s:Process<'s>)=
      s |> Map.fold (fun st _ x -> x |> List.fold f st) acc 
    static member ToList(Proc p:Process<'s>) = p |> Map.toList
                                             |> List.map snd
    static member OfList(ss:Strand<'s> list) = ss |> List.mapi (fun i x -> i, x)
                                              |> Map.ofList
                                              |> Process.Proc
    static member Compare (cle:CLE<'s, 'a>) (p:Process<'s>) (q:Process<'s>) : int =
      List.compareWith 
        (List.compareWith cle.compare)
        (p |> Process<'s>.ToList) 
        (q |> Process<'s>.ToList)
    
    
    static member Canonical (cle:CLE<'s, 'a>) (p:Process<'s>) : Process<'s> =
      cle.toCanonicalFormProcess p
    
    static member refresh (cle : CLE<'s, 'a>) (idProvider : string ->int) (p : Process<'s>) =
      p |> Process.Map (cle.refresh idProvider)

    (*****************************************************************)      
    (** End of processes canonical form ******************************)
    (*****************************************************************)
    
(* terms *)
and Mset<'s when 's : equality> = (int * Term<'s>) list
and 
  [<DebuggerDisplay("{ToString(this)}")>]
  Term<'s when 's : equality> = 
  (* standard logic programming terms *)
  | Var    of int * string            // variable ID (int), debug string (the string field can be erased in principle)
  | Const  of string                  // "some string"
  | Float  of float
  | Func   of string * Term<'s> list  // f(t1, ..., tn)
  | TList  of Term<'s> list           // [t1; t2; ...; tN]
  | TCons  of Term<'s> * Term<'s>     // [t1; ...; tK # Rest]    // TODO: change? E.g. to a "partial list" type TListp of (Term list * Var), where Var is the rest of the list
  | TCRN   of Term<'s> list           // a CRN with a set { R1, ... Rk } of k reactions. Each reaction is encoded as Prolog terms as Term.Func("_rxn", [ Term.TMSet; Term.Func("_massActionRate", [Term expression]); Term.TMSet])
  | TMSet  of Mset<'s>                // a multiset {| n1 t1, ... nk tk |}, where ni is the multiplicity of term ti. An example of a TMSet is 2a + 3b, which is written as {| 2 a, 3 b|} in multiset notation
                                      // The multiplicity ni must be a concrete integer number (e.g. 2 in "2 a + 3 b"), it cannot be a generic logic variable X currently
                                      (* TODO: possible future extensions: 
                                          - add undefined CRNs, e.g. {t1, ..., tk # X}, where X is some undefined set; X must be computed through unification
                                          - add undefined multisets, e.g. {| n1 t1, ..., nk tk # X |}, where X is the same as above
                                          - add undefined multiplicities, e.g. n1 can be either an integer or a variable X *)
  (* DSD specific terms *)
  | Proc    of Process<'s>
  | Pat     of Pattern<'s>
  with
  static member Wildcard : Term<'s>= Term.Var (-1, "_")
  static member FlattenList t =
    let rec flatten t = 
      match t with 
      | TCons (x, ts) -> 
          match flatten ts with
          | Choice1Of2 groundList -> Choice1Of2 (x :: groundList)
          | Choice2Of2 (leadingItems, listVar) -> Choice2Of2 (x::leadingItems, listVar)
      | TList x  -> Choice1Of2 x
      | Var(x,y)    -> Choice2Of2 ([], Var(x,y)) // unexpanded list that ends in a variable
      | _ -> failwith <| sprintf "Ill-formed list: %s" (Term<'s>.ToString t)
    flatten t
  static member ToStringWith (cle:CLE<'s, 'a>) (x:Term<'s>) =
    let printer = Term.ToStringWith cle
    match x with
    | Var(x,y)        -> printVar (x,y)
    | Const c         -> sprintf "\"%s\"" c
    | Float f         -> sprintf "%f" f
    | Func (f, ts)    -> let printRate r = match r with 
                                           | Term.Func("_functionalRate", [e])
                                           | Term.Func("_massActionRate", [e]) -> 
                                                if e = Term.Float 1.0 
                                                  then "" 
                                                  else match r with 
                                                       | Term.Func("_massActionRate", _) -> sprintf "{%s}" (e |> Term<'s>.ToExpression |> Microsoft.Research.CRNEngine.Expression.to_string (Term.ToStringWith cle))
                                                       | Term.Func("_functionalRate", _) -> sprintf "[%s]" (e |> Term<'s>.ToFuncExpression |> Microsoft.Research.CRNEngine.Expression.to_string (Microsoft.Research.CRNEngine.Key.to_string (Term.ToStringWith cle)))
                                                       | _ -> failwith ""
                                           | _ -> failwithf "Ill-formed rate %s" (Term.ToStringWith cle r)
                         match f with 
                         | "_ctx" -> match ts with 
                                     | [_; Var (x,y)]    -> sprintf "ctx(%s)" (printVar (x,y))
                                     | [_; TList hs] -> sprintf "ctx(%s)" (hs |> List.map printer |> String.concat "")
                                     | _             -> sprintf "Ill-formed context %s in" (printer x)
                         | "_rxn" -> match ts with 
                                     | [cata; mset1; r; bwr; mset2] ->
                                        let reactants = printer mset1
                                        let products  = printer mset2 
                                        let x = r
                                        let rate      = printRate r
                                        
                                        let hasCata   = cata <> Term.TMSet([])
                                        let catalysts = if hasCata then printer cata else ""
                                        
                                        let hasBwRate = bwr <> Term.Func("",[])
                                        let bwRate    = if hasBwRate then printRate bwr else ""
                                        
                                        match hasCata, hasBwRate with 
                                        | false, false -> sprintf "%s ->%s %s"         reactants rate products
                                        | true, false  -> sprintf "%s ~ %s ->%s %s"    catalysts reactants rate products
                                        | true, true   -> sprintf "%s ~ %s <->%s%s %s" catalysts reactants rate bwRate products
                                        | false, true  -> sprintf "%s <->%s%s %s"      reactants rate bwRate products

                                     | _ -> sprintf "Ill-formed reaction %s in" (printer x)
                         | _ -> sprintf "%s(%s)" f (ts |> List.map printer |> String.concat ", ")
    | TList ts        -> ts |> List.map printer |> String.concat "; " |> sprintf "[%s]" 
    | TCons _         -> match Term<'s>.FlattenList x with 
                         | Choice1Of2 groundList -> TList groundList |> printer
                         | Choice2Of2 (leadingItems, terminator) -> 
                              let leads = leadingItems 
                                          |> List.map printer 
                                          |> String.concat "; "
                              let trailer = terminator |> printer
                              sprintf "[%s # %s]" leads trailer
    | TCRN ts           -> ts |> List.map printer |> String.concat " | " |> sprintf "{ %s }"
    | TMSet ts          -> ts 
                           |> List.map (fun (i, t) -> (if i = 1 then printer t else sprintf "%i %s" i (printer t))) |> String.concat ":" // |> sprintf "_complex(%s)"

    | Pat p             -> Pattern.ToStringWith cle p
    | Proc s            -> Process.ToStringWith cle s

  static member ToString(x:Term<'s>) = Term<'s>.ToStringWith CLE.empty x
  
  static member FromExpression(e:Microsoft.Research.CRNEngine.Expression.t<Term<'s>>) : Term<'s> = 
    match e with 
    | Microsoft.Research.CRNEngine.Expression.Float    n  -> Term.Float n
    | Microsoft.Research.CRNEngine.Expression.Key      k  -> Term.Func("_key",      [k])
    | Microsoft.Research.CRNEngine.Expression.Times    xs -> Term.Func("_Times",    xs |> List.map Term.FromExpression)
    | Microsoft.Research.CRNEngine.Expression.Plus     xs -> Term.Func("_Plus",     xs |> List.map Term.FromExpression)
    | Microsoft.Research.CRNEngine.Expression.Divide   xs -> Term.Func("_Divide",   [Term.FromExpression xs.div1; Term.FromExpression xs.div2])
    | Microsoft.Research.CRNEngine.Expression.Power    xs -> Term.Func("_Power",    [Term.FromExpression xs.base_; Term.FromExpression xs.exponent])
    | Microsoft.Research.CRNEngine.Expression.Minus    xs -> Term.Func("_Minus",    [Term.FromExpression xs.sub1; Term.FromExpression xs.sub2])
    | Microsoft.Research.CRNEngine.Expression.Absolute x  -> Term.Func("_Absolute", [Term.FromExpression x])
    | Microsoft.Research.CRNEngine.Expression.Log      x  -> Term.Func("_Log",      [Term.FromExpression x])
    | Microsoft.Research.CRNEngine.Expression.Modulo   x  -> Term.Func("_Modulo",   [Term.FromExpression x.div; Term.FromExpression x.modulo])
    | Microsoft.Research.CRNEngine.Expression.Ceiling  x  -> Term.Func("_Ceiling",  [Term.FromExpression x])
    | Microsoft.Research.CRNEngine.Expression.Floor    x  -> Term.Func("_Floor",    [Term.FromExpression x])
    | Microsoft.Research.CRNEngine.Expression.Round    x  -> Term.Func("_Round",    [Term.FromExpression x])
    | Microsoft.Research.CRNEngine.Expression.If(x, y, z) -> failwith "Conditional expression are not supported yet."

  static member FromFuncExpression(e:Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Term<'s>>>) : Term<'s> = 
    match e with 
    | Microsoft.Research.CRNEngine.Expression.Float    n  -> Term.Float n
    | Microsoft.Research.CRNEngine.Expression.Key      k  -> match k with 
                                                             | Microsoft.Research.CRNEngine.Key.Species s    -> Term.Func("_key_species", [s])
                                                             | Microsoft.Research.CRNEngine.Key.Parameter p  -> Term.Func("_key_param",   [Term.Const p])
                                                             | Microsoft.Research.CRNEngine.Key.Rate r       -> Term.Func("_key_rate",    [Term.Const r])
                                                             | Microsoft.Research.CRNEngine.Key.Time         -> Term.Const "_time"
    | Microsoft.Research.CRNEngine.Expression.Times    xs -> Term.Func("_Times",    xs |> List.map Term.FromFuncExpression)
    | Microsoft.Research.CRNEngine.Expression.Plus     xs -> Term.Func("_Plus",     xs |> List.map Term.FromFuncExpression)
    | Microsoft.Research.CRNEngine.Expression.Divide   xs -> Term.Func("_Divide",   [Term.FromFuncExpression xs.div1; Term.FromFuncExpression xs.div2])
    | Microsoft.Research.CRNEngine.Expression.Power    xs -> Term.Func("_Power",    [Term.FromFuncExpression xs.base_; Term.FromFuncExpression xs.exponent])
    | Microsoft.Research.CRNEngine.Expression.Minus    xs -> Term.Func("_Minus",    [Term.FromFuncExpression xs.sub1; Term.FromFuncExpression xs.sub2])
    | Microsoft.Research.CRNEngine.Expression.Absolute x  -> Term.Func("_Absolute", [Term.FromFuncExpression x])
    | Microsoft.Research.CRNEngine.Expression.Log      x  -> Term.Func("_Log",      [Term.FromFuncExpression x])
    | Microsoft.Research.CRNEngine.Expression.Modulo   x  -> Term.Func("_Modulo",   [Term.FromFuncExpression x.div; Term.FromFuncExpression x.modulo])
    | Microsoft.Research.CRNEngine.Expression.Ceiling  x  -> Term.Func("_Ceiling",  [Term.FromFuncExpression x])
    | Microsoft.Research.CRNEngine.Expression.Floor    x  -> Term.Func("_Floor",    [Term.FromFuncExpression x])
    | Microsoft.Research.CRNEngine.Expression.Round    x  -> Term.Func("_Round",    [Term.FromFuncExpression x])
    | Microsoft.Research.CRNEngine.Expression.If(x, y, z) -> failwith "Conditional expression are not supported yet."

  
  static member IsExpression (e:Term<'s>) : bool = 
    match e with 
    | Term.Float _                            -> true
    | Term.Func("_key",      [_])             -> true
    | Term.Func("_key_species", [_])          -> true
    | Term.Func("_key_param",   [_])          -> true
    | Term.Func("_key_rate",    [_])          -> true
    | Term.Func("_Times",    ts)              -> (ts |> List.forall Term.IsExpression)
    | Term.Func("_Plus",     ts)              -> (ts |> List.forall Term.IsExpression)
    | Term.Func("_Divide",   [div1; div2])    -> Term.IsExpression div1 && Term.IsExpression div2
    | Term.Func("_Power",    [bas; exponent]) -> Term.IsExpression bas  && Term.IsExpression exponent
    | Term.Func("_Minus",    [sub1; sub2])    -> Term.IsExpression sub1 && Term.IsExpression sub2
    | Term.Func("_Absolute", [x])             -> Term.IsExpression x
    | Term.Func("_Log",      [x])             -> Term.IsExpression x
    | Term.Func("_Modulo",   [div; modulo])   -> Term.IsExpression div && Term.IsExpression modulo
    | Term.Func("_Ceiling",  [x])             -> Term.IsExpression x
    | Term.Func("_Floor",    [x])             -> Term.IsExpression x  
    | Term.Func("_Round",    [x])             -> Term.IsExpression x   
    | _   -> false

  static member ToExpression (e:Term<'s>) : Microsoft.Research.CRNEngine.Expression.t<Term<'s>> = 
    match e with 
    | Term.Float n                            -> Microsoft.Research.CRNEngine.Expression.Float    n
    | Term.Func("_key",      [Term.Float n])  -> Microsoft.Research.CRNEngine.Expression.Float    n
    | Term.Func("_key",      [k])             -> Microsoft.Research.CRNEngine.Expression.Key      k
    | Term.Func("_Times",    ts)              -> Microsoft.Research.CRNEngine.Expression.Times    (ts |> List.map Term.ToExpression)
    | Term.Func("_Plus",     ts)              -> Microsoft.Research.CRNEngine.Expression.Plus     (ts |> List.map Term.ToExpression)
    | Term.Func("_Divide",   [div1; div2])    -> Microsoft.Research.CRNEngine.Expression.Divide   {Microsoft.Research.CRNEngine.Expression.div1 = Term.ToExpression div1; Microsoft.Research.CRNEngine.Expression.div2 = Term.ToExpression div2}
    | Term.Func("_Power",    [bas; exponent]) -> Microsoft.Research.CRNEngine.Expression.Power    {Microsoft.Research.CRNEngine.Expression.base_ = Term.ToExpression bas; Microsoft.Research.CRNEngine.Expression.exponent = Term.ToExpression exponent}
    | Term.Func("_Minus",    [sub1; sub2])    -> Microsoft.Research.CRNEngine.Expression.Minus    {Microsoft.Research.CRNEngine.Expression.sub1 = Term.ToExpression sub1; Microsoft.Research.CRNEngine.Expression.sub2 = Term.ToExpression sub2}
    | Term.Func("_Absolute", [x])             -> Microsoft.Research.CRNEngine.Expression.Absolute (Term.ToExpression x)
    | Term.Func("_Log",      [x])             -> Microsoft.Research.CRNEngine.Expression.Log      (Term.ToExpression x)
    | Term.Func("_Modulo",   [div; modulo])   -> Microsoft.Research.CRNEngine.Expression.Modulo   {Microsoft.Research.CRNEngine.Expression.div = Term.ToExpression div; Microsoft.Research.CRNEngine.Expression.modulo = Term.ToExpression modulo }   
    | Term.Func("_Ceiling",  [x])             -> Microsoft.Research.CRNEngine.Expression.Ceiling  (Term.ToExpression x)
    | Term.Func("_Floor",    [x])             -> Microsoft.Research.CRNEngine.Expression.Floor    (Term.ToExpression x)  
    | Term.Func("_Round",    [x])             -> Microsoft.Research.CRNEngine.Expression.Round    (Term.ToExpression x)   
    | _   ->   failwithf "Conversion of term %s to expression not supported." (Term.ToString e)

  static member ToFuncExpression (e:Term<'s>) : Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Term<'s>>> = 
    match e with 
    | Term.Const "_time"                       -> Microsoft.Research.CRNEngine.Expression.Key      Microsoft.Research.CRNEngine.Key.Time
    | Term.Const s                             -> Microsoft.Research.CRNEngine.Expression.Key      (Microsoft.Research.CRNEngine.Key.Parameter s)
    | Term.Float n                             -> Microsoft.Research.CRNEngine.Expression.Float    n
    | Term.Func("_key",      [Term.Float n])   -> Microsoft.Research.CRNEngine.Expression.Float    n
    | Term.Func("_key",      [k])              -> Microsoft.Research.CRNEngine.Expression.Key      (Microsoft.Research.CRNEngine.Key.Species k)
    | Term.Func("_key_species",[s])            -> Microsoft.Research.CRNEngine.Expression.Key      (Microsoft.Research.CRNEngine.Key.Species s)
    | Term.Func("_key_param",  [Term.Const p]) -> Microsoft.Research.CRNEngine.Expression.Key      (Microsoft.Research.CRNEngine.Key.Parameter p)
    | Term.Func("_key_rate",   [Term.Const r]) -> Microsoft.Research.CRNEngine.Expression.Key      (Microsoft.Research.CRNEngine.Key.Rate r)
    | Term.Func("_Times",    ts)               -> Microsoft.Research.CRNEngine.Expression.Times    (ts |> List.map Term.ToFuncExpression)
    | Term.Func("_Plus",     ts)               -> Microsoft.Research.CRNEngine.Expression.Plus     (ts |> List.map Term.ToFuncExpression)
    | Term.Func("_Divide",   [div1; div2])     -> Microsoft.Research.CRNEngine.Expression.Divide   {Microsoft.Research.CRNEngine.Expression.div1 = Term.ToFuncExpression div1; Microsoft.Research.CRNEngine.Expression.div2 = Term.ToFuncExpression div2}
    | Term.Func("_Power",    [bas; exponent])  -> Microsoft.Research.CRNEngine.Expression.Power    {Microsoft.Research.CRNEngine.Expression.base_ = Term.ToFuncExpression bas; Microsoft.Research.CRNEngine.Expression.exponent = Term.ToFuncExpression exponent}
    | Term.Func("_Minus",    [sub1; sub2])     -> Microsoft.Research.CRNEngine.Expression.Minus    {Microsoft.Research.CRNEngine.Expression.sub1 = Term.ToFuncExpression sub1; Microsoft.Research.CRNEngine.Expression.sub2 = Term.ToFuncExpression sub2}
    | Term.Func("_Absolute", [x])              -> Microsoft.Research.CRNEngine.Expression.Absolute (Term.ToFuncExpression x)
    | Term.Func("_Log",      [x])              -> Microsoft.Research.CRNEngine.Expression.Log      (Term.ToFuncExpression x)
    | Term.Func("_Modulo",   [div; modulo])    -> Microsoft.Research.CRNEngine.Expression.Modulo   {Microsoft.Research.CRNEngine.Expression.div = Term.ToFuncExpression div; Microsoft.Research.CRNEngine.Expression.modulo = Term.ToFuncExpression modulo }   
    | Term.Func("_Ceiling",  [x])              -> Microsoft.Research.CRNEngine.Expression.Ceiling  (Term.ToFuncExpression x)
    | Term.Func("_Floor",    [x])              -> Microsoft.Research.CRNEngine.Expression.Floor    (Term.ToFuncExpression x)  
    | Term.Func("_Round",    [x])              -> Microsoft.Research.CRNEngine.Expression.Round    (Term.ToFuncExpression x)   
    | _   ->   failwithf "Conversion of term %s to expression not supported." (Term.ToString e)

  // decode a term into a CRN rate
  static member ToRate (t:Term<'s>) : Microsoft.Research.CRNEngine.Rate<Microsoft.Research.CRNEngine.Value, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Term<'s>>>> option = 
    let toMassActionRate (x:Term<'s>) = 
      let exp  = x |> Term.ToExpression
      let isConvertible = Microsoft.Research.CRNEngine.Expression.mentions exp 
                          |> List.forall (fun x -> match x with 
                                                   | Term.Const _                    -> true
                                                   | _                               -> false)
      if not isConvertible
        then None // TODO: throw an exception?
        else 
          exp |> Microsoft.Research.CRNEngine.Expression.map (fun x -> match x with 
                                                                       | Term.Const s -> s
                                                                       | _ -> failwith "")
              |> Microsoft.Research.CRNEngine.MassAction
              |> Some
          
    let toFunctionalRate (x:Term<'s>) = 
      let exp  = x |> Term.ToFuncExpression
      let isConvertible = Microsoft.Research.CRNEngine.Expression.mentions exp 
                          |> List.forall (fun x -> match x with 
                                                   | Microsoft.Research.CRNEngine.Key.Species (Term.Const _)
                                                   | Microsoft.Research.CRNEngine.Key.Species (Term.Pat (Pattern.Inner [_, _]))
                                                   | Microsoft.Research.CRNEngine.Key.Parameter _
                                                   | Microsoft.Research.CRNEngine.Key.Rate _ 
                                                   | Microsoft.Research.CRNEngine.Key.Time -> true
                                                   | _                               -> false)
      if not isConvertible
        then None 
        else 
          exp (*|> Expression.map (fun x -> match x with 
                                          | Key.Parameter s -> Key.Parameter s
                                          | Key.Rate r -> Key.Rate r
                                          | Key.Time -> Key.Time
                                          | Key.Species (Term.Const)
                                          | Term.Const s  -> Key.Parameter s
                                          | _             -> Key.Species x)*)
                                          //| Term.Proc p                      -> Key.Species p
                                          //| Term.Pat (Pattern.Inner [sp, _]) -> Key.Species (Process.OfList [[sp]])
                                          //| _ -> failwith "")
              |> Microsoft.Research.CRNEngine.Function
              |> Some
    match t with 
    | Term.Func  (fwRateKind, [fr]) -> 
      if not (Term.IsExpression fr)
        then None
        else match fwRateKind with 
             | "_massActionRate" -> toMassActionRate fr
             | "_functionalRate" -> toFunctionalRate fr
             | _                 -> None
    | _ -> None 

  // decode a term into a CRN reaction
  static member ToReactions (t:Term<'s>) : Microsoft.Research.CRNEngine.Reaction<Term<'s>, Microsoft.Research.CRNEngine.Value, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Term<'s>>>> list =
    let toReaction (t:Term<'s>)          : Microsoft.Research.CRNEngine.Reaction<Term<'s>, Microsoft.Research.CRNEngine.Value, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Term<'s>>>> option =
      match t with 
      | Term.Func ("_rxn", [ Term.TMSet catalysts
                           ; Term.TMSet reactants
                           ; fwr
                           ; bwr
                           ; Term.TMSet products]) -> 
        let fwRateOpt = Term.ToRate fwr
        
        match fwRateOpt with
        | None        -> None
        | Some fwRate ->  
            let bwRate = Term.ToRate bwr
            let toCrnMset (x:(int * Term<'s>) list) : Microsoft.Research.CRNEngine.Mset.t<Term<'s>> =
              x |> List.fold (fun acc (i, t) -> { Microsoft.Research.CRNEngine.Mset.element = t; 
                                                  Microsoft.Research.CRNEngine.Mset.multiplicity = i } :: acc) []
                    (*match accOpt with 
                    | None     -> None 
                    | Some acc -> match t with 
                                  | Term.Proc p                      -> Some ({ element = p; multiplicity = i } :: acc)
                                  | Term.Pat (Pattern.Inner [sp, _]) -> Some ({ element = Process.OfList [[sp]]; multiplicity = i } :: acc)
                                  // | Term.TMSet ts                    -> 
                                  | _ -> None) (Some [])*)
            let cata = toCrnMset catalysts
            let reac = toCrnMset reactants
            let prod = toCrnMset products
            // TODO: the compiler cannot link to Microsoft.Research.CRNEngine.Reaction.create for unknown reasons; substitute mkR with Reaction.create
            let mkR c r br fr p = { Microsoft.Research.CRNEngine.catalysts = c; 
                                    Microsoft.Research.CRNEngine.reactants = r;
                                    Microsoft.Research.CRNEngine.reverse   = br;
                                    Microsoft.Research.CRNEngine.rate      = fr;
                                    Microsoft.Research.CRNEngine.products  = p} 
            if cata.IsEmpty 
              then Some <| mkR Microsoft.Research.CRNEngine.Mset.empty reac bwRate fwRate prod
              else Some <| mkR cata reac bwRate fwRate prod
      | _ -> None 
    
    match t with 
    | Term.TCRN  ts  
    | Term.TList ts -> ts |> List.choose toReaction
    | _ -> match toReaction t with 
           | Some r -> [r]
           | None   -> []

  /// NB: Compare is only meant to be used in the normal form algorithm
  static member Compare (cle:CLE<'s, 'a>) (t1:Term<'s>) (t2:Term<'s>) =
        let termNumber t = 
          match t with
          | Term.Func   _ -> 0
          | Term.Const  _ -> 1
          | Term.Float  _ -> 2
          | Term.Pat    _ -> 3
          | Term.Proc   _ -> 4
          | Term.TCons  _ -> 5
          | Term.TList  _ -> 6
          | Term.Var    _ -> 7
          | Term.TCRN   _ -> 8
          | Term.TMSet  _ -> 9
        let n1, n2 = termNumber t1, termNumber t2
        if n1 < n2 then LT
        elif n1 > n2 then GT
        else (* same type*)
          match t1 , t2 with
          | Term.Func(name1, args1), Term.Func(name2,args2) -> 
            let nameComparison = String.Compare(name1, name2)
            if nameComparison < 0 then LT
            elif nameComparison > 0 then GT
            else List.zip args1 args2 
                 |> List.fold (fun acc (a1, a2) -> if acc = GT || acc = LT 
                                                     then acc 
                                                     else Term.Compare cle a1 a2) EQ
          | Term.Const name1, Term.Const name2 -> String.Compare(name1, name2)
          | Term.Float f1,    Term.Float f2    -> if f1 < f2 then LT
                                                    elif f1 > f2 then GT
                                                    else EQ
          | Term.Var (_,v1),  Term.Var (_,v2)          -> String.Compare(v1, v2)
          | Term.Pat p1,          Term.Pat p2          -> 
            match p1, p2 with
            | Pattern.Inner s1, Pattern.Inner s2 ->
              s2 |> List.compareWith (fun (x1, y1) (x2, y2) -> 
                      let cmp = cle.compare x1 x2
                      if cmp = EQ
                        then Location.Compare y1 y2
                        else cmp) s1
            | _, _ -> failwith "Tagging a domain with a pattern is not supported yet." 
          | Term.TList ts1,       Term.TList ts2       -> if ts1.IsEmpty && ts2.IsEmpty then EQ 
                                                            elif ts1.IsEmpty  then GT
                                                            else let t1 = ts1.Head 
                                                                 let t2 = ts2.Head
                                                                 let comp = Term<'s>.Compare cle t1 t2 
                                                                 if comp = EQ
                                                                  then Term.Compare cle (Term.TList ts1.Tail) (Term.TList ts2.Tail)
                                                                  else comp
          | Term.TCons(t1, ts1),  Term.TList ts2       -> if ts2.IsEmpty then LT 
                                                            else let t2 = ts2.Head 
                                                                 let comp = Term.Compare cle t1 t2 
                                                                 if comp = EQ
                                                                  then Term.Compare cle ts1 (Term.TList ts2.Tail) 
                                                                  else comp
          | Term.TList ts1,       Term.TCons (t2, ts2) -> if ts1.IsEmpty then LT 
                                                            else let t1 = ts1.Head 
                                                                 let comp = Term.Compare cle t1 t2 
                                                                 if comp = EQ
                                                                  then Term.Compare cle (Term.TList ts1.Tail) ts2
                                                                  else comp
          | Term.TCons(t1, ts1),  Term.TCons (t2, ts2) -> let comp = Term.Compare cle t1 t2 
                                                          if comp = EQ
                                                            then Term.Compare cle ts1 ts2
                                                            else comp
          | Term.TMSet ts1,       Term.TMSet ts2       -> match ts1, ts2 with  // Assumption: ts1 and ts2 are in canonical form, therefore sorted
                                                          | [], [] -> EQ
                                                          | [], _  -> LT
                                                          | _, []  -> GT 
                                                          | (n1, t1)::tail1, (n2, t2)::tail2 -> 
                                                            if n1 = n2 
                                                              then let x = Term.Compare cle t1 t2
                                                                   if x <> EQ then x else Term.Compare cle (Term.TMSet tail1) (Term.TMSet tail2)
                                                              else compare n1 n2
          | Term.TCRN ts1,        Term.TCRN ts2        -> Term.Compare cle (Term.TList ts1) (Term.TList ts2) // Assumption: ts1 and ts2 are in canonical form, therefore sorted
          | Term.Proc p1,         Term.Proc p2         -> Process.Compare cle p1 p2
          | _, _ -> failwith ""

  static member Canonical cle (t:Term<'s>) = 
    match t with 
    | Term.Func(x, ts)    -> Term<'s>.Func(x, ts |> List.map (Term<'s>.Canonical cle))
    | Term.Const  _       -> t
    | Term.Float  _       -> t
    | Term.Pat p          -> p |> fun z -> Pattern<'s>.Canonical cle z |> Term<'s>.Pat
    | Term.Proc p         -> p |> Process<'s>.Canonical cle |> Term<'s>.Proc
    | Term.TCons (t1, t2) -> (Term.Canonical cle t1, Term.Canonical cle t2) |> Term.TCons
    | Term.TList ts       -> ts |> List.map (Term.Canonical cle) |> Term.TList
    | Term.Var    _       -> t
    | Term.TMSet ts       -> ts 
                             |> List.map (fun (n, t) -> n, Term.Canonical cle t)
                             |> List.sortWith (fun (n1, t1) (n2, t2) -> let x = compare n1 n2
                                                                        if x <> EQ 
                                                                          then x 
                                                                          else Term.Compare cle t1 t2)
                             |> Term.TMSet
    | Term.TCRN ts        -> ts 
                             |> List.map (Term.Canonical cle)
                             |> List.sortWith (Term.Compare cle)
                             |> List.distinct
                             |> Term.TCRN

  static member refresh (cle:CLE<'s, 'a>) (idProvider : string -> int) (t:Term<'s>) = 
    match t with 
    | Term.Var (-1, "_")  -> t
    | Term.Var (x, y)     -> Term.Var (idProvider y, y)
    | Term.Func(x, ts)    -> Term<'s>.Func(x, ts |> List.map (Term.refresh cle idProvider))
    | Term.Const  _       -> t
    | Term.Float  _       -> t
    | Term.Pat p          -> Term.Pat  (p |> Pattern.refresh cle idProvider)
    | Term.Proc p         -> Term.Proc (p |> Process.refresh cle idProvider)
    | Term.TCons (t1, t2) -> let t1' = Term.refresh cle idProvider t1 
                             let t2' = Term.refresh cle idProvider t2 
                             Term.TCons (t1', t2')
    | Term.TList ts       -> Term<'s>.TList (ts |> List.map (Term.refresh cle idProvider))
    | Term.TCRN ts        -> Term<'s>.TCRN  (ts |> List.map (Term.refresh cle idProvider))
    | Term.TMSet ts       -> Term<'s>.TMSet (ts |> List.map (fun (i, t) -> i, t |> Term.refresh cle idProvider)) 

  static member Map (f:'s -> 't) (t:Term<'s>) : Term<'t> = 
    match t with 
    | Term.Var (-1, "_")  -> Term.Var (-1, "_")
    | Term.Var (x, y)     -> Term.Var (x, y)
    | Term.Func(x, ts)    -> Term<'t>.Func(x, ts |> List.map (Term.Map f) )
    | Term.Const x        -> Term.Const x
    | Term.Float x        -> Term.Float x
    | Term.Pat p          -> Term.Pat  (p |> Pattern.Map (fun (x,y) -> f x, y))
    | Term.Proc p         -> Term.Proc (p |> Process.Map f)
    | Term.TCons (t1, t2) -> let t1' = Term.Map f t1 
                             let t2' = Term.Map f t2 
                             Term.TCons (t1', t2')
    | Term.TList ts       -> Term<'t>.TList (ts |> List.map (Term.Map f))
    | Term.TCRN ts        -> Term<'t>.TCRN  (ts |> List.map (Term.Map f))
    | Term.TMSet ts       -> Term<'t>.TMSet (ts |> List.map (fun (i, t) -> i, t |> Term.Map f)) 
  
  static member wildcard : Term<'s> = Term.Var (-1, "_")

  static member Species (t:Term<'s>) : 's list = 
    let rec f x = 
      match x with 
      | Term.Var _    
      | Term.Const  _ 
      | Term.Float  _       -> []
      | Term.TCRN   ts
      | Term.Func (_, ts) 
      | Term.TList  ts      -> ts |> List.collect f
      | Term.TCons (t1, t2) -> f t1 @ f t2 
      | Term.TMSet  s       -> s |> List.collect (snd >> f)
      | Term.Proc   p       -> p |> Process.ToList |> List.concat
      | Term.Pat    p       -> p |> Pattern.Fold (fun acc (x, _) -> x::acc) []
    f t |> List.distinct
(* predicates *)
and 
  [<DebuggerDisplay("{this.String}")>]
  Predicate<'s when 's : equality> = Pred of string * Term<'s> list
  with
  static member ToStringWith cle (Pred (p, ts)) = sprintf "%s(%s)" p (ts |> List.map (Term<'s>.ToStringWith cle)
                                                                         |> String.concat ", ")
  static member ToString (Pred (p, ts)) = Predicate.ToStringWith CLE.empty (Pred (p, ts)) 
  member this.String = Predicate.ToString this
  member this.Name   = match this with Pred (n,_) -> n
  member this.Args   = match this with Pred (_,a) -> a
  static member refresh cle idProvider (Pred (n, ts)) = Pred (n, ts |> List.map (Term.refresh cle idProvider))
  static member Map (f:Term<'s> -> Term<'s>) (Pred (p, ts):Predicate<'s>) = Pred (p, ts |> List.map f)
  static member Species (Pred (_, ts):Predicate<'s>) = ts |> List.collect Term.Species

(* formulas *)
and 
  [<DebuggerDisplay("{this.String}")>]
  Literal<'s when 's : equality> = Pos of Predicate<'s>
                                 | Neg of Predicate<'s>
  with 
    static member ToStringWith cle (l:Literal<'s>) =
      match l with 
      | Pos p -> Predicate.ToStringWith cle p
      | Neg p -> sprintf "not %s" (Predicate.ToStringWith cle p)
    static member ToString(l:Literal<'s>) = Literal.ToStringWith CLE.empty l
    member this.String = Literal<'s>.ToString this

    static member Map (f:Term<'s> -> Term<'s>) (l:Literal<'s>) = match l with   
                                                                 | Pos p -> Pos (p |> Predicate<'s>.Map f)
                                                                 | Neg p -> Neg (p |> Predicate<'s>.Map f)
    static member Species (l:Literal<'s>) = 
      match l with 
      | Pos p 
      | Neg p -> Predicate.Species p
    static member refresh cle idProvider (l:Literal<'s>) = 
      match l with 
      | Pos p -> Pos (Predicate.refresh cle idProvider p)
      | Neg p -> Neg (Predicate.refresh cle idProvider p)
(*
type Formula  = Atom    of Predicate<'s>
              | Not     of Formula
              | And     of Formula * Formula
              | Or      of Formula * Formula
              | Implies of Formula * Formula
              | Iff     of Formula * Formula
              | ForAll  of Variable * Formula
              | Exists  of Variable * Formula
              *)

(* logic programs *)
and
  [<DebuggerDisplay("{this.String}")>] 
  Clause<'s when 's : equality> = { head: Predicate<'s>
                                    body: Literal<'s> list } with
  static member Create(p:Predicate<'s>, bs:Literal<'s> list) = {head=p; body=bs}
  static member Create(p:Predicate<'s>)                  = {head=p; body=[]}
  static member Map (f:Term<'s> -> Term<'s>) (c:Clause<'s>) = { head = c.head |> Predicate<'s>.Map f
                                                                body = c.body |> List.map (Literal<'s>.Map f)}
  static member Species (c:Clause<'s>) = Predicate.Species(c.head) @ (c.body |> List.collect Literal.Species) |> List.distinct

  static member ToStringWith cle (c:Clause<'s>) = 
    if c.body.IsEmpty
      then sprintf "%s." (Predicate.ToStringWith cle c.head)
      else sprintf "%s :- %s." (Predicate.ToStringWith cle  c.head)
                               (c.body |> List.map (Literal.ToStringWith cle) |> String.concat ", ") 
  static member ToString(c:Clause<'s>) = Clause.ToStringWith CLE.empty c
  member this.String = Clause<'s>.ToString this

type Signature = string * int // the name and the number of arguments of a predicate (the head of a Horn clause in the program)
type RulesProgram<'s when 's : comparison> = System.Collections.Generic.Dictionary<Signature, Set<Clause<'s>>>
let toProgram p = 
    p 
    |> (List.fold (fun (acc:Map<Signature, Set<Clause<'s>>>) clause -> 
                  let name = clause.head.Name 
                  let args = clause.head.Args
                  let key  = name, args.Length
                  if acc.ContainsKey(key)
                    then let set = acc.[key]
                         acc.Add(key, Set.add clause set)
                    else acc.Add(key, Set.singleton(clause))
                ) Map.empty
                >> Map.toSeq
                >> fun zz -> let x = new System.Collections.Generic.Dictionary<Signature, Set<Clause<'s>>>()
                             for k,v in zz do
                               x.Add(k,v)
                             x)
