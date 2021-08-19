[<JavaScript>]
module Microsoft.Research.GEC.LogicGEC

open RulesDSD.Syntax
open RulesDSD.Substitution
open RulesDSD.Resolution
open Microsoft.Research.CRNEngine
open Parser

// variable, wildcard or identifier parser
let pname = (name .>> spaces) <|> kw "_"
let noTypePart = "%NoTypePart"


type Element = Part     of Part   // e.g. r0040::prom<neg(tetR)>
             | Var      of Var    // X
 with static member wildcard = Var (-1, "_")
      static member doParse domainKeywords postParsingDisambigation idProvider : Parser.t<Element> = 
        Parser.plookAheadWith (pname >>= fun str ->
        if domainKeywords |> List.contains str
          then preturn None
          else choice [ kw "("  >>. preturn None
                        kw "["  >>. preturn None 
                        kw "[[" >>. preturn None 
                        kw "::" >>. preturn (Some true)
                        preturn (if str = "_" || System.Char.IsUpper (str.Chars 0) then Some false else Some true) ])  
        >>= fun isPart -> 
          match isPart with
          | None            -> failParser ""
          | Some isConcrete -> if isConcrete 
                                then Part.doParse idProvider domainKeywords postParsingDisambigation |>> Element.Part
                                else pname |>> fun x -> Element.Var (idProvider x)
      static member parse domainKeywords idProvider = Element.doParse idProvider false domainKeywords 
      static member ToString e = 
        match e with 
        | Element.Var v ->  printVar v
        | Element.Part p -> Part.ToString p
and Part = { name  : Term<Element>   // either a variable or a string. CS: I set Element as parametric value just to make unification and resolution more uniform
           ; type_ : Term<Element> } // either a variable or a string 
 with static member Create (name : Term<Element>, type_ : Term<Element>) = { name  = name; type_ = type_ }
      static member CreateByName (name:string)  = Part.Create(Term.Const name, Term.wildcard)
      static member CreateByType (type_:string) = Part.Create(Term.wildcard,   Term.Const type_)
      static member doParse idProvider domainKeywords postParsingDisambiguation : Parser.t<Part> = 
        Parser.plookAheadWith (pname >>= fun str ->
          if domainKeywords |> List.contains str
            then preturn None
            else choice [ kw "("  >>. preturn None
                          kw "["  >>. preturn None
                          kw "[[" >>. preturn None
                          kw "::" >>. preturn (Some true)
                          preturn (Some false)]) 
          >>= fun isPart -> 
            match isPart with
            | None -> failParser ""
            | Some isConcrete -> 
                let toTerm (s) : Term<Element> = 
                  if s = "_" then Term.wildcard
                    elif System.Char.IsUpper (s.Chars 0)
                      then idProvider s |> Term.Var
                    else Term.Const s
                // let pterm = RulesDSD.Parser.pterm (Element.parse domainKeywords idProvider) idProvider domainKeywords
                
                pname >>= fun partName -> 
                  let nameTerm = toTerm partName
                  if isConcrete 
                    then kw "::" >>. pname >>= fun partType ->
                      let typeTerm = toTerm partType
                      preturn (Part.Create(nameTerm, typeTerm) ) 
                    else preturn (Part.Create(nameTerm, if postParsingDisambiguation then Term.Const noTypePart
                                                                                     else Term.wildcard))
      static member parse idProvider domainKeywords : Parser.t<Part> = Part.doParse idProvider domainKeywords false
      static member ToString p =  // TODO: add cle
        let stringTerm t = 
          match t with 
          | Term.Const x -> x
          | _ -> Term.ToString t
        stringTerm p.name + "::" + stringTerm p.type_
type Engine    = RulesDSD.Syntax.CLE<Element, unit>
type Semantics = RulesDSD.Syntax.RulesProgram<Element>

// find all concrete parts mentioned in the semantics (concrete meaning no open Prolog variable, each term is ground)
let getParts (s:Semantics) : Part list =
  s.Values 
  |> Seq.collect(Set.toList >> List.collect Clause.Species >> List.distinct)
  |> Seq.distinct
  |> Seq.choose (fun e -> 
      match e with 
      | Element.Part p -> match p.name, p.type_ with 
                          | Term.Var _, _
                          | _, Term.Var _  -> None
                          | _              -> Some p
      | Element.Var _ -> None)
  |> Seq.toList
  
                  
let PREDEFINED_PART_TYPE_NAMES = ["ter"; "prom"; "rbs"; "pcr"; "cds"] 
let PREDEFINED_PART_TYPES : Term<Element> list = PREDEFINED_PART_TYPE_NAMES |> List.map Term.Const

type Rate = Term<Element>
type Complex = Element Mset
let toComplex x : Complex = [1, x]
let mapComplex (f:Element -> Element) (c:Complex) : Complex = 
  // let g = Process.Map f 
  let g = Term.Map f 
  c |> List.map (fun (i, t) -> i, t |> Term.Map f) //  |> Term.Map g)


let termToDevice cle t : Element list = 
  let err x = failwithf "Unexpected term %s is not a device (a list of parts <p1 ... pN>)." (Term.ToStringWith cle x)
  match t with 
  | Term.Proc p -> match Process.ToList p with 
                   | [x] -> x
                   | _   -> err t
  | _   -> err t


type Instruction = Device     of Element list
                 | Constraint of Literal<Element>
                 | Reaction   of Complex option * Complex * Complex * Rate * Rate option // cata, react, prod, rate, bw rate
                 | Initial    of Term<Element> * Complex // concentration, molecule
  with 
    static member Map (f:Element -> Element) i = 
      match i with 
      | Device d              -> d |> List.map f |> Device
      | Constraint c          -> c |> Literal.Map (Term.Map f) |> Constraint
      | Reaction (a,b,c,d,e)  -> let g = mapComplex f
                                 let h = Rate.Map f
                                 Reaction (a |> Option.map g, g b, g c, h d, Option.map h e)
      | Initial (pop, t)      -> Initial ((pop |> Term.Map f), mapComplex f t)
    static member Species i : Element list = 
      let f  = List.collect (snd >> Term.Species)
      match i with 
      | Device d              -> d
      | Constraint c          -> Literal.Species c
      | Reaction (a,b,c,d,e)  -> let s1 = a |> Option.toList |> List.collect f
                                 let s2 = f b
                                 let s3 = f c
                                 
                                 let s4 = d |> Term.Species
                                 let s5 = e |> Option.toList |> List.collect Term.Species
                                 s1 @ s2 @ s3 @ s4 @ s5 |> List.distinct

      | Initial (pop, t)      -> (Term.Species pop @ f t) |> List.distinct
    static member Apply (cle:CLE<Element, unit>) (Sub s:Substitution<Element, unit>) i =
      match i with 
      | Device d             -> Device (d |> List.map (fun x -> cle.applySub s x))
      | Constraint c         -> Constraint (c |> applyL cle (Sub s))
      | Reaction (a,b,c,d,e) -> failwith "Reactions not supported yet"
      | Initial (pop, t)     -> let pop' = Substitution.Apply(Sub s, pop, cle)
                                let t'   = t |> List.map (fun (i,x) -> i, Substitution.Apply(Sub s, x, cle))
                                Initial (pop', t')
    static member ToString (cle:CLE<Element, unit>) i : string = 
      let printDevice d = d |> List.map Element.ToString |> String.concat " " |> sprintf "<%s>"
      match i with 
      | Instruction.Device d     -> printDevice d
      | Instruction.Constraint c -> Literal.ToStringWith cle c
      | Reaction (a,b,c,d,e)     -> failwith "Reactions not supported yet"
      | Initial (pop, t)         -> sprintf "%s %s" (Term.ToStringWith cle pop) (Term.ToStringWith cle (TMSet t))

    static member GetParts (i:Instruction) = 
      let f e = match e with 
                | Element.Part p -> match p.name, p.type_ with 
                                    | Term.Var _, _
                                    | _, Term.Var _  -> None
                                    | _              -> Some p
                | Element.Var _ -> None
      
      match i with 
      | Device d                    -> d |> List.choose f
      | Constraint c                -> c |> Literal.Species |> List.choose f
      | Initial (c, i)  -> let x = c |> Term.Species |> List.choose f
                           let y = i |> List.collect (snd >> Term.Species >> List.choose f)
                           x @ y
      | Reaction _      -> failwith "Reactions are not supported yet"
      |> List.distinct

// Disambiguation step:
// if X is both a term var and an element var, turn Term.Var X into Term.Pattern [Pattern.Inner [Element.Var X, Locatiom.wildcard]]
// create var to assignments map
let rec updateMap (m:Map<Var, bool * bool * Term<Element> option * bool * bool>)  // what has v been used for so far? As a term, a part, a part name (store the part type if so, so that var and part type can reconstruvct the full type), a part type, a location?
              (v:Var)
              (arg:Choice<unit, unit, Term<Element>, unit, unit>) =  // what is v being used for?
  if m.ContainsKey v
    then m.Add (v, (match m.[v] with 
                     (a,b,c,d,e) -> 
                      match arg with 
                      | Choice1Of5 _ -> (true,b,c,d,e)
                      | Choice2Of5 _ -> (a,true,c,d,e)
                      | Choice3Of5 t -> 
                        // TODO: check if c or t are Term.wildcard
                        (a,b,Some t,d,e)
                      | Choice4Of5 _ -> (a,b,c,true,e) 
                      | Choice5Of5 _ -> (a,b,c,d,true) ))
    else m.Add (v, match arg with 
                   | Choice1Of5 _ -> (true,false,None,false,false)
                   | Choice2Of5 _ -> (false,true,None,false,false)
                   | Choice3Of5 t -> (false,false,Some t,false,false)
                   | Choice4Of5 _ -> (false,false,None,true,false)
                   | Choice5Of5 _ -> (false,false,None,false,true)
    )
// 
and getMapElement m (e:Element) =
  match e with 
  | Element.Var v -> updateMap m v (Choice2Of5 ())
  | Element.Part p -> 
    let m' = match p.name with 
             | Term.Var (i,v) -> updateMap m (i, v) (Choice3Of5 p.type_)
             | _              -> getMapTerm m p.name      
    match p.type_ with 
    | Term.Var (i,v) -> updateMap m' (i, v) (Choice4Of5 ())
    | _              -> getMapTerm m' p.name      

and getMapLocation m l = 
  match l with 
  | Location.Var (x, y) -> let v = (x,y) in updateMap m v (Choice5Of5 ())
  | Location.Loc _      -> m

and getMapHole m (s:Element, l:Location) = getMapLocation (getMapElement m s) l

and getMapPattern m = 
  function 
  | Pattern.Inner hs          -> hs  |> List.fold getMapHole m
  | Pattern.Nihil             -> m   
  | Pattern.FivePrime hs      -> hs  |> List.fold getMapHole m
  | Pattern.ThreePrime hs     -> hs  |> List.fold getMapHole m
  | Pattern.Nicking(hs1, hs2) -> let m' = hs1 |> List.fold getMapHole m
                                 hs2 |> List.fold getMapHole m'
  | Pattern.Strand hs         -> hs  |> List.fold getMapHole m

and getMapProcess m =
  function 
  | Process.Proc strandsMap -> strandsMap 
                               |> Map.toList 
                               |> List.map snd 
                               |> List.fold (fun acc s -> s |> List.fold getMapElement acc) m
and getMapComplex m (c:Complex) =
  c |> List.fold (fun acc x -> getMapTerm acc (snd x)) m 
and getMapTerm m = 
  function 
  | Term.Var (x, y)  -> updateMap m (x, y) (Choice1Of5 ())
  | Term.Const _ 
  | Term.Float _     -> m
  | Func   (_, ts) 
  | TList  ts        -> ts |> List.fold getMapTerm m
  | TCons  (t1, t2)  -> let m' = getMapTerm m t1 
                        getMapTerm m' t2
  | Proc p           -> getMapProcess m p
  | Pat  p           -> getMapPattern m p
  | Term.TMSet ts    -> ts |> List.fold (fun acc (_,v) -> getMapTerm acc v) m
  | Term.TCRN ts     -> ts |> List.fold getMapTerm m

and getMapPredicate m = function 
                        | Predicate.Pred(_, args) -> args |> List.fold getMapTerm m
                      
and getMapLit m = function 
                  | Pos p -> getMapPredicate m p
                  | Neg p -> getMapPredicate m p

(*
let zzz (cle:Engine) m xmap xapply =
  m 
  |>
  Map.fold (fun x varName assignments -> 
  // disambiguate each variable
  if varName = (-1, "_") 
    then x // skip wildcards
    else 
  match assignments with 
  // skip unambiguous variables
  | false, false, None,   false, false 
  | true,  false, None,   false, false 
  | false, true,  None,   false, false 
  | false, false, Some _, false, false 
  | false, false, None,   true,  false 
  | false, false, None,   false, true  -> x
  | _(*hasTVar*), hasEVar, nameVar, hasTypeVar, hasLocVar ->
    let hasNameVar = Option.isSome nameVar
    let p'  = match nameVar with 
              | Some ty -> { name = Term.Var varName; type_ = ty }
              | None    -> { name = Term.wildcard;    type_ = Term.Var varName }
    if hasLocVar 
      then
        if hasEVar || hasNameVar || hasTypeVar
          then  failwith <| sprintf "Cannot unify location variable \"%s\" with a %s variable" (snd varName)
                              (if   hasTypeVar then "part type"
                               elif hasNameVar then "part name"
                               else                 "part")
          else // must be TVar too
            let pattern = Term.Pat <| Pattern.Inner [Element.Var (-1,"_"), Location.Var varName]
            let sub     = Substitution<Element,_>.Create(varName, pattern).Add(varName, Choice2Of4 <| Location.Var varName, cle)
            xapply x sub
    // disambiguate variable <varName>
    elif hasNameVar || hasTypeVar
      then 
        if hasEVar 
          then 
            // Consider this example:   P = C[X], P = [X::Y]  
            // Disambiguation interprets X in the first term a part with name X and type Y
            x 
            |> xmap (fun t -> 
              t 
              |> Term.Map (fun e -> 
                match e with 
                | Element.Var (x,y) -> if (x,y) = varName then Part p' else e
                | Element.Part p''  -> Part <| if hasNameVar && p''.name = p'.name then p' else p''
            ))
        else x // if it's a TVar, nothing to do 
    else // not a LocVar, NameVar or TypeVar: must be a TVar and EVar
      let rec f (term:Term<Element>) = 
        match term with 
        | Term.Var (x,y)      -> if (x,y) = varName 
                                  then Term.Pat <| Pattern.Inner [Part p', Location.wildcard] 
                                  else term
        | Term.Const _ 
        | Term.Float _ 
        | Term.Pat   _ 
        | Term.Proc  _        -> term
        | Term.Func  (n, ts)  -> Term.Func (n, ts |> List.map f)
        | Term.TCons (t1, t2) -> Term.TCons (f t1, f t2) 
        | Term.TList ts       -> Term.TList (ts |> List.map f)
        | Term.TCRN  ts       -> Term.TCRN  (ts |> List.map f)
        | Term.TMSet ts       -> Term.TMSet (ts |> List.map (fun (i, x) -> i, f x))
      x |> xmap f 
  )
*)

///////////////////////////////////
// Logic GEC Custom Logic Engine //
///////////////////////////////////
and fvPart (p:Part) = 
  [p.name; p.type_]
  |> List.map (fvt cle) 
  |> Set.unionMany

and fvElement (e:Element) =
  let rec fv t = 
    match t with 
    | Term.Var (-1, "_") -> Set.empty
    | Term.Var (x,y)     -> Set.singleton (TVar (x,y))
    | Term.Const _       -> Set.empty
    | Term.Float _       -> Set.empty
    | Term.Func (_, ts)  -> ts |> unionFold fv
    | Term.TList ts      -> ts |> unionFold fv
    | Term.TCons (t,t')  -> [t;t'] |> unionFold fv
    | Term.Pat _         -> Set.empty  // p |> fvPattern cle
    | Term.Proc _        -> Set.empty  // TODO
    | Term.TCRN ts       -> ts |> unionFold fv
    | Term.TMSet ts      -> ts |> List.map snd |> unionFold fv
  match e with 
  | Part p     -> fvPart p
  | Var x      -> if x = (-1, "_") then Set.empty else Set.singleton (SVar (x, e))

and  elemToString (x : Element) : string = 
  let toString t   = match t with 
                     | Term.Const x -> x 
                     | _            -> Term.ToStringWith cle t
  match x with 
  | Part p     -> let typ = match p.type_ with 
                            | Term.Var (-1, "_") -> ""
                            | _                  -> " : " + toString p.type_
                  toString p.name // + typ
  | Var (_, y) -> y

and elemCompare (x:Element) (y:Element) = 
  match x, y with 
  | Part _,     Var _        -> RulesDSD.Syntax.LT
  | Var _,      Part _       -> RulesDSD.Syntax.GT
  | Part p1,    Part p2      -> Term.Compare cle p1.name p2.name
  | Var (v1, _), Var (v2, _) -> compare v1 v2

and elemCanonicalForm (x:Element) = x

and elemUnderscore () = Var (-1, "_")

and elemRefresh (idProvider : string -> int) e = 
  match e with 
  | Part p     -> 
     let refresher  = Term.refresh cle idProvider 
     let n  = refresher p.name 
     let t  = refresher p.type_
     
     Part (Part.Create(n, t))
  | Var (_, y) -> Var (idProvider y, y)

and unimplemented _ = failwith ""

and deviceComposition p q = Process.ToList p @ Process.ToList q |> Process.OfList

and elemResolveGoal (p, args) _ = 
  match (p, args) with 
  | _ -> None


and elemDisambiguateVarsWith m (c:Clause<Element>) : Clause<Element> =
  // TODO: is this still necessary?
  
  // remove "noTypePart" types from parts (in patterns and processes)
  let g(e:Element) = 
    match e with 
    | Element.Var _ -> e
    | Element.Part p -> if p.type_ = Term.Const noTypePart 
                          then { p with type_ = Term.wildcard }
                          else p
                        |> Element.Part
  
  // turn all other "noTypePart" parts into (Term.Const part.name)
  let rec f (e:Term<Element>) = 
    match e with 
    | Term.Pat (Pattern.Inner [Element.Part ({ name = n; type_ = Term.Const "%NoTypePart"}), Location.Var (-1, "_")]) -> n
    | Term.Float _ 
    | Term.Const _
    | Term.Var _ -> e
    | Term.Pat p -> p |> Pattern.Map (fun (x, y) -> g x, y) |> Term.Pat
    | Term.Proc p -> p |> RulesDSD.Syntax.Process.Map g
                       |> Term.Proc
    | Term.TList ts       -> ts |> List.map f |> Term.TList 
    | Term.TCons (t1, t2) -> Term.TCons (f t1, f t2)
    | Term.TCRN ts        -> ts |> List.map f |> Term.TCRN
    | Term.TMSet ts       -> ts |> List.map (fun (i, t) -> i, f t) |> Term.TMSet
    | Term.Func (n, ts)   -> Term.Func (n, ts |> List.map f)

  let c' = c |> Clause.Map f
  match c'.head with
  | Pred (_, lits) -> 
    // collect how each variable is used
    c'.body 
    |> List.fold getMapLit (lits |> List.fold getMapTerm Map.empty) 
    // m
    |> Map.fold (fun (clause:Clause<Element>) varName assignments -> 
      // disambiguate each variable
      if varName = (-1, "_") 
        then clause // skip wildcards
        else 
      match assignments with 
      // skip unambiguous variables
      | false, false, None,   false, false 
      | true,  false, None,   false, false 
      | false, true,  None,   false, false 
      | false, false, Some _, false, false 
      | false, false, None,   true,  false 
      | false, false, None,   false, true  -> clause
      | _(*hasTVar*), hasEVar, nameVar, hasTypeVar, hasLocVar ->
        let hasNameVar = Option.isSome nameVar
        let p'  = match nameVar with 
                  | Some ty -> { name = Term.Var varName; type_ = ty }
                  | None    -> { name = Term.wildcard;    type_ = Term.Var varName }
        if hasLocVar 
          then
            if hasEVar || hasNameVar || hasTypeVar
              then  failwith <| sprintf "Cannot unify location variable \"%s\" with a %s variable" (snd varName)
                                  (if   hasTypeVar then "part type"
                                   elif hasNameVar then "part name"
                                   else                 "part")
              else // must be TVar too
                let pattern = Term.Pat <| Pattern.Inner [Element.Var (-1,"_"), Location.Var varName]
                let sub     = Substitution<Element,_>.Create(varName, pattern).Add(varName, Choice2Of4 <| Location.Var varName, cle)
                sub.Apply(clause,cle)
        // disambiguate variable <varName>
        elif hasNameVar || hasTypeVar
          then 
            if hasEVar 
              then 
                // Consider this example:   P = C[X], P = [X::Y]  
                // Disambiguation interprets X in the first term a part with name X and type Y
                clause 
                |> Clause<Element>.Map (fun t -> 
                  t 
                  |> Term.Map (fun e -> 
                    match e with 
                    | Element.Var (x,y) -> if (x,y) = varName then Part p' else e
                    | Element.Part p''  -> Part <| if hasNameVar && p''.name = p'.name then p' else p''
                ))
            else clause // if it's a TVar, nothing to do 
        else // not a LocVar, NameVar or TypeVar: must be a TVar and EVar
          let rec f (term:Term<Element>) = 
            match term with 
            | Term.Var (x,y)      -> if (x,y) = varName 
                                      then Term.Pat <| Pattern.Inner [Part p', Location.wildcard] 
                                      else term
            | Term.Const _ 
            | Term.Float _ 
            | Term.Pat   _ 
            | Term.Proc  _        -> term
            | Term.Func  (n, ts)  -> Term.Func (n, ts |> List.map f)
            | Term.TCons (t1, t2) -> Term.TCons (f t1, f t2) 
            | Term.TList ts       -> Term.TList (ts |> List.map f)
            | Term.TCRN  ts       -> Term.TCRN  (ts |> List.map f)
            | Term.TMSet ts       -> Term.TMSet (ts |> List.map (fun (i, x) -> i, f x))
          clause |> Clause.Map f 
     ) c'
     //) c

and elemDisambiguateVars (c:Clause<Element>) : Clause<Element> = 
  let m = match c.head with Pred (_, lits) -> c.body |> List.fold getMapLit (lits |> List.fold getMapTerm Map.empty) 
  elemDisambiguateVarsWith m c

and elemApplySub (theta:Sub<Element, unit>) (e:Element) : Element = 
  match e with 
  | Var (-1, "_") -> e
  | Var v         -> if theta.ContainsKey v
                      then match theta.[v] with 
                           | Choice1Of4 _ -> failwith ""
                           | Choice2Of4 _ -> failwith ""
                           | Choice3Of4 x -> x
                           | Choice4Of4 _ -> failwith ""
                      else e
  | Part p        -> let applier (x:Term<Element>) = Substitution.Apply(Sub theta, x, cle)
                     let n  = applier p.name 
                     let t  = applier p.type_
                     Part (Part.Create(n, t))

and elemApplyAll (theta:Sub<Element, unit>) (x:Choice<Element, unit>) = 
  match x with 
  | Choice1Of2 e  -> elemApplySub theta e
  | Choice2Of2 () -> failwith ""

and elemUnify (x:Element, y:Element) : Sub<Element, unit> list = 
  match x, y with 
  | Part p,     Var x   
  | Var x,      Part p      -> [Map.ofList [x, Choice3Of4 <| Part p]]
  | Var x,      Var y       -> [Map.ofList [y, Choice3Of4 <| Var x]]
  | Part p1,  Part p2  -> 
    let eq1 = RulesDSD.Unification.TEq (p1.name,  p2.name)
    let eq2 = RulesDSD.Unification.TEq (p1.type_, p2.type_)
    let eqs = [eq1; eq2]
    RulesDSD.Unification.unify cle eqs 

      |> List.map (fun (Sub x) -> x)

and cle : Engine = 
  { toString               = elemToString           // print 's
    toStringTempVar        = unimplemented          // print 'a
    compare                = elemCompare            // compare two 's, used for sorting; the actual ordering is unimportant, as long as it is a partial order
    cast                   = unimplemented          // cast a 'a into a 's (e.g. a bond X can be cast into a site _!X); used in substitutions
    toCanonicalForm        = elemCanonicalForm      // find the canonical form of 's
    toCanonicalFormProcess = id                     // find the canonical form of a process. Since species 's might reference each other (e.g. as in bonds), it is necessary to have a Process level canonical form function
    unify                  = elemUnify              // species unification. The core algorithm that finds a 's and 'a variable substitution such that two species are equivalent after applying it
    underscore             = elemUnderscore         // wildcard "_" for 's
    applyAll               = elemApplyAll           // apply a substitution to a species or subspecies
    applySub               = elemApplySub           // apply a species substitution
    applySubVar            = unimplemented          // apply a substitution to a subspecies
    fvs                    = fvElement              // free variables in 's
    refresh                = elemRefresh            // provide a copy of 's where variables have been renamed by an ID provider. Used in resolution
    disambiguateVars       = elemDisambiguateVars   // post-parsing step applied to each parsed clause. In Logic DSD this is used to disambiguate the use of domain variables (e.g. in P = C[D][D!i], D can be cast down to an unbound domain rather than a generic site)
    ComposeProcesses       = deviceComposition      // compose two processes together (possibly forming a complex in Logic DSD)
    resolveGoal            = elemResolveGoal        // species-specific predicates resolution. The core resolution algorithm that executes custom predicates for species (e.g. "compl(D, E)" is a custom predicate in Logic DSD to find the complement E of a domain D)
    domainKeywords         = [] }


// free variables
let fvi (i:Instruction) =
  let rec fvElement (e:Element) = 
    match e with 
    | Element.Part p -> 
      let f x = fvt cle x
      Set.union (f p.name) (f p.type_)
    | Element.Var v ->  Set.singleton (Variable.SVar (v, e))
  
  let rec fvComplex (c:Complex) =
    c |> List.fold (fun acc (_, tp) -> 
          let t = tp |> Term.Species //  |> List.collect (Process.ToList >> List.concat) |> List.distinct
          Set.union acc (Set.unionMany (t |> List.map fvElement))) Set.empty
  
  match i with 
  | Device d            -> d |> List.map fvElement |> List.fold (Set.union) Set.empty
  | Constraint c        -> fvl cle c
  | Initial (n,c)       -> Set.union (fvt cle n) (fvComplex c)
  | Reaction(a,b,c,d,e) -> 
    let av = a |> Option.toList |> List.map fvComplex
    let ev = e |> Option.toList |> List.map (fvt cle)
    let fvs = av @ ([b;c] |> List.map fvComplex) @ [fvt cle d] @ ev
    Set.unionMany fvs

type Program = Instruction list

let enumerateDeviceSubstitutions (cle:Engine) (db:Semantics) (prog:Program) =
  let freevars = 
    prog 
    |> List.map fvi 
    |> Set.unionMany
  let maxVar = 
    freevars
    |> Set.map (fun x -> 
      match x with 
      | Variable.SVar ((n,_), _)
      | Variable.IVar ((n,_), _)
      | Variable.LVar (n,_)
      | Variable.TVar (n,_) -> n)
    |> fun s -> if s.IsEmpty then 0 else Set.maxElement s 
  let counter = ref (maxVar + 1)
  
  let deviceEnumeration d sols = 
    d |> List.fold (fun acc elem ->
      match acc with
      | None -> None 
      | Some solutions ->
        solutions
        |> List.collect (fun (s:RulesDSD.Substitution.Substitution<Element, unit>) -> 
          match elem with 
          | Part part -> 
              // prepare device query
              let pName  = s.Apply(part.name , cle)
              let pType  = s.Apply(part.type_, cle)
              let qPart  = Part { name = pName; type_ = pType }
              let query  = [ Pos (Pred ("part",     [Term.Pat (Pattern.Inner [qPart, Location.wildcard]) ])) ]
              
              // interrogate DB
              let goal   = RulesDSD.Resolution.Goal<Element, unit>.Create(query, cle)
              match RulesDSD.Resolution.resolveInner [goal] cle counter db [] RulesDSD.Resolution.Mode.AllAnswers with
              | None         -> []
              | Some newSols -> if solutions = [Substitution.id]
                                  then newSols
                                  else newSols |> List.map (fun s' -> RulesDSD.Substitution.Substitution.Compose s s' cle)
          | Var v -> 
              // look for any part in the DB
              let query  = [ Pos (Pred ("part",     [Term.Pat (Pattern.Inner [Var v, Location.wildcard]) ])) ]
              let goal   = RulesDSD.Resolution.Goal<Element, unit>.Create(query, cle)
              match RulesDSD.Resolution.resolveInner [goal] cle counter db [] RulesDSD.Resolution.Mode.AllAnswers with
              | None         -> []
              | Some newSols -> if solutions = [Substitution.id]
                                  then newSols
                                  else newSols |> List.map (fun s' -> RulesDSD.Substitution.Substitution.Compose s s' cle) )
        |> fun x -> if x.IsEmpty then None else Some (x |> List.distinct)
      ) (Some sols)

  prog |> List.fold (fun (solutionsSet:RulesDSD.Substitution.Substitution<Element, unit> list option) i -> 
    match solutionsSet with 
    | None -> None // some constraint is unsatisfiable; abort enumeration
    | Some sols ->
      match i with 
      | Initial (_, [1, Term.Proc p]) -> match p |> Process.ToList with 
                                         | [d] -> deviceEnumeration d sols
                                         | _   -> failwithf "Unexpected multiple devices in initial %s." (Instruction.ToString cle i)
      | Device d     -> deviceEnumeration d sols

      | Constraint c -> let sols' = 
                          sols 
                          |> List.collect (fun (s:RulesDSD.Substitution.Substitution<Element, unit>) -> 
                            let c' = s.Apply(c, cle)
                            let g = RulesDSD.Resolution.Goal<Element, unit>.Create([c'], cle)
                            match RulesDSD.Resolution.resolveInner [g] cle counter db [] RulesDSD.Resolution.Mode.AllAnswers with
                            | None         -> []
                            | Some newSols -> if sols.IsEmpty 
                                                then newSols
                                                else newSols |> List.map (fun s' -> RulesDSD.Substitution.Substitution.Compose s s' cle) )
                        match sols' with 
                        | [] -> None 
                        | _  -> Some sols'
                        
      | Reaction _   -> solutionsSet // TODO: check that the interaction is in the DB?
      | Initial _    -> solutionsSet
      ) (Some [Substitution.id])

let enumerateDevices (cle:Engine) (db:Semantics) (prog:Program) : Program list = 
  let maxVar = 
    prog
    |> List.map fvi 
    |> Set.unionMany
    |> Set.map (fun x -> 
      match x with 
      | Variable.SVar ((n,_), _)
      | Variable.IVar ((n,_), _)
      | Variable.LVar (n,_)
      | Variable.TVar (n,_) -> n)
    |> fun s -> if s.IsEmpty then 0 else Set.maxElement s 
    |> ref
  
  let fullProgram = 
    prog
    |> List.map (Instruction.Map (fun e ->
      match e with 
      | Element.Var _ -> e
      | Element.Part p -> if p.name = Term.wildcard 
                            then 
                              let x = !maxVar
                              maxVar := !maxVar + 1
                              let freshVar = Term.Var(x, sprintf "X_%i" x)
                              Element.Part { p with name = freshVar}
                            else e))

  
  let devs = 
    fullProgram 
    |> List.filter (fun i -> match i with 
                             | Instruction.Device _ -> true 
                             | Instruction.Initial _ -> true
                             | _ -> false)
  
  match enumerateDeviceSubstitutions cle db fullProgram with 
  | None -> []
  | Some sols ->
    sols
    |> List.map (fun (RulesDSD.Substitution.Substitution.Sub s) -> 
      devs
      |> List.map (Instruction.Map (cle.applySub s)))
    |> List.distinct



(* GEC calculus*)
let makeGecCalculus (db:Semantics) : Calculus<Term<Element>> =
  // prepare the set of "reaction" predicates to run in the calculus
  
  // collect all "reactions" signatures
  // find all reactions predicate
  let reactionsSigs = seq { for kv in db.Keys do 
                              if fst kv = "reactions" then yield kv else () }
                      |> Seq.sortWith (fun (_, x) (_, y) -> compare x y)
                      |> Seq.toList
                      |> Seq.map (fun x -> x, db.Item x)
  
  { react = fun (oldElements:Term<Element> list) (newElement:Term<Element>) -> 
      // find all query combos: reactions(CRN), reactions(newElement, CRN), reactions (newElement, oldElem, CRN), reactions (newElement, oldElem1, oldElem2, ..., CRN) ...
      // run all queries
      // create new reactions
      let newProc  = newElement  |> Term.Canonical cle 
      let oldProcs = oldElements |> List.map (Term.Canonical cle)
      let rec combo n i : Term<Element> list list =
        if n <= 0 
          then []
          else let ccombo = if n = i 
                              then [[newProc]]
                              else combo (n-1) i
               if ccombo.IsEmpty
                then [oldProcs]
                else
               oldProcs 
               |> List.collect (fun p -> 
                  ccombo 
                  |> List.map (fun ps -> p :: ps))
      
      let queries = 
        reactionsSigs
        |> Seq.filter (fun ((_, numberOfArguments), _) -> numberOfArguments >= 2 ) // take reactions(E1, ...., En, CRN) predicates
        |> Seq.collect (fun ((queryName, numberOfArguments), preds) ->
            match numberOfArguments with
            | 2 -> preds |> Set.map (fun p -> match p.head.Args with [_; x] -> Pos (Pred(queryName, [newProc; x])) | _ -> failwith "") |> Set.toSeq
            | n -> let allElementArgs = [1..n-1] |> List.collect (fun i -> combo (n-1) i) 
                   preds 
                   |> Set.map (fun (p:Clause<Element>) -> allElementArgs 
                                                          |> List.map (fun args -> Pos (Pred(queryName, args @ [p.head.Args |> List.last] ))))
                   |> Set.toSeq
                   |> Seq.concat)
        |> Seq.distinct
        
      // query the Prolog engine and create CRN reactions if any "reactions()" predicate matched
      queries
      |> Seq.choose (fun query -> 
        match RulesDSD.Resolution.resolveAll query db cle with 
        | None -> None
        | Some sols ->
          sols 
          |> List.collect (fun theta -> 
            match theta.Apply(query, cle) with
            | Pos p -> 
              // reactants are already in canonical form by construction
              match p.Args |> List.last with 
              | Term.TCRN ts -> ts 
                                |> List.map (Term.Canonical cle)
                                |> List.collect Term.ToReactions
              | _            -> let str = query |> Literal.ToStringWith cle
                                failwithf "Error: enumeration predicate %s generated a non-CRN term." str
            | _ -> failwith ""  )
          |> Some
        )
      |> List.concat
      |> List.distinct
      |> List.filter (fun r -> r.reactants <> r.products)
  }


///////////////////////////
///////////////////////////
////  Parser
///////////////////////////
///////////////////////////

let pInstruction (idProvider : string -> RulesDSD.Syntax.Var) = 
  let pElement       = Element.doParse cle.domainKeywords true idProvider
  let pDevice        = Parser.between (kw "<") (kw ">") (Parser.many1 pElement) 
  let pDeviceAst     = pDevice |>> Instruction.Device
  let pConstraintAst = RulesDSD.Parser.pliteral cle pElement idProvider cle.domainKeywords |>> Instruction.Constraint
  let pInitialAst    = RulesDSD.Parser.pterm pElement cle idProvider cle.domainKeywords .>>. 
                        choice [ pDevice |>> Choice1Of2 
                                 RulesDSD.Parser.pterm pElement cle idProvider cle.domainKeywords |>> Choice2Of2 ]
                          >>= fun (i, x) -> 
                            match x with 
                            | Choice1Of2 x -> preturn (Instruction.Initial (i, x |> List.singleton |> Process.OfList |> Term.Proc |> toComplex))
                            | Choice2Of2 x -> preturn (Instruction.Initial (i, x |> toComplex))
                        

  Parser.plookAheadWith 
    (choice [
      // device
      kw "<" >>. preturn (Choice1Of4 ())
      // constraint
      kw "not" >>. pname >>. kw "(" >>. preturn (Choice2Of4 ())
      pname >>. kw "(" >>. preturn (Choice2Of4 ())
      // reaction
      // choice [ pdevice >>. choice [kw "+"; kw "->"; preturn Choice1Of4 ()]  // last case is actually a device
      //          kw "->" ] // no reactants reaction

      // initial
      Expression.parse pname >>. preturn (Choice4Of4 ())
    ]) >>= 
    fun next -> 
      match next with
      | Choice1Of4 _ -> pDeviceAst
      | Choice2Of4 _ -> pConstraintAst
      | Choice3Of4 _ -> failParser "Reactions not supported yet"
      | Choice4Of4 _ -> pInitialAst


let generateCRN (cle:Engine) (db:Semantics) (prog:Program) = 
  // collect devices, initials and reactions // TODO: merge devices and initials into a single field initials of type (Term<Element> * Process<Element>)
  let ps, is, rs = 
    prog |> List.fold (fun (ps, is, rs) instruction -> 
                        match instruction with 
                        | Constraint _          -> (ps, is, rs)                         // skip constraints
                        | Reaction (a,b,c,d,e)  -> (ps, is, (a,b,c,d,e) :: rs)
                        | Device d              -> (Process.OfList [d] :: ps, is, rs)
                        | Initial (a,b)         -> if a <> Term.Float 0.0 
                                                    then (ps, (a,b)::is, rs)
                                                    else (ps, is, rs)) ([], [], [])
  
  // species namer
  let default_namer () : (Term<Element> -> string) =
    let cache = new System.Collections.Generic.Dictionary<Term<Element>, string>()
    let names = new System.Collections.Generic.HashSet<string>()
    let namer species : string =
      let sp = species |> Term.Canonical cle
      if cache.ContainsKey sp then
        cache.Item sp
      else
        let mutable unique_name = ""
        let mutable c = names.Count + 1
        while unique_name = "" do
          if not (names.Contains ("sp" + string c)) then
              unique_name <- "sp" + string c
          else c <- c + 1
        let name = unique_name
  
        cache.Add(sp, name)
        ignore(names.Add(name))
        name
    namer
  let namer = default_namer ()
  
  
  // prepare initials and reactions for CRN enumeration
  // TODO: get the initial concentration from the core GEC program after merging process and initials
  let inits  = (is |> List.map (fun (n, i) -> let t = match i with 
                                                      | [1, x] -> x 
                                                      | _      -> Term.TMSet i
                                              let c : Value = 
                                                match n with
                                                | Term.Float x    -> Expression.Float x
                                                | Term.Const x    -> Expression.Key x
                                                | Term.Var (_, x) -> Expression.Key x
                                                | _ -> failwithf "Initial condition %s must be a float, const or a variable." (Term.ToStringWith cle n)
                                              c, t))
               @ (ps |> List.map (fun p -> Expression.Float 1.0, p |> Term.Proc)) 
               |> List.map Initial.create
               
  let toMSet (x:Complex) : Microsoft.Research.CRNEngine.Mset.t<Term<Element>> = 
    x |> List.map (fun (i, sp) -> { Microsoft.Research.CRNEngine.Mset.element = sp
                                  ; Microsoft.Research.CRNEngine.Mset.multiplicity = i})
  let reacts = rs |> List.choose ( fun (catalysts,reactants,products,fwRate,bwRate) -> 
                                    let r = reactants |> toMSet
                                    let p = products |> toMSet
                                    match Term.ToRate fwRate with 
                                    | None -> None
                                    | Some fr -> 
                                        let br = match bwRate with 
                                                 | None   -> None 
                                                 | Some b -> Term.ToRate b
                                        match catalysts with 
                                        | None   -> Reaction.create([], r, br, fr, p)          |> Some 
                                        | Some c -> Reaction.create(c |> toMSet, r, br, fr, p) |> Some )

  // expand the GEC program into a CRN using the GEC calculus
  let equalsSpecies p q = Term.Compare cle (p |> Term.Canonical cle) (q|> Term.Canonical cle) = EQ
  let plotMatcher   p q = Term.Compare cle p q = EQ
  let gecCalculus = makeGecCalculus db
  
  let gecRenderer (s:Term<Element>) : Attributes = 
    { name      = namer s
    ; structure = Term.ToStringWith cle s
    ; svg       = "" }
      
  
  
  let crn = Crn.from_calculus_translated 
              (fun p -> namer p |> Species.create)
              gecRenderer
              equalsSpecies
              plotMatcher
              (fun sp -> (fvt cle sp).IsEmpty)
              "" 
              Microsoft.Research.CRNEngine.Crn_settings.defaults
              gecCalculus  
              inits
              reacts
              false
              true
              true
            |> Crn.group_reactions
  
  crn

let pGecProgram : Parser.t<Semantics> = RulesDSD.Parser.pprogram cle (Element.doParse cle.domainKeywords true)