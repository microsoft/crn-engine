[<JavaScript>]
module Microsoft.Research.DNA.LogicDSD

open RulesDSD.Syntax
open RulesDSD.Substitution
open RulesDSD.Unification
open RulesDSD.ProcessEquality

open System 
open Parser
open System.Diagnostics

let deSub (Sub s) = s

//let NO_TAG  = Term<_>.Func("%NO_TAG", [])
//let ANY_TAG = Term<_>.Func("%ANY_TAG", [])



(* domain terms *)
[<DebuggerDisplay("{ToString(this)}")>]
type Tag = NoTag | AnyTag | Tag of Term<SiteT>
  with 
  static member ToString(t:Tag) = 
    match t with 
    | NoTag -> "{}"
    | AnyTag -> ""
    | Tag tag -> sprintf "{%s}" (Term.ToString tag)
  static member IsConcrete (x : Tag) = 
    match x with 
    | NoTag 
    | AnyTag -> false
    | Tag _ -> true
  static member Canonical cle tag =
    match tag with 
    | NoTag 
    | AnyTag -> tag 
    | Tag t -> Tag (Term.Canonical cle t)
  static member Compare cle tag1 tag2 =
    match tag1, tag2 with 
    | NoTag, NoTag    -> EQ
    | NoTag, _        -> LT
    | AnyTag, NoTag   -> GT
    | AnyTag, AnyTag  -> EQ
    | AnyTag, Tag _   -> LT 
    | Tag t1, Tag t2  -> Term.Compare cle t1 t2
    | Tag _, _        -> GT

and 
 [<DebuggerDisplay("{ToString(this)}")>]
 Domain = { name            : string
            // TODO: should there be a DNA seq : string?
            isComplementary : bool
            isToehold       : bool
            // tag             : Term<SiteT> }
            tag             : Tag }
  with
  //static member NoTag : Term<SiteT> = NO_TAG  // Term<SiteT>.Func("%NO_TAG", [])
  //static member AnyTag : Term<SiteT> = ANY_TAG // = Term<SiteT>.Func("%ANY_TAG", [])
  //static member IsConcreteTag (x : Term<SiteT>) = x <> Domain.NoTag && x <> Domain.AnyTag 
  static member ToString(x:Domain) = 
    x.name 
    + if x.isToehold then "^" else ""
    + if x.isComplementary then "*" else ""
    + if x.tag = Tag.NoTag 
        then "" // domains get NoTag as default value when they are parsed
        else Tag.ToString x.tag
    //+  if x.tag = Domain.NoTag then ""
    //   elif x.tag = Domain.AnyTag then "{_}"
    //   else sprintf "{%s}" (Term.ToString x.tag)
  override this.ToString() = Domain.ToString this
  static member ToStringSiteGraph(x:Domain) = 
    x.name 
    // there is no need to print "^" for toeholds, because toeholds are colored in the SVG visualization
    + if x.isToehold then "^" else ""
    + if x.isComplementary then "*" else ""   
  static member Create(name:string, isComplementary:bool, isToehold:bool, tag:Tag) =
    { name            = name
      isComplementary = isComplementary
      isToehold       = isToehold       
      tag             = tag }
  static member Create(name:string, isComplementary:bool, isToehold:bool) =
    { name            = name
      isComplementary = isComplementary
      isToehold       = isToehold       
      tag             = NoTag }

  /// NB: Compare is only meant to be used in the normal form algorithm
  static member Compare (d1:Domain) (d2:Domain) : int =
        match System.String.Compare(d1.name, d2.name) with
        | x when x = LT -> LT
        | x when x = GT -> GT
        | x when x = EQ -> match d1.isToehold, d2.isToehold with
                           | true,  false -> LT
                           | false, true -> GT
                           | true,  true 
                           | false, false -> match d1.isComplementary, d2.isComplementary with
                                             | true,  false -> LT
                                             | false, true -> GT
                                             | true,  true                   
                                             | false, false -> EQ
        | _ -> failwith "Unexpected error."    
  static member Canonical cle (d:Domain) =
    { name            = d.name
      isComplementary = d.isComplementary
      isToehold       = d.isToehold       
      tag             = d.tag |> Tag.Canonical cle }

and [<DebuggerDisplay("{ToString(this)}")>]
    Bond = Bond of BondID
          | Var of Var
     with
      static member ToString(x:Bond) = 
        match x with 
        | Bond d -> sprintf "%i" d
        | Var v  -> printVar v
      override this.ToString() = Bond.ToString this
      static member Compare compareBonds (b1:Bond) (b2:Bond) : int =
        match b1, b2 with
        | Bond.Var (_,X),  Bond.Var (_,Y)     -> System.String.Compare(X, Y)
        | Bond.Var _,  Bond.Bond _            -> LT
        | Bond.Bond _, Bond.Var _             -> GT
        | Bond.Bond i, Bond.Bond j            -> compareBonds i j

and [<DebuggerDisplay("{ToString(this)}")>]
    DomainT = Dom     of Domain
            // | Var     of Var * Term<SiteT> (* domain variable, tag for that domain *)
            | Var     of Var * Tag 
    with
    static member ToString(x:DomainT) = 
      match x with 
      | Dom d      -> Domain.ToString d
      | Var (v, t) -> printVar v + Tag.ToString t
    override this.ToString() = DomainT.ToString this
    static member ToStringSiteGraph(x:DomainT) = 
      match x with 
      | Dom d                            -> Domain.ToStringSiteGraph d
      | Var (v,_)                        -> printVar v
    static member Compare cle (d1:DomainT) (d2:DomainT) : int =
      let domNumber x = match x with 
                        | DomainT.Dom     d   -> match d.tag with 
                                                 | NoTag -> 0
                                                 | AnyTag -> 1
                                                 | Tag _ -> 2
                        | DomainT.Var(_, tag) -> match tag with 
                                                 | AnyTag -> 3
                                                 | NoTag -> 4
                                                 | Tag _ -> 5
      let n1 = domNumber d1
      let n2 = domNumber d2
      if n1 < n2 then LT
      elif n1 > n2 then GT
      else 
        match d1, d2 with
        | DomainT.Var ((_,X), NoTag), DomainT.Var ((_,Y), NoTag) -> System.String.Compare(X, Y)
        | DomainT.Var ((_,X), tag1), DomainT.Var ((_,Y), tag2) -> 
          let cmp = System.String.Compare(X, Y)
          if cmp = EQ
            then Tag.Compare cle tag1 tag2
            else cmp
        | DomainT.Dom d, DomainT.Dom e -> 
          let cmp = Domain.Compare d e
          if cmp = EQ
            then 
              Tag.Compare cle d.tag e.tag
              // if   d.tag = NoTag  && e.tag = Domain.NoTag  then EQ 
              // elif d.tag = Domain.NoTag  && e.tag <> Domain.NoTag then LT
              // elif d.tag <> Domain.NoTag && e.tag = Domain.NoTag  then GT
              // else Term.Compare cle d.tag e.tag
            else cmp
        | _,_ -> failwith ""
    static member Canonical cle (d:DomainT) =
      match d with 
      | DomainT.Dom d -> d |> Domain.Canonical cle |> DomainT.Dom
      | _             -> d

(* Site terms *)
and 
   [<DebuggerDisplay("{ToString(this)}")>]
   Site = Unbound of DomainT 
          | Bound of DomainT * Bond
  with
  static member ToStringWith domPrinter (x:Site) = 
    match x with 
    | Unbound d -> domPrinter d
    | Bound (d, b) -> sprintf "%s!%s" (domPrinter d) (Bond.ToString b)
  static member ToString(x:Site) = Site.ToStringWith (DomainT.ToString) x
  override this.ToString() = Site.ToString this

  static member GetBonds x = 
    match x with 
    | Unbound _     -> Set.empty
    | Bound (_, b)  -> Set.singleton b
  static member Map(f:Bond -> Bond, this) =
    match this with
    | Unbound _ -> this
    | Bound (d, b) -> Bound (d, f b)
  static member Compare cle compareBonds (s1:Site) (s2:Site) : int =
        match s1, s2 with
        | Site.Unbound X, Site.Unbound Y         -> DomainT.Compare cle X Y
        | Site.Unbound _, Site.Bound _           -> LT
        | Site.Bound _,   Site.Unbound _         -> GT
        | Site.Bound(d1, b1), Site.Bound(d2, b2) ->
          match DomainT.Compare cle d1 d2 with
          | x when x = LT -> LT
          | x when x = GT -> GT
          | x when x = EQ -> Bond.Compare compareBonds b1 b2
          | _             -> failwith "Unexpected error."
  
  static member Canonical cle (x:Site) =
    match x with
    | Site.Bound(d,b) -> (DomainT.Canonical cle d, b) |> Site.Bound
    | Site.Unbound(d) -> d |> DomainT.Canonical cle   |> Site.Unbound

and  
  [<StructuralEquality;StructuralComparison>]
  [<DebuggerDisplay("{ToString(this)}")>]
  SiteT = Site   of Site
          | Var  of Var
  with
  static member ToStringWith domPrinter (x:SiteT) = 
    match x with 
    | Site s -> Site.ToStringWith domPrinter s
    | Var  v -> printVar v
  static member ToString(x:SiteT) = SiteT.ToStringWith (DomainT.ToString) x
  override this.ToString() = SiteT.ToString this

  static member GetBonds x =
    match x with 
    | Site s -> Site.GetBonds s
    | Var  _ -> Set.empty
  
  static member Map(f:Bond -> Bond, this) =
    match this with 
    | SiteT.Var _  -> this
    | SiteT.Site s -> SiteT.Site (Site.Map(f, s))

  static member Compare cle compareBonds (x:SiteT) (y:SiteT) : int =
    match x, y with
    | SiteT.Var (_,X), SiteT.Var (_,Y)  -> System.String.Compare(X, Y)
    | SiteT.Var _, SiteT.Site _         -> LT
    | SiteT.Site _, SiteT.Var _         -> GT
    | SiteT.Site s1, SiteT.Site s2      -> Site.Compare cle compareBonds s1 s2
  
  static member Canonical cle (x:SiteT) =
    match x with 
    | SiteT.Var _ -> x
    | SiteT.Site s -> s |> Site.Canonical cle |> SiteT.Site











(*****************************************************************)
(*****************************************************************)
(* Canonical form. 
   Sorts a process's strands and bonds in canonical form.
   A process is in canonical form if:
   * strands are sorted in lexicographic order, whereby 
     domains are sorted alphabetical order on the name of 
     the domain
   * bonds are enumerated in increasing order, such
     that the first bond to occur in the sorted list of strands
     has ID 0, the second bond to occur has ID 1, etc.           *)
(*****************************************************************)
(*****************************************************************)
let dsdCanonicalForm (cle:CLE<SiteT, 'a>) (p:Process<SiteT>) = // TODO factor out refresher?
  let speciesCompare = cle.compare
  
  (* bond names renamer, starting from 0 upwards *)
  let newBondRefresher () = 
    let freshBondsCounter = ref 0
    let refreshedBonds = ref Map.empty 
    let bondRefresher (s:SiteT) = 
      SiteT.Map(fun b -> match b with 
                         | Bond.Var _ -> b
                         | Bond.Bond i -> if Map.containsKey i !refreshedBonds
                                            then
                                              Bond.Bond (!refreshedBonds).[i]
                                            else
                                              let freshID = !freshBondsCounter
                                              freshBondsCounter := freshID + 1
                                              refreshedBonds := Map.add i freshID (!refreshedBonds)
                                              Bond.Bond freshID
                , s)
    bondRefresher

  (* Bisimulation algorithm to split up lexicographically equivalent strands.
     Strands can be grouped lexicographically, i.e. they have exactly the same domain names
     and site kinds (bound or not), but they might be used in different contexts (e.g. one is bound to 
     a single strand while another one is bound to a bigger complex).

     For example, if we have:
       <a!0 b!1>
       | <a!2 b !3>
       | <b*!3 a*!2 b*!1 a*!0>
     <a!0 b!1> and <a!2 b !3> are lexicographically equivalent (same names, same type of sites), but 
     in <b*!3 a*!2 b*!1 a*!0> the strand <a!0 b!1> is bound at the '3 end, while 
     <a!2 b !3> is bound at the 5' end. 
     This is very important to find a unique canonical form.

     Sometimes strands cannot be differentiated by the context.
     For example in:
     <a!0 a*!1> | <a!1 a*!0>
     the strands are lexicographically equivalent, and they are perfectly symmetrical.
     Any permutation of bonds produces the same system.
     We want to find out these cases too; we can give their bond any names without affecting the canonical form 
     (hopefully, I don't have a proof that the canonical form algorithm below is indeed unique yet. This is a variant of
     the Paige-Tarjan block refinement algorithm to find equivalent strands up to bonds; see chapter 6 of Handbook of Process Algebra 
     for an overview. I believe that Nauty by McKay uses the same principle in its search for a canonical form, see 
     "McKay's Canonical Graph Labeling Algorithm" by S. G. Hartke and A. J. Radcliffe).

     If two strands have the same domains, this algorithm tries to differentiate them
     by trying to find the earliest bond that points to different (non-bisimilar) strands.
     *)
  let sortIntoEquivalenceClasses p =
    let bondRefresher = newBondRefresher ()
    let rec f (acc:Strand<SiteT> list list) (xs:Strand<SiteT> list) = 
      match xs with 
      | []      -> acc // sort internally each equivalence class by bond value
                   |> List.map (List.sortWith (fun str -> str |> List.compareWith (SiteT.Compare cle compare)))
      | x::xs'  -> if acc.IsEmpty
                      then f [[x]] xs'
                      else match List.compareWith (SiteT.Compare cle (fun _ _ -> EQ)) x acc.Head.Head with
                           | 0 (* EQ *) ->  let acc' = (x::acc.Head) :: acc.Tail
                                            f acc' xs'
                           | _  -> f ([x] :: acc) xs'
    p
    |> Process.ToList
    // rename bonds so that they start from 0 upwards
    |> List.fold (fun acc strand -> let strand' = strand |> List.map bondRefresher
                                    strand' :: acc) []
    |> List.sortWith (List.compareWith ((SiteT.Compare cle (fun _ _ -> EQ)) (*integer compare*)))
    |> f [] 
    |> List.rev
    
  let rec bisimulationSort (eqBlocks (* equivalence blocks *) : (Strand<SiteT> list) list) = 
    let foldi fold first source  =
     source 
     |> List.fold(fun (prev,i) c -> (fold i prev c,i + 1)) (first,0)
     |> fst
    let bondsInfo : ((int * int * int) Set) array =  // array that maps a bond to a set of matching
                                                     // (block index, strand index, site index)
      eqBlocks 
      |> foldi (fun blockIdx acc block -> 
          block 
          |> foldi (fun strandIdx acc strand -> 
              strand
              |> foldi (fun siteIdx acc site ->
                  match site with 
                  | SiteT.Site (Site.Bound(_,Bond.Bond i)) -> 
                    let bondInfo = (blockIdx, strandIdx, siteIdx)
                    if Map.containsKey i acc
                      then acc.Add(i, Set.add bondInfo acc.[i])
                      else acc.Add(i, Set.singleton bondInfo)
                  | _ -> acc
                 ) acc
             ) acc
        ) Map.empty
     |> Map.toArray
     |> fun m -> m |> Array.sortInPlaceBy fst
                 m |> Array.map snd

    let getBonds (s:Strand<SiteT>) : (int * int) list =  // (siteIndex, bond) list
      s |> foldi (fun siteIdx acc site -> 
            match site with 
            | SiteT.Site (Site.Bound(_,Bond.Bond b)) -> (siteIdx, b) :: acc
            | _                                         -> acc
           ) []
        |> List.rev

    let comparer x y = if x = y then EQ elif x < y then LT else GT
            
    let splitBlock =
      eqBlocks
      |> foldi (fun blockIdx acc block -> 
          match acc with
          | Some _ -> acc 
          | None   -> 
            block
            |> foldi (fun splitterIdx acc splitter ->
                match acc with 
                | Some _ -> acc 
                | None   -> 
                  let splitterBonds = getBonds splitter
                  let splitterResult = // try to split the block
                    block 
                    |> foldi (fun targetIdx (ltAcc, eqAcc, gtAcc) target -> // (list of bisimilar strands to acc, list of non-bisimilar strands to acc)
                        if targetIdx = splitterIdx
                          then (ltAcc, target :: eqAcc, gtAcc)
                          else
                            let targetBonds = target |> getBonds 
                            let isSplittable =
                              List.zip splitterBonds targetBonds
                              |> List.fold (
                                  fun acc ((siteIdx1, bond1), (siteIdx2, bond2)) -> 
                                    if acc <> EQ
                                      then acc
                                      else
                                    let info1 = (blockIdx, splitterIdx, siteIdx1)
                                    let info2 = (blockIdx, targetIdx,   siteIdx2)
                                    let splitterInfo = Set.remove info1 bondsInfo.[bond1]
                                    let targetInfo   = Set.remove info2 bondsInfo.[bond2]
                                    if targetInfo.Count <> splitterInfo.Count
                                      then comparer targetInfo.Count splitterInfo.Count
                                    elif targetInfo.Count = 0 
                                      then EQ
                                    elif targetInfo.Count > 1
                                      then failwith <| sprintf "Bond %i cannot occur more than twice in a strand"  bond1
                                      else 
                                        let (tBlock, tStrand, tSite) = targetInfo   |> Set.toList |> List.head
                                        let (sBlock, sStrand, sSite) = splitterInfo |> Set.toList |> List.head
                                        if tBlock <> sBlock
                                          then comparer tBlock sBlock
                                        elif tStrand <> sStrand 
                                          then comparer tStrand sStrand 
                                        elif tSite <> sSite 
                                          then comparer tSite sSite 
                                          else EQ
                                        ) EQ
                            match isSplittable with
                            | -1 (* LT *) -> target::ltAcc, eqAcc,          gtAcc
                            | 0  (* EQ *) -> ltAcc,         target::eqAcc,  gtAcc
                            | 1  (* GT *) -> ltAcc,         eqAcc,          target::gtAcc
                            | _ -> failwith ""
                          ) ([], [], []) 
                  match splitterResult with
                  | [], _, []     -> None // not a splitter
                  | lts, eqs, gts -> Some (blockIdx, lts, eqs, gts)
                       
              ) None
         ) None 
    match splitBlock with
    | None -> eqBlocks // no splitter discovered: eqBlocks is a bisimulation
    | Some (blockIdx, lessThanSplitter, equalsToSplitter, greaterThanSplitter)-> 
        let eqBlocks' = List.take blockIdx eqBlocks
                        @ ([lessThanSplitter; equalsToSplitter; greaterThanSplitter]
                            |> List.filter (List.isEmpty >> not))
                        @ List.skip (blockIdx + 1) eqBlocks
        bisimulationSort eqBlocks' 
  (*/bisimulation sort*)

  let normalise (p: Process<SiteT>) = 
    let bondRefresher = newBondRefresher()
    
    p 
    |> sortIntoEquivalenceClasses
    |> bisimulationSort
    // flatten all equivalence blocks
    |> List.concat  
    // refresh bond names according to the new sorting
    |> List.fold (fun acc strand -> let strand' = strand |> List.map bondRefresher
                                    strand' :: acc) []
    |> List.rev
    |> Process.OfList      


  // put a process in canonical form
  p 
  |> normalise



let logicDsdComposeProcesses (shiftBonds:bool) (p:Process<SiteT>) (q:Process<SiteT>) =
    let q' = 
      if shiftBonds
        then 
          let maxBondP = p |> Process.Fold 
                               (fun acc x -> match x with 
                                             | SiteT.Site (Site.Bound (_, Bond.Bond i)) -> 
                                                if acc <= i
                                                  then acc + 1
                                                  else acc
                                             | _ -> acc) 0
          q |> Process.Map 
            (fun x -> 
              match x with 
              | SiteT.Site (Site.Bound (d, Bond.Bond i)) -> 
                SiteT.Site (Site.Bound (d, Bond.Bond (maxBondP + i)))
              | _ -> x)
        else q
    (Process.ToList p) @ (Process.ToList q') 
      |> Process.OfList








let rec unifyTag cle tag1 tag2 : Sub<SiteT, Choice<DomainT, Bond>> list = 
  match tag1, tag2 with 
  | NoTag, NoTag 
  | AnyTag, _
  | _, AnyTag -> [Substitution<SiteT, Choice<DomainT, Bond>>.id |> deSub]
  | Tag t1, Tag t2 -> unify cle [TEq (t1, t2)] |> List.map deSub
  | Tag _, NoTag 
  | NoTag, Tag _ -> []

and unifyDomain (cle:CLE<SiteT, Choice<DomainT, Bond>> ) (d1:DomainT, d2:DomainT) : Sub<SiteT, Choice<DomainT, Bond>> list = 
  //let tagsMatch tag1 tag2 = 
  //  if tag1 = tag2
  //    then true 
  //    else match tag1, tag2 with
  //         | AnyTag, NoTag
  //         | NoTag, AnyTag -> false 
  //         | AnyTag, _
  //         | _, AnyTag -> true
  //         | Tag t, _ 
  //         | _, Tag t  when t = Term.Wildcard -> t = Term.Wildcard
  //         | _ -> false
  //let unifyTags tag1 tag2 = 
  //  if tagsMatch tag1 tag2 
  //    then [Substitution<SiteT, Choice<DomainT, Bond>>.id |> deSub]
  //  elif Tag.IsConcrete tag1 && Tag.IsConcrete tag2
  //    then unify cle [TEq (tag1, tag2)]  |> List.map deSub
  //  else []
  match d1, d2 with
  | DomainT.Var ((-1,"_"), tag1), td2
  | td2 , DomainT.Var ((-1,"_"), tag1) -> 
    match td2 with 
    | DomainT.Var (_, tag2) -> unifyTag cle tag1 tag2  
    | DomainT.Dom d         -> unifyTag cle tag1 d.tag 
    
  (*
  | _, DomainT.Var ((-1,"_"), tag1) 
  // wildcards match anything
  | DomainT.Var ((-1,"_"), None), _
  | _, DomainT.Var ((-1,"_"), None) -> [Substitution<SiteT, Choice<DomainT, Bond>>.id |> deSub]
  // generate a solution
  | DomainT.Var (x, None), d
  | d, DomainT.Var (x, None) -> 
      let asd : Choice<DomainT, Bond> = Choice1Of2 d
      [Substitution<SiteT, Choice<DomainT, Bond>>.Create(x, asd) |> deSub]
  // Skip wildcards
  | DomainT.Var ((-1, "_"), Some t1), DomainT.Var(_, Some t2)
  | DomainT.Var (_ , Some t1), DomainT.Var((-1, "_"), Some t2)
    -> unify cle [TEq (t1, t2)]  |> List.map deSub
  | DomainT.Var ((-1, "_"), Some tag1), DomainT.Dom d
  | DomainT.Dom d, DomainT.Var ((-1, "_"), Some tag1)
    -> match d.tag with
       | None      -> []
       | Some tag2 -> unify cle [TEq (tag1, tag2)] |> List.map deSub
  *)
  // generate a solution
  | DomainT.Var (x, AnyTag), d
  | d, DomainT.Var (x, AnyTag) -> 
      let asd : Choice<DomainT, Bond> = Choice1Of2 d
      [Substitution<SiteT, Choice<DomainT, Bond>>.Create(x, asd) |> deSub]
  


  | DomainT.Var (x, tag1), DomainT.Var (y, tag2) ->
    let theta = Substitution<SiteT, Choice<DomainT, Bond>>.Create(x, Choice1Of2 <| DomainT.Var(y, tag2))
    unifyTag cle tag1 tag2
    |> List.map (fun theta' -> Substitution.Compose theta (Sub theta') cle |> deSub)
  | DomainT.Var (x, tag1), DomainT.Dom d
  | DomainT.Dom d, DomainT.Var (x, tag1) ->
    let theta = Substitution<SiteT, Choice<DomainT, Bond>>.Create(x, Choice1Of2 <| DomainT.Dom d)
    unifyTag cle tag1 d.tag
    |> List.map (fun theta' -> Substitution.Compose theta (Sub theta') cle |> deSub)

  // check for equality
  | DomainT.Dom d1, DomainT.Dom d2 ->
    if d1.name = d2.name 
       && d1.isComplementary = d2.isComplementary
       && d1.isToehold = d2.isToehold
       then unifyTag cle d1.tag d2.tag 
       else []

and unifyBond (i1:Bond, i2:Bond) =
  match i1, i2 with
  | Bond.Var (-1,"_"), _
  | _, Bond.Var (-1,"_") -> Some Substitution.id
  // generate a solution
  | Bond.Var x, i
  | i, Bond.Var x -> Some (Substitution<SiteT, Choice<DomainT, Bond>>.Create(x, Choice2Of2 i))
  // check for equality
  | Bond.Bond b1, Bond.Bond b2 -> if b1 = b2 
                                    then Some Substitution.id
                                    else None
and unifySite cle (site1:SiteT, site2:SiteT) = 
  match site1, site2 with
  | SiteT.Var (-1,"_"), _
  | _, SiteT.Var (-1,"_") -> [Substitution<SiteT, Choice<DomainT, Bond>>.id |> deSub]
  | SiteT.Var x, s 
  | s, SiteT.Var x -> [Substitution<SiteT, Choice<DomainT, Bond>>.Create(x, s) |> deSub]
                      
  | SiteT.Site (Unbound d1),
    SiteT.Site (Unbound d2) -> unifyDomain cle (d1, d2) 
  | SiteT.Site (Bound (d1, i1)),
    SiteT.Site (Bound (d2, i2)) -> 
      match unifyDomain cle (d1, d2) with
      | [] -> []
      | thetas1 -> 
        match unifyBond (i1, i2) with
        | Some theta2 -> thetas1 |> List.map (fun theta1 -> Substitution<SiteT, Choice<DomainT, Bond>>.Compose (Sub theta1) theta2 cle |> deSub)
        | None -> []
  
  | _, _-> []



let domApply unwrap (cle:CLE<SiteT, Choice<DomainT, Bond>>) (sub:Sub<SiteT, Choice<DomainT, Bond>>) (d:DomainT)  = 
  let resolveDomain x = 
    let msg y z = failwithf "Cannot substitute %s %s in domain %s" y z (snd x) 
    match sub.TryFind x with
    | Some (Choice4Of4 (Choice1Of2 d))                -> d
    | Some (Choice1Of4 a)                             -> msg "site" (Term.ToStringWith cle a)
    | Some (Choice2Of4 a)                             -> msg "location" (Location.ToString a)
    | Some (Choice3Of4 (SiteT.Site (Site.Unbound d))) -> if unwrap then d else msg "site" (cle.toString (SiteT.Site (Site.Unbound d)))
    | Some (Choice3Of4 a )                            -> msg "site" (SiteT.ToString a)
    | Some (Choice4Of4 (Choice2Of2 a))                -> msg "bond" (Bond.ToString a)
    | None                                            -> DomainT.Var (x, AnyTag) ///NoTag) 
  
  match d with
  | DomainT.Var ((-1, "_"), AnyTag) 
  | DomainT.Var ((-1, "_"), NoTag) -> d
  | DomainT.Var ((-1, "_"), Tag tag) ->
      let tag' = Substitution.Apply(Sub sub, tag, cle)
      DomainT.Var ((-1, "_"), Tag tag')
  
  | DomainT.Var (x, tag1)    -> 
      match tag1 with 
      | AnyTag -> resolveDomain x
      | NoTag  -> match resolveDomain x with 
                  | DomainT.Var (z, NoTag)  -> DomainT.Var (z, NoTag)
                  | DomainT.Var (z, AnyTag) -> DomainT.Var (z, NoTag)
                  | DomainT.Var (z, Tag t)  -> failwith <| sprintf "Tag mismatch: cannot substitute %s in %s{} with %s{%s}" (RulesDSD.Syntax.printVar x) (RulesDSD.Syntax.printVar x) (RulesDSD.Syntax.printVar z) (Term.ToStringWith cle t)
                  | DomainT.Dom d          -> if d.tag = NoTag 
                                               then DomainT.Dom d 
                                               else failwith <| sprintf "Tag mismatch: cannot substitute %s in %s{} with %s" (RulesDSD.Syntax.printVar x) (RulesDSD.Syntax.printVar x) (Domain.ToString d)
      | Tag t1 -> match resolveDomain x with 
                  | DomainT.Var (z, AnyTag)   -> DomainT.Var (z, Tag t1)
                  | DomainT.Var (z, NoTag)    -> failwith <| sprintf "Tag mismatch: cannot substitute %s in %s{%s} with %s{}" (printVar x) (printVar x) (Term.ToString t1) (printVar z)
                  | DomainT.Var (z, Tag tag2) -> if Substitution.Apply(Sub sub, t1, cle) = tag2
                                                  then DomainT.Var (z, Tag tag2)
                                                  else failwith <| sprintf "Cannot substitute %s in %s with %s" (RulesDSD.Syntax.printVar x) (DomainT.ToString d) (snd x)
                  | DomainT.Dom d           -> match d.tag with 
                                               | NoTag -> failwith ""
                                               | AnyTag -> DomainT.Dom { d with tag = Tag t1 }
                                               | Tag tag2 ->
                                                 if Substitution.Apply(Sub sub, t1, cle) = tag2
                                                   then DomainT.Dom { d with tag = Tag tag2 }
                                                   else failwith <| sprintf "Cannot substitute %s in %s with %s" (RulesDSD.Syntax.printVar x) (Domain.ToString d) (Domain.ToString d)
                                               (*
                                               if d.tag = AnyTag
                                                 then DomainT.Dom d 
                                               elif Substitution.Apply(Sub sub, t1, cle) = d.tag
                                                 then DomainT.Var (z, Tag tag2)
                                                 else failwith <| sprintf "Tag mismatch: cannot substitute %s in %s{%s} with %s{}" (printVar x) (printVar x) (Term.ToString t1) (printVar z)
                                               *)
  (*
  | DomainT.Var (x, Tag tag1)  -> 
    match resolveDomain x with 
    | DomainT.Var (y, t2) -> 
      match t2 with 
      | NoTag    -> failwith ""
      | AnyTag   -> DomainT.Var (y, Tag (Substitution.Apply(Sub sub, tag1, cle)))
      | Tag tag2 -> if Substitution.Apply(Sub sub, tag1, cle) = tag2
                      then DomainT.Var (y, Tag tag2)
                      else failwith <| sprintf "Cannot substitute %s in %s with %s" (RulesDSD.Syntax.printVar x) (DomainT.ToString d) (snd x)
    | DomainT.Dom dom ->  
      match dom.tag with 
      | NoTag -> failwith ""
      | AnyTag -> DomainT.Dom { dom with tag = Tag tag1 }
      | Tag tag2 ->
        if Substitution.Apply(Sub sub, tag1, cle) = tag2
          then DomainT.Dom { dom with tag = Tag tag2 }
          else failwith <| sprintf "Cannot substitute %s in %s with %s" (RulesDSD.Syntax.printVar x) (DomainT.ToString d) (DomainT.ToString d)
  *)

  | DomainT.Dom dom      -> 
      match dom.tag with 
      | NoTag 
      | AnyTag -> d
      | Tag tag2 -> let tag2' = Substitution.Apply(Sub sub, tag2, cle)
                    DomainT.Dom {dom with tag = Tag tag2' }
  
let bondApply (cle:CLE<SiteT, Choice<DomainT, Bond>>) (sub:Sub<SiteT, Choice<DomainT, Bond>>) (b:Bond) =
  let msg y z = failwithf "Cannot substitute %s %s in bond %s" y z (Bond.ToString b) 
  match b with 
  | Bond.Var v -> match sub.TryFind v with
                  | Some (Choice4Of4 (Choice2Of2 b)) -> b
                  | Some (Choice1Of4 a)              -> msg "site" (Term.ToStringWith cle a)
                  | Some (Choice2Of4 a)              -> msg "location" (Location.ToString a)
                  | Some (Choice3Of4 a)              -> msg "site" (cle.toString a)
                  | Some (Choice4Of4 (Choice1Of2 d)) -> msg "domain" (DomainT.ToString d)
                  | None -> b
  | Bond.Bond _ -> b
    
let siteApply (cle:CLE<SiteT, Choice<DomainT, Bond>>) (sub:Sub<SiteT, Choice<DomainT, Bond>>) (s:SiteT) = 
  let msg v y z = failwithf "Cannot substitute site variable %s with %s %s" (snd v) y z
  match s with 
  | SiteT.Var (-1, "_") -> s
  | SiteT.Var v -> match sub.TryFind v with 
                   | None -> s 
                   | Some (Choice3Of4 s') -> s'
                   | Some (Choice1Of4 t) -> msg v "term" (Term.ToStringWith cle t)
                   | Some (Choice2Of4 l) -> msg v "location" (Location.ToString l)
                   | Some (Choice4Of4 (Choice1Of2 d)) -> SiteT.Site (Site.Unbound d)
                   | Some (Choice4Of4 (Choice2Of2 b)) -> SiteT.Site (Site.Bound (DomainT.Var((-1, ""), AnyTag), b))
  | SiteT.Site(Unbound (DomainT.Var (x, t))) -> let unwrap = match sub.TryFind x with 
                                                             | Some (Choice3Of4 (SiteT.Site (Unbound _))) -> true 
                                                             | _   -> false
                                                SiteT.Site(Unbound (domApply unwrap cle sub (DomainT.Var (x, t))))
  | SiteT.Site(Unbound d )  -> SiteT.Site(Unbound (domApply false cle sub d))
  | SiteT.Site(Bound (d,i)) -> SiteT.Site(Bound (domApply false cle sub d, bondApply cle sub i))
  
let dsdSubApply (cle:CLE<SiteT, Choice<DomainT, Bond>>) (sub:Sub<SiteT, Choice<DomainT, Bond>>) (y:Choice<SiteT, Choice<DomainT, Bond>>) : SiteT = 
  match y with 
  | Choice1Of2 s -> siteApply cle sub s
  | Choice2Of2 c -> match c with 
                    | Choice1Of2 d -> SiteT.Site (Site.Unbound (domApply true cle sub d))
                    | Choice2Of2 b -> SiteT.Site (Site.Bound (DomainT.Var((-1, "_"), AnyTag), bondApply cle sub b))


let dsdSubApply2 (cle:CLE<SiteT, Choice<DomainT, Bond>>) (sub:Sub<SiteT, Choice<DomainT, Bond>>) (y:Choice<DomainT, Bond>) : Choice<DomainT, Bond> = 
  match y with 
  | Choice1Of2 d -> Choice1Of2 (domApply false cle sub d)
  | Choice2Of2 b -> Choice2Of2 (bondApply cle sub b)


let rec fvDomain dt = 
  match dt with 
  | DomainT.Dom _      -> Set.empty
  | DomainT.Var (x, t) -> let fvTag = match t with 
                                      | NoTag 
                                      | AnyTag  -> Set.empty
                                      | Tag tag -> fvt ({ CLE<SiteT, Choice<DomainT, Bond>>.empty with fvs = fvSiteT}) tag
                          let fvx = if x = (-1, "_") then Set.empty else Set.singleton (IVar (x, Choice1Of2 dt)) (*DVar x*)
                          Set.union fvTag fvx

and fvBond b =
  match b with
  | Bond.Bond _        -> Set.empty
  | Bond.Var (-1, "_") -> Set.empty
  | Bond.Var x         -> Set.singleton (Variable.IVar (x, Choice2Of2 b)) (*BVar x*)

and fvSiteT (st:SiteT) = 
  match st with 
  | Site (Bound (d, i)) -> Set.union (fvDomain d) (fvBond i)
  | Site (Unbound d)    -> fvDomain d
  | SiteT.Var x         -> if x = (-1, "_") then Set.empty else Set.singleton (SVar (x, st))

// split a process into connected components of hybridized strands
let toDsdComplexes(p:Process<SiteT>) : Process<SiteT> list =
  // TODO: optimize using a better algorithm to find connected components
  //       (e.g. https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm, cfr. SiteGraphReactor)
  let rec findComplex (complex:(SiteT list) list) (bonds:Set<Bond>) strands = 
    let neighbours, strands' = 
      strands 
      |> List.partition (fun (x,_) -> Set.intersect bonds x 
                                      |> Set.isEmpty 
                                      |> not)
  
    if neighbours.IsEmpty
      then 
        let system = complex
                     |> List.mapi (fun i x -> i, x)
                     |> Map.ofList
                     |> Process<SiteT>.Proc
        (system, strands)
      else 
        let complex' = complex @ (neighbours |> List.map snd)
        let bonds'   = Set.unionMany (bonds :: (neighbours |> List.map fst))
        findComplex complex' bonds' strands'


  let rec findAllComplexes complexesAcc strandsAcc : Process<SiteT> list =
    match strandsAcc with
    | [] -> complexesAcc
    | (bonds, strand)::rest -> 
        let (complex, strandsAcc') = findComplex [strand] bonds rest
        let complexesAcc' = complex :: complexesAcc 
        findAllComplexes complexesAcc' strandsAcc'

  let findComplexes (Process.Proc sys:Process<SiteT>) : Process<SiteT> list =
    let strands = sys 
                  |> Map.toList 
                  |> List.map (fun (_, strand) -> (strand 
                                                    |> List.map SiteT.GetBonds 
                                                    |> Set.unionMany
                                                  , strand))
    findAllComplexes [] strands

  p 
  |> findComplexes




let toStringSiteGraph (cle:CLE<SiteT, Choice<DomainT, Bond>>) (x:Term<SiteT>) = 
  let printer x = match x with 
                  | Choice1Of2 d -> DomainT.ToStringSiteGraph d
                  | Choice2Of2 b -> Bond.ToString b
  let cle' = { cle with toStringTempVar =  printer }
  Term<SiteT>.ToStringWith cle' x

(* Logic DSD predicates *)
let rec resolveDsdGoal (cle:CLE<SiteT, Choice<DomainT, Bond>>) (name, args) (theta:Sub<SiteT, Choice<DomainT, Bond>>) = 
  // (Substitution<SiteT, Choice<DomainT, Bond>> *Substitution<SiteT, Choice<DomainT, Bond>> * Literal<SiteT> list * Clause<SiteT>) list option 
  let f (Sub s) = s
  let extendSolutions (cle:CLE<SiteT, Choice<DomainT, Bond>>) (theta:Substitution<SiteT, Choice<DomainT, Bond>>) solutions = 
    match solutions with
    | [] -> None
    | sols -> sols |> List.fold (fun acc sol -> 
                          let theta' = Substitution.Update theta sol cle
                          (f sol, f theta') :: acc
                      ) []
                      |> Some

  let g = Pred(name, args)
  match g with 
  (*****************)
  (* Context setup *)
  (*****************)
  | Pred ("toComplexes", [Term.Proc p; output]) 
      ->
        let ps = p |> toDsdComplexes
                   |> List.map Term.Proc
                   |> Term.TList
        unify cle [TEq (ps, output)]
        |> extendSolutions cle (Sub theta)

  | Pred ("fromComplexes", [term; output]) 
      ->
        match Term.FlattenList term with
        | Choice1Of2 ts -> 
          let p0 = RulesDSD.Syntax.Process.OfList []
          match ts |> List.fold 
                 (fun st t -> match st with 
                              | None -> None 
                              | Some (q:RulesDSD.Syntax.Process<SiteT>) 
                                        -> match t with 
                                           | Term.Proc p -> Some (cle.ComposeProcesses (*false*) q p )
                                           | _           -> None
                              )
                 (Some p0)
           with 
           | None    -> None
           | Some p -> unify cle [TEq (Term.Proc p, output) ] 
                       |> extendSolutions cle (Sub theta)
        | Choice2Of2 _ -> None

  | Pred ("toList", [Term.Proc (Process.Proc s); output])
      -> let strands = s 
                       |> Map.toList  
                       |> List.map (snd >> (List.map (fun d -> Pat (Pattern.Inner [d, Location.Var (-1, "_")]))) >> TList)
                       // |> List.map (fun d -> Pattern (Pattern.Inner [d]))
         unify cle [TEq (output, TList strands) ] 
         |> extendSolutions cle (Sub theta)

  | Pred ("toDomains", [Term.Proc (Process.Proc s); output ])
      -> let domains = s 
                       |> Map.toList  
                       |> List.collect snd 
                       |> List.map (fun d -> match d with 
                                             | SiteT.Site (Site.Bound (dm,_)) -> Pat (Pattern.Inner [SiteT.Site (Site.Unbound dm), Location.Var (-1, "_")])
                                             | SiteT.Site (Site.Unbound dm)   -> Pat (Pattern.Inner [d, Location.Var (-1, "_")])
                                             | _                              -> Pat (Pattern.Inner [d, Location.Var (-1, "_")]))
                       |> List.distinct
         unify cle [TEq (output, TList domains) ]
         |> extendSolutions cle (Sub theta)
  
  | Pred ("toSites", [Term.Proc (Process.Proc s); output ])
      -> let domains = s 
                       |> Map.toList  
                       |> List.collect snd 
                       |> List.distinct
                       |> List.map (fun d -> Pat (Pattern.Inner [d, Location.Var (-1, "_")]))
         unify cle [TEq (output, TList domains) ] 
         |> extendSolutions cle (Sub theta)

  (********************)
  (* Strand utilities *)
  (********************)
  | Pred ("compl",   [ Pat (Pattern.Inner [Site (Unbound (DomainT.Dom d)), _])
                       Pat (Pattern.Inner [Site (Unbound (DomainT.Dom d')), _])]) 
      -> if d.isComplementary = not (d'.isComplementary) 
            && d.name = d'.name
            && d.isToehold = d'.isToehold
          then Some [f Substitution.id, theta]
          else None
  | Pred ("compl",   [dvar; Pat (Pattern.Inner [Site (Unbound (DomainT.Dom d)), l])]) 
  | Pred ("compl",   [Pat (Pattern.Inner [Site (Unbound (DomainT.Dom d)), l]); dvar]) 
      -> let d' = { d with isComplementary = not d.isComplementary; tag = NoTag }
         unify cle [TEq (dvar, Pat (Pattern.Inner [Site (Unbound (DomainT.Dom d')), l]))]
         |> extendSolutions cle (Sub theta)
  | Pred ("freshBond", [pat; Term.TList sys ]) 
      ->
         match ProcessEquality.Flatten cle (*false*) <| Term.TList sys with
         | Some p -> let g' = ("freshBond", [pat; Term.Proc p ]) 
                     resolveDsdGoal cle g' theta
         | None   -> None
  | Pred ("freshBond", [Term.Var (i,y); Term.Proc sys])
  | Pred ("freshBond", [Pat (Pattern.Inner [Site (Bound (_, Bond.Var (i,y))), _]); Term.Proc sys ]) 
        -> 
      let freshBond = sys 
                      |> Process.Fold
                          (fun acc site -> 
                            match site with 
                            | SiteT.Site (Site.Bound(_, Bond.Bond i)) -> 
                                if acc <= i
                                  then i+1
                                  else acc
                            | _ -> acc) 0
      let sol    = Substitution<SiteT, Choice<DomainT, Bond>>.Create( ((i,y) : Var), Choice2Of2 (Bond.Bond freshBond))
      let theta' = (Sub theta).Add((i,y), Choice2Of2 (Bond.Bond freshBond), cle)
      Some [f sol, f theta'] 

  | Pred ("complementary", [Pat (Pattern.Inner [SiteT.Site (Unbound (Dom d)), _])])
  | Pred ("complementary", [Pat (Pattern.Inner [SiteT.Site (Bound(Dom d, _)), _])])
    ->
    if d.isComplementary
      then Some [f Substitution.id, theta] 
      else None             
  | Pred ("toehold", [Pat (Pattern.Inner [SiteT.Site (Unbound (Dom d)), _])])
  | Pred ("toehold", [Pat (Pattern.Inner [SiteT.Site (Bound(Dom d, _)), _])]) 
    ->
    if d.isToehold 
      then Some [f Substitution.id, theta] 
      else None             
  | Pred ("unbound", [Pat (Pattern.Inner [SiteT.Site (Unbound (Dom _)), _])])
    ->
      Some [f Substitution.id, theta] 
  | _ -> None

let refreshTag cle idProvider tag =
  match tag with
  | NoTag  -> NoTag 
  | AnyTag -> AnyTag
  | Tag t  -> Tag (Term.refresh cle idProvider t)

let refreshDomain (cle : CLE<SiteT, Choice<DomainT, Bond>>) idProvider (d:Domain) = 
  if d.tag = NoTag || d.tag = AnyTag 
    then d
    else { d with tag = refreshTag cle idProvider d.tag }

let refreshDomainT cle idProvider = function 
  | DomainT.Var ((-1, "_"), tag) ->  let tag' = tag |> refreshTag cle idProvider
                                     DomainT.Var ((-1, "_"), tag')
  | DomainT.Var ((_, x), tag) ->  let tag' = tag |> refreshTag cle idProvider
                                  DomainT.Var ((idProvider x, x), tag')
  | DomainT.Dom d -> DomainT.Dom (refreshDomain cle idProvider d)

let refreshBond idProvider = function 
  | Bond.Bond b        -> Bond b
  | Bond.Var (-1, "_") -> Bond.Var (-1, "_")
  | Bond.Var (_, x)    -> Bond.Var (idProvider x, x)

let refreshSite cle idProvider = function 
  | Site.Bound (d, b) -> let d' = refreshDomainT cle idProvider d 
                         let b' = refreshBond idProvider b
                         Site.Bound (d', b')
  | Site.Unbound d    -> let d' = refreshDomainT cle idProvider d 
                         Site.Unbound d'
// TODO: refactor refresh into respective types
let refreshSiteT cle idProvider = function
  | SiteT.Site s        -> SiteT.Site (refreshSite cle idProvider s)
  | SiteT.Var (-1, "_") -> SiteT.Var (-1, "_")
  | SiteT.Var (_, x)    -> SiteT.Var (idProvider x, x)



let disambiguateVars (cle:CLE<SiteT, Choice<DomainT, Bond>>) (c:Clause<SiteT>) : Clause<SiteT> =
  let updateMap (m : Map<Var, bool*bool*bool*bool*bool>)                   // what has v been mapped to so far? A term, location, site, domain, bond?
                 v                                                         // variable
                (arg:Choice<Term<Site>, Location, SiteT, DomainT, Bond>) = // what type of element is v being mapped to?
    if m.ContainsKey v
      then m.Add (v, (match m.[v] with (a,b,c,d,e) -> match arg with 
                                                      | Choice1Of5 _ -> (true,b,c,d,e)
                                                      | Choice2Of5 _ -> (a,true,c,d,e)
                                                      | Choice3Of5 _ -> (a,b,true,d,e)
                                                      | Choice4Of5 _ -> (a,b,c,true,e)
                                                      | Choice5Of5 _ -> (a,b,c,d,true)))
      else m.Add (v, match arg with 
                     | Choice1Of5 _ -> (true,false,false,false,false)
                     | Choice2Of5 _ -> (false,true,false,false,false)
                     | Choice3Of5 _ -> (false,false,true,false,false)
                     | Choice4Of5 _ -> (false,false,false,true,false)
                     | Choice5Of5 _ -> (false,false,false,false,true))

  let rec getMapDomain m d =
    match d with 
    | DomainT.Var (v, tag) -> let m' = updateMap m v (Choice4Of5 d)
                              match tag with 
                              | NoTag 
                              | AnyTag -> m' 
                              | Tag t  -> getMapTerm m' t
    | DomainT.Dom dom      -> match dom.tag with
                              | NoTag 
                              | AnyTag -> m 
                              | Tag t ->getMapTerm m t

  and getMapBond m b =
    match b with 
    | Bond.Var v  -> updateMap m v (Choice5Of5 b)
    | Bond.Bond _ -> m
  and getMapLocation m l = 
    match l with 
    | Location.Var (x, y) -> let v = (x,y) in updateMap m v (Choice2Of5 l)
    | Location.Loc _      -> m
                             
  and getMapSite m s = 
    match s with 
    | SiteT.Var  v                  -> updateMap m v (Choice3Of5 s)
    | SiteT.Site (Site.Unbound d)   -> getMapDomain m d 
    | SiteT.Site (Site.Bound (d,b)) -> getMapBond (getMapDomain m d) b
  and getMapHole m (s:SiteT, l:Location) = getMapLocation (getMapSite m s) l
  
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
                                 |> List.fold (fun acc s -> s |> List.fold getMapSite acc) m
  and getMapTerm m = 
    function 
    | Term.Var (x, y) -> let v = (x, y)
                         if m.ContainsKey v
                          then m.Add (v, (match m.[v] with (_, a,b,c,d) -> (true,a,b,c,d))                 )
                          else m.Add (v, (true,false,false,false,false))
    | Term.Const _ 
    | Term.Float _   -> m
    | Func   (_, ts) 
    | TList  ts       -> ts |> List.fold getMapTerm m
    | TCons  (t1, t2) -> let m' = getMapTerm m t1 
                         getMapTerm m' t2
    | Proc p -> getMapProcess m p
    | Pat  p -> getMapPattern m p
    | Term.TMSet _ -> failwith "Multisets not supported in Logic DSD yet"
    | Term.TCRN _  -> failwith "CRNs not supported in Logic DSD yet"




  let getMapPredicate m = function 
                          | Predicate.Pred(_, args) -> args |> List.fold getMapTerm m
                        
  let getMapLit m = function 
                    | Pos p -> getMapPredicate m p
                    | Neg p -> getMapPredicate m p

  
  match c.head with
  | Pred (_, lits) -> 
    // Build a map [variable name -> set of variable types associated with that name in the clause]
    c.body 
      |> List.fold getMapLit (lits |> List.fold getMapTerm Map.empty)
      |> Map.fold (fun (clause:Clause<SiteT>) varName assignments -> 
        if varName = (-1, "_") then clause // skip wildcards
        else 
        match assignments with 
        | false, false, false, false, false 
        | true,  false, false, false, false 
        | false, true,  false, false, false 
        | false, false, true,  false, false 
        | false, false, false, true,  false 
        | false, false, false, false, true  -> clause
        | hasTVar, hasLVar, hasSVar, hasDVar, hasBond ->
          if hasBond
          then 
            if hasTVar
              then failwith <| sprintf "Operation unsupported: cannot use bond variable \"%s\" alone as a term (it must be used in a pattern, e.g. D!%s)" (snd varName) (snd varName)
              else failwith <| sprintf "Cannot unify bond variable \"%s\" with a %s variable" (snd varName)
                                (if   hasDVar then  "domain"
                                  elif hasSVar then  "site"
                                  else               "location")
          elif hasLVar then
            if hasSVar || hasDVar || hasBond
              then  failwith <| sprintf "Cannot unify location variable \"%s\" with a %s variable" (snd varName)
                                  (if   hasDVar then "domain"
                                   elif hasSVar then "site"
                                   else              "bond")
              elif not hasTVar 
                then clause
                else 
                  let pattern = Term.Pat <| Pattern.Inner [SiteT.Var (-1,"_"), Location.Var varName]
                  let sub     = Substitution<SiteT,_>.Create(varName, pattern).Add(varName, Choice2Of4 <| Location.Var varName, cle)
                  sub.Apply(clause,cle)
            
          else // disambiguate variable <varName>
            let rec substituteTerm (v:Var) (x:Term<SiteT>) ss (* site substitution *) (t:Term<SiteT>) : Term<SiteT>= 
              let f g x = 
                match x with 
                | NoTag -> NoTag 
                | AnyTag -> AnyTag 
                | Tag y -> Tag (g y)
              let mapSite s = 
                match s with 
                | SiteT.Site (Site.Unbound (DomainT.Dom d))         -> SiteT.Site (Site.Unbound (DomainT.Dom ( { d  with tag = f (substituteTerm v x ss) d.tag }))) 
                | SiteT.Site (Site.Bound (DomainT.Dom d, l))        -> SiteT.Site (Site.Bound (DomainT.Dom ( { d  with tag = f (substituteTerm v x ss) d.tag }), l)) 
                | SiteT.Site (Site.Unbound (DomainT.Var (x', tag))) -> if tag = NoTag then s else SiteT.Site (Site.Unbound (DomainT.Var (x', f (substituteTerm v x ss) tag)))
                | SiteT.Site (Site.Bound (DomainT.Var (x', tag),l)) -> if tag = NoTag then s else SiteT.Site (Site.Bound (DomainT.Var (x', f (substituteTerm v x ss) tag),l))
                | SiteT.Var v' -> match ss with 
                                  | None    -> s 
                                  | Some s' -> if v = v' then s' else s
              match t with 
              | Term.Var (v1, v2)   -> if v = (v1, v2) then x else t
              | Term.Const _        
              | Term.Float _        -> t
              | Term.Func (n, ts)   -> Term.Func (n, ts |> List.map (substituteTerm v x ss))
              | Term.TList ts       -> Term.TList (ts |> List.map (substituteTerm v x ss))
              | Term.TCons (t1, t2) -> Term.TCons (t1 |> substituteTerm v x ss, t2 |> substituteTerm v x ss)
              | Term.Proc p         -> p |> Process.Map (mapSite) 
                                         |> Term.Proc
              | Term.Pat p          -> p |> Pattern.Map (fun (s,l) -> mapSite s, l) 
                                         |> Term.Pat
              | Term.TMSet _        -> failwith "Multisets not supported in Logic DSD yet"
              | Term.TCRN _         -> failwith "CRNs not supported in Logic DSD yet"
            
            if hasDVar
              then 
                let site    = SiteT.Site   <| (Site.Unbound (DomainT.Var (varName, AnyTag)))
                let pattern = Term.Pat <| Pattern.Inner [site, Location.Var (-1,"_")]
                let f       = substituteTerm varName pattern (Some site)
                Clause.Map f clause
              else // can only be SVar at this point
                let site    = SiteT.Var varName
                let pattern = Term.Pat <| Pattern.Inner [site, Location.Var (-1,"_")]
                let f       = substituteTerm varName pattern None
                Clause.Map f clause
       ) c



let rec composer x y = dsdSubApply engine x (Choice1Of2 y)
and printer x = match x with 
                | Choice1Of2 x -> DomainT.ToString x
                | Choice2Of2 x -> Bond.ToString x
and dsdToCanonicalFormProcess x = dsdCanonicalForm engine  x
and dsdUnify                  x = unifySite engine         x
and dsdApplySub               x = dsdSubApply engine       x
and dsdApplySubVar            x = dsdSubApply2 engine      x
and dsdCompose                x = composer                 x
// and dsdComposeVar             x = dsdSubApply2 engine      x
and dsdRefresh                x = refreshSiteT engine      x
and dsdResolveGoal            x = resolveDsdGoal engine    x
and dsdDisambiguateVars       x = disambiguateVars engine  x
and dsdCompare                x = SiteT.Compare engine compare x
and dsdCanonicalSite          x = SiteT.Canonical engine x
and engine : CLE<SiteT, Choice<DomainT, Bond>> = 
  {
    toString               = SiteT.ToString 
    toStringTempVar        = printer // fun _ -> failwith "" 
    compare                = dsdCompare
    cast                   = fun (x:Choice<DomainT, Bond>) ->
                               match x with
                               | Choice1Of2 d -> SiteT.Site (Site.Unbound d)
                               | Choice2Of2 b -> SiteT.Site (Site.Bound (DomainT.Var((-1, "_"), AnyTag), b))
    toCanonicalForm        = dsdCanonicalSite 
    toCanonicalFormProcess = dsdToCanonicalFormProcess
    unify                  = dsdUnify
    underscore             = fun _ -> SiteT.Var(-1, "_")
    applyAll               = dsdApplySub
    applySub               = dsdCompose
    applySubVar            = dsdApplySubVar
    fvs                    = fvSiteT 
    disambiguateVars       = dsdDisambiguateVars
    ComposeProcesses       = logicDsdComposeProcesses false //  Process<'s> -> Process<'s> -> Process<'s>
    refresh                = dsdRefresh
    resolveGoal            = dsdResolveGoal
    domainKeywords         = ["!"; "{"; "@"]
    }















/////////////////
/////////////////
//// Parsing ////
/////////////////
/////////////////
let pbond idProvider = 
  choice [
    RulesDSD.Parser.pname |>> (idProvider >> Bond.Var)
    pint32 .>> spaces     |>> Bond.Bond
  ]

let rec psite cle  (idProvider : string -> (int*string)) isInitials : Parser.t<SiteT> = 
  let ptag st = Parser.choice [ Parser.braces (choice [ RulesDSD.Parser.pterm (psite cle idProvider isInitials) cle idProvider engine.domainKeywords |>> Tag 
                                                        preturn NoTag ]) 
                                preturn (if isInitials then NoTag else AnyTag) ] st // "st" avoids an infinite loop in the definition of ptag
  let pdomain = 
    // make sure not to parse Prolog terms by mistake
    Parser.plookAhead (RulesDSD.Parser.pname >>. choice [Parser.kw "(" >>. Parser.failParser "" >>= fun _ -> failwith "asdasdasd"
                                                         preturn ()] ) >>.
    // parse domain
    RulesDSD.Parser.pname >>= fun domName -> 
      if domName = "_" || System.Char.IsUpper (domName, 0)
        then // dom is a variable
          let domVar = if domName = "_" 
                         then (-1, "_") 
                         else idProvider domName
          ptag |>> fun tag -> DomainT.Var (domVar, tag)
        else // dom is a concrete domain
          choice [
            kw "^" >>. 
              choice [
                (kw "*" >>. ptag |>> fun tag -> DomainT.Dom (Domain.Create(domName, true, true, tag)))
                (ptag            |>> fun tag -> DomainT.Dom (Domain.Create(domName, false, true, tag)))
              ]
            (kw "*" >>. ptag |>> fun tag -> DomainT.Dom (Domain.Create(domName, true, false, tag)))
            (ptag            |>> fun tag -> DomainT.Dom (Domain.Create(domName, false, false, tag)))
          ]
  pdomain .>>. choice [kw "!" >>. pbond idProvider |>> Some 
                       preturn None]
          |>> fun (d, b) -> 
                match b with
                | None      -> let ud = SiteT.Site (Site.Unbound d)
                               match d with 
                               | DomainT.Var (x, tag)  -> if tag = AnyTag then SiteT.Var x else ud
                               | _                     -> ud
                | Some bond -> SiteT.Site (Site.Bound (d, bond))







//////////////////////
// Concrete process //
//////////////////////

(* The main purpose of the following definitions is to provide an intermediate data structure 'ProcessC' that strips a Syntax.Process
   of Term-related components, thus providing a simpler data structure to render in the tool. *)

// the suffix 'C' is for 'concrete', and it helps to separate this module's namespace from Syntax's namespace
type TagC = NoTagC | AnyTagC | TagC of string 
type DomainC = { name            : string
                 isComplementary : bool
                 isToehold       : bool
                 tag             : TagC } with
               override this.ToString() = this.name + (if this.isToehold then "^" else "") + (if this.isComplementary then "*" else "")
               end
type BondC    = int
type SiteC = BoundC   of DomainC * BondC
           | UnboundC of DomainC
           with override this.ToString() = match this with
                                           | BoundC (domain,bond) -> domain.ToString()+"."+bond.ToString()
                                           | UnboundC domain -> domain.ToString()
           end
type StrandC  = SiteC list
type ProcessC = StrandC list


let to_concrete_domain (dt:DomainT) : DomainC =
  match dt with
  | DomainT.Dom d ->
      { name            = d.name 
        isComplementary = d.isComplementary 
        isToehold       = d.isToehold
        tag             = match d.tag with 
                          | NoTag  -> NoTagC
                          | AnyTag -> AnyTagC
                          | Tag t  -> TagC(Term.ToString t) }
  | DomainT.Var ((_,v),_) -> failwith <| sprintf "Cannot create concrete domain from %s, which is a variable." v

let to_concrete_bond (b:Bond) =
  match b with
  | Bond.Bond i     -> i
  | Bond.Var (_, v) -> failwith <| sprintf "Cannot create concrete bond from %s, which is a variable." v
let to_concrete_site (st:SiteT) : SiteC =
  match st with 
  | SiteT.Site s -> match s with
                    | Site.Bound (d, b) -> let d' = to_concrete_domain d
                                           let b' = to_concrete_bond b
                                           BoundC(d', b')
                    | Site.Unbound d -> UnboundC <| to_concrete_domain d
  | SiteT.Var (_, v) -> failwith <| sprintf "Cannot create concrete site from %s, which is a variable." v

let to_concrete_strand (s:Strand<SiteT>) : StrandC = 
  s 
  |> List.map to_concrete_site

let to_concrete_process (Process.Proc p) : ProcessC =
  p 
  |> Map.toList
  |> List.map (snd >> to_concrete_strand)