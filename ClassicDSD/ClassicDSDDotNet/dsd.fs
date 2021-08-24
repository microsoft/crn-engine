// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Dsd
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open RulesDSD.Syntax
open RulesDSD.ProcessEquality
open Microsoft.Research.DNA.LogicDSD

type value = Expression.t<string>
type expression = Expression.t<Key<Species>>
type syntax = Syntax.t

type dom_colour =
  { domain : string
  ; colour : string }

type metadata =
  { meta : Sequence.mapping
  ; info : Species.t_env
  ; domain_colours : Hashtable.t<string,string>
  ; subdomains : Hashtable.t<string list, Domain.t> }

let empty_meta =
  let opts = Options.default_options in
  { meta = Sequence.initialise_mapping opts
  ; info = Species.empty_env 0
  ; domain_colours = Hashtable.empty ()
  ; subdomains = Hashtable.empty () }

type term = Crn
type populations = Populations<Species.t, expression>
type reaction = Reaction<Species.t, value, expression>
type pop_info = Population<Species.t, expression>
type event = Event<Species.t, float, expression>

let cle  = Microsoft.Research.DNA.LogicDSD.engine
let cle' = { cle with ComposeProcesses       = Microsoft.Research.DNA.LogicDSD.logicDsdComposeProcesses true } 

let build_meta opts info env dom_env species =
  let subdomains = Hashtable.empty () in
  let dom_cols = Hashtable.empty () in
  let find n toe =
    List.pick
     (function
       | (Value.Sys(s, d)) ->
          if s=n then
            if toe then Some (Domain.Toe (d, Domain.value.Float (1.0, Types.emprange), false, None))
            else Some (Domain.Normal (d, false, None))
          else None
       | _ -> None) env in
  let collect (d: Syntax.dom_assignment) =
    (match d.dom_colour with Some c -> Hashtable.add dom_cols d.domain c | None -> ());
    (match d.subdomains with Some (ds,toe) -> Hashtable.add subdomains ds (find d.domain toe) | None -> ()) in
  List.iter collect dom_env;
  let domains = species |> List.collect Species.domains
  { info = info
  ; meta = Sequence.add_domains (Sequence.initialise_mapping opts) domains
  ; domain_colours = dom_cols
  ; subdomains = subdomains }

let default_namer (opts:Options.t) (species_env:Species.t_env) : (Species.t -> string) =
  let cache = new System.Collections.Generic.Dictionary<Species.t,string>()
  let names = new System.Collections.Generic.HashSet<string>()
  let namer sp : string =
    if cache.ContainsKey sp then
      cache.Item sp
    else
      let sn = Species.try_find_name opts species_env true sp
      let name =
        match sn with
        | Some species_name ->
          species_name
        | None ->
            match sp with
            | Species.UNKNOWN species_name -> species_name
            | _ -> let mutable unique_name = ""
                   let mutable c = names.Count + 1
                   while unique_name = "" do
                     if not (names.Contains ("sp" + string c)) then
                         unique_name <- "sp" + string c
                     else c <- c + 1
                   unique_name

      cache.Add(sp, name);
      ignore(names.Add(name));
      name
  namer

let apply_infinite_reactions sub_map opts senv pops =
  let upd_pops_meta meta (pops:Populations<Species.t,Functional>) =
    let fold (acc_pops, acc_meta) (pi: Population<Species.t, expression>) =
      let products =
        match Species.initial_reactions sub_map opts pi.species with
        | [] -> []
        | [ps] -> if pi.initial then List.map (Species.standard_form opts) ps (* RLP: test, initial_reactions should normalize *)
                  else failwith ("Error: " + (Lib.quote "fast_reactions") + " function has missed an infinite reaction.")
        | _ -> if pi.initial then Errors.conflicting_initial_error pi.species
               else failwith ("Error: " + (Lib.quote "fast_reactions") + " function has missed some infinite reactions.") in
      if products.IsEmpty then (* s stays in the mix *)
        Populations.merge Expression.add acc_pops (Populations.create [pi]), acc_meta
      else (* products are added instead of s *)
        let product_pis = List.map (fun s -> { Population.species  = s
                                             ; Population.value    = pi.value
                                             ; Population.constant = pi.constant
                                             ; Population.initial  = pi.initial (* these should all be initial, but why not be generic *)
                                             ; Population.input    = false (* not input, we just built it *)
                                             ; Population.spatial_initial = pi.spatial_initial
                                             ; Population.max      = pi.max
                                             }) products in
        let new_pops = Populations.merge Expression.add acc_pops (Populations.create product_pis) in
        (new_pops, Dsdreaction.new_species_env opts products acc_meta) in
    let pis = pops.get_pop_info_list in
    List.fold fold (Populations.empty (), meta) pis in
  let rec apply_irs meta (pops:Populations<Species.t,Functional>) =
    let ss = pops.get_calculus_species_list in
    let new_pops, new_meta = upd_pops_meta meta pops in
    let new_ss = new_pops.get_calculus_species_list  in
    let _(*seen_ss*), unseen_ss = List.partition (fun s -> List.contains s ss) new_ss in
    if unseen_ss.IsEmpty then new_pops (* fix point reached *)
    else apply_irs new_meta new_pops in
  apply_irs senv pops

let resolvePlotMissing (env:Syntax.proc_env) (p:Syntax.t_plot_base) : Species.t option =
  match p with
  | Syntax.t_plot_base.String x       -> failwith ("Unexpected plot " + x)
  | Syntax.t_plot_base.Molecule m     -> Some m
  | Syntax.t_plot_base.Module (n, _) -> 
      // type t_bind = Def of string * parameter list * (value list -> bool)  * t
      let matches ((Process.t_bind.Def (moduleName, _, _, p) : Process.t_bind)) = 
        if moduleName = n
          then match p with 
               | Process.Strand  (s, _)          -> Some (Species.STRAND s)
               | Process.Gate    (g, _)          -> Some (Species.GATE g)
               | Process.Origami _               -> failwith <| "Cannot plot uninstantiated origami module " + n
               | Process.LogicDsdProcess (pr, _) -> Some (Species.LogicDsdProcess pr)
               | Process.Repeat   _
               | Process.Instance _
               | Process.Parallel _
               | Process.New      _
               | Process.Chemical _              -> None
          else None
      List.tryPick matches env 
  
let resolvePlot (env:Process.species_env) (p:Syntax.t_plot_base) : Species.t option =
  match p with
  | Syntax.t_plot_base.String x       -> failwith ("Unexpected plot " + x)
  | Syntax.t_plot_base.Molecule m     -> Some m
  | Syntax.t_plot_base.Module (n, _) -> 
      let speciesAndInfo = Stringmap.tryFind n (fst env)
      match speciesAndInfo with
        | Some moduleInstances -> let zz = 
                                    moduleInstances
                                    |> List.map (fun (sp, _,_,_,_,_) -> sp )  
                                    |> Seq.distinct 
                                    |> Seq.filter (fun sp -> let e = match snd env  with | Species.t_disp.Names (e, x) -> e
                                                             let r = Stringmap.find (sp |> Species.display) e
                                                             r = n)
                                    |> Seq.toList
                                  match zz with 
                                    | [sp] -> Some sp
                                    | []   -> None
                                    | _ -> failwith ("Module \"" + n + "\" produces two or more species, but only one species can be plotted")
        | None   -> None


type ClassicBundle = 
    { settings   : Crn_settings<Expression.t<Key<Species.t>>>
    ; meta       : metadata
    ; sreactions : Process.sreaction list
    ; initials   : Initial<Species.t, Species.prim> list 
    ; dsdOptions : Options.t}
    
type RulesBundle =    
    { settings   : Crn_settings<Expression.t<Key<Process<SiteT>>>>
    ; initials   : Initial<Process<SiteT>, Expression.t<string>> list 
    ; reactions  : Reaction<Process<SiteT>
                           , Expression.t<string>
                           , Expression.t<Key<Process<SiteT>>>> list
    ; plotsCache : System.Collections.Generic.Dictionary<Process<SiteT>,string>
    ; dsdOptions : Options.t
    ; rules      : RulesProgram<SiteT> }
    
type LogicBundle =    
    { settings   : Crn_settings<Expression.t<Key<Term<SiteT>>>>
    ; initials   : Initial<Term<SiteT>, Expression.t<string>> list 
    ; reactions  : Reaction<Term<SiteT>
                           , Expression.t<string>
                           , Expression.t<Key<Term<SiteT>>>> list
    ; plotsCache : System.Collections.Generic.Dictionary<Term<SiteT>,string>
    ; dsdOptions : Options.t
    ; rules      : RulesProgram<SiteT> }

type bundle =
  | ClassicDSD of ClassicBundle
  | Rules   of RulesBundle
    
let get_options b = match b with ClassicDSD b -> b.dsdOptions | Rules b -> b.dsdOptions
let set_options b o = match b with ClassicDSD b -> ClassicDSD {b with dsdOptions = o} | Rules b -> Rules {b with dsdOptions = o}

let toLogicDomain (d:Domain.t) : Microsoft.Research.DNA.LogicDSD.Domain =
  let makeName n i = if i <= 0 then n else sprintf "_%s_%i"n i
  match d with 
  | Domain.t.Normal(Value.DomainS(n,i,_,_,_,_), b, _)  // we ignore information associated with the domain other name and complement
  | Domain.t.Normal(Value.Domain (n,i,_,_,_), b, _)   -> {name = makeName n i
                                                          isToehold = false 
                                                          isComplementary = b
                                                          tag = NoTag }
  | Domain.t.Normal(Value.Variable(n,_) , b, _) -> {name = n
                                                    isToehold = false
                                                    isComplementary = b
                                                    tag = NoTag }
  | Domain.t.Toe(Value.Variable(n,_), c, b, _)  -> {name = n
                                                    isToehold = true
                                                    isComplementary = b
                                                    tag = match c with 
                                                          | Domain.value.Float    (x,_) -> if x <> 1.0 then Term.Func ("complementarity", [Term.Float x]) |> Tag else NoTag
                                                          | Domain.value.Variable (x,_) -> Term.Func ("complementarity", [Term.Const x]) |> Tag
                                                          | _ -> NoTag }
  | Domain.t.Toe(Value.DomainS(n,i,_,_,_,_), c, b, _)   
  | Domain.t.Toe(Value.Domain(n,i,_,_,_), c, b, _)  -> {name = makeName n i
                                                        isToehold = true
                                                        isComplementary = b
                                                        tag = match c with 
                                                              | Domain.value.Float    (x,_) -> if x <> 1.0 then Term.Func ("complementarity", [Term.Float x]) |> Tag else NoTag
                                                              | Domain.value.Variable (x,_) -> Term.Func ("complementarity", [Term.Const x]) |> Tag
                                                              | _ -> NoTag }
  | _ -> failwith "Conversion failed."

let toUnboundSite (d:Domain.t) : Microsoft.Research.DNA.LogicDSD.SiteT = d |> toLogicDomain |> Dom |> Unbound |> Site
let logicComplement (d: Microsoft.Research.DNA.LogicDSD.Domain) = { d with isComplementary = not d.isComplementary }

type FlatGate = SJoinTop    of Segment.t * FlatGate
              | SJoinBottom  of Segment.t * FlatGate
              | SEndWith   of Segment.t


let flattenGate (g:Gate.t) (*st: FlatGate option*) : FlatGate =
  let rec flGate gate =
    match gate with
    | []
    | []::_             -> failwith "Unexpected empty strand in Classic to Logic DSD translation."
    | [[s]]             -> SEndWith s
    | [s::rest]         -> let restGate = flGate [rest] 
                           SJoinBottom (s, restGate)
    | [s]::rest         -> let restGate = flGate rest
                           SJoinTop (s, restGate)
    | (s::rest1)::rest2 -> let restGate = flGate (rest1::rest2)
                           SJoinBottom (s, restGate)
  
  // reverse the gate, and start flattening from the end
  let g' = g// |> List.map List.rev // |> List.rev
  flGate g'



type IntermediateStrand = Single of RulesDSD.Syntax.Strand<SiteT>
                        | Double of RulesDSD.Syntax.Strand<SiteT> * RulesDSD.Syntax.Strand<SiteT>
let toLogicProcess (g:FlatGate) : RulesDSD.Syntax.Process<SiteT> = 
  let counter = ref 0
  let toStrand : Domain.t list -> Strand<SiteT> = 
    List.map (toLogicDomain >> Dom >> Unbound >> Site) 
  
  let toDoubleStrand (s:Domain.t list) : Strand<SiteT> * Strand<SiteT> =
    s  
    |> List.fold (fun (top, bot) d -> 
        let d'      = toLogicDomain d
        let d''     = logicComplement d'
        let newBond = Bond !counter
        counter    := !counter + 1
        let sTop    = SiteT.Site (Site.Bound (Dom d', newBond))
        let sBottom = SiteT.Site (Site.Bound (Dom d'', newBond))
        sTop::top, sBottom :: bot
      ) ([], [])
    |> fun (top, bottom) -> (top |> List.rev, bottom)

  let segmentToStrands (s:Segment.t) = 
    match s with 
    (* {LB}<LT>[S]<RT>{RB} *)
    | Segment.Double (lb, lt, s, rt, rb) -> 
        let lb'     = lb |> List.rev |> toStrand
        let lt'     = lt |> toStrand
        let s', ss' = s  |> toDoubleStrand
        let rt'     = rt |> toStrand
        let rb'     = rb |> List.rev |> toStrand

        Double ( lt' @ s'  @ rt'
                , rb' @ ss' @ lb')
    (* {OB}<OT>[S]{HP> *)
    | Segment.Hairpin (ob, ot, s, hp, Segment.Right) -> 
        let ob'     = ob |> toStrand
        let ot'     = ot |> toStrand
        let s', ss' = s  |> toDoubleStrand
        let hp'     = hp |> toStrand
        Single (ot' 
                @ s' 
                @ (List.rev hp')
                @ ss'
                @ (List.rev ob'))

    (* <HP}[S]<OT>{OB} *)
    | Segment.Hairpin (ob, ot, s, hp, Segment.Left)  -> 
        let ob'     = ob |> toStrand
        let ot'     = ot |> toStrand 
        let s', ss' = s  |> toDoubleStrand 
        let hp'     = hp |> toStrand
        Single ((List.rev ob') 
                @ ss' 
                @ (List.rev hp')
                @ s'
                @ ot')

  let rec toProc (g:FlatGate) =
    match g with 
    | SEndWith s          -> segmentToStrands s, Set.empty
    | SJoinTop (s,g')     -> let sr, acc = toProc g'
                             match sr with
                             | Single tr -> 
                                match segmentToStrands s with 
                                | Single tl       -> Single (tl @ tr), acc
                                | Double (tl, bl) -> Double (tl@tr, bl), acc
                             | Double (tr, br) -> 
                                let acc' = acc.Add br
                                match segmentToStrands s with 
                                | Single tl       -> Single (tl @ tr), acc'
                                | Double (tl, bl) -> Double (tl @ tr, bl), acc'
    | SJoinBottom (s, g') -> let sr, acc = toProc g'
                             match sr with
                             | Single br -> 
                                match segmentToStrands s with 
                                | Single bl       -> Single (br @ bl), acc
                                | Double (tl, bl) -> Double (tl, br@bl), acc
                             | Double (tr, br) -> 
                                let acc' = acc.Add tr
                                match segmentToStrands s with 
                                | Single bl       -> Single (br @ bl), acc'
                                | Double (tl, bl) -> Double (tl, br @ bl), acc'
  match toProc g with
  | Single s, acc -> acc.Add s |> Set.toList |> Process.OfList
  | Double (s1, s2), acc -> acc.Add s1 |> Set.add s2 |> Set.toList |> Process.OfList

let parse_extended toeholds specificities code =
  match code |> Parser.from_string DSDParser.parser with
  | ((task, z, opts, maybeRules), s ) ->
    let opts = Options.setToeholdsText toeholds opts
    let opts = Options.setSpecificitiesText specificities opts
    let mod_plots : Syntax.t_plot_base list = [] 

    (* build base environment from Filzbach parameters *)
    let base_env = z.parameters |> List.map (fun (p:Parameter) -> p.name, p.value) |> Types.bindings_to_env
  
    (* Expand the syntax and build environment *)
    let counter,names,p,edef,evals,dom_env = Syntax.expand opts base_env mod_plots s
  
    (* Translate the process into a list of species, using the environment to allow tracking of module calls for plotting *)
    let ((_(*names*),_(*counter*)),species_env,reactions) = Process.eval_to_species opts (names,counter) edef evals p in
    let species_list = Species.env_to_species species_env in
    Species.check (List.map (fun (pi:Species.sim_t,_) -> pi.species) species_list) |> ignore
  
    (* translate modules to species in the plots *)
    let sanitize_species x = 
      match resolvePlot species_env x with
      | Some sp -> Some (Species.eval evals sp |> Species.erasePos)
      | None    -> match resolvePlotMissing edef x with
                   | Some sp -> Some (Species.eval evals sp |> Species.erasePos)
                   | None    -> None   // removes plots not mentioned in the DSD program
    
    let sane_z = z
                 // resolve plots AND rate expressions, if possible
                 |> fun z -> z.map (Expression.map (Key.map sanitize_species))
                 // filter out unresolved species (marked by 'None')
                 |> fun z -> z.collect_plots
                               (fun sanitizedPlot -> 
                                  if sanitizedPlot 
                                     |> Expression.mentions
                                     |> List.exists (fun k -> match k with 
                                                              | Key.Species None -> true
                                                              | _ -> false)
                                    then []
                                    else [sanitizedPlot])
                 // convert settings from 'Species.t option' to 'Species.t'
                 |> fun z -> z.map 
                              (Expression.map (Key.map (fun x -> match x with 
                                                                 | Some sp -> sp
                                                                 | None    -> failwith "Unexpected null plot")))
    (* Generate calculus specific to the rxn reactions. *)
    let pick_unique_rate (s: Process.sreaction seq) =
      if s |> Seq.pairwise |> Seq.forall (fun (a,b) -> (*Reaction.rateEquals*) a.rate = b.rate) then
        s |> Seq.head
      else failwith "Multiple rate definitons for same rxn reaction" in
  
    let reactions =
      reactions
       |> Reaction.normalise_list
       |> Seq.ofList
       |> Seq.groupBy (fun r -> r.reactants, r.products)
       |> Seq.map (snd >> pick_unique_rate)
       |> Seq.toList in
  
    let declaredInitials =
      species_list // TODO run infinite reactions
      |> List.map (fun (pi, t) -> Initial.create(pi.constant, pi.value, pi.species, t, pi.spatial_initial))
    let zero : value = Expression.Float 0.0

    // custom reactions may include undeclared species, which must be added to the initials
    let reactionInitials = reactions 
                           |> List.collect (fun r -> r.allSpecies)
                           |> List.filter (fun x -> not (List.contains x (species_list |> List.map (fun (pi:Species.sim_t,_) -> pi.species) )))
                           |> Seq.distinct |> Seq.toList
                           |> List.map (fun sp -> Initial.create(false, zero, sp, None, None))

    let initials  = declaredInitials @ reactionInitials

    let meta = build_meta opts species_env evals dom_env (initials |> List.map (fun i -> i.species)) in
    let bundle = match maybeRules with
                 | None -> 
                     ClassicDSD
                       { settings   = sane_z
                       ; meta       = meta
                       ; sreactions = reactions
                       ; initials   = initials 
                       ; dsdOptions = opts }
                 | Some rules -> 
                     let toLogicDSDProcess (x:Species.t) : RulesDSD.Syntax.Process<SiteT> =
                       match x with
                         | Species.STRAND  s ->
                           let toStrand = List.map (toLogicDomain >> Dom >> Unbound >> Site)
                           match s with 
                           | Strand.Upper ds -> [ds |> toStrand ] |> RulesDSD.Syntax.Process.OfList
                           | Strand.Lower ds -> [ds |> List.rev |> toStrand ] |> RulesDSD.Syntax.Process.OfList
                         | Species.GATE    g -> let fg = g |> flattenGate 
                                                let p  = fg |> toLogicProcess
                                                p
                         | Species.ORIGAMI _ -> failwith "Origami's are not supported in Logic DSD yet."
                         | Species.UNKNOWN x -> failwith <| sprintf "Unknown species %s" x
                         | Species.LogicDsdProcess p -> p
                       |> RulesDSD.Syntax.Process.Canonical cle
               
                     let settings'  = sane_z.map (Expression.map (Key.map (toLogicDSDProcess)))
                     let plotsCache = 
                       match snd meta.info with 
                       | Species.t_disp.Names (m, x) -> 
                         m 
                         |> Map.toList
                         |> List.map (fun (p, name) -> 
                             p 
                             |> Parser.from_string (DSDParser.parse_molecule_dots false) 
                             |> DSDParser.to_species (-1, -1)
                             |> toLogicDSDProcess
                             , name)
                         |> Map.ofList
                         :> System.Collections.Generic.IDictionary<_,_>
                         |> System.Collections.Generic.Dictionary
                       
               
                     Rules 
                       { settings   = settings'
                       ; initials   = initials 
                                       |> List.map (Initial.mapSpecies toLogicDSDProcess)
                                       |> List.groupBy (fun i -> i.species, i.constant, i.spatial, i.time)
                                       |> List.map (fun ((sp,c,spat,time), rest) -> 
                                                           let v = Expression.Plus (rest |> List.map (fun i -> i.value))
                                                                   |> Expression.simplify
                                                           Initial.create(c, v, sp, time, spat))
                       ; reactions  = reactions
                                       |> List.map (Reaction.map toLogicDSDProcess id (Expression.map (Key.map toLogicDSDProcess)))
                       ; plotsCache = plotsCache
                       ; dsdOptions = opts 
                       ; rules      = rules 
                                      :> System.Collections.Generic.IDictionary<_,_>
                                      |> System.Collections.Generic.Dictionary }
    bundle,task
  
let parse code = let (bundle,_) = parse_extended (Options.getToeholdsText Options.default_options) (Options.getSpecificitiesText Options.default_options) code in bundle

let to_engine_species namer s =
  namer s |> Species.create

let get_species_attributes namer (b:ClassicBundle) : (Species.t -> Attributes) =
  let rendering = Options.getRendering b.dsdOptions
  let render s =
    match rendering.renderer with
    | Options.Branches ->
        let style = BranchesRenderer.species_style b.meta.domain_colours (b.initials |> List.map (fun i -> i.species))
        s |> BranchesRenderer.species_to_svg_branches rendering.arrange rendering.mode b.meta.meta rendering.rotate_labels style
    | Options.Circles ->
        let style = BranchesRenderer.species_style b.meta.domain_colours (b.initials |> List.map (fun i -> i.species))
        s |> CirclesRenderer.species_to_svg_circles style
    | Options.Classic ->
        let normalise = rendering.mode <> Options.Nucleotides
        let style = Svg.species_style b.meta.domain_colours (b.initials |> List.map (fun i -> i.species))
        s |> Svg.species_to_svg_mode rendering.mode b.meta.meta |> Svg.to_string_normalise normalise style
  fun (s:Species.t) ->
    { name      = namer s
    ; structure = Species.display s
    ; svg       = render s }

let createRulesEmptyCache () = new System.Collections.Generic.Dictionary<Process<SiteT>,string>(), ref 0
let getRulesDSDNamer (cache:System.Collections.Generic.Dictionary<Process<SiteT>,string>, counter : int ref) = 
  // let counter = ref 0
  let namer (sys : Process<SiteT>)= 
    if cache.ContainsKey sys
      then cache.Item(sys)
      else 
        let n = !counter
        counter := n + 1
        let freshName = sprintf "sp_%i" n
        cache.Add(sys, freshName)
        freshName

  namer

let rulesRenderer (options:Options.t) (namer:Process<SiteT> -> string) (assigned:System.Collections.Generic.Dictionary<string,string>) (s:Process<SiteT>) : Attributes = 
  let graphics =
    match options.rendering.renderer with
    | Options.Circles ->
      let style = BranchesRenderer.species_style assigned [Species.LogicDsdProcess s]
      CirclesRenderer.species_to_svg_circles style (Species.LogicDsdProcess s)
    | Options.Classic
    | Options.Branches -> 
      let style = BranchesRenderer.species_style assigned [Species.LogicDsdProcess s]
      BranchesRenderer.species_to_svg_branches options.rendering.arrange options.rendering.mode Sequence.empty options.rendering.rotate_labels style (Species.LogicDsdProcess s)
  { name      = namer s
  ; structure = s |> Term.Proc |> Term.ToStringWith Microsoft.Research.DNA.LogicDSD.engine
  ; svg       = graphics   }
    
let convert_unexpanded (bundle:bundle) =
  match bundle with 
  | ClassicDSD bundle ->
    let namer = default_namer bundle.dsdOptions bundle.meta.info
    let dsd_dummy = { Calculus.react = fun _ _ -> [] }
    // Expand the calculus to a CRN
    // TODO: Case with no species except in react-style reactions
    //       Might not get the species, then

    let equalsSpecies = Species.equal bundle.dsdOptions
    let plotMatcher = Species.matches bundle.dsdOptions
    let crn = Crn.from_calculus_translated 
                (to_engine_species namer)
                (get_species_attributes namer bundle)
                equalsSpecies 
                plotMatcher 
                (fun sp -> (Species.free_names sp).IsEmpty)
                "" 
                bundle.settings 
                dsd_dummy 
                bundle.initials 
                bundle.sreactions
                (Options.get_is_jit bundle.dsdOptions)
                (Options.get_is_jit bundle.dsdOptions)
                false
             |> Crn.group_reactions
    crn.initialise()
  | Rules bundle -> 
    let dsd_dummy = { Calculus.react = fun _ _ -> [] }
    let equalsSpecies (sys1:Process<SiteT>) (sys2:Process<SiteT>) : bool = 
      sys1 = sys2

    let namer = getRulesDSDNamer (createRulesEmptyCache ())
    let colors = new System.Collections.Generic.Dictionary<string,string>()
    let colors = BranchesRenderer.dom_map colors (List.map (fun (i:Initial<Process<SiteT>,Expression.t<string>>) -> Species.LogicDsdProcess i.species) bundle.initials)
    Crn.from_calculus_translated 
              (to_engine_species namer)
              (rulesRenderer bundle.dsdOptions namer colors)
              equalsSpecies 
              equalsSpecies // TODO: plot matcher for Strand Graphs
              (fun _ -> true)
              "" 
              bundle.settings 
              dsd_dummy 
              bundle.initials 
              []
              (Options.get_is_jit bundle.dsdOptions)
              (Options.get_is_jit bundle.dsdOptions)
              false

let makeDsdCalculus (b:bundle) =
 match b with 
 | ClassicDSD bundle ->
   { Calculus.react =
         (fun ss s -> 
           let new_rs, _, _(*new_ss, new_env*) = Dsdreaction.add_new_species_reactions bundle.meta.subdomains bundle.settings bundle.dsdOptions bundle.meta.meta s ss bundle.meta.info [] bundle.sreactions
           new_rs) // TODO: This may give many duplicated reactions
    } 
 | Rules _ -> failwith "ClassicDSD/RulesDSD calculus integration not available yet."

// let debugMap = ref Map.empty // uncomment "debugMap" comments to enable performance profiling of Logic DSD
 
let to_rules_calculus b =
    let b = match b with Rules b -> b | _ -> failwith "Rules bundle expected"
    // prepare the set of "reaction" predicates to run in the calculus
    let reactionKinds = 
      let key = "reaction", 3
      if not <| b.rules.ContainsKey key
        then []
        else
      b.rules.Item key 
      |> Set.filter (fun s -> s.head.Name = "reaction" 
                              && match s.head.Args.Head with 
                                 | Term.TList _ -> true 
                                 | Term.Var _  -> true
                                 | _ -> failwith <| sprintf "The first argument of %s must be a list or a variable." s.head.String)
      |> Set.map (fun s -> match s.head.Args.Head with 
                           | Term.TList ts -> ts.Length, s.head
                           | Term.Var _    -> -1, s.head
                           | _  -> failwith "")
      |> Seq.fold (fun (acc:Map<int, Predicate<SiteT> list>) (k:int,v) -> if acc.ContainsKey k
                                                                            then Map.add k (v::acc.[k]) acc
                                                                            else Map.add k [v] acc) Map.empty
      |> Map.toList
    
    {
      react = fun (oldSystems:Process<SiteT> list) (newSystem:Process<SiteT>) -> 
        // combine each complex Q in oldSystems with complex P in newSystem, to form (P | Q)
        let rec combo n : Term<SiteT> list list =
          if n <= 0 
            then []
            else let ccombo = combo (n-1)
                 if ccombo.IsEmpty
                  then oldSystems |> List.map (fun p -> [Term.Proc p])
                  else
                 oldSystems 
                 |> List.collect (fun p -> 
                    ccombo 
                    |> List.map (fun ts -> let t = Term.Proc p
                                           t :: ts))
        // build the available "reaction"s given the new and old processes
        let queries = 
          reactionKinds
          |> List.collect 
              (fun (reactantsStoichiometry, preds) ->
                match reactantsStoichiometry with
                | -1 -> // predicates of the form reaction(Var, Rate, Qs)
                        preds |> List.map (fun p -> Pred (p.Name, Term.Proc newSystem :: (p.Args.Tail)))
                | 0  -> preds
                | 1  -> preds |> List.map (fun p -> Pred (p.Name, TList [Term.Proc newSystem] :: (p.Args.Tail)))
                | _  -> 
                        let oldReactants = combo (reactantsStoichiometry - 1)
                        preds 
                        |> List.collect (fun p -> 
                          oldReactants 
                          |> List.map (fun rs -> 
                            let procs        = match ProcessEquality.Flatten cle' <| Func ("|", Term.Proc newSystem :: rs) with
                                                | Some p -> p 
                                                            |> Process<SiteT>.Canonical cle' 
                                                            |> Microsoft.Research.DNA.LogicDSD.toDsdComplexes
                                                            |> List.map Term.Proc
                                                | None -> failwith""
                            let newReactants = Term.TList procs
                            let newArgs      = newReactants :: p.Args.Tail
                            Pred (p.Name, newArgs)
                            ))
            )
          |> List.map Literal.Pos

        let errorMsg t = 
          failwith <| sprintf "Unexpected term %s found in reaction enumeration; expecting a process or a list of processes." (Term.ToStringWith Microsoft.Research.DNA.LogicDSD.engine t)
        
        // query the Prolog engine and create reactions if any reaction predicate matched
        queries
        |> List.choose (fun query -> 
          match RulesDSD.Resolution.resolveAll query b.rules cle (*debugMap*) with 
          | None -> None
          | Some sols ->
            let procToMSet (t : Term<SiteT>) (toCanonize : bool ) : Mset.t<Process<SiteT>> =
              let toMSet procs : Mset.t<Process<SiteT>> =
                procs 
                |> List.sortWith (Process<SiteT>.Compare cle)
                |> List.countBy id
                |> List.map (fun (sp, count) -> { element = sp; multiplicity = count })
              let removeAnyTags p = 
                let removeAnyTag (d:DomainT) =
                  match d with 
                  | DomainT.Dom dom -> if dom.tag = AnyTag 
                                        then DomainT.Dom { dom with tag = NoTag }
                                        else d  
                  | DomainT.Var (v, AnyTag) -> DomainT.Var (v, NoTag) 
                  | _ -> d
                p |> Process.Map (fun x -> match x with 
                                                    | SiteT.Site s -> match s with 
                                                                      | Site.Bound (d,b) -> (removeAnyTag d, b) |> Site.Bound |> SiteT.Site
                                                                      | Site.Unbound d -> d |> removeAnyTag |> Site.Unbound |> SiteT.Site
                                                    | SiteT.Var v -> x)
              match t with 
              | Proc p' -> 
                  let p = removeAnyTags p'
                  if toCanonize
                    then p |> Process.Canonical cle    // sorts all complexes in canonical form
                           |> Microsoft.Research.DNA.LogicDSD.toDsdComplexes 
                           |> List.map (Process.Canonical cle)
                    else [p]
                  |> toMSet 
              | TList ts -> if toCanonize
                              then 
                                  let qs = ts 
                                           |> List.map (ProcessEquality.Flatten cle)
                                           
                                  if qs |> List.exists Option.isNone
                                    then failwith""
                                    else 
                                      let q = qs 
                                              |> List.map (Option.get >> Process.ToList)
                                              |> List.concat
                                              |> Process.OfList
                                              |> removeAnyTags
                                              |> Term.Proc
                                      match ProcessEquality.Flatten cle q with 
                                       | Some p -> p 
                                                   |> Process<SiteT>.Canonical cle// sorts complexes in canonical form
                                                   |> Microsoft.Research.DNA.LogicDSD.toDsdComplexes
                                                   |> List.map (Process.Canonical cle)
                                                   |> toMSet 
                                       | _ -> errorMsg t
                              else ts 
                                    |> List.map (fun x -> match x with 
                                                          | Term.Proc p -> removeAnyTags p
                                                          | _ -> failwith"")
                                    |> toMSet
              | _ -> errorMsg t
          
            let rec termToCrnExpr t : Value = 
              match t with 
              | Term.Float i   -> Expression.Float (float i)
              | Term.Const n -> Expression.Key n
              | Term.Func("*", ts) -> ts 
                                      |> List.map termToCrnExpr
                                      |> Expression.Times
              | Term.Func("+", ts) -> ts 
                                      |> List.map termToCrnExpr
                                      |> Expression.Plus
              | Term.Func("-", [t1; t2]) ->  Expression.Minus {sub1 = termToCrnExpr t1; sub2 = termToCrnExpr t2}
              | Term.Func("**", [t1; t2]) -> Expression.Power {base_ = termToCrnExpr t1; exponent = termToCrnExpr t2}
              | Term.Func("/", [t1; t2]) ->  Expression.Divide {div1 = termToCrnExpr t1; div2 = termToCrnExpr t2}
              | Term.Func(op, _)  -> failwith <| sprintf "Conversion to rate not support for operation \"%s\"" op
              | Term.Proc _     -> failwith <| sprintf "Conversion to functional rates not support yet"
              | _ -> failwith <| sprintf "Unexpected term \"%s\"in rate conversion." (Term.ToStringWith Microsoft.Research.DNA.LogicDSD.engine t)
            sols 
            |> List.map (fun theta -> 
              match theta.Apply(query, cle) with
              | Pos (Pred ("reaction", [P; Rt;  Q])) -> 
                // reactants are already in canonical form by construction
                let reactants = procToMSet P true
                let rate : Rate<Value, Expression.t<Key<Process<SiteT>>>> = 
                  Rt 
                  |> termToCrnExpr  
                  |> Rate.MassAction
                let products  = procToMSet Q true
                                  // |> List.sort
                let r = Reaction.create(reactants, None, rate, products)
                r
              | _ -> failwith ""  )
            |> Some
          )
        |> List.concat
        |> List.distinct
        |> List.filter (fun r -> r.reactants <> r.products)
      }

let convert_expand (b:bundle) =
  match b with 
  | ClassicDSD bundle ->
    let namer = default_namer bundle.dsdOptions bundle.meta.info in
  
    // Expand the calculus to a CRN
    let dsdCalculus   = makeDsdCalculus b
    let equalsSpecies = Species.equal bundle.dsdOptions
    let plotMatcher   = Species.matches bundle.dsdOptions
    let crn = Crn.from_calculus_translated 
                (to_engine_species namer) 
                (get_species_attributes namer bundle)
                equalsSpecies
                plotMatcher
                (fun sp -> (Species.free_names sp).IsEmpty)
                "" 
                bundle.settings
                dsdCalculus  
                bundle.initials 
                bundle.sreactions
                false
                true
                false
              |> Crn.group_reactions
    crn.initialise()
  | Rules bundle -> 
    let namer = getRulesDSDNamer (createRulesEmptyCache ()) // bundle.spCache
    let rulesCalculus = to_rules_calculus b
    let colors = new System.Collections.Generic.Dictionary<string,string>()
    let colors = BranchesRenderer.dom_map colors (List.map (fun (i:Initial<Process<SiteT>,Expression.t<string>>) -> Species.LogicDsdProcess i.species) bundle.initials)
    //debugMap := Map.empty
    let crn = Crn.from_calculus_translated 
                (to_engine_species namer) 
                (rulesRenderer bundle.dsdOptions namer colors)
                (=) // process equality
                (=) // process equality // TODO: implement plot pattern matching for systems
                (fun _ -> true)
                "" 
                bundle.settings
                rulesCalculus  
                bundle.initials 
                []
                false
                true
                false
              |> Crn.group_reactions
    (* // see commented out declaration of debugMap to enable Logic DSD performance profiling
    !debugMap 
    |> Map.toList
    |> List.sortWith (fun x y -> System.TimeSpan.Compare(snd x, snd y))
    |> List.iter (fun (x, y) -> printf "%s,%s\n" (y.ToString("G").Substring(8)) x)
    *)
    crn.initialise()

(* "Compile" a program, which involves parsing, typechecking and compiling it.
   We also fill in the species, but do not perform expansion. *)
let compile text = parse text |> convert_expand
let compile_extended toeholds specificities text = let (bundle,task) = parse_extended toeholds specificities text in (bundle |> convert_expand, task)


(*************************)
let generic_to_jit 
  (calculus:Calculus<'a>) 
  speciesEquals
  speciesMatches
  (initials:Initial<'a, Expression.t<string>> list) 
  (reactions:Reaction<'a, Expression.t<string>, Expression.t<Key<'a>>> list)
  (settings:Crn_settings<Expression.t<Key<'a>>>) 
  (makeJitPlottable : 'a -> Jit.newplottable) = 
    // calculate dsd reactions allowed by the initial species
    //let initialSpecies = bundle.initials 
    //                      |> List.map (fun i -> i.species)
  
  
    let rec loop toExpand expanded newReactions newSpecies =
      let isNew s =
        match List.tryFind (speciesEquals s) expanded with
        | None -> true
        | Some _ -> false
      match toExpand with
      | []    -> newReactions, newSpecies |> Seq.distinct |> Seq.toList
      | i::is ->
        if not (isNew i)
          then loop is (i::expanded) newReactions newSpecies
          else 
        //    let new_reactions = calculus.react species_acc i in
        //    let new_species = 
        //      new_reactions
        //      |> List.collect (fun r -> r.products |> Mset.elements)
        //      |> List.distinct
        //      |> List.filter is_new
        //    loop (new_reactions@reactions_acc) (i::species_acc) (is @ new_species)
               let eReactions = calculus.react expanded i
               let eSpecies   = eReactions
                                 |> List.collect (fun r -> r.products |> Mset.elements)
                                 |> List.filter isNew
               loop is (i::expanded) (eReactions @ newReactions) (eSpecies @ newSpecies)

    let reactionSpecies = reactions 
                           |> List.collect (fun r -> r.products |> Mset.elements)
    let bundleSpecies   = initials |> List.map (fun i -> i.species)
    let initialSpecies  = bundleSpecies @ reactionSpecies
                           |> List.rev
                           |> List.fold (fun acc sp -> if List.exists (speciesEquals sp) acc
                                                            then acc
                                                            else acc @ [sp]) [] 
  
    let newReactions, newSpecies  = loop initialSpecies [] [] []
    // add user-defined reactions only if they are not already generated by the calculus
    // CS: this seems to be the behaviour in Visual DSD
    let allReactions = reactions 
                        @ (newReactions 
                           |> List.filter (fun x -> not (List.exists ((=) x) reactions)) ) 
  
    // setup species
    let zero : Expression.t<string> = Expression.Float 0.0
    let newInitials = newSpecies 
                        |> List.distinct
                        // filter out species already present in some initial (this can happen if the calculus finds a new species in loop which is also in the is list)
                        |> List.filter (fun sp -> not (initials |> List.exists (fun i-> i.species = sp)))
                        |> List.map (fun sp -> Initial.create(false, zero, sp, None, None))
    let allInitials = initials @ newInitials

    // setup constant inlining
    let env  : Environment.t = Parameters.to_env settings.parameters
    let eval = Expression.eval (Environment.find env)
    let rates : Map<string,Expression.t<Inlined<'a>>> = Key.inline_rates_env env settings.rates
    let inliner x = Key.inline_keys env rates x
  
    // inline constants
    let iPlots      = Expression.expand inliner
    let iSettings : Crn_settings<Expression.t<Inlined<'a>>> = settings.map_plots iPlots iPlots //AP NB TODO: define separate functions for mapping rates and plots 
    let iReactions  = allReactions
                      |> List.map (Reaction.map id eval (Expression.expand inliner))
  
    // return jit
    Jit.create iSettings.simulation iSettings.stochastic iSettings.rates iSettings.parameters initialSpecies allInitials iReactions makeJitPlottable speciesEquals speciesMatches

(*************************)

let to_jit (b:bundle) : Choice<Jit.t<Species.t>, Jit.t<RulesDSD.Syntax.Process<SiteT>>> =
  match b with 
  | ClassicDSD bundle ->
    let equalsSpecies = Species.equal bundle.dsdOptions
    let calculus = makeDsdCalculus b
  
    // calculate dsd reactions allowed by the initial species
    //let initialSpecies = bundle.initials 
    //                      |> List.map (fun i -> i.species)
  
  
    let rec loop toExpand expanded newReactions newSpecies =
      let isNew s =
        match List.tryFind (equalsSpecies s) expanded with
        | None -> true
        | Some _ -> false
      match toExpand with
      | []    -> newReactions, newSpecies |> Seq.distinct |> Seq.toList
      | i::is ->
        if not (isNew i)
          then loop is (i::expanded) newReactions newSpecies
          else 
        //    let new_reactions = calculus.react species_acc i in
        //    let new_species = 
        //      new_reactions
        //      |> List.collect (fun r -> r.products |> Mset.elements)
        //      |> List.distinct
        //      |> List.filter is_new
        //    loop (new_reactions@reactions_acc) (i::species_acc) (is @ new_species)
               let eReactions = calculus.react expanded i
               let eSpecies   = eReactions
                                 |> List.collect (fun r -> r.products |> Mset.elements)
                                 |> List.filter isNew
               loop is (i::expanded) (eReactions @ newReactions) (eSpecies @ newSpecies)

    let reactionSpecies = bundle.sreactions 
                           |> List.collect (fun r -> r.products |> Mset.elements)
    let bundleSpecies   = bundle.initials |> List.map (fun i -> i.species)
    let initialSpecies  = bundleSpecies @ reactionSpecies
                           |> List.rev
                           |> List.fold (fun acc sp -> if List.exists (equalsSpecies sp) acc
                                                            then acc
                                                            else acc @ [sp]) [] 
  
    let newReactions, newSpecies  = loop initialSpecies [] [] []
    // add user-defined reactions only if they are not already generated by the calculus
    // CS: this seems to be the behaviour in Visual DSD
    let allReactions = bundle.sreactions 
                        @ (newReactions 
                           |> List.filter (fun x -> not (List.exists ((=) x) bundle.sreactions)) ) 
  
    // setup species
    let zero : Expression.t<string> = Expression.Float 0.0
    let newInitials = newSpecies 
                        |> List.distinct
                        // filter out species already present in some initial (this can happen if the calculus finds a new species in loop which is also in the is list)
                        |> List.filter (fun sp -> not (bundle.initials |> List.exists (fun i-> i.species = sp)))
                        |> List.map (fun sp -> Initial.create(false, zero, sp, None, None))
    let allInitials = bundle.initials @ newInitials

    // setup constant inlining
    let env  : Environment.t = Parameters.to_env bundle.settings.parameters
    let eval = Expression.eval (Environment.find env)
    let rates = Key.inline_rates_env env bundle.settings.rates
    let inliner (x:Key<Species.t>) = Key.inline_keys env rates x
  
    // inline constants
    let iPlots      = Expression.expand inliner
    let iSettings:Crn_settings<Expression.t<Inlined<Species.t>>> = bundle.settings.map_plots iPlots iPlots //AP NB TODO: define separate functions for mapping rates and plots 
    let iReactions  = allReactions
                      |> List.map (Reaction.map id eval (Expression.expand inliner))
  
    // prepare the generator for new plottables
    let count = ref 0 
    let namer = default_namer bundle.dsdOptions bundle.meta.info
    let makeJitPlottable sp =
      let attributes = get_species_attributes namer bundle sp
      //let attributes = get_species_attributes (fun sp -> (count := !count + 1; sprintf "jit_%i" !count)) bundle sp
      { Jit.name = attributes.name; Jit.structural = attributes.structure |> Some; Jit.svg = attributes.svg |> Some }
  
    // return jit
    Choice1Of2 <| Jit.create iSettings.simulation iSettings.stochastic iSettings.rates iSettings.parameters initialSpecies allInitials iReactions makeJitPlottable (Species.equal bundle.dsdOptions) (Species.matches bundle.dsdOptions)
  | Rules bundle -> 
    let calculus          = to_rules_calculus b 
    let processEquals p q = (p = q)
    let processMatch  p q = 
      let eq = [RulesDSD.Unification.TEq ( RulesDSD.Syntax.Term.Proc p, RulesDSD.Syntax.Term.Proc q)]
      RulesDSD.Unification.unify cle eq |> List.isEmpty |> not
    
    // make jit plotter
    let namer = getRulesDSDNamer (bundle.plotsCache, ref bundle.plotsCache.Count)// bundle.spCache
    let colors = new System.Collections.Generic.Dictionary<string,string>()
    let colors = BranchesRenderer.dom_map colors (List.map (fun (i:Initial<Process<SiteT>,Expression.t<string>>) -> Species.LogicDsdProcess i.species) bundle.initials)
    let makeJitPlottable p =
      let rendered = (rulesRenderer bundle.dsdOptions namer colors)
      let attributes = rendered p
      { Jit.name = attributes.name; Jit.structural = attributes.structure |> Some; Jit.svg = attributes.svg |> Some }
    
    let rulesJit = 
          generic_to_jit 
            calculus 
            processEquals
            processMatch
            bundle.initials
            []
            bundle.settings
            makeJitPlottable
    Choice2Of2 rulesJit


let is_jit (b:bundle) = 
  match b with 
  | ClassicDSD bundle -> Options.get_is_jit bundle.dsdOptions
  | Rules bundle -> Options.get_is_jit bundle.dsdOptions

let update_bundle_after_jit (b:bundle) (jit:Jit.t<Species.t>) : bundle =
    // Create new initials list
  match b with
  | ClassicDSD bundle ->
    // FP: I'm removing the mapping step, because it's producing wrong species counts. I'm not sure why it was done. The initials from JIT should be correct already.
    (*let all_species = jit |> Jit.get_species
    let map_initial (sp:Initial<Species.t,Expression.t<string>>) = 
      match List.tryFind (fun (i:Initial<Species.t,Expression.t<string>>) -> i.species = sp.species) bundle.initials with
          | Some i -> i
          | None -> sp
    let initials = all_species |> List.map map_initial*)
    let initials = jit |> Jit.get_species
    // Create new reactions list
    let all_reactions = jit |> Jit.get_reactions
    let map_reaction (r:Reaction<'s, float, Expression.t<Inlined<'s>>>) =
      Reaction.map id Expression.Float (fun f -> Expression.map (fun k -> match k with
                                                                          | Inlined.Species s -> Key.Species s
                                                                          | Inlined.Time -> Key.Time) f) r
    let reactions = all_reactions |> List.map map_reaction
    ClassicDSD { bundle with initials = initials; sreactions = reactions }
  | Rules bundle -> 
      let initials =jit |> Jit.get_species
      let all_reactions = jit |> Jit.get_reactions
      let map_reaction (r:Reaction<'s, float, Expression.t<Inlined<'s>>>) =
        Reaction.map id Expression.Float (fun f -> Expression.map (fun k -> match k with
                                                                            | Inlined.Species s -> Key.Species s
                                                                            | Inlined.Time -> Key.Time) f) r
      let reactions = all_reactions |> List.map map_reaction
      // Rules { bundle with initials = initials; reactions = reactions }
      failwith "JIT simulations not implemented for RulesDSD yet"

let to_logic_calculus b =
    let b = match b with Rules b -> b | _ -> failwith "Rules bundle expected"
    // prepare the set of "reaction" predicates to run in the calculus
    let reactionKinds = 
      let key1 = "reaction", 3    // reactions of the form reaction(P, Rate, Q)
      let key2 = "reactions", 2   // reactions of the form reactions(P, Crn)
      if not <| b.rules.ContainsKey key1
         && not <| b.rules.ContainsKey key2
        then []
        else
      let reactions1 = 
        b.rules.Item key1 
        |> Set.filter (fun s -> s.head.Name = "reaction" 
                                && match s.head.Args.Head with 
                                   | Term.TList _ -> true 
                                   | Term.Var _  -> true
                                   | _ -> failwith <| sprintf "The first argument of %s must be a list or a variable." s.head.String)
        |> Set.map (fun s -> match s.head.Args.Head with 
                             | Term.TList ts -> ts.Length, s.head
                             | Term.Var _    -> -1, s.head
                             | _  -> failwith "")
      let reactions2 = 
        b.rules.Item key2
        |> Set.filter (fun s -> s.head.Name = "reactions")
        |> Set.map (fun s -> -1, s.head)
      
      Set.union reactions1 reactions2
        |> Seq.fold (fun (acc:Map<int, Predicate<SiteT> list>) (k:int,v) -> if acc.ContainsKey k
                                                                              then Map.add k (v::acc.[k]) acc
                                                                              else Map.add k [v] acc) Map.empty
        |> Map.toList
    {
      react = fun (oldTerms:Term<SiteT> list) (newTerm:Term<SiteT>) -> 
        // combine each complex Q in oldSystems with complex P in newSystem, to form (P | Q)
        let rec combo n : Term<SiteT> list list =
          if n <= 0 
            then []
            else let ccombo = combo (n-1)
                 if ccombo.IsEmpty
                  then oldTerms |> List.map (fun p -> [p])
                  else
                 oldTerms
                 |> List.collect (fun t -> 
                    ccombo 
                    |> List.map (fun ts -> t :: ts))
        // build the available "reaction"s given the new and old processes
        let queries = 
          reactionKinds
          |> List.collect 
              (fun (reactantsStoichiometry, preds) ->
                match reactantsStoichiometry with
                | -1 -> // predicates of the form reaction(Var, Rate, Qs)
                        preds |> List.map (fun p -> Pred (p.Name, newTerm :: (p.Args.Tail)))
                | 0  -> preds
                | 1  -> preds |> List.map (fun p -> Pred (p.Name, TList [newTerm] :: (p.Args.Tail)))
                | _  -> 
                        let oldReactants = combo (reactantsStoichiometry - 1)
                        preds 
                        |> List.collect (fun p -> 
                          oldReactants 
                          |> List.map (fun rs -> 
                            let ps, terms    = newTerm :: rs
                                               |> List.partition (fun x -> match x with 
                                                                           | Term.Proc _ -> true
                                                                           | _ -> false) 
                            let procs        = match ProcessEquality.Flatten cle' <| Func ("|", ps) with
                                               | Some p -> p 
                                                           |> Process<SiteT>.Canonical cle
                                                           |> Microsoft.Research.DNA.LogicDSD.toDsdComplexes
                                                           |> List.map Term.Proc
                                               | None -> failwith""
                            let newReactants = Term.TList (procs @ terms)
                            let newArgs      = newReactants :: p.Args.Tail
                            Pred (p.Name, newArgs)
                            ))
            )
          |> List.map Literal.Pos

        let errorMsg t = 
          failwith <| sprintf "Unexpected term %s found in reaction enumeration; expecting a process or a list of processes." (Term.ToStringWith Microsoft.Research.DNA.LogicDSD.engine t)
        
        // query the Prolog engine and create reactions if any reaction predicate matched
        queries
        |> List.choose (fun query -> 
          match RulesDSD.Resolution.resolveAll query b.rules cle (*debugMap*) with 
          | None -> None
          | Some sols ->
            let procToMSet (t : Term<SiteT>) (toCanonize : bool ) : Mset.t<Term<SiteT>> =
              let toMSet procs : Mset.t<Term<SiteT>> =
                procs 
                |> List.sortWith (Term<SiteT>.Compare cle)
                |> List.countBy id
                |> List.map (fun (sp, count) -> { element = sp; multiplicity = count})
              let removeAnyTags p = 
                let removeAnyTag (d:DomainT) =
                  match d with 
                  | DomainT.Dom dom -> if dom.tag = AnyTag 
                                        then DomainT.Dom { dom with tag = NoTag }
                                        else d  
                  | DomainT.Var (v, AnyTag) -> DomainT.Var (v, NoTag) 
                  | _ -> d
                p |> Process.Map (fun x -> match x with 
                                                    | SiteT.Site s -> match s with 
                                                                      | Site.Bound (d,b) -> (removeAnyTag d, b) |> Site.Bound |> SiteT.Site
                                                                      | Site.Unbound d -> d |> removeAnyTag |> Site.Unbound |> SiteT.Site
                                                    | SiteT.Var v -> x)
              match t with 
              | Proc p' -> 
                  let p = removeAnyTags p'
                  if toCanonize
                    then p |> Process.Canonical cle    // sorts all complexes in canonical form
                           |> Microsoft.Research.DNA.LogicDSD.toDsdComplexes
                           |> List.map (Process.Canonical cle)
                    else [p]
                  |> List.map Term.Proc
                  |> toMSet 
              | TList ts -> if toCanonize
                              then 
                                  // TODO: split Qs into processes (potentially canonical) and terms
                                  let qs = ts 
                                           |> List.map (ProcessEquality.Flatten cle)
                                           
                                  if qs |> List.exists Option.isNone 
                                    then failwith""
                                    else 
                                      let q = qs 
                                              |> List.map (Option.get >> Process.ToList)
                                              |> List.concat
                                              |> Process.OfList
                                              |> removeAnyTags
                                              |> Term.Proc
                                      match ProcessEquality.Flatten cle q with 
                                       | Some p -> p 
                                                   |> Process.Canonical cle // sorts complexes in canonical form
                                                   |> Microsoft.Research.DNA.LogicDSD.toDsdComplexes 
                                                   |> List.map (Process.Canonical cle)
                                                   |> List.map Term.Proc
                                                   |> toMSet 
                                       | _ -> errorMsg t
                              else ts 
                                    |> List.map (fun x -> match x with 
                                                          | Term.Proc p -> removeAnyTags p
                                                          | _ -> failwith"")
                                    |> List.map Term.Proc
                                    |> toMSet
              | _ -> errorMsg t
          
            let rec termToCrnExpr t : Value = 
              match t with 
              | Term.Float i   -> Expression.Float (float i)
              | Term.Const n -> Expression.Key n
              | Term.Func("*", ts) -> ts 
                                      |> List.map termToCrnExpr
                                      |> Expression.Times
              | Term.Func("+", ts) -> ts 
                                      |> List.map termToCrnExpr
                                      |> Expression.Plus
              | Term.Func("-", [t1; t2]) ->  Expression.Minus {sub1 = termToCrnExpr t1; sub2 = termToCrnExpr t2}
              | Term.Func("**", [t1; t2]) -> Expression.Power {base_ = termToCrnExpr t1; exponent = termToCrnExpr t2}
              | Term.Func("/", [t1; t2]) ->  Expression.Divide {div1 = termToCrnExpr t1; div2 = termToCrnExpr t2}
              | Term.Func(op, _)  -> failwith <| sprintf "Conversion to rate not support for operation \"%s\"" op
              | Term.Proc _     -> failwith <| sprintf "Conversion to functional rates not support yet"
              | _ -> failwith <| sprintf "Unexpected term \"%s\"in rate conversion." (Term.ToStringWith Microsoft.Research.DNA.LogicDSD.engine t)
            
            let makeCrnReaction P Rt Q = 
              let reactants = procToMSet P true
              let rate : Rate<Value, Expression.t<Key<Term<SiteT>>>> = 
                Rt 
                |> termToCrnExpr  
                |> Rate.MassAction
              let products  = procToMSet Q true
                                // |> List.sort
              Reaction.create(reactants, None, rate, products)
            
            sols 
            |> List.map (fun theta -> 
              match theta.Apply(query, cle) with
              | Pos (Pred ("reaction", [P; Rt;  Q])) -> 
                // reactants are already in canonical form by construction
                [makeCrnReaction P Rt Q]
              | Pos (Pred ("reactions", [_; crn])) -> 
                // TODO: transform Crn into a list of reactions
                match crn with
                | Term.TList(ts) -> 
                    ts 
                    |> List.map (fun t -> 
                        match t with 
                        | Term.Func ("_reaction", [P; Rt; Q]) ->       
                              makeCrnReaction (Term.Canonical cle P) Rt Q // canonize P first
                        | _ -> failwith "")
                | _ -> []
              | _ -> failwith ""  )
            |> Some
          )
        |> List.concat
        |> List.concat
        |> List.distinct
        |> List.filter (fun r -> r.reactants <> r.products)
      }