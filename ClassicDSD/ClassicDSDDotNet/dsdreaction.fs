// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Dsdreaction
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type value = Expression.t<string> 
type species = Microsoft.Research.DNA.Species.t
type domain = Domain.t
type strand = Strand.t
type segment = Segment.t
type gate = Gate.t
type origami = Origami.t
type semantics = Options.semantics
type options = Options.t
type species_env = Species.t_env
type sreaction = Reaction<species, value, Expression.t<Key<species>>>
type settings = Crn_settings<Expression.t<Key<species>>>
type enzyme =
  | Nicking of domain * domain (* creates a nick between the two domains (when found in sequence) *)
  | Polymerase of bool      (* true = displacing, false = not displacing *)

type 'species crn_rate when 'species:equality =
  | Crn_ma of value
  | Crn_func of Expression.t<Key<'species>>

let map_crn_rate f = function
  | Crn_ma v -> Crn_ma v
  | Crn_func e -> Expression.map (Key.map f) e |> Crn_func

type 'species mass_action_reaction when 'species:equality =
  { ma_reactants:   'species Mset.t
  ; ma_products:    'species Mset.t
  ; ma_catalysts:   'species Mset.t
  ; ma_rate:        'species crn_rate
  ; ma_reverseRate: 'species crn_rate option }

let map_mass_action_reaction f r =
  { ma_reactants = Mset.map f r.ma_reactants
  ; ma_products = Mset.map f r.ma_products
  ; ma_catalysts = Mset.map f r.ma_catalysts
  ; ma_rate = map_crn_rate f r.ma_rate
  ; ma_reverseRate = Lib.option_map2 (map_crn_rate f) r.ma_reverseRate }


(* Type for the different kinds of reaction. *)
type t =
  | Unbind of gate * domain * species list            (* Unbind reaction on the given domain. *)
  | Bind of species * species * domain * species list (* Bind reaction on the given domain. *)
  | Leak of species * species * bool * species list   (* Leak reaction (maybe fast). *)
  | Tau of gate * species list                        (* Tau reaction. *)
  | Migrate of gate * domain list * gate              (* Branch migration reaction over list of domains. *)
  | Displace of gate * domain list * species list     (* Displacement reaction over list of domains. *)
  | Cover of gate * domain * gate                     (* Cover reaction on the given domain. *)
  | MigrateOpen of gate * domain list * gate          (* Branch migration reaction leading to opening a hairpin *)
  | UnbindOpen of gate * domain * species list        (* Unbind reaction on the given domain leading to opening a hairpin  *) 
  | Pin of species * domain * species list            (* Hairpin creation reaction on the given domain *)
  | OrigamiReact of origami * t * origami             (* Internal reactions on an origami, between anchored strands and gates *)
  | OrigamiEntry of origami * species * t * origami   (* Reactions between an origami and an external species *)
  | OrigamiExpel of origami * t * species list        (* Reactions within an origami that expel species *)
  | OrigamiExchange of origami * species * t * species list (* Reactions between an origami and an external species that both enter and expel species *)
  | Remote of domain list * t                         (* Displacement or migration happening at a distance *)
  | Nick of species * enzyme * species                (* Nicking enzyme *)
  | Replication of species * enzyme * int * int * species list (* Replication of a gate: n1 = nr copied but not displaced base pairs, n2 = nr copied and displaced base pairs *)
  | Chemical of species mass_action_reaction          (* A general chemical reaction in low-level format *)

(* Get the reactants / products of a reaction, as a list of species. *)
let rec reactants (r:t) =
  match r with
  | Unbind(g,_,_) | Tau(g,_) | Migrate(g,_,_) | Displace(g,_,_) | Cover(g,_,_) | MigrateOpen(g,_,_) | UnbindOpen(g,_,_) -> [Species.GATE g]
  | Bind(i1,i2,_,_) | Leak(i1,i2,_,_) -> [i1;i2]
  | Pin(i,_,_) -> [i]
  | OrigamiReact(o,_,_) | OrigamiExpel(o,_,_) -> [Species.ORIGAMI o]
  | OrigamiEntry(o,i,_,_) | OrigamiExchange(o,i,_,_) -> [Species.ORIGAMI o; i]
  | Remote (_,r) -> reactants r
  | Nick (i,_,_) | Replication (i,_,_,_,_) -> [i]
  | Chemical r -> Mset.to_list r.ma_reactants
let rec products (r:t) =
  match r with
  | Unbind(_,_,is) | Bind(_,_,_,is) | Leak(_,_,_,is) | Tau(_,is) | Displace(_,_,is) | UnbindOpen(_,_,is) | Pin(_,_,is) -> is
  | Migrate(_,_,g) | Cover(_,_,g) | MigrateOpen(_,_,g) -> [Species.GATE g]
  | OrigamiReact(_,_,o) | OrigamiEntry(_,_,_,o) -> [Species.ORIGAMI o]
  | OrigamiExpel(_,_,is) | OrigamiExchange(_,_,_,is)-> is
  | Remote (_,r) -> products r
  | Nick (_,_,i) -> [i] | Replication (_,_,_,_,is) -> is
  | Chemical r -> Mset.to_list r.ma_products

(* Get the list of all species in a reaction. *)
let species (r:t) = (reactants r)@(products r)


(* helper functions for migration/displacement *)
let domainLength (opts:options) (ns: domain list) = 
    let thlen = Options.get_toehold_length opts in
    let splen = Options.get_specificity_length opts in
    (Lib.fold_left (fun total n -> total + (Domain.length thlen splen n)) 0 ns )
   
let migrationRate (opts:options) (ns: domain list) =
    let len = domainLength opts ns in
    Expression.div (Options.get_elementary_migration_rate opts) (Expression.Float (float (len * len)))

(*  helper functions for migration/displacement in SequenceRate *)
(*  If sequence rates is used, a seperate default migration / displacement 
    Also a different formula to relate length and displacement is used. *)

(*  IMPORTANT: In regular mode, directive migrate <float> specifies an elementary migration step. 
    The default rate is 8000, and this gives 20/s for a domain of length 20. This length^2 model is due to Zhang and Winfree.

    In sequenceRate, we take the value to be equal to the migration rate for a 20-nt domain and scale it according to our model 
    Also see draft paper dannenberg et al  *)

let scaleFunction (length:int) = 
    float (30*length + (length-1)*length) // 30 is the assumed delay in initialiation of migration

let baseMigrationRateSequence (opts:options)(ns: domain list) =
    let len = domainLength opts ns in
    let regularLen = 20 in
    let scalar = (scaleFunction regularLen) / (scaleFunction len) in
    Expression.mul (Options.get_elementary_migration_rate opts) (Expression.Float scalar)

let migrationSequenceRate opts (before_energy : SequenceCalc.energyExpression) (ns: domain list) (after_energy : SequenceCalc.energyExpression) = 
    let energy_diff = 0.5 |> SequenceCalc.energyExpression_lmul <| (before_energy |> SequenceCalc.energyExpression_minus <| after_energy) in
    let bindRate = baseMigrationRateSequence opts ns in   // Options.get_elementary_migration_rate opts in 
    (SequenceCalc.unbindRateExpression (SequenceCalc.tempCintoK (Options.getTemperature opts)) bindRate energy_diff) 

let displacementRateSequence (opts:options) (mapping:Sequence.mapping) (before: gate) (ns: domain list) (after: species list) =
     let before_energy = SequenceCalc.getGateEnergy opts mapping before in
     let after_energy = after |> List.map (SequenceCalc.getSpeciesEnergy opts mapping) |> List.reduce SequenceCalc.energyExpression_plus in 
     (migrationSequenceRate opts before_energy ns after_energy )


let migrationRateSequence (opts:options) (mapping:Sequence.mapping) (before: gate) (ns: domain list) (after: gate) =
     let before_energy = SequenceCalc.getGateEnergy opts mapping before in
     let after_energy = SequenceCalc.getGateEnergy opts mapping after in
     (migrationSequenceRate opts before_energy ns after_energy )
     

(* Get the rate of a reaction. *)
let rec rate (z:settings) (opts:options) (mapping:Sequence.mapping) (r:t) =
  match r with
  | Unbind(r,n,ps) | UnbindOpen(r,n,ps) ->
    if Options.getSequenceRates opts then
      let r_energy = SequenceCalc.getGateEnergy opts mapping r in
      let ps_energy = ps |> List.map (SequenceCalc.getSpeciesEnergy opts mapping) |> List.reduce SequenceCalc.energyExpression_plus in
      let energy_diff = r_energy |> SequenceCalc.energyExpression_minus <| ps_energy  in
      let bindRate = 
        let kt = (Domain.bind_rate n) in
        let conc_scale = 10.0 ** (float (match z.units.concentration with Concentration.Molar n -> n)) in         // FD : unbind rates are given in  per (nano) molar / s units
        Expression.div kt (Expression.Float conc_scale)
      in
      SequenceCalc.unbindRateExpression (SequenceCalc.tempCintoK (Options.getTemperature opts)) bindRate energy_diff |> Crn_ma
    else Domain.unbind_rate n |> Crn_ma
  | Bind(r1,r2,n,ps) ->
    if Options.getSequenceRates opts && (Options.getRules opts).Equals(Options.Infinite)  then
      let r1_energy = SequenceCalc.getSpeciesEnergy opts mapping r1 in
      let r2_energy = SequenceCalc.getSpeciesEnergy opts mapping r2 in
      let ps_energy = ps |> List.map (SequenceCalc.getSpeciesEnergy opts mapping) |> List.reduce SequenceCalc.energyExpression_plus in
      //let energy_diff = SequenceCalc.slowDown  ( (r1_energy + r2_energy) - ps_energy)  in
      //let diff = (r1_energy + r2_energy) - ps_energy in
      //let energy_diff = Prim.min diff  {dH = 0.0, dS = 0.0} in
      //let energy_diff = 0.5 * (r1_energy + r2_energy - ps_energy)   in
      let energy_diff = 0.0 |> SequenceCalc.energyExpression_lmul <| (r1_energy |> SequenceCalc.energyExpression_plus <| r2_energy |> SequenceCalc.energyExpression_minus <| ps_energy)   in
      let bindRate = 
         let kt = (Domain.bind_rate n)
          (* FD: no need to scale when the reaction is bimolecular. *)
         // let conc_scale = 10.0 ** (float (Settings.get_concentration_exp z)) in         // FD : unbind rates are given in  per (nano) molar / s units
         //  Expression.div kt (Expression.Float conc_scale)
         kt
      in
      SequenceCalc.unbindRateExpression (SequenceCalc.tempCintoK (Options.getTemperature opts)) bindRate energy_diff |> Crn_ma
    else Domain.bind_rate n |> Crn_ma         (* Assuming that the two reactants are different species for bind? *)
  | Pin(_,n,_) ->  Domain.bind_rate n |> Crn_ma 
  | Leak(_,_,fast,_) ->                                                (* Assuming that the two reactants are different species? *)
      (if fast then Options.get_leak_rate_l opts
       else Options.get_leak_rate_w opts) |> Crn_ma
  | Tau _ -> Options.get_tau_rate opts |> Crn_ma
  | Displace(before,ns,after)  ->  
      if Options.getSequenceRates opts then   
          (displacementRateSequence opts mapping before ns after) |> Crn_ma
      else
          (migrationRate opts ns) |> Crn_ma
  | Migrate(before,ns,after) | MigrateOpen(before,ns,after) ->  
      if Options.getSequenceRates opts then
          (migrationRateSequence opts mapping before ns after) |> Crn_ma
      else
          (migrationRate opts ns) |> Crn_ma
  | Cover(_,n,_) ->
      let thlen = Options.get_toehold_length opts in
      let splen = Options.get_specificity_length opts in
      let len = Domain.length thlen splen n in 
       Expression.div (Options.get_elementary_migration_rate opts) (Expression.Float (float (len * len))) |> Crn_ma
  | OrigamiReact(_,r,_) | OrigamiEntry(_,_,r,_) | OrigamiExpel(_,r,_) | OrigamiExchange(_,_,r,_) -> rate z opts Sequence.empty r
  | Remote (ds,r) ->
      (match Options.get_pinleak_rate opts with
       | Some e -> Crn_ma e
       | None ->
           let thlen = Options.get_toehold_length opts in
           let splen = Options.get_specificity_length opts in
           let len = ds |> List.sumBy (Domain.length thlen splen) |> float in
           let local_conc = 10000.0 / len in
           (match rate z opts mapping r with
            | Crn_ma v -> v |> Expression.mul (Expression.Float local_conc) |> Crn_ma
            | Crn_func f -> Expression.Times [Expression.Float local_conc; f] |> Crn_func)
      )
  | Nick _ -> Expression.Float 1.0 |> Crn_ma (* TODO: put in Options? *)
  | Replication (_,_,n1,n2,_) ->
      let copy_rate = 1.0 in (* TODO: put in Options? *)
      let copy_displace_rate = 1.0 in (* TODO: put in Options? *)
      let rate = copy_rate / (float n1) + copy_displace_rate / (float n2) in
       Expression.Float rate |> Crn_ma
  | Chemical r -> r.ma_rate

(* Do two reactions have matching species? *)
let matching_species (opts:options) (r1:t) (r2:t) =
  (Lib.is_permutation (Species.equal opts) (reactants r1) (reactants r2)) &&
  (Lib.is_permutation (Species.equal opts) (products r1) (products r2))

(* Does a reaction have matching species with an sreaction? *)
let matching_species_rsr (opts:options) (r1:t) (r2:sreaction) =
  (Mset.is_perm (Species.equal opts) (Mset.from_list (reactants r1)) r2.reactants) &&
  (Mset.is_perm (Species.equal opts) (Mset.from_list (products r1)) r2.products)


(* Do two reactions have matching kinds and matching species? *)
let rec matching_reactions (opts:options) (r1:t) (r2:t) =
  match r1,r2 with
  | Unbind _, Unbind _ | Tau _ , Tau _ | Migrate _ , Migrate _  
  | Displace _ , Displace _ | Cover _ , Cover _ | MigrateOpen _ , MigrateOpen _ | UnbindOpen _ , UnbindOpen _ 
  | Bind _ , Bind _ | Leak _ , Leak _ | Pin _ , Pin _ | Chemical _, Chemical _-> matching_species opts r1 r2
  | OrigamiReact(_,r1t,_),OrigamiReact(_,r2t,_) | OrigamiExpel(_,r1t,_),OrigamiExpel(_,r2t,_)
  | OrigamiEntry(_,_,r1t,_),OrigamiEntry(_,_,r2t,_) | OrigamiExchange(_,_,r1t,_),OrigamiExchange(_,_,r2t,_) -> matching_reactions opts r1t r2t
  | Nick (_, e1, _), Nick (_, e2, _) | Replication (_, e1, _, _, _), Replication (_, e2, _, _, _) -> e1 = e2 && matching_species opts r1 r2
  | Remote (ds1, r1), Remote (ds2, r2) -> ds1 = ds2 && matching_species opts r1 r2
  | _,_ -> false

(* Is a particular reaction a leak? fast leak? slow leak? tau reaction? *)
let is_leak      = function Leak _ -> true | _ -> false
let is_fast_leak = function Leak(_,_,true,_) -> true | _ -> false
let is_slow_leak = function Leak(_,_,false,_) -> true | _ -> false
let is_tau = function Tau _ -> true | _ -> false

let looks_like_leak o (r:sreaction) = 
  match r.rate with
   | Rate.MassAction f -> f = Options.get_leak_rate_l o || f = Options.get_leak_rate_w o
   | _ -> false

(* Decide whether a reaction is circular (i.e. products = reactants). *)
let is_circular (opts:options) (r:t) = Lib.is_permutation (Species.equal opts) (reactants r) (products r)

(* Produce labels for reactions. *)
let bme_label (r:sreaction) = r.rate.to_string (Expression.to_string id) (Expression.to_string (Key.to_string Species.display))
  (*
  match rate z opts r with
  | Expression.PrimFloat r -> Lib.display_float r
  | Expression.PrimVar s -> s
  *)

(*
let leak_label (fast:bool) = if fast then "leak_l" else "leak_w"
let label (r:t) = match r with
 | Unbind(_,n,_) | Bind(_,_,n,_) -> Domain.display n
 | Leak(_,_,fast,_) -> leak_label fast
 | Tau _ -> "tau"
 | Displace(_,ns,_) -> "displace"
 | Migrate(_,ns,_) -> "migrate"
 | Cover(_,n,_) -> "cover"
 | Chemical cr -> "chemical" + Expression.to_string cr.ma_rate

let sbml_label (r:t) = match r with
 | Unbind _ -> "unbind" | Bind _ -> "bind" | Tau _ -> "tau" | Leak(_,_,fast,_) -> leak_label fast
 | Displace _ -> "displace" | Migrate _ -> "migrate" | Cover _ -> "cover"

(* Get toehold specificity for a binding / unbinding reaction. *)
let toehold_specificity (r:t) = match r with
  | Unbind(_,n,_) | Bind(_,_,n,_) -> 
      (match n with
        | Domain.Toe(_,spec,_) -> Some spec
        | Domain.Normal(_,_) -> failwith ("Error - Normal domain " + (Domain.display n) + " in Reaction.t"))
  | _ -> None
*)

(* Get string representations of the products, reactants and domain involved in a reaction. *)
let get_product_strings (r:t) : string list = List.map Species.display (products r)
let get_reactant_strings (r:t) : string list = List.map Species.display (reactants r)
(*
let get_product_strings_sorted (r:t) : string list = Lib.sort String.compare (get_product_strings r)
let get_reactant_strings_sorted (r:t) : string list = Lib.sort String.compare (get_reactant_strings r)
*)

(* String representations of (reversible) reactions. *)
let display_reversible _ (opts:options) ((r,rinv):sreaction*sreaction option) =
  let reactants = List.map Species.display (Mset.to_list r.reactants) in
  let products = List.map Species.display (Mset.to_list r.products) in
  let arrow = (match rinv with Some rinv -> "{" + (bme_label rinv) + "}<" | None -> "") +
              (if looks_like_leak opts r then "~" else "-") + ">{" + (bme_label r) + "}"
  in
  (Lib.string_of_list Lib.id " + " reactants) + "  " + arrow + "  " + (Lib.string_of_list Lib.id " + " products)

(* String representation of a list of (reversible) reactions. *)
let display_reversibles (z:settings) (opts:options) (rs:(sreaction*sreaction option) list) =
  let newline:string = "\r\n" in
  let sb = Stringbuilder.empty () in
  List.iter (fun r -> Stringbuilder.append sb newline; Stringbuilder.append sb (display_reversible z opts r)) rs;
  Stringbuilder.value sb

(* Produce a DOT representation of a (reversible) reaction. *)
let to_dot_reversible _ (opts:options) (i:int) ((r,rinv):sreaction*sreaction option) =
  let quote (s:string) = "\"" + s + "\"" in 
  let newline:string = "\r\n" in
  let ident (i:int) = quote ("i" + (string i)) in
  (*let mklabel r = match r with Unbind _ -> "rm" + (label r) | _ -> "r" + (label r) in*)
  let r_gates,r_strands,_ = Species.separate (Mset.to_list r.reactants) in
  let p_gates,p_strands,_ = Species.separate (Mset.to_list r.products) in
  let mkColour r = if looks_like_leak opts r then "grey" else "black" in (*match r with Tau _ -> "purple" | Leak _ -> "grey" | _ -> "black" in*)
  let r_edge_dir = match rinv with Some _ -> "back" | None -> "none" in
  let r_edge_colour = match rinv with Some rinv -> mkColour rinv | None -> mkColour r in
  let p_edge_colour = mkColour r in
  (* The intermediate node for this reaction. *)
  newline + ident i + "[label = \"" + (bme_label r) +
  (match rinv with Some rinv -> "\\n" + (bme_label rinv) | None -> "") + "\", shape=box, width=0.1, height=0.1]" +
  (* Edges from the reactants to the intermediate node. *)
  newline + "{" + (Lib.string_of_list (Gate.display >> quote) " " r_gates) +
  (match r_gates,r_strands with (_::_),(_::_) -> " " | _,_ -> "") +
  (Lib.string_of_list (Strand.display >> quote) " " r_strands) + "}" +
  " -> " + ident i + "[dir=" + r_edge_dir + ", color=" + r_edge_colour + "]" +
  (* Edges from the intermediate node to the products. *)
  newline + ident i  + " -> " +
  "{" + (Lib.string_of_list (Gate.display >> quote) " " p_gates) +
  (match p_gates,p_strands with (_::_),(_::_) -> " " | _,_ -> "") +
  (Lib.string_of_list (Strand.display >> quote) " " p_strands) + "}" +
  "[color=" + p_edge_colour + "]"

(* DOT representation of a list of (reversible) reactions. *)
let to_dot_reversibles (z:settings) (opts:options) (rs:(sreaction*sreaction option) list) =
  let preamble = "digraph G {node[fontsize = 20, shape=box] edge[fontsize = 20,fontname=times] " in
  let postamble = "\n}" in
  let sb = Stringbuilder.init preamble in
  Lib.iteri (fun i r -> Stringbuilder.append sb (to_dot_reversible z opts (i+1) r)) rs;
  Stringbuilder.append sb postamble;
  Stringbuilder.value sb

(* Does r2 match as an inverse for r1? *)
let inverse (opts:options) (r1:t) (r2:t) =
  (is_leak r1) = (is_leak r2) &&
  (Lib.is_permutation (Species.equal opts) (reactants r1) (products r2)) &&
  (Lib.is_permutation (Species.equal opts) (reactants r2) (products r1))

(* Turn a list of normal reactions into a list of (potentially) reversible ones. *)
let rec reversibles (opts:options) (rs:t list) =
  match rs with
  | [] -> []
  | r::rs ->
    let invs = List.filter (inverse opts r) rs
    (match invs with (* %%%% Currently keeps the first inverse and throws the others away. *)
      | rinv::_ -> (r,Some rinv)::(reversibles opts (List.filter (fun r' -> not(List.contains r' invs)) rs))
      | [] -> (r,None)::(reversibles opts rs))

(* Put all reactants and products in a reaction into standard form in one go. *)
let rec standard_form (opts:options) (r:t) =
  //let sf_c   c = Branch.standard_form opts c in
  let sf_g   g = Gate.standard_form opts g in
  let sf_o   o = Origami.standard_form opts o in
  let sf_i   i = Species.standard_form opts i in
  let sf_is is = List.map sf_i is in
  match r with
    | Unbind(g,n,is)            -> Unbind((sf_g g),n,(sf_is is))
    | Bind(i1,i2,n,is)          -> Bind((sf_i i1),(sf_i i2),n,(sf_is is))
    | Leak(i1,i2,fast,is)       -> Leak((sf_i i1),(sf_i i2),fast,(sf_is is))
    | Tau(g,is)                 -> Tau((sf_g g),(sf_is is))
    | Displace(g,ns,is)         -> Displace((sf_g g),ns,(sf_is is))
    | Migrate(g,ns,g')          -> Migrate((sf_g g),ns,(sf_g g'))
    | Cover(g,ns,g')            -> Cover((sf_g g),ns,(sf_g g'))
    | MigrateOpen(g,ns,g')      -> MigrateOpen((sf_g g), ns, (sf_g g'))
    | UnbindOpen(g,n,is)        -> UnbindOpen((sf_g g), n, (sf_is is))
    | Pin(s,n,is)               -> Pin((sf_i s),n,(sf_is is))
    | OrigamiReact(o,r,o')      -> OrigamiReact((sf_o o),standard_form opts r,(sf_o o'))
    | OrigamiEntry(o,s,r,o')    -> OrigamiEntry((sf_o o),(sf_i s),standard_form opts r,(sf_o o'))
    | OrigamiExpel(o,r,is)      -> OrigamiExpel((sf_o o),standard_form opts r,(sf_is is))
    | OrigamiExchange(o,s,r,is) -> OrigamiExchange((sf_o o),(sf_i s),standard_form opts r,(sf_is is))
    | Remote(ds,r)              -> Remote(ds, standard_form opts r)
    | Nick (i, e, i')           -> Nick (sf_i i, e, sf_i i')
    | Replication (i, e, n1, n2, is) -> Replication (sf_i i, e, n1, n2, sf_is is)
    | Chemical r                -> Chemical (map_mass_action_reaction sf_i r)

(******************************************************************************)

(** Compute all possible unbinding reactions from a gate. *)
let unbind_reactions sub_map (opts:options) (g:gate) =
  let build_single_step (g,n,gs',ss') = standard_form opts (Unbind(g,n,(Species.collate gs' ss' []))) in  (* sem = Detailed *)
  let build_merged_original (g,n,gs',ss') =  (* sem = Default *)
    let gs' = List.map (Gate.standard_form opts) gs' in
    match (Gate.fast_reactions sub_map opts gs') with
      | [] -> [standard_form opts (Unbind(g,n,(Species.collate gs' ss' [])))]
      | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Unbind(g,n,(Species.collate gs'' (ss'@ss'') [])))) rs
  in
  match (Options.getRules opts) with
    | Options.Detailed -> List.map build_single_step (Gate.unsticking_reactions sub_map opts g)
    | Options.Default -> Lib.collect build_merged_original (Gate.unsticking_reactions sub_map opts g)
    | Options.Infinite | Options.Finite -> []

(** Compute all possible unbind reactions that open a hairpin *)
let unbind_open_reactions sub_map (opts:options) (g:gate) =
  let build_single_step (g,n,gs',s') = standard_form opts (UnbindOpen(g,n,(Species.collate gs' s' []))) in (* sem = Detailed *)
  let build_merged_original (g,n,gs',s') =
    let gs' = List.map (Gate.standard_form opts) gs' in
    match (Gate.fast_reactions sub_map opts gs') with
      | [] -> [standard_form opts (UnbindOpen(g,n,(Species.collate gs' s' [])))]
      | rs -> List.map (fun (gs'', ss'') -> standard_form opts (Unbind(g,n,(Species.collate gs'' ss'' [])))) rs
  in
  match (Options.getRules opts) with
    | Options.Detailed -> List.map build_single_step (Gate.unbind_open_reactions opts g)
    | Options.Default -> Lib.collect build_merged_original (Gate.unbind_open_reactions opts g)
    | _ -> []

(** Compute all possible binding reactions between a new strand and the existing gates/strands. *)
let bind_reactions_new_strand sub_map (opts:options) (s:strand) (existing_gs:gate list) (existing_ss:strand list) =
  (* Function to stick new strand to gates. *)
  let build_sg (g,s,n,g') =
    match (Options.getRules opts) with
    | Options.Detailed | Options.Finite -> [standard_form opts (Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']))]
    | Options.Default | Options.Infinite ->
        let g' = Gate.standard_form opts g' in
        (match (Gate.fast_reactions sub_map opts [g']) with
        | [] -> [standard_form opts (Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']))]
        | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Bind(Species.GATE g, Species.STRAND s, n, (Species.collate gs'' ss'' [])))) rs)
  in
  (* Function to stick new strand to existing strands. *)
  let build_ss (s1,s2,n,g') =
    match (Options.getRules opts) with
    | Options.Detailed | Options.Finite -> [standard_form opts (Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']))]
    | Options.Default | Options.Infinite ->
        let g' = Gate.standard_form opts g' in
        (match (Gate.fast_reactions sub_map opts [g']) with
        | [] -> [standard_form opts (Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']))]
        | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Bind(Species.STRAND s1, Species.STRAND s2, n, (Species.collate gs'' ss'' [])))) rs)
  in
  (* Collect all the reactions up! *)
  let unproductive = Options.getUnproductive opts in
  (Lib.collect build_sg (Lib.collect (Gate.stick_strand_to_gate sub_map unproductive s) existing_gs)) @
  (Lib.collect build_ss (Lib.collect (Gate.stick_strand_to_strand sub_map unproductive s) existing_ss))

(** Compute all possible binding reactions between a new gate and the existing gates/strands. *)
let bind_reactions_new_gate sub_map (opts:options) (g:gate) (existing_gs:gate list) (existing_ss:strand list) =
  (* Function to stick new gate to existing strands. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UGH - CODE DUPLICATION... *)
  let build_sg (g,s,n,g') =
    match (Options.getRules opts) with
    | Options.Detailed
    | Options.Finite -> [standard_form opts (Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']))]
    | Options.Default
    | Options.Infinite ->
        let g' = Gate.standard_form opts g' in
        (match (Gate.fast_reactions sub_map opts [g']) with
        | [] -> [standard_form opts (Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']))]
        | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Bind(Species.GATE g, Species.STRAND s, n, (Species.collate gs'' ss'' [])))) rs)
  in    
  (* Function to stick new gate to existing gates. *)
  let build_gg (g1,g2,n,g') =
    if (Options.getPolymers opts)
    then
      match (Options.getRules opts) with
      | Options.Detailed | Options.Finite -> [standard_form opts (Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']))]
      | Options.Default | Options.Infinite ->
          let g' = Gate.standard_form opts g' in
          (match (Gate.fast_reactions sub_map opts [g']) with
          | [] -> [standard_form opts (Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']))]
          | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Bind(Species.GATE g1, Species.GATE g2, n, (Species.collate gs'' ss'' [])))) rs)
    else
      Errors.illegal_polymerisation_error (Gate.display g1) (Gate.display g2)
  in
  let strand_gate_reactions =
    Lib.collect build_sg (Lib.collect (fun s -> Gate.stick_strand_to_gate sub_map (Options.getUnproductive opts) s g) existing_ss)
  in
  let gate_gate_reactions =
    Lib.collect build_gg (Lib.collect (Gate.stick_gate_to_gate sub_map opts g) (g::existing_gs))
  in
  strand_gate_reactions @ gate_gate_reactions

(** Compute all possible binding reactions in a new strand that make a hairpin. *)
let pin_reactions_new_strand sub_map (opts:options) (s:strand)  =
  (* Function to pin new strand into hairpin. *)
  let build_pins (s1,n,g') =
    match (Options.getRules opts) with
    | Options.Detailed
    | Options.Finite -> [standard_form opts (Pin(Species.STRAND s1, n, [Species.GATE g']))]
    | Options.Default
    | Options.Infinite ->
        let g' = Gate.standard_form opts g' in
        (match (Gate.fast_reactions sub_map opts [g']) with
        | [] -> [standard_form opts (Pin(Species.STRAND s1, n, [Species.GATE g']))]
        | rs -> List.map (fun (gs,ss) -> standard_form opts (Pin(Species.STRAND s1, n, (Species.collate gs ss [])))) rs)
  in
  (Lib.collect build_pins (Gate.pin_strand (Options.getUnproductive opts) s))

(** Compute all possible pining reactions in a new gate. *)
let pin_reactions_new_gate sub_map (opts:options) (g:gate) =
  let build_gg (g,n,g') =
    match (Options.getRules opts) with
     | Options.Detailed | Options.Finite -> [standard_form opts (Pin(Species.GATE g, n, [Species.GATE g']))]
     | Options.Default | Options.Infinite ->
        let g'' = Gate.standard_form opts g' in
         (match (Gate.fast_reactions sub_map opts [g'']) with
          | [] -> [standard_form opts (Pin(Species.GATE g, n, [Species.GATE g'']))]
          | rs -> List.map (fun (gs,ss) -> standard_form opts (Pin(Species.GATE g, n, (Species.collate gs ss [])))) rs)
  in
  let pin_gate_reactions = Lib.collect build_gg (Gate.pin_gate (Options.getUnproductive opts) g) 
  in pin_gate_reactions

(** Compute all possible leak pin migration reactions on a new gate. *)
let pin_migration_new_gate sub_map (opts:options) (g:gate) =
  if Options.getPinLeaks opts then
    let build (hp,g,ns,g') =
      match (Options.getRules opts) with
      | Options.Detailed | Options.Finite -> [standard_form opts (Remote(hp,Migrate(g,ns,g')))]
      | Options.Default | Options.Infinite ->
          let g'' = Gate.standard_form opts g' in
           (match (Gate.fast_reactions sub_map opts [g'']) with
            | [] -> [standard_form opts (Remote(hp,Migrate(g,ns,g'')))]
            | rs -> List.map (fun (gs,ss) -> standard_form opts (Remote(hp,Displace(g, ns, (Species.collate gs ss []))))) rs)
    in
    Lib.collect build (Gate.close_migration_reactions opts g)
  else []

(** Compute all possible leak pin displacing reactions on a new gate. *)
let pin_displacing_new_gate sub_map (opts:options) (g:gate) =
  if Options.getPinLeaks opts then
    let build (hp,g,ds,gs,ss) =
      match (Options.getRules opts) with
       | Options.Detailed | Options.Finite -> [standard_form opts (Remote(hp,Displace (g, ds, (List.map Species.STRAND ss)@(List.map Species.GATE gs))))]
       | Options.Default | Options.Infinite ->
          let gs' = List.map (Gate.standard_form opts) gs in
           (match (Gate.fast_reactions sub_map opts gs') with
            | [] -> [standard_form opts (Remote(hp,Displace (g, ds, (List.map Species.STRAND ss)@(List.map Species.GATE gs'))))]
            | rs -> List.map (fun (gs,ss) -> standard_form opts (Remote(hp,Displace(g, ds, (Species.collate gs ss []))))) rs)
    in
    Lib.collect build (Gate.close_displacing_reactions opts g)
  else []

(** Compute all possible leak pin open reactions on a new gate. *)
let pin_open_new_gate (opts:options) (g:gate) =
  if Options.getPinLeaks opts then
    List.map (fun (hp,g,ns,g') -> standard_form opts (Remote(hp,MigrateOpen(g,ns,g')))) (Gate.close_open_reactions opts g)
  else []

(** Compute all possible leak reactions between a new strand and the existing gates/strands. *)
let leak_reactions_new_strand sub_map (opts:options) (s:strand) (existing_gs:gate list) _ =
  if (Options.getLeaks opts) then
    let build_single_step (g,s,fast,gs',ss') = [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))] in (* sem = Detailed *)
    let build_merged_tau (g,s,fast,gs',ss') = [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))] in  (* sem = Finite *)
    let build_merged_original (g,s,fast,gs',ss') = (* sem = Default *)
      let gs' = List.map (Gate.standard_form opts) gs' in
      match (Gate.fast_reactions sub_map opts gs') with
        | [] -> [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))]
        | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs'' (ss'@ss'') [])))) rs
    in
    let build_merged_infinite (g,s,fast,gs',ss') = (* sem = InFinite *) (* %%%%%%%%%%%%%%%%%%%% CODE DUPLICATION!!! *)
      let gs' = List.map (Gate.standard_form opts) gs' in
      match (Gate.fast_reactions sub_map opts gs') with
        | [] -> [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))]
        | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs'' (ss'@ss'') [])))) rs
    in
    let build = 
      match (Options.getRules opts) with
      | Options.Detailed -> build_single_step
      | Options.Finite -> build_merged_tau
      | Options.Default -> build_merged_original
      | Options.Infinite -> build_merged_infinite
    in
    Lib.collect build (Lib.collect (Gate.leak_strand_into_gate opts s) existing_gs)
  else
    []

(** Compute all possible leak reactions between a new gate and the existing gates/strands. *)
let leak_reactions_new_gate sub_map (opts:options) (g:gate) (existing_gs:gate list) (existing_ss:strand list) =
  if (Options.getLeaks opts) then
    (* Functions for dealing with strand-gate leaks. *)
    let build_sg_single_step (g,s,fast,gs',ss') = [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))] in (* sem = Detailed *)
    let build_sg_merged_tau (g,s,fast,gs',ss') = [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))] in  (* sem = Finite *)
    let build_sg_merged_original (g,s,fast,gs',ss') = (* sem = Default *)
      let gs' = List.map (Gate.standard_form opts) gs' in
      match (Gate.fast_reactions sub_map opts gs') with
      | [] -> [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))]
      | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs'' (ss'@ss'') [])))) rs
    in
    let build_sg_merged_infinite (g,s,fast,gs',ss') = (* sem = InFinite *) (* %%%%%%%%%%%%%%%%%%%% CODE DUPLICATION!!! *)
      let gs' = List.map (Gate.standard_form opts) gs' in
      match (Gate.fast_reactions sub_map opts gs') with
      | [] -> [standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs' ss' [])))]
      | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Leak(Species.GATE g, Species.STRAND s, fast, (Species.collate gs'' (ss'@ss'') [])))) rs
    in
    let build_sg = 
      match (Options.getRules opts) with
      | Options.Detailed -> build_sg_single_step
      | Options.Finite -> build_sg_merged_tau
      | Options.Default -> build_sg_merged_original
      | Options.Infinite -> build_sg_merged_infinite
    in
    (* Functions for dealing with gate-gate leaks. *)
    let build_gg_single_step (g1,g2,fast,gs',ss') = [standard_form opts (Leak(Species.GATE g1, Species.GATE g2, fast, (Species.collate gs' ss' [])))] in (* sem = Detailed *)
    let build_gg_merged_tau (g1,g2,fast,gs',ss') = [standard_form opts (Leak(Species.GATE g1, Species.GATE g2, fast, (Species.collate gs' ss' [])))] in  (* sem = Finite *)
    let build_gg_merged_original (g1,g2,fast,gs',ss') = (* sem = Default *)
      let gs' = List.map (Gate.standard_form opts) gs' in
      match (Gate.fast_reactions sub_map opts gs') with
      | [] -> [standard_form opts (Leak(Species.GATE g1, Species.GATE g2, fast, (Species.collate gs' ss' [])))]
      | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Leak(Species.GATE g1, Species.GATE g2, fast, (Species.collate gs'' (ss'@ss'') [])))) rs
    in
    let build_gg_merged_infinite (g1,g2,fast,gs',ss') = (* sem = InFinite *) (* %%%%%%%%%%%%%%%%%%%% CODE DUPLICATION!!! *)
      let gs' = List.map (Gate.standard_form opts) gs' in
      match (Gate.fast_reactions sub_map opts gs') with
      | [] -> [standard_form opts (Leak(Species.GATE g1, Species.GATE g2, fast, (Species.collate gs' ss' [])))]
      | rs -> List.map (fun (gs'',ss'') -> standard_form opts (Leak(Species.GATE g1, Species.GATE g2, fast, (Species.collate gs'' (ss'@ss'') [])))) rs
    in
    let build_gg =
      match (Options.getRules opts) with
      | Options.Detailed -> build_gg_single_step
      | Options.Finite -> build_gg_merged_tau
      | Options.Default -> build_gg_merged_original
      | Options.Infinite -> build_gg_merged_infinite
    in
    (* Gather up the different kinds of interaction. *)
    let strand_gate_leaks = Lib.collect build_sg (Lib.collect (fun s -> Gate.leak_strand_into_gate opts s g) existing_ss) in
    (* If we've not selected polymers, do we throw an exception here? *)
    let gate_gate_leaks =
      if (Options.getPolymers opts)
      then Lib.collect build_gg (Lib.collect (Gate.leak_gate_into_gate opts g) (g::existing_gs))
      else [] (* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% failwith "Leaks enabled but polymers disabled. Should polymerising leaks be silently ignored?" *)
    in
    (* NB: Loop over g::gs because g could polymerise with another copy of itself. *)
    strand_gate_leaks @ gate_gate_leaks
  else []

(** Compute all possible tau reactions from a gate. *)
let tau_reactions sub_map (opts:options) (g:gate) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Finite -> List.map (fun (gs',ss') -> standard_form opts (Tau(g,(Species.collate gs' ss' [])))) (Gate.fast_reactions sub_map opts [g])
  | Options.Default | Options.Infinite | Options.Detailed -> []

(** Compute all possible displacement reactions from a gate. *)
let displacement_reactions (opts:options) (g:gate) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Detailed -> List.map (fun (g,ns,gs',ss') -> standard_form opts (Displace(g,ns,(Species.collate gs' ss' [])))) (Gate.displacement_reactions opts g)
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Compute all possible migration reactions on a gate. *)
let migration_reactions (opts:options) (g:gate) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Detailed -> List.map (fun (g,ns,g') -> standard_form opts (Migrate(g,ns,g'))) (Gate.migration_reactions g)
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Compute all possible migration open reactions on a gate. *)
let migration_open_reactions (opts:options) (g:gate) =
  match Options.getRules opts with
  | Options.Detailed -> List.map (fun (g,ns,g') -> standard_form opts (MigrateOpen(g,ns,g'))) (Gate.open_migration_reactions opts g)
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Compute all possible cover reactions on a gate. *)
let cover_reactions (opts:options) (g:gate) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Detailed -> List.map (fun (g,n,g') -> standard_form opts (Cover(g,n,g'))) (Gate.cover_reactions g)
  | Options.Default | Options.Infinite | Options.Finite -> []

(******************************************************************************)
(* Reactions between origami and other species                                *)


(* Origami entry reactions with a new gate *)
let entry_reactions_new_gate sub_map (opts:options) (g:gate) (existing_os:origami list) =
  let build_ogs (o,(g,s,n,g',o')) =
    match (Options.getRules opts) with
    | Options.Detailed
    | Options.Finite -> 
      [standard_form opts (OrigamiEntry(o,Species.GATE g,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
    | Options.Default
    | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        (match (Origami.fast_reactions sub_map opts o') with
        | [] -> [standard_form opts (OrigamiEntry(o,Species.GATE g,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
        | rs -> List.map (fun (gs',ss',o'') ->
                          if List.isEmpty gs' && List.isEmpty ss' 
                            then standard_form opts (OrigamiEntry(o,Species.GATE g,(Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g'])),o''))
                            else standard_form opts (OrigamiExchange(o,Species.GATE g,(Bind(Species.GATE g, Species.STRAND s, n,[Species.GATE g'])),Species.collate gs' ss' [o''])))
                         rs)

  let build_ogg (o,(g1,g2,n,g',o')) =
    if (Options.getPolymers opts) then
      match (Options.getRules opts) with
      | Options.Detailed
      | Options.Finite -> 
        [standard_form opts (OrigamiEntry(o,Species.GATE g1,Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']),o'))]
      | Options.Default
      | Options.Infinite ->
        let o' = Origami.standard_form opts o'
        match (Origami.fast_reactions sub_map opts o') with
        | [] -> [standard_form opts (OrigamiEntry(o,Species.GATE g1,Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']),o'))]
        | rs -> List.map (fun (gs',ss',o'') -> 
                          if List.isEmpty gs' && List.isEmpty ss'
                          then standard_form opts (OrigamiEntry(o,Species.GATE g1,Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']),o''))
                          else standard_form opts (OrigamiExchange(o,Species.GATE g1, Bind(Species.GATE g1,Species.GATE g2,n,[Species.GATE g']), Species.collate gs' ss' [o''])))
                          rs
    else
      Errors.illegal_polymerisation_error (Gate.display g1) (Gate.display g2)
 
  let gate_strand_entries = Lib.collect (fun o -> List.map (fun r -> (o,r)) (Origami.entry_reactions_gs sub_map opts o g)) existing_os
  let gate_gate_entries = Lib.collect (fun o -> List.map (fun r -> (o,r)) (Origami.entry_reactions_gg sub_map opts o g)) existing_os
  (Lib.collect build_ogs gate_strand_entries)@(Lib.collect build_ogg gate_gate_entries) 

(* Origami entry reactions with a new gate *)
let entry_reactions_new_strand sub_map (opts:options) (s:strand) (existing_os:origami list) =
  let build_oss (o,(s1,s2,n,g',o')) =
    match (Options.getRules opts) with
    | Options.Detailed | Options.Finite -> 
      [standard_form opts (OrigamiEntry(o,Species.STRAND s1,Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']),o'))]
    | Options.Default | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        (match (Origami.fast_reactions sub_map opts o') with
        | [] -> [standard_form opts (OrigamiEntry(o,Species.STRAND s1,Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']),o'))]
        | rs -> List.map (fun (gs',ss',o'') -> 
                          if List.isEmpty gs' && List.isEmpty ss'
                          then standard_form opts (OrigamiEntry(o,Species.STRAND s1,(Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g'])),o''))
                          else standard_form opts (OrigamiExchange(o,Species.STRAND s1,Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']), Species.collate gs' ss' [o''])))
                         rs)

  let build_osg (o,(g,s,n,g',o')) =
     match (Options.getRules opts) with
     | Options.Detailed
     | Options.Finite -> 
       [standard_form opts (OrigamiEntry(o,Species.STRAND s,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
     | Options.Default
     | Options.Infinite ->
       let o' = Origami.standard_form opts o' in
       (match (Origami.fast_reactions sub_map opts o') with
         | [] -> [standard_form opts (OrigamiEntry(o,Species.STRAND s,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
         | rs -> List.map (fun (gs',ss',o'') -> 
                           if List.isEmpty gs' && List.isEmpty ss'
                           then standard_form opts (OrigamiEntry(o,Species.STRAND s,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o''))
                           else standard_form opts (OrigamiExchange(o,Species.STRAND s, Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),Species.collate gs' ss' [o''])))
                           rs)

  let strand_strand_entries = Lib.collect (fun o -> List.map (fun r -> (o,r)) (Origami.entry_reactions_ss sub_map opts o s)) existing_os
  let strand_gate_entries = Lib.collect (fun o -> List.map (fun r -> (o,r)) (Origami.entry_reactions_sg sub_map opts o s)) existing_os
  (Lib.collect build_oss strand_strand_entries)@(Lib.collect build_osg strand_gate_entries) 

(* All entry reactions that occur when a new origami is created *)
let entry_reactions_new_origami sub_map (opts:options) (o:origami) (existing_ss:strand list) (existing_gs:gate list) =
  let build_ogs (g,s,n,g',o') =
    match (Options.getRules opts) with
    | Options.Detailed
    | Options.Finite -> 
      [standard_form opts (OrigamiEntry(o,Species.GATE g,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
    | Options.Default
    | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        (match (Origami.fast_reactions sub_map opts o') with
        | [] -> [standard_form opts (OrigamiEntry(o,Species.GATE g,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
        | rs -> List.map (fun (gs',ss',o'') -> 
                          if List.isEmpty gs' && List.isEmpty ss'
                           then standard_form opts (OrigamiEntry(o,Species.GATE g,(Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g'])),o''))
                           else standard_form opts (OrigamiExchange(o,Species.GATE g,(Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g'])),Species.collate gs' ss' [o''])))
                          rs)
  
  let build_ogg (g1,g2,n,g',o') =
    if (Options.getPolymers opts) then
      let inner_react = Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']) in
      match (Options.getRules opts) with
      | Options.Detailed
      | Options.Finite -> 
        [standard_form opts (OrigamiEntry(o,Species.GATE g1,inner_react,o'))]
      | Options.Default
      | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        (match (Origami.fast_reactions sub_map opts o') with
          | [] -> [standard_form opts (OrigamiEntry(o,Species.GATE g1,inner_react,o'))]
          | rs -> List.map (fun (gs',ss',o'') -> 
                            if List.isEmpty gs' && List.isEmpty ss'
                              then standard_form opts (OrigamiEntry(o,Species.GATE g1,inner_react,o''))
                              else standard_form opts (OrigamiExchange(o,Species.GATE g1,inner_react,Species.collate gs' ss' [o''])))
                            rs)
    else
      Errors.illegal_polymerisation_error (Gate.display g1) (Gate.display g2)
  
  let build_oss (s1,s2,n,g',o') = 
    let inner_react = Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']) in
    match (Options.getRules opts) with
     | Options.Detailed | Options.Finite -> 
       [standard_form opts (OrigamiEntry(o,Species.STRAND s1,inner_react,o'))]
     | Options.Default | Options.Infinite ->
         let o' = Origami.standard_form opts o' in
         (match (Origami.fast_reactions sub_map opts o') with
         | [] -> [standard_form opts (OrigamiEntry(o,Species.STRAND s1,inner_react,o'))]
         | rs -> List.map (fun (gs',ss',o'') -> 
                           if List.isEmpty gs' && List.isEmpty ss'
                             then standard_form opts (OrigamiEntry(o,Species.STRAND s1,inner_react,o''))
                             else standard_form opts (OrigamiExchange(o,Species.STRAND s1,inner_react,Species.collate gs' ss' [o''])))
                           rs)

  let build_osg (g,s,n,g',o') =
      let inner_react = Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']) in
      match (Options.getRules opts) with
         | Options.Detailed | Options.Finite -> 
           [standard_form opts (OrigamiEntry(o,Species.STRAND s,inner_react,o'))]
         | Options.Default | Options.Infinite ->
           let o' = Origami.standard_form opts o' in
           (match (Origami.fast_reactions sub_map opts o') with
             | [] -> [standard_form opts (OrigamiEntry(o,Species.STRAND s,inner_react,o'))]
             | rs -> List.map (fun (gs',ss',o'') -> 
                               if List.isEmpty gs' && List.isEmpty ss'
                               then standard_form opts (OrigamiEntry(o,Species.STRAND s,inner_react,o''))
                               else standard_form opts (OrigamiExchange(o,Species.STRAND s,inner_react,Species.collate gs' ss' [o''])))
                              rs)
  
  let strand_strand_entries = Lib.collect (Origami.entry_reactions_ss sub_map opts o) existing_ss
  let strand_gate_entries = Lib.collect (Origami.entry_reactions_sg sub_map opts o) existing_ss
  let gate_strand_entries = Lib.collect (Origami.entry_reactions_gs sub_map opts o) existing_gs
  let gate_gate_entries = Lib.collect (Origami.entry_reactions_gg sub_map opts o) existing_gs

  (Lib.collect build_oss strand_strand_entries)@
  (Lib.collect build_osg strand_gate_entries)@
  (Lib.collect build_ogs gate_strand_entries)@
  (Lib.collect build_ogg gate_gate_entries)


(******************************************************************************)

(* Is a reaction an origami reaction from given origami *)
let origami_reaction (o:origami) = function 
 | OrigamiReact(o',_,_)
 | OrigamiEntry (o',_,_,_)
 | OrigamiExpel (o',_,_)
 | OrigamiExchange(o',_,_,_) -> o=o' (* RLP: Does this expect normal form? *)
 | _ -> false

let looks_like_origami_reaction (o:origami) (r:sreaction) =
  r.reactants |>  Mset.nonzero_elements |> List.exists ((=) (Species.ORIGAMI o)) (* RLP: Does this expect normal form? *)

(* Pull out the internal reaction that happened within the origami *)
let origami_internal_reaction = function
  | OrigamiReact(_,r,_)
  | OrigamiEntry(_,_,r,_)
  | OrigamiExpel(_,r,_)
  | OrigamiExchange(_,_,r,_) -> r
  | _ -> failwith "origami_internal_reaction not given only origami reactions"

(* Use the reactants of existing origami reactions to eliminate gates from other reactions, since they existed on previous iterations *)
let rec origami_possible_new_gates (opts:options) (rs:t list) (o: origami) (gates : gate list)=
  let ors = List.filter (origami_reaction o) rs
  let reactant_gates =
    ors
    |> Lib.collect (fun r ->
                        match origami_internal_reaction r,r with
                        | Unbind(g,_,_),_
                        | UnbindOpen(g,_,_),_
                        | Pin(Species.GATE g,_,_),_
                        | Tau(g,_),_
                        | Displace(g,_,_),_
                        | Migrate(g,_,_),_
                        | Cover(g,_,_),_
                        | MigrateOpen(g,_,_),_
                            -> [g]

                        | Bind(Species.GATE g,Species.STRAND _ ,_,_),OrigamiEntry(_,Species.STRAND _,_,_)
                        | Bind(Species.STRAND _ ,Species.GATE g,_,_),OrigamiEntry(_,Species.STRAND _,_,_) 
                        | Bind(Species.GATE g,Species.STRAND _ ,_,_),OrigamiExchange(_,Species.STRAND _,_,_)
                        | Bind(Species.STRAND _ ,Species.GATE g,_,_),OrigamiExchange(_,Species.STRAND _,_,_)
                            -> [g]

                        | Bind(Species.GATE g,Species.STRAND _ ,_,_),OrigamiEntry(_,Species.GATE g1,_,_)
                        | Bind(Species.STRAND _ ,Species.GATE g,_,_),OrigamiEntry(_,Species.GATE g1,_,_) 
                        | Bind(Species.GATE g,Species.STRAND _ ,_,_),OrigamiExchange(_,Species.GATE g1,_,_)
                        | Bind(Species.STRAND _ ,Species.GATE g,_,_),OrigamiExchange(_,Species.GATE g1,_,_) 
                            -> if Gate.equal opts g g1 then [] else [g]

                        | Bind(Species.GATE g1,Species.GATE g2,_,_),OrigamiEntry(_,Species.GATE g,_,_)
                        | Bind(Species.GATE g1,Species.GATE g2,_,_),OrigamiExchange(_,Species.GATE g,_,_) 
                            -> if Gate.equal opts g g1 then [g2] else [g1]

                        | Bind(Species.GATE g,Species.STRAND _ ,_,_),_
                        | Bind(Species.STRAND _ ,Species.GATE g,_,_), _
                            -> [g]

                        | Bind(Species.GATE g1,Species.GATE g2,_,_), _
                        | Bind(Species.GATE g1,Species.GATE g2,_,_), _
                            -> [g1;g2]
                                                                       
                        | _  -> [])
  Lib.difference (Gate.equal opts) gates reactant_gates

(* Use the reactants of existing origami reactions to eliminate strands from other reactions, since they existed on previous iterations *)
let rec origami_possible_new_strands _ (rs:t list) (o:origami) (strands : strand list)=
  let ors = List.filter (origami_reaction o) rs in
  let reactant_strands = ors |> Lib.collect (fun r ->
                                   match origami_internal_reaction r,r with
                                   | Pin(Species.STRAND s,_,_),_
                                   | Bind(Species.STRAND s,Species.GATE _ ,_,_),OrigamiEntry(_,Species.GATE _,_,_)
                                   | Bind(Species.GATE _ ,Species.STRAND s,_,_),OrigamiEntry(_,Species.GATE _,_,_) 
                                   | Bind(Species.STRAND s,Species.GATE _ ,_,_),OrigamiExchange(_,Species.GATE _,_,_)
                                   | Bind(Species.GATE _ ,Species.STRAND s,_,_),OrigamiExchange(_,Species.GATE _,_,_)
                                     -> [s]

                                   | Bind(Species.STRAND s,Species.GATE _ ,_,_),OrigamiEntry(_,Species.STRAND s1 ,_,_)
                                   | Bind(Species.GATE _ ,Species.STRAND s,_,_),OrigamiEntry(_,Species.STRAND s1,_,_) 
                                   | Bind(Species.STRAND s,Species.GATE _ ,_,_),OrigamiExchange(_,Species.STRAND s1,_,_)
                                   | Bind(Species.GATE _ ,Species.STRAND s,_,_),OrigamiExchange(_,Species.STRAND s1,_,_)
                                     -> if Strand.equal s s1 then [] else [s]

                                   | Bind(Species.STRAND s,Species.GATE _ ,_,_), _
                                   | Bind(Species.GATE _ ,Species.STRAND s,_,_), _
                                     -> [s]

                                   | Bind(Species.STRAND s1,Species.STRAND s2,_,_),OrigamiEntry(_,Species.STRAND s,_,_)
                                   | Bind(Species.STRAND s1,Species.STRAND s2,_,_),OrigamiExchange(_,Species.STRAND s,_,_) 
                                     -> if Strand.equal s s1 then [s2] else [s1]

                                   | Bind(Species.STRAND s1,Species.STRAND s2,_,_), _
                                   | Bind(Species.STRAND s1,Species.STRAND s2,_,_), _
                                     -> [s1;s2]
                                   
                                   | _  -> [])
  Lib.difference Strand.equal strands reactant_strands

(** Compute all possible unbinding reactions from a gate in an origami, that don't expel *)
let origami_unbind_reactions sub_map (opts:options) (o:origami) (gs:gate list) =
  let build_single_step (g,n,gs',ss,o') = standard_form opts (OrigamiReact(o,Unbind(g,n,(Species.collate gs' ss [])),o')) in  (* sem = Detailed *)
  let build_merged_original (g,n,gs',ss,o') =  (* sem = Default *)
    let o' = Origami.standard_form opts o' in
    match (Origami.fast_reactions sub_map opts o') with
      | [] -> [standard_form opts (OrigamiReact(o,Unbind(g,n,(Species.collate gs' ss [])),o'))]
      | rs -> List.map (fun (gs'',ss',o'') -> 
                        if List.isEmpty gs'' && List.isEmpty ss'
                          then standard_form opts (OrigamiReact(o,Unbind(g,n,(Species.collate gs' ss [])),o''))
                          else standard_form opts (OrigamiExpel(o,Unbind(g,n,(Species.collate gs' ss [])),Species.collate gs'' ss' [o''])))
                        rs
  in
  match (Options.getRules opts) with
    | Options.Detailed -> List.map build_single_step (Lib.collect (fun g -> Origami.unsticking_reactions sub_map opts g o) gs)
    | Options.Default -> Lib.collect build_merged_original (Lib.collect (fun g -> (Origami.unsticking_reactions sub_map opts g o)) gs)
    | Options.Infinite | Options.Finite -> []

(** Compute all possible unbinding reactions from a gate in an origami, that do expel *)
let origami_unbind_reactions_expel sub_map (opts:options) (o:origami) (gs:gate list) =
  let build_single_step (g,n,gs',ss,o',egs,ess) = 
      standard_form opts (OrigamiExpel(o,Unbind(g,n,(Species.collate gs' ss [])),
                                      [Species.ORIGAMI(o')]@(List.map Species.GATE egs)@(List.map Species.STRAND ess))) in  (* sem = Detailed *)
  let build_merged_original (g,n,gs',ss,o',egs,ess) =  (* sem = Default *)
    match (Origami.fast_reactions sub_map opts o') with
      | [] -> [standard_form opts (OrigamiExpel(o,Unbind(g,n,(Species.collate gs' ss [])),
                                                [Species.ORIGAMI o']@(List.map Species.GATE egs)@(List.map Species.STRAND ess)))]
      | rs -> List.map (fun (gs'',ss',o'') -> 
                        standard_form opts (OrigamiExpel(o,Unbind(g,n,(Species.collate gs' ss [])),Species.collate (egs@gs'') (ess@ss') [o''])))
                       rs
  in
  match (Options.getRules opts) with
    | Options.Detailed -> List.map build_single_step (Lib.collect (fun g -> Origami.unsticking_reactions_expel sub_map opts g o) gs)
    | Options.Default -> Lib.collect build_merged_original (Lib.collect (fun g -> (Origami.unsticking_reactions_expel sub_map opts g o)) gs)
    | Options.Infinite | Options.Finite -> []

(** Compute all possible unbind reactions that open a hairpin on gates in an origami*)
let origami_unbind_open_reactions sub_map (opts:options) (o:origami) (gs:gate list) =
  let build_single_step (g,n,gs',s',o') = standard_form opts (OrigamiReact(o,UnbindOpen(g,n,(Species.collate gs' s' [])),o')) in 
  let build_merged_original (g,n,gs',s',o') =
    let o' = Origami.standard_form opts o' in
    match (Origami.fast_reactions sub_map opts o') with
      | [] -> [standard_form opts (OrigamiReact(o,UnbindOpen(g,n,(Species.collate gs' s' [])),o'))]
      | rs -> List.map (fun (gs'', ss'',o'') -> 
                        if List.isEmpty gs'' && List.isEmpty ss''
                          then standard_form opts (OrigamiReact(o,Unbind(g,n,Species.collate gs' s' []),o''))
                        else standard_form opts (OrigamiExpel(o,Unbind(g,n,Species.collate gs' s' []),Species.collate gs'' ss'' [o'']))) rs
  in
  match (Options.getRules opts) with
    | Options.Detailed | Options.Finite -> List.map build_single_step (Lib.collect (fun g -> Origami.unbind_open_reactions opts g o) gs)
    | Options.Default | Options.Infinite -> Lib.collect build_merged_original (Lib.collect (fun g -> (Origami.unbind_open_reactions opts g o)) gs)

(** Compute all possible binding reactions in an origami with a new strands and gates *)
let origami_bind_reactions sub_map (opts:options) (o:origami) (ss:strand list) (gs:gate list)  =
  let build_ogs (g,s,n,g',o') =
    match (Options.getRules opts) with
    | Options.Detailed | Options.Finite -> 
      [standard_form opts (OrigamiReact(o,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
    | Options.Default | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        (match (Origami.fast_reactions sub_map opts o') with
        | [] -> [standard_form opts (OrigamiReact(o,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
        | rs -> List.map (fun (gs',ss',o'') ->
                          if List.isEmpty gs' && List.isEmpty ss' 
                            then standard_form opts (OrigamiReact(o,(Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g'])),o''))
                          else standard_form opts (OrigamiExpel(o,(Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g'])),Species.collate gs' ss' [o''])))
                          rs)
  in
  let build_ogg (g1,g2,n,g',o') =
     if (Options.getPolymers opts)
     then
         match (Options.getRules opts) with
         | Options.Detailed
         | Options.Finite -> 
           [standard_form opts (OrigamiReact(o,Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']),o'))]
         | Options.Default
         | Options.Infinite ->
           let o' = Origami.standard_form opts o' in
           (match (Origami.fast_reactions sub_map opts o') with
             | [] -> [standard_form opts (OrigamiReact(o,Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']),o'))]
             | rs -> List.map (fun (gs',ss',o'') ->
                               if List.isEmpty gs' && List.isEmpty ss' 
                                 then standard_form opts (OrigamiReact(o,Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']),o''))
                               else standard_form opts (OrigamiExpel(o,Bind(Species.GATE g1, Species.GATE g2, n, [Species.GATE g']),Species.collate gs' ss' [o''])))
                               rs)
     else Errors.illegal_polymerisation_error (Gate.display g1) (Gate.display g2)
  in
  let build_oss (s1,s2,n,g',o') =
    match (Options.getRules opts) with
    | Options.Detailed
    | Options.Finite -> 
       [standard_form opts (OrigamiReact(o,Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']),o'))]
    | Options.Default
    | Options.Infinite ->
         let o' = Origami.standard_form opts o' in
         (match (Origami.fast_reactions sub_map opts o') with
         | [] -> [standard_form opts (OrigamiReact(o,Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g']),o'))]
         | rs -> List.map (fun (gs',ss',o'') -> 
                           if List.isEmpty gs' && List.isEmpty ss' 
                             then standard_form opts (OrigamiReact(o,(Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g'])),o''))
                           else standard_form opts (OrigamiExpel(o,(Bind(Species.STRAND s1, Species.STRAND s2, n, [Species.GATE g'])),Species.collate gs' ss' [o''])))
                           rs)
  in
  let build_osg (g,s,n,g',o') =
      match (Options.getRules opts) with
      | Options.Detailed | Options.Finite -> 
        [standard_form opts (OrigamiReact(o,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
      | Options.Default | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        (match (Origami.fast_reactions sub_map opts o') with
          | [] -> [standard_form opts (OrigamiReact(o,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o'))]
          | rs -> List.map (fun (gs',ss',o'') -> 
                            if List.isEmpty gs' && List.isEmpty ss' 
                              then standard_form opts (OrigamiReact(o,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),o''))
                            else standard_form opts (OrigamiExpel(o,Bind(Species.GATE g, Species.STRAND s, n, [Species.GATE g']),Species.collate gs' ss' [o''])))
                            rs)
  in
  let gate_strand = Lib.collect build_ogs (Lib.collect (Origami.sticking_reactions_gs sub_map opts o) gs) in
  let gate_gate = Lib.collect build_ogg (Lib.collect (Origami.sticking_reactions_gg sub_map opts o) gs) in
  let strand_strand = Lib.collect build_oss (Lib.collect (Origami.sticking_reactions_ss sub_map opts o) ss) in
  let strand_gate = Lib.collect build_osg (Lib.collect (Origami.sticking_reactions_sg sub_map opts o) ss) in
  (*Remove duplicate reactions already found in gate_strand when there are "new" gates and strands *)
  let strand_gate = if (List.isEmpty ss || List.isEmpty gs) then strand_gate    (* exists??? *)
                      else Lib.fold_left (fun acc r -> if (Lib.forall (fun r2 -> matching_reactions opts r r2) gate_strand) then acc else r::acc) [] strand_gate in
  strand_strand@strand_gate@gate_strand@gate_gate

(** Compute all possible binding reactions that make a hairpin in an origami with new strands and gates. *)
let origami_pin_reactions sub_map (opts:options) (o:origami) =
  let build_s (s1,n,g',o') =
    match (Options.getRules opts) with
    | Options.Detailed | Options.Finite -> [standard_form opts (OrigamiReact(o,Pin(Species.STRAND s1, n, [Species.GATE g']),o'))]
    | Options.Default | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        match (Origami.fast_reactions sub_map opts o') with
        | [] -> [standard_form opts (OrigamiReact(o,Pin(Species.STRAND s1, n, [Species.GATE g']),o'))]
        | rs -> List.map
                  (fun (gs,ss,o'') -> 
                          if List.isEmpty gs && List.isEmpty ss then
                            standard_form opts (OrigamiReact(o,Pin(Species.STRAND s1, n, [Species.GATE g']),o''))
                          else
                            standard_form opts (OrigamiExpel(o,Pin(Species.STRAND s1, n, [Species.GATE g']), Species.collate gs ss [o''])))
                  rs

  let build_g (g,n,g',o') =
    match (Options.getRules opts) with
     | Options.Detailed | Options.Finite -> [standard_form opts (OrigamiReact(o,Pin(Species.GATE g, n, [Species.GATE g']),o'))]
     | Options.Default | Options.Infinite ->
        let o' = Origami.standard_form opts o' in
        match (Origami.fast_reactions sub_map opts o') with
        | [] -> [standard_form opts (OrigamiReact(o,Pin(Species.GATE g, n, [Species.GATE g']),o'))]
        | rs -> List.map
                  (fun (gs,ss,o'') -> 
                          if List.isEmpty gs && List.isEmpty ss then
                            standard_form opts (OrigamiReact(o,Pin(Species.GATE g, n, [Species.GATE g']),o''))
                          else
                            standard_form opts (OrigamiExpel(o,Pin(Species.GATE g, n, [Species.GATE g']),Species.collate gs ss [o''])))
                  rs

  let pin_strand_reactions = Lib.collect build_s (Origami.pinning_reactions_s opts o) in
  let pin_gate_reactions = Lib.collect build_g (Origami.pinning_reactions_g opts o) in
   pin_strand_reactions@pin_gate_reactions

(** Compute all possible leak pin migration reactions inside an origami. *)
let origami_pin_migration sub_map (opts:options) (o:origami) =
  if Options.getPinLeaks opts then
    let build (hp,g,ns,g',o') =
      match (Options.getRules opts) with
      | Options.Detailed
      | Options.Finite ->
        [standard_form opts (OrigamiReact(o,Remote(hp,Migrate(g,ns,g')),o'))]
      | Options.Default
      | Options.Infinite ->
        let o'' = Origami.standard_form opts o' in
        match (Origami.fast_reactions sub_map opts o'') with
        | [] -> [standard_form opts (OrigamiReact(o,Remote(hp,Migrate(g,ns,g')),o''))]
        | rs -> List.map
                  (fun (gs,ss,o'') -> 
                        if List.isEmpty gs && List.isEmpty ss then
                          standard_form opts (OrigamiReact(o,Remote(hp,Migrate(g,ns,g')),o''))
                        else
                          standard_form opts (OrigamiExpel(o,Remote(hp,Migrate(g,ns,g')),Species.collate gs ss [o''])))
                  rs

    Lib.collect build (Origami.close_migration_reactions opts o)
  else
    []

(** Compute all possible leak pin displacing reactions inside an origami. *)
let origami_pin_displacing sub_map (opts:options) (o:origami) =
  if Options.getPinLeaks opts then
    let build (hp,g,ds,gs,ss,o') =
      let react (fgs, fss, o'') = 
        if List.isEmpty gs && List.isEmpty ss && List.isEmpty fgs && List.isEmpty fss then
          standard_form opts (OrigamiReact(o,(Remote(hp,Displace (g, ds, (List.map Species.STRAND ss)@(List.map Species.GATE gs)))),o''))
        else
          standard_form opts (OrigamiExpel( o,(Remote(hp,Displace (g, ds, (List.map Species.STRAND ss)@(List.map Species.GATE gs))))
                                          , Species.collate (fgs@gs) (fss@ss) [o'']))

      match (Options.getRules opts) with
      | Options.Detailed
      | Options.Finite ->
        [react ([],[],o')]
      | Options.Default
      | Options.Infinite ->
         let o'' = Origami.standard_form opts o'
         match (Origami.fast_reactions sub_map opts o'') with
         | [] -> [react ([],[],o'')]
         | rs -> List.map react rs

    Lib.collect build (Origami.close_displacing_reactions opts o)
  else []

(** Compute all possible leak pin open reactions inside an origami. *)
let origami_pin_open (opts:options) (o:origami) =
  if Options.getPinLeaks opts then
    List.map (fun (hp,g,ns,g',o') -> standard_form opts (OrigamiReact(o,Remote(hp,MigrateOpen(g,ns,g')),o'))) (Origami.close_open_reactions opts o)
  else []

(** Compute all possible tau reactions from new gates in an origami. *)
let origami_tau_reactions sub_map (opts:options) (o:origami) =
  let gs, _ = Origami.origami_species o in
  let sem = Options.getRules opts in
  match sem with
  | Options.Finite -> 
    Lib.collect (fun g -> 
                 List.map (fun (gs',ss',o') -> 
                           if List.isEmpty gs' && List.isEmpty ss'
                            then standard_form opts (OrigamiReact(o,Tau(g,[]),o'))
                            else standard_form opts (OrigamiExpel(o,Tau(g,[]),Species.collate gs' ss' [o'])))
                          (Origami.fast_reactions sub_map opts o)) gs
  | Options.Default | Options.Infinite | Options.Detailed -> []

(** Compute all possible displacement reactions from new gates in an origami, that don't expel. *)
let origami_displacement_reactions (opts:options) (o:origami) (gs:gate list) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Detailed -> 
    Lib.collect (fun g ->
                 List.map (fun (g,ns,gs',ss',o') -> standard_form opts (OrigamiReact(o,Displace(g,ns,(Species.collate gs' ss' [])),o')))
                          (Origami.displacement_reactions opts g o)) gs
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Compute all possible displacement reactions from new gates in an origami, that do expel. *)
let origami_displacement_reactions_expel (opts:options) (o:origami) (gs:gate list) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Detailed -> 
    Lib.collect (fun g ->
                 List.map (fun (g,ns,gs',ss',o',egs,ess) -> 
                            standard_form opts (OrigamiExpel(o,Displace(g,ns,(Species.collate gs' ss' [])),
                                                             [Species.ORIGAMI o']@(List.map Species.GATE egs)@(List.map Species.STRAND ess))))
                          (Origami.displacement_reactions_expel opts g o)) gs
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Compute all possible migration reactions on new gates in an origami. *)
let origami_migration_reactions (opts:options) (o:origami)(gs:gate list) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Detailed -> Lib.collect (fun g ->
                                            List.map (fun (g,ns,g',o') -> standard_form opts (OrigamiReact(o,Migrate(g,ns,g'),o'))) 
                                                                          (Origami.migration_reactions opts g o))
                                                     gs
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Compute all possible migration open reactions on new gates in an origami. *)
let origami_migration_open_reactions (opts:options) (o:origami) (gs:gate list) =
  match Options.getRules opts with
  | Options.Detailed -> Lib.collect (fun g ->
                              List.map (fun (g,ns,g',o') -> standard_form opts (OrigamiReact(o,MigrateOpen(g,ns,g'),o')))
                                                            (Origami.open_migration_reactions opts g o))
                                       gs
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Compute all possible cover reactions on new gates in an origami. *)
let origami_cover_reactions (opts:options) (o:origami) (gs:gate list) =
  let sem = Options.getRules opts in
  match sem with
  | Options.Detailed -> Lib.collect (fun g->
                               List.map (fun (g,n,g',o') -> standard_form opts (OrigamiReact(o,Cover(g,n,g'),o')))
                                                            (Origami.cover_reactions opts g o))
                                        gs
  | Options.Default | Options.Infinite | Options.Finite -> []

(** Enzymatic reactions *******************************************************)
(*
  These typically does not depend on the existing set of species, as the new species is reacting with enzymes instead.
  But it then depends on the list of enzymes; should we represent enzymes as species rather than presence?
*)

(* f gives list of possible replacements for an element, all one-element replacements are then calculated *)
let rec replace f = function
  | [] -> []
  | [x] -> List.map (fun y -> [y]) (f x)
  | x::xs -> 
    let rxs = replace f xs in
    let later = List.map (fun l -> x::l) rxs in
    let now = List.map (fun r -> r::xs) (f x) in
    now @ later

let find_nicks (x, y) (s: domain list) =
  let rec inner acc processed = function
    | [] -> acc
    | [_] -> acc
    | a::b::t ->
      let top_nicks = 
        if (a = x && b = y) then (* nick top strand *)
          [List.rev (a::processed), b::t, true]
        else []
      let bottom_nicks =
        if (Domain.are_complements a y && Domain.are_complements b x) then (* nick bottom strand *)
          [List.rev (a::processed), b::t, true]
        else []
      inner (top_nicks @ bottom_nicks @ acc) (a::processed) (b::t) in
  inner [] [] s

let nick_gate (x, y) g =
  let rec outer acc outer_processed inner_processed = function
    | [] -> acc
    | []::os -> outer acc (inner_processed::outer_processed) [] os
    | (Segment.Double (a,b,s,c,d)::is)::os ->
      let nicks = find_nicks (x, y) s in
      let apply_nick (before, after, top) =
        if top then (* top is nicked so need to join at the bottom strand, ie at the inner list *)
             List.rev outer_processed
           @ [ List.rev inner_processed
             @ Segment.Double (a,b,before,[],[])
            :: Segment.Double ([],[],after,c,d)
            :: is ]
           @ os          
        else (* bottom is nicked so need to join at the bottom strand, ie at the outer list *)
             (List.rev ((Segment.Double (a,b,before,[],[]) :: List.rev inner_processed) :: outer_processed))
           @ ((Segment.Double ([],[],after,c,d) :: is) :: os) in
      outer ((List.map apply_nick nicks) @ acc) outer_processed (Segment.Double (a,b,s,c,d) :: inner_processed) (is::os)
    | (i::is)::os -> outer acc outer_processed (i::inner_processed) (is::os) in
  outer [] [] [] g

let nick_reactions (g:gate) (e:enzyme) =
  match e with
  | Nicking (x, y) ->
      let new_gates = nick_gate (x, y) g in
      List.map (fun rg -> Nick (Species.GATE g, e, Species.GATE rg)) new_gates
  | Polymerase _ -> []

let rec nick_reactions_species (i:species) (e:enzyme) =
  match e with
  | Nicking (x, y) ->
    begin
      match i with
      | Species.STRAND _ -> []
      | Species.GATE g ->
        let new_gates = nick_gate (x, y) g in
        List.map (fun rg -> Nick (Species.GATE g, e, Species.GATE rg)) new_gates
(*
      | Species.COMPLEX c ->
        begin
          match c with
          | Branch.Solo g -> nick_reactions_species (Species.GATE g) e
          | _ -> [] (* TODO *)
        end
*)
      | Species.ORIGAMI _ -> [] (* TODO *)
      | Species.UNKNOWN _ -> []
      | Species.LogicDsdProcess _ -> failwith "unexpected LogicDSD species"
    end
  | Polymerase _ -> []


let Replication_reactions (_:species) (e:enzyme) =
  match e with
  | Nicking _ -> []
  | Polymerase _ -> []

(******************************************************************************)
  
(* Add a list of reactions onto the existing list.
 * This function does some filtering, to remove circular reactions and
 * filter unwanted leak reactions (ones where there is an equivalent faster reaction). *)
let combine (opts:options) (rs_existing:t list) (rs_new:t list) =
  let add_one (rs:t list) (r:t) =
    (* Remove any circular reactions (i.e. where reactants and products are the same. *)
    if (is_circular opts r) then rs else
      (* Fast leak reaction *)
      if (is_fast_leak r) then
        if List.exists (fun r' -> (not(is_leak r')) && matching_species opts r r') rs then rs
        else let rs = List.filter (fun r' -> not(is_leak r' && matching_species opts r r')) rs in rs@[r]
      (* Slow leak reaction *)
      else if (is_slow_leak r) then
        if List.exists (fun r' -> (not(is_slow_leak r')) && matching_species opts r r') rs then rs
        else let rs = List.filter (fun r' -> not(is_slow_leak r' && matching_species opts r r')) rs in rs@[r]
      (* Tau reaction *)
      else if (is_tau r) then
        if List.exists (fun r' -> is_tau r' && matching_species opts r r') rs then rs
        else let rs = List.filter (fun r' -> not(is_leak r' && matching_species opts r r')) rs in rs@[r]
      (* Other reaction *)
      else
        (*call to is_leak is questionable, as it does allow duplicate reactions which may be good but may be too permissive *)
        let rs = List.filter (fun r' -> not(is_leak r' && matching_species opts r r')) rs in rs@[r]
  in
  Lib.fold_left add_one rs_existing rs_new

(* Append a list of reaction lists, while filtering. *)
let combine_many (opts:options) (rss:t list list) = Lib.fold_left (combine opts) [] rss

(******************************************************************************)

(* "Fix" a reaction by replacing any rotated species with their original versions. *)
let rec fix (opts:options) (existing_gs:gate list) (existing_ss:strand list) (existing_os:origami list) (r:t) =
  let existing_gs, existing_ss =
    Lib.fold_left
      (fun (acc_gates, acc_strands) o ->
         let gs, ss = Origami.origami_species o in
         gs@acc_gates, ss@acc_strands)
      (existing_gs, existing_ss)
      existing_os in
  let fix_gate (g:gate) =
    let g_rot = Gate.rotate opts g in
    if (List.contains g_rot existing_gs) then g_rot else g
  in
  let fix_strand (s:strand) = (* RLP: This should now be unnecessary, since normalize chooses a rotation *)
    let s_rot = Strand.rotate s in
    if (List.contains s_rot existing_ss) then s_rot else s
  in
  let fix_origami (o:origami) =
    Origami.origami_map fix_strand fix_gate o |> List.sort in
    (*
    let o_rot = Origami.rotate opts o in
    if (List.contains o_rot existing_os) then o_rot else o in
    *)
  let fix_species (i:species) =
    match i with
    | Species.STRAND s -> Species.STRAND (fix_strand s)
    //| Species.COMPLEX c -> Species.COMPLEX c (* Need to fix relative to rotation notions *)
    | Species.GATE g -> Species.GATE (fix_gate g)
    | Species.ORIGAMI o -> Species.ORIGAMI (fix_origami o)
    | Species.UNKNOWN s -> Species.UNKNOWN s
    | Species.LogicDsdProcess _ -> failwith "unexpected LogicDSD species"
  in
  let fix_species_list (is:species list) = List.map fix_species is in
  match r with
  | Unbind(g,n,is)        -> Unbind((fix_gate g),n,(fix_species_list is))
  | Bind(i1,i2,n,is)      -> Bind((fix_species i1),(fix_species i2),n,(fix_species_list is))
  | Leak(i1,i2,fast,is)   -> Leak((fix_species i1),(fix_species i2),fast,(fix_species_list is))
  | Tau(g,is)             -> Tau((fix_gate g),(fix_species_list is))
  | Migrate(g,ns,g')      -> Migrate((fix_gate g),ns,(fix_gate g'))
  | Displace(g,ns,is)     -> Displace((fix_gate g),ns,(fix_species_list is))
  | Cover(g,ns,g')        -> Cover((fix_gate g),ns,(fix_gate g'))
  | MigrateOpen(g,ns,g')  -> MigrateOpen((fix_gate g),ns,(fix_gate g'))
  | UnbindOpen(g,ns,is)   -> UnbindOpen((fix_gate g), ns,(fix_species_list is))
  | Pin(i,n,is)           -> Pin((fix_species i), n, (fix_species_list is))
  | OrigamiReact(o,r,o')  -> OrigamiReact((fix_origami o),fix opts existing_gs existing_ss existing_os r, (fix_origami o'))
  | OrigamiEntry(o,s,r,o')-> OrigamiEntry((fix_origami o),(fix_species s),fix opts existing_gs existing_ss existing_os r,fix_origami o')
  | OrigamiExpel(o,r,is)  -> OrigamiExpel((fix_origami o),fix opts existing_gs existing_ss existing_os r, (fix_species_list is))
  | OrigamiExchange(o,s,r,is) -> OrigamiExchange((fix_origami o),(fix_species s),fix opts existing_gs existing_ss existing_os r, (fix_species_list is))
  | Remote(ds,r)          -> Remote(ds, fix opts existing_gs existing_ss existing_os r)
  | Nick (i,e,i')         -> Nick (fix_species i, e, fix_species i')
  | Replication (i, e, n1, n2, is) -> Replication (fix_species i, e, n1, n2, fix_species_list is)
  | Chemical r            -> Chemical (map_mass_action_reaction fix_species r)

let fix_reactions (opts:options) (existing_gs:gate list) (existing_ss:strand list) (existing_os:origami list) existing_rs (rs:t list) =
  let fix_fold (fixed_acc, known_gs, known_ss, known_os) r =
    let fixed_r = fix opts known_gs known_ss known_os r in
    let new_gs, new_ss, new_os = species fixed_r |> Species.separate in
    (fixed_r::fixed_acc, new_gs@known_gs, new_ss@known_ss, new_os@known_os) in

  let known = Lib.collect (fun (r: sreaction) -> Mset.elements r.products) existing_rs in
  let known_gs, known_ss, known_os = Species.separate known in

  let reactions,_,_,_ = Lib.fold_left fix_fold ([], existing_gs@known_gs, existing_ss@known_ss, existing_os@known_os) rs in
  List.rev reactions



let translate_rate (z:settings) (opts:options) (mapping:Sequence.mapping) (r:t) =
  match rate z opts mapping r with
  | Crn_ma v -> Rate.MassAction v
  | Crn_func e -> Rate.Function (e)

(* Translate a reaction into a simulator-style reaction. *)
let translate (z:settings) (opts:options) (mapping:Sequence.mapping) (r:t) =
  let sr_rate = translate_rate z opts mapping r in
  Reaction.create(
    [], 
    Mset.from_list (reactants r),
    None,
    sr_rate,
    Mset.from_list (products r)
  )

(* Translate potentially reversible reactions into string-based reactions, for the graph viewer. *)
let translate_reversible (z:settings) (opts:options) (mapping:Sequence.mapping) (rev:(t * t option)) =
  let fwd_sr_rate = translate_rate z opts mapping (fst rev) in
  let bwd_sr_rate_opt = Lib.option_map2 (translate_rate z opts mapping) (snd rev) in
  Reaction.create(
    [],
    Mset.from_list (reactants (fst rev)),
    bwd_sr_rate_opt,
    fwd_sr_rate,
    Mset.from_list (products (fst rev))
  )

(*****)
(* Version of combine with two kinds of reactions *)
let combine_with_existsing (z:settings) (opts:options) (mapping:Sequence.mapping) (rs_existing:sreaction list) (rs_new:t list) =
  let translate = translate z opts mapping in
  let add_one (rs:sreaction list) (r:t) =
    (* Remove any circular reactions (i.e. where reactants and products are the same. *)
    if (is_circular opts r) then rs else
      (* Fast leak reaction *)
      if (is_fast_leak r) then
        if List.exists (fun r' -> (not(looks_like_leak opts r')) && matching_species_rsr opts r r') rs then rs
        else let rs = List.filter (fun r' -> not(looks_like_leak opts r' && matching_species_rsr opts r r')) rs in rs@[translate r]
      (* Slow leak reaction *)
      else if (is_slow_leak r) then
        if List.exists (fun r' -> (not(looks_like_leak opts r')) && matching_species_rsr opts r r') rs then rs
        else let rs = List.filter (fun r' -> not(looks_like_leak opts r' && matching_species_rsr opts r r')) rs in rs@[translate r]
      (* Tau reaction *)
      else if (is_tau r) then
        if List.exists (fun r' -> (looks_like_leak opts r') && matching_species_rsr opts r r') rs then rs
        else let rs = List.filter (fun r' -> not(looks_like_leak opts r' && matching_species_rsr opts r r')) rs in rs@[translate r]
      (* Other reaction *)
      else
        (*call to is_leak is questionable, as it does allow duplicate reactions which may be good but may be too permissive *)
        let rs = List.filter (fun r' -> not(looks_like_leak opts r' && matching_species_rsr opts r r')) rs in rs@[translate r]
  in
  Lib.fold_left add_one rs_existing rs_new


(******************************************************************************)

(* Compute all the new reactions produced by adding an extra strand. *)
let new_strand_reactions sub_map (opts:options) (s:strand) (existing_gs:gate list) (existing_ss:strand list) (existing_os:origami list)=
  let pins = pin_reactions_new_strand sub_map opts s in
  let binds = bind_reactions_new_strand sub_map opts s existing_gs existing_ss in
  let leaks = leak_reactions_new_strand sub_map opts s existing_gs existing_ss in

  let entries = entry_reactions_new_strand sub_map opts s existing_os in
  let reactions = (combine_many opts [leaks; binds; pins; entries]) in

  let reactions_fixed = fix_reactions opts existing_gs (s::existing_ss) existing_os [] reactions in
  reactions_fixed

(* Compute all the new reactions produced by adding an extra gate. *)
let new_gate_reactions sub_map (opts:options) (g:gate) (existing_gs:gate list) (existing_ss:strand list) (existing_os:origami list) existing_rs =
  let unbinds = unbind_reactions sub_map opts g in
  let open_pins = unbind_open_reactions sub_map opts g in
  let binds = bind_reactions_new_gate sub_map opts g existing_gs existing_ss in
  let pins = pin_reactions_new_gate sub_map opts g in
  let leaks = leak_reactions_new_gate sub_map opts g existing_gs existing_ss in
  let taus = tau_reactions sub_map opts g in
  let displacements = displacement_reactions opts g in
  let migrations = migration_reactions opts g in
  let open_migrations = migration_open_reactions opts g in
  let pin_migrations = pin_migration_new_gate sub_map opts g in
  let pin_displacing = pin_displacing_new_gate sub_map opts g in
  let pin_opens = pin_open_new_gate opts g in
  let covers = cover_reactions opts g in
  let enzymes = [] : enzyme list in (* TODO, put in Options? *)
  let nicks = List.collect (nick_reactions g) enzymes in

  let entries = entry_reactions_new_gate sub_map opts g existing_os in
  let reactions = combine_many opts [unbinds; open_pins; binds; pins; leaks; taus; displacements; migrations; open_migrations; pin_migrations; pin_displacing; pin_opens; covers; entries; nicks] in
  
(*  let reactions = List.map (fix opts (g::existing_gs) existing_ss existing_os) reactions in *)
  fix_reactions opts (g::existing_gs) existing_ss existing_os existing_rs reactions

(* Compute all the new reactions produced by adding an extra origami. *)
let new_origami_reactions sub_map (opts:options) (o:origami) (existing_gs:gate list) (existing_ss:strand list) (existing_os:origami list) (existing_rs: sreaction list) =
  let anchored_gates, anchored_strands = Origami.origami_species o in
  (*
  let new_gates,new_strands = origami_possible_new_gates opts existing_rs o anchored_gates,
                              origami_possible_new_strands opts existing_rs o anchored_strands in
  *)
  let reactants = existing_rs |> List.filter (looks_like_origami_reaction o) |> List.collect (fun (r:sreaction) -> Mset.elements r.reactants) in
  let reactant_gates,reactant_strands,_ = Species.separate reactants in
  let new_gates = Lib.difference (Gate.equal opts) anchored_gates reactant_gates in (* RLP: check case of exchange reactions *)
  let new_strands = Lib.difference Strand.equal anchored_strands reactant_strands in (* RLP: check case of exchange reactions *)
  
  let entries = entry_reactions_new_origami sub_map opts o existing_ss existing_gs in
  let unbinds = origami_unbind_reactions sub_map opts o new_gates in
  let unbind_expels = origami_unbind_reactions_expel sub_map opts o new_gates in
  let open_pins = origami_unbind_open_reactions sub_map opts o new_gates in
  let binds = origami_bind_reactions sub_map opts o new_strands new_gates |> Lib.remove_duplicates (matching_species opts) in (* Each bind reaction will appear twice *) (* RLP: find way to avoid this *)
  let pins = origami_pin_reactions sub_map opts o in
(* TODO
  let leaks = () in
  let tau = () in
*)
  let tau = origami_tau_reactions sub_map opts o in
  let displacements = origami_displacement_reactions opts o new_gates in
  let displacement_expels = origami_displacement_reactions_expel opts o new_gates in
  let migrations = origami_migration_reactions opts o new_gates in
  let open_migrations = origami_migration_open_reactions opts o new_gates in
  let pin_migrations = origami_pin_migration sub_map opts o in
  let pin_displacing = origami_pin_displacing sub_map opts o in
  let pin_opens = origami_pin_open opts o in
  let covers = origami_cover_reactions opts o new_gates in
  let reactions = combine_many opts [entries;unbinds;unbind_expels;open_pins;binds;pins;tau;displacements;displacement_expels;migrations;open_migrations;pin_migrations;pin_displacing;pin_opens;covers] in
  let reactions = fix_reactions opts existing_gs existing_ss (o::existing_os) [] reactions in
  reactions


let new_reactions sub_map (opts:options) (i:species) (existing_is:species list) existing_rs =
  let existing_gs,existing_ss,existing_os = Species.separate existing_is in
  let new_reactions =
    match i with
    | Species.GATE g -> new_gate_reactions sub_map opts g existing_gs existing_ss existing_os existing_rs
    //| Species.COMPLEX c -> [] (*new_complex_reactions opts c existing_gs existing_ss existing_os*)
    | Species.STRAND s -> new_strand_reactions sub_map opts s existing_gs existing_ss existing_os
    | Species.ORIGAMI o -> new_origami_reactions sub_map opts o existing_gs existing_ss existing_os existing_rs
    | Species.UNKNOWN _ -> []
    | Species.LogicDsdProcess _ -> failwith "unexpected LogicDSD species"

  combine opts [] new_reactions

let new_species_env opts products species_env =
  Lib.fold_right (Species.add_env opts "%") (List.map (fun s -> (s,Expression.Float 1.0,false,false,None,None)) products) species_env

(* Compute all the new reactions produced by adding an extra species. *)
let add_new_species_reactions sub_map (settings:settings) (opts:options) (mapping:Sequence.mapping) (i:species) (existing_is:species list) (existing_is_env:species_env) (existing_rs:sreaction list) (rxn_rs:sreaction list) =
  let rxn_override (r: sreaction) =
    let is_r (rxn: sreaction) =
      (Mset.is_perm (Species.equal opts) rxn.reactants r.reactants) &&
      (Mset.is_perm (Species.equal opts) rxn.products r.products) in
    match rxn_rs |> List.tryFind is_r with
    | None -> Some r
    | Some rxn ->
      if existing_rs |> List.exists is_r then None // do not add the reaction again
      else Some rxn
  in
  let new_reactions = new_reactions sub_map opts i existing_is existing_rs in
  let added_reactions =
    new_reactions
    |> Lib.rev_map (translate settings opts mapping)
    |> List.choose rxn_override in
  let reactions = added_reactions @ existing_rs in
  let products = Lib.collect products new_reactions in
  (reactions, products, new_species_env opts products existing_is_env)
  