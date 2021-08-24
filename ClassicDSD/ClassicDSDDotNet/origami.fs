// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Origami
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine 

module Prim = Microsoft.Research.CRNEngine.Expression

type options = Options.t
type value_env = Value.t_env
type domain = Domain.t
type strand = Strand.t
type gate = Gate.t

type tag = Domain.tag

type content = C_strand of strand | C_gate of gate
type t = content list

(* Function to Lib.collect over an origami *)
let origami_collect s_proc g_proc cs =
  Lib.collect (fun c ->
                 match c with C_strand s -> s_proc s
                            | C_gate g -> g_proc g) cs

(* Function to Lib.map over an origami *)
let origami_map s_proc g_proc cs =
  List.map (fun c -> 
              match c with C_strand s -> C_strand (s_proc s)
                         | C_gate g ->  C_gate (g_proc g)) cs

(* Function to iterate over an origami *)
let origami_iter s_proc g_proc cs =
  List.iter (fun c ->
               match c with C_strand s -> s_proc s
                          | C_gate g -> g_proc g) cs

(* Render an origami to a string, only rendering anchored strands and gates once, with debugging information *)
let debug_display (cs : list<content>) =
  let as_string =
    cs
    |> Lib.string_of_list 
        (fun c ->
          match c with
          | C_strand s -> "Strand: " + Strand.display(s)
          | C_gate g -> "Gate: " + Gate.display(g))
        "; "
  "[[" + as_string + "]]"

(* Render an origami to a string, only rendering anchored strands and gates once and ignoring shared anchors *)
let display (cs : t) = 
  "[[ " + (Lib.string_of_list Lib.id "\n | " 
         (origami_collect (fun s -> [Strand.display s]) (fun g -> [Gate.display g]) cs)) + " ]]"

let display_option d = function
  | None -> "None"
  | Some x -> d x

(* Collect all free names from the anchored strands and gates on an origami *)
let free_names (cs:t) = origami_collect Strand.free_names Gate.free_names cs

(* Collect all domains from the anchored strands and gates on an origami *)
let domains (cs : t) : domain list = origami_collect Strand.domains Gate.domains cs

(* Evaluate all of the values within the anchored strands and gates of an origami *)
let eval env (cs:t) = origami_map (Strand.eval env) (Gate.eval env) cs
(* Type check all of the anchored strands and gates of an origami *)
let inferType env (cs:t) = origami_iter (Strand.inferType (Some true) env) (Gate.inferType (Some true) env) cs
(* Erase or replace source information in all anchored domains, strands, and gates *)
let erasePosns (cs:t) = origami_map Strand.erasePosns Gate.erasePosns cs
let replacePosns t cs = origami_map (Strand.replacePosns t) (Gate.replacePosns t) cs

(* Does origami cs 'match' the pattern origami csp, i.e. do the contained species match *)
(* NB: Need to include the notion of optional anchored species for this match *)
let matches opts csp cs =
 (List.length csp = List.length cs) &&
  List.forall2 (fun cp c ->
                    match cp,c with
                      (C_strand sp, C_strand s) -> Strand.matches sp s
                    | (C_gate gp, C_gate g) -> Gate.matches opts gp g
                    | _,_ -> false) csp cs

let matches_env (_:options) (_: Domain.match_env) (_:t) (_:t) = (None: Domain.match_env option)

let has_wildcard (o:t) =
  let content_has_wildcard = function
  | C_strand s -> Strand.has_wildcard s
  | C_gate g -> Gate.has_wildcard g in
  List.exists content_has_wildcard o

(* Convert all of the anchored strands and gates into their standard forms, according to the semantics *)
let standard_form opts (cs : t) =
  origami_map Strand.standard_form (Gate.standard_form opts) cs
  |> List.sort

(* Collect a list of all of the neighbouring toeholds on the anchored strands and gates.
   Used for a deprecated(?) correctness check *)
let neighbouring_toeholds (cs : t) : (domain*domain) option list =
  origami_collect (fun s -> [Strand.neighbouring_toeholds s]) (fun g -> [Gate.neighbouring_toeholds g]) cs

(* Collect a list of all exposed nontoehold domains on the anchored strands and gates. *)
let exposed_nontoeholds (cs : t) : domain list = origami_collect Strand.exposed_nontoeholds Gate.exposed_nontoeholds cs

(* An origami is reactive if any of the anchored species are reactive *)
let is_reactive sub_map (opts : options) (cs : t) : bool = 
  List.exists (fun c ->
                 match c with
                   C_strand s -> Strand.is_reactive s
                 | C_gate g -> Gate.is_reactive sub_map opts g) cs

(* rotate the anchored gates and strands of an origami *) 
let rotate (opts : options) (cs : t) : t = origami_map Strand.rotate (Gate.rotate opts) cs

(*Types for lists of anchored components, associated with their domain *)
type gate_assoc = (domain * (gate * domain list)) list
type strand_assoc = (domain * (strand * domain list)) list
type domain_assoc = (domain * int) list

(* Returns the anchored location of a tethered domain; -1 as an illegal location *)
let get_anchor = function Domain.Toe(_,_,_,Some (_,i)) | Domain.Normal(_,_,Some (_,i)) -> i | _ -> -1

(* compare tethers by their anchored location *)
let matching_tether d1 d2 =
  match d1,d2 with
  | Domain.Toe(_,_,_,Some (_,i)), Domain.Toe(_,_,_,Some(_,j))
  | Domain.Normal(_,_,Some (_,i)), Domain.Normal(_,_,Some(_,j)) -> i=j
  | _ -> false

let less_than_tether d1 d2 = (get_anchor d1) < (get_anchor d2)

let matching_tether_options d1 d2 =
  match d1,d2 with Some(d1),Some(d2) -> matching_tether d1 d2 | _ -> false

(* try_find_domain, association find comparing only the tethered position of a domain *)
let rec try_find_domain d = function
    [] -> None
  | (dk,store)::assocs -> 
    if matching_tether d dk then Some(store) else try_find_domain d assocs

let rec lookup_anchor i = function
    [] -> None
  | (dk,store)::assocs -> 
    if (get_anchor dk) = i then Some(store) else lookup_anchor i assocs

(* try_remove_domain, association remove comparing only the tetthered position of a domain *)
let rec try_remove_domain d = function
    [] -> []
  | (dk,store)::assocs ->
    if matching_tether d dk then assocs else (dk,store)::(try_remove_domain d assocs)

let try_remove_domain_option d ds = 
  match d with Some(d) -> try_remove_domain d ds | None -> ds

(* remove all domain associations in ds from the association lists *)
let rec remove_all ds = function
    [] -> []
  | (d,a)::assocs -> if (List.exists (matching_tether d) ds) then remove_all ds assocs else (d,a)::(remove_all ds assocs)

(* Pull apart an origami to look at the individual strands and gates, but retain the domain sharing information *)
let separate_species (o : t) : strand list * gate list = //strand_assoc * gate_assoc * domain_assoc =
  let rec joined_domains acc_s acc_g = function
      [] -> acc_s,acc_g
    | (C_strand s)::ts -> joined_domains (s::acc_s) acc_g ts
    | (C_gate g)::ts -> joined_domains acc_s (g::acc_g) ts
  in
  let ss,gs = joined_domains [] [] o in
  List.rev ss, List.rev gs

(* Give the anchored species of an origami *)
let origami_species o = separate_species o |> (fun (ss, gs) -> gs, ss)

(* Build a new origami based on the anchored domain order of an existing origami and affiliated associations *)
let construct_origami (strands : strand list) (gates : gate list) =
  (strands |> List.map C_strand) @ (gates |> List.map C_gate)

(* Do two origami contain equal anchored domains and equal species, upto gate rotation *)
let equal opts csp cs =
 (List.length csp = List.length cs) &&
  List.forall2 (fun cp c ->
                    match cp,c with
                      C_strand sp, C_strand s -> Strand.equal sp s
                    | C_gate gp, C_gate g -> Gate.equal opts gp g
                    | _ -> false) csp cs

(**************************************************************************************************************************************)
(** Sticking reactions using an origami: both inside the origami and causing new strands and gates to enter                           *)

let localize_bind_rate_from_tags opts m =
  let get_tag_lc (t,_:float) = Options.get_local_concentration opts t in
  match Domain.get_tags m with
  | None -> m
  | Some tags ->
      let lcs = List.map get_tag_lc tags in
      let lc = Lib.fold_left Prim.add (Prim.Float 0.0) lcs in (* RLP: Is max in the paper *)
      Domain.localize_bind_rate m lc

let remove_strand = Lib.remove_first Strand.equal
let remove_gate opts = Lib.remove_first (Gate.equal opts)

(* Function to build the different gate + strand sticking functions, using maps_over *)
let sticking_reactions_strand_gate sub_map (opts : options) (o:t) build_reactants =
  let strands,gates = separate_species o in
  let gate_strand_stick (s:strand, g:gate) = 
    let rs = (Gate.stick_strand_to_gate sub_map (Options.getUnproductive opts) (Strand.annotate_tag_sets s) (Gate.annotate_tag_sets g)) in
    List.map (fun (_,_,dom,gn) -> (g,s,localize_bind_rate_from_tags opts dom,Gate.unannotate_tag_sets gn)) rs in
  let build_origami_reaction (g,s,(dom:domain),(gn:gate)) =
    let strands = remove_strand s strands in
    let gates = remove_gate opts g gates in
    (g,s,dom,gn,construct_origami strands (gn::gates))
  in
  build_reactants strands gates
    |> List.collect gate_strand_stick
    |> List.map build_origami_reaction

(* Function to build the different strand + strand sticking functions, using build_reactants *)
let sticking_reactions_strand_strand sub_map (opts : options) (o:t) build_reactants =
  let strands,gates = separate_species o in
  let strand_strand_stick (s1, s2) = 
     let rs = (Gate.stick_strand_to_strand sub_map (Options.getUnproductive opts) (Strand.annotate_tag_sets s1) (Strand.annotate_tag_sets s2)) in
     List.map (fun (_,_,dom,gn) -> (s1,s2,localize_bind_rate_from_tags opts dom,Gate.unannotate_tag_sets gn)) rs in
  let build_origami_reaction (s1,s2,(dom:domain),(gn:gate)) =
    let strands = strands |> remove_strand s1 |> remove_strand s2 in
    (s1,s2,dom,gn,construct_origami strands (gn::gates))

  build_reactants strands gates
    |> List.collect strand_strand_stick
    |> List.map build_origami_reaction

(* Function to build the different gate + gate sticking functions, using build_reactants *)
let sticking_reactions_gate_gate sub_map (opts : options) (o:t) build_reactants =
  let strands,gates = separate_species o in
  let gate_gate_stick (g1, g2) = 
    let rs = (Gate.stick_gate_to_gate sub_map opts (Gate.annotate_tag_sets g1) (Gate.annotate_tag_sets g2)) in
    List.map (fun (_,_,dom,gn) -> (g1,g2,localize_bind_rate_from_tags opts dom,Gate.unannotate_tag_sets gn)) rs in
  let build_origami_reaction (g1,g2,(dom:domain),(gn:gate)) =
    let gates = gates |> remove_gate opts g1 |> remove_gate opts g2 in
    (g1,g2,dom,gn,construct_origami strands (gn::gates))
  in
  build_reactants strands gates
   |> List.collect gate_gate_stick
   |> List.map build_origami_reaction

(** Sticking reactions between strands and a given gate within an origami *)

(** ... gates and a given strand within an origami *)
let sticking_reactions_sg sub_map (opts:options) (o:t) (s:strand) : (gate * strand * domain * gate * t) list =
  let build_reactants (_:strand list) (gates:gate list) = List.map (fun g -> s, g) gates in
  sticking_reactions_strand_gate sub_map opts o build_reactants

(** ... strands and a given gate within an origami *)
let sticking_reactions_gs sub_map (opts:options) (o:t) (g:gate) : (gate * strand * domain * gate * t) list = 
  let build_reactants (strands:strand list) (_:gate list) = List.map (fun s -> s, g) strands in
  sticking_reactions_strand_gate sub_map opts o build_reactants

(** ... strands and a given strand within an origami *)
let sticking_reactions_ss sub_map (opts:options) (o:t) (s:strand) : (strand * strand * domain * gate * t) list =
  let build_reactants (strands:strand list) (_:gate list) = List.choose (fun st -> if Strand.equal s st then None else Some (s, st)) strands in
  sticking_reactions_strand_strand sub_map opts o build_reactants

(** ... gates and a given gate within an origami *)     
let sticking_reactions_gg sub_map (opts:options) (o:t) (g:gate) : (gate * gate * domain * gate * t) list =
  let build_reactants (_:strand list) (gates:gate list) = List.choose (fun gt -> if Gate.equal opts g gt then None else Some (g, gt)) gates in
  sticking_reactions_gate_gate sub_map opts o build_reactants

(** Sticking reactions that bring a new strand or gate into an origami *)

(* ... a new strand sticking onto gates within an origami *)
let entry_reactions_sg sub_map (opts:options) (o:t) (s:strand) : (gate * strand * domain* gate * t) list =
  let build_reactants (_:strand list) (gates:gate list) = List.map (fun g -> s, g) gates in
  sticking_reactions_strand_gate sub_map opts o build_reactants

(* ... a new gate sticking onto strands within an origami *)
let entry_reactions_gs sub_map (opts:options) (o:t) (g:gate) : (gate * strand * domain * gate * t) list =
  let build_reactants (strands:strand list) (_:gate list) = List.map (fun s -> s, g) strands in
  sticking_reactions_strand_gate sub_map opts o build_reactants

(* ... a new strand sticking onto strands within an origami *)
let entry_reactions_ss sub_map (opts:options) (o:t) (s:strand) : (strand * strand * domain * gate * t) list =
  let build_reactants (strands:strand list) (_:gate list) = List.map (fun st -> s, st) strands in
  sticking_reactions_strand_strand sub_map opts o build_reactants

(* ... a new gate sticking onto gates within an origami *)
let entry_reactions_gg sub_map (opts:options) (o:t) (g:gate) : (gate * gate * domain * gate * t) list =
  let build_reactants (_:strand list) (gates:gate list) = List.map (fun gt -> g, gt) gates in
  sticking_reactions_gate_gate sub_map opts o build_reactants

(**************************************************************************************************************************************)
(** Pinning reactions on gates and strands within an origami                                                                          *)

(*  Pinning reactions on strands inside of an origami *)
let pinning_reactions_s (opts:options) (o:t) : (strand * domain * gate * t) list = 
  let strands,gates = separate_species o in
  let pin s = Gate.pin_strand (Options.getUnproductive opts) s in
  let build_origami_reaction (s,(dom:domain),(gn:gate)) =
    let strands = remove_strand s strands in
    (s,dom,gn,construct_origami strands (gn::gates))
  in 
  let reactions = Lib.collect pin strands in
  List.map build_origami_reaction reactions

(* Pinning reactions on gate overhangs inside of an origami *)
let pinning_reactions_g (opts:options) (o:t) = 
  let strands,gates = separate_species o in
  let pin g = Gate.pin_gate (Options.getUnproductive opts) g in
  let build_origami_reaction (g,(dom:domain),(gn:gate)) =
    let gates = remove_gate opts g gates in
    (g,dom,gn,construct_origami strands (gn::gates))
  in 
  let reactions = Lib.collect pin gates in
  List.map build_origami_reaction reactions

(** Compute all possible leak pin migration reactions on a new gate in an origami. *)
let close_migration_reactions (opts:options) (o:t) = 
  let strands,gates = separate_species o in
  let pin g = Gate.close_migration_reactions opts g in
  let build_origami_reaction (hp:domain list, g,(doms:domain list),(gn:gate)) =
    let gates = remove_gate opts g gates in
    (hp, g,doms,gn,construct_origami strands (gn::gates))
  in 
  let reactions = Lib.collect pin gates in
  List.map build_origami_reaction reactions

(** Compute all possible leak pin displacing reactions on a new gate in an origami. *)
let is_strand_free = Strand.tethered_domains >> List.isEmpty
let is_gate_free = Gate.tethered_domains >> List.isEmpty

let close_displacing_reactions (opts:options) (o:t) = 
  let strands,gates = separate_species o in
  let pin g = Gate.close_displacing_reactions opts g in
  let build_origami_reaction (hp:domain list, g,(doms:domain list),(gs:gate list),(ss:strand list)) =
    let gates = remove_gate opts g gates in
    let ss_out, ss_in = ss |> List.partition is_strand_free in
    let gs_out, gs_in = gs |> List.partition is_gate_free in
    (hp, g,doms,gs_out,ss_out,construct_origami (ss_in@strands) (gs_in@gates))
  in 
  let reactions = Lib.collect pin gates in
  List.map build_origami_reaction reactions

(** Compute all possible leak pin open reactions on a new gate in an origami. *)
let close_open_reactions (opts:options) (o:t) = 
  let strands,gates = separate_species o in
  let pin g = Gate.close_open_reactions opts g in
  let build_origami_reaction (hp:domain list, g,(doms:domain list),(gn:gate)) =
    let gates = remove_gate opts g gates in
    (hp, g,doms,gn,construct_origami strands (gn::gates))
  in 
  let reactions = Lib.collect pin gates in
  List.map build_origami_reaction reactions

(**************************************************************************************************************************************)
(** Unsticking reactions on a gate within an origami, some of which expell a gate or a strand if no constituent domains are anchored. *)

let is_reaction_expelling (_, _, gs, ss) =
  List.exists is_gate_free gs || List.exists is_strand_free ss

(*Function for generating unsticking or displacement reactions, that don't expell *)
let separating_reactions (opts:options) (o:t) (reactions: (gate * 'dom * gate list * strand list) list) =
  let build_origami_reaction (g, d:'dom, gs, ss) =
    let strands,gates = separate_species o in
    let gates = remove_gate opts g gates in
    (g, d, gs, ss, construct_origami (ss@strands) (gs@gates)) in
  reactions |> List.filter (is_reaction_expelling >> not) |> List.map build_origami_reaction

(*Function for generating unsticking or displacement reactions, that do expell *)
let separating_reactions_expel (opts:options) (o:t) (reactions: (gate * 'dom * gate list * strand list) list) =
  let build_origami_reaction (g, d:'dom, gs, ss) =
    let strands,gates = separate_species o in
    let gates = remove_gate opts g gates in
    let ss_out, ss_in = List.partition is_strand_free ss in
    let gs_out, gs_in = List.partition is_gate_free gs in
    (g, d, gs, ss, construct_origami (ss_in@strands) (gs_in@gates), gs_out, ss_out) in
  reactions |> List.filter is_reaction_expelling |> List.map build_origami_reaction

(** Return all unsticking reactions possible on a given gate in an origami, without expulsion *)
let unsticking_reactions sub_map (opts:options) (g:gate) (o:t) : (gate * domain * gate list * strand list * t) list =
  separating_reactions opts o (Gate.unsticking_reactions sub_map opts g)

(** Return all unsticking reactions possible on gates in an origami, with expulsion *)
let unsticking_reactions_expel sub_map (opts:options) (g:gate) (o:t): (gate * domain * gate list * strand list * t * gate list * strand list) list = 
  separating_reactions_expel opts o (Gate.unsticking_reactions sub_map opts g)

(** Return all (single) displacement reactions possible from anchored gates in an origami, without expulsion *)
let displacement_reactions (opts:options) (g:gate) (o:t): (gate * domain list * gate list * strand list * t) list = 
  separating_reactions opts o (Gate.displacement_reactions opts g)

(** Return all (single) displacement reactions possible from anchored gates in an origami with expulsion **)
let displacement_reactions_expel(opts:options) (g:gate) (o:t): (gate * domain list * gate list * strand list * t * gate list * strand list) list = 
  separating_reactions_expel opts o (Gate.displacement_reactions opts g)

(** Return all reactions that open a hairpin after an unbind on gates in an origami *)
let unbind_open_reactions(opts:options) (g:gate) (o:t): (gate * domain * gate list * strand list * t) list = 
  separating_reactions opts o (Gate.unbind_open_reactions opts g)

(*************************************************************************************************************************************)
(* Reactions within an origami on one gate                                                                                           *)

(* Function to generate interior reactions on one gate in an origami *)
let interior_reactions (opts: options) (o:t) (reactions: (gate * 'dom * gate) list) =
   let strands,gates = separate_species o in
   let build_origami_reaction ((g:gate),dom:'dom,gt2) = 
      let gates = gt2::(remove_gate opts g gates) in
      (g,dom,gt2,construct_origami strands gates)      
   in
   List.map build_origami_reaction reactions 

(** Return all migration reactions possible on anchored gates in an origami. *)
let migration_reactions (opts: options) (g:gate) (o:t) : (gate * domain list * gate * t) list =
  interior_reactions opts o (Gate.migration_reactions g)

(** Return all migration reactions that open a hairpin on gates in an origami. *)
let open_migration_reactions (opts:options) (g:gate) (o:t): (gate * domain list * gate * t) list = 
  interior_reactions opts o (Gate.open_migration_reactions opts g)

(** Return all cover reactions possible on an anchored gate in an origami. *)
let cover_reactions (opts:options) (g:gate) (o:t) : (gate * domain * gate * t) list = 
  interior_reactions opts o (Gate.cover_reactions g)

(***********************************************************************************************************************)
(* Functions to gather reactions                                                                                       *)

(** Get all fast reactions on anchored gates in an origami (rate may be tau or infinity depending on semantics), may expel. *)
let fast_reactions sub_map (opts:options) (o:t): (gate list * strand list * t) list =
  (* Consider the results equivalent up to reordering of the strand list. *)
  let tau_match (gs1,ss1,os1,o1) (gs2,ss2,os2,o2) = 
      (Lib.is_permutation (Gate.equal opts) gs1 gs2) && (Lib.is_permutation Strand.equal ss1 ss2) && 
      (Lib.is_permutation (equal opts) (Lib.union (equal opts) [o1] os1) (Lib.union (equal opts) [o2] os2)) in
  (* All single tau reactions possible from a gate. The strand list is to accumulate displaced strands. *)
  let next_steps (o:t) (os:t list) =
    let _,gates = separate_species o in
    let displacements = List.map (fun (_,_,_,_,o') -> ([],[],os,o')) 
                                 (Lib.collect (fun g -> (displacement_reactions opts g o)) gates) in
    let displacement_expels =
      List.map (fun (_,_,_,_,o',gs,ss) -> ((List.map(Gate.standard_form opts) gs),ss,os,o')) 
               (Lib.collect (fun g -> (displacement_reactions_expel opts g o)) gates) in
    let opens =
      List.map (fun (_,_,_,o') -> [],[],os,o') 
               (Lib.collect (fun g -> open_migration_reactions opts g o) gates) in
    let unsticks =
      match (Options.getRules opts) with (* Don't include unsticking reactions as fast in the "Original" merged semantics. *)
      | Options.Infinite | Options.Finite ->
          List.map (fun (_,_,_,_,o') -> ([],[],os,o')) 
                   (Lib.collect (fun g -> (unsticking_reactions sub_map opts g o)) gates)
      | Options.Default -> []
      | Options.Detailed -> failwith "Origami.fast_reactions shouldn't be called in single step mode"
    let unstick_expels =
      match (Options.getRules opts) with (* Don't include unsticking reactions as fast in the "Original" merged semantics. *)
      | Options.Infinite | Options.Finite ->
          List.map (fun (_,_,_,_,o',gs,ss) -> (List.map(Gate.standard_form opts) gs,ss,os,o')) 
                   (Lib.collect (fun g -> (unsticking_reactions_expel sub_map opts g o)) gates)
      | Options.Default -> []
      | Options.Detailed -> failwith "Origami.fast_reactions shouldn't be called in single step mode"
    let unstick_opens = 
      match (Options.getRules opts) with (* Similarly don't open in all semantics *)
      | Options.Infinite | Options.Finite ->
         List.map (fun (_,_,_,_,o') -> ([],[],os,o')) 
                  (Lib.collect (fun g -> (unbind_open_reactions opts g o)) gates)
      | Options.Default -> []
      | Options.Detailed -> failwith "Origami.fast_reactions shouldn't be called in single step"
    let covers =
      List.map
        (fun (_,_,_,o') -> ([],[],os,o'))
        (Lib.collect (fun g->(cover_reactions opts g o)) gates) in
                     
    Lib.union tau_match displacements
                        (Lib.union tau_match displacement_expels 
                          (Lib.union tau_match opens 
                             (Lib.union tau_match unsticks 
                                (Lib.union tau_match unstick_expels
                                   (Lib.union tau_match unstick_opens covers)))))
  in
  (* Search the tau reaction space to compute transitive closure of tau reactions. *)
  let rec search ((gs:gate list),(ss:strand list),(os:t list),o) =
    let ost = standard_form opts o in
    match (next_steps ost (o::os)) with
      | [] -> [gs,ss,os,o]
      | xs -> xs
              |> List.map (fun (gs',ss',os',o') -> (gs'@gs, ss'@ss,os',o'))
              |> Lib.collect_union tau_match search
  in
  let answer = search ([],[],[],o) in
  List.map (fun (gs,ss,_,o') -> (gs,ss,o')) answer 

(** Get all "initial" reactions on anchored gates in an origami. *)
let initial_reactions sub_map (opts:options) (o:t) : (gate list * strand list * t) list =
  match Options.getRules opts with
  | Options.Infinite | Options.Default -> fast_reactions sub_map opts o
  | _ -> []


let universal_counters (cs:t) = origami_map Strand.universal_counters Gate.universal_counters cs
