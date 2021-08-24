// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Species
open Microsoft.Research.DNA
open System.Diagnostics

type Population<'a,'b> = Microsoft.Research.CRNEngine.Population<'a,'b>
type Populations<'a,'b> when 'a:equality = Microsoft.Research.CRNEngine.Populations<'a,'b>
module Stringmap = Microsoft.Research.CRNEngine.Stringmap
module Lib = Microsoft.Research.CRNEngine.Lib
module BMEValue = Microsoft.Research.CRNEngine.Expression
type prim = Microsoft.Research.CRNEngine.Expression.t<string>

type options = Options.t
type key = string
type domain = Domain.t
type strand = Strand.t
//type branch = Branch.t
type origami = Origami.t

[<DebuggerDisplay("{DisplayMe()}")>] // displays meaningful information in debug mode
type t = STRAND of strand 
       | GATE of Gate.t 
       (*COMPLEX of branch |*) 
       | ORIGAMI of origami 
       | UNKNOWN of string  
       | LogicDsdProcess of RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>
         member this.DisplayMe()  = match this with 
                                    | STRAND s -> Strand.display s 
                                    | GATE g -> Gate.display g
                                    //| COMPLEX b -> Branch.display b
                                    | ORIGAMI ts -> Origami.display ts
                                    | UNKNOWN s -> s
                                    | LogicDsdProcess p -> RulesDSD.Syntax.Process.ToStringWith Microsoft.Research.DNA.LogicDSD.engine p

type sim_t = Population<t, prim>

type t_map = (t * prim * bool * bool * prim option * Microsoft.Research.CRNEngine.Spatial_initial.t option)
type t_disp = Names of string Stringmap.t * int
type t_env = (t_map list Stringmap.t * t_disp) 

(* Decide whether a given species is a gate, a strand, or an origami *)
let is_strand = function STRAND _ -> true | _ -> false
let is_gate = function GATE _ -> true | _ -> false (*To be deprecated for below*)
//let is_complex = function COMPLEX _ -> true | _ -> false
let is_origami = function ORIGAMI _ -> true | _ -> false

(* Produce a string representation of a species. *)
let display = function 
    STRAND s -> Strand.display s 
  | GATE g -> Gate.display g
  //| COMPLEX b -> Branch.display b
  | ORIGAMI ts -> Origami.display ts
  | UNKNOWN s -> s
  | LogicDsdProcess p -> sprintf "[%s]" (RulesDSD.Syntax.Process.ToStringWith Microsoft.Research.DNA.LogicDSD.engine p)


(* Produce the name associated with a species, if allowed by opts and check_opts, otherwise return display string *)
let display_name (opts:options) ((_,Names(env,_)):t_env) (check_opts:bool) (sp:t) =
  let disp = display sp in
  let name = if Stringmap.inDomain env disp then Stringmap.find disp env else disp in
  if (check_opts && Options.getPlotNames opts || not check_opts) then name else disp   

let try_find_name (_:options) ((_,Names(env,_)):t_env) (_:bool) (sp:t) =
  let disp = display sp in
  if Stringmap.inDomain env disp then Some (Stringmap.find disp env) else None

(*
let display_name (opts:options) ((_,Names(env,_)):t_env) (sp:t) =
  let disp = display sp in
  Stringmap.tryFind disp env

let display_ad_opts (opts:options) ((_,Names(env,_)):t_env) (sp:t) =
  let disp = display sp in
  if Options.getPlotNames opts then match Stringmap.tryFind disp env with
    | Some n -> n
    | none -> disp
  else disp
  *)

(* Convert a species into its standard form, according to the semantics. *)
let rec standard_form (opts:options) (i:t) =
  match i with
  | STRAND s -> STRAND (Strand.standard_form s)
  | GATE g -> GATE (Gate.standard_form opts g)
//  | COMPLEX b -> COMPLEX (Branch.standard_form opts b)
  | ORIGAMI sps -> ORIGAMI (Origami.standard_form opts sps)
  | UNKNOWN s -> UNKNOWN s
  | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD"

let rotate opts = function
  | STRAND s -> STRAND (Strand.rotate s)
  | GATE g -> GATE (Gate.rotate opts g)
//| COMPLEX b -> COMPLEX (Branch.rotate opts b)
  | ORIGAMI sps -> ORIGAMI (Origami.rotate opts sps)
  | UNKNOWN s -> UNKNOWN s
  | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD"

(* Does species i' match the "pattern" i? *)
let rec matches (opts:options) (i:t) (i':t) = 
  //let (debugi, debugii) = (display i),(display i') in
  match (standard_form opts i),(standard_form opts i') with
  | STRAND s, STRAND s2     -> Strand.matches s s2
  | GATE g, GATE g2         -> Gate.matches opts g g2
//  | COMPLEX b, COMPLEX b2   -> Branch.matches opts b b2
  | ORIGAMI ts, ORIGAMI ts2 -> Origami.matches opts ts ts2
  | UNKNOWN a, UNKNOWN a2   -> a = a2
  | _ -> false

(* Is there an extension to env such that species i' matches the "pattern" i? *)
let rec matches_env (opts:options) env (i:t) (i':t) = 
  //let (debugi, debugii) = (display i),(display i') in
  match (standard_form opts i),(standard_form opts i') with
  | STRAND s, STRAND s2     -> Strand.matches_env env s s2
  | GATE g, GATE g2         -> Gate.matches_env opts env g g2
//  | COMPLEX b, COMPLEX b2   -> Branch.matches_env opts env b b2
  | ORIGAMI ts, ORIGAMI ts2 -> Origami.matches_env opts env ts ts2
  | UNKNOWN a, UNKNOWN a2   -> if a = a2 then Some env else None
  | _ -> None

let is_predicate = function
  | STRAND s  -> Strand.has_wildcard s
  | GATE g    -> Gate.has_wildcard g
//  | COMPLEX b -> Branch.has_wildcard b
  | ORIGAMI o -> Origami.has_wildcard o
  | UNKNOWN _ -> false
  | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD"

(* Are two species equal, up to physical rotation of a complex *)
let rec equal (opts:options) (i:t) (i':t) =
  match i,i' with
  | STRAND s, STRAND s2 -> Strand.equal s s2
  | GATE g, GATE g2 -> Gate.equal opts g g2
//  | COMPLEX b, COMPLEX b2 -> Branch.equal opts b b2
  | ORIGAMI ts, ORIGAMI ts2 -> Origami.equal opts ts ts2
  | UNKNOWN s, UNKNOWN s2 -> s=s2
  | _ -> false

(* Add a species (potentially combining population counts) to a species map list *)
let rec add opts sps_lst (s,pop,c,plt,t,sp) =
  match sps_lst with
  | [] -> [s,pop,c,plt,t,sp]
  | (s_n,pop_n,c_n,plt_n,t_n,sp_n)::sps -> if equal opts s s_n && t = t_n && sp = sp_n
                                            then (s_n,BMEValue.add pop_n pop |> BMEValue.simplify,c||c_n,plt_n||plt,t_n,sp_n)::sps 
                                            else (s_n,pop_n,c_n,plt_n,t_n,sp_n)::(add opts sps (s,pop,c,plt,t,sp))

(* an empty species environment, with given species count *)
let empty_env c = (Stringmap.empty : t_map list Stringmap.t), Names(Stringmap.empty, c)

(* If n is in name to species environment, remove it and return the mapped species and the new env*)
let env_mem_name n (spec_env,name_env) =
  match Stringmap.tryFind n spec_env with
  | None -> None
  | Some(sps) -> Some(sps, (Stringmap.remove n spec_env,name_env))
(* Is s in species to name environment? *)
let env_mem_species s (_,Names(name_env,_)) = Option.isSome (Stringmap.tryFind (display s) name_env)

(* Combine species environments, where env1 is a "newer" environment than env2 *)
let union_env opts ((s_env1,n_env1:t_disp):t_env (*as env1*)) ((*(s_env2,n_env2) as*) env2) =
  let rec st_count_lst = function
      [] -> []
    | (s,_:prim,_:bool,_:bool,_,_(*spatial initial option*))::lst -> (s,display s,List.length lst)::(st_count_lst lst) in
  let rec merge (e1_lst,n_env1:t_disp) ((s_e2,Names(n_env2,c2)) as e2) =
    match e1_lst with
        | [] -> e2
        | (n,sps)::e1_lst -> 
          let s_e2_n,sps_c = (match env_mem_name n e2 with
                               | None -> (Stringmap.add n sps s_e2),0 
                               | Some(sps2,_) -> (Stringmap.add n (Lib.fold_left (add opts) sps sps2) s_e2),(List.length sps2)) in
          let n_env2_n,cn = Lib.fold_right 
                              (fun (sp,s,sc) (n_env_n,c_n) -> if env_mem_species sp (s_e2,Names(n_env_n,c_n)) then n_env_n,c_n
                                                              else if n = "%" then (Stringmap.add s ("sp_" + string c_n)n_env_n,c_n+1)
                                                              else if sc=0 && sps_c = 0 then (Stringmap.add s n n_env_n,c_n)
                                                              else (Stringmap.add s (n + "_" + string (sc + sps_c)) n_env_n,c_n))
                              (st_count_lst sps) (n_env2,c2) in
          merge (e1_lst,n_env1) (s_e2_n,Names(n_env2_n,cn))                  
  in 
  merge ((List.map (fun n -> (n,Stringmap.find n s_env1)) (Stringmap.getKeys s_env1)),n_env1) env2

(* Add a sim_species, name and plot flag to the species environments *)
let add_env (opts : options) (n : string) ((s,_,_,_,_,_) as sp: t_map) ((species_env,Names(name_env,c)) as env :t_env)=
  let name,c_n = if n="%" then ("sp_" + string c),c+1 else n,c in
  match env_mem_name n env, env_mem_species s env with
    None,false -> Stringmap.add n [sp] species_env, Names(Stringmap.add (display s) name name_env,c_n)
  | None,true -> Stringmap.add n [sp] species_env,Names(name_env,c)
  | Some(sps2,(senv,Names(nenv,c))), false -> let z = add opts sps2 
                                              Stringmap.add n (z sp) senv, 
                                              (*NB Consider what to do if n_i is already in table, escalating _?*)
                                              if n="%" then Names(Stringmap.add (display s) name name_env,c_n)
                                              else Names(Stringmap.add (display s) (name + "_" + string (List.length sps2)) nenv,c)
  | Some(sps2,(species_env,name_env)),true -> Stringmap.add n (add opts sps2 sp) species_env, name_env

(* map f over the sim species in the environment *)
let env_map (f: t_map -> t_map) ((s_env,n_env) : t_env) = Stringmap.map (fun _ sps -> List.map f sps) s_env, n_env

(* extract all sim_species from an env *)
let env_to_species ((s_env,_) : t_env) = 
  let rec add (si :sim_t, t : prim option) (ses: (sim_t * prim option) list) = 
    match ses with
    | [] -> [si, t]
    | (si2,t2)::species -> 
      if si.species=si2.species && si.constant=si2.constant && t=t2 then
        ( { Population.species = si2.species
          ; Population.value = BMEValue.add si.value si2.value
          ; Population.constant = si.constant
          ; Population.input = si.input
          ; Population.initial = si.initial 
          ; Population.spatial_initial = si.spatial_initial
          ; Population.max = si.max }
        , t)::species
      else (si2,t2)::(add (si,t) species)

  let m = (Stringmap.fold (fun l _ sps -> sps@l) [] s_env)
  let m' =
    m |> List.map
        (fun (s,p,c,_,t,sp) ->
            { Population.species = s
              Population.value = p
              Population.constant = c
              Population.input = true
              Population.initial = true
              Population.spatial_initial = sp
              Population.max = None }, t)
  Lib.fold_left (fun l si -> add si l) [] m'

(*extract all plotted species from an env*)
let env_to_plot_species ((s_env,_) : t_env) =
  List.map (fun (s,_,_,_,_,_) -> s) (List.filter (fun (_,_,_,b,_,_) -> b) (Stringmap.fold (fun l _ sps -> sps@l) [] s_env))

(* Look for any neighbouring exposed toeholds in a species. *)
let rec neighbouring_toeholds i =
  match i with
  | STRAND s -> [Strand.neighbouring_toeholds s]
  | GATE g -> [Gate.neighbouring_toeholds g]
 // | COMPLEX b -> [Branch.neighbouring_toeholds b]
  | ORIGAMI sps -> Origami.neighbouring_toeholds sps
  | UNKNOWN _ -> []
  | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD"

(* Compute the list of exposed non-toeholds in a species. *)
let rec exposed_nontoeholds (i:t) =
  match i with
  | STRAND s -> Strand.exposed_nontoeholds s
  | GATE g -> Gate.exposed_nontoeholds g
//  | COMPLEX b -> Branch.exposed_nontoeholds b
  | ORIGAMI sps -> Origami.exposed_nontoeholds sps
  | UNKNOWN _ -> []
  | LogicDsdProcess _ -> []

(* Try to find a non-toehold domain "a" where both "a" and "a*" appear exposed.
 * Raise an error if such a domain exists. *)
let check_interacting_nontoeholds (is:t list) : unit =
  (* Get the list of exposed non-toeholds in the gates and strands. *)
  let exposed = Lib.collect exposed_nontoeholds is in
  (* Loop through the list and return the first element (if any) that also appears complemented. *)
  let rec loop = function
    | [] -> None
    | (d::ds) -> if (Lib.contains (Domain.complement d) ds) then Some d else loop ds
  in
  match loop exposed with
    | None -> ()
    | Some d -> Errors.interacting_nontoehold_error (Domain.display (Domain.unstar d)) (Domain.display (Domain.star d))

(* Check for any neighbouring exposed toeholds, throwing an exception if any are found. *)
(*
let check_neighbouring_toeholds (is:t list) : unit =
  match Lib.lookForSome (fun i -> match neighbouring_toeholds i with None -> None | Some(t1,t2) -> Some(i,t1,t2)) is with
    | None -> ()
    | Some (i,t1,t2) -> Errors.neighbouring_toeholds_error (display i) (Domain.display t1) (Domain.display t2)
*)

(* Check a list of species to see that they meet certain syntactic criteria. *)
let check (is:t list) : unit = begin check_interacting_nontoeholds is (*; check_neighbouring_toeholds is*) end

(* Collect all the domains in a species. *)
let rec domains (i:t) =
  match i with
  | STRAND s -> Strand.domains s
  | GATE g -> Gate.domains g
//  | COMPLEX b -> Branch.domains b
  | ORIGAMI sps -> Origami.domains sps
  | UNKNOWN _ -> []
  | LogicDsdProcess p -> 
      p 
      |> RulesDSD.Syntax.Process.ToList
      |> List.collect (List.map (fun x -> 
          match x with 
          | Microsoft.Research.DNA.LogicDSD.SiteT.Site (Microsoft.Research.DNA.LogicDSD.Bound (Microsoft.Research.DNA.LogicDSD.Dom d, _))
          | Microsoft.Research.DNA.LogicDSD.SiteT.Site (Microsoft.Research.DNA.LogicDSD.Unbound (Microsoft.Research.DNA.LogicDSD.Dom d)) -> 
              let dom = Value.Domain(d.name, 0, Microsoft.Research.CRNEngine.Expression.Float 0.0, Microsoft.Research.CRNEngine.Expression.Float 0.0, None)
              if d.isToehold 
                then Domain.t.Toe(dom, Value.Float(1.0, None), d.isComplementary, None)
                else Domain.t.Normal(dom, d.isComplementary, None)
          | _ -> failwith "Unexpected uninstantiated Logic DSD process.")) 

(** Get all "initial" unary reactions on a species. *)
let initial_reactions sub_map (opts:options) (i:t) : t list list =
  match i with
  | STRAND _ -> []
  | GATE g -> List.map (fun (gs,ss) -> (List.map GATE gs)@(List.map STRAND ss)) (Gate.initial_reactions sub_map opts g)
//  | COMPLEX b -> List.map (fun (bs,ss) -> (List.map (fun b->COMPLEX b) bs)@(List.map (fun s -> STRAND s) ss)) (Branch.initial_reactions sub_map opts b)
  | ORIGAMI _ -> [] (* Determine if there should be some *)
  | UNKNOWN _ -> []
  | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD"

(** Collate a list of branches, a list of gates (which should be folded into branches), a list of strands, and a list of origami into a single list of species. *)
let collate (*cs:branch list*) (gs:Gate.t list) (ss:strand list) (os: origami list): t list = 
   (List.map GATE gs)@(List.map STRAND ss)@(List.map ORIGAMI os)

(** Separate a single list of species into a list of branches, a list of gates (which should be folded into branches), a list of strands, and a list of origami. *)
let separate (is:t list) : (*branch list *) Gate.t list * strand list * origami list =
  Lib.fold_left 
    (fun (gs,ss,os) i ->
      match i with 
      | GATE g    -> (gs@[g],ss,os) 
    //| COMPLEX b -> (cs@[b],gs,ss,os) 
      | STRAND s  -> (gs,ss@[s],os) 
      | ORIGAMI o -> (gs,ss,o::os) 
      | UNKNOWN _ -> (gs,ss,os)
      | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD") 
    ([],[],[]) is

(** Separate an association list keyed by species into four association lists. *)
let separate_assocs (ixs:(t * 'a) list) : (*branch * 'a) list *) (Gate.t * 'a) list * (strand * 'a) list * (origami * 'a) list =
  Lib.fold_left 
    (fun (gxs,sxs,oxs) (i,x) -> 
       match i with
       | GATE g    -> (gxs@[(g,x)], sxs,oxs)
       //| COMPLEX b -> (cxs@[b,x],gxs,sxs,oxs)  
       | STRAND s  -> (gxs,sxs@[(s,x)],oxs) 
       | ORIGAMI o -> (gxs,sxs,(o,x)::oxs) 
       | UNKNOWN _ -> (gxs,sxs,oxs)
       | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD")
    ([],[],[]) ixs

(* Is a species "reactive"? A syntactic approximation to reactivity as defined by the reaction rules... *)
let rec is_reactive sub_map (opts:options) (i:t) =
  match i with
  | STRAND s -> Strand.is_reactive s
  | GATE g -> Gate.is_reactive sub_map opts g
  //| COMPLEX b -> Branch.is_reactive sub_map opts b
  | ORIGAMI sps -> Origami.is_reactive sub_map opts sps
  | UNKNOWN _ -> false
  | LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD"

(**************************************************************************************************************)

(* Types for raw species, as produced by the parser. *)
type raw = t * ((bool * t) list) * Types.range
//type raw_branch = RawBranch of raw * Branch.connection option * (raw_branch * Branch.connection option) list

(* Convert a raw species into a proper one, checking for well-formedness along the way. *)
let convert_raw_gate ((seg,segs,r):raw) : t =
  let f acc (isUpper,seg) =
    match acc,isUpper,seg with
    | (GATE g),isUpper,(GATE g') -> GATE(Gate.join g isUpper g')
    | (GATE g),false,(STRAND(Strand.Lower s')) -> GATE(Gate.append_strand_to_gate g (Strand.Lower s'))
    | (GATE g),true,(STRAND(Strand.Upper s')) -> GATE(Gate.append_strand_to_gate g (Strand.Upper s'))
    | (STRAND(Strand.Lower s)),false,(GATE g') -> GATE(Gate.cons_strand_to_gate (Strand.Lower s) g')
    | (STRAND(Strand.Upper s)),true,(GATE g') -> GATE(Gate.cons_strand_to_gate (Strand.Upper s) g')
    | (STRAND(Strand.Lower s)),false,(STRAND(Strand.Lower s')) -> STRAND(Strand.append (Strand.Lower s) (Strand.Lower s'))
    | (STRAND(Strand.Upper s)),true,(STRAND(Strand.Upper s')) -> STRAND(Strand.append (Strand.Upper s) (Strand.Upper s'))
    | _,_,_ -> Errors.syntax_error (Types.rangeBegin r) (Types.rangeEnd r) "Malformed gate"

  Lib.fold_left f seg segs
(*
let convert_raw_branch raw_b = 
  let convert_sub (top:bool) raw_sp r = 
     let sp = convert_raw_gate raw_sp in
       (match sp with 
         | STRAND s -> if top then STRAND s else Errors.syntax_error (Types.rangeBegin r) (Types.rangeEnd r) "Malformed strand"
         | GATE g   -> if top then GATE g   else COMPLEX(Branch.Solo(g)) 
         | _        -> Errors.syntax_error (Types.rangeBegin r) (Types.rangeEnd r) "Malformed branch")
  in     
  let rec convert_internal (top:bool) = function
  | RawBranch(((_,_,r) as raw_s),None,[]) -> convert_sub top raw_s r
  | RawBranch(((_,_,r) as raw_s),Some(i,u,l),raw_cogs) -> 
    let connect i = function
      | Branch.ConnectsTo _ -> Branch.ConnectsTo i
      | Branch.BreaksTo _   -> Branch.BreaksTo i in
    let rec first_pass i = function 
      | [] -> []
      | (COMPLEX(b),Some(_,u,l))::cogs -> (b,(i,u,l))::(first_pass (1+i) cogs)
      | c:(t*(Branch.connection option))::cs -> failwith "convert_internal failed to eliminate strands from a branch"
    in
    let rec second_pass upper f_lower = function
      | [] -> []
      | [(b : branch,(j,u:Branch.joined,l))] -> [(b,(j,upper,f_lower))]
      | (b,(j,u,l))::cogs -> (b,(j,upper,(connect (j+1) l)))::(second_pass (connect j l) f_lower cogs)
    in
    let cog = convert_sub false raw_s r in
    let complex = match cog with COMPLEX(Branch.Solo(c)) -> c | c -> failwith "convert_internal failed" in
    let cogs = List.map (fun (rb,c) -> convert_internal false rb,c) raw_cogs in
    let identified_cogs = first_pass 1 cogs in
    let connected_cogs  = second_pass (connect i u) (connect i l) identified_cogs in
    (*TODO: need to orient based on least cog(s) *)
    COMPLEX(Branch.Branch((complex,(i,u,l)),connected_cogs,1))
  | RawBranch((_,_,r),_,_) -> Errors.syntax_error (Types.rangeBegin r) (Types.rangeEnd r) "Malformed branch"
  in
  convert_internal true raw_b
*)
let compare (l : t) (r : t) : int =
    match l,r with
    | STRAND(sl),STRAND(sr)                  -> Strand.compare sl sr
    | GATE(gl),GATE(gr)                      -> Gate.compare gl gr
 //   | COMPLEX(bl),COMPLEX(br)                -> Branch.compare bl br
    | STRAND _ , GATE _ (*| STRAND _,COMPLEX _*) -> -1
    | GATE _ , STRAND _ (*|COMPLEX _, STRAND _*) -> 1
    (*
    | GATE _, COMPLEX _                      -> -1
    | COMPLEX _ , GATE _                     -> 1
    *)
    | ORIGAMI _, _ | _, ORIGAMI _ | UNKNOWN _, _ | _, UNKNOWN _ -> 0 
    | LogicDsdProcess _, _
    | _, LogicDsdProcess _ -> failwith "Logic DSD complexes are not supported in Classic DSD"

(* Convert a list of species into an origami, for the parser *)
let convert_raw_origami (spe : t list) =
  let gates,strands,_ = separate spe in
  (gates |> List.map Origami.C_gate) @ (strands |> List.map Origami.C_strand)

(* Helpers for melting

Test with
  {x*}<x^*z1>[z^*x]<y*>{z1^x1}::{y^z1}<x^*y1>[y]<z*>{x*}

  directive polymers
  <y}[y^* y]<z^* x^*>{z}::{y}<y>[x]<y>{x}::{x^}<z^>[z^]<y>{x}:{z^*}<x^>[z^]<z^>{y^}
*)
type gate_strands =
  | Two of domain list * domain list (* upper, lower *)
  | One of domain list               (* as lower *)

let melt_segment = function
  | Segment.Double (lb, lt, sh, rt, rb) ->
    let sht, shb = Domain.unstick_list sh in
    let top = List.concat [lt; sht; rt] in //reconstruct top strand
    let bot = List.concat [lb; shb; rb] in //reconstruct bottom strand
    Two (top, bot)
  | Segment.Hairpin (rb,rt,sh,hp,Segment.Left) ->
    let sht, shb = Domain.unstick_list sh in
    let bot = List.concat [List.rev rt; List.rev sht; hp; shb; rb] in //reconstruct bottom strand
    One bot
  | Segment.Hairpin (lb,lt,sh,hp,Segment.Right) ->
    let sht, shb = Domain.unstick_list sh in
    let bot = List.concat [lb; shb; hp; List.rev sht; List.rev lt] in //reconstruct bottom strand
    One bot

let strand_to_lower s =
  match s with
  | Strand.Upper _ -> s
  | Strand.Lower _ -> Strand.rotate s

(* returns the view from the right and the set of loose strands *)
let connect_lower a b =
  match a, b with
  | One _, One _ -> failwith "circular DNA"
  | One l1, Two (u, l2) -> Two (u, l1 @ l2), []
  | Two (u, l1), One l2 -> One (l1 @ l2), [Strand.Upper u]
  | Two (u1, l1), Two (u2, l2) -> Two (u2, l1 @ l2), [Strand.Upper u1]

(* returns the view from the right and the set of loose strands *)
let connect_upper a b =
  match a, b with
  | One _, One _ -> failwith "circular DNA"
  | One l1, Two (u, l2) -> Two ((List.rev l1) @ u, l2), []
  | Two (u, l1), One l2 -> One (l2 @ (List.rev u)), [Strand.Lower l1]
  | Two (u1, l1), Two (u2, l2) -> Two (u1 @ u2, l2), [Strand.Lower l1]

let rec melt_gate g =
  let collect_lower gs segs =
    let gss = List.map melt_segment segs in
    let f (prev_gs, ss) gs =
      let (next_gs, extra_ss) = connect_lower prev_gs gs in
      next_gs, extra_ss @ ss in
    Lib.fold_left f gs gss in
  let collect_upper gs segss =
    let f (prev_gs, ss) = function
      | [] -> prev_gs, ss
      | seg::segs ->
        let gs = melt_segment seg in
        let (next_gs, extra_ss) = connect_upper prev_gs gs in
        collect_lower (next_gs, extra_ss @ ss) segs in
    Lib.fold_left f gs segss in
  match g with
  | [] -> []
  | []::segs -> melt_gate segs
  | (seg::segs)::segss ->
    let lowers = collect_lower (melt_segment seg, []) segs in
    let gs, ss = collect_upper lowers segss in
    match gs with
    | One l -> Strand.Lower l::ss
    | Two (u, l) -> Strand.Upper u::Strand.Lower l::ss

let melt_with f (s:t) =     
    match s with 
    | STRAND st -> [f st]
    | GATE g -> melt_gate g |> List.map f
    | ORIGAMI o ->
      let gates, loose_strands = Origami.origami_species o in
      let gate_strands = gates |> List.collect melt_gate in
      (loose_strands @ gate_strands) |> List.map f
    | _ -> raise (new System.Exception("The melting of complex and unknown species is not yet implemented"))

(* return all species' strands (as if the species was melted by increasing the temperature) *)
let melt s = melt_with (strand_to_lower >> STRAND) s

let eval env = function
  | GATE g -> GATE (Gate.eval env g)
  | STRAND s -> STRAND (Strand.eval env s)
 // | COMPLEX b -> COMPLEX (Branch.eval env b)
  | ORIGAMI o -> ORIGAMI (Origami.eval env o)
  | UNKNOWN u -> UNKNOWN u
  | LogicDsdProcess p -> LogicDsdProcess p

(* hack for removing positions in events *)
(* Proper solution might be that evnts have Process.t rather than Species.t *)
let erasePos = function
  | GATE g -> GATE (Gate.erasePosns g)
  | STRAND s -> STRAND (Strand.erasePosns s)
  //| COMPLEX b -> COMPLEX (Branch.erasePosns b)
  | ORIGAMI o -> ORIGAMI (Origami.erasePosns o)
  | UNKNOWN u -> UNKNOWN u
  | LogicDsdProcess p -> LogicDsdProcess p

let universal_counters = function
  | GATE g -> GATE (Gate.universal_counters g)
  | STRAND s -> STRAND (Strand.universal_counters s)
//  | COMPLEX b -> COMPLEX (Branch.universal_counters b)
  | ORIGAMI o -> ORIGAMI (Origami.universal_counters o)
  | UNKNOWN u -> UNKNOWN u
  | LogicDsdProcess p -> LogicDsdProcess p

let free_names = function
  | GATE g ->    Gate.free_names g
  | STRAND s ->  Strand.free_names s
//  | COMPLEX b -> Branch.free_names b
  | ORIGAMI o -> Origami.free_names o
  | UNKNOWN _ -> []
  | LogicDsdProcess _ -> []