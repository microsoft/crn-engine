// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Gate
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

(* Multiple segments can be concatenated to form a gate *)(* Multiple segments can be concatenated to form a gate *)
type value = Value.t
type value_env = Value.t_env
type domain = Domain.t
type strand = Strand.t
type options = Options.t
type semantics = Options.semantics

(* We represent gates as lists of lists of segments.
 * The inner lists represent segments joined along a continuous lower strand, so "inner" cons corresponds to lower cons.
 * The outer list represent lower-stranded segments joined along an upper strand, so "outer" cons is upper cons. *)
type t = Segment.t list list

(* Candidate to replace above representation, to make connections more explicit *)
type connects = Uppers | Lowers
type t_complex = Segment of Segment.t
               (* first t_complex should be segment not t_complex *)
               | Sequence of connects * t_complex * t_complex

let first_segment (ct:t) :Segment.t = List.head (List.head ct)
let last_segment (ct:t) :Segment.t = List.last (List.last ct)

let rec first_segment_cp (ct:t_complex) :Segment.t =
  match ct with
  | Segment(s) -> s
  | Sequence(_,s,_) -> first_segment_cp s
let rec last_segment_cp (ct:t_complex) :Segment.t =
  match ct with
  | Segment(s) -> s
  | Sequence(_,_,s) -> last_segment_cp s

let replace_first (ct:t) (s:Segment.t) : t = (s::(List.tail (List.head ct)))::(List.tail ct)
let replace_last (ct:t) (s:Segment.t) : t = List.rev (replace_first (List.rev ct) s)

let rec replace_first_cp (ct:t_complex) (s:Segment.t) : t_complex =
  match ct with
  | Segment(_) -> Segment(s)
  | Sequence(c,f,l) -> Sequence(c,replace_first_cp f s,l)
let rec replace_last_cp (ct:t_complex) (s:Segment.t) : t_complex =
  match ct with
  | Segment(_) -> Segment(s)
  | Sequence(c,f,l) -> Sequence(c,f,replace_first_cp l s)

(* Mirror a gate between top and bottom. *)
let rec mirror (gs:t) : t =
  match gs with
  | [] -> []
  | (cs::gs) -> Lib.glue (Lib.separate (List.map Segment.mirror cs)) (mirror gs)

(* reverse <1 2>[3 4]<5 6> : <7 8>[9 10]<11 12> = <12 11>[10 9]<8 7> : <6 5>[4 3]<2 1> *)
let reverse (gs:t) : t = List.map (fun cs -> List.map Segment.reverse (List.rev cs)) (List.rev gs)

(* "Normalise" a pair of segments by moving shared overhangs to the leftward segment. *)
let normalise_pair ((cl:Segment.t),(cr:Segment.t)) =
  match cl,cr with
  | Segment.Double(lb1,lt1,s1,rt1,rb1),Segment.Double(lb2,lt2,s2,rt2,rb2) ->
    Segment.Double(lb1,lt1,s1,rt1,(rb1@lb2)),Segment.Double([],lt2,s2,rt2,rb2)
  | Segment.Double(lb1,lt1,s1,rt1,rb1),Segment.Hairpin(lb2,lt2,s2,hp,Segment.Right) ->
    Segment.Double(lb1,lt1,s1,rt1,(rb1@lb2)),Segment.Hairpin([],lt2,s2,hp,Segment.Right)
  | Segment.Hairpin(rb1,rt1,s1,hp,Segment.Left),Segment.Double(lb2,lt2,s2,rt2,rb2) ->
    Segment.Hairpin(rb1@lb2,rt1,s1,hp,Segment.Left),Segment.Double([],lt2,s2,rt2,rb2) 
  | Segment.Hairpin(rb1,rt1,s1,hp1,Segment.Left),Segment.Hairpin(lb2,lt2,s2,hp2,Segment.Right) ->
    Segment.Hairpin(rb1@lb2,rt1,s1,hp1,Segment.Left),Segment.Hairpin([],lt2,s2,hp2,Segment.Right)
  | a,b -> failwith ("Illegally constructed pair of segments " + Segment.display(a) + " and " + Segment.display(b))

let rec sets_check compare lls1 lls2 =
    match lls1,lls2 with
    | [],[] -> 0
    | l1::lls1,l2::lls2 ->
        let t_ans = compare l1 l2 in
        if t_ans = 0 then
            sets_check compare lls1 lls2
        else
            t_ans
    | _ -> failwith "Gate.sets_check given unequal lists"

(*"lexically" compare two gates *)
let compare (g1:t) (g2:t) = 
  let g1_length, g2_length = List.length g1, List.length g2 in
  if g1_length < g2_length then
    -1
  elif g1_length > g2_length then
    1
  else
    let ig1, ig2 = (List.map List.length g1), (List.map List.length g2) in
    let id = compare ig1 ig2 in
    if id <> 0 then
      id
    else
      sets_check (sets_check Segment.compare) g1 g2

let orient (g:t) : t =
  let rg = reverse (mirror g) in
  if g < rg then g else rg

(* "Normalise" how gates are represented by moving shared overhangs to the leftward segment. *)
let normalise (g:t) : t =
  (* Sort out shared bottom overhangs. *)
  let g = List.map (Lib.map_pairs normalise_pair) g in
  (* Use the same code to sort out shared top overhangs. *)
  (*orient*) (mirror (List.map (Lib.map_pairs normalise_pair) (mirror g)))

(* "Normalise" a complex by moving shared overhangs to the leftward segment, and moving segments leftward *)
let rec normalise_complex (c:t_complex) : t_complex =
  match c with 
  | Segment(s) -> Segment(s) (* A lone segment is always normalised *)
  | Sequence(Lowers, Segment(s1),Segment(s2)) ->
    let ns1,ns2= normalise_pair (s1,s2) in Sequence(Lowers, Segment(ns1), Segment(ns2))
  | Sequence(Uppers, Segment(s1), Segment(s2)) -> 
     let ns1,ns2 = normalise_pair ((Segment.mirror s1),(Segment.mirror s2)) in
     Sequence(Uppers, Segment(Segment.mirror ns1), Segment(Segment.mirror ns2)) (*use mirror to normalise, then put segment back*)
  | Sequence(Lowers, Segment(s1),Sequence(c,Segment(s2),comp)) ->
    let ns1,ns2= normalise_pair (s1,s2) in 
      Sequence(Lowers, Segment(ns1), normalise_complex(Sequence(c,Segment(ns2),comp)))
  | Sequence(Uppers, Segment(s1), Sequence(c,Segment(s2),comp)) -> 
     let ns1,ns2 = normalise_pair ((Segment.mirror s1),(Segment.mirror s2)) in
     Sequence(Uppers, Segment(Segment.mirror ns1), normalise_complex(Sequence(c,Segment(Segment.mirror ns2),comp)))
  | Sequence(c, Segment s, comp) ->
    normalise_complex (Sequence (c, Segment s, normalise_complex comp))
  | Sequence(c,Sequence(c2,comp1,comp2),comp3) ->
    normalise_complex (Sequence(c2,comp1,(normalise_complex (Sequence(c,comp2,comp3)))))

(* "Unnormalise" a pair of segments by moving shared overhangs to the rightward segment. *)
let unnormalise_pair ((cl:Segment.t),(cr:Segment.t)) =
  match cl,cr with
  | Segment.Double(lb1,lt1,s1,rt1,rb1),Segment.Double(lb2,lt2,s2,rt2,rb2) ->
    Segment.Double(lb1,lt1,s1,rt1,[]),Segment.Double((rb1@lb2),lt2,s2,rt2,rb2)
  | Segment.Double(lb1,lt1,s1,rt1,rb1),Segment.Hairpin(lb2,lt2,s2,hp,Segment.Right) ->
    Segment.Double(lb1,lt1,s1,rt1,[]),Segment.Hairpin((rb1@lb2),lt2,s2,hp,Segment.Right)
  | Segment.Hairpin(rb1,rt1,s1,hp,Segment.Left),Segment.Double(lb2,lt2,s2,rt2,rb2) ->
    Segment.Hairpin([],rt1,s1,hp,Segment.Left),Segment.Double((rb1@lb2),lt2,s2,rt2,rb2) 
  | Segment.Hairpin(rb1,rt1,s1,hp1,Segment.Left),Segment.Hairpin(lb2,lt2,s2,hp2,Segment.Right) ->
    Segment.Hairpin([],rt1,s1,hp1,Segment.Left),Segment.Hairpin((rb1@lb2),lt2,s2,hp2,Segment.Right)
  | _,_ -> failwith "Illegally constructed pair of segments"

(* "Unnormalise" a gate by moving shared overhangs to the rightward segment. *)
let unnormalise (g:t) : t =
  (* Sort out shared bottom overhangs. *)
  let g = List.map (Lib.map_pairs unnormalise_pair) g in
  (* Use the same code to sort out shared top overhangs. *)
  mirror(List.map (Lib.map_pairs unnormalise_pair) (mirror g))

(* Attach a "strand" to the right-hand end of a list of segments. *)
let append_strand_to_segments (cs:Segment.t list) (str:strand) =
  let f c =
    match c with
     | (Segment.Double(lb,lt,s,rt,rb)) -> 
       (match str with
         | Strand.Lower ns -> Segment.Double(lb,lt,s,rt,(rb@ns))
         | Strand.Upper ns -> Segment.Double(lb,lt,s,(rt@ns),rb))
     | Segment.Hairpin(ob,ot,s,hp,Segment.Left) ->
       (match str with
         | Strand.Lower ns -> Segment.Hairpin(ob@ns,ot,s,hp,Segment.Left)
         | Strand.Upper ns -> Segment.Hairpin(ob,ot@ns,s,hp,Segment.Left))
     | _ -> failwith "A right hairpin must be the last element in a gate"           
  in
  Lib.map_last f cs

(* Attach a "strand" to the left-hand end of a list of segments. *)
let cons_strand_to_segments (str:strand) (cs:Segment.t list) =
  let f c =
    match c with
    | Segment.Double(lb,lt,s,rt,rb) -> 
      (match str with
        | Strand.Lower ns -> Segment.Double((ns@lb),lt,s,rt,rb)
        | Strand.Upper ns -> Segment.Double(lb,(ns@lt),s,rt,rb))
    | Segment.Hairpin(ob,ot,s,hp,Segment.Right) ->
      (match str with
        | Strand.Lower ns -> Segment.Hairpin((ns@ob),ot,s,hp,Segment.Right)
        | Strand.Upper ns -> Segment.Hairpin(ob,(ns@ot),s,hp,Segment.Right))
    | _ -> failwith "A left hairpin must be the first element in a gate"
  in
  Lib.map_first f cs

(* Attach a "strand" to the right-hand end of a gate. *)
let append_strand_to_gate (g:t) (str:strand) : t =
  let f cs = append_strand_to_segments cs str in
  Lib.map_last f g

(* Attach a "strand" to the left-hand end of a gate. *)
let cons_strand_to_gate (str:strand) (g:t) : t =
  let f cs = cons_strand_to_segments str cs in
  Lib.map_first f g

(* Join two gates along the upper/lower strand. *)
let join (g1:t) (upper:bool) (g2:t) : t =
  if upper then g1@g2 else Lib.glue g1 g2

(***************************************************************************************************)

(* Produce a string representation of a gate. *)
let display (gs:t) = 
  let rec display_lower (cs:Segment.t list) = Lib.string_of_list Segment.display ":" cs in
  Lib.string_of_list display_lower "::" gs

(* Compute free names, all domains, or all tethered domains appearing in a gate. *)
let free_names (gs:t) = Lib.collect (Lib.collect Segment.free_names) gs
let domains (gs:t) = Lib.collect (Lib.collect Segment.domains) gs
let tethered_domains (gs:t) = Lib.collect (Lib.collect Segment.tethered_domains) gs
let get_tags g = Domain.tags_in_common_list (tethered_domains g)
let rev_fold f init (gs:t) =
  let rec loop_o (m, acc_o) = function
  | [] -> m, acc_o
  | u::us ->
    let rec loop_i (m, acc_i) = function
    | [] -> m, acc_i
    | l::ls ->
      let m1, x = f m l in
      loop_i (m1, x::acc_i) ls in
    let m1, l = loop_i (m, []) u in
    loop_o (m1, l::acc_o) us in
  loop_o (init, []) gs 

let map f gs =
  let m () c = (), f c
  let skip () (c:Segment.t) = (), c
  gs |> rev_fold m () |> snd |> rev_fold skip () |> snd

let annotate_tag_sets (gs:t) =
  let propagate_left_tags lt c =
    let clt, crt = Segment.tag_sets c in
    let seen_lt = match clt with Some _ -> clt | None -> lt in
    let next_lt = match crt with Some _ -> crt | None -> seen_lt in
    next_lt, Segment.set_tags c seen_lt in
  let propagate_right_tags rt c =
    let clt, crt = Segment.tethered_tag_sets c in
    let seen_rt = match crt with Some _ -> crt | None -> rt in
    let next_rt = match clt with Some _ -> clt | None -> seen_rt in
    next_rt, Segment.add_tags c seen_rt in
  gs |> rev_fold propagate_left_tags None |> snd |> rev_fold propagate_right_tags None |> snd

let unannotate_tag_sets (gs:t) =
  let clear_tags c = Segment.set_tags c None in
  gs |> map clear_tags

(** Evaluate the values in a gate. *)
let eval (e:value_env) (g:t) = List.map (List.map (Segment.eval e)) g

(* Check types and deal with position information. *)
let inferType in_origami (env:Types.type_env) (g:t) = List.iter (List.iter (Segment.inferType in_origami env)) g
let getPosn (g:t) = Segment.getPosn (first_segment g)
let erasePosns (g:t) = List.map (List.map Segment.erasePosns) g
let replacePosns (mrs:Types.range Stringmap.t) (g:t) = List.map (List.map (Segment.replacePosns mrs)) g

(* Get the list of all NON-TOEHOLD domains exposed in the gate. *)
let exposed_nontoeholds (g:t) = Lib.collect (Lib.collect Segment.exposed_nontoeholds) g

(** Look for exposed neighbouring toeholds. *)
let neighbouring_toeholds (g:t) = Lib.lookForSome (Lib.lookForSome Segment.neighbouring_toeholds) g

(***************************************************************************************************)

(** Migrate all branches as far as possible towards the right. Remember: each moves by less than one segment. *)
let migrate_right_all (gs:t) =
  let rec top_migrations gs =
    let gs' = Lib.map_pairs Segment.migrate_right_simple gs in
    if (gs=gs') then gs else top_migrations gs'
  in
  (* First do all the top strand migrations. *)
  let gs = List.map top_migrations gs in
  (* Then do all the bottom strand migrations. *)
  mirror(List.map top_migrations (mirror gs))

let migrate_right_all_complex c = c

(** Migrate all branches as far as possible to the left. Remember: each moves by less than one segment. *)
let migrate_left_all (gs:t) = reverse (migrate_right_all (reverse gs))

(** Use branch migration to put molecules into a standard form.
  * Also use the "normalise" function to ensure that gates are always represented consistently. *)
let standard_form (opts:options) (gs:t) =
  match (Options.getRules opts) with
  | Options.Detailed -> normalise gs
  | Options.Finite | Options.Default | Options.Infinite -> normalise(migrate_right_all gs)

let standard_form_complex (opts:options) (ct:t_complex) =
  match (Options.getRules opts) with
  | Options.Detailed -> normalise_complex ct
  | Options.Finite | Options.Default | Options.Infinite -> normalise_complex (migrate_right_all_complex ct)

(* "Physically rotate" a gate. Once it's rotated, put it back into standard form. *)
let rotate (opts:options) (s:t) = standard_form opts (reverse(mirror s))

(* Rotate a complex, add standard_form call *)
let rec rotate_complex (opts:options) (c:t_complex) = 
   standard_form_complex opts 
     (match c with
      | Segment s1 -> (Segment (Segment.rotate s1))
      | Sequence(Uppers,c1,c2) -> Sequence(Lowers, rotate_complex opts c2, rotate_complex opts c1)
      | Sequence(Lowers,c1,c2) -> Sequence(Uppers, rotate_complex opts c2, rotate_complex opts c1))

let rec compare_complex (c1:t_complex) (c2:t_complex) = 
  match c1,c2 with
  | Segment s1, Segment s2 -> Segment.compare s1 s2
  | Sequence(_, cf1, cr1),Sequence(_,cf2, cr2) -> 
     let cfs = compare_complex cf1 cf2 in
     if cfs <> 0 then cfs else compare_complex cr1 cr2 
  | Segment _ , Sequence _ -> -1
  | Sequence _, Segment _ -> 1

(* Compare two gates, up to physical rotation. *)
let equal (opts:options) (g1:t) (g2:t) = ((g1 = g2) || (g1 = (rotate opts g2)))

(* compare two complexes, without changing orientation. *)
let rec equal_complex_static (c1: t_complex) (c2:t_complex) =
  match c1,c2 with
   | (Segment s1, Segment s2) -> (Segment.equals s1 s2)
   | (Sequence(conn1,cl1,cr1),Sequence(conn2,cl2,cr2)) -> 
     (conn1=conn2 && (equal_complex_static cl1 cl2) && (equal_complex_static cr1 cr2))
   | _,_ -> false

(* compare two complexes, up to physical rotation *)
let equal_complex (opts:options) (c1:t_complex) (c2:t_complex) = 
    equal_complex_static c1 c2 || equal_complex_static c1 (rotate_complex opts c2)

(* Does gate g' match the "pattern" g? *)
let matches (opts:options) (g1:t) (g2:t) =
  let inner_match g1 g2 = Lib.forall2 (Lib.forall2 Segment.matches) g1 g2 in
  (inner_match g1 g2) || (inner_match g1 (rotate opts g2))

(* Is there an extension of env such that gate g' matches the "pattern" g? *)
let matches_env (opts:options) (env: Domain.match_env) (g1:t) (g2:t) =
  let inner_match_env g1 g2 = Lib.option_and2 (Lib.option_and2 Segment.matches_env) env g1 g2 in
  Lib.option_or [inner_match_env g1 g2; inner_match_env g1 (rotate opts g2)]

(* Does gate g' match the "pattern" g? *)
let matches_static (g1:t) (g2:t) = Lib.forall2 (Lib.forall2 Segment.matches) g1 g2  

let has_wildcard (g: t) = List.exists (List.exists Segment.has_wildcard) g

(***************************************************************************************************)
(*** STICKING REACTIONS. *)

(** Sticking reactions between a strand and a gate. Note: only the strand may rotate *)
let stick_strand_to_gate sub_map (unproductive:bool) (s:strand) (g:t) =
  let rec gather_top (s:strand) (rs:(domain * t) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> rs
    | (c::gs) ->
        (* Try and stick the strand s to the selected gate at segment c, removing unproductive reactions if requested. *)
        let allow cl cr stuck_to_rhs =
          if unproductive then true else
          (* Transfer all shared overhangs to the middle segment, temporarily. *)
          let c1,cc,c2 =
            if stuck_to_rhs then
              match gs with
              | [] -> let c1,cc = unnormalise_pair (cl,cr) in (Some c1),cc,None
              | c2::_ -> let c1,cc = unnormalise_pair (cl,cr) in let cc,c2 = normalise_pair (cc,c2) in (Some c1),cc,(Some c2)
            else
              match Lib.get_last_element acc with
              | None -> let cc,c2 = normalise_pair (cl,cr) in None,cc,(Some c2)
              | Some(_,c1) -> let cc,c2 = normalise_pair (cl,cr) in let c1,cc = unnormalise_pair (c1,cc) in (Some c1),cc,(Some c2)

          (match c1 with
            | None -> false
            | Some c1 -> (Option.isSome (Segment.migrate_left c1 cc)) ||
                         (Option.isSome (Segment.displace_left c1 cc)) ||
                         (Option.isSome (Segment.migrate_open_left c1 cc))) ||
          (match c2 with
            | None -> false
            | Some c2 -> (Option.isSome (Segment.migrate_right cc c2)) ||
                         (Option.isSome (Segment.displace_right cc c2)) ||
                         (Option.isSome (Segment.migrate_open_right cc c2))) ||
          (Option.isSome (Segment.cover_left cc)) ||
          (Option.isSome (Segment.cover_right cc))
        in
        let possible_branch stuck_to_rhs (lhs: t) (acc:Segment.t list) (gs:Segment.t list) (rhs: t) = 
           (List.isEmpty acc && (lhs <> []) && not stuck_to_rhs)
        || (List.isEmpty gs && (rhs <> []) && stuck_to_rhs)  in
        let f (cl,cr,(n:Segment.domain, sup),stuck_to_rhs) =
          if (sup || (allow cl cr stuck_to_rhs)) && not (possible_branch stuck_to_rhs lhs acc gs rhs)
          then [n,(lhs@[acc@[cl;cr]@gs]@rhs)] else [] in
        let rs' = Lib.collect f (Segment.stick_sg sub_map c s) in
        gather_top s (rs@rs') (acc@[c]) lhs gs rhs
  in
  (* Compute all sticking reactions of strand "s" onto top strand of gate "g". *)
  let top_stickings g s =
    List.map
      (fun (n,g') -> (g, s, n, g'))
      (Lib.collect_contextual (gather_top s [] []) g)
  (* Compute all sticking reactions of strand "s" onto bottom strand of gate "g". *)
  let bottom_stickings g s =
    List.map
      (fun (n,g') -> (g, s, n, (mirror g')))
      (Lib.collect_contextual (gather_top (Strand.mirror s) [] []) (mirror g))
  (* Try normally and rotated *)
  let normal = (top_stickings g s) @ (bottom_stickings g s) in
  let rotated = (top_stickings g (Strand.rotate s)) @ (bottom_stickings g (Strand.rotate s)) in
  normal @ rotated

(** Sticking reactions between a strand and a list of other strands. *)
(* NB: I think that the "well-formedness" constraints will mean that the only reactions of this sort are unproductive... *)
let stick_strand_to_strand sub_map (unproductive:bool) (s1:strand) (s2:strand) =
  let allow c =
    if unproductive then true else
    ((Option.isSome (Segment.cover_left c)) || (Option.isSome (Segment.cover_right c)))
  in
  let stickings s1 s2 =
    let f (c,(n:Segment.domain, sup)) = if sup || allow c then [(s1,s2,n,[[c]])] else [] in
    Lib.collect f (Segment.stick_ss sub_map s1 s2)
  in
  (* Try normally and rotated *)
  let normal = stickings s1 s2 in
  let rotated = stickings s1 (Strand.rotate s2) in
  normal @ rotated

(** Sticking reactions between a strand and a list of other strands. *)
(* NB: I think that the "well-formedness" constraints will mean that the only reactions of this sort are unproductive... *)
let stick_strand_to_strand_COMPLEX sub_map (unproductive:bool) (s1:strand) (s2:strand) =
  let allow c =
    if unproductive then true else
    ((Option.isSome (Segment.cover_left c)) || (Option.isSome (Segment.cover_right c)))
  in
  let stickings s1 s2 =
    let stick (c,(n:Segment.domain, sup)) = if sup || allow c then [(s1,s2,n,Segment(c))] else [] in
    Lib.collect stick (Segment.stick_ss sub_map s1 s2)
  in
  (* Try normally and rotated *)
  let normal = stickings s1 s2 in
  let rotated = stickings s1 (Strand.rotate s2) in
  normal @ rotated

(** Sticking reactions between two gates. *)
let stick_gate_to_gate sub_map (opts:options) (g1:t) (g2:t) =
  let unproductive = Options.getUnproductive opts in
  let allow c1 cc c2 left_on_top =
    if unproductive then true else (* Ignore unproductive polymerisations if "polymers" is disabled. *)
    (if left_on_top then
       (Option.isSome (Segment.migrate_left (Segment.mirror c1) (Segment.mirror cc))) ||
       (Option.isSome (Segment.displace_left (Segment.mirror c1) (Segment.mirror cc))) ||
       (Option.isSome (Segment.migrate_open_left (Segment.mirror c1) (Segment.mirror cc)))
     else
       (Option.isSome (Segment.displace_left c1 cc)) ||
       (Option.isSome (Segment.migrate_left c1 cc)) ||
       (Option.isSome (Segment.migrate_open_left (Segment.mirror c1) (Segment.mirror cc))))
    ||
    (if left_on_top then
       (Option.isSome (Segment.displace_right cc c2)) ||
       (Option.isSome (Segment.migrate_right cc c2)) ||
       (Option.isSome (Segment.migrate_open_right cc c2))
     else
       (Option.isSome (Segment.migrate_right (Segment.mirror cc) (Segment.mirror c2))) ||
       (Option.isSome (Segment.displace_right (Segment.mirror cc) (Segment.mirror c2))) ||
       (Option.isSome (Segment.migrate_open_right (Segment.mirror cc) (Segment.mirror c2))))
    ||
    (Option.isSome (Segment.cover_left cc)) ||
    (Option.isSome (Segment.cover_right cc))
  in
  (* Find the extreme segments and see whether they can polymerise. *)
  (* %%%%%%%%%%%%%%%%%%%%%% TO DO: add a check for unwanted secondary structure...
     %%%%%%%%%%%%%%%%%%%%%%        This may arise if two gates interact but NOT at two extreme segments... *)
  let stickings (gl:t) (gr:t) =
//    let debug_gl,debug_gr = display gl, display gr in
    let gll,csl,cl =
      match Lib.get_last_element gl with
      | Some(gll,cslc) ->
         (match Lib.get_last_element cslc with
          | Some(csl,c) -> gll,csl,c
          | None -> failwith "Gate.stick_gate_to_gate: cslc is empty")
      | None -> failwith "Gate.stick_gate_to_gate: gl is empty"
    in
    let cr,csr,grr =
      match gr with
      | ccsr::grr ->
         (match ccsr with
          | cr::csr -> cr,csr,grr
          | [] -> failwith "Gate.stick_gate_to_gate: ccsr is empty")
      | [] -> failwith "Gate.stick_gate_to_gate: gr is empty"
    in
    Lib.collect (fun (c1,cc,c2,(n, sup),left_on_top) ->
      if sup || allow c1 cc c2 left_on_top then
        let g' = if left_on_top then gll@[csl@[c1]]@[cc::c2::csr]@grr
                                else gll@[csl@[c1;cc]]@[c2::csr]@grr
        in [(gl, gr, n, g')]
      else [])
      (Segment.stick_gg sub_map cl cr)
  in
  (* RLP: Order preservation is important for origami code *)
  let swapped_stickings g2 g1 =
    stickings g2 g1 |> List.map (fun (g2, g1, d, cs) -> (g1, g2, d, cs)) in
  (* Try normally and rotated *)
  let normal = (stickings g1 g2) @ (swapped_stickings g2 g1) in
  let rotated = (stickings g1 (rotate opts g2)) @ (swapped_stickings (rotate opts g2) g1) in
  //let debug_gen_gates = Lib.string_of_list display "   " (List.map (fun (_,_,_,g) -> g) (normal@rotated)) in
  normal @ rotated
  (* %%%%%%%%%%%%%%%%%%%% NB: what do we do if g1=g2 - can we discard the duplicate reaction or do we need two copies to get the kinetics right? *)

(** Sticking reactions that cause a hairpin in a strand *)
(* NB: I think that the "well-formedness" constraints will mean that the only reactions of this sort are unproductive... *)
let pin_strand (unproductive:bool) (s1:strand)  =
  let allow c = 
    if unproductive then true else
    ((Option.isSome (Segment.cover_left c)) || (Option.isSome (Segment.cover_right c)))
  in
  let pinings s =
    let f (c,(n:Segment.domain, sup)) = if sup || allow c then [(s1,n,[[c]])] else [] in
    Lib.collect f (Segment.pin_strand s)
  in
    pinings s1

(** Sticking reactions in a gate that result in a hairpin at one edge of the gate *)
(* NB: This doesn't consider the possibility of a hairpin occuring anywhere except at the leftmost or rightmost segment of a gate
       because otherwise a hairpin must be allowed as the overhang of a segment *)
let pin_gate (unproductive:bool) (g:t) =
  let rec gather_top (rs:(domain * t) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> rs
    | (c::gs) ->
        (* Try and pin a bottom overhang of the selected gate at segment c, removing unproductive reactions if requested. *)
        (* Don't allow hairpins at non-extreme points, regardless of productivity *)
        let allow cl cr pinned_rhs = 
         if ((unproductive && pinned_rhs && List.isEmpty rhs && List.isEmpty gs) ||
             (unproductive && (not pinned_rhs) && List.isEmpty acc && List.isEmpty lhs)) then true else
          (* Transfer all shared overhangs to the middle segment, temporarily. *)
          let c1,cc,c2 =
            if pinned_rhs then
              match gs,rhs with
              | [],[] -> let c1,cc = unnormalise_pair (cl,cr) in (Some c1),Some(cc),None
              | _,_ -> None,None,None (* Don't pin none-end segments *)
            else
              match acc,lhs with
              | [],[] -> let cc,c2 = normalise_pair (cl,cr) in None,Some(cc),(Some c2)
              | _,_ -> None,None,None (* Don't pin none-end segments *)
          in
          (match c1,cc,c2 with
            | None,None,None -> false
            | Some c1,Some cc,None -> (Option.isSome (Segment.migrate_left c1 cc)) ||
                                      (Option.isSome (Segment.displace_left c1 cc)) ||
                                      (Option.isSome (Segment.migrate_open_left c1 cc))
            | None, Some cc, Some c2 -> (Option.isSome (Segment.migrate_right cc c2)) ||
                                        (Option.isSome (Segment.displace_right cc c2)) ||
                                        (Option.isSome (Segment.migrate_open_right cc c2))
            | _,_,_ -> false) 
        in
        let f (cl,cr,(n:Segment.domain,sup),pinned_rhs) = if sup || allow cl cr pinned_rhs then [n,(lhs@[acc@[cl;cr]@gs]@rhs)] else [] in
        let rs' = Lib.collect f (Segment.pin_segment c) in
        gather_top (rs@rs') (acc@[c]) lhs gs rhs
  in
  (* Compute all pinings of segment overhangs on bottom of gate "g". *)
  let bottom_pinings g = List.map (fun (n,g') -> (g, n, g')) (Lib.collect_contextual (gather_top [] []) g)
  in
  (* Compute all pinings of segment overhangs on top of gate "g". *)
  let top_pinings g = List.map (fun (n,g') -> (g, n, (mirror g'))) (Lib.collect_contextual (gather_top [] []) (mirror g))
  in
  (top_pinings g) @ (bottom_pinings g)

(***************************************************************************************************)
(*** UNSTICKING REACTIONS. *)

(* If a strand is ejected (by unbinding, displacing or leaking) determine whether anything else is ejected too. *)
let ejected (lhs:t) (acc:Segment.t list) (s:strand) (gs:Segment.t list) (rhs:t) : ((t list) * (strand list)) =
  (* Decide what the "left" and "right" sides of the ejected species should be. *)
  let left,right =
    match s with
    | Strand.Upper _ -> ((if List.isEmpty acc then lhs else []),(if List.isEmpty gs then rhs else []))
    | Strand.Lower _ -> ((if List.isEmpty acc then [] else lhs@[acc]),(if List.isEmpty gs then [] else gs::rhs)) (* RLP: should this be [gs]::rhs ? *)
  in
  match left,right with
    | [],[] -> ([],[s])
    | left,[] -> let g = append_strand_to_gate left s in ([g],[])
    | [],right -> let g = cons_strand_to_gate s right in ([g],[])
    | left,right -> let g = join left (Strand.is_upper s) (cons_strand_to_gate s right) in ([g],[])

(** Return all unsticking reactions possible from a given gate. *)
let unsticking_reactions sub_map (opts:options) (g:t) =
  let rec gather (rs:(t * domain * t list * strand list) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> rs
    | c::gs ->
      (* Try and do an unsticking operation. *)
      let try_unstick lhs acc c gs rhs =
        match (Segment.unstick sub_map c) with
        (* lhs :: (acc : <l>[n^]<r> : gs) :: rhs *)
        | Some(s1,s2,n) ->
            let (gs1,ss1) = ejected lhs acc s1 gs rhs in
            let (gs2,ss2) = ejected lhs acc s2 gs rhs in
            [g,n,(gs1@gs2),(ss1@ss2)]
        | None -> []
      in
      match (try_unstick lhs acc c gs rhs) with
      | [] ->
        (* If structural congruence includes branch migration, left migrate (if possible) and try again. *)
        (match (Options.getRules opts) with
          | Options.Detailed -> gather rs (acc@[c]) lhs gs rhs
          | _ ->
              let c',gs',rhs' = 
                match migrate_left_all ((c::gs)::rhs) with (* %%%% If structural congruence includes branch migration, try left migrating first. *)
                  | (c'::gs')::rhs' -> c',gs',rhs'
                  | _ -> failwith "Gate.displacement_reactions: migrate_left_all should return a non-empty list with a non-empty head"
              in
              match (try_unstick lhs acc c' gs' rhs') with
                | [] -> gather rs (acc@[c]) lhs gs rhs
                | rs' -> gather (rs@rs') (acc@[c]) lhs gs rhs)
      (* Managed to do an unbinding straight away. *)
      | rs' -> gather (rs@rs') (acc@[c]) lhs gs rhs
    in
  Lib.collect_contextual (gather [] []) g

(***************************************************************************************************)
(*** Unbind open reactions **)

(** Return all reactions where a hairpin opens and gloms onto the overhang of the gate, or just turns into a strand *)
let unbind_open_reactions (opts:options) (g:t) =
  let tenacious =
    match (Options.getRules opts) with
    | Options.Detailed -> false
    | _ -> true in
  let migrate_top c s =
    match Segment.migrate_left c s with
    | Some (mc, ms, _) -> Some (mc, ms)
    | None -> None in
  let migrate_bottom c s =
    match Segment.migrate_left (Segment.mirror c) (Segment.mirror s) with
    | Some (mc, ms, _) -> Some (Segment.mirror mc, Segment.mirror ms)
    | None -> None in
  let tenacious_unbind_gg_lcon c s =
    match Segment.unbind_open_gg_lcon c s with
    | Some r ->  Some r
    | None ->
      if tenacious then
        (match migrate_top c s with (* %%%% If structural congruence includes branch migration, try left migrating first. *)
        | Some (mc, ms) -> Segment.unbind_open_gg_lcon mc ms
        | None -> None)
      else None in
  let tenacious_unbind_gg_ucon c s =
    match Segment.unbind_open_gg_ucon c s with
    | Some r ->  Some r
    | None ->
      if tenacious then
        (match migrate_bottom c s with (* %%%% If structural congruence includes branch migration, try left migrating first. *)
        | Some (mc, ms) -> Segment.unbind_open_gg_ucon mc ms
        | None -> None)
      else None in
  let left_opens = function
  | [[c]] ->
    (match Segment.unbind_open_down c with
     | None -> []
     | Some (st,n) -> [g,n,[],[st]])
  | [c]::[s]::cs ->
    (match tenacious_unbind_gg_ucon c s with
     | None -> []
     | Some (st,n) -> [g,n,[[st]::cs],[]])
  | [c]::(s::ss)::cs -> 
    (match tenacious_unbind_gg_ucon c s with
     | None -> []
     | Some (st,n) -> [g,n,[(st::ss)::cs],[]])
  | (c::s::ss)::cs -> 
    (match tenacious_unbind_gg_lcon c s with
     | None -> []
     | Some (st,n) -> [g,n,[(st::ss)::cs],[]])
  | _ -> [] in
  let rotate_result (g:t, n:Segment.domain, gs, sts) = (g, n, List.map (rotate opts) gs, List.map Strand.rotate sts) in
  let lefts = left_opens g in
  let rights = left_opens (rotate opts g) |> List.map rotate_result in
  lefts@rights

(***************************************************************************************************)
(*** LEAK REACTIONS. *)

(** Return all leak reactions possible between a strand and a gate. *)
let leak_strand_into_gate (opts:options) (s:strand) (g:t) :(t * strand * bool * t list * strand list) list =
  (* Collect all the different ways that the strand s could leak into this segment. *)
  let find_leaks (s:strand) (acc:Segment.t list) (c:Segment.t) (lhs:t) (gs:Segment.t list) (rhs:t) =
    let f (c',s',fast: bool) =
      (* NB: from "Segment.leak_sg" we can assume that s' is an UPPER strand. *)
      let gs_out,ss_out =
        match ejected lhs acc s' gs rhs with
        | [g_out],[] -> [[acc@[c']@gs]; g_out],[]
        | [],[s_out] -> [lhs@[acc@[c']@gs]@rhs],[s_out]
        | _,_ -> failwith "Gate.leak_strand_into_gate: ejection not a single gate or strand"
      in
      (fast,gs_out,ss_out)
    in
    List.map f (Segment.leak_sg c s)
  in
  (* Gather all the possible leak reactions. *)
  let rec gather_top (s:strand) (rs:(bool * t list * strand list) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> failwith "Gate.leak_reactions: empty gate"
    | [c] -> rs@(find_leaks s acc c lhs [] rhs)
    | (c::c'::gs) ->
        (* If structural congruence includes branch migration, try again after left migrating (if possible).
           This shouldn't loop as the migration won't be possible a second time.
           Also, the call to this function (in reaction.ml) should put the gate back into standard form. *)
        let (c1,c1') =
          (match (Options.getRules opts) with
           | Options.Detailed -> (c,c')
           | Options.Infinite | Options.Finite | Options.Default -> Segment.migrate_left_simple (c,c'))
        in
        (* Collect all the different ways that this segment could leak. *)
        let rs' = find_leaks s acc c1 lhs (c1'::gs) rhs in   (* Use the migrated segments here. *)
        gather_top s (rs@rs') (acc@[c]) lhs (c'::gs) rhs     (* Revert to the original segments for the recursive call. *)
  in
  (* Both upper and lower strands can leak in, and we must also account for rotation. *)
  let leaks_with s =
    match s with
    | Strand.Upper _ -> List.map
                          (fun (fast,gs',ss') -> (g, s, fast, gs', ss'))
                          (Lib.collect_contextual (gather_top s [] []) g)
    | Strand.Lower _ -> List.map (fun (fast,gs',ss') ->
                          (g, s, fast, (List.map mirror gs'), (List.map Strand.mirror ss')))
                          (Lib.collect_contextual (gather_top (Strand.mirror s) [] []) (mirror g))
  in
  let leaks = (leaks_with s) @ (leaks_with (Strand.rotate s)) in
  (* Remove any leak reaction where the products are the same as the reactants. *)
  List.filter (fun (g,s,_,gs',ss') -> (gs',ss') <> ([g],[s])) leaks

(** Return all leak reactions possible between two gates. *)
(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% *)
let leak_gate_into_gate (_:options) (_:t) (_:t) = ([]:(t * t * bool * t list * strand list) list) (* %%%%%%%%%%%%%%%%%%%%%%%%%%%% *)

(***************************************************************************************************)

(** Return all single displacement reactions possible from a given gate. *)
let displacement_reactions (opts:options) (g:t) =
  let rec gather_top (rs:(domain list * t list * strand list) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> rs
    | [_] -> rs
    | (c1::c2::gs) ->
        let left =
         (match Segment.displace_left c1 c2 with
          | Some(c,s,ns) ->
              (match ejected lhs acc s [] [] with
                | [g_out],[] -> [ns,[([[c]@gs]@rhs);g_out],[]]
                | [],[s_out]-> [ns,[lhs@[acc@[c]@gs]@rhs],[s_out]]
                | _,_ -> failwith "Gate.displacement_reactions: leftward ejection not a single gate or strand")
          | None -> [])
        in
        let c2',gs' =
         (match (Options.getRules opts) with
          | Options.Detailed -> c2,gs
          | Options.Infinite | Options.Finite | Options.Default ->
              match migrate_left_all [c2::gs] with (* %%%% If structural congruence includes branch migration, try left migrating first. *)
                | [c2'::gs'] -> c2',gs'
                | _ -> failwith "Gate.displacement_reactions: migrate_left_all should return singleton list")
        in
        let right =
         (match Segment.displace_right c1 c2' with
          | Some(c,s,ns) ->
              (match ejected [] [] s gs' rhs with
                | [g_out],[] -> [ns,[(lhs@[acc@[c]]); g_out],[]]
                | [],[s_out] -> [ns,[lhs@[acc@[c]@gs']@rhs],[s_out]]
                | _,_ -> failwith "Gate.displacement_reactions: rightward ejection not a single gate or strand")
          | None -> [])
        in
        gather_top (rs@left@right) (acc@[c1]) lhs (c2::gs) rhs
  in
  (* Compute all displacement reactions from top strand. *)
  let top_displacements = List.map (fun (ns,gs',ss') -> (g,ns,gs',ss')) (Lib.collect_contextual (gather_top [] []) g) in
  (* Compute all displacement reactions from bottom strand. *)
  let bottom_displacements =
    List.map
      (fun (ns,gs',ss') -> (g, ns, (List.map mirror gs'), (List.map Strand.mirror ss')))
      (Lib.collect_contextual (gather_top [] []) (mirror g))

  top_displacements @ bottom_displacements

(***************************************************************************************************)
(** Return all hairpin opening migrations possible on a given gate. *)
let open_migration_reactions (opts:options) (g:t) =
  let rec gather_top (rs:(domain list * t) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> rs
    | [_] -> rs
    | (c1::c2::gs) ->
        let left =
         (match Segment.migrate_open_left c1 c2 with
          | Some(c,ns) -> [ns, (lhs@[acc@[c]@gs]@rhs)]
          | None -> [])
        in
        let c2',gs' =
         (match (Options.getRules opts) with
          | Options.Detailed -> c2,gs
          | Options.Infinite | Options.Finite | Options.Default ->
              match migrate_left_all [c2::gs] with (* %%%% If structural congruence includes branch migration, try left migrating first. *)
                | [c2'::gs'] -> c2',gs'
                | _ -> failwith "Gate.open_migration_reactions: migrate_left_all should return singleton list")
        in
        let right =
         (match Segment.migrate_open_right c1 c2' with
          | Some(c,ns) -> [ns, (lhs@[acc@[c]@gs']@rhs)]
          | None -> [])
        in
        gather_top (rs@left@right) (acc@[c1]) lhs (c2::gs) rhs
  in
  (* Compute all open migration reactions from top strand. *)
  let top_opens = List.map (fun (ns,gs') -> (g,ns,gs')) (Lib.collect_contextual (gather_top [] []) g) in
  (* Compute all open migration reactions from bottom strand. *)
  let bottom_opens = List.map (fun (ns,gs') -> (g, ns, (mirror gs')))
                              (Lib.collect_contextual (gather_top [] []) (mirror g))
  in
  top_opens @ bottom_opens

(***************************************************************************************************)
(** Return all hairpin closing migrations possible on a given gate. 
  * These are technically leak reactions, but localised ones so relatively fast.
  *)
let close_migration_reactions (opts:options) (g:t) =
  let close_lower_left = function
    | [] -> []
    | []::_ -> [] (* Consider raising an error or recursing *)
    | [c]::gs -> Segment.close_lower_left_migrate c |> List.map (fun (hpds, hp,rg,ds) -> hpds, ds, [hp;rg]::gs)
    | (c1::c2::cs)::gs -> 
      let c, cr = normalise_pair (c1, c2) in
      Segment.close_lower_left_migrate c |> List.map (fun (hpds, hp,rg,ds) -> hpds, ds, (hp::rg::cr::cs)::gs) in
  
  let close_front gs =
    let top_closes = gs |> close_lower_left in
    let bottom_closes = gs |> mirror |> close_lower_left |> List.map (fun (hpds, d,g') -> (hpds, d, mirror g')) in
    top_closes @ bottom_closes in

  let front_closes = g |> close_front |> List.map (fun (hpds, d,g') -> (hpds, g,d,g')) in
  let back_closes = g |> rotate opts |> close_front |> List.map (fun (hpds, d,g') -> (hpds, g,d,rotate opts g')) in

  front_closes @ back_closes

(** Return all hairpin closing displacings possible on a given gate. 
  * These are technically leak reactions, but localised ones so relatively fast.
  *)
let close_displacing_reactions (opts:options) (g:t) = (* do the whole context thing using ejected *)
  let close_lower_left = function
    | [] -> []
    | []::_ -> [] (* Consider raising an error or recursing *)
    | [c]::gs -> Segment.close_lower_left_displace c
                  |> List.map (fun (hp, c', s, ds) -> 
                                 (match ejected [] [] s [c'] gs with
                                  | [], ss -> hp, ds, [[c']::gs], ss
                                  | egs, ss -> hp, ds, [[c']]::egs, ss
                                 ))
    | (c1::c2::cs)::gs -> 
      let c, cr = normalise_pair (c1, c2) in
      Segment.close_lower_left_displace c
        |> List.map (fun (hp, c', s, ds) -> 
                       (match ejected [] [] s (c'::cr::cs) gs with
                        | [], ss -> hp, ds, [(c'::cr::cs)::gs], ss
                        | egs, ss -> hp, ds, [[c']]::egs, ss
                       )) in
  
  let close_front gs =
    let top_closes = gs |> close_lower_left in
    let bottom_closes = gs |> mirror |> close_lower_left |> List.map (fun (hp, ds,egs,ess) -> (hp, ds, List.map mirror egs, List.map Strand.mirror ess)) in
    top_closes @ bottom_closes in

  let front_closes = g |> close_front |> List.map (fun (hp, ds,egs,ess) -> (hp, g, ds, egs, ess)) in
  let back_closes = g |> rotate opts |> close_front |> List.map (fun (hp, ds,egs,ess) -> (hp, g, ds, List.map (rotate opts) egs, List.map Strand.rotate ess)) in

  front_closes @ back_closes



(** Return all hairpin closing displacings opening a hairpin on the other side. 
  * These are technically leak reactions, but localised ones so relatively fast.
  *)
let close_open_reactions (opts:options) (g:t) =
  let close_lower_left = function
    | [[c]] -> Segment.close_lower_left_open c |> List.map (fun (hpds, s,ds) -> hpds, [[s]],ds)
    | _ -> [] in
  
  let close_front gs =
    let top_closes = gs |> close_lower_left in
    let bottom_closes = gs |> mirror |> close_lower_left |> List.map (fun (hpds, g',ds) ->hpds,  mirror g', ds) in
    top_closes @ bottom_closes in

  let front_closes = g |> close_front |> List.map (fun (hpds, g',ds) -> (hpds, g,ds,g')) in
  let back_closes = g |> rotate opts |> close_front |> List.map (fun (hpds, g',ds) -> (hpds, g, ds, rotate opts g')) in

  front_closes @ back_closes

(***************************************************************************************************)

(** Return all migration reactions possible on a given gate. *)
let migration_reactions (g:t) =
  let rec gather_top (rs:(domain list * t) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> rs
    | [_] -> rs
    | (c1::c2::gs) ->
        let left =
         (match (Segment.migrate_left c1 c2) with
          | Some(c1',c2',ns) -> [ns,(lhs@[acc@[c1';c2']@gs]@rhs)]
          | _ -> [])
        in
        let right =
         (match (Segment.migrate_right c1 c2) with
          | Some(c1',c2',ns) -> [ns,(lhs@[acc@[c1';c2']@gs]@rhs)]
          | _ -> [])
        in
        gather_top (rs@left@right) (acc@[c1]) lhs (c2::gs) rhs
  in
  (* Compute all migration reactions on top strand. *)
  let top_migrations = List.map (fun (ns,g') -> (g,ns,g')) (Lib.collect_contextual (gather_top [] []) g) in
  (* Compute all migration reactions on bottom strand. *)
  let bottom_migrations =
    List.map (fun (ns,g') ->
      (g, ns, mirror g'))
      (Lib.collect_contextual (gather_top [] []) (mirror g))
  top_migrations @ bottom_migrations

(***************************************************************************************************)

(** Return all cover reactions possible on a given gate. *)
let cover_reactions (g:t) =
   (* Gather intra-segment covers computed using the function passed in as first argument. *)
   let rec gather (cover:Segment.t -> (Segment.t * domain) option) (rs:(domain * t) list) (acc:Segment.t list) (lhs:t) (gs:Segment.t list) (rhs:t) =
     match gs with
     | [] -> rs
     | c::gs ->
        let rs' =
          match (cover c) with
          | Some(c',d) -> [d,(lhs@[acc@[c']@gs]@rhs)]
          | None -> []
        gather cover (rs@rs') (acc@[c]) lhs gs rhs

   (* Get all cover reactions on the gate, using unnormalisation to compute the leftward covers. *)
   let leftward_covers =
     List.map
       (fun (n,g') -> (g, n, normalise g'))
       (Lib.collect_contextual (gather Segment.cover_left [] []) (unnormalise g))

   let rightward_covers =
     List.map 
       (fun (n,g') -> (g, n, g'))
       (Lib.collect_contextual (gather Segment.cover_right [] []) g)
 
   leftward_covers @ rightward_covers

(***************************************************************************************************)

(** Get all fast reactions on a list of gates (rate may be tau or infinity depending on semantics). *)
let fast_reactions sub_map (opts:options) (gs:t list) : (t list * strand list) list =
  (* Consider the results equivalent up to reordering of the strand list. *)
  let tau_match (gs1,ss1) (gs2,ss2) = (Lib.is_permutation (equal opts) gs1 gs2) && (Lib.is_permutation Strand.equal ss1 ss2) in
  (* All single tau reactions possible from a gate. The strand list is to accumulate displaced strands. *)
  let next_steps (g:t) =
    let disp_reacts = displacement_reactions opts g in
    let displacements = List.map (fun (_,_,gs',ss') -> ((List.map (standard_form opts) gs'),ss')) disp_reacts in
    let opens = List.map (fun (_,_,gs') -> [((standard_form opts) gs')],[]) (open_migration_reactions opts g) in
    let unsticks =
     (match (Options.getRules opts) with (* Don't include unsticking reactions as fast in the "Original" merged semantics. *)
      | Options.Infinite | Options.Finite ->
          List.map (fun (_,_,gs',ss') -> ((List.map (standard_form opts) gs'),ss')) (unsticking_reactions sub_map opts g)
      | Options.Default -> []
      | Options.Detailed -> failwith "Gate.fast_reactions shouldn't be called in single step mode") in
    let unstick_opens =
     (match (Options.getRules opts) with (* Similarly don't open in all semantics *)
      | Options.Infinite | Options.Finite ->
         List.map (fun (_,_,gs',s') -> ((List.map (standard_form opts) gs'),s')) (unbind_open_reactions opts g)
      | Options.Default -> []
      | Options.Detailed -> failwith "Gate.fast_reactions shouldn't be called in single step") in
    let covers = List.map (fun (_,_,g') -> ([g'],[])) (cover_reactions g) in
    Lib.union tau_match unstick_opens
                        (Lib.union tau_match opens (Lib.union tau_match displacements (Lib.union tau_match unsticks covers)))
  in
  (* Search the tau reaction space to compute transitive closure of tau reactions. *)
  (*
  let rec search ((gs:t list),(ss:strand list)) =
    let gs = List.map (standard_form opts) gs in
    match (Lib.collect next_steps gs) with
      | [] -> [(gs,ss)]
      | xs -> let xs = List.map (fun (gs',ss') -> (gs', ss@ss')) xs in
              Lib.collect_union tau_match search xs
  in
  *)
  let rec search ((gs:t list),(ss:strand list)) =
    let gs = List.map (standard_form opts) gs in
    let fold (nexts, stills, progress) g =
      match next_steps g with
      | [] -> nexts, g::stills, progress
      | xs -> xs@nexts, stills, true in
    let (nexts, stills, progress) = Lib.fold_left fold ([], [], false) gs in
    if progress then
      let xs = List.map (fun (gs',ss') -> (stills@gs', ss@ss')) nexts in
      Lib.collect_union tau_match search xs
    else [(gs,ss)]
  in

  let answer = search (gs,[]) in
  if answer = [(gs,[])] then [] else answer (* Remove reflexive edges from otherwise empty results. *)

(* ************************************************************************************************ *)

(** Get all "initial" reactions on a gate. *)
let initial_reactions sub_map (opts:options) (g:t) : (t list * strand list) list =
  match (Options.getRules opts) with
  | Options.Infinite | Options.Default -> fast_reactions sub_map opts [g]
  | _ -> []

(* ************************************************************************************************ *)

(* Get the leftmost segment of a gate. *)
let leftmost_segment (g:t) =
  match g with
  | [] | ([]::_) -> failwith "leftmost_segment: illegal gate structure"
  | ((c::_)::_) -> c

(* Get the rightmost segment of a gate. *)
let rightmost_segment (g:t) = Segment.reverse(leftmost_segment (reverse g))

(* Is a gate "reactive"? *)
let is_reactive sub_map (opts:options) (g:t) =
  (* Scan the syntax of the gate for available toeholds on the main backbone. *)
  (* NB Need to update to check for any available toehold, since reactions will potentially occur on any open hang *)
  let rec scan (very_first_segment:bool) (lhs:t) (acc:Segment.t list) (gs:Segment.t list) (rhs:t) =
    match gs with
    | [] -> (match rhs with [] -> false | (gs'::rhs') -> scan very_first_segment (if List.isEmpty acc then lhs else lhs@[acc]) [] gs' rhs')
    | (c::gs') -> if (very_first_segment && (Domain.contains_toehold(Segment.bottom_left_overhang c) || Domain.contains_toehold(Segment.top_left_overhang c))) then true
                  else match gs',rhs with
                         | [],[] -> if Domain.contains_toehold(Segment.bottom_right_overhang c) then true else Domain.contains_toehold(Segment.top_right_overhang c)
                         | [],rhs -> if Domain.contains_toehold(Segment.top_right_overhang c) then true else scan false lhs (acc@[c]) [] rhs
                         | gs',rhs -> if Domain.contains_toehold(Segment.bottom_right_overhang c) then true else scan false lhs (acc@[c]) gs' rhs
  in
  (* Check whether unary reactions are possible from the gate. *)
  let unary_reaction_possible (opts:options) (g:t) =
    (match unsticking_reactions sub_map opts g with [] -> false | _ -> true) ||
    (match displacement_reactions opts g with [] -> false | _ -> true) ||
    (match Options.getRules opts with (* ??? Is this correct? ??? *)
      | Options.Detailed -> (match migration_reactions g with [] -> false | _ -> true)
      | _ -> false) ||
    (match cover_reactions g with [] -> false | _ -> true)
  in
  (scan true [] [] [] g) || (unary_reaction_possible opts g)

let is_reactive_complex (_:options) (c:t_complex) =
  let rec reactive_toe_holds = function
     | Segment(s) -> Segment.is_reactive s
     | Sequence(_,c,rst) -> reactive_toe_holds c || reactive_toe_holds rst
  in
  (*Check for unary reactions on the complex, based on the semantics *)
  (* ADD THIS AFTER THE REACTION GENS ARE CONVERTED *)
  let unary_reaction_check (_:t_complex) = false in
  reactive_toe_holds c || unary_reaction_check c

(* ************************************************************************************************ *)
let universal_counters (g:t) = List.map (List.map Segment.universal_counters) g



(*  FD Additions *)
//let getEnergy (gate : t) =
//    SequenceCalc.getGateEnergy(gate)
    
    


