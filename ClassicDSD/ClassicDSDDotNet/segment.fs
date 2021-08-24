// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Segment
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open System.Diagnostics

(* A segment can be a lower toehold strand or a double-strand *)
type value = Value.t
type value_env = Value.t_env
type domain = Domain.t
type strand = Strand.t

(* A segment is either a double strand with overhangs, written {LB}<LT>[S]<RT>{RB},
                    or a double strand with some overhangs and a hairpin, written {OB}<OT>[S]<HP}.
   In the double-stranded portion, the "complement" flag refers to the domain in the TOP strand.
   The hairpin segment indicates whether the pin is on the right or the left with side flag
   Right hairpins bind to the top and left hairpins bind to the bottom, a mneme can be the following:
     <a b} starts out as a top strand but ends as a bottom strand
     thus the a attaches to the top and the b to the bottom, so <a b}[c] melts to <c* b a c>
   *)
type side = Left | Right
[<DebuggerDisplay("{DisplayMe()}")>] // displays a segment in debug mode
type t = 
  | Double of (domain list * domain list * domain list * domain list * domain list)
  | Hairpin of (domain list * domain list * domain list * domain list * side)
    member this.DisplayMe() = 
      let overhang (upper:bool) (s:domain list) =
        let start,stop = if upper 
                            then "<",">" 
                            else "{","}"
        (if List.isEmpty s then "" else start + Domain.display_sequence s + stop)
      match this with
      | Double(lb,lt,s,rt,rb) ->
         (* {LB}<LT>[S]<RT>{RB} *)
         (overhang false lb) + (overhang true lt) + "[" + Domain.display_sequence s + "]" + (overhang true rt) + (overhang false rb)
      | Hairpin(ob,ot,s,hp,Right) ->
         (* if {OB}<OT>[S]{HP> *)
         let hp_s = "{" + Domain.display_sequence hp + ">" in
         (overhang false ob) + (overhang true ot) + "[" + Domain.display_sequence s + "]" + hp_s
      | Hairpin(ob,ot,s,hp,Left) ->
         (* if <HP}[S]<OT>{OB} *)
         let hp_s = "<" + (Domain.display_sequence hp) + "}" in
         hp_s + "[" + Domain.display_sequence s + "]" + (overhang true ot) + (overhang false ob)

(* Access the various domain lists for a given segment. *)
(* For hairpins, overhangs that aren't present return nil *)
let bottom_left_overhang = function 
    Double(lb,_,_,_,_) -> lb
  | Hairpin(ob,_,_,_,Left) -> ob
  | Hairpin(_,_,_,_,Right) -> []    
let top_left_overhang = function
    Double(_,lt,_,_,_) -> lt
  | Hairpin(_,ot,_,_,Left) -> ot
  | Hairpin(_,_,_,_,Right) -> [] 
let double_stranded_region = function 
    Double(_,_,s,_,_) | Hairpin(_,_,s,_,_) -> s
let top_right_overhang = function
    Double(_,_,_,rt,_) -> rt
  | Hairpin(_,ot,_,_,Right) -> ot
  | Hairpin(_,_,_,_,Left) -> [] 
let bottom_right_overhang = function 
    Double(_,_,_,_,rb) -> rb
  | Hairpin(ob,_,_,_,Right) -> ob
  | Hairpin(_,_,_,_,Left) -> []    
let hairpin_region = function
    Double _ -> []
  | Hairpin(_,_,_,hp,_) -> hp

(* Produce a string representation. *)
let display seg = 
  let overhang (upper:bool) (s:domain list) =
      let start,stop = if upper then "<",">" else "{","}" in
      (if List.isEmpty s then "" else start + Domain.display_sequence s + stop)
  in
  match seg with
  | Double(lb,lt,s,rt,rb) ->
     (* {LB}<LT>[S]<RT>{RB} *)
     (overhang false lb) + (overhang true lt) + "[" + Domain.display_sequence s + "]" + (overhang true rt) + (overhang false rb)
  | Hairpin(ob,ot,s,hp,Right) ->
     (* if {OB}<OT>[S]{HP> *)
     let hp_s = "{" + Domain.display_sequence hp + ">" in
     (overhang false ob) + (overhang true ot) + "[" + Domain.display_sequence s + "]" + hp_s
  | Hairpin(ob,ot,s,hp,Left) ->
     (* if <HP}[S]<OT>{OB} *)
     let hp_s = "<" + (Domain.display_sequence hp) + "}" in
     hp_s + "[" + Domain.display_sequence s + "]" + (overhang true ot) + (overhang false ob)

let gotten_tag_sets get = function
  | Double(lb,lt,_,rt,rb) -> get (lb@lt), get (rb@rt)
  | Hairpin(ob,ot,_,_,sd) ->
    let a, b = Domain.all_tags_list (ob@ot), None in
    match sd with
    | Right -> a, b
    | Left  -> b, a

let tag_sets d = gotten_tag_sets Domain.all_tags_list d

let tethered_tag_sets d = gotten_tag_sets Domain.all_tethered_tags_list d

let set_tags c ts =
  let set = Domain.set_tags_list ts in
  match c with
  | Double(lb,lt,s,rt,rb) -> Double(set lb, set lt, set s, set rt, set rb)
  | Hairpin(ob,ot,s,hp,sd) -> Hairpin(set ob, set ot, set s, set hp, sd)

let add_tags c ts =
  let add = Domain.add_tags_list ts in
  match c with
  | Double(lb,lt,s,rt,rb) -> Double(add lb, add lt, add s, add rt, add rb)
  | Hairpin(ob,ot,s,hp,sd) -> Hairpin(add ob, add ot, add s, add hp, sd)
 
(* Evaluate the values contained in a segment *)
let eval (ev:value_env) = function
   Double(lb,lt,s,rt,rb) ->
     let evalS = fun ds -> Domain.eval_sequence ev ds in 
      Double(evalS lb, evalS lt, evalS s, evalS rt, evalS rb)
 | Hairpin(ob,ot,s,hp,sd) ->
     let evalS = fun ds -> Domain.eval_sequence ev ds in 
     Hairpin( evalS ob, evalS ot, evalS s, evalS hp, sd)

(* Mirror a segment. *)
(* mirror <1 2 3>[4 5 6]{7 8 9} = {1 2 3}[4* 5* 6*]<7 8 9> *)
(* mirror <1 2 3>[4 5 6]{7 8 9> = {1 2 3}[4* 5* 6*]{9 8 7> *)
(* mirror <1 2 3}[4 5 6]{7 8 9} = <3 2 1}[4* 5* 6*]<7 8 9> *)
let mirror = function
    Double(lb,lt,s,rt,rb) -> 
       Double(lt, lb, List.map Domain.complement s, rb, rt)
  | Hairpin(ob,ot,s,hp,sd) -> Hairpin(ot, ob, List.map Domain.complement s,List.rev hp, sd)

(* reverse <1 2 3>[4 5 6]<7 8 9> = <9 8 7>[6 5 4]<3 2 1> *)
(* reverse <1 2 3>[4 5 6]{7 8 9> = <9 8 7}[6 5 4]<3 2 1> *)
let reverse = function
    Double(lb,lt,s,rt,rb) -> Double(List.rev rb, List.rev rt, List.rev s, List.rev lt, List.rev lb) 
  | Hairpin(ob,ot,s,hp,Left) -> Hairpin(List.rev ob, List.rev ot, List.rev s, List.rev hp, Right)
  | Hairpin(ob,ot,s,hp,Right) -> Hairpin(List.rev ob, List.rev ot, List.rev s, List.rev hp, Left)

(* Physically rotate a segment *)
let rotate s = reverse (mirror s) 

(* Compare the components of two segments for equality, without rotation *)
let equals seg1 seg2 =
  match(seg1,seg2) with
  | Double(lb,lt,s,rt,rb),Double(lb',lt',s',rt',rb') -> lb=lb' && lt=lt' && s=s' && rt=rt' && rb=rb'
  | Hairpin(ob,ot,s,hp,sd),Hairpin(ob',ot',s',hp',sd') -> 
            ob=ob' && ot=ot' && s=s' && sd=sd' && hp=hp'
  | _ -> false

(* Provide an ordering between two segments for sorting, -1 when seg1 < seg2, 0 when seg1 = seg2, 1 when seg1 < seg2
   Hairpins are arbitrarily less than Doubles, pins on the left are arbitrarily less than pins on the right
   Otherwise, from left to right following the lists compare based on length of list, then lexicographically and length on domains
   Short domains are arbitrarily less than long domains
 *)
let compare seg1 seg2 = 
  let domains_check (l1 : domain list) (l2 : domain list) = 
       let len1, len2 = List.length l1, List.length l2 in
       if len1 < len2 then -1
       else if (len1 = len2) 
               then Lib.fold_right (fun (d1,d2) eq -> if eq=0 then compare d1 d2 else eq) (Lib.zip l1 l2) 0
            else 1
  in
  let rec sets_check lls1 lls2 =
     match lls1,lls2 with
       | [],[] -> 0
       | l1::lls1,l2::lls2 -> let t_ans = domains_check l1 l2 in
                              if t_ans = 0 then sets_check lls1 lls2 else t_ans
       | _ -> failwith "sets_check given unequal lists"
  in
  match(seg1,seg2) with
  | Double(lb,lt,s,rt,rb),Double(lb2,lt2,s2,rt2,rb2) -> sets_check [lb;lt;s;rt;rb] [lb2;lt2;s2;rt2;rb2]
  | Hairpin(ob,ot,s,hp,sd),Hairpin(ob2,ot2,s2,hp2,sd2) ->
    (match sd,sd2 with
     | Left,Right -> -1
     | Right,Left -> 1
     | Left,Left -> sets_check [hp;s;ot;ob] [hp2;s2;ot2;ob2]
     | Right,Right -> sets_check [ob;ot;s;hp] [ob2;ot2;s2;hp2])
  | Double _, Hairpin _ -> 1
  | Hairpin _, Double _ -> -1

(* Compute free names of a segment. *)
let free_names = function
    Double(lb,lt,s,rt,rb) -> Lib.collect Domain.free_names (lb@lt@s@rt@rb)
  | Hairpin(ob,ot,s,hp,_) -> Lib.collect Domain.free_names (ob@ot@s@hp)

(* Return all domains that appear in the segment. *)
let domains = function
    Double(lb,lt,s,rt,rb) -> lb@lt@s@rt@rb
  | Hairpin(ob,ot,s,hp,_) -> ob@ot@s@hp

(* A segment is reactive if contains any exposed toeholds, not on the hairpin *)
let is_reactive = function
   | Double(lb,lt,_,rt,rb) -> Domain.contains_toehold (lb@lt@rt@rb)
   | Hairpin(ob,ot,_,_,_) -> Domain.contains_toehold (ob@ot)

(* Return all tethered domains (separating bonded domains that are both tethered) that appear in the segment. *)
let tethered_domains seg =
  let tethers ds = List.filter Domain.is_tethered ds in
  match seg with
    Double(lb,lt,s,rt,rb) -> (tethers lb)@(tethers lt)@(tethers s)@(tethers rt)@(tethers rb)
  | Hairpin(ob,ot,s,_,_) -> (tethers ob)@(tethers ot)@(tethers s)

(** Check type of a segment. *)
let inferType in_origami (env:Types.type_env) = function
    Double(lb,lt,s,rt,rb) -> List.iter (Domain.inferType in_origami env) (lb@lt@s@rt@rb)
  | Hairpin(ob,ot,s,hp,_) -> List.iter (Domain.inferType in_origami env) (ob@ot@s@hp)

(** Get the position of the "first" domain in a segment for error reporting **)
let getPosn seg =
  Domain.getPosn (List.head (match seg with
                            | Double(lb,lt,s,_,_) -> lb@lt@s
                            | Hairpin(_,_,_,hp,Left) -> hp
                            | Hairpin(ob,ot,s,_,Right) -> ob@ot@s))

(** Erase positions (set them all to default value) throughout. *)
let erasePosns seg = 
    let f = List.map Domain.erasePosns in 
    match seg with 
      Double(lb,lt,s,rt,rb) -> Double(f lb, f lt, f s, f rt, f rb)
    | Hairpin(ob,ot,s,hp,sd) -> Hairpin(f ob, f ot, f s, f hp, sd)

(** Replace positions consistently throughout. *)
let replacePosns (mrs:Types.range Stringmap.t) (seg:t) =
    let f = List.map (Domain.replacePosns mrs) in 
    match seg with 
      Double(lb,lt,s,rt,rb) -> Double(f lb, f lt, f s, f rt, f rb)
    | Hairpin(ob,ot,s,hp,sd) -> Hairpin(f ob, f ot, f s, f hp, sd)

(* Does segment s' match the "pattern" s? *)
let matches seg seg' = 
  match seg,seg' with
    Double(lb,lt,s,rt,rb), Double(lb',lt',s',rt',rb') ->
      (Domain.matches_list lb lb') && (Domain.matches_list lt lt') && (Domain.matches_list s s') &&
      (Domain.matches_list rt rt') && (Domain.matches_list rb rb')
  | Hairpin(ob,ot,s,hp,sd), Hairpin(ob',ot',s',hp',sd') ->
      (Domain.matches_list ob ob') && (Domain.matches_list ot ot') && (Domain.matches_list s s') &&
      (Domain.matches_list hp hp') && sd=sd'
  | _ -> false

let matches_env env seg seg' = 
  match seg,seg' with
    Double(lb,lt,s,rt,rb), Double(lb',lt',s',rt',rb') ->
      Lib.option_and env
        [ (fun e -> Domain.matches_list_env e lb lb')
        ; (fun e -> Domain.matches_list_env e lt lt')
        ; (fun e -> Domain.matches_list_env e s s')
        ; (fun e -> Domain.matches_list_env e rt rt')
        ; (fun e -> Domain.matches_list_env e rb rb') ]
  | Hairpin(ob,ot,s,hp,sd), Hairpin(_,ot',s',hp',sd') ->
      if sd=sd' then
        Lib.option_and env
          [ (fun e -> Domain.matches_list_env e ob ot')
          ; (fun e -> Domain.matches_list_env e ot ot')
          ; (fun e -> Domain.matches_list_env e s s')
          ; (fun e -> Domain.matches_list_env e hp hp') ]
      else None        
  | _,_ -> None

let has_wildcard = function
  | Double(lb,lt,s,rt,rb) -> List.exists (List.exists Domain.is_wildcard) [lb; lt; s; rt; rb]
  | Hairpin(ob,ot,s,hp,_) -> List.exists (List.exists Domain.is_wildcard) [ob; ot; s; hp]

(* Get the list of all NON-TOEHOLD domains exposed in the segment. *)
let exposed_nontoeholds = function
    Double(lb,lt,_,rt,rb) -> List.filter (Domain.is_toehold >> not) (lb@lt@rt@rb)
  | Hairpin(ob,ot,_,_,_) -> List.filter (Domain.is_toehold >> not) (ob@ot)

(** Look for exposed neighbouring toeholds. *)
let neighbouring_toeholds = function
    Double(lb,lt,_,rt,rb) -> Lib.lookForSome Domain.neighbouring_toeholds [lb;lt;rt;rb]
  | Hairpin(ob,ot,_,_,_) -> Lib.lookForSome Domain.neighbouring_toeholds [ob;ot]

(* Does the segment contain an available lower toehold? *)
let lower_toehold_available = function
    Double(lb,_,_,_,rb) -> (Domain.contains_toehold lb) || (Domain.contains_toehold rb)
  | Hairpin(ob,_,_,_,_) -> Domain.contains_toehold ob

(* Does the segment contain an available upper toehold? *)
let upper_toehold_available = function
    Double(_,lt,_,rt,_) -> (Domain.contains_toehold lt) || (Domain.contains_toehold rt)
  | Hairpin(_,ot,_,_,_) -> (Domain.contains_toehold ot)

(***************************************************************************************************)
(* COVER REACTIONS on a single segment. Zero or one reactions may occur. *)

(** Cover a single exposed toehold to the right within a segment. *)
(* <l>[s]<n^ r> : n^ -> <l>[s n^]<r> *)
let cover_right (c:t) =
  match c with
  | Double(lb,lt,s,((Domain.Toe _  as n)::rt),((Domain.Toe _  as n')::rb)) ->
      if (Domain.are_complements n n') then 
        let n'' = Domain.stick n n' in Some(Double(lb,lt,s@[(n'')],rt,rb), n'')
      else None
  | Hairpin(((Domain.Toe _  as n)::ob),((Domain.Toe _ as n')::ot),s,hp,Left) -> 
     if (Domain.are_complements n n') then Some(Hairpin(ob,ot,s@[(Domain.stick n' n)],hp,Left), n)
     else None
  | _ -> None
  
(** Cover a single exposed toehold to the left within a segment. *)
(* n^ : <l n^>[s]<r> -> <l>[n^ s]<r> *)
let cover_left (c:t) =
  match c with
  | Double(lb,lt,s,rt,rb) ->
      (match (Lib.get_last_element lb),(Lib.get_last_element lt) with
        | Some(lb,(Domain.Toe _  as n')),Some(lt,(Domain.Toe _ as n)) ->
            if (Domain.are_complements n n') then 
              let n'' = Domain.stick n n' in Some(Double(lb,lt,(n'')::s,rt,rb), n'')
            else None
        | _ -> None)
  | Hairpin(ob,ot,s,hp,Right) -> 
     (match (Lib.get_last_element ob),(Lib.get_last_element ot) with
       | Some(ob,(Domain.Toe _ as n')),Some(ot,(Domain.Toe _ as n)) ->
           if (Domain.are_complements n n') then 
             let n'' = Domain.stick n n' in Some(Hairpin(ob,ot,(n'')::s,hp,Right), n'')
           else None
       | _ -> None) 
  | _ -> None

(***************************************************************************************************)
(* MIGRATION REACTIONS within a pair of segments. Zero or one reactions may occur. *)

(* Function to find common head domains on a double segment and a "strand" and swap them*)
let swap_shared_domains_head (doubled : domain list) (free : domain list) =
  let top_strand,bottom_strand = Domain.unstick_list doubled in
  let rec matches new_doubled new_free top bot free =
    match top,bot,free with
    | [],[],[] -> (new_doubled, new_free, [], [])
    | [],[],free_rest -> (new_doubled, new_free, [], free_rest)
    | top_rest,bot_rest,[] -> (new_doubled, new_free, List.map2 Domain.stick top_rest bot_rest, [])
    | (t::top_rest),(b::bot_rest),(f::free_rest) -> 
       if Domain.equal t f then matches (new_doubled@[Domain.stick f b]) (new_free@[t]) top_rest bot_rest free_rest
                           else (new_doubled, new_free, (List.map2 Domain.stick top bot), free)
    | _ -> failwith "segment.swap_shared_domains_head did not have equal sized top and bottom lists"
  in matches [] [] top_strand bottom_strand free

(* Like swap_shared_domains_head, but starting at the other end of the segments *)
let swap_shared_domains_tail (doubled: domain list) (free: domain list) =
 let shared_double, shared_free, double_rest, double_free = swap_shared_domains_head (List.rev doubled) (List.rev free) in
 List.rev shared_double, List.rev shared_free, List.rev double_rest, List.rev double_free

(** Migrate towards the right (by less than one segment). *)
(* <l1>[s1]<s2 r1> : <l2>[s2 s3]<r2> -> <l1>[s1 s2]<r1> : <l2 s2>[s3]<r2> *)
let migrate_right (cl:t) (cr:t) =
  match cl,cr with
  | Double(lb1,lt1,s1,rt1,[]),Double([],lt2,s2,rt2,rb2) -> (* Assume it's a lower join with no gap, so rb1 & lb2 must be empty. *)
    (match (swap_shared_domains_head s2 rt1) with
      | [],_,_,_ -> None (* No shared domains, second item is also [] *)
      | _,_,[],_ -> None (* Not doing a displacement, so s2_rest must be non-empty *)
      | shared_s2,shared_top,s2_rest,rt1_rest ->
        Some(Double(lb1,lt1,(s1@shared_s2),rt1_rest,[]), Double([],(lt2@shared_top),s2_rest,rt2,rb2), shared_top))
  | Hairpin([],ot,s,hp,Left),Double([],lt,s2,rt, rb) -> (* Assume lower join, hairpin on the left (not opening) *)
     (match (swap_shared_domains_head s2 ot) with
      | [],_,_,_ -> None (* No shared domains *)
      | _,_,[],_ -> None (* see above *)
      | shared_s2, shared_top, s2_rest, ot_rest ->
          Some(Hairpin([],ot_rest,(s@shared_s2),hp,Left),Double([],(lt@shared_top),s2_rest,rt,rb),shared_top))
  | Double(lb1,lt1,s1,rt1,[]),Hairpin([],ot,s2,hp,Right) -> (* Assume lower join, hairpin on the right (not opening) *)
      (match (swap_shared_domains_head s2 rt1) with
       | [],_,_,_ -> None (* No shared domains *)
       | _,_,[],_ -> None (* Not opening, so s2_rest must be non-empty *)
       | shared_s2, shared_top, s2_rest, rt1_rest ->
          Some(Double(lb1,lt1,(s1@shared_s2), rt1_rest,[]), Hairpin([],ot@shared_top,s2_rest,hp,Right), shared_top))
  | Hairpin([],ot1,s1,hp1,Left),Hairpin([],ot2,s2,hp2,Right) ->
     (match (swap_shared_domains_head s2 ot1) with
       | [],_,_,_ -> None
       | _,_,[],_ -> None (* Not opening, so s2_rest must be non-empty *)
       | shared_s2, shared_top, s2_rest, ot1_rest ->
          Some(Hairpin([],ot1_rest,s1@shared_s2,hp1,Left),Hairpin([],ot2@shared_top,s2_rest,hp2,Right),shared_top))
  | _,_ -> None

(** Migrate towards the left (by less than one segment). Zero or one reactions may occur. *)
(* <l1>[s1 s2]<r1> : <l2 s2>[s3]<r2> -> <l1>[s1]<s2 r1> : <l2>[s2 s3]<r2> *)
(* {lb1}<lt1>[s1]<rt1> : <lt2>[s2]<rt2>{rb2} *)
let migrate_left (cl:t) (cr:t) =
  match cl,cr with
  | Double(lb1,lt1,s1,rt1,[]),Double([],lt2,s2,rt2,rb2) -> (* Assume it's a lower join with no gap, so rb1 & lb2 must be empty. *)
    (match (swap_shared_domains_tail s1 lt2) with
     | [],_,_,_ -> None (* No shared domains, second item is [] *)
     | _,_,[],_ -> None (* Not displacing, so s1_rest must be non-empty *)
     | shared_s1,shared_top,s1_rest,lt2_rest ->
       Some(Double(lb1,lt1,s1_rest,(shared_top@rt1),[]),Double([],lt2_rest,(shared_s1@s2),rt2,rb2),shared_top))
  | Hairpin([],ot,s1,hp,Left),Double([],lt2,s2,rt2,rb2) ->
     (match (swap_shared_domains_tail s1 lt2) with
       | [],_,_,_ -> None
       | _,_,[],_ -> None (* Not doing a migrate open, so s1_rest must be non-empty *)
       | shared_s1,shared_top,s1_rest,lt2_rest ->
         Some(Hairpin([],shared_top@ot,s1_rest,hp,Left),Double([],lt2_rest,(shared_s1@s2),rt2,rb2),shared_top))
  | Double(lb1,lt1,s1,rt1,[]),Hairpin([],ot,s2,hp,Right) ->
    (match (swap_shared_domains_tail s1 ot) with
      | [],_,_,_ -> None
      | _,_,[],_ -> None (* Not a displace *)
      | shared_s1,shared_top,s1_rest,ot_rest -> 
        Some(Double(lb1,lt1,s1_rest,shared_top@rt1,[]),Hairpin([],ot_rest,(shared_s1@s2),hp,Right),shared_top))
  | Hairpin([],ot1,s1,hp,Left),Hairpin([],ot2,s2,hp2,Right) ->
    (match (swap_shared_domains_tail s1 ot2) with
      | [],_,_,_ -> None
      | _,_,[],_ -> None (* Not a migrate open *)
      | shared_s1,shared_top, s1_rest, ot2_rest -> 
        Some(Hairpin([],shared_top@ot1,s1_rest,hp,Left),Hairpin([],ot2_rest,(shared_s1@s2),hp2,Right),shared_top))
  | _,_ -> None

(* Simple migration functions - don't care if it succeeds or fails.
 * These just return the two segments after any reaction, i.e. they may be unchanged. *)
let migrate_left_simple ((c:t),(c':t)) = match migrate_left c c' with Some(c1,c1',_) -> c1,c1' | None -> (c,c')
let migrate_right_simple ((c:t),(c':t)) = match migrate_right c c' with Some(c1,c1',_) -> c1,c1' | None -> (c,c')

(** Migrate towards the right (by less than one segment), opening a hairpin *)
(* <l1>[s1]<s2 s3 r1> : <l2>[s2 s3]{hp> -> <l1>[s1 s2 s3]<r1>{hp s3 s2 l2} *)
let migrate_open_right (c:t) (c':t) =
  match c,c' with
  | Double(lb1,lt1,s1,rt1,[]),Hairpin([],ot,s2,hp,Right) -> (* Assume lower join, hairpin on the right, opening *)
    (match (swap_shared_domains_head s2 rt1) with
      | [],_,_,_ -> None (* No shared domains, second item is [] *)
      | shared_s2,shared_top,[],rt1_rest ->
        Some(Double(lb1,lt1,s1@shared_s2,rt1_rest,hp@(List.rev shared_top)@(List.rev ot)),shared_top)
      | _,_,_,_ -> None) (* s2_rest must be empty to open *)
  | Hairpin([],ot1,s1,hp1,Left),Hairpin([],ot2,s2,hp2,Right) ->
     (match (swap_shared_domains_head s2 ot1) with
       | [],_,_,_ -> None
       | shared_s2,shared_top, [], ot1_rest -> 
         Some(Hairpin(hp2@List.rev(shared_top)@List.rev(ot2), ot1_rest, s1@shared_s2,hp1,Left), shared_top)
       | _,_,_,_ -> None) (* Not a migrate *)
   | _,_ -> None

(** Migrate towards the left (by less than one segment), opening a hairpin *)
(* <hp}[s1]<r1> : <l2 s1>[s2]<r2> -> <l2>{r1 s2 s1 hp}[s1 s2]<r1> *)
let migrate_open_left (c:t) (c':t) =
  match c,c' with
  | Hairpin([],ot,s1,hp,Left),Double([],lt2,s2,rt2,rb2) ->
     (match (swap_shared_domains_tail s1 lt2) with
       | [],_,_,_ -> None (*No shared domains*)
       | shared_s1,shared_top,[],lt2_rest -> 
         Some(Double(List.rev(ot)@List.rev(shared_top)@hp,lt2_rest,shared_s1@s2,rt2,rb2), shared_top)
       | _,_,_,_ -> None)
  | Hairpin([],_,s1,hp,Left),Hairpin([],ot2,s2,hp2,Right) ->
    (match (swap_shared_domains_tail s1 ot2) with
      | [],_,_,_ -> None
      | shared_s1,shared_top, [], ot2_rest -> 
        Some(Hairpin(List.rev(ot2)@List.rev(shared_top)@hp,ot2_rest,shared_s1@s2,hp2,Right), shared_top)
      | _,_,_,_ -> None)
   | _,_ -> None

(***************************************************************************************************)
(* DISPLACMENT REACTIONS involving a single segment. *)

(** Displace a single strand to the right. Zero or one reactions may occur. *)
(* <l1>[s1]<s2 r1> | <l2>[s2]<r2> -> <l2 s2 r2> | <l1>[s1 s2]<r1> *)
let displace_right (cl:t) (cr:t) =
  match cl,cr with 
  | Double(lb1,lt1,s1,rt1,[]),Double([],lt2,s2,rt2,rb2) -> (* Assume it's a lower join with no gap, so rb1 & lb2 must be empty. *)
    (match (swap_shared_domains_head s2 rt1) with
     | [],_,_,_ -> None (* No shared domains, second item is [] *)
     | shared_s2,shared_top,[],rt1_rest -> 
       Some(Double(lb1,lt1,(s1@shared_s2),rt1_rest,rb2), Strand.Upper(lt2@shared_top@rt2), s2)
     | _,_,_,_ -> None) (* To displace, s2_rest must be empty. *)
  | Hairpin([],ot,s,hp,Left),Double([],lt,s2,rt, rb) -> (* Assume lower join, hairpin on the left *)
     (match (swap_shared_domains_head s2 ot) with
      | [],_,_,_ -> None
      | shared_s2,shared_top,[], ot_rest -> 
        Some(Hairpin(rb,ot_rest,(s@shared_s2),hp,Left), Strand.Upper(lt@shared_top@rt), s2)
      | _,_,_,_ -> None) (* A displacement, so s2_rest empty *)
  | _,_ -> None

(** Displace a single strand to the left. Zero or one reactions may occur. *)
(* <l1>[s1]<r1> | <l2 s1>[s2]<r2>  -> <l1 s1 r1> | <l2>[s1 s2]<r2> *)
let displace_left (cl:t) (cr:t) =
  match cl,cr with 
  | Double(lb1,lt1,s1,rt1,[]),Double([],lt2,s2,rt2,rb2) -> (* Assume it's a lower join with no gap, so rb1 & lb2 must be empty. *)
     (match (swap_shared_domains_tail s1 lt2) with
      | [],_,_,_ -> None
      | shared_s1,shared_top,[],lt2_rest -> Some(Double(lb1,lt2_rest,(shared_s1@s2),rt2,rb2), Strand.Upper(lt1@shared_top@rt1), s1)
      | _,_,_,_ -> None) (* We're doing a displacement so s1_rest must be empty. *)
  | Double(lb1,lt1,s1,rt1,[]),Hairpin([],ot,s2,hp,Right) ->
    (match (swap_shared_domains_tail s1 ot) with
      | [],_,_,_ -> None
      | shared_s1,shared_top,[],ot_rest -> Some(Hairpin(lb1, ot_rest, (shared_s1@s2), hp, Right), Strand.Upper(lt1@shared_top@rt1), s1)
      | _,_,_,_ -> None)
  | _,_ -> None

(***************************************************************************************************)
(* TOEHOLD BINDING AND UNBINDING REACTIONS involving a single segment (maybe also an incoming strand). *)

let stick (n,subn) (n',subn') =
  let n'' = Domain.stick n n' in
  let n'', stuck =
    match subn, subn' with
    | None, None -> (n'', false), [n'']
    | Some sub, Some sub' ->
        let tags = sub |> List.choose Domain.get_tags |> List.concat in
        let tags' = sub' |> List.choose Domain.get_tags |> List.concat in
        let common_tags = match Lib.intersection (=) tags tags' with
                          | [] -> None
                          | cs -> cs |> Lib.remove_duplicates (=) |> Some in
        let n_tagged = n'' |> Domain.set_tags common_tags in
        (n_tagged, true), List.map2 Domain.stick sub sub'
    | _ -> failwith "subdomains do not match" in
  n'', stuck

let stick_simple n n' =
  let n'' = Domain.stick n n' in
  (n'', false), [n'']

(* Possibly stick an upper strand to a gate. (Lower strand interactions are dealt with by another call following a mirror.)
   This includes the possibility of unproductive reactions, which may be ruled out in gate.ml. *)
let stick_sg sub_map (c:t) (s:strand) =
  match c,s with
  | Double(lb,lt,s,rt,rb), Strand.Upper(ns) ->
    let lhs =
      (Domain.matching_toeholds sub_map ns lb)
      |> List.map
        (fun ((nsl,n,nsr),(lbl,n',lbr)) -> let n'', stuck = stick n n'
                                           (Double(lbl,nsl,stuck,nsr,lbr), Double([],lt,s,rt,rb), n'', false))
        

    let rhs =
      (Domain.matching_toeholds sub_map ns rb)
      |> List.map
        (fun ((nsl,n,nsr),(rbl,n',rbr)) -> let n'', stuck = stick n n'
                                           (Double(lb,lt,s,rt,rbl), Double([],nsl,stuck,nsr,rbr), n'', true))
        
    lhs@rhs

  | Hairpin(ob,ot,s,hp,Right),Strand.Upper(ns) ->
    (Domain.matching_toeholds sub_map ns ob)
    |> List.map 
      (fun ((nsl,n,nsr),(obl,n',obr)) -> let n'', stuck = stick n n'
                                         (Double(obl,nsl,stuck,nsr,obr), Hairpin([],ot,s,hp,Right), n'', false))
     
  | Hairpin(ob, ot, s, hp, Left), Strand.Upper(ns) ->

    (Domain.matching_toeholds sub_map ns ob)
    |> List.map
      (fun ((nsl,n,nsr),(obl,n',obr)) -> let n'', stuck = stick n n'
                                         (Hairpin(obl,ot,s,hp,Left), Double([],nsl,stuck,nsr,obr), n'', true))

  | _,_ -> []

(* Possibly stick two strands together. *)
let stick_ss sub_map (s:strand) (s':strand) =
  match s,s' with
  | Strand.Upper(ns), Strand.Lower(ms)
  | Strand.Lower(ms), Strand.Upper(ns) ->
    (Domain.matching_toeholds sub_map ns ms)
    |> List.map
      (fun ((nsl,n,nsr),(msl,n',msr)) -> let n'', stuck = stick n n'
                                         (Double(msl,nsl,stuck,nsr,msr), n''))
      
  | _,_ -> []

(* Possible stick two gates together using their extreme segments. *)
let stick_gg sub_map (cl:t) (cr:t) =
  match cl,cr with
  (* Get at the various parts of the segments. *)
  | Double(lb1,lt1,s1,rt1,rb1), Double(lb2,lt2,s2,rt2,rb2) -> 
    (* There are two possible interaction modes. *)
    let c1t_c2b =
      Domain.matching_toeholds sub_map rt1 lb2
      |> List.map
        (fun ((rt1l,n,rt1r),(lb2l,n',lb2r)) ->
            let n'', stuck = stick n n'
            (Double(lb1,lt1,s1,[],rb1), Double(lb2l,rt1l,stuck,rt1r,lb2r), Double([],lt2,s2,rt2,rb2), n'', true))
           
    let c1b_c2t =
      Domain.matching_toeholds sub_map lt2 rb1
      |> List.map
        (fun ((lt2l,n,lt2r),(rb1l,n',rb1r)) ->
            let n'', stuck = stick n n'
            (Double(lb1,lt1,s1,rt1,[]), Double(rb1l,lt2l,stuck,lt2r,rb1r), Double(lb2,[],s2,rt2,rb2), n'', false))
           
    c1t_c2b@c1b_c2t

  | Double(lb1,lt1,s1,rt1,rb1), Hairpin(ob,ot,s2,hp,Right) -> 
    let c1t_c2b =
      Domain.matching_toeholds sub_map rt1 ob
      |> List.map
        (fun ((rt1l,n,rt1r),(obl,n',obr)) -> 
          let n'', stuck = stick n n'
          (Double(lb1,lt1,s1,[],rb1), Double(obl,rt1l,stuck,rt1r,obr), Hairpin([],ot,s2,hp,Right), n'', true))
                
    let c1b_c2t =
      Domain.matching_toeholds sub_map rb1 ot
      |> List.map
        (fun ((rb1l,n',rb1r),(otl,n,otr)) -> 
          let n'', stuck = stick n n'
          (Double(lb1,lt1,s1,rt1,[]), Double(rb1l,otl,stuck,otr,rb1r), Hairpin(ob,[],s2,hp,Right), n'', false))
                
    c1t_c2b@c1b_c2t

  | Hairpin(ob,ot,s1,hp,Left), Double(lb2,lt2,s2,rt2,rb2) -> 
    let c1t_c2b =
      Domain.matching_toeholds sub_map ot lb2
      |> List.map
        (fun ((otl,n,otr),(lb2l,n',lb2r)) -> 
          let n'', stuck = stick n n'
          (Hairpin(ob,[],s1,hp,Left), Double(lb2l,otl,stuck,otr,lb2r), Double([],lt2,s2,rt2,rb2), n'', true))
                
    let c1b_c2t =
      Domain.matching_toeholds sub_map ob lt2
      |> List.map
        (fun ((obl,n',obr),(l2tl,n,lt2r)) -> 
          let n'', stuck = stick n n'
          (Hairpin([],ot,s1,hp,Left), Double(obl,l2tl,stuck,lt2r,obr), Double(lb2,[],s2,rt2,rb2), n'', false))

    c1t_c2b@c1b_c2t

  | Hairpin(ob1,ot1,s1,hp1,Left), Hairpin(ob2,ot2,s2,hp2,Right) -> 
    let c1t_c2b =
      Domain.matching_toeholds sub_map ot1 ob2
      |> List.map
        (fun ((ot1l,n,ot1r),(ob2l,n',ob2r)) -> 
          let n'', stuck = stick n n'
          (Hairpin(ob1,[],s1,hp1,Left), Double(ob2l,ot1l,stuck,ot1r,ob2r), Hairpin([],ot2,s2,hp2,Right), n'', true))

    let c1b_c2t =
      Domain.matching_toeholds sub_map ob1 ot2
      |> List.map
        (fun ((ob1l,n',ob1r),(o2tl,n,ot2r)) -> 
          let n'', stuck = stick n n'
          (Hairpin([],ot1,s1,hp1,Left), Double(ob1l,o2tl,stuck,ot2r,ob1r), Hairpin(ob2,[],s2,hp2,Right), n'', false))
                
    c1t_c2b@c1b_c2t

  | _,_ -> []

let swap_shared_domains_middle s t =
  let rec swaps pre = function
    | [] -> []
    | ((d::post) as ds) ->
      (match swap_shared_domains_head s ds with
       | [],_,_,_ -> swaps (d::pre) post
       | ss,st,sr,tr -> (pre,ss,st,sr,tr)::swaps (d::pre) post) in
  match t with
  | [] -> []
  | [_] -> []
  | d::ds -> swaps [d] ds

let close_lower_left_migrate = function
| Double ([],_,_,_,_) -> []
| Hairpin ([],_,_,_,_) -> []
| Double (lb,lt,s,rt,rb) ->
  let t = lb |> List.rev in
  swap_shared_domains_middle s t
    |> List.collect (function
                     | _,_,_,[],_ -> [] (* Not doing a displacement, so sr must be non-empty *)
                     | tl,ss,st,sr,tr -> [ tl
                                         , Hairpin ([],tr,ss,tl,Left)
                                         , Double ([],lt@ss,sr,rt,rb)
                                         , st ])
| Hairpin(ob,ot,s,hp,Right) ->
  let t = ob |> List.rev in
  swap_shared_domains_middle s t
    |> List.collect (function
                     | _,_,_,[],_ -> [] (* Not doing a displacement, so sr must be non-empty *)
                     | tl,ss,st,sr,tr -> [ tl
                                         , Hairpin ([],tr,ss,tl,Left)
                                         , Hairpin ([],ot@ss,sr,hp,Right)
                                         , st ])
| _ -> []

let close_lower_left_displace = function
| Double ([],_,_,_,_) -> []
| Double (lb,lt,s,rt,rb) ->
  let t = lb |> List.rev in
  swap_shared_domains_middle s t
    |> List.collect (function
                     | tl,ss,st,[],tr -> [ tl
                                         , Hairpin (rb,tr,ss,tl,Left) (* tl is guaranteed to be non-empty by swap_shared_domains_middle *)
                                         , Strand.Upper (lt@st@rt)
                                         , st ]
                     | _ -> []) (* Doing a displacement, so sr must be empty *)
| _ -> []

let close_lower_left_open = function
| Hairpin ([],_,_,_,_) -> []
| Hairpin(ob,ot,s,hp,Right) ->
  let t = ob |> List.rev in
  swap_shared_domains_middle s t
    |> List.collect (function
                     | _,_,_,[],_ -> [] (* Not doing a displacement, so sr must be non-empty *)
                     | tl,ss,st,_,tr -> [ tl
                                         , Hairpin (hp@(List.rev s)@(List.rev ot),tr,ss,tl,Left)
                                         , st ])
| _ -> []


(* Possibly unstick a strand from a segment. *)
let unstick (sub_map: Domain.sub_map) (c:t) =
  match c with
  | Double(lb,lt,[((Domain.Toe _) as n)],rt,rb) -> let upper,lower = Domain.unstick n in
                                                   Some(Strand.Lower(lb@[lower]@rb), Strand.Upper(lt@[upper]@rt), n)
  //(*
  | Double(lb,lt,s,rt,rb) ->
      let s_names = List.map Domain.get_name s in
      (match Hashtable.tryFind sub_map s_names with
      | None -> None
      | Some d ->
        if Domain.is_toehold d then
          let upper,lower = List.unzip (List.map Domain.unstick s) in
          Some(Strand.Lower(lb@lower@rb), Strand.Upper(lt@upper@rt), d)
        else None
      )
  //*)
  | _ -> None

(* Possibly create a hairpin from a strand with complementing toeholds; may be unproductive *)
let pin_strand (s:strand) =
  match s with 
  | Strand.Upper(doms) -> List.map (fun (lft, n', hp, n, rght) -> 
      let n'', stuck = stick_simple n' n in (Hairpin(rght,lft,stuck,hp,Right),n''))
                                   (Domain.internal_complements doms)
  | Strand.Lower(doms) -> List.map (fun (lft, n, hp, n', rght) -> 
      let n'', stuck = stick_simple n n' in (Hairpin(rght,lft,stuck,hp,Left),n''))
                                   (Domain.internal_complements doms)

(* Possibly create hairpins from lower overhangs; from upper overhangs will be handled after a rotation *)
let pin_segment (c:t) =
  match c with
  | Double(lb,lt,s,rt,rb) -> 
    (* Pinned on the lower left overhang *)
    (List.map (fun (lft,n,hp,n',rght) -> 
      let n'', stuck = stick_simple n n' in (Hairpin(rght,lft,stuck,hp,Left),Double([],lt,s,rt,rb),n'',false))
              (Domain.internal_complements lb)) @
    (* Pinned on the lower right overhang *)
    (List.map (fun (lft,n',hp,n,rght) -> 
      let n'', stuck = stick_simple n n' in (Double(lb,lt,s,rt,[]),Hairpin(lft,(List.rev(rght)),stuck,hp,Right),n'',true))
              (Domain.internal_complements rb))
  | Hairpin(ob,ot,s,_,Left) -> 
    (List.map (fun (lft,n',hp,n,rght) -> 
      let n'', stuck = stick_simple n n' in (Hairpin([],ot,s,hp,Left),Hairpin(lft,List.rev(rght),stuck,hp,Right),n'',true))
              (Domain.internal_complements ob))
  | Hairpin(ob,ot,s,_,Right) -> 
    (List.map (fun (lft,n,hp,n',rght) -> 
      let n'', stuck = stick_simple n n' in (Hairpin(rght,lft,stuck,hp,Left),Hairpin([],ot,s,hp,Right),n'',false))
              (Domain.internal_complements ob))

(* Possibly open a left hairpin into a lower strand *)
let unbind_open_up (c:t) =
  match c with
  | Hairpin(ob,ot,[(Domain.Toe _ as t)],hp,Left) -> 
    let t,t' = Domain.unstick t in
    let flip_ot = ot |> List.rev in
    Some(Strand.Lower(flip_ot@[t]@hp@[t']@ob),t)
  | _ -> None

(* Possibly open a left hairpin into an upper strand *)
let unbind_open_down (c:t) =
  match c with
  | Hairpin(ob,ot,[(Domain.Toe _ as t)],hp,Left) -> 
    let t,t' = Domain.unstick t in
    let flip_ob = ob |> List.rev in
    Some(Strand.Upper(flip_ob@[t']@(List.rev hp)@[t]@ot),t)
  | _ -> None

 (* Possibly open up a left hairpin next to another segment,
    incorporating the lower strand onto the lower overhang of the segment 
    (right hairpin handled via rotation) *)
let unbind_open_gg_lcon (c1:t) (c2:t) =
  match c1,c2 with
  | Hairpin (_,_,_,_,Left) , Double(lb,lt,s,rt,rb) -> 
    (match unbind_open_up c1 with
      | None -> None
      | Some(Strand.Lower(ds),t) -> Some(Double(ds@lb,lt,s,rt,rb),t)
      | _ -> failwith "unbind_open_up only returns lower strands")
  | _,_ -> None

 (* Possibly open down a left hairpin next to another segment,
    incorporating the upper strand onto the upper overhang of the segment 
    (right hairpin handled via rotation) *)
let unbind_open_gg_ucon (c1:t) (c2:t) =
  match c1,c2 with
  | Hairpin (_,_,_,_,Left) , Double(lb,lt,s,rt,rb) -> 
    (match unbind_open_down c1 with
      | None -> None
      | Some(Strand.Upper(ds),t) -> Some(Double(lb,ds@lt,s,rt,rb),t)
      | _ -> failwith "unbind_open_down only returns upper strands")
  | _,_ -> None

(***************************************************************************************************)
(* LEAK REACTIONS involving a single segment and an incoming strand. Zero or more reactions may happen. *)

(** Possibly instigate a leak reaction from a segment.
  * Assume that the caller (Gate.leak_reactions) has migrated the branches on either side
  * as far towards each other as possible towards the centre. *)
let leak_sg (g:t) (s_in:strand) = 
  (* Rule LS: <L1>[S]<R1> | <L S R> ->{l} <L>[S]<R> | <L1 S R1> *)
  let leaks_ls = 
    match g,s_in with
    | Double(lb1,lt1,s,rt1,rb1), Strand.Upper(ns) ->
       (match s with
        | [] -> failwith "Segment.leak: empty double strand"
        | [Domain.Toe _] -> [] (* Enforce that S =/= N^ (side-condition of rule LS) *)
        | _ -> let f = (fun (l,r) ->
                 let gs_out = Double(lb1,l,s,r,rb1) in
                 let s_out = Strand.mk_upper(lt1@s@rt1) in (gs_out, s_out, true))
               in List.map f (Lib.split_around_sublist s ns))
    | _ -> []

  (* Rule LR: <L1>[S N^]<R1> | <L S R> ->{l}  <L>[S]<R>:N^ | <L1 S N^ R1> *)
  let leaks_lr = 
    match g,s_in with
    | Double(lb1,lt1,s,rt1,rb1), Strand.Upper(ns) ->
       (match (Lib.get_last_element s) with
        | Some(s,(Domain.Toe _ as n)) when s <> [] ->
            let f = (fun (l,r) ->
              (* Enforce that R =/= N^ R' *)
              if (not(Lib.is_first n r)) then
                let gs_out = Double(lb1,l,s,r,((Domain.complement n)::rb1)) in
                let s_out = Strand.mk_upper(lt1@s@[n]@rt1) in [gs_out, s_out, true]
              else [])
            in Lib.collect f (Lib.split_around_sublist s ns)
        | _ -> [])
    | _ -> []

  (* Rule LL: <L1>[N^ S]<R1> | <L S R> ->{l}  N^:<L>[S]<R> | <L1 N^ S R1> *)
  let leaks_ll = 
    match g,s_in with
    | Double(lb1,lt1,s,rt1,rb1), Strand.Upper(ns) ->
       (match s with
        | ((Domain.Toe _ as n)::s) when s <> [] ->
            let f = (fun (l,r) ->
               (* Enforce that L =/= L' N^ *)
              if (not(Lib.is_last n l)) then
                let gs_out = Double((lb1@[Domain.complement n]),l,s,r,rb1) in
                let s_out = Strand.mk_upper(lt1@[n]@s@rt1) in [gs_out, s_out, true]
              else [])
            in Lib.collect f (Lib.split_around_sublist s ns)
        | _ -> [])
    | _ -> []

  (* Rule LB: <L1>[N1^ S N2^]<R1> | <L S R> ->{w}  N1^:<L>[S]<R>:N2^ | <L1 N1^ S N2^ R1> *)
  let leaks_lb = [] : (t * Strand.t * bool) list (*(match g,s_in with (* DISABLING THESE FOR NOW. *)
    | Double(lb1,lt1,s,rt1,rb1), Strand.Upper(ns) -> (match (Lib.get_first_and_last s) with
         | Some((Domain.Toe _ as n1),s,(Domain.Toe _ as n2)) when s <> [] ->
            let f = (fun (l,r) ->
              (* Enforce that L =/= L' N1^ and R =/= N2^ R' *)
              if (not(Lib.is_last n1 l)) && (not(Lib.is_first n2 r)) then
                let gs_out = Double((lb1@[Domain.complement n1]),l,s,r,((Domain.complement n2)::rb1)) in
                let s_out = Strand.Upper(lt1@[n1]@s@[n2]@rt1) in [gs_out, s_out, false]
              else [])
            in Lib.collect f (Lib.split_around_sublist s ns)
        | _ -> [])
    | _ -> [])*)

  leaks_ls @ leaks_lr @ leaks_ll @ leaks_lb

(*let leak_gg ... *)

(***************************************************************************************************)
let universal_counters seg = 
    let f = List.map Domain.universal_counters in 
    match seg with 
      Double(lb,lt,s,rt,rb) -> Double(f lb, f lt, f s, f rt, f rb)
    | Hairpin(ob,ot,s,hp,sd) -> Hairpin(f ob, f ot, f s, f hp, sd)


(* FD Additions *)
// empty constructor
let empty = Double([],[],[],[],[])