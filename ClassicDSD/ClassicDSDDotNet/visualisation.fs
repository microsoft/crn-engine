[<JavaScript>]
module Microsoft.Research.DNA.Visualisation
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

module Species = Microsoft.Research.DNA.Species
module Svg = Microsoft.Research.CRNEngine.Svg

type domain = Domain.t
type strand = Strand.t
type segment = Segment.t
type gate = Gate.t
type origami = Origami.t

(* This module contains functions that convert DSD species into data structures that better express how they should be drawn. Note that it is entirely separate from SVG rendering. *)

(* This type gives more detailed information on how to draw the segments of a molecule. *)
type strandinfo = { arrow:bool; gapleft:bool; gapright:bool; lefthang:domain list; righthang:domain list }
type hairpininfo = { onRight:bool; pins:domain list}
type t = { domains : domain list;
           upper   : strandinfo option;
           lower   : strandinfo option;
           hairpin : hairpininfo option;
           tethereds : (t list list) option } 

(* Produce string representations for debugging. *)
let display_strandinfo (si:strandinfo) =
  "{ arrow=" + (string si.arrow) +
  ", gapLeft=" + (string si.gapleft) +
  ", gapRight=" + (string si.gapright) +
  ", lefthang=[" + (Domain.display_sequence si.lefthang) + "]" +
  ", righthang=[" + (Domain.display_sequence si.righthang) + "] }"
let display_hairpininfo (hi:hairpininfo) =
  "{ onRight=" + (string hi.onRight) + ", pins=[" + (Domain.display_sequence hi.pins) + "] }"
let rec display (v:t) =
  "{ domains=[" + (Domain.display_sequence v.domains) + "]" +
  ", upper=" + (Lib.option_map display_strandinfo "{}" v.upper) +
  ", lower=" + (Lib.option_map display_strandinfo "{}" v.lower) + 
  ", hairpin=" + (Lib.option_map display_hairpininfo "{}" v.hairpin) + 
  ", tethered=" + (Lib.option_map (Lib.string_of_list (Lib.string_of_list display " ") "; ") "{}" v.tethereds) +
  " }"

(**************************************************************************************)

(* Draw the bottom left strand start arrow? *)
let drawBottomLeftArrow (v:t) = Lib.option_map (fun si -> si.arrow) false v.lower

(* Draw the top right strand start arrow? *)
let drawTopRightArrow (v:t) = Lib.option_map (fun si -> si.arrow) false v.upper
  
(* Leave a gap at the top left? *)
let gapTopLeft (v:t) = Lib.option_map (fun si -> si.gapleft) false v.upper

(* Leave a gap at the top right? *)
let gapTopRight (v:t) = Lib.option_map (fun si -> si.gapright) false v.upper

(* Leave a gap at the bottom left? *)
let gapBottomLeft (v:t) = Lib.option_map (fun si -> si.gapleft) false v.lower

(* Leave a gap at the bottom right? *)
let gapBottomRight (v:t) = Lib.option_map (fun si -> si.gapright) false v.lower

(* Draw the top strand line? *)  
let drawTopStrand (v:t) = Option.isSome v.upper

(* Draw the bottom strand line? *)  
let drawBottomStrand (v:t) = Option.isSome v.lower

(* Draw a hairpin? *)
let drawHairpin (v:t) = Option.isSome v.hairpin

(* Draw a hairpin on the left? *)
let drawHairpinLeft (v:t) = Lib.option_map (fun hi -> not hi.onRight) false v.hairpin

(* Draw a hairpin on the right? *)
let drawHairpinRight (v:t) = Lib.option_map (fun hi -> hi.onRight) false v.hairpin

(* Is this an origami? *)
let drawInOrigami (v:t) = Option.isSome v.tethereds

(* Get the main domains in a section. *)
let mainDomains (v:t) = v.domains

(* Get the bottom left overhang domains, if any. *)
let overhangBottomLeft (v:t) = Lib.option_map (fun si -> si.lefthang) [] v.lower

(* Get the top left overhang domains, if any. *)
let overhangTopLeft (v:t) = Lib.option_map (fun si -> si.lefthang) [] v.upper

(* Get the top right overhang domains, if any. *)
let overhangTopRight (v:t) = Lib.option_map (fun si -> si.righthang) [] v.upper

(* Get the bottom right overhang domains, if any. *)
let overhangBottomRight (v:t) = Lib.option_map (fun si -> si.righthang) [] v.lower

(* Get the hairpinned domains, if any. *)
let hairpinDomains (v:t) = Lib.option_map (fun hi -> hi.pins) [] v.hairpin

(* Get the leftmost domain. *)
let leftDomain (v:t) = List.head (mainDomains v)

(* Get the rightmost domain. *)
let rightDomain (v:t) = List.head (List.rev (mainDomains v))

(* Get the anchored species. *)
let anchoredSpecies (v:t) = Lib.option_map id [] v.tethereds
 
(**************************************************************************************)

(* Make the "strandinfo" for a trivial single stranded portion. Can specify whether to draw an arrow or not. *)
let make_strandinfo (arrow:bool) = Some { arrow=arrow; gapleft=false; gapright=false; lefthang=[]; righthang=[] }

(* Convert a strand into a visualisation data structure. *)
let from_strand (s:strand) : t list =
  match s with
  | Strand.Lower ns -> [{ domains = ns; upper = None; lower = make_strandinfo true ; hairpin = None ; tethereds = None}]
  | Strand.Upper ns -> [{ domains = ns; upper = make_strandinfo true; lower = None ; hairpin = None ; tethereds = None}]

(* Convert a gate into a visualisation data structure. *)
let from_gate (g:gate) : t list =
  (* Get at the rightmost corners of segment lists. *)
  let getRightHang (upper:bool) (cs:segment list) : domain list =
    match(Lib.get_last_element cs) with
    | None -> failwith "Visualisation.getRightHang: empty segment list"
    | Some(_,Segment.Double(_,_,_,rt,rb)) -> if upper then rt else rb
    | Some(_,Segment.Hairpin(ob,ot,_,_,Segment.Left)) -> if upper then ot else ob
    | Some(_,Segment.Hairpin(_,_,_,_,Segment.Right)) -> []
  in
  (* Get at the leftmost corners of segment lists. *)
  let getLeftHang (upper:bool) (cs:segment list) : domain list =
    match cs with
    | [] -> failwith "Visualisation.getLeftHang: empty segment list"
    | (Segment.Double(lb,lt,_,_,_))::_ -> if upper then lt else lb
    | (Segment.Hairpin(ob,ot,_,_,Segment.Right))::_ -> if upper then ot else ob
    | (Segment.Hairpin(_,_,_,_,Segment.Left))::_ -> []
  in
  (* Gather the results of converting segments into visualisation data structures. *)
  let rec gather (rs:t list) (acc:segment list) (lhs:segment list list) (gs:segment list) (rhs:segment list list) =
    match gs with
    | [] -> rs
    | ((Segment.Double(lb,lt,s,rt,rb)) as c)::gs' ->
        (* These are determined by the leftward context. *)
        let lowerArrow = (List.isEmpty lb && List.isEmpty acc) in
        let lowerGapLeft = (List.isEmpty acc && lhs<>[] && List.isEmpty lt && (getRightHang true (List.head lhs) |> List.isEmpty)) in
        let lowerLeftHang = lb in
        let upperGapLeft = (acc<>[] && List.isEmpty lb && (getRightHang false acc |> List.isEmpty)) in
        let upperLeftHang = lt in
        (* These are determined by the rightward context. *)
        let upperArrow = (List.isEmpty rt && not(List.isEmpty gs' && rhs<>[])) in
        let upperGapRight = (gs'<>[] && List.isEmpty rb && (getLeftHang false gs' |> List.isEmpty)) in
        let lowerGapRight = (List.isEmpty gs' && rhs<>[] && List.isEmpty rt && (getLeftHang true (List.head rhs) |> List.isEmpty)) in
        let (upperRightHang,lowerRightHang,vs') =
          match gs',rhs with
          | [],[]     -> (rt,rb,[])
          | [],(_::_) -> let vs' = (if List.isEmpty rt then [] else [{domains=rt; lower=None; upper=make_strandinfo false (*upperArrow*); hairpin=None ; tethereds=None}]) in ([],rb,vs')
          | (_::_),_  -> let vs' = (if List.isEmpty rb then [] else [{domains=rb; lower=make_strandinfo false (*lowerArrow*); upper=None; hairpin=None; tethereds=None}]) in (rt,[],vs')
        in
        (* Create the new visualisation data structures and recurse. *)
        let upperStrand = Some { arrow=upperArrow; gapleft=upperGapLeft; gapright=upperGapRight; lefthang=upperLeftHang; righthang=upperRightHang } in
        let lowerStrand = Some { arrow=lowerArrow; gapleft=lowerGapLeft; gapright=lowerGapRight; lefthang=lowerLeftHang; righthang=lowerRightHang } in
        let vs = [{ domains=s; upper=upperStrand; lower=lowerStrand; hairpin=None; tethereds=None}] in
        gather (rs@vs@vs') (acc@[c]) lhs gs' rhs
    | ((Segment.Hairpin(ob,ot,s,hp,Segment.Left)) as c)::gs' -> 
      (* There should be no leftward context *)
      if not (List.isEmpty acc) then failwith "Left hairpin not the left-most segment" 
      else 
        (* Determined by the rightward context. *)
        let upperArrow = (List.isEmpty ot && not(List.isEmpty gs' && rhs<>[])) in
        let upperGapRight = (gs'<>[] && List.isEmpty ob && (getLeftHang false gs' |> List.isEmpty)) in
        let lowerGapRight = (List.isEmpty gs' && rhs<>[] && List.isEmpty ot && (getLeftHang true (List.head rhs) |> List.isEmpty)) in
        let (upperRightHang,LowerRightHang,vs') =
          match gs',rhs with
          | [],[] -> (ot,ob,[])
          | [],(_::_) -> let vs' = (if List.isEmpty ot then [] else [{domains=ot; lower=None; upper=make_strandinfo false (*upperArrow*); hairpin=None;tethereds=None}]) in ([],ob,vs')
          | (_::_),_  -> let vs' = (if List.isEmpty ob then [] else [{domains=ob; lower=make_strandinfo false (*lowerArrow*);upper=None; hairpin=None; tethereds=None}]) in (ot,[],vs')
        in
        let upperStrand = Some {arrow=upperArrow; gapleft=false; gapright=upperGapRight; lefthang=[]; righthang=upperRightHang} in
        let lowerStrand = Some {arrow=false; gapleft=false; gapright=lowerGapRight; lefthang=[]; righthang=LowerRightHang}in
        let hairPin = Some {onRight = false; pins = hp} in
        let vs = [{ domains=s; upper=upperStrand; lower=lowerStrand; hairpin=hairPin; tethereds=None}] in
        gather (rs@vs@vs') (acc@[c]) lhs gs' rhs
    | ((Segment.Hairpin(ob,ot,s,hp,Segment.Right)) as c)::gs' ->
      if (not (List.isEmpty gs')) then failwith "Right hairpin not the right-most segment"
      else
        (* These are determined by the leftward context. *)
        let lowerArrow = (List.isEmpty ob && List.isEmpty acc) in
        let lowerGapLeft = (List.isEmpty acc && lhs<>[] && List.isEmpty ot && (getRightHang true (List.head lhs) |> List.isEmpty)) in
        let lowerLeftHang = ob in
        let upperGapLeft = (acc<>[] && List.isEmpty ob && (getRightHang false acc |> List.isEmpty)) in
        let upperLeftHang = ot in
        (* Create the new visualisation data structures and recurse. *)
        let upperStrand = Some { arrow=false; gapleft=upperGapLeft; gapright=false; lefthang=upperLeftHang; righthang=[] } in
        let lowerStrand = Some { arrow=lowerArrow; gapleft=lowerGapLeft; gapright=false; lefthang=lowerLeftHang; righthang=[] } in
        let hairPinStrand = Some { onRight = true; pins = List.rev hp } in
        let vs = [{ domains=s; upper=upperStrand; lower=lowerStrand; hairpin=hairPinStrand; tethereds=None}] in
        gather (rs@vs) (acc@[c]) lhs gs' rhs
  in
  (* Normalise the gate first, just to be sure... *)
  let vs = Lib.collect_contextual (gather [] []) (Gate.normalise g) in
  (* Tweak any extreme overhanging single toehold domains to be horizontal... *)
  let isSingleToehold (ds:domain list) = match ds with [Domain.Toe _] -> true | _ -> false in
  let convertLeft (vs:t list) =
    match vs with
    | (v::vs') ->
     (match v.upper, v.lower with
      | Some ui, Some li ->
       (match ui.lefthang, li.lefthang with
        (* Upper overhang made horizontal *)
        | ([Domain.Toe _ as toe],ds) when not(isSingleToehold ds) ->
            let v1 = {domains=[toe]; upper=(make_strandinfo false); lower=None; hairpin=None; tethereds=None} in
            let v2 = {v with upper=Some{ui with lefthang=[]}}  in
            [v1;v2]@vs'
        (* Lower overhang made horizontal *)
        | (ds,[Domain.Toe _ as toe]) when not(isSingleToehold ds) ->
            let v1 = {domains=[toe]; upper=None; lower=(make_strandinfo true); hairpin=None; tethereds=None} in
            let v2 = {v with lower=Some{li with lefthang=[]}} in
            [v1;v2]@vs'
        | _,_ -> vs)
      | _,_ -> vs)
    | [] -> vs
  in
  let convertRight (vs:t list) =
    match Lib.get_last_element vs with
    | Some(vs',v) ->
     (match v.upper, v.lower with
      | Some ui, Some li ->
       (match ui.righthang, li.righthang with
        (* Upper overhang made horizontal *)
        | ([Domain.Toe _ as toe],ds) when not(isSingleToehold ds) ->
            let v1 = {v with upper=Some{ui with righthang=[]}} in
            let v2 = {domains=[toe]; upper=(make_strandinfo true); lower=None; hairpin=None; tethereds=None} in
            vs'@[v1;v2]
        (* Lower overhang made horizontal *)
        | (ds,[Domain.Toe _ as toe]) when not(isSingleToehold ds) ->
            let v1 = {v with lower=Some{li with righthang=[]}} in
            let v2 = {domains=[toe]; upper=None; lower=(make_strandinfo false); hairpin=None; tethereds=None} in
            vs'@[v1;v2]
        | _,_ -> vs)
      | _,_ -> vs)
    | None -> vs
  in
  convertLeft (convertRight vs)

let from_origami (o:origami) =
  [{domains=[];
    upper=None;
    lower=None;
    hairpin=None;
    tethereds = Some (Lib.collect (fun t ->
                                     match t with
                                     | Origami.C_strand s -> [from_strand(s)]
                                     | Origami.C_gate g -> [from_gate(g)]
                                   ) o)}]