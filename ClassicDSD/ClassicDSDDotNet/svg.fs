// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Svg
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open Microsoft.Research.DNA.Options
open Microsoft.Research.CRNEngine

module Species = Microsoft.Research.DNA.Species
module CrnSvg = Microsoft.Research.CRNEngine.Svg

(* This module contains the logic that creates SVG elements representing a DSD species. Note that this is independant of the Visualisation module. *)

type domain = Domain.t
type strand = Strand.t
type segment = Segment.t
type gate = Gate.t
type origami = Origami.t

///The angle at which overhangs bend.
let bend_angle = 45.0
///The arrow segment, relative to the endpoint. This is for upper strands. For lower strands, this vector will be reversed.
let arrow_vector = { Svg.x = -7.0; Svg.y = -4.0 }
///The distance between upper and lower strands in complement mode. More accurately, this is the point at which a strand ends, when its opposite starts at the origin.
let segment_sep_complement = { Svg.x = 0.0; Svg.y = 8.0 }
///The distance between upper and lower strands in condensed mode. More accurately, this is the point at which a strand ends, when its opposite starts at the origin.
let segment_sep_condensed = { Svg.x = 0.0; Svg.y = 18.0 }
///The distance between upper and lower strands in nucleotides mode. More accurately, this is the point at which a strand ends, when its opposite starts at the origin.
let segment_sep_nucleotides = { Svg.x = 0.0; Svg.y = 34.0 }
///The distance between segments in a gate.
let segment_skip = 7.0
///The distance between species in an origami.
let species_skip = 10.0
///The margin around species in an origami.
let origami_sep = { Svg.x = 16.0; Svg.y = 16.0 }
///The size of the origami box corners.
let origami_round = 32.0
///Standard width of a character for the purpose of determining domain name length.
let letterwidth = 8
///The extra strand length around toehold domains, in characters.
let toehold_pad = 3
///The extra strand length around normal domains, in characters.
let normal_pad = 4
///The assumed font size; note that this is actually reported in the style.
let fontsize = 15.0
///The thickness of the strand.
let linewidth = 2.5
///The extra strand length added when two overhangs cross, to prevent the domain labels from overlapping.
let cross_gap = fontsize + 2.0 (* allow for asterisk *)
///The extra strand length added to overhangs in modes that put text on the inside, to prevent the domain labels from appearing ovre the horizontal strand.
let inlabel_gap = 2. * float letterwidth
///The height of a hairpin domain name label.
let baseline_height = 5.0
///The distance between the strand and the domain labels, for complement mode.
let label_sep_complement = 8.0
///The distance between the strand and the domain labels, for condensed mode, for upper strands (and double strands).
let label_sep_condensed_upper = segment_sep_condensed.y / 2. + fontsize / 2. - linewidth
///The distance between the strand and the domain labels, for condensed mode, for lower overhang strands.
let label_sep_condensed_lower = segment_sep_condensed.y / 2. + fontsize / 2. - linewidth - 1.5
///The distance between the strand and the domain labels, for nucleotides mode, for lower strands.
let label_sep_nucleotides_lower = 14.0
///The distance between the strand and the domain labels, for nucleotides mode, for upper strands.
let label_sep_nucleotides_upper = 15.0

///Provides the string for a domain.
let dom_string d = Domain.display_picture d true

///Calculates the extra strand length, in pixels, around a domain.
let dom_pad d = (if Domain.is_toehold d then toehold_pad else normal_pad) * letterwidth
///Calculates the render width of a domain name.
let len_dom_name = Domain.display_length >> ((*) letterwidth)
let len_dom_seq mapping d = (match Sequence.get_domain mapping d with Some (Some s) -> s | _ -> []) |> List.length |> ((*) letterwidth)

///Calculates the total render width of a domain.
let len_dom mode mapping d = (d |> (if mode = Nucleotides then len_dom_seq mapping else len_dom_name)) + (d |> dom_pad)
///Calculates the total render width of a domain list.
let len_doms mode mapping = Lib.fold_left (fun s d -> s + len_dom mode mapping d) 0

///This CSS class will be added to all elements of the picture, and can then be used for styling.
let baseClass = "svgdsd"
let dom_name d = d |> Domain.unstar |> Domain.to_string
///Provides a valid CSS class identifier for a domain.
let dom_id d ="dom_" + (dom_name d)

///Describes how a label is going to be positioned. This can affect spacing.
type labelMode =
///On an upper strand, above the strand.
| UpperOuter
///On an upper strand, below the strand.
| UpperInner
///On a lower strand, below the strand.
| LowerOuter
///On a lower strand, above the strand.
| LowerInner

///Produces a Svg.group that renders the given domain list, as a straight line. The line is composed of a number of line paths, correctly aligned. The line starts at the origin and ends at (length,0). The resulting Svg.group has two anchors at those points, named "start" and "end" respectively.
let dom_list_to_svg mode mapping label ds =
  let make_label x d =
    match label with
    | Some label ->
      let dom_class = (baseClass + " " + (d |> dom_id) + (if mode = Nucleotides then " fixedwidth" else "")) |> Some in
      let y = match mode with
              | Complement -> -label_sep_complement
              | Condensed -> match label with 
                             | UpperOuter | UpperInner -> label_sep_condensed_upper
                             | LowerOuter | LowerInner -> label_sep_condensed_lower
              | Nucleotides -> match label with
                               | UpperOuter | UpperInner -> label_sep_nucleotides_upper
                               | LowerOuter | LowerInner -> label_sep_nucleotides_lower
      let anchor = { Svg.x = x ; Svg.y = y } in
      let dir = Svg.translate_point_by { x = 10.0; y = 0.0 } anchor
      let text = match mode with
                 | Nucleotides -> let s = match Sequence.get_domain mapping d with Some (Some s) -> s | _ -> []
                                  // Note that the sequence has already been complemented, if necessary, by Sequence.get_domain.
                                  Sequence.string_of_sequence s
                 | _ -> dom_string d
      { Svg.label_text = text
      ; Svg.label_anchor = anchor
      ; Svg.label_class = dom_class
      ; Svg.letter_width = Svg.default_letter_width
      ; Svg.label_dir = dir } |> Some
    | None -> None
  let ps, end_point =
    Lib.fold_left
      (fun (acc_paths, acc_p) d ->
         let dom_class = (baseClass + " " + (d |> dom_id)) |> Some in
         let do_tether = Domain.is_tethered d in
         if do_tether then
           let new_p = acc_p |> Svg.translate_point_by { Svg.x = len_dom mode mapping d |> float; Svg.y = 0.0 } in
           let tether_p =
             { Svg.x = (new_p.x + acc_p.x) / 2.0 ; Svg.y = 0.0 } in
           let new_path1 =
             { Svg.path_class = dom_class
             ; Svg.commands = [ Svg.MoveTo acc_p; Svg.LineTo tether_p ]
             ; Svg.path_label = make_label ((new_p.x + acc_p.x) / 2.0) d } in
           let new_path2 =
             { Svg.path_class = dom_class
             ; Svg.commands = [ Svg.MoveTo tether_p; Svg.LineTo new_p ]
             ; Svg.path_label = None } in
           let tether_path =
             { Svg.path_class = baseClass + " tether-pin" |> Some
             ; Svg.commands = [ Svg.MoveTo (tether_p |> Svg.translate_point_by { x = 0.0; y = 3.0})
                              ; Svg.ArcTo ( tether_p |> Svg.translate_point_by { x = 0.0; y = -3.0}
                                          , { rx = 3.0; ry = 3.0; large_flag = true; sweep_flag = false; angle = 0.0 })
                              ; Svg.ArcTo ( tether_p |> Svg.translate_point_by { x = 0.0; y = 3.0}
                                          , { rx = 3.0; ry = 3.0; large_flag = true; sweep_flag = false; angle = 0.0 }) ]
             ; Svg.path_label = None } in
           new_path2::tether_path::new_path1::acc_paths, new_p
         else
           let new_p = acc_p |> Svg.translate_point_by { Svg.x = len_dom mode mapping d |> float; Svg.y = 0.0 } in
           let new_path =
             { Svg.path_class = dom_class
             ; Svg.commands = [ Svg.MoveTo acc_p; Svg.LineTo new_p ]
             ; Svg.path_label = make_label ((new_p.x + acc_p.x) / 2.0) d } in
           new_path::acc_paths, new_p)
      ([], Svg.origo)
      ds in
  { Svg.name = ""
  ; Svg.content = List.rev ps |> Svg.Paths
  ; Svg.anchors = [ { anchor_id = "start"; anchor_point = Svg.origo }
                  ; { anchor_id = "end"; anchor_point = end_point } ]
  ; Svg.debug_anchors = false
  ; Svg.sub_groups = []
  ; Svg.offset = None }

///Produces a Svg.group that renders the given domain list, arranged as a hairpin arc. The line is composed of a number of arc paths, correctly aligned. The arc starts at the origin, runs counterclockwise, and ends at the strand separation point (therefore, this is a right-hairpin; the caller needs to rotate it if it's meant to be a left-hairpin).
let dom_list_to_hp_svg mode mapping ds =
  let segment_sep = match mode with
                    | Complement -> segment_sep_complement
                    | Condensed -> segment_sep_condensed
                    | Nucleotides -> segment_sep_nucleotides
  let straight = dom_list_to_svg mode mapping (Some LowerInner) ds in
  let mult = if mode = Nucleotides then 4.0 else 1.0 in
  let len = mult * (Svg.get_anchor_point "end" straight).x in
  let d = 6.9282 - 1.0 + len / (3.0 * (float letterwidth)) in
  let c = { Svg.x = d; Svg.y = segment_sep.y / 2.0 } in
  let total_v = 360.0 - (Svg.dir c) * 360.0 / Svg.pi in
  let r = Svg.norm c in
  ///Used to make the label for a domain (in Complement and Condensed modes).
  let make_label v_mid d =
    let offset = (len_dom_name d |> float) / 2.0 + label_sep_complement in
    let sink = Svg.rotate_point_around Svg.origo (v_mid - 90.0) c |> Svg.scale_to baseline_height in
    let label_anchor = Svg.rotate_point_around c v_mid { x = -offset * c.x / r; y = -offset * c.y / r } |> Svg.translate_point_by sink in
    let label_direction = Svg.rotate_point_around c v_mid { x = (-offset-10.0) * c.x / r; y = (-offset-10.0) * c.y / r } |> Svg.translate_point_by sink in
    let dom_class = (baseClass + " " + (d |> dom_id) + (if mode = Nucleotides then " fixedwidth" else "")) |> Some in
    let text = dom_string d
    { Svg.label_text = text
    ; Svg.label_anchor = label_anchor
    ; Svg.label_class = dom_class
    ; Svg.letter_width = Svg.default_letter_width
    ; Svg.label_dir = label_direction } |> Some
  ///Used to make the label for a single base (in Nucleotide mode).
  let make_base_label v_mid d (b:char) =
    // Adding baseline_height because the translate to sink afterwards will remove that.
    let offset = -label_sep_nucleotides_upper + baseline_height in
    let sink = Svg.rotate_point_around Svg.origo (v_mid) c |> Svg.scale_to baseline_height in
    let label_anchor = Svg.rotate_point_around c v_mid { x = -offset * c.x / r; y = -offset * c.y / r } |> Svg.translate_point_by sink in
    let label_direction = Svg.rotate_point_around c v_mid { x = (-offset-10.0) * c.x / r; y = (-offset-10.0) * c.y / r } |> Svg.translate_point_by sink in
    let label_direction = Svg.rotate_point_around label_anchor 90.0 label_direction
    let dom_class = (baseClass + " " + (d |> dom_id) + (if mode = Nucleotides then " fixedwidth" else "")) |> Some in
    let text = string b // note: using b.ToString() causes the ASCII code to be emitted in JavaScript
    { Svg.label_text = text
    ; Svg.label_anchor = label_anchor
    ; Svg.label_class = dom_class
    ; Svg.letter_width = Svg.default_letter_width
    ; Svg.label_dir = label_direction } |> Some
  let to_arc v =
    { Svg.rx = r
    ; Svg.ry = r
    ; Svg.large_flag = v > 180.0
    ; Svg.sweep_flag = true
    ; Svg.angle = 0.0 } in
  let ps_rev, _, _ =
    Lib.fold_left
      (fun (acc_paths, acc_p, acc_v) d ->
         match mode with 
         | Nucleotides ->
           // Renders one arc Svg.path per base (plus two for padding).
           let bases = match Sequence.get_domain mapping d with Some (Some s) -> Sequence.string_of_sequence s | _ -> "" in
           let bases = bases.ToCharArray() in
           // This is the total length of the domain, as a fraction of the total hairpin length.
           let total_len = mult * (len_dom mode mapping d |> float) / len in
           // This is the padding length, as a fraction of the total hairpin length.
           let pad_len = 20. / len in
           // This is the length of one base, as a fraction of the total hairpin length.
           let v_frac = (total_len - 2. * pad_len) / (Array.length bases |> float) in
           // Function that adds an arc Svg.path with no label, for padding.
           let add_padding (acc_paths, acc_p, acc_v) =
             let v = pad_len * total_v in
             let new_v = v + acc_v in
             let new_p = Svg.rotate_point_around c new_v Svg.origo in
             let new_path =
               { Svg.path_class = (baseClass + " " + (d |> dom_id)) |> Some
               ; Svg.commands = [ Svg.MoveTo acc_p; Svg.ArcTo (new_p, to_arc v) ]
               ; Svg.path_label = None } in
             (new_path::acc_paths,new_p,new_v) in
           // Function that adds an arc Svg.path with a single base label.
           let make_base_arc (acc_paths,acc_p,acc_v) b =
             let v = v_frac * total_v in
             let new_v = v + acc_v in
             let new_p = Svg.rotate_point_around c new_v Svg.origo in
             let new_path =
               let v_mid = (new_v + acc_v) / 2.0 in
               { Svg.path_class = (baseClass + " " + (d |> dom_id)) |> Some
               ; Svg.commands = [ Svg.MoveTo acc_p; Svg.ArcTo (new_p, to_arc v) ]
               ; Svg.path_label = make_base_label v_mid d b } in
             (new_path::acc_paths,new_p,new_v) in
           // Add an arc with no label for the padding at the beginning of the domain.
           let (acc_paths,acc_p,acc_v) = add_padding (acc_paths,acc_p,acc_v) in
           // Add an arc Svg.path for each base.
           let (acc_paths,acc_p,acc_v) = Array.fold make_base_arc (acc_paths,acc_p,acc_v) bases in
           // Add an arc with no label for the padding at the beginning of the domain.
           let (acc_paths,acc_p,acc_v) = add_padding (acc_paths,acc_p,acc_v) in
           (acc_paths,acc_p,acc_v)
         | _ ->
           // Renders one arc Svg.path for the domain.
           let v_frac = (len_dom mode mapping d |> float) / len in
           let v = v_frac * total_v in
           let new_v = v + acc_v in
           let new_p = Svg.rotate_point_around c new_v Svg.origo in
           let new_path =
             let v_mid = (new_v + acc_v) / 2.0 in
             { Svg.path_class = (baseClass + " " + (d |> dom_id)) |> Some
             ; Svg.commands = [ Svg.MoveTo acc_p; Svg.ArcTo (new_p, to_arc v) ]
             ; Svg.path_label = make_label v_mid d } in
           new_path::acc_paths, new_p, new_v
         )
      ([], Svg.origo, 0.0)
      (ds |> List.rev) in
  let ps_rev_end =
    match ps_rev |> List.map (Svg.translate_path_by { x = 1.0; y = 0.0 }) with
    | [] -> []
    | p::ps -> { p with Svg.commands = p.commands @ [Svg.LineTo segment_sep] }::ps in
  let ps_bend =
    match ps_rev_end |> List.rev with
    | [] -> []
    | path::ps ->
      (match path.commands with
       | (Svg.MoveTo p)::cs ->  { path with
                                    commands = (p |> Svg.translate_point_by { x = -1.0; y = 0.0 } |> Svg.MoveTo) :: Svg.LineTo p :: cs }::ps
       | _ -> path::ps) in
  { Svg.name = ""
  ; Svg.content = ps_bend |> Svg.Paths
  ; Svg.anchors = [ { anchor_id = "start"; anchor_point = Svg.origo }
                  ; { anchor_id = "end"; anchor_point = segment_sep } ]
  ; Svg.debug_anchors = false
  ; Svg.sub_groups = []
  ; Svg.offset = None }

///This type represents a rendered SVG single strand, plus anchors.
type svg_strand =
  ///The SVG rendering of the strand.
  { strand : Svg.path list
  ///The anchor for the strand start.
  ; start_anchor : Svg.point
  ///The anchor for the strand end.
  ; end_anchor : Svg.point
  ///The anchor for the east side of the strand. This is independent of strand direction (it's always horizontally at the east end), and independent of overhang bending (it's always vertically at the same height as the main segment body).
  ; east_anchor : Svg.point }

///Applies a transform to a strand's SVG representation. This includes the anchors.
let transform_strand_by t s =
  { strand = s.strand |> List.map (Svg.transform_path_by t)
  ; start_anchor = s.start_anchor |> t
  ; end_anchor = s.end_anchor |> t
  ; east_anchor = s.east_anchor |> t }
///Translates a strand's SVG representation. This includes the anchors.
let translate_strand_by = Svg.translate_point_by >> transform_strand_by

///Attaches [s2] at the end of [s1] creating a longer strand. This is assuming upper strands (end in the east), so the east anchor becomes s2's east anchor.
let (-@) s1 s2 =
  let v = Svg.sub s1.end_anchor s2.start_anchor in
  { strand = s1.strand @ (s2.strand |> List.map (Svg.translate_path_by v))
  ; start_anchor = s1.start_anchor
  ; end_anchor = s2.end_anchor |> Svg.translate_point_by v
  ; east_anchor = s2.east_anchor |> Svg.translate_point_by v }

///Attaches [s1] at the start of [s2] creating a longer strand. This is assuming upper strands (end in the east), so the east anchor remains s2's east anchor.
let (@-) s1 s2 =
  let v = Svg.sub s2.start_anchor s1.end_anchor in
  { strand = (s1.strand |> List.map (Svg.translate_path_by v)) @ s2.strand
  ; start_anchor = s1.start_anchor |> Svg.translate_point_by v
  ; end_anchor = s2.end_anchor
  ; east_anchor = s2.east_anchor }

///Attaches [s2] at the end of [s1] creating a longer strand. This is assuming lower strands (start in the east), so the east anchor remains s1's east anchor.
let (-&) s1 s2 =
  let v = Svg.sub s1.end_anchor s2.start_anchor in
  { strand = s1.strand @ (s2.strand |> List.map (Svg.translate_path_by v))
  ; start_anchor = s1.start_anchor
  ; end_anchor = s2.end_anchor |> Svg.translate_point_by v
  ; east_anchor = s1.east_anchor }

///Attaches [s1] at the start of [s2] creating a longer strand. This is assuming lower strands (start in the east), so the east anchor becomes s1's east anchor.
let (&-) s1 s2 =
  let v = Svg.sub s2.start_anchor s1.end_anchor in
  { strand = (s1.strand |> List.map (Svg.translate_path_by v)) @ s2.strand
  ; start_anchor = s1.start_anchor |> Svg.translate_point_by v
  ; end_anchor = s2.end_anchor
  ; east_anchor = s1.east_anchor |> Svg.translate_point_by v }

///Returns [s2] moved such that its east_anchor is on top of [s1]s east_anchor. The reason this does not necessarily cause strands to overlap is that a strand's east anchor is not necessarily right on its eastern end.
let (-<) s1 s2 =
  let v = Svg.sub s1.east_anchor s2.east_anchor in
  s2 |> translate_strand_by v

///Returns [s1] moved such that its east_anchor is on top of [s2]s east_anchor. The reason this does not necessarily cause strands to overlap is that a strand's east anchor is not necessarily right on its eastern end.
let (>-) s1 s2 =
  let v = Svg.sub s2.east_anchor s1.east_anchor in
  s1 |> translate_strand_by v

///Returns [so2] moved such that its east_anchor is on top of [s1]s east_anchor. The reason this does not necessarily cause strands to overlap is that a strand's east anchor is not necessarily right on its eastern end.
let (/<) s1 so2 = match so2 with None -> None | Some s2 -> s1 -< s2 |> Some
///Returns [so1] moved such that its east_anchor is on top of [s2]s east_anchor. The reason this does not necessarily cause strands to overlap is that a strand's east anchor is not necessarily right on its eastern end.
let (>/) so1 s2 = match so1 with None -> None | Some s1 -> s1 >- s2 |> Some

///Attaches [so2] to the end of [s1], if present. Assumes upper strands (end in the east).
let (/@) s1 so2 = match so2 with None -> s1 | Some s2 -> s1 -@ s2
///Attaches [so1] to the start of [s2]. Assumes upper strands (end in the east).
let (@/) so1 s2 = match so1 with None -> s2 | Some s1 -> s1 @- s2
///Attaches [so2] to the end of [s1]. Assumes lower strands (start in the east).
let (/&) s1 so2 = match so2 with None -> s1 | Some s2 -> s1 -& s2
///Attaches [so1] to the start of [s2]. Assumes lower strands (start in the east).
let (&/) so1 s2 = match so1 with None -> s2 | Some s1 -> s1 &- s2

///Returns the same strand, with an added arrow. The arrow always goes at the end, but I still need to know whether the strand is upper or lower in order to draw the arrowhead in the correct direction.
let add_arrow upper s =
  let v = if upper then arrow_vector else Svg.rotate_point_around Svg.origo 180.0 arrow_vector in
  match s.strand |> List.rev with
  | l::r ->
    (match l.commands |> List.rev with
     | (Svg.LineTo p)::_ ->
       let al = 
         { l with commands = l.commands @ [p |> Svg.translate_point_by v |> Svg.LineTo] } in
         { s with strand = (al::r) |> List.rev }
     | _ -> s)
  | _ -> s

///Rotates a strand around its start point. Needs to be told whether it's an upper strand because I want to keep a tiny horizontal segment as a connector, and the direction of that depends on whether the start point is to the left or right.
let bend_start upper angle s =
  let rcs = List.map (Svg.rotate_path_command_around s.start_anchor angle) in
  let rps = List.map (Svg.rotate_path_around s.start_anchor angle) in
  // Tiny horizontal segment that doesn't bend; paints over the small empty wedge that would otherwise appear between this strand and any attached horizontal strands. I.e. it acts as a linejoin between two distinct path commands.
  let d = if upper then { Svg.x = 1.0; Svg.y = 0.0 } else { Svg.x = -1.0; Svg.y = 0.0 } in
  let strand =
    match s.strand with
    | path::ps ->
      (match path.commands with
       | (Svg.MoveTo p as mp) :: cs -> { path with
                                           commands = mp :: (p |> Svg.translate_point_by d |> Svg.LineTo) :: (cs |> rcs)
                                           path_label = match path.path_label with None -> None | Some path_label -> path_label |> Svg.rotate_label_around s.start_anchor angle |> Some } :: (ps |> rps)
       | _ -> s.strand |> rps )
    | _ -> s.strand |> rps in
  { strand = strand
  ; start_anchor = s.start_anchor
  ; end_anchor = s.end_anchor |> Svg.rotate_point_around s.start_anchor angle
  // Note that the east anchor remains unchanged, regardless of upper or lower. I've tried having it rotate as well, but there seems to be no effect. I figure the reason is as follows; if the strand is upper, then rotating at start means that this is a top right overhang, so there's nothing to be connected at the east side, so we don't care about the east anchor. If the strand is lower, then the east anchor corresponds to he start point, so rotation has no effect anyway.
  ; east_anchor = s.east_anchor }

///Rotates a strand around its end point. Needs to be told whether it's an upper strand because I want to keep a tiny horizontal segment as a connector, and the direction of that depends on whether the end point is to the left or right.
let bend_end upper angle s =
  let rcs = List.map (Svg.rotate_path_command_around s.end_anchor angle) in
  let rps = List.map (Svg.rotate_path_around s.end_anchor angle) in
  // Tiny horizontal segment that doesn't bend; paints over the small empty wedge that would otherwise appear between this strand and any attached horizontal strands. I.e. it acts as a linejoin between two distinct path commands.
  let d = if upper then { Svg.x = -1.0; Svg.y = 0.0 } else { Svg.x = 1.0; Svg.y = 0.0 } in
  let strand =
    match s.strand |> List.rev with
    | path::ps ->
      (match path.commands |> List.rev with
       | (Svg.LineTo p as lp) :: cs -> ({ path with
                                            commands = (lp :: (p |> Svg.translate_point_by d |> Svg.LineTo) :: (cs |> rcs)) |> List.rev
                                            path_label = match path.path_label with None -> None | Some path_label -> path_label |> Svg.rotate_label_around s.end_anchor angle |> Some } :: (ps |> rps)) |> List.rev
       | _ -> s.strand |> rps)
    | _ -> s.strand |> rps in
  { strand = strand
  ; start_anchor = s.start_anchor |> Svg.rotate_point_around s.end_anchor angle
  ; end_anchor = s.end_anchor
  // Note that the east anchor remains unchanged, regardless of upper or lower. I've tried having it rotate as well, but there seems to be no effect. I figure the reason is as follows; if the strand is upper, then the east anchor corresponds to the end point, so rotation has no effect anyway. If the strand is lower, then rotating at end means that this is a bottom right overhang, so there's nothing to be connected at the east side, so we don't care about the east anchor.
  ; east_anchor = s.east_anchor }

///Returns a strand where the start point has been moved by the supplied vector. This is meant to be used to extend the strand's start domain. Therefore, the vector should always be horizontal, and it should be positive for lower segments, and negative for upper segments. Note that this function does not change the east anchor. The caller should change the east anchor if this is a lower strand.
let extend_start d s =
  let t = Svg.translate_point_by d in
  { strand = ( match s.strand with
             | path::ps -> (match path.commands with
                            | (Svg.MoveTo p)::cs -> { path with commands = (p |> t |> Svg.MoveTo) :: cs } :: ps
                            | _ -> s.strand)
             | _ -> s.strand)
  ; start_anchor = s.start_anchor |> t
  ; end_anchor = s.end_anchor
  ; east_anchor = s.east_anchor }

///Returns a strand where the end point has been moved by the supplied vector. This is meant to be used to extend the strand's end domain. Therefore, the vector should always be horizontal, and it should be positive for upper segments, and negative for lower segments. Note that this function does not change the east anchor. The caller should change the east anchor if this is an upper strand.
let extend_end d s =
  let t = Svg.translate_point_by d in
  { strand = ( match s.strand |> List.rev with
             | path::ps -> (match path.commands |> List.rev with
                            | (Svg.LineTo p)::cs -> ({ path with commands = ((p |> t |> Svg.LineTo) :: cs) |> List.rev } :: ps) |> List.rev
                            | _ -> s.strand)
             | _ -> s.strand)
  ; start_anchor = s.start_anchor
  ; end_anchor = s.end_anchor |> t
  ; east_anchor = s.east_anchor }

///Returns a strand that has been extended (by a cross-gap length) on the start side. Needs to know whether the start side is east (i.e. lower strand) or west (i.e. upper strand). This may be used for example for a strand that's supposed to be bent and that is predicted to cross another overhang from an adjacent segment, so it needs extra length to prevent domain names from overlapping.
let strut_start len east s =
  let d, d_inv = if east then {Svg.x=len;Svg.y=0.},{Svg.x=0.-len;Svg.y=0.} else {Svg.x=0.-len;Svg.y=0.},{Svg.x=len;Svg.y=0.} in
  let es = extend_start d s |> translate_strand_by d_inv in
  if east then { es with east_anchor = es.east_anchor |> Svg.translate_point_by d } else es

///Returns a strand that has been extended (by a cross-gap length) on the end side. Needs to know whether the end side is east (i.e. upper strand) or west (i.e. lower strand). This may be used for example for a strand that's supposed to be bent and that is predicted to cross another overhang from an adjacent segment, so it needs extra length to prevent domain names from overlapping.
let strut_end len east s =
  let d, d_inv = if east then {Svg.x=len;Svg.y=0.},{Svg.x=0.-len;Svg.y=0.} else {Svg.x=0.-len;Svg.y=0.},{Svg.x=len;Svg.y=0.} in
  let es = extend_end d s |> translate_strand_by d_inv in
  if east then { es with east_anchor = es.east_anchor |> Svg.translate_point_by d } else es

///Represents a strand in the form of a half-segment. As such, the representation comprises up to three strands: a left overhang, a middle, and a right overhang. Note that this does not have a direction, so it may be upper (arrow on the right overhang) or lower (arrow in the left overhang). So, by "left" we mean "screen left". Note that if this is a hairpin half-segment, then "left" and "right" do not have the correct meaning, and should be considered as "upper" and "lower" instead. For left-hairpins, the left overhang is upper and the right overhang is lower. For right-hairpins, the left overhang is lower and the right overhang is upper.
type svg_halfseg =
  ///The left overhang strand (start for upper segments, end for lower segments or hairpins). For left-hairpins, this is the upper strand. For right-hairpins, this is the lower strand.
  { left_overhang: svg_strand option
  ///The middle strand (always horizontal).
  ; middle: svg_strand
  ///The right overhang strand (start for lower segments or hairpins, end for upper segments). For left-hairpins, this is the lower strand. For right-hairpins, this is the upper strand.
  ; right_overhang: svg_strand option }

///Extends a halfseg by the segment_skip length. The extension is done at the start of the left overhang, or at the start of the middle (if there is no left overhang). Note that this is only ever called on upper segments, and "east" is only ever true. So the exact meaning of "east" in this context is unclear, because the segment is actually extended to the west.
let extend_left_start east s =
  let d = if east then { Svg.x = -segment_skip; Svg.y = 0.0 } else { Svg.x = segment_skip; Svg.y = 0.0 } in
  let e = extend_start d in
  match s.left_overhang with
  | None -> { s with middle = s.middle |> e }
  | Some l -> { s with left_overhang = l |> e |> Some }

///Extends a halfseg by the segment_skip length. The extension is done at the start of the right overhang, or at the start of the middle (if there is no right overhang).
let extend_right_start east s =
  let d = if east then { Svg.x = -segment_skip; Svg.y = 0.0 } else { Svg.x = segment_skip; Svg.y = 0.0 } in
  let e = extend_start d in
  match s.right_overhang with
  | None -> { s with middle = s.middle |> e }
  | Some r -> { s with right_overhang = r |> e |> Some }

///Extends a halfseg by the segment_skip length. The extension is done at the end of the right overhang, or at the end of the middle (if there is no right overhang). Note that this is only ever called on upper segments, and "west" is only ever true. So the exact meaning of "west" in this context is unclear, because the segment is actually extended to the east.
let extend_right_end west s =
  let d = if west then { Svg.x = segment_skip; Svg.y = 0.0 } else { Svg.x = -segment_skip; Svg.y = 0.0 } in
  let e = extend_end d in
  match s.right_overhang with
  | None -> { s with middle = s.middle |> e }
  | Some r -> { s with right_overhang = r |> e |> Some }

///Extends a halfseg by the segment_skip length. The extension is done at the end of the left overhang, or at the end of the middle (if there is no left overhang).
let extend_left_end west s =
  let d = if west then { Svg.x = segment_skip; Svg.y = 0.0 } else { Svg.x = -segment_skip; Svg.y = 0.0 } in
  let e = extend_end d in
  match s.left_overhang with
  | None -> { s with middle = s.middle |> e }
  | Some l -> { s with left_overhang = l |> e |> Some }

///Joins two upper half-segs to form a new upper half-seg. The output is anchored to hs1.
let (--@) hs1 hs2 =
  let hs1, hs2 = hs1 |> extend_right_end true, hs2 |> extend_left_start true in
  let middle = hs1.middle /@ hs1.right_overhang /@ hs2.left_overhang -@ hs2.middle in
  { left_overhang = hs1.left_overhang
  ; middle = middle
  ; right_overhang = hs2.right_overhang >/ middle }

///Joins an upper halfseg and a right-hairpin halfseg to form a new right-hairpin halfseg. The output is anchored to hs1. Hairpin halfsegs are treated as lower, and this includes the output of this operator, so the left overhang of the hairpin becomes the left overhang of the output, while the left overhang of the upper becomes the right overhang of the output. Everything else becomes the new middle.
let (@--) hs1 hs2 =
  let hs1, hs2 = hs1 |> extend_right_end true, hs2 |> extend_right_start true in
  let middle = hs1.middle /@ hs1.right_overhang /@ hs2.right_overhang -@ hs2.middle in
  { left_overhang = hs2.left_overhang >/ middle
  ; middle = middle
  ; right_overhang = hs1.left_overhang }

///Joins a left-hairpin halfseg and an upper halfseg to form a new upper halfseg. The output is anchored to hs1. Hairpin halfsegs are treated as lower, therefore hs1 needs to be inverted (i.e. the left side of hs1 is joined to the left side of hs2, while the right side of hs1 becomes the left side of the output). Note that this operator produces an upper halfseg that includes a hairpin, but it's still considered upper.
let (--&) hs1 hs2 =
  let hs1, hs2 = hs1 |> extend_left_end true, hs2 |> extend_left_start true in
  let middle = hs1.middle /@ hs1.left_overhang /@ hs2.left_overhang -@ hs2.middle in
  { left_overhang = hs1.right_overhang
  ; middle = middle
  ; right_overhang = hs2.right_overhang >/ middle }

///Joins a lower (or right-hairpin) halfseg to a lower (or left-hairpin) halfseg to form a new lower halfseg (which may be interpreted as a hairpin). Everything is lower, so there is no need to invert anything, but the direction is right-to-left.
let (&--) hs1 hs2 =
  let hs1, hs2 = hs1 |> extend_left_end false, hs2 |> extend_right_start false in
  let middle = hs1.middle &- (hs1.left_overhang &/ (hs2.right_overhang &/ hs2.middle)) in
  { left_overhang = hs2.left_overhang
  ; middle = middle
  ; right_overhang = hs1.right_overhang >/ middle }

///Unused. Aligns hs2 with hs1 (based on the east anchors).
let (--<) hs1 hs2 =
  let t s = hs1.middle -< s in
  { left_overhang = hs2.left_overhang |> Lib.option_map2 t
  ; middle = hs2.middle |> t
  ; right_overhang = hs2.right_overhang |> Lib.option_map2 t }

///Aligns hs1 with hs2 (based on the east anchors).
let (>--) hs1 hs2 =
  let t s = s >- hs2.middle in
  { left_overhang = hs1.left_overhang |> Lib.option_map2 t
  ; middle = hs1.middle |> t
  ; right_overhang = hs1.right_overhang |> Lib.option_map2 t }

///Adds the arrowhead to a halfseg. Needs to know whether it is upper or lower.
let add_seg_arrow upper s =
  let a = add_arrow upper in
  if upper then
    match s.right_overhang with
    | None -> { s with middle = s.middle |> a }
    | Some r -> { s with right_overhang = r |> a |> Some }
  else
    match s.left_overhang with
    | None -> { s with middle = s.middle |> a }
    | Some l -> { s with left_overhang = l |> a |> Some }

///Adds the arrow to a left hairpin halfseg. Note that this is specifically for left hairpins, because they are special in that they are interpreted as lower strands but have their arrow pointing right (like upper strands). Right hairpins do not need special handling in this sense, because their arrow is pointing left, as is normal for lower strands.
let add_seg_arrow_hp s =
  let a = add_arrow true in
  match s.left_overhang with
  | None -> { s with middle = s.middle |> a }
  | Some l -> { s with left_overhang = l |> a |> Some }

///This type represents a part of a gate. The part can be a Two, which corresponds to a standard segment made of two half-segs. Or it can be a hairpin. If it's a hairpin, it's a single half-seg, and it can be on the left or on the right.
type gate_strands =
  ///Standard part. Has an upper halfseg, and a lower halfseg.
  | Two of svg_halfseg * svg_halfseg
  ///Hairpin part to the left. Both overhangs are to the right. This is interpreted as a lower strand: the field named left_overhang is the top overhang; the field named right_overhang is the bottom overhang.
  | OneL of svg_halfseg
  ///Hairpin part to the right. Both overhangs are to the left. This is interpreted as a lower strand: the field named left_overhang is the bottom overhang; the field named right_overhang is the top overhang.
  | OneR of svg_halfseg

///Orients a rendered left-to-right domain list according to whether it is part of an upper strand (if it isn't, it gets rotated so that it becomes right-to-left). Here g is an SVG group that contains a rendered domain list. If the shift flag is specified, the result is translated backwards by its length. The result is then translated by v.
let spin mode upper shift v g =
  let segment_sep = match mode with
                    | Complement -> segment_sep_complement
                    | Condensed -> segment_sep_condensed
                    | Nucleotides -> segment_sep_nucleotides
  let rg = if upper then g else g |> Svg.rotate_group_around_anchor "start" 180.0 in
  let sg = if shift then Svg.attach_group_to (g, "start") (rg, "end") else rg in
  let tg = sg |> Svg.translate_group_by v in
  if upper then tg else tg |> Svg.translate_group_by segment_sep

///Converts a domain list into an svg_strand. The east anchor is at the origin. The single flag indicates whether this is a single strand.
let melt_dom_list mode mapping single upper shift v ds =
  let rev = if upper then Lib.id else List.rev in
  // Determine the exact positioning of the label, depending on the mode and the strand type.
  let label = match (mode, single, upper) with
              | (Condensed, _, true) -> Some UpperInner
              | (Condensed, false, false) -> None
              | (Condensed, true, false) -> Some LowerInner
              | (Nucleotides, _, true) -> Some UpperInner
              | (Nucleotides, _, false) -> Some LowerInner
              | (_, _, true) -> Some UpperOuter
              | (_, _, false) -> Some LowerOuter
  let g = ds |> rev |> dom_list_to_svg mode mapping label |> spin mode upper shift v in
  { strand = g.content |> (function Svg.Paths ps -> ps | Svg.Raw _ -> [])
  ; start_anchor = Svg.get_anchor_point "start" g
  ; end_anchor = Svg.get_anchor_point "end" g
  ; east_anchor = Svg.origo }

///Converts a Strand.t into a halfseg. The halfseg is just a middle strand in this case.
let melt_strand mode mapping single s =
  let strand = s |> Strand.domains |> melt_dom_list mode mapping single (Strand.is_upper s) true Svg.origo |> add_arrow (Strand.is_upper s) in
  { left_overhang = None
  ; middle = strand
  ; right_overhang = None }

///Converts a domain list into an svg_strand. The east anchor is at the origin.
let melt_dom_list_o mode mapping single upper shift v = function [] -> None | ds -> melt_dom_list mode mapping single upper shift v ds |> Some

///Converts a domain list into a hairpin halfseg.
let melt_dom_list_hp mode mapping right v ds =
  let g = dom_list_to_hp_svg mode mapping ds |> spin mode right right v in
  { strand = g.content |> (function Svg.Paths ps -> ps | Svg.Raw _ -> [])
  ; start_anchor = Svg.get_anchor_point "start" g
  ; end_anchor = Svg.get_anchor_point "end" g
  ; east_anchor = Svg.origo }

///Converts a double-stranded domain sequence into two svg_strand (upper and lower).
let melt_shared mode mapping s =
  let su, sl = Domain.unstick_keep_doc_list s in
  let u = su |> melt_dom_list mode mapping false true false Svg.origo in
  let l = sl |> melt_dom_list mode mapping false false true Svg.origo in
  u, l

///Converts a Segment.t into a gate_strands.
let melt_segment mode mapping s = 
  match s with
  | Segment.Double (lb,lt,s,rt,rb) ->
    // Get two svg-strands in default position (i.e. start at the origin).
    let t, b = melt_shared mode mapping s in
    // Bottom-left: rotate it, do not shift it, leave the start at the origin.
    let mlb = lb |> melt_dom_list_o mode mapping true false false Svg.origo in
    // Top-left: do not rotate it, shift it, leave the start at the origin.
    let mlt = lt |> melt_dom_list_o mode mapping true true true Svg.origo in
    // Bottom-right: rotate it, shift it, put it at the end of the double strand.
    let mrb = rb |> melt_dom_list_o mode mapping true false true t.end_anchor in
    // Top-right: do not rotate it, do not shift it, put it at the end of the double strand.
    let mrt = rt |> melt_dom_list_o mode mapping true true false t.end_anchor in

    let hst = { left_overhang = mlt
              ; middle = t
              ; right_overhang = mrt } in
    let hsb = { left_overhang = mlb
              ; middle = b
              ; right_overhang = mrb } in
    Two (hst, hsb)
  | Segment.Hairpin (rb,rt,s,hp,Segment.Left) -> (* ignoring top *)
    let t, b = melt_shared mode mapping s in
    // Hairpin: do not rotate it, leave the start at the origin.
    let mhp = hp |> melt_dom_list_hp mode mapping false Svg.origo in
    // Bottom overhang: rotate it, shift it, put it at the end of the double strand.
    let mrb = rb |> melt_dom_list_o mode mapping true false true t.end_anchor in
    // Top overhang: do not rotate it, do not shift it, put it at the end of the double strand.
    let mrt = rt |> melt_dom_list_o mode mapping true true false t.end_anchor in
    // The hairpin is logically shifted to be considered as a bottom-strand. Therefore, the top overhang becomes the left overhang, the double strand becomes a single strand with the hairpin in the middle, and the bottom overhang becomes the right overhang.    <=  -->  -v-
    let hs = { left_overhang = mrt
             // Note that you can use either -@ or -& to join b and mhp, because the east anchor for middle will ultimately get set by t anyway.
             ; middle = b -@ mhp -@ t 
             ; right_overhang = mrb } in
    OneL hs
  | Segment.Hairpin (lb,lt,s,hp,Segment.Right) -> (* ignoring top *)
    let t, b = melt_shared mode mapping s in
    // Bottom overhang: rotate it, do not shift it, leave the start at the origin.
    let mlb = lb |> melt_dom_list_o mode mapping true false false Svg.origo in
    // Top overhang: do not rotate it, shift it, leave the start at the origin.
    let mlt = lt |> melt_dom_list_o mode mapping true true true Svg.origo in
    // Hairpin: rotate it, put it at the end of the double strand.
    let mhp = hp |> melt_dom_list_hp mode mapping true t.end_anchor in
    // The hairpin is logically shifted to be considered a bottom-strand. Therefore, the bottom overhang becomes the left overhand, the double strand becomes a single strand with the hairpin in the middle, and the top overhang becomes the right overhang.    =>  -->  -v-
    let hs = { left_overhang = mlb
             // Note that it doesn't matter whether you use -@ or -& to join t, mhp and b. It's a right-hairpin, so nothing will get anchored east of it anyway.
             ; middle = t -@ mhp -@ b
             ; right_overhang = mlt } in
    OneR hs

///Bends the left overhang, assuming this is a top strand.
let bend_upper_left s = { s with left_overhang = s.left_overhang |> Lib.option_map2 (bend_end true bend_angle) }
///Bends the left overhang, assuming this is a lower strand. Can also bend the bottom overhang of a right-hairpin.
let bend_lower_left s = { s with left_overhang = s.left_overhang |> Lib.option_map2 (bend_start false -bend_angle) }
///Bends the right overhang, assuming this is a top strand.
let bend_upper_right s = { s with right_overhang = s.right_overhang |> Lib.option_map2 (bend_start true -bend_angle) }
///Bends the right overhang, assuming this is a bottom strand. Can also bend the bottom overhang of a left-hairpin.
let bend_lower_right s = { s with right_overhang = s.right_overhang |> Lib.option_map2 (bend_end false bend_angle) }
///Bends the upper overhang, assuming this is a left hairpin (therefore, the upper overhang is the left overhang).
let bend_upper_left_hp s = { s with left_overhang = s.left_overhang |> Lib.option_map2 (bend_start true -bend_angle) }
///Bends the upper overhang, assuming this is a right hairpin (therefore, the upper overhang is the right overhang).
let bend_upper_right_hp s = { s with right_overhang = s.right_overhang |> Lib.option_map2 (bend_end true bend_angle) }

///Returns true if the strand has a left overhang.
let has_left s = Option.isSome s.left_overhang
///Returns true if the strand has a right overhang.
let has_right s = Option.isSome s.right_overhang

///Connects two segments with a lower join. Returns the new segment, plus maybe a loose strand. A loose strand happens when the join operation results in two upper strands. In that case, the rightmost upper strand becomes the upper strand of the new segment, while the leftmost upper strand is returned separately. This design is in support of connecting segments left-to-right, so that the leftmost loose strand, if present, won't need to be used again (because any further connection will be on the right).
let connect_lower mode (a:gate_strands) (b:gate_strands) =
  match a, b with
  | OneL _, OneR _ -> failwith "circular DNA" // i.e. two hairpins touching.
  | OneL b1, Two (t, b2) ->
    // If both segments also have upper overhangs (left overhang for the left hairpin) on the connecting sides, then they'll need to be extended a bit to prevent the domain names from overlapping after bending.
    let b1, t =
      match b1.left_overhang, t.left_overhang, b1.right_overhang, b2.left_overhang with
      | Some b1l, Some tl, None, None ->
        { b1 with left_overhang = b1l |> strut_start cross_gap false |> Some },
        { t with left_overhang = tl |> strut_end cross_gap true |> Some }
      | _ -> b1, t in
    // If we are using a mode that draws labels on the inside, then the upper overhangs will need to be extended a bit to prevent the domain names from overlapping with the horizontal strand.
    let b1 = if mode = Complement then b1 else match b1.left_overhang with None -> b1 | Some b1l -> { b1 with left_overhang = b1l |> strut_start inlabel_gap false |> Some }
    let t = if mode = Complement then t else match t.left_overhang with None -> t | Some tl -> { t with left_overhang = tl |> strut_end inlabel_gap true |> Some }
    let b = b2 &-- (b1 |> add_seg_arrow_hp |> bend_upper_left_hp) in
    Two (t |> bend_upper_left >-- b, b), []
  | Two (t, b1), OneR b2 ->
    // If both segments also have upper overhangs (right overhang for the right hairpin) on the connecting sides, then they'll need to be extended a bit to prevent the domain names from overlapping after bending.
    let t, b2 =
      match t.right_overhang, b2.right_overhang, b1.right_overhang, b2.left_overhang with
      | Some tr, Some b2r, None, None ->
        { t with right_overhang = tr |> strut_start cross_gap false |> Some },
        { b2 with right_overhang = b2r |> strut_end cross_gap true |> Some }
      | _ -> t, b2 in
    // If we are using a mode that draws labels on the inside, then the upper overhangs will need to be extended a bit to prevent the domain names from overlapping with the horizontal strand.
    let t = if mode = Complement then t else match t.right_overhang with None -> t | Some tl -> { t with right_overhang = tl |> strut_start inlabel_gap false |> Some }
    let b2 = if mode = Complement then b2 else match b2.right_overhang with None -> b2 | Some b2l -> { b2 with right_overhang = b2l |> strut_end inlabel_gap true |> Some }
    OneR ((bend_upper_right_hp b2) &-- b1), [t |> add_seg_arrow true |> bend_upper_right]
  | Two (t1, b1), Two (t2, b2) ->
    // If both segments also have upper overhangs on the connecting sides, then they'll need to be extended a bit to prevent the domain names from overlapping after bending.
    let t1, t2 =
      match t1.right_overhang, t2.left_overhang, b1.right_overhang, b2.left_overhang with
      | Some t1r, Some t2l, None, None ->
        { t1 with right_overhang = t1r |> strut_start cross_gap false |> Some },
        { t2 with left_overhang = t2l |> strut_end cross_gap true |> Some }
      | _ -> t1, t2 in
    // If we are using a mode that draws labels on the inside, then the upper overhangs will need to be extended a bit to prevent the domain names from overlapping with the horizontal strand.
    let t1 = if mode = Complement then t1 else match t1.right_overhang with None -> t1 | Some t1l -> { t1 with right_overhang = t1l |> strut_start inlabel_gap false |> Some }
    let t2 = if mode = Complement then t2 else match t2.left_overhang with None -> t2 | Some t2l -> { t2 with left_overhang = t2l |> strut_end inlabel_gap true |> Some }
    let b = b2 &-- b1 in
    Two (t2 |> bend_upper_left >-- b, b), [t1 |> add_seg_arrow true |> bend_upper_right]
  | _ -> failwith "malformed species" // i.e. with a right-hairpin on the left or vice versa.

///Connects two segments with an upper join. Returns the new segment, plus maybe a loose strand. A loose strand happens when the join operation results in two lower strands. In that case, the rightmost lower strand becomes the lower strand of the new segment, while the leftmost lower strand is returned separately. This design is in support of connecting segments left-to-right, so that the leftmost loose strand, if present, won't need to be used again (because any further connection will be on the right).
let connect_upper mode (a:gate_strands) (b:gate_strands) =
  match a, b with
  | OneL _, OneR _ -> failwith "circular DNA" // i.e. two hairpins touching.
  | OneL b1, Two (t, b2) ->
    // If both segments also have lower overhangs (right overhang for the left hairpin) on the connecting sides, then they'll need to be extended a bit to prevent the domain names from overlapping after bending.
    let b1, b2 =
      match b1.right_overhang, b2.left_overhang, b1.left_overhang, t.left_overhang with
      | Some b1r, Some b2l, None, None ->
        { b1 with right_overhang = b1r |> strut_end cross_gap false |> Some },
        { b2 with left_overhang = b2l |> strut_start cross_gap true |> Some }
      | _ -> b1, b2 in
    // If we are using a mode that draws labels on the inside, then the upper overhangs will need to be extended a bit to prevent the domain names from overlapping with the horizontal strand.
    let b1 = if mode = Complement then b1 else match b1.right_overhang with None -> b1 | Some b1l -> { b1 with right_overhang = b1l |> strut_end inlabel_gap false |> Some }
    let b2 = if mode = Complement then b2 else match b2.left_overhang with None -> b2 | Some b2l -> { b2 with left_overhang = b2l |> strut_start inlabel_gap true |> Some }
    let t2 = (b1 |> bend_lower_right) --& t in 
    Two (t2, b2 |> add_seg_arrow false |> bend_lower_left >-- t2), []
  | Two (t, b1), OneR b2 ->
    // If both segments also have lower overhangs (left overhang for the right hairpin) on the connecting sides, then they'll need to be extended a bit to prevent the domain names from overlapping after bending.
    let b1, b2 =
      match b1.right_overhang, b2.left_overhang, t.right_overhang, b2.right_overhang with
      | Some b1r, Some b2l, None, None ->
        { b1 with right_overhang = b1r |> strut_end cross_gap false |> Some },
        { b2 with left_overhang = b2l |> strut_start cross_gap true |> Some }
      | _ -> b1, b2 in
    // If we are using a mode that draws labels on the inside, then the upper overhangs will need to be extended a bit to prevent the domain names from overlapping with the horizontal strand.
    let b1 = if mode = Complement then b1 else match b1.right_overhang with None -> b1 | Some b1l -> { b1 with right_overhang = b1l |> strut_end inlabel_gap false |> Some }
    let b2 = if mode = Complement then b2 else match b2.left_overhang with None -> b2 | Some b2l -> { b2 with left_overhang = b2l |> strut_start inlabel_gap true |> Some }
    OneR (t @-- (b2 |> add_seg_arrow false |> bend_lower_left)), [b1 |> bend_lower_right]
  | Two (t1, b1), Two (t2, b2) ->
    // If both segments also have lower overhangs on the connecting sides, then they'll need to be extended a bit to prevent the domain names from overlapping after bending.
    let b1, b2 =
      match b1.right_overhang, b2.left_overhang, t1.right_overhang, t2.left_overhang with
      | Some b1r, Some b2l, None, None ->
        { b1 with right_overhang = b1r |> strut_end cross_gap false |> Some },
        { b2 with left_overhang = b2l |> strut_start cross_gap true |> Some }
      | _ -> b1, b2 in
    // If we are using a mode that draws labels on the inside, then the upper overhangs will need to be extended a bit to prevent the domain names from overlapping with the horizontal strand.
    let b1 = if mode = Complement then b1 else match b1.right_overhang with None -> b1 | Some b1l -> { b1 with right_overhang = b1l |> strut_end inlabel_gap false |> Some }
    let b2 = if mode = Complement then b2 else match b2.left_overhang with None -> b2 | Some b2l -> { b2 with left_overhang = b2l |> strut_start inlabel_gap true |> Some }
    let t = t1 --@ t2 in
    Two (t, (b2 |> add_seg_arrow false |> bend_lower_left) >-- t), [b1 |> bend_lower_right]
  | _ -> failwith "malformed species" // i.e. with a right-hairpin on the left or vice versa.

///Produces the SVG intermediate structure for a Gate.t. Remember that a Gate.t is a list of upper-linked lists of lower-linked segments, left to right.
let rec melt_gate mode mapping g =
  ///Need to spread the left side of the first segment iff it has overhangs both top and bottom.
  let should_spread_left =
    match Gate.first_segment g with
    | Segment.Double (lb, lt, _, _, _)
    | Segment.Hairpin (lb, lt, _, _, Segment.Right) -> lb <> [] && lt <> []
    | _ -> false in
  ///Need to spread the right side of the last segment iff it has overhangs both top and bottom.
  let should_spread_right =
    match Gate.last_segment g with
    | Segment.Double (_, _, _, rt, rb)
    | Segment.Hairpin (rb, rt, _, _, Segment.Left) -> rb <> [] && rt <> []
    | _ -> false in
  ///Converts all of the specified Segment.t into single gate_strands, and joins them with the specified gate_strand, left-to-right, with lower joins.
  let collect_lower gs segs =
    let gss = List.map (melt_segment mode mapping) segs in
    let f (prev_gs, ss) gs =
      let (next_gs, extra_ss) = connect_lower mode prev_gs gs in
      next_gs, extra_ss @ ss in
    Lib.fold_left f gs gss in
  ///Converts all of the specified Segment.t lists into lower-joined gate_strands, and joins them with the specified gate_strand, left-to-right, with upper joins.
  let collect_upper gs segss =
    let f (prev_gs, ss) = function
      | [] -> prev_gs, ss
      | seg::segs ->
        let gs = melt_segment mode mapping seg in
        let (next_gs, extra_ss) = connect_upper mode prev_gs gs in
        collect_lower (next_gs, extra_ss @ ss) segs in
    Lib.fold_left f gs segss in
  ///Adds the left-pointing arrow to a gate_strands (whether it is a hairpin or not).
  let add_arrow_left s =
    match s with
    | OneL _ -> s
    | OneR l -> l |> add_seg_arrow false |> OneR
    | Two (u, l) -> Two (u, l |> add_seg_arrow false) in
  ///Adds the right-pointing arrow to a gate_strands (whether it is a hairpin or not).
  let add_arrow_right s =
    match s with
    | OneL l -> l |> add_seg_arrow_hp |> OneL
    | OneR _ -> s
    | Two (u, l) -> Two (u |> add_seg_arrow true, l) in
  ///Bends the overhangs of first segment. These will be left-overhangs.
  let spread_left s =
    if should_spread_left then
      match s with
      // If the first segment is a left-hairpin, take no action, because it cannot have left-overhangs.
      | OneL _ -> s
      // If the first segment is a right-hairpin, then bend the left overhang (lower), but also bend the right overhang, because in a right-hairpin that's visually the upper left.
      | OneR l -> l |> bend_lower_left |> bend_upper_right_hp |> OneR
      //| OneR _ -> s
      | Two (u, l) -> Two (u |> bend_upper_left, l |> bend_lower_left)
    else s in
  let spread_right s =
    if should_spread_right then
      match s with
      | OneL l -> l |> bend_lower_right |> bend_upper_left_hp |> OneL
      | OneR _ -> s
      //| OneR l -> l |> bend_lower_left |> bend_upper_right_hp |> OneR
      | Two (u, l) -> (u |> bend_upper_right, l |> bend_lower_right) |> Two
    else s in
  match g with
  | [] -> []
  | []::segs -> melt_gate mode mapping segs
  | (seg::segs)::segss ->
    let lowers = collect_lower (melt_segment mode mapping seg |> add_arrow_left |> spread_left, []) segs in
    let gs, ss = collect_upper lowers segss in
    match gs |> add_arrow_right |> spread_right with
    | OneL l -> l::ss
    | OneR l -> l::ss
    | Two (u, l) -> u::l::ss

///Converts a svg_halfseg to an SVG group.
let halfseg_to_svg hs =
  let l = match hs.left_overhang with None -> [] | Some s -> s.strand in
  let r = match hs.right_overhang with None -> [] | Some s -> s.strand in
  { Svg.name = ""
  ; Svg.content = l @ hs.middle.strand @ r |> Svg.Paths
  ; Svg.anchors = []
  ; Svg.debug_anchors = false
  ; Svg.sub_groups = []
  ; Svg.offset = None }

///Produces the SVG representation of a species. Requires the rendering mode, and the sequences mapping (for nucleotide mode).
let melt mode mapping (s:Species.t) =
    let group_of_strand st = [st |> melt_strand mode mapping true |> halfseg_to_svg] |> Svg.emerge_groups in
    let group_of_gate = melt_gate mode mapping >> List.map halfseg_to_svg >> Svg.emerge_groups in
    match s with 
    | Species.STRAND st -> st |> group_of_strand
    | Species.GATE g -> g |> group_of_gate
    | Species.ORIGAMI o ->
      let gs, ss = Origami.origami_species o in
      let strands = ss |> List.map group_of_strand in
      let gates = gs |> List.map group_of_gate in
      let b = (strands @ gates) |> Svg.stackv origami_sep species_skip in
      let border_label = "" in
      let border_inset = (String.length border_label * letterwidth |> float) / 2.0 in
      let border_offset = { Svg.x = 2.0; Svg.y = 2.0 } in
      let border_dim = b.box_dim |> Svg.translate_point_by (border_offset |> Svg.scale_by -1.0) in
      { box_group = { b.box_group with
                        content = [ { Svg.path_class = baseClass + " origami-border" |> Some
                                    ; Svg.commands = Svg.rounded_box border_offset border_dim origami_round
                                    ; Svg.path_label = { Svg.label_text = border_label
                                                       ; Svg.label_anchor = Svg.sub b.box_dim origami_sep |> Svg.translate_point_by { x = -border_inset; y = 0.0 }
                                                       ; Svg.label_class = Some baseClass
                                                       ; Svg.letter_width = Svg.default_letter_width
                                                       ; Svg.label_dir = Svg.sub b.box_dim origami_sep |> Svg.translate_point_by { x = (*1*)0.0 - border_inset; y = 0.0 } } |> Some } ]
                                  |> Svg.Paths }
      ; box_dim = b.box_dim }
    | _ -> raise (new System.Exception("The visualisation of complex and unknown species is not yet implemented"))

///Produces the CSS style for the SVG produced by this module. Requires the set of domain->color associations. The rest of the styles are fixed.
let style cm =
  let dom_styles = cm |> Lib.string_of_list (fun (id, col) -> "path."+baseClass+"."+id + " { stroke: " + col + " } text."+baseClass+"."+id+" { fill: " + col + " }") " " in
  let base_style = "g { fill: none; } path."+baseClass+" { stroke: silver; stroke-width: " + Svg.display_float linewidth + "; stroke-linejoin: round; } text."+baseClass+" { fill: black; stroke-width: 0; font-family: Verdana,Arial,sans-serif; font-size: " + string (int fontsize) + "px; text-anchor: middle; } text.fixedwidth { font-family:Lucida Console,monospace }"
  let tether_style = "path."+baseClass+".tether-pin { stroke : black }"
  base_style + " " + dom_styles + " " + tether_style
  
let palette = [ "red"; "green"; "blue"; "orange"; "purple"
              ; "cyan"; "brown"; "magenta"; "dodgerblue"; "orchid"
              ; "lawngreen"; "lightskyblue"; "darksalmon"; "teal"; "rosybrown"
              ; "navajowhite"; "olive"; "darkseagreen"; "darkgoldenrod"; "pink" ]

///Takes a partial map of domains->colors and a complete list of species, and returns a complete map of domain IDs->colors.
let dom_map (dcs:Hashtable.t<string,string>) species_list =
  let dom_name d = d |> Domain.unstar |> Domain.to_string
  let (_,taken_colours) = Hashtable.to_list dcs |> List.unzip
  let free_colours = palette |> List.filter (fun c -> List.exists ((=) c) taken_colours |> not) in
  let dom_counter = ref 0 in
  let ret = Hashtable.empty() in
  let add d =
    let d_id = d |> dom_id in
    match d_id |> Hashtable.tryFind ret with
    | None ->
      let name = d |> dom_name in
      let colour = match name |> Hashtable.tryFind dcs with
                   | None ->
                     let c = free_colours.[!dom_counter] in
                     dom_counter := (!dom_counter + 1) % free_colours.Length;
                     c
                   | Some c -> c in
      Hashtable.add ret d_id colour
    | Some _ -> ()
  let xx = species_list
            |> List.collect Species.domains
            |> List.filter Domain.is_toehold
            |> Lib.remove_duplicates (=)
  List.iter add xx;
  ret
  
///Returns the CSS style for the SVG representation of the given species. You can pass a partial map of domains->colors; the rest of the domains will get assigned colors automatically.
let species_style dc sp =
  let dc = dom_map dc sp
  dc |> Hashtable.to_list |> style

///Returns an SVG representation of a species, with the specified rendering mode.
let species_to_svg_mode (mode:renderer_mode) (mapping:Sequence.mapping) s = s |> melt mode mapping
///Returns an SVG representation of a species, with the default rendering mode.
let species_to_svg s = species_to_svg_mode Complement Sequence.empty s