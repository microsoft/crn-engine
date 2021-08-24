// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.BranchesRenderer

open System
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Svg
open Microsoft.Research.DNA
open Microsoft.Research.DNA.Options
open Microsoft.Research.DNA.LogicDSD

// Abstract representation suitable for rendering algorithms based on a double-stranded branching tree structure. Note that this does not use DSD species, so that it can also be used for RulesDSD.

// Some configuration flags follow.

/// If true, bulges are rendered as lines, rather than curves.
let bulge_lines = false
/// If true, shows key anchors for debugging purposes.
let debug_anchors = false

/// Type that represents a domain instance. Note that the color does not appear here, because it is part of the style.
type DDomain = {
    name: string;
    nucleotides: Sequence.t;
    toehold: bool;
    complemented: bool;
    tether: string option;
    pseudoknot: int option;
} with
  member this.GetComplemented() = { this with complemented = not this.complemented; nucleotides = Sequence.complement this.nucleotides |> List.rev }
  override this.ToString() = this.name + (if this.toehold then "^" else "") + (if this.complemented then "*" else "") + (match this.pseudoknot with Some s -> "!"+s.ToString() | None -> "")
end

/// The type of a strand.
type DStrandKind = TopStrand | BottomStrand | DoubleStrand | LeftHairpinStrand | RightHairpinStrand

/// Type that represents a strand, i.e. a sequence of domains.
type DStrand = {
    kind: DStrandKind;
    domains: DDomain list;
} with
  override this.ToString() = let rec add s domains = match domains with [] -> s | dom::rest -> add (s+" "+dom.ToString()) rest in add "" this.domains
let empty_dstrand={kind=DoubleStrand;domains=[]}

/// The type of connection between two segments (forming a gate). The double link (not available in ClassicDSD) implies that any overhangs on that side become bulges.
type DSegmentConnection = TopLink | BottomLink | DoubleLink
let conn_to_string conn = match conn with | None -> " " | Some TopLink -> "-" | Some BottomLink -> "_" | Some DoubleLink -> "="

/// Type that represents a segment, i.e. a double strand that may or may not have overhangs and/or a hairpin. If the segment is part of a gate, and is not the last segment of the gate, then it also indicates the type of connection it has to the next segment in the gate.
type DSegment = {
    top_left: DStrand option;
    top_right: DStrand option;
    bottom_left: DStrand option;
    bottom_right: DStrand option;
    double_strand: DStrand;
    hairpin_left: DStrand option;
    hairpin_right: DStrand option;
    connection: DSegmentConnection option;
} with
  override this.ToString() =
    let tl = match this.top_left with None -> None | Some s -> "tl:"+s.ToString() |> Some
    let tr = match this.top_right with None -> None | Some s -> "tr:"+s.ToString() |> Some
    let bl = match this.bottom_left with None -> None | Some s -> "bl:"+s.ToString() |> Some
    let br = match this.bottom_right with None -> None | Some s -> "br:"+s.ToString() |> Some
    let hl = match this.hairpin_left with None -> None | Some s -> "hl:"+s.ToString() |> Some
    let hr = match this.hairpin_right with None -> None | Some s -> "hr:"+s.ToString() |> Some
    let ds = "ds:"+this.double_strand.ToString() |> Some
    String.Join(";",List.choose (fun x->x) [ds;hl;tl;bl;tr;br;hr])+(conn_to_string this.connection)
end
let empty_dsegment = {top_left=None;top_right=None;bottom_left=None;bottom_right=None;double_strand=empty_dstrand;hairpin_left=None;hairpin_right=None;connection=None}

///Type that represents a gate. It's just a list of segments. The types of the connections between segments are declared within the segment type.
type DGate = {
  left_connection: DSegmentConnection option;
  segments: DSegment list;
} with
  override this.ToString() =
    (conn_to_string this.left_connection)+(String.Join("",this.segments |> (fun seg -> seg.ToString())))
end

/// Type that represents a branch. In this representation, the species is tree-like, which means that the branching structure is directed. Therefore, a gate can have children gates. This is represented in this type.
type DBranch = {
    gate: DGate;
    children: DBranch list;
}

/// Union type of objects that may be parts of an origami. In this representation, species that are not origami can be either a single strand, or a branch. If it is a branch, it may be as simple as a single gate with a single segment with just a double strand of a single domain - but it's still considered a branch object.
type DOrigamiElement = Strand of DStrand | Branch of DBranch

/// Type that represents an origami. It's just a list of origami elements.
type DOrigami = DOrigamiElement list


// "Branches" SVG renderer. The rendering algorithm works by creating a "box" for each DSegment (or isolated DStrand). Each box has anchor points which are used to align it with the boxes representing adjacent segments. The composition of these boxes results in a container box, representing a DGate and having its own anchor points. In order to render a DBranch, the DGate box is positioned horizontally, while other DBranches are rendered recursively and then positioned at an appropriate angle and connected to the DGate box. DOrigamis are rendered as stacked objects.

/// The style for a branch join.
let branch_join_style = "stroke-dasharray:4"
/// The style for a short branch join.
let short_branch_join_style = ""
/// The length below which a branch join is considered to be short (uses a different style).
let short_branch_join = 16.0
/// The distance between the baseline and the top or bottom strands.
let half_separation = 4.0
/// The assumed font size; note that this is actually reported in the style.
let font_size = 15.0
/// The distance between the strand and the domain labels.
let label_sep = 11.0
/// The distance between the strand and the domain labels, if rotate_labels is true (which causes the Svg layer not to apply normalisation, which changes the baseline).
let label_sep_rotated = 7.0
/// Additional vertical offset for labels.
let label_baseline = 5.0
/// Additional vertical offset for labels, in nucleotide mode (different font).
let label_baseline_nucleotides = 4.0
/// Standard width of a character for the purpose of determining domain name length.
let letter_width = 8.0
/// Letter width multiplier for nucleotides in a hairpin.
let nucleotide_hairpin_factor = 1.4
/// Width of a character for the name of a pseudoknot domain (bold).
let pseudoknot_letter_width = 9.0
/// Extra padding length for domains.
let domain_pad = letter_width * 4.0
/// Extra padding length for domains on an arc.
let arc_domain_pad = letter_width * 2.0
/// The angle formed by an arrow terminator.
let arrow_angle = Math.PI / 6.0
/// The length of an arrowhead.
let arrow_length = 15.0
/// The thickness of the strand.
let line_width = 2.5
/// Extra strand length drawn, for the purpose of joining domains at angles.
let margin = 0.5
/// The length of the connection extension between strands.
let extension_length = 8.0
/// The angle for overhangs.
let overhang_angle = 45.0
/// This CSS class will be added to all elements of the picture, and can then be used for styling.
let base_class = "svgdsd"
let sanitize_id (id:string) = id.Replace("_","__").Replace('\'','_')
/// The CSS id for a given domain.
let dom_id d = "dom_"+(sanitize_id d.name)
/// The extra distance from the midpoint for bulges.
let bulge_sep = 16.0
/// The extra length for an overhang that crosses.
let strut_len = letter_width * 5.0
///The distance between species in an origami.
let species_skip = 10.0
///The margin around species in an origami.
let origami_sep = { Svg.x = 16.0; Svg.y = 16.0 }
///The size of the origami box corners.
let origami_round = 32.0

let make_label_text (nucleotides:bool) (d:DDomain) =
  match d.tether with
  | Some s -> s
  | _ ->
    let text = if nucleotides then Sequence.string_of_sequence d.nucleotides else d.name
    let text = if d.complemented && not nucleotides then (text+"*") else text
    let text = text + match d.pseudoknot with Some s -> "!"+s.ToString() | None -> ""
    text

/// This function produces the CSS class for the given domain.
let get_domain_class (nucleotides:bool) (d:DDomain) =
  let domain_class = base_class
  let domain_class = if nucleotides then (domain_class + " fixedwidth") else domain_class
  let domain_class = if d.toehold then (domain_class + " " + dom_id d) else domain_class
  let domain_class = if d.pseudoknot.IsSome then domain_class + " pseudoknot" else domain_class
  domain_class

/// Produces a Svg.path object for a single-strand linear domain (not a hairpin). This function needs to be provided with additional context. The length_function provides a method to get the desired length of the domain, any left extension, and any right extension. The voffset and hoffset are the vertical and horizontal offsets respectively. The upper_label flag indicates whether the label needs to be positioned above the path (if false, it gets positioned below the path). The left_arrow flag indicates that an arrow needs to be drawn on the left end; the right_arrow flag indicates that an arrow needs to be drawn on the right end. This function returns one or two path objects (in case of tethers), plus the rightmost x coordinate of the domain (which can be used as the horizontal offset for the next domain in a sequence).
let ddomain_to_path (mode:renderer_mode) (rotate_labels:bool) (length_function:DDomain->(float*float*float)) (voffset:float) (hoffset:float) (upper_label:bool) (left_arrow:bool) (right_arrow:bool) (d:DDomain) : Svg.path list * float =
  let nucleotides = mode = renderer_mode.Nucleotides
  let label_baseline = if nucleotides then label_baseline_nucleotides else label_baseline
  let make_label_text = make_label_text nucleotides
  let get_domain_class = get_domain_class nucleotides
  // Determine the CSS class.
  let domain_class = get_domain_class d
  // Determine the domain length (based on the text, +1 for the complemented marker).
  let (domain_length,left_extension,right_extension) = length_function d
  let total_domain_length = domain_length+left_extension+right_extension
  // Create the SVG path commands. May start with a left arrow or not.
  let commands = if left_arrow then [MoveTo{x=hoffset-margin+Math.Cos(arrow_angle)*arrow_length;y=voffset+Math.Sin(arrow_angle)*arrow_length};LineTo{x=hoffset;y=voffset}] else [MoveTo{x=hoffset-margin;y=voffset}]
  // Draw the line.
  let commands = commands@[LineTo{x=hoffset+total_domain_length+margin;y=voffset}]
  // May end with a right arrow or not.
  let commands = if right_arrow then commands@[LineTo{x=hoffset+total_domain_length+margin-Math.Cos(arrow_angle)*arrow_length;y=voffset-Math.Sin(arrow_angle)*arrow_length}] else commands
  // Determine where to locate the label. Horizontally it'll be in the middle; vertically, it may be above or below.
  let rotated_baseline = (not nucleotides && rotate_labels) || (nucleotides&&not upper_label)
  let label_sep = if rotated_baseline then label_sep_rotated else label_sep
  let label_anchor =
    { // Horizontally, it will be in the middle. In nucleotides mode, I have to ignore the extension length, because I want the nucleotides to be aligned.
      x = if nucleotides then
            hoffset + (domain_length/2.0) + left_extension
          else
            hoffset + (total_domain_length/2.0)
      // Verticall
      y = if nucleotides then
            voffset + if upper_label then label_sep else -label_baseline - label_sep
          else
            voffset + (if rotated_baseline then 0.0 else label_baseline) + (if upper_label then -label_sep else label_sep)
    }
  // Determine the label text. This is the domain name, with the complement marker.
  let label_text = make_label_text d
  // Create the label object.
  let label = { label_text = label_text
                label_anchor = label_anchor
                label_class = Some domain_class
                letter_width = if d.pseudoknot.IsSome then pseudoknot_letter_width else letter_width
                label_dir = Svg.translate_point_by {x=(if (nucleotides&&upper_label)||(not nucleotides&&(upper_label||not rotate_labels)) then 10.0 else -10.0);y=0.0} label_anchor } |> Some
  let main_path = { path_class = Some domain_class
                    commands = commands
                    path_label = label }
  if d.tether.IsSome then
    let tether_p = {x=hoffset+total_domain_length/2.0;y=voffset}
    let tether_path= { Svg.path_class = base_class + " tether-pin" |> Some
                     ; Svg.commands = [ Svg.MoveTo (tether_p |> Svg.translate_point_by { x = 0.0; y = 3.0})
                                      ; Svg.ArcTo ( tether_p |> Svg.translate_point_by { x = 0.0; y = -3.0}
                                                  , { rx = 3.0; ry = 3.0; large_flag = true; sweep_flag = false; angle = 0.0 })
                                      ; Svg.ArcTo ( tether_p |> Svg.translate_point_by { x = 0.0; y = 3.0}
                                                  , { rx = 3.0; ry = 3.0; large_flag = true; sweep_flag = false; angle = 0.0 }) ]
                     ; Svg.path_label = None } in
    // Return the path objects, plus the horizontal offset for the next domain in the sequence.
    [main_path;tether_path], hoffset + total_domain_length
  else
    // Return the path object, plus the horizontal offset for the next domain in the sequence.
    [main_path], hoffset + total_domain_length

/// Produces a Svg.path object for a single-strand arc domain (a hairpin domain or a bulge). The center is on the origin. These domains never have extensions or arrows. This function returns one or two path objects (in case of tethers), plus the final coordinate for the arc (which can be used as the starting point for the next domain in a sequence). The side flags controls the sweep flag of the arc (on which side the arc is drawn).
let ddomain_to_arc (mode:renderer_mode) (rotate_labels:bool) (radius:float) (start_point:Svg.point) (angle:float) (d:DDomain) (side:bool) : Svg.path list * Svg.point =
  let nucleotides = mode = renderer_mode.Nucleotides
  let label_baseline = if nucleotides then label_baseline_nucleotides else label_baseline
  let make_label_text = make_label_text nucleotides
  let get_domain_class = get_domain_class nucleotides
  // Determine the CSS class.
  let domain_class = get_domain_class d
  // Determine the end point. Remember that in the context of trigonometric functions the Y-axis points upwards, while in the context of SVG rendering, the Y axis points downwards. 
  let start_angle = Math.Asin(-start_point.y/radius);
  let start_angle = if start_point.x < 0.0 then Math.PI - start_angle else start_angle
  // Arc domains are always rendered clockwise, so "angle" here should be negative.
  let end_angle = start_angle+angle;
  let end_point = {x=radius*Math.Cos(end_angle);y= -radius*Math.Sin(end_angle)}
  // Create the Svg.arc object, which contains the SVG parameters for the arc.
  let arc = { rx = radius
              ry = radius
              large_flag = angle > Math.PI || angle < -Math.PI
              sweep_flag = side
              angle = 0.0 }
  // Determine the label text. This is the domain name, with the complement marker.
  let label_text = make_label_text d
  let paths =
    if nucleotides then
      // The angular length of the domain pad.
      let angular_pad = arc_domain_pad / radius / 2.0
      // The angular length that can be used for each nucleotide.
      let nucleotide_angle = (if angle > 0.0 then angle - 2.0 * angular_pad else angle + 2.0 * angular_pad) / float label_text.Length
      let end_angle = end_angle - angular_pad - (nucleotide_angle / 2.0)
      let make_letter i n =
        let label_dist = radius - label_sep
        let label_angle = end_angle - nucleotide_angle * float i
        let label_anchor = {x=label_dist*Math.Cos(label_angle);y= -label_dist*Math.Sin(label_angle)}
        let label_dir = Svg.translate_point_by {x=10.0*Math.Cos(label_angle-Math.PI/2.0);y= -10.0*Math.Sin(label_angle-Math.PI/2.0)} label_anchor
        let label = { label_text = string n
                      label_anchor = label_anchor
                      label_class = Some domain_class
                      letter_width = letter_width
                      label_dir = label_dir }
        { path_class = None
          commands = []
          path_label = Some label }
      let letter_paths = Seq.mapi make_letter label_text |> Seq.toList
      { path_class = Some domain_class
        commands = [MoveTo start_point;ArcTo(end_point,arc)]
        path_label = None }::letter_paths
    else
      // Determine placement for the label. It should appear at an angular position in the middle of the arc, at a distance from the arc.
      let label_sep = if rotate_labels then label_sep_rotated else label_sep
      let label_angle = start_angle+angle/2.0
      let label_dist = radius+label_sep
      let (label_anchor,label_dir) =
        if rotate_labels then
          let label_anchor = {x=label_dist*Math.Cos(label_angle);y= -label_dist*Math.Sin(label_angle)}
          let label_dir = {x=(label_dist+10.0)*Math.Cos(label_angle);y= -(label_dist+10.0)*Math.Sin(label_angle)}
          let label_dir = Svg.rotate_point_around label_anchor 90.0 label_dir
          (label_anchor,label_dir)
        else
          let label_xoffset = letter_width * float (label_text.Length-1) / 2.0
          let label_xoffset = if Math.Cos(label_angle) > 0.0 then label_xoffset else -label_xoffset
          let label_xoffset = if Math.Abs(label_dist*Math.Sin(label_angle)) > radius+(label_sep/2.0) then 0.0 else label_xoffset
          let label_anchor = {x=label_dist*Math.Cos(label_angle)+label_xoffset;y= -label_dist*Math.Sin(label_angle)+label_baseline}
          let label_dir = Svg.translate_point_by {x=10.0;y=0.0} label_anchor
          (label_anchor,label_dir)
      // Create the label object.
      let label = { label_text = label_text
                    label_anchor = label_anchor
                    label_class = Some domain_class
                    letter_width = if d.pseudoknot.IsSome then pseudoknot_letter_width else letter_width
                    label_dir = label_dir }
      [{ path_class = Some domain_class
         commands = [MoveTo start_point;ArcTo(end_point,arc)]
         path_label = Some label }]
  if d.tether.IsSome then
    let tether_angle = (start_angle+end_angle)/2.0
    let tether_point = {x=radius*Math.Cos(tether_angle);y= -radius*Math.Sin(tether_angle)}
    let tether_path= { Svg.path_class = base_class + " tether-pin" |> Some
                     ; Svg.commands = [ Svg.MoveTo (tether_point |> Svg.translate_point_by { x = 0.0; y = 3.0})
                                      ; Svg.ArcTo ( tether_point |> Svg.translate_point_by { x = 0.0; y = -3.0}
                                                  , { rx = 3.0; ry = 3.0; large_flag = true; sweep_flag = false; angle = 0.0 })
                                      ; Svg.ArcTo ( tether_point |> Svg.translate_point_by { x = 0.0; y = 3.0}
                                                  , { rx = 3.0; ry = 3.0; large_flag = true; sweep_flag = false; angle = 0.0 }) ]
                     ; Svg.path_label = None } in
    // Return the path objects, plus the horizontal offset for the next domain in the sequence.
    tether_path::paths, end_point
  else
    // Return the path object, plus the end point (which is the start point for the next domain in the sequence).
    paths, end_point

/// Renders a DStrand to the intermediate Svg representation. This function needs to be provided with some additional context. The left_connection parameter indicates whether this strand is used to connect to another segment on the left, and the type of connection. The right_connection parameter indicates whether this strand is used to connect to another segment on the right, and the type of connection. The no_left_arrow flag indicates that the strand should never have a left arrow, even if it is a bottom or double strand (e.g. because it's a double with a bottom left overhang, or a bottom right overhang). The no_right_arrow flag indicates that the strand should never have a right arrow, even if it is a top or double strand (e.g. because it's a double with a top right overhang, or a top left overhang).
let dstrand_to_group (mode:renderer_mode) (rotate_labels:bool) (left_connection:DSegmentConnection option) (left_strut:bool) (right_connection:DSegmentConnection option) (right_strut:bool) (no_left_arrow:bool) (no_right_arrow:bool) (strand:DStrand) : Svg.group =
  let nucleotides = mode = renderer_mode.Nucleotides
  let make_label_text = make_label_text nucleotides
  let ddomain_to_path = ddomain_to_path mode rotate_labels
  let ddomain_to_arc = ddomain_to_arc mode rotate_labels
  let half_separation = if nucleotides then half_separation + label_sep else half_separation
  // Render the strand. The exact algorithm depends on the strand kind.
  match strand.kind with
  | TopStrand ->
      // Top strand: render as a straight sequence. The last domain has an arrow.
      let top_domain (o:float) (i:int,d:DDomain) =
        let left_extension = if (i=0 && left_connection=Some TopLink) then extension_length else 0.0
        let left_extension = left_extension + if (i=0&&left_strut) then strut_len else 0.0
        let right_extension = if (i=strand.domains.Length-1 && right_connection= Some TopLink) then extension_length else 0.0
        let right_extension = right_extension + if (i=strand.domains.Length-1&&right_strut) then strut_len else 0.0
        let domain_length d = (make_label_text d |> String.length |> float) * letter_width + domain_pad , left_extension , right_extension
        let left_arrow = false
        let right_arrow = (i=strand.domains.Length-1 && not no_right_arrow)
        ddomain_to_path domain_length 0.0 o true left_arrow right_arrow d
      let (paths,length) = List.mapi (fun i d -> i,d) strand.domains |> List.mapFold top_domain 0.0
      let paths = List.concat paths
      { name = "TopStrand "+strand.ToString()
        content = Paths paths
        // This group only has top anchors.
        anchors = [{anchor_id="tl";anchor_point={x=0.0;y=0.0}}
                   {anchor_id="tr";anchor_point={x=length;y=0.0}}]
        debug_anchors = false
        sub_groups = []
        offset = None }
  | BottomStrand ->
      // Bottom strand: render as a straight sequence. The first domain has an arrow.
      let bottom_domain (o:float) (i:int,d:DDomain) =
        let left_extension = if (i=0 && left_connection=Some BottomLink) then extension_length else 0.0
        let left_extension = left_extension + if (i=0&&left_strut) then strut_len else 0.0
        let right_extension = if (i=strand.domains.Length-1 && right_connection=Some BottomLink) then extension_length else 0.0
        let right_extension = right_extension + if (i=strand.domains.Length-1&&right_strut) then strut_len else 0.0
        let domain_length d = (make_label_text d |> String.length |> float) * letter_width + domain_pad , left_extension , right_extension
        let left_arrow = (i=0 && not no_left_arrow)
        let right_arrow = false
        ddomain_to_path domain_length 0.0 o false left_arrow right_arrow d
      let (paths,length) = List.mapi (fun i d -> i,d) strand.domains |> List.mapFold bottom_domain 0.0
      let paths = List.concat paths
      { name = "BottomStrand "+strand.ToString()
        content = Paths paths
        // This group only has bottom anchors.
        anchors = [{anchor_id="bl";anchor_point={x=0.0;y=0.0}}
                   {anchor_id="br";anchor_point={x=length;y=0.0}}]
        debug_anchors = false
        sub_groups = []
        offset = None }
  | DoubleStrand ->
      // Double strand: render as two straight sequences. The top sequence is rendered as a top single strand and the bottom sequence is rendered as a bottom single strand; the only exception is that a vertical offset is added to implement strand separation.
      let thoffset = if left_connection=Some BottomLink then extension_length else 0.0
      let bhoffset = if left_connection=Some TopLink then extension_length else 0.0
      let top_domain (hoffset:float) (i:int,d:DDomain) =
        let left_extension = if (i=0 && left_connection=Some TopLink) then extension_length else 0.0
        let right_extension = if (i=strand.domains.Length-1 && right_connection= Some TopLink) then extension_length else 0.0
        let domain_length d = (make_label_text {d with complemented=true} |> String.length |> float) * letter_width + domain_pad , left_extension , right_extension
        let left_arrow = false
        let right_arrow = (i=strand.domains.Length-1 && not no_right_arrow)
        ddomain_to_path domain_length -half_separation hoffset true left_arrow right_arrow d
      let bottom_domain (hoffset:float) (i:int,d:DDomain) =
        let left_extension = if (i=0 && left_connection=Some BottomLink) then extension_length else 0.0
        let right_extension = if (i=strand.domains.Length-1 && right_connection=Some BottomLink) then extension_length else 0.0
        let domain_length d = (make_label_text {d with complemented=true} |> String.length |> float) * letter_width + domain_pad , left_extension , right_extension
        let left_arrow = (i=0 && not no_left_arrow)
        let right_arrow = false
        ddomain_to_path domain_length half_separation hoffset false left_arrow right_arrow (d.GetComplemented())
      let (tpaths,tlength) = List.mapi (fun i d -> i,d) strand.domains |> List.mapFold top_domain thoffset
      let (bpaths,blength) = List.mapi (fun i d -> i,d) strand.domains |> List.mapFold bottom_domain bhoffset
      let paths = List.concat (tpaths@bpaths)
      // I now have two path lists, one for top and one for bottom; I will concatenate them within the resulting group.
      { name = "DoubleStrand "+strand.ToString()
        content = Paths paths
        // This group has top and bottom anchors.
        anchors = [{anchor_id="tl";anchor_point={x=thoffset;y= -half_separation}}
                   {anchor_id="tr";anchor_point={x=tlength;y= -half_separation}}
                   {anchor_id="bl";anchor_point={x=bhoffset;y=half_separation}}
                   {anchor_id="br";anchor_point={x=blength;y=half_separation}}]
        debug_anchors = false
        sub_groups = []
        offset = None }
  | LeftHairpinStrand ->
      match strand.domains with
      | [] -> 
        // Degenerate hairpin: render as an arc with the same diameter as the strand separation.
        let arc = { rx = half_separation
                    ry = half_separation
                    large_flag = false
                    sweep_flag = false
                    angle = 0.0 }
        let start_point = {x=0.0;y=half_separation}
        let mid_point = {x= -half_separation;y=0.0}
        let end_point = {x=0.0;y= -half_separation}
        let path1 = { path_class = Some base_class
                      commands = [MoveTo start_point;ArcTo(mid_point,arc)]
                      path_label = None }
        let path2 = { path_class = Some base_class
                      commands = [MoveTo mid_point;ArcTo(end_point,arc)]
                      path_label = None }
        { name = "LeftHairpin "+strand.ToString()
          content = Paths [path1;path2]
          anchors = [{anchor_id="tr";anchor_point=end_point}
                     {anchor_id="br";anchor_point=start_point}]
          debug_anchors = false
          sub_groups = []
          offset = None }
      | domains ->
        // Left hairpin: render as a sequence of arcs. A hairpin never has arrows or extensions. The arc will be centered on the origin, and   sweep clockwise.
        let arc_length =
            if nucleotides then
              List.fold (fun i d -> i + (float d.nucleotides.Length) * letter_width * nucleotide_hairpin_factor + arc_domain_pad) arc_domain_pad domains
            else
              float (max 2 domains.Length) * letter_width * 3.0
        let radius = arc_length / Math.PI / 2.0
        // Calculate the start and end of the arc, both as coordinates and as angles. Remember that in the context of trigonometric functions   the Y-axis points upwards, while in the context of SVG rendering, the Y axis points downwards.
        // I'm tweaking the position based on the line width, in order to display a better connection. Note that I'll have to undo this when generating the anchor points.
        let sep = half_separation-line_width/2.0
        let start_angle = -Math.Asin(-sep/radius)
        let end_angle = 2.0*Math.PI - Math.Asin(sep/radius)
        let start_point = {x=radius*Math.Cos(start_angle);y= -sep}
        let end_point = {x=radius*Math.Cos(end_angle);y= sep}
        let paths =
          if nucleotides then
              let available_angle = end_angle-start_angle
              let available_length = available_angle * radius
              let arc_domain (start_point:Svg.point) (d:DDomain) : Svg.path list*Svg.point =
                let domain_angle = ((float d.nucleotides.Length) * letter_width * nucleotide_hairpin_factor + arc_domain_pad) * available_length / arc_length / radius
                ddomain_to_arc radius start_point domain_angle d false
              List.mapFold arc_domain start_point domains |> fst |> List.concat
          else
            let domain_angle = (end_angle-start_angle) / float domains.Length;
            let arc_domain (start_point:Svg.point) (d:DDomain) : Svg.path list*Svg.point = ddomain_to_arc radius start_point domain_angle d false
            List.mapFold arc_domain start_point domains |> fst |> List.concat
        { name = "LeftHairpin "+strand.ToString()
          content = Paths paths
          // This group has top and bottom anchors, to the right only.
          anchors = [{anchor_id="tr";anchor_point={end_point with y= -half_separation}}
                     {anchor_id="br";anchor_point={start_point with y=half_separation}}]
          debug_anchors = false
          sub_groups = []
          offset = None }
  | RightHairpinStrand ->
      match strand.domains with
      | [] -> 
        // Degenerate hairpin: render as an arc with the same diameter as the strand separation.
        let arc = { rx = half_separation
                    ry = half_separation
                    large_flag = false
                    sweep_flag = false
                    angle = 0.0 }
        let start_point = {x=0.0;y= -half_separation}
        let mid_point = {x= half_separation;y=0.0}
        let end_point = {x=0.0;y=half_separation}
        let path1 = { path_class = Some base_class
                      commands = [MoveTo start_point;ArcTo(mid_point,arc)]
                      path_label = None }
        let path2 = { path_class = Some base_class
                      commands = [MoveTo mid_point;ArcTo(end_point,arc)]
                      path_label = None }
        { name = "RightHairpin "+strand.ToString()
          content = Paths [path1;path2]
          anchors = [{anchor_id="tl";anchor_point=start_point}
                     {anchor_id="bl";anchor_point=end_point}]
          debug_anchors = false
          sub_groups = []
          offset = None }
      | domains ->
        // Right hairpin: render as a sequence of arcs. A hairpin never has arrows or extensions. The arc will be centered on the origin, and sweep clockwise.
        let arc_length =
          if nucleotides then
            List.fold (fun i d -> i + (float d.nucleotides.Length) * letter_width * nucleotide_hairpin_factor + arc_domain_pad) 0.0 domains
          else
            float (max 2 domains.Length) * letter_width * 3.0
        let radius = arc_length / Math.PI / 2.0
        // Calculate the start and end of the arc, both as coordinates and as angles. Remember that in the context of trigonometric functions the Y-axis points upwards, while in the context of SVG rendering, the Y axis points downwards.
        // I'm tweaking the position based on the line width, in order to display a better connection. Note that I'll have to undo this when generating the anchor points.
        let sep = half_separation-line_width/2.0
        let start_angle = -Math.PI + Math.Asin(sep/radius)
        let end_angle = Math.PI + Math.Asin(-sep/radius)
        let start_point = {x=radius*Math.Cos(start_angle);y= sep}
        let end_point = {x=radius*Math.Cos(end_angle);y= -sep}
        let paths =
          if nucleotides then
            let available_angle = end_angle-start_angle
            let available_length = available_angle * radius
            let arc_domain (start_point:Svg.point) (d:DDomain) : Svg.path list*Svg.point =
              let domain_angle = ((float d.nucleotides.Length) * letter_width * nucleotide_hairpin_factor + arc_domain_pad) * available_length / arc_length / radius
              ddomain_to_arc radius start_point domain_angle d false
            List.mapFold arc_domain start_point domains |> fst |> List.concat
          else
            let domain_angle = (end_angle-start_angle) / float domains.Length;
            let arc_domain (start_point:Svg.point) (d:DDomain) : Svg.path list*Svg.point = ddomain_to_arc radius start_point domain_angle d false
            List.mapFold arc_domain start_point domains |> fst |> List.concat
        { name = "RightHairpin "+strand.ToString()
          content = Paths paths
          // This group has top and bottom anchors, to the right only.
          anchors = [{anchor_id="tl";anchor_point={end_point with y= -half_separation}}
                     {anchor_id="bl";anchor_point={start_point with y=half_separation}}]
          debug_anchors = false
          sub_groups = []
          offset = None }
  
/// Renders a bulge to the intermediate Svg representation, as a double linear sequence of domains with a much wider separation than double strands. Bulges never have arrows. The *_extension parameters are the adjacent double-stranded domains, which will be used to draw an extended line in the case of bulges that only have one side.
let bulge_to_group_lines (mode:renderer_mode) (rotate_labels:bool) (top:DDomain list) (top_extension:DDomain) (bottom:DDomain list) (bottom_extension:DDomain) : Svg.group =
  let ddomain_to_path = ddomain_to_path mode rotate_labels
  // The total length of the top and bottom strands needs to be equal. I will calculate the length of the longest strand, and scale the length of the domains in the shorter strand accordingly.
  let domain_length d = (float (d.name.Length + if d.complemented then 1 else 0)) * letter_width + domain_pad
  let min_top_length = List.sumBy domain_length top
  let min_bottom_length = List.sumBy domain_length bottom
  let length = max min_top_length min_bottom_length
  let top_domain_length d = (domain_length d) * length / min_top_length, 0.0, 0.0
  let bottom_domain_length d = (domain_length d) * length / min_bottom_length, 0.0, 0.0
  let top_junctions =
    match top with
    | [] ->
      let top_Path = { path_class = Some (base_class+" "+dom_id top_extension)
                       commands = [MoveTo{x=0.0;y= -half_separation};LineTo{x=bulge_sep;y= -half_separation-bulge_sep};LineTo{x=bulge_sep+length;y= -half_separation-bulge_sep};LineTo{x=bulge_sep+bulge_sep+length;y= -half_separation}]
                       path_label = None }
      [top_Path]
    | _ ->
      // Each side of the bulge starts with an angled connector, then proceeds with the strand, then terminates with another angled connector.
      let top_left = { path_class = Some (base_class+" "+dom_id top.Head)
                       commands = [MoveTo{x=0.0;y= -half_separation};LineTo{x=bulge_sep;y= -half_separation-bulge_sep}]
                       path_label = None }
      let top_right = { path_class = Some (base_class+" "+dom_id (List.rev top).Head)
                        commands = [MoveTo{x=bulge_sep+length;y= -half_separation-bulge_sep};LineTo{x=bulge_sep+bulge_sep+length;y= -half_separation}]
                        path_label = None }
      [top_left;top_right]
  let bottom_junctions =
    match bottom with
    | [] ->
      let bottom_path = { path_class = Some (base_class+" "+dom_id bottom_extension)
                          commands = [MoveTo{x=0.0;y=half_separation};LineTo{x=bulge_sep;y=half_separation+bulge_sep};LineTo{x=bulge_sep+length;y=half_separation+bulge_sep};LineTo{x=bulge_sep+bulge_sep+length;y=half_separation}]
                          path_label = None }
      [bottom_path]
    | _ ->
      let bottom_left = { path_class = Some (base_class+" "+dom_id bottom.Head)
                          commands = [MoveTo{x=0.0;y=half_separation};LineTo{x=bulge_sep;y=half_separation+bulge_sep}]
                          path_label = None }
      let bottom_right = { path_class = Some (base_class+" "+dom_id (List.rev bottom).Head)
                           commands = [MoveTo{x=bulge_sep+length;y=half_separation+bulge_sep};LineTo{x=bulge_sep+bulge_sep+length;y=half_separation}]
                           path_label = None }
      [bottom_left;bottom_right]
  // Top strand: render as a straight sequence.
  let top_domain (o:float) (d:DDomain) = ddomain_to_path top_domain_length (-half_separation-bulge_sep) o true false false d
  let top_domain_paths = List.mapFold top_domain bulge_sep top |> fst |> List.concat
  // Bottom strand: render as a straight sequence.
  let bottom_domain (o:float) (d:DDomain) = ddomain_to_path bottom_domain_length (half_separation+bulge_sep) o false false false d
  let bottom_domain_paths = List.mapFold bottom_domain bulge_sep bottom |> fst |> List.concat
  { name = "Bulge"
    content = Paths (top_junctions@bottom_junctions@top_domain_paths@bottom_domain_paths)
    anchors = [{anchor_id="bl";anchor_point={x=0.0;y=half_separation}}
               {anchor_id="tl";anchor_point={x=0.0;y= -half_separation}}
               {anchor_id="tr";anchor_point={x=length+bulge_sep+bulge_sep;y= -half_separation}}
               {anchor_id="br";anchor_point={x=length+bulge_sep+bulge_sep;y=half_separation}}]
    debug_anchors = false
    sub_groups = []
    offset = None }

/// Renders a bulge to the intermediate Svg representation, as two semicircles. Bulges never have arrows. The *_extension parameters are the adjacent double-stranded domains, which will be used to draw an extended line in the case of bulges that only have one side.
let bulge_to_group_arcs (mode:renderer_mode) (rotate_labels:bool) (top:DDomain list) (top_extension:DDomain) (bottom:DDomain list) (bottom_extension:DDomain) : Svg.group =
  let ddomain_to_arc = ddomain_to_arc mode rotate_labels
  // The total length of the top and bottom strands needs to be equal. I will calculate the length of the longest strand, and scale the length of the domains in the shorter strand accordingly.
  let radius = float (max 2 (max top.Length bottom.Length)) * letter_width * 3.0 / Math.PI
  let start_angle = Math.PI - Math.Asin(half_separation/radius);
  let end_angle = Math.Asin(half_separation/radius);
  let top_angle = (end_angle-start_angle) / float top.Length
  let bottom_angle = (end_angle-start_angle) / float bottom.Length
  let top_domain_paths =
    match top with
    | [] ->
         [{ path_class = Some (base_class+" "+dom_id top_extension)
            commands = [MoveTo {x= -radius;y= -half_separation};LineTo{x= radius;y= -half_separation}]
            path_label = None }]
    | _ -> let top_domain (start_point:Svg.point) (d:DDomain) : Svg.path list*Svg.point = ddomain_to_arc radius start_point top_angle d true
           List.mapFold top_domain {x= -radius;y= -half_separation} top |> fst |> List.concat
  let bottom_domain_paths =
    match bottom with
    | [] ->
         [{ path_class = Some (base_class+" "+dom_id bottom_extension)
            commands = [MoveTo {x= -radius;y= half_separation};LineTo{x= radius;y= half_separation}]
            path_label = None }]
    | _ -> let bottom_domain (start_point:Svg.point) (d:DDomain) : Svg.path list*Svg.point = ddomain_to_arc radius start_point bottom_angle d true
           List.mapFold bottom_domain {x= radius;y=half_separation} (List.rev bottom) |> fst |> List.concat
  { name = "Bulge"
    content = Paths (top_domain_paths@bottom_domain_paths)
    anchors = [{anchor_id="bl";anchor_point={x= -radius;y=half_separation}}
               {anchor_id="tl";anchor_point={x= -radius;y= -half_separation}}
               {anchor_id="tr";anchor_point={x=radius;y= -half_separation}}
               {anchor_id="br";anchor_point={x=radius;y=half_separation}}]
    debug_anchors = false
    sub_groups = []
    offset = None }

/// Renders a DSegment to the intermediate SVG representation. This function requires additional context, in the form of the previous and next segments (which may be empty), in order to correctly draw connections.
let dsegment_to_group (mode:renderer_mode) (rotate_labels:bool) (prev:DSegment) (segment:DSegment) (next:DSegment) : Svg.group =
  let dstrand_to_group = dstrand_to_group mode rotate_labels
  let prev_empty = prev.double_strand.domains=[]
  let next_empty = next.double_strand.domains=[]
  let left_connection = prev.connection
  let right_connection = segment.connection
  // Render the double strand (always present). Note that I need to determine whether an extension is needed to connect to adjacent segments.
  let ds = dstrand_to_group 
             (if segment.top_left.IsSome && left_connection = Some TopLink then None else if segment.bottom_left.IsSome && left_connection = Some BottomLink then None else left_connection) false
             (if segment.top_right.IsSome && right_connection = Some TopLink then None else if segment.bottom_right.IsSome && right_connection = Some BottomLink then None else right_connection) false
             (segment.bottom_left.IsSome || segment.hairpin_left.IsSome || left_connection = Some BottomLink || left_connection = Some DoubleLink)
             (segment.top_right.IsSome || segment.hairpin_right.IsSome || right_connection = Some TopLink || right_connection = Some DoubleLink)
             segment.double_strand
  /// This function is a shortcut for Svg.attach_group_to; it's used to position a rendered overhang correctly with respect to the double strand. Note that, for the purpose of attaching hairpins, either top or bottom anchors will do.
  let attach (anchor_on_double:string) (anchor_on_group:string) (seg_group:Svg.group) : Svg.group =
    Svg.attach_group_to (ds,anchor_on_double) (seg_group,anchor_on_group)
  // For each possible element, if it is present, then render it, rotate it, and attach it to the double strand.
  // Bulges happen when two segments are connected by a double link with overhangs.
  let has_bulge_left = left_connection = Some DoubleLink && (segment.top_left.IsSome || segment.bottom_left.IsSome) && not prev_empty
  let has_bulge_right = right_connection = Some DoubleLink && (segment.top_right.IsSome || segment.bottom_right.IsSome) && not next_empty
  let bulge_to_group = if bulge_lines then bulge_to_group_lines else bulge_to_group_arcs
  let bulge_to_group = bulge_to_group mode rotate_labels
  // Render any bulges. Top and bottom strands are rendered as a unit.
  let bulgel = if not has_bulge_left then None else
               bulge_to_group (match segment.top_left with None -> [] | Some strand -> strand.domains) (segment.double_strand.domains.Head) (match segment.bottom_left with None -> [] | Some strand -> strand.domains) (segment.double_strand.domains.Head)
               |> (attach "tl" "tr") |> Some
  let bulger = if not has_bulge_right then None else
               bulge_to_group (match segment.top_right with None -> [] | Some strand -> strand.domains) (segment.double_strand.domains |> List.last) (match segment.bottom_right with None -> [] | Some strand -> strand.domains) (segment.double_strand.domains |> List.last)
               |> (attach "tr" "tl") |> Some
  // Render each possible overhang. Note that the presence of a bulge overrides overhangs. Note that an overhang should generally be rotated, unless it is at the end of a segment that doesn't connect to anything, or it is itself a connector to another segment of the same gate. If both top and bottom overhangs are present, they both must be rotated regardless.
  let tl = if has_bulge_left then None else
           match segment.top_left with
           | None -> None
           | Some strand -> let straight = segment.bottom_left.IsNone && (left_connection=None || (left_connection=Some TopLink && not prev_empty))
                            dstrand_to_group left_connection false None (prev.top_right.IsSome) false true strand
                            |> (fun g -> if straight then g else Svg.rotate_group_around_anchor "tr" overhang_angle g)
                            |> (attach "tl" "tr") |> Some
  let bl = if has_bulge_left then None else
           match segment.bottom_left with
           | None -> None
           | Some strand -> let straight = segment.top_left.IsNone && (left_connection=None || (left_connection=Some BottomLink && not prev_empty))
                            dstrand_to_group left_connection false None (prev.bottom_right.IsSome) (left_connection=Some BottomLink || left_connection=Some DoubleLink) false strand
                            |> (fun g -> if straight then g else Svg.rotate_group_around_anchor "br" -overhang_angle g)
                            |> (attach "bl" "br") |> Some
  let tr = if has_bulge_right then None else
           match segment.top_right with
           | None -> None
           | Some strand -> let straight = segment.bottom_right.IsNone && (right_connection=None || (right_connection=Some TopLink && not next_empty))
                            dstrand_to_group None (next.top_left.IsSome) right_connection false false (right_connection=Some TopLink || right_connection=Some DoubleLink) strand
                            |> (fun g -> if straight then g else Svg.rotate_group_around_anchor "tl" -overhang_angle g)
                            |> (attach "tr" "tl") |> Some
  let br = if has_bulge_right then None else
           match segment.bottom_right with
           | None -> None
           | Some strand -> let straight = segment.top_right.IsNone && (right_connection=None || (right_connection=Some BottomLink && not next_empty))
                            dstrand_to_group None (next.bottom_left.IsSome) right_connection false true false strand
                            |> (fun g -> if straight then g else Svg.rotate_group_around_anchor "bl" overhang_angle g)
                            |> (attach "br" "bl") |> Some
  // Render hairpins. Note that here I'm trusting that the segment is well-formed, i.e. does not have a hairpin and overhangs on the same side, and if it has a hairpin then it's disconnected on that side.
  let hl = match segment.hairpin_left with
           | None -> None
           | Some strand -> dstrand_to_group None false None false true true strand
                            |> (attach "tl" "tr") |> Some
  let hr = match segment.hairpin_right with
           | None -> None
           | Some strand -> dstrand_to_group None false None false true true strand
                            |> (attach "tr" "tl") |> Some
  // Determine the combined anchor points. The group for a DSegment always has top and bottom anchor points. It may not have left or right anchor points, if there is a hairpin on that side. For each overhang position, if there is an overhang, then the segment anchor point is the anchor point for the overhang. If there is no overhang, then the segment anchor point is the anchor point for the double strand.
  let a_tl = if segment.hairpin_left.IsSome then None else
             let a = match bulgel with Some g -> g | None -> match tl with Some g -> g | None -> ds
             Some {anchor_id="tl";anchor_point=Svg.get_anchor_point "tl" a}
  let a_tr = if segment.hairpin_right.IsSome then None else
             let a = match bulger with Some g -> g | None -> match tr with Some g -> g | None -> ds
             Some {anchor_id="tr";anchor_point=Svg.get_anchor_point "tr" a}
  let a_bl = if segment.hairpin_left.IsSome then None else
             let a = match bulgel with Some g -> g | None -> match bl with Some g -> g | None -> ds
             Some {anchor_id="bl";anchor_point=Svg.get_anchor_point "bl" a}
  let a_br = if segment.hairpin_right.IsSome then None else
             let a = match bulger with Some g -> g | None -> match br with Some g -> g | None -> ds
             Some {anchor_id="br";anchor_point=Svg.get_anchor_point "br" a}
  // Create the group object. Note that this group does not actually have content of its own; it only has sub-groups.
  { name = "Segment "+segment.ToString()
    content = Paths []
    // Gather the anchors that actually exist.
    anchors = List.choose (fun g->g) [a_tl;a_tr;a_bl;a_br]
    debug_anchors = false
    // Gather the double strand group and the overhang groups that actually exist.
    sub_groups = List.choose (fun x->x) [Some ds;tl;tr;bl;br;hl;hr;bulgel;bulger]
    offset = None }

/// Renders a DGate to the intermediate SVG representation.
let dgate_to_group (mode:renderer_mode) (rotate_labels:bool) (gate:DGate) : Svg.group =
  let dsegment_to_group = dsegment_to_group mode rotate_labels
  /// This function renders a DSegment within a gate, and attaches it to the group that was rendered from a previous DSegment. The signature is designed for use with List.mapFold. It returns the rendered group, and a tuple of the segment plus the rendered group (which will become the parameter of the next iteration).
  let attach (prev:DSegment,prevg:Svg.group,curr:DSegment) (next:DSegment) : Svg.group*(DSegment*Svg.group*DSegment) =
    // Render the new segment and align it with the previous segment, according to the connection type.
    let gseg = dsegment_to_group prev curr next
    let currg = match prev.connection with
                | None -> failwith "unconnected segment"
                | Some TopLink -> Svg.attach_group_to (prevg,"tr") (gseg,"tl")
                | Some BottomLink -> Svg.attach_group_to (prevg,"br") (gseg,"bl")
                | Some DoubleLink -> Svg.attach_group_to (prevg,"tr") (gseg,"tl")
    currg,(curr,currg,next)
  // Get the first group.
  let firstseg = gate.segments.Head
  let nextseg = match gate.segments.Tail with [] -> empty_dsegment | seg::_ -> seg
  let firstg = dsegment_to_group ({empty_dsegment with connection=gate.left_connection}) firstseg nextseg
  // Render the segment sequence.
  let (groups,(lastseg,lastg)) =
    match gate.segments.Tail with
    | [] -> ([firstg],(firstseg,firstg))
    | next::rest ->
      let (groups,(curr,_,last)) = List.mapFold attach (firstseg,firstg,next) rest
      let groups = firstg::groups
      // Render the last segment.
      let (last_group,(lastseg,lastg,_)) = attach(curr,(List.last groups),last) empty_dsegment
      (groups@[last_group],(lastseg,lastg))
  let left_connection = gate.left_connection
  let right_connection = lastseg.connection
  // Determine the gate anchors. Each anchor may need to be extended a bit, if the adjacent gate is not actually connected there.
  let a_tl =
    if firstseg.hairpin_left.IsSome then None else
    let p = Svg.get_anchor_point "tl" firstg
    let p = if left_connection=Some DoubleLink||left_connection=Some TopLink then p else
            if p.y = -half_separation then {p with x=p.x-extension_length} else
            Svg.rotate_point_around p -overhang_angle {p with x=p.x-extension_length}
    Some {anchor_id="tl";anchor_point=p}
  let a_tr =
    if lastseg.hairpin_right.IsSome then None else
    let p = Svg.get_anchor_point "tr" lastg
    let p = if right_connection=Some DoubleLink||right_connection=Some TopLink then p else
            if p.y = -half_separation then {p with x=p.x+extension_length} else
            Svg.rotate_point_around p -overhang_angle {p with x=p.x+extension_length}
    Some {anchor_id="tr";anchor_point=p}
  let a_bl =
    if firstseg.hairpin_left.IsSome then None else
    let p = Svg.get_anchor_point "bl" firstg
    let p = if left_connection=Some DoubleLink||left_connection=Some BottomLink then p else
            if p.y = half_separation then {p with x=p.x-extension_length} else
            Svg.rotate_point_around p overhang_angle {p with x=p.x-extension_length}
    Some {anchor_id="bl";anchor_point=p}
  let a_br =
    if lastseg.hairpin_right.IsSome then None else
    let p = Svg.get_anchor_point "br" lastg
    let p = if right_connection=Some DoubleLink||right_connection=Some BottomLink then p else
            if p.y = half_separation then {p with x=p.x+extension_length} else
            Svg.rotate_point_around p overhang_angle {p with x=p.x+extension_length}
    Some {anchor_id="br";anchor_point=p}
  // Return the containing group. This group has no content of its own, just subgroups.
  { name = "Gate "+gate.ToString()
    content = Paths [] 
    // A DGate always has top and bottom anchors, which correspond to the anchors for the leftmost and rightmost segments. It may not have left or right anchors, if the first segment has a left hairpin or the last segment has a right hairpin, respectively.
    anchors = List.choose (fun g->g) [a_tl;a_tr;a_bl;a_br]
    debug_anchors = debug_anchors
    // Return all the rendered subgroups.
    sub_groups = groups
    offset = None }

/// This function draws connecting lines between branches as appropriate. Parameters: the root of the node to which the gates belong, and tuples of already-rendered group and gates for the two gates. Precondition: the groups must be in the same frame of reference. Returns the updated g1.
let connect_branches (mode:renderer_mode) (root:DGate) (g1:group,gate1:DGate) (g2:group,gate2:DGate) : group =
  let nucleotides = mode = renderer_mode.Nucleotides
  let get_domain_class = get_domain_class nucleotides
  let connector_anchor = "cc"
  /// Returns the top right domain for the given gate, along with a flag that tells whether it is an overhang.
  let get_top_right_domain (gate:DGate) =
    let last_segment = List.last gate.segments
    match last_segment.top_right with Some strand -> List.last strand.domains,true | None -> List.last last_segment.double_strand.domains,false
  /// Returns the top left domain for the given gate, along with a flag that tells whether it is an overhang.
  let get_top_left_domain (gate:DGate) =
    let first_segment = List.head gate.segments
    match first_segment.top_left with Some strand -> List.head strand.domains,true | None -> List.head first_segment.double_strand.domains,false
  /// Returns the bottom right domain for the given gate, along with a flag that tells whether it is an overhang.
  let get_bottom_right_domain (gate:DGate) =
    let last_segment = List.last gate.segments
    match last_segment.bottom_right with Some strand -> List.last strand.domains,true | None -> List.last last_segment.double_strand.domains,false
  /// Returns the bottom left domain for the given gate, along with a flag that tells whether it is an overhang.
  let get_bottom_left_domain (gate:DGate) =
    let first_segment = List.head gate.segments
    match first_segment.bottom_left with Some strand -> List.head strand.domains,true | None -> List.head first_segment.double_strand.domains,false
  /// Chooses which pair of classes to use, based on which ones are supposed to avoid fraying.
  let choose_classes (class1,avoid_fraying_1) (class2,avoid_fraying_2) : string*string =
    if avoid_fraying_1 && not avoid_fraying_2 then (class2,class2) else
    if avoid_fraying_2 && not avoid_fraying_1 then (class1,class1) else
    (class1,class2)
  /// Creates a set of paths that connects the given anchors of the given groups, with the given CSS classes.
  let connect_anchors (a1,g1,c1) (a2,g2,c2) =
    let p1 = Svg.get_anchor_point a1 g1
    let p2 = Svg.get_anchor_point a2 g2
    let short = sqrt ((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y)) < short_branch_join
    let style = if short then " shortbranchjoin" else " branchjoin"
    if c1 = c2 then
      [{ path_class = Some (c1+style)
       ; commands = [MoveTo p1;LineTo p2]
       ; path_label = None }]
    else
      let mid = {x=(p1.x+p2.x)/2.0;y=(p1.y+p2.y)/2.0}
      let a = { path_class = Some (c1+style)
              ; commands = [MoveTo p1;LineTo mid]
              ; path_label = None }
      let b = { path_class = Some (c2+style)
              ; commands = [MoveTo mid;LineTo p2]
              ; path_label = None }
      [a;b]
  if gate1 = root then
    // First link. Connect gate1's top right to gate2's top left, iff gate2 has a top or double link at the beginning.
    match gate2.left_connection with
    | Some BottomLink
    | None -> {g1 with sub_groups=g2::g1.sub_groups}
    | Some TopLink
    | Some DoubleLink ->
      let ct1 = let (d,overhang) = gate1|>get_top_right_domain in d|>get_domain_class,d.toehold&&not overhang
      let ct2 = let (d,overhang) = gate2|>get_top_left_domain in d|>get_domain_class,d.toehold&&not overhang
      //let (class1,class2) = choose_classes ct1 ct2
      let (class1,class2) = base_class,base_class
      let paths = connect_anchors ("tr",g1,class1) ("tl",g2,class2)
      let pg = { Svg.content_group (Paths paths) with name = "Connector" }
      {g1 with sub_groups=g2::pg::g1.sub_groups}
  else if gate2 = root then
    // Last link. Connect gate1's bottom left to gate2's bottom right, iff gate2 has a top or bottom link at the beginning. Note that, in this case, g2 is already present within g1, therefore thereturned group does not need to include it in the sub_groups.
    match (List.last gate2.segments).connection with
    | Some TopLink
    | None -> {g1 with sub_groups=g1.sub_groups}
    | Some BottomLink
    | Some DoubleLink ->
      let ct1 = let (d,overhang) = gate1|>get_bottom_left_domain in d|>get_domain_class,d.toehold&&not overhang
      let ct2 = let (d,overhang) = gate2|>get_bottom_right_domain in d|>get_domain_class,d.toehold&&not overhang
      //let (class1,class2) = choose_classes ct1 ct2
      let (class1,class2) = base_class,base_class
      let paths = connect_anchors (connector_anchor,g1,class1) ("br",g2,class2)
      let pg = { Svg.content_group (Paths paths) with name = "Connector" }
      {g1 with sub_groups=pg::g1.sub_groups}
  else
    // Connect gate1's bottom left to gate2's top left, iff gate2 has a top or double link at the beginning.
    match gate2.left_connection with
    | Some BottomLink
    | None -> {g1 with sub_groups=g2::g1.sub_groups}
    | Some TopLink
    | Some DoubleLink ->
      let ct1 = let (d,overhang) = gate1|>get_bottom_left_domain in d|>get_domain_class,d.toehold&&not overhang
      let ct2 = let (d,overhang) = gate2|>get_top_left_domain in d|>get_domain_class,d.toehold&&not overhang
      //let (class1,class2) = choose_classes ct1 ct2
      let (class1,class2) = base_class,base_class
      let paths = connect_anchors (connector_anchor,g1,class1) ("tl",g2,class2)
      let pg = { Svg.content_group (Paths paths) with name = "Connector" }
      {g1 with sub_groups=g2::pg::g1.sub_groups}

/// Renders a DBranch to the intermediate SVG representation.
let rec dbranch_to_group_wide (mode:renderer_mode) (rotate_labels:bool) (branch:DBranch) : Svg.group =
  let dgate_to_group = dgate_to_group mode rotate_labels
  let dbranch_to_group_wide = dbranch_to_group_wide mode rotate_labels
  let connect_branches = connect_branches mode
  let rootgate = branch.gate
  let rendered_gate = dgate_to_group rootgate,rootgate
  let rendered_children = branch.children |> List.map (fun b -> dbranch_to_group_wide b,b.gate)
  let edge_count = float (rendered_children.Length+1)
  let min_anchor_x (g:group) = (g.anchors |> List.map (fun a -> a.anchor_point.x) |> List.min) 
  let max_anchor_x (g:group) = (g.anchors |> List.map (fun a -> a.anchor_point.x) |> List.max) 
  let mid_anchor_y_left (g:group) = ((Svg.get_anchor_point "bl" g).y + (Svg.get_anchor_point "tl" g).y) / 2.0
  let anchor_height_left (g:group) = abs ((Svg.get_anchor_point "bl" g).y - (Svg.get_anchor_point "tl" g).y)
  let mid_anchor_y_right (g:group) = ((Svg.get_anchor_point "br" g).y + (Svg.get_anchor_point "tr" g).y) / 2.0
  let anchor_height_right (g:group) = abs ((Svg.get_anchor_point "br" g).y - (Svg.get_anchor_point "tr" g).y)
  // Note that if there are no children, then this entire function won't actually do anything. Also, in that case there is the possibility that the dgate does not have right anchors, which would cause anchor_height_right to crash. So I'm better off not calculating the height at all.
  let max_height = if rendered_children=[] then 0.0 else max (rendered_gate|>fst|>anchor_height_right) ((rendered_children) |> List.map fst |> List.maxBy anchor_height_left |> anchor_height_left)
  let circumference = (max_height + 1.0) * edge_count
  let radius = circumference / Math.PI / 2.0
  let deg_diff = 360.0 / edge_count
  // I'm adding vertical center anchors to the groups, which correspond to the center of the circle.
  let rendered_gate =
    // For the same reason as above, if I have no children then there's no point in adding the right-central anchor, and it may fail if the dgate lacks right anchors. So I'm skipping it.
    if rendered_children=[] then rendered_gate else
    let (g,gate) = rendered_gate
    {g with anchors = {anchor_id="rc";anchor_point={x=max_anchor_x g + radius;y=mid_anchor_y_right g}}::g.anchors},gate
  let rendered_children = rendered_children |> List.map (fun (g,gate) -> {g with anchors = {anchor_id="lc";anchor_point={x=min_anchor_x g-radius;y=mid_anchor_y_left g}}::g.anchors},gate)
  /// Rotates, translates and connects the children of the given (group,gate), starting from the given angle. Returns the updated group and the last connected gate.
  let rec attach (group,gate) angle (children:(group*DGate)list) : group*DGate =
    match children with
    | [] ->
      let (group,gate) = if gate = rootgate then (group,gate) else connect_branches rootgate (group,gate) rendered_gate, snd rendered_gate
      (group,gate)
    | (child_group,child_gate)::rest ->
      let child_group = Svg.rotate_group_around_anchor "lc" angle child_group
      let child_group = Svg.attach_group_to (group,"rc") (child_group,"lc")
      let cgroup = connect_branches rootgate (group,gate) (child_group,child_gate)
      let cgroup = {cgroup with anchors={anchor_id="cc";anchor_point=Svg.get_anchor_point "bl" child_group}::cgroup.anchors}
      attach (cgroup,child_gate) (angle+deg_diff) rest
  let (group,_) = attach rendered_gate (deg_diff-180.0) rendered_children
  group

/// Renders a DBranch to the intermediate SVG representation.
let rec dbranch_to_group_babylon (mode:renderer_mode) (rotate_labels:bool) (branch:DBranch) : Svg.group =
  let connect_branches = connect_branches mode
  let dgate_to_group = dgate_to_group mode rotate_labels
  let dbranch_to_group_babylon = dbranch_to_group_babylon mode rotate_labels
  // Render the root.
  let rootgate = branch.gate
  let rootgroup = dgate_to_group rootgate
  // If this is just the root, everything else is useless. Return immediately.
  if branch.children = [] then rootgroup else
  // Render the children.
  let rendered_children = branch.children |> List.map (fun b -> dbranch_to_group_babylon b,b.gate)
  /// This function returns the length of the side of a group that's facing towards the node.
  let side_len (group,gate) =
    let vec = if gate = rootgate then Svg.anchor_to_anchor (group,"br") (group,"tr") else Svg.anchor_to_anchor (group,"bl") (group,"tl")
    let len = Svg.norm vec
    len
  // Calculate the array of side lengths.
  let side_lengths = List.map side_len ((rootgroup,rootgate)::rendered_children)
  /// Provides the angle covered by a side.
  let side_angle radius side_len = 2.0 * Math.Asin (side_len/radius/2.0)
  let total_angle radius =
    List.sumBy (side_angle radius) side_lengths
  /// Finds a crude numerical solution to the babylonian problem.
  let rec find_radius last_radius =
    let next_radius = last_radius - 1.0
    if next_radius <= 0.0 then next_radius else
    let next_total_angle = total_angle next_radius
    if Double.IsNaN next_total_angle || next_total_angle > 2.0*Math.PI then last_radius else
    find_radius next_radius
  // Get the radius. If the longest side is greater than all the rest put together, then the babylonian problem has no solution. I'll just use the max side as radius.
  let max_side = List.max side_lengths
  let radius = if max_side >= (List.sum side_lengths) / 2.0 then max_side/2.0 else find_radius max_side
  let radius = if radius <= 0.0 then max_side/2.0 else radius
  let radius = radius + float (rendered_children.Length+1) * 1.0
  // Get the total angle occupied by branch sides. This will typically be a bit less than 360°.
  let total_seg_angle = total_angle radius
  // Get the remaining angle. I'll allocate this as extra spacing between branches.
  let total_spare_angle = Math.PI * 2.0 - total_seg_angle
  /// Weight function for the allocation of extra spacing between branches. Takes two adjacent branches and returns a number that's proportional to how much extra angular spacing should be allocated between them.
  let spare_angle_weight (group1,gate1) (group2,gate2) =
    let trunk_dist_1 = abs (if gate1=rootgate then (Svg.get_anchor_point "tr" group1).y else (Svg.get_anchor_point "bl" group1).y)
    let trunk_dist_2 = abs (if gate2=rootgate then (Svg.get_anchor_point "br" group2).y else (Svg.get_anchor_point "tl" group2).y)
    1.0 / ((trunk_dist_1+trunk_dist_2)*(trunk_dist_1+trunk_dist_2))
  // Create a table associating couples of successive gates with the weight to be given to the allocation of the spare angle between them.
  let spare_angle_weights =
    let (couples,_) = List.mapFold (fun curr next -> (curr,next),next) (rootgroup,rootgate) (rendered_children@[(rootgroup,rootgate)])
    List.map (fun ((groupa,gatea),(groupb,gateb)) -> (gatea,gateb),spare_angle_weight (groupa,gatea) (groupb,gateb)) couples
  // Calculate the total weight for the allocation of spare angles.
  let spare_angle_total_weight = List.sumBy (fun (_,w) -> w) spare_angle_weights
  // Redefine the weight function to use the table created above. This ensures it produces the correct result after the groups get rototranslated.
  let spare_angle_weight gate1 gate2 =
    let (_,w) = List.find (fun ((g1,g2),_) -> g1=gate1&&g2=gate2) spare_angle_weights
    w
  /// Calculates the extra angular spacing to be inserted between two groups.
  let spare_angle_per_connection g1 g2 = total_spare_angle * (spare_angle_weight g1 g2) / spare_angle_total_weight
  /// Adds to the group an anchor located at the center of the circle.
  let add_center_anchor (g:group) : group =
    // I need to figure out an anchor located on the center of the circle. For the root, it's on the right; for the children, it's on the left.
    let root = (g = rootgroup)
    // Get the two anchors on the side facing the node.
    let (p1,p2) = if root then (Svg.get_anchor_point "tr" g), (Svg.get_anchor_point "br" g) else (Svg.get_anchor_point "tl" g), (Svg.get_anchor_point "bl" g)
    // Calculate the intersection points of two circles of equal radius, centered on the two anchors. Good explanation at https://math.stackexchange.com/questions/256100/how-can-i-find-the-points-at-which-two-circles-intersect.
    let d2 = (p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y)
    let k = sqrt(4.0*radius*radius/d2-1.0)/2.0
    let xa = (p1.x+p2.x)/2.0 + k * (p2.y-p1.y)
    let xb = (p1.x+p2.x)/2.0 - k * (p2.y-p1.y)
    let ya = (p1.y+p2.y)/2.0 + k * (p2.x-p1.x)
    let yb = (p1.y+p2.y)/2.0 - k * (p2.x-p1.x)
    let c1 = {x=xa;y=yb}
    let c2 = {x=xb;y=ya}
    // I have two intersections. Get the one on the correct side.
    let c = if root then
              if xa > xb then c1 else c2
            else
              if xa > xb then c2 else c1
    let anchor = {anchor_id="cen";anchor_point=c}
    {g with anchors=anchor::g.anchors}
  // I'm adding vertical center anchors to the groups, which correspond to the center of the circle. The root group doesn't get the anchor, because it doesn't get rotated.
  let rootgroup1 = rootgroup |> add_center_anchor
  let rendered_children1 = rendered_children |> List.map (fun (group,gate) -> (add_center_anchor group),gate)
  // Move all children so that their center anchors are superposed with the center anchor of the root.
  let rendered_children2 = rendered_children1 |> List.map (fun (group,gate) -> (Svg.attach_group_to (rootgroup1,"cen") (group,"cen")),gate )
  /// This function rotates a child so that its side is located clockwise of the previous child, plus some spare angle.
  let rotate_child (prev_group:group,prev_gate:DGate,prev_angle:float) (child_group:group,child_gate:DGate) : group*float =
    // This doesn't work because the child's center anchor does not necessarily lie on its axis, so the child may initially be at an angle different from 0.
    let child_angle =
      let child_side_1 = Svg.get_anchor_point "tl" child_group
      let child_side_2 = Svg.get_anchor_point "bl" child_group
      let child_center = Svg.get_anchor_point "cen" child_group
      let child_side_center = {x=(child_side_1.x+child_side_2.x)/2.0;y=(child_side_1.y+child_side_2.y)/2.0}
      let v = Svg.sub child_side_center child_center
      let angle = Svg.ang v
      angle
    let spare_angle = (spare_angle_per_connection prev_gate child_gate) |> Svg.rad_to_deg
    let prev_side = side_len (prev_group,prev_gate)
    let child_side = side_len (child_group,child_gate)
    let prev_side_angle = side_angle radius prev_side |> Svg.rad_to_deg
    let child_side_angle = side_angle radius child_side |> Svg.rad_to_deg
    let angle = prev_angle + prev_side_angle/2.0 + spare_angle + child_side_angle/2.0
    let group = Svg.rotate_group_around_anchor "cen" (angle - child_angle) child_group
    group,angle
  // Note that the starting angle is -180.0, because all children have the center to the left.
  let (rendered_children3,final_angle) = rendered_children2 |> List.mapFold (fun prev_group (group,gate) -> let (group,a) = rotate_child prev_group (group,gate) in (group,gate),(group,gate,a)) (rootgroup1,rootgate,-180.0)

  // Connect all children.
  let rec attach (group,gate) (children:(group*DGate)list) : group*DGate =
    match children with
    | [] ->
      let (group,gate) = if gate = rootgate then (group,gate) else connect_branches rootgate (group,gate) (rootgroup1,rootgate), rootgate
      (group,gate)
    | (child_group,child_gate)::rest ->
      let cgroup = connect_branches rootgate (group,gate) (child_group,child_gate)
      let cgroup = {cgroup with anchors={anchor_id="cc";anchor_point=Svg.get_anchor_point "bl" child_group}::cgroup.anchors}
      attach (cgroup,child_gate) rest
  let (group,_) = attach (rootgroup1,rootgate) rendered_children3
  group
  
let dbranch_to_group (arrange:arrange_mode) (mode:renderer_mode) (rotate_labels:bool) (branch:DBranch) : Svg.group =
  match arrange with
  | Babylon -> dbranch_to_group_babylon mode rotate_labels branch
  | Wide -> dbranch_to_group_wide mode rotate_labels branch

let dorigami_to_group (arrange:arrange_mode) (mode:renderer_mode) (rotate_labels:bool) (origami:DOrigami) : Svg.group =
  let dstrand_to_group = dstrand_to_group mode rotate_labels
  let dbranch_to_group = dbranch_to_group arrange mode rotate_labels
  let (dstrands,dbranches) = origami |> List.partition (fun (el:DOrigamiElement) -> match el with Strand _ -> true | _ -> false)
  let dstrands = dstrands |> List.map (fun el -> match el with Strand s -> s | _ -> failwith "")
  let dbranches = dbranches |> List.map (fun el -> match el with Branch b -> b | _ -> failwith "")
  let gstrands = dstrands |> List.map (dstrand_to_group None false None false false false)
  let gbranches = dbranches |> List.map dbranch_to_group
  let b = (gstrands @ gbranches) |> List.map (fun g -> Svg.emerge_groups [g]) |> Svg.stackv origami_sep species_skip in
  let border_label = "" in
  let border_inset = (String.length border_label |> float) * letter_width / 2.0 in
  let border_offset = { Svg.x = 2.0; Svg.y = 2.0 } in
  let border_dim = b.box_dim |> Svg.translate_point_by (border_offset |> Svg.scale_by -1.0) in
  let border = { Svg.path_class = base_class + " origami-border" |> Some
               ; Svg.commands = Svg.rounded_box border_offset border_dim origami_round
               ; Svg.path_label = { Svg.label_text = border_label
                                  ; Svg.label_anchor = Svg.sub b.box_dim origami_sep |> Svg.translate_point_by { x = -border_inset; y = 0.0 }
                                  ; Svg.label_class = Some base_class
                                  ; Svg.letter_width = Svg.default_letter_width
                                  ; Svg.label_dir = Svg.sub b.box_dim origami_sep |> Svg.translate_point_by { x = -border_inset; y = 0.0 } }
                                  |> Some }
  {b.box_group with content = Paths [border]}

let unknown_to_group (unknown:string) : Svg.group =
  { name = unknown
    content = Raw unknown
    anchors = []
    debug_anchors = false 
    sub_groups = [] 
    offset = None }


// Conversion from ClassicDSD species. This is fairly straightforward, as the underlying representations are similar.

let from_dsd_domain (mapping:Sequence.mapping) (dom:Domain.t) : DDomain =
  let dname = Domain.get_name dom
  let name = if dname.EndsWith("*") then dname.Substring(0,dname.Length-1) else dname
  { name = name
    nucleotides = match Sequence.get_domain mapping dom with None | Some None -> Domain.get_sequence dom |> Sequence.parse_sequence | Some (Some s) -> s
    toehold = Domain.is_toehold dom
    complemented = Domain.is_complemented dom
    tether = if Domain.is_tethered dom then Domain.display_picture dom true |> Some else None
    pseudoknot = None }

let from_dsd_strand (mapping:Sequence.mapping) (st:Strand.t) : DStrand =
  let from_dsd_domain = from_dsd_domain mapping
  match st with
  | Strand.Upper doms -> { kind = TopStrand; domains = List.map from_dsd_domain doms }
  | Strand.Lower doms -> { kind = BottomStrand; domains = List.map from_dsd_domain doms }

let from_dsd_segment (mapping:Sequence.mapping) (seg:Segment.t) : DSegment =
  let from_dsd_domain = from_dsd_domain mapping
  match seg with
  | Segment.Double (bl, tl, m, tr, br) ->
    { top_left = if tl=[] then None else { kind = TopStrand; domains = List.map from_dsd_domain tl } |> Some
      top_right = if tr=[] then None else { kind = TopStrand; domains = List.map from_dsd_domain tr } |> Some
      bottom_left = if bl=[] then None else { kind = BottomStrand; domains = List.map from_dsd_domain bl } |> Some
      bottom_right = if br=[] then None else { kind = BottomStrand; domains = List.map from_dsd_domain br } |> Some
      double_strand = { kind = DoubleStrand; domains = List.map from_dsd_domain m }
      hairpin_left = None
      hairpin_right = None
      connection = None }
  | Segment.Hairpin (b, t, m, h, side) ->
    match side with
    | Segment.Left -> 
      { top_left = None
        top_right = if t=[] then None else { kind = TopStrand; domains = List.map from_dsd_domain t } |> Some
        bottom_left = None
        bottom_right = if b=[] then None else { kind = BottomStrand; domains = List.map from_dsd_domain b } |> Some
        double_strand = { kind = DoubleStrand; domains = List.map from_dsd_domain m }
        hairpin_left = { kind = LeftHairpinStrand; domains = List.map from_dsd_domain h } |> Some
        hairpin_right = None
        connection = None }
    | Segment.Right -> 
      { top_left = if t=[] then None else { kind = TopStrand; domains = List.map from_dsd_domain t } |> Some
        top_right = None
        bottom_left = if b=[] then None else { kind = BottomStrand; domains = List.map from_dsd_domain b } |> Some
        bottom_right = None
        double_strand = { kind = DoubleStrand; domains = List.map from_dsd_domain m }
        hairpin_left = None
        hairpin_right = { kind = RightHairpinStrand; domains = List.map from_dsd_domain h } |> Some
        connection = None }

let from_dsd_gate (mapping:Sequence.mapping) (g:Gate.t) : DGate =
  let from_dsd_segment = from_dsd_segment mapping
  // A DSD gate is a double-list. Elements in the inner list are all bottom-connected, and are top-connected to other element sequences in the outer list.
  let to_bottom_sequence (final: bool) (segs:Segment.t list) =
    let len = segs.Length
    List.mapi (fun i seg -> { from_dsd_segment seg with connection = if i = len-1 then (if final then None else Some TopLink) else Some BottomLink}) segs
  let len = g.Length
  let segments = List.mapi (fun i segl -> to_bottom_sequence (i=len-1) segl) g |> List.concat
  { segments = segments; left_connection = None }

let from_dsd_origami (mapping:Sequence.mapping) (o:Origami.t) : DOrigami =
  let from_dsd_strand = from_dsd_strand mapping
  let from_dsd_gate = from_dsd_gate mapping
  let gate_to_simple_dbranch (g:Gate.t) = { gate = from_dsd_gate g; children = [] }
  let to_origami_element (c:Origami.content) =
    match c with
    | Origami.C_strand str -> from_dsd_strand str |> Strand
    | Origami.C_gate gate -> gate_to_simple_dbranch gate |> Branch
  List.map to_origami_element o


// Conversion from LogicDSD processes. This goes through a graph representation (where bind sites are edges and everything else is nodes).

/// Site type for the intermediate representation. This is basically the same as SiteC, except that it also allows for the notion of an explicit "eliminated pseudoknot".
type LSite = LBound   of DomainC * BondC
           | LUnbound of DomainC
           | LPseudoknotBound of DomainC * BondC
           with override this.ToString() = match this with
                                           | LPseudoknotBound (domain,bond) -> domain.ToString()+"/"+bond.ToString()
                                           | LBound (domain,bond) -> domain.ToString()+"."+bond.ToString()
                                           | LUnbound domain -> domain.ToString()
           end
/// Process type for the intermediate representation. It's a list of strands, which are a list of sites.
type LProcess = LSite list list
/// Type representing a site sequence; this is basically a ProcessC where gaps between StrandC instances are replaced with a None object.
type LSeq = LSite option list
/// Node type for the intermediate graph representation. A node is conceptually a circle made of interleaved edges and LSeq instances. The first LSeq represents the unprocessed sites that lie on the node between the edge from its parent node and the first child edge (leading sequence). The tuples in the list are made of an edge, and the unprocessed sites (trailing sequence) that lie on the node between the edge and the next edge (or, in the case of the last tuple in the list, between the edge and the edge coming from the parent).
type LNode = { leading: LSeq; edges: (LEdge * LSeq) list }
/// Edge type for the intermediate graph representation. An edge is a DGate, plus the LNode to which it connects. Edges are directed. They connect to their parent node on the left, and to child nodes on the right.
and LEdge = { gate: DGate; child: LNode }

/// This functions converts a DomainC to a DDomain.
let domainc_to_domain (domain:DomainC) (pseudoknot:int option) : DDomain =
 { name = domain.name
   nucleotides = []
   toehold = domain.isToehold
   complemented = domain.isComplementary
   tether = match domain.tag with 
            | NoTagC  -> None 
            | AnyTagC -> Some "_"
            | TagC s  -> Some s
   pseudoknot = pseudoknot }

/// This function converts a LSeq into a DStrand. It's used for cases where the entire graph is composed of a node with no edges. This means that it is a single strand. This is a special case, because it cannot be represented as a DBranch.
let from_single_node_lgraph (lseq:LSeq) : DStrand =
  let site_to_domain (s:LSite) : DDomain =
    let (domain,pseudoknot) = match s with LBound (dom,_) -> (dom,None) | LPseudoknotBound (dom,pk) -> (dom,Some pk) | LUnbound dom -> (dom,None)
    domainc_to_domain domain pseudoknot
  let domains = lseq |> List.choose (fun x->x) |> List.map site_to_domain
  // I expect the sequence to be a single strand, i.e. connected domains and a single "empty" site at the end. If there's more than oneempty site, then something is wrong. A process with more than one strand should always have at least one bond.
  if domains.Length <> lseq.Length-1 then failwith "Error: root node is a split leaf" else
  // By convention, I assume this strand to be top.
  { domains = domains; kind = DStrandKind.TopStrand }

/// This function "irons" the graph, that is, moves all domains that are inside an LNode into edges proper.
let iron_root_graph (root:LNode) : LNode =
  /// This function irons each edge's leading sequence into the top left of the edge. 
  let rec iron_top_left_all (leading:LSeq,edges:(LEdge*LSeq)list) =
      /// Moves all domains from the leading sequence to the adjacent edge, up to the end or the first gap. Returns the modified leading sequence and modified edge.
      let rec iron_top_left (leading:LSeq) (edge:LEdge) : (LSeq*LEdge) =
        /// Moves one domain from the leading sequence to the adjacent edge, if possible. Returns the modified leading sequence and modified edge. If there's no domain to move, returns None.
        let iron_top_left_single (leading:LSeq) (edge:LEdge) : (LSeq*LEdge) option =
          match List.rev leading with
          | [] -> None
          | None::revrest -> None
          | site::revrest ->
            // I've removed a non-gap site immediately preceding the edge. Convert the site to a domain.
            let domain = match site with
                         | Some (LUnbound dom) -> domainc_to_domain dom None
                         | Some (LPseudoknotBound (dom,pk)) -> domainc_to_domain dom (Some pk)
                         | _ -> failwith "this should never happen (bound domain in a node)"
            let rest = List.rev revrest
            // Attach it to the top left of the edge gate.
            let dgate:DGate =
              match edge.gate.segments with
              | [] -> failwith "this should never happen (empty edge gate)"
              | leftmost_segment::rest_gate ->
                // Create the overhang if it doesn't exist yet.
                let strand = match leftmost_segment.top_left with None -> {domains=[];kind=DStrandKind.TopStrand} | Some strand -> strand
                let strand = {strand with domains=domain::strand.domains}
                {edge.gate with segments={leftmost_segment with top_left = Some strand}::rest_gate}
            Some (rest,{edge with gate = dgate})
        let updated = iron_top_left_single leading edge
        match updated with
        | None ->
          // Set the top connection for the leftmost gate. See if it has a top connection to the rest of the node.
          let has_top_connection = leading = []
          let connection = match edge.gate.left_connection with
                           | None | Some TopLink -> if has_top_connection then Some TopLink else None
                           | Some BottomLink | Some DoubleLink -> if has_top_connection then Some DoubleLink else Some BottomLink
          // Recurse on children.
          let (child_node_leading,child_node_edges) = iron_top_left_all (edge.child.leading,edge.child.edges)
          let edge = {gate={edge.gate with left_connection=connection}; child={leading=child_node_leading;edges=child_node_edges}}
          (leading,edge)
        | Some (new_leading,new_edge) -> iron_top_left new_leading new_edge
      // To make this easier, I'll temporarily convert the node into a form where each edge is associated to its leading sequence (instead of the trailing sequence). To help avoiding mistakes, the tuple for this association will be in the opposite order.
      let trail_node_edges,trailing = List.mapFold (fun leading (edge,trailing) -> (leading,edge),trailing) leading edges
      let trail_node_edges = List.map (fun (leading,edge) -> iron_top_left leading edge) trail_node_edges
      // Convert back to the form where each edge is associated with its trailing sequence.
      let edges,leading = List.mapFoldBack (fun (leading,edge) trailing -> (edge,trailing),leading) trail_node_edges trailing
      (leading,edges)
  /// This function irons each edge's trailing sequence into the bottom left of the edge. 
  let rec iron_bottom_left_all (leading:LSeq,edges:(LEdge*LSeq)list) =
      /// Moves all domains from the trailing sequence to the adjacent edge, up to the end or the first gap. Returns the modified edge and trailing sequence.
      let rec iron_bottom_left (trailing:LSeq) (edge:LEdge) : (LEdge*LSeq) =
        /// Moves one domain from the trailing sequence to the adjacent edge, if possible. Returns the modified edge and trailing sequence. If there's no domain to move, returns None.
        let iron_bottom_left_single (trailing:LSeq) (edge:LEdge) : (LEdge*LSeq) option =
          match trailing with
          | [] -> None
          | None::rest -> None
          | site::rest ->
            // I've removed a non-gap site immediately following the edge. Convert it to a domain.
            let domain = match site with
                         | Some (LUnbound dom) -> domainc_to_domain dom None
                         | Some (LPseudoknotBound (dom,pk)) -> domainc_to_domain dom (Some pk)
                         | _ -> failwith "this should never happen (bound domain in a node)"
            // Attach it to the bottom left of the edge gate.
            let dgate:DGate =
              match edge.gate.segments with
              | [] -> failwith "this should never happen (empty edge gate)"
              | leftmost_segment::rest_gate ->
                // Create the overhang if it doesn't exist yet.
                let strand = match leftmost_segment.bottom_left with None -> {domains=[];kind=DStrandKind.BottomStrand} | Some strand -> strand
                let strand = {strand with domains=domain::strand.domains}
                {edge.gate with segments={leftmost_segment with bottom_left = Some strand}::rest_gate}
            Some ({edge with gate=dgate},rest)
        let updated = iron_bottom_left_single trailing edge
        match updated with
        | None ->
          // Set the bottom connection for the leftmost gate. See if it has a bottom connection to the rest of the node.
          let has_bottom_connection = trailing = []
          let connection = match edge.gate.left_connection with
                           | None | Some BottomLink -> if has_bottom_connection then Some BottomLink else None
                           | Some TopLink | Some DoubleLink -> if has_bottom_connection then Some DoubleLink else Some TopLink
          // Recurse on children.
          let (child_node_leading,child_node_edges) = iron_bottom_left_all (edge.child.leading,edge.child.edges)
          let edge = {gate={edge.gate with left_connection=connection}; child={leading=child_node_leading;edges=child_node_edges}}
          (edge,trailing)
        | Some (new_trailing,new_edge) -> iron_bottom_left new_edge new_trailing
      let edges = List.map (fun (edge,trailing) -> iron_bottom_left trailing edge) edges
      (leading,edges)
  /// This function irons all domains in an edge's child node's leading sequence into the top right of the edge.
  let rec iron_top_right (edge:LEdge) : LEdge =
      /// Moves one domain in the child node's leading sequence into the edge's top right overhang. Returns the updated edge, or None if no changes were made.
      let iron_top_right_single (edge:LEdge) : LEdge option =
        match edge.child.leading with
        | [] -> None
        | None::rest -> None
        | site::rest ->
          // I've removed a non-gap site at the start of the node.
          let domain = match site with
                       | Some (LUnbound dom) -> domainc_to_domain dom None
                       | Some (LPseudoknotBound (dom,pk)) -> domainc_to_domain dom (Some pk)
                       | _ -> failwith "this should never happen (bound domain in a node)"
          // Attach it to the top right of the edge gate.
          let dgate:DGate =
            match List.rev edge.gate.segments with
            | [] -> failwith "this should never happen (empty edge gate)"
            | rightmost_segment::rest_gate ->
              // Create the overhang if it doesn't exist yet.
              let strand = match rightmost_segment.top_right with None -> {domains=[];kind=TopStrand} | Some strand -> strand
              let strand = {strand with domains=strand.domains@[domain]}
              {edge.gate with segments = {rightmost_segment with top_right = Some strand}::rest_gate |> List.rev}
          Some {edge with gate=dgate;child={edge.child with leading=rest}}
      let updated = iron_top_right_single edge
      match updated with
      | None ->
        // Set the top connection for the rightmost gate. See if it has a top connection to the rest of the node.
        let has_top_connection = edge.child.leading = []
        match List.rev edge.gate.segments with
        | [] -> failwith "this should never happen (empty edge gate)"
        | rightmost_segment::rest_gate ->
          let connection = match rightmost_segment.connection with
                           | None | Some TopLink -> if has_top_connection then Some TopLink else None
                           | Some BottomLink | Some DoubleLink -> if has_top_connection then Some DoubleLink else Some BottomLink
          let rightmost_segment = {rightmost_segment with connection=connection}
          let dgate = {edge.gate with segments=List.rev (rightmost_segment::rest_gate)}
          {edge with gate=dgate}
      | Some edge -> iron_top_right edge
  /// This function irons all domains in an edge's child node's final trailing sequence into the bottom right of the edge.
  let rec iron_bottom_right (edge:LEdge) : LEdge =
      /// Moves one domain in the child node's final trailing sequence into the edge's bottom right overhang. Returns the updated edge, or None if no changes were made.
      let iron_bottom_right_single (edge:LEdge) : LEdge option =
        // I need the last child node edge. 
        match List.rev edge.child.edges with
        | [] ->
          // If the child node has no edges, I'll need to use the leading sequence instead (it'll end up being a right-hairpin).
          match List.rev edge.child.leading with
          | [] -> None
          | None::rest -> None
          | site::rest ->
            let domain = match site with
                         | Some (LUnbound dom) -> domainc_to_domain dom None
                         | Some (LPseudoknotBound (dom,pk)) -> domainc_to_domain dom (Some pk)
                         | _ -> failwith "this should never happen (bound domain in a node)"
            // Attach it to the bottom right of the edge gate.
            let dgate:DGate =
              match List.rev edge.gate.segments with
              | [] -> failwith "this should never happen (empty edge gate)"
              | rightmost_segment::rest_gate ->
                // Create the overhang if it doesn't exist yet.
                let strand = match rightmost_segment.bottom_right with None -> {domains=[];kind=DStrandKind.BottomStrand} | Some strand -> strand
                let strand = {strand with domains=strand.domains@[domain]}
                {edge.gate with segments={rightmost_segment with bottom_right = Some strand}::rest_gate |> List.rev}
            // Return the altered DGate and child node.
            Some {edge with gate=dgate;child={leading=List.rev rest;edges=[]}}
        | (last_child_edge,trailing)::other_child_edges ->
          match List.rev trailing with
          | [] -> None
          | None::rest -> None
          | site::rest ->
            let domain = match site with
                         | Some (LUnbound dom) -> domainc_to_domain dom None
                         | Some (LPseudoknotBound (dom,pk)) -> domainc_to_domain dom (Some pk)
                         | _ -> failwith "this should never happen (bound domain in a node)"
            // Attach it to the bottom right of the edge gate.
            let dgate:DGate =
              match List.rev edge.gate.segments with
              | [] -> failwith "this should never happen (empty edge gate)"
              | rightmost_segment::rest_gate ->
                // Create the overhang if it doesn't exist yet.
                let strand = match rightmost_segment.bottom_right with None -> {domains=[];kind=DStrandKind.BottomStrand} | Some strand -> strand
                let strand = {strand with domains=strand.domains@[domain]}
                {edge.gate with segments={rightmost_segment with bottom_right = Some strand}::rest_gate |> List.rev}
            // Rebuild the last edge of the child node, to subtract the final trailing site.
            let updated_last_child_edge = (last_child_edge,List.rev rest)
            // Rebuild the child node's edges list, with the updated last edge.
            let updated_child_edges = List.rev (updated_last_child_edge::other_child_edges)
            // Return the altered DGate and child node.
            Some {edge with gate=dgate;child={edge.child with edges=updated_child_edges}}
      let updated = iron_bottom_right_single edge
      match updated with
      | None -> 
        // Set the bottom connection for the rightmost gate. See if it has a bottom connection to the rest of the node.
        let has_bottom_connection = match (List.rev edge.child.edges) with [] -> edge.child.leading = [] | (_,trailing)::_ -> trailing = []
        match List.rev edge.gate.segments with
        | [] -> failwith "this should never happen (empty edge gate)"
        | rightmost_segment::rest_gate ->
          let connection = match rightmost_segment.connection with
                           | None | Some BottomLink -> if has_bottom_connection then Some BottomLink else None
                           | Some TopLink | Some DoubleLink -> if has_bottom_connection then Some DoubleLink else Some TopLink
          let rightmost_segment = {rightmost_segment with connection=connection}
          let dgate = {edge.gate with segments =List.rev (rightmost_segment::rest_gate)}
          {edge with gate=dgate}
      | Some edge -> iron_bottom_right edge
  // Get a version of the node where all non-gap domains have been moved to the edges.
  let rec iron_graph (root:LNode) : LNode =
    let (leading,edges) = iron_top_left_all (root.leading,root.edges)
    let (leading,edges) = iron_bottom_left_all (leading,edges)
    let edges = List.map (fun (edge,trailing) -> (iron_top_right edge),trailing) edges
    let edges = List.map (fun (edge,trailing) -> (iron_bottom_right edge),trailing) edges
    // Recurse on child nodes.
    let edges = List.map (fun (edge,trailing) -> {edge with child=iron_graph edge.child}, trailing) edges
    // Now I have a graph where the nodes don't have any site, except for gaps.
    {leading=leading;edges=edges}
  // Special handling of the root node. Because it does not have an in-edge, and it only has a single out-edge, the leading and trailing loop back. Note that I'm going to be using new names for each step (leading1, leading2...) because this makes it far easier to view each step's result while debugging.
  let (edge,trailing) = match root.edges with [(edge,trailing)] -> (edge,trailing) | _ -> failwith "root node does not have 1 edge"
  // Move the edge's trailing to the leading, so that iron_top_left_all can handle it properly.
  let leading1 = trailing@root.leading
  let edges1=[(edge,[])]
  let (leading2,edges2) = iron_top_left_all (leading1,edges1)
  // Move the leading into the edge's trailing, so that iron_bottom_left_all can handle it properly.
  let (edge,trailing) = match edges2 with [(edge,trailing)] -> (edge,trailing) | _ -> failwith "root node does not have 1 edge"
  let edges3=[(edge,trailing@leading2)]
  let leading3=[]
  let (leading4,edges4) = iron_bottom_left_all (leading3,edges3)
  // The right side does not need special handling.
  let edges5 = List.map (fun (edge,trailing) -> (iron_top_right edge),trailing) edges4
  let edges6 = List.map (fun (edge,trailing) -> (iron_bottom_right edge),trailing) edges5
  // Recurse on child nodes.
  let edges7 = List.map (fun (edge,trailing) -> {edge with child=iron_graph edge.child}, trailing) edges6
  // Now I have a graph where the nodes don't have any site, except for gaps.
  {leading=leading4;edges=edges7}

/// This function collapses single-edge nodes, turning them into DBranch terminators.
let collapse_single_edge_nodes (root:LNode) : LNode =
  /// This function handles an edge.
  let rec collapse_edge (edge:LEdge) : LEdge =
    match edge.child with
    | {leading=leading;edges=[]} ->
      // The edge leads to a leaf node. The node is used to terminate the edge on the right. The node should either have a single gap site, or no sites at all.
      match leading with
      | [None] -> {edge with child={leading=[];edges=[]}} // The node has a gap, so the two strands of the edge don't connect. The edge is fine as it is.
      | [] ->
        // The node does not have a gap, so the two strands of the edge connect. This means that the right side of the edge (upper and lower) need to get joined and become a hairpin.
        match List.rev edge.gate.segments with
        | rightmost_segment::rest_of_gate ->
          let top_right = match rightmost_segment.top_right with None -> [] | Some strand -> strand.domains
          let bottom_right = match rightmost_segment.bottom_right with None -> [] | Some strand -> strand.domains
          let updated_rightmost_segment = { rightmost_segment with hairpin_right = Some {domains=(List.rev top_right)@bottom_right;kind=DStrandKind.RightHairpinStrand}
                                                                   top_right = None
                                                                   bottom_right = None
                                                                   connection = None }
          let dgate = {edge.gate with segments = List.rev (updated_rightmost_segment::rest_of_gate)}
          // Update the dgate.
          {edge with gate=dgate;child={leading=[];edges=[]}}
        | _ -> failwith "this should never happen (empty dgate)"
      | _ -> failwith "this should never happen (domain found on a node during collapse)"
    | {leading=leading;edges=edges} ->
      // The edge leads to a non-leaf node. Recurse on all children.
      let edges = List.map (fun (edge,trailing) -> (collapse_edge edge,trailing)) edges
      // Return the edge with the modified children.
      {edge with child={leading=leading;edges=edges}}
  // The root node is special, because it doesn't have an incoming edge. This means that it might itself be used to terminate an edge on the left.
  match root with
  | {leading=leading;edges=[edge,trailing]} ->
    // The root node is a leaf. The node is used to terminate the single out-edge on the left. The node should either have a single gap site, or no sites at all. Note that the leading and trailing sequences can be merged for this purpose, as they'll be adjacent.
    match leading@trailing with
    | [None] -> {leading=[];edges=[collapse_edge (edge),trailing]} // The node has a gap, so the edge is fine as it is.
    | [] ->
      // The node has no gap, so the top-left and bottom-left overhangs need to be joined into a left-hairpin.
      match edge.gate.segments with
      | leftmost_segment::rest_of_gate ->
        let top_left = match leftmost_segment.top_left with None -> [] | Some strand -> strand.domains
        let bottom_left = match leftmost_segment.bottom_left with None -> [] | Some strand -> strand.domains
        let updated_leftmost_segment = { leftmost_segment with hairpin_left = Some {domains=bottom_left@(List.rev top_left);kind=DStrandKind.LeftHairpinStrand}
                                                               top_left = None
                                                               bottom_left = None }
        let dgate = {edge.gate with segments= updated_leftmost_segment::rest_of_gate}
        // Update the dgate and the node, and recurse on the child.
        {leading=[];edges=[collapse_edge ({edge with gate=dgate}),trailing]}
      | _ -> failwith "this should never happen (empty dgate)"
    | _ -> failwith "this should never happen (domain found on a node during collapse)"
  | {leading=leading;edges=edges} ->
    // The root node is not a leaf. Recurse on all children.
    let edges = List.map (fun (edge,trailing) -> (collapse_edge edge),trailing) edges
    {leading=leading;edges=edges}

/// This function changes the graph's direction (i.e. picks a new root node) so that the root is a leaf node. Note that the root should always be a leaf node at this stage, so concretely I'm just doing validation here.
let rotate_graph_structure (root:LNode) : LNode =
  match root.edges with
  | first::second::more ->
    failwith "graph root is not a leaf node"
  | _ -> root

/// This function collapses dual-edge nodes, getting rid of them altogether, joining the two edges into a single edge. Precondition: the graph should be ironed, and the root node should be a leaf.
let rec collapse_dual_edge_nodes (root:LNode) : LNode =
  /// If the edge's child node has a single child, then this function collapses the node and returns the modified edge. Otherwise, it does not modify the edge. Either way, it recurses.
  let rec collapse_edge (edge:LEdge) : LEdge =
    match edge.child.edges with
    | [child_edge,trailing] ->
      // The node has a single child. There are three possible cases: 1) the leading sequence is a gap and the trailing sequence is empty; 2) the leading sequence is empty and the trailing sequence is a gap; 3) both the leading and trailing sequences are empty. Note that there should never be a gap in both the leading and trailing sequences, because that would mean the strands are disconnected.
      // Look at the right side of the parent segment and the left side of the child segment. Note that these sides cannot be hairpins (or there wouldn't even be a branch), so I don't need to check for that.
      match List.rev edge.gate.segments with
      | last_parent_segment::rest_parent ->
        match child_edge.gate.segments with
        | first_child_segment::rest_child ->
          match (edge.child.leading,trailing) with
          | [None],[] ->
            // Case 1. Remove the node. Join the branches with a lower connection.
            let connected_segment = { last_parent_segment with connection = Some BottomLink }
            let merged_dgate = {child_edge.gate with segments = ((List.rev rest_parent)@[connected_segment])@child_edge.gate.segments;left_connection=edge.gate.left_connection}
            // Recurse on the same edge, because it's now pointing to a different node.
            {child_edge with gate=merged_dgate} |> collapse_edge
          | [],[None] ->
            // Case 2. Remove the node. Join the branches with an upper connection.
            let connected_segment = { last_parent_segment with connection = Some TopLink }
            let merged_dgate = {child_edge.gate with segments= ((List.rev rest_parent)@[connected_segment])@child_edge.gate.segments;left_connection=edge.gate.left_connection}
            // Recurse on the same edge, because it's now pointing to a different node.
            {child_edge with gate=merged_dgate} |> collapse_edge
          | [],[] ->
            // Case 3. I'd have to join branches with both upper and lower connections. There are two possible cases: A) the parent branch ends on a double strand and the child branch starts on a double strand; B) any other case.
            match (last_parent_segment.top_right,last_parent_segment.bottom_right,first_child_segment.top_left,first_child_segment.bottom_left) with
            | None,None,None,None ->
              // Case A. Remove the node. Join the segments into a single segment, at the double strand.
              let merged_segment = { top_left = last_parent_segment.top_left
                                     bottom_left = last_parent_segment.bottom_left
                                     hairpin_left = last_parent_segment.hairpin_left
                                     top_right = first_child_segment.top_right
                                     bottom_right = first_child_segment.bottom_right
                                     hairpin_right = first_child_segment.hairpin_right 
                                     double_strand = { domains = last_parent_segment.double_strand.domains@first_child_segment.double_strand.domains; kind = DStrandKind.DoubleStrand };
                                     connection = first_child_segment.connection }
              let merged_dgate = {child_edge.gate with segments=((List.rev rest_parent)@[merged_segment])@rest_child;left_connection=edge.gate.left_connection}
              // Recurse on the same edge, because it's now pointing to a different node.
              {child_edge with gate=merged_dgate} |> collapse_edge
            | _ -> 
              // Case B. The segments have at least one single strand between them. Remove the node. Join the branches with a double connection, forming a bulge.
              let connected_segment = { last_parent_segment with connection = Some DoubleLink }
              let merged_dgate = {child_edge.gate with segments=((List.rev rest_parent)@[connected_segment])@child_edge.gate.segments;left_connection=edge.gate.left_connection}
              // Recurse on the same edge, because it's now pointing to a different node.
              {child_edge with gate=merged_dgate} |> collapse_edge
          | _ -> failwith "this should never happen (unexpected dual-edge node content)"
        | [] -> failwith "this should never happen (empty dgate)"
      | [] -> failwith "this should never happen (empty dgate)"
    | _ -> {edge with child=collapse_dual_edge_nodes edge.child} // The node is either a leaf, or it has more than one child. The first case can be ignored, the second case can never be collapsed. Recurse on outgoing edges.
  let edges = List.map (fun (edge,trailing) -> (collapse_edge edge),trailing) root.edges
  {root with edges=edges}

/// This function takes a graph that's already fully collapsed, and produces the corresponding DBranch.
let convert_to_dbranch (root:LNode) : DBranch =
  match root.edges with
  | [edge,trailing] ->
    let rec convert_child child_edge : DBranch =
      { gate = child_edge.gate
        children = List.map (fun (edge,_) -> convert_child edge) child_edge.child.edges }
    convert_child edge
  | _ -> failwith "this should never happen (root node is not a leaf)"

/// This function converts the intermediate representation into a DSD-style object that can be rendered. If the node is a leaf, then this is a single DStrand. Otherwise, this is a DBranch.
let from_lgraph (root:LNode) : DOrigamiElement =
  match root.edges with
  | [] -> Strand (from_single_node_lgraph root.leading) // Special case: single-node graph. It's just a strand.
  | _ ->
    // This is a proper graph. First of all, I need to "iron" it. This gets me a graph where all domains are in the edges.
    let ironed = iron_root_graph root
    // Collapse single-edge nodes into DGate terminators. This will get me a graph where all leaf nodes are empty and can be ignored.
    let collapsed1 = collapse_single_edge_nodes ironed
    // Ensure that the root is a leaf node. This ensures that any dual-edge nodes have exactly one incoming and one outgoing edge.
    let rotated = rotate_graph_structure collapsed1
    // Collapse dual-edge nodes into DGate junctions. This will get me a graph where leaf nodes can be ignored, and all other nodes become branches.
    let collapsed2 = collapse_dual_edge_nodes rotated
    // The root node is special because it has no in-edge. The method that sets left_connection assumes the presence of a in_edge, so the result may be wrong. Fixing this is trivial, as the left_connection of a root node is always None.
    let collapsed2 = match collapsed2.edges with
                     | [edge,trailing] -> { collapsed2 with edges = [{edge with gate={edge.gate with left_connection=None}},trailing]}
                     | _ -> ironed
    // Convert collapsed graph into a DBranch.
    convert_to_dbranch collapsed2 |> Branch

let from_logic_dsd (l:RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>) : DOrigamiElement =
  let original_proc:ProcessC = to_concrete_process l
  let map_site s =
    match s with
    | BoundC (dom,b) -> LBound (dom,b)
    | UnboundC dom -> LUnbound dom
  let map_strandc (s:StrandC) : LSite list = List.map map_site s
  let proc:LProcess = List.map map_strandc original_proc
  /// This function changes the ordering of StrandC instances within proc so that there are no pseudoknots. If a pseudoknot is inevitable, then one of the bonds that make up the pseudoknot should be "unbound" for the purposes of rendering, and marked so that it can be displayed in some other way.
  let rec reorder_process_avoid_pseudoknots (p:LProcess) : LProcess option =
    /// Takes an element and a list. Returns the set of all lists generated by inserting that element at each possible position within the list.
    let distrib e L =
      let rec aux pre post = 
        seq {
          match post with
          | [] -> yield (L @ [e])
          | h::t -> yield (List.rev pre @ [e] @ post)
                    yield! aux (h::pre) t 
        }
      aux [] L
    /// Takes a list of objects, returns a set of lists that are all possible permutations of the original list.
    let rec perms = function 
      | [] -> Seq.singleton []
      | h::t -> Seq.collect (distrib h) (perms t)
    // Get all possible permutations of the strands within the ProcessC.
    let all_perms : LProcess seq = perms p
    /// Takes a ProcessC and returns a site involved in a pseudoknot within the process, or None if there is no pseudoknot. The process is converted into a single sequence, which is then traversed. Each time a bond site is found, the traversal begins searching for the matching bond site. This is done recursively, so that at any given time I am searching for the site that matches the last unmatched bond site. When the matching bond site is found, the traversal resumes searching for any bond site. If the traversal ends while still searching for a match for some bond site, it means that the matching bond site was skipped because it was encountered while searching for some other match. This in turn means that there is a pseudoknot, and the pseudoknot involves the unmatched site.
    let find_pseudoknot (p:LProcess) : BondC option =
      // For the purpose of finding pseudoknots, all I care about is bonds and their ordering. So I'm getting rid of everything else. I'll filter out non-bond sites, only take the bond ID, and concatenate everything. I'll end up with a list of ints.
      let bonds_sequence : BondC list = p |> List.map (fun s -> List.choose (fun site -> match site with LBound (_,bond) -> Some bond | _ -> None) s) |> List.concat
      // This may have eliminated everything.
      if bonds_sequence = [] then None else
      /// This function searches the sequence for the matching bond, and recursively searches it for any other matches it encounters. If it turns out to be impossible to match a bond, then that bond is returned.
      let rec find_match (b:BondC) (sequence:BondC list) : BondC option*BondC list =
        match sequence with
        | [] -> Some b,[] // No more bonds. No match found.
        | [bb] -> (if b = bb then None else Some b),[] // End of the sequence: last chance to match the bond.
        | bb::rest -> // More bonds.
          // If this is the match, then I'm satisfied and I'll return the rest of the sequence.
          if b = bb then None,rest else
          // This is not the match. I need to start a new search for the match to the new bond.
          let (pseudoknot,more) = find_match bb rest
          // Let's see if the new bond was matched.
          match pseudoknot with
          | None -> find_match b more // The new bond was matched. I'll keep searching for the match to my own bond.
          | _ -> (pseudoknot,more) // The new bond was not matched. Return the pseudoknot.
      /// This functions searches a sequence for pseudoknots. It returns one of the bonds involved in the pseudoknot, or None if there is no pseudoknot.
      let find_pseudoknot (sequence:BondC list) : BondC option =
        /// This function implements a recursive search on the sequence.
        let rec find_pseudoknot_internal (sequence:BondC list) : BondC option*BondC list =
          // If the sequence is empty, I'm done and I have no pseudoknots.
          if sequence = [] then None,[] else
          // Let's see if the first bond has a pseudoknot.
          let pseudoknot = find_match sequence.Head sequence.Tail
          match pseudoknot with
          | None,rest -> find_pseudoknot_internal rest // No pseudoknot so far. Let's look at the next bond.
          | _ -> pseudoknot // Pseudoknot. Return it.
        // Begin the recursive search.
        let (pseudoknot,_) = find_pseudoknot_internal sequence
        pseudoknot
      let pseudoknot = find_pseudoknot bonds_sequence
      pseudoknot
    /// This function returns true if the process is connected. A process that's not connected would result in disjointed graphs.
    let is_connected (p:LProcess) : bool =
      let tagged = List.map (fun st -> (st, ref false)) p
      let rec tag_by_bond (bond:BondC) =
        let has_bond (st:LSite list) = List.exists (fun s -> match s with LBound (_,b) -> b=bond | _ -> false) st
        List.iter (fun (st,tag) -> if has_bond st then tag_strand (st,tag) else ()) tagged
      and tag_strand (st,tag) =
        if !tag then () else
        tag := true
        List.iter (fun site -> match site with LBound (_,b) -> tag_by_bond b | _ -> ()) st
      tag_strand tagged.Head
      not (List.exists (fun t -> false = !(snd t)) tagged)
    // Find a "good" permutation. A good permutation has no pseudoknots and no unbound strands.
    match Seq.tryFind (fun pp -> let pseudoknot = find_pseudoknot pp in pseudoknot.IsNone && is_connected pp) all_perms with
    | Some ok_process -> Some ok_process
    | None ->
      // No permutation that avoids pseudoknots could be found. I'll unbind domains one after the other until I end up with a "good" permutation.
      let all_bonds : BondC list = p |> List.map (fun s -> List.choose (fun site -> match site with LBound (_,bond) -> Some bond | _ -> None) s) |> List.concat |> List.distinct
      let try_remove_bond (bond:BondC) : LProcess option =
        // The bound domain will be unbound and marked.
        let map_site (s:LSite) =
          match s with
          | LBound (dom,b) -> (if b = bond then LPseudoknotBound (dom,b) else LBound (dom,b))
          | other -> other
        let new_process = List.map (fun s -> List.map map_site s) p
        reorder_process_avoid_pseudoknots new_process
      // For each bond, get the process that results from removing that bond (provided that the removal results in a "good" permutation).
      let candidates:LProcess list = List.choose try_remove_bond all_bonds
      // Search for the "best" solution.
      match candidates with 
      | [] -> None
      | _ -> 
        /// Ranks a process (returns the number of eliminated pseudoknot bonds). The lower, the better.
        let rank (p:LProcess) = p |> List.concat |> List.sumBy (fun st -> match st with LPseudoknotBound _ -> 1 | _ -> 0)
        // Find the candidate where the least number of bonds has been eliminated.
        let pp = List.minBy rank candidates
        Some pp
      
  // Get a ProcessC with no pseudoknots.
  let reordered_proc = match reorder_process_avoid_pseudoknots proc with
                       | Some proc -> proc
                       | None -> failwith "this should never happen (unable to eliminate pseudoknots and/or create a fully connected graph)"
  // Convert the process into a LSeq. The LSeq is the result of stacking the strands back-to-back and putting a gap between them.
  let original_lseq = (List.map (fun s -> (List.map Some s)@[None]) reordered_proc) |> List.concat
  /// This function reorders a LSeq by moving elements from the bottom to the top, in such a way that the first level of a recursive search for bonded couples will only find one couple. This results in the graph only having one out-edge from the root. This is a property that's required later on. Note that doing this is allowed because the full initial LSeq can be considered to be circular.
  let make_single_root lseq : LSeq =
    /// This function splits the sequence in two, around a point that's between two bond sites that have no other bond sites between them. Any sequence without pseudoknots has to have at least one such point.
    let rec take_leading (before:LSeq) (sourceseq:LSeq) =
      match sourceseq with
      | [] -> (before,[])
      | head::rest ->
        let new_before = (before@[head])
        match head with
        | Some (LBound (_,bond)) ->
          let rec find_match seq =
            match seq with
            | (Some (LBound (_,bond2)))::_ -> bond=bond2
            | [] -> false
            | _::rest -> find_match rest
          if find_match rest then new_before,rest else
          take_leading new_before rest
        | _ -> take_leading new_before rest
    let (before,after) = take_leading [] lseq
    after@before
  let lseq = make_single_root original_lseq
  /// This function "processes" a lseq into a node. It examines all sites in sequence. When it finds a bound site, it searches for the matching site in the rest of the sites. The two bound sites become an edge, and sites between them gets split off into a new child node, which gets recursively processed. Examination then resumes from the first site immediately following the second bound site.
  let rec process_lseq (sourceseq:LSeq) : LNode =
    // Get the leading part of the node. This is the sequence that goes from the start of the node (the site immediately after the parent-edge), to either the first child-edge, or the end of the node (if the node has no bound sites and therefore no children).
    let rec take_sequence (leading:LSeq,rest:LSeq) : LSeq*LSeq =
      match rest with
      | Some (LBound (_,_))::_ -> (leading,rest) // found a bond site: the leading bit is done. Note I'm not consuming the site at this point.
      | site::more -> take_sequence (leading@[site],more) // found a non-bond site: add it to the leading bit and recurse.
      | [] -> (leading,rest) // reached the end: return.
    // Get the edges, including the actual edge, the child node, and the trailing sequence.
    let rec take_edges (edges:(LEdge*LSeq)list,rest:LSeq) : (LEdge*LSeq)list*LSeq =
      match rest with
      | [] -> (edges,rest) // reached the end: return.
      | Some (LBound (dom1,bond))::more -> // found a bond site, now I need to scan for the corresponding site.
        let rec take_child_seq (childseq:LSeq,rest:LSeq) : LSeq*LSeq =
          match rest with
          | Some (LBound (_,same_bond))::moremore when same_bond = bond -> (childseq,moremore) // found the bound site. I'm consuming it and returning the child sequence.
          | site::moremore -> take_child_seq (childseq@[site],moremore) // bound site not yet found, add this to the sequence and recurse.
          | [] -> failwith "cannot find a matching bond site; this should never happen (bad pseudoknot elimination?)"
        let (child_seq,moreseq) = take_child_seq ([],more)
        // Create the edge gate.
        let edge_gate_segments = [{ top_left = None
                                    top_right = None
                                    bottom_left = None
                                    bottom_right = None
                                    hairpin_left = None
                                    hairpin_right = None
                                    double_strand = { domains = [domainc_to_domain dom1 None]; kind = DoubleStrand }
                                    connection = None }]
        let edge_gate = {segments=edge_gate_segments; left_connection=None}
        // Recursively create the child node.
        let child_node = process_lseq child_seq
        // Get the trailing sequence.
        let (trailing_sequence,moremoreseq) = take_sequence ([],moreseq)
        // Make the new edge + trailing sequence.
        let edge = {gate=edge_gate;child=child_node},trailing_sequence
        // Append it.
        let edges = edges@[edge]
        // Recurse on the following sequence.
        let (edges,restseq) = take_edges (edges,moremoreseq)
        (edges,restseq)
      | _ -> failwith "non-bond site after following a leading/trailing sequence; this should never happen"
    let (leading,edgesseq) = take_sequence ([],sourceseq)
    let (edges,_) = take_edges ([],edgesseq)
    {leading=leading;edges=edges}
  let topNode = process_lseq lseq
  let dbranch = from_lgraph topNode
  dbranch


// "Branches" renderer for DSD species

/// Produces the CSS style for the SVG produced by this module. Requires the set of domain->color associations. The rest of the styles are fixed.
let style cm =
  let dom_styles = cm |> Lib.string_of_list (fun (id:string, col) -> let id = sanitize_id id in "path."+base_class+".dom_"+id + " { stroke: " + col + " } text."+base_class+".dom_"+id+" { fill: " + col + " }") " " in
  let base_style = "g { fill: none; } path."+base_class+" { stroke: silver; stroke-width: " + Svg.display_float line_width + "; stroke-linejoin: round; } text."+base_class+" { fill: black; stroke-width: 0; font-family: Verdana,Arial,sans-serif; font-size: " + string (int font_size) + "px; text-anchor: middle } text.fixedwidth { font-family:Lucida Console,monospace } text.pseudoknot { font-weight:bold } path.shortbranchjoin { "+short_branch_join_style+" } path.branchjoin { "+branch_join_style+" }"
  let path_style = sprintf "path { stroke: silver; stroke-width: %f; stroke-linejoin: round; fill: none; }" line_width
  let text_style = sprintf "text { stroke: black; fill: black; stroke-width: 0; font-family: Verdana,Arial,sans-serif; font-size: %fpx; text-anchor: middle; }" font_size
  let base_style = path_style+" "+text_style+" "+base_style
  let tether_style = "path."+base_class+".tether-pin { stroke : black }"
  base_style + " " + dom_styles + " " + tether_style
  
let palette = [ "red"; "green"; "blue"; "orange"; "purple"
              ; "cyan"; "brown"; "magenta"; "dodgerblue"; "orchid"
              ; "lawngreen"; "lightskyblue"; "darksalmon"; "teal"; "rosybrown"
              ; "navajowhite"; "olive"; "darkseagreen"; "darkgoldenrod"; "pink" ]

/// Takes a partial map of domains->colors and a complete list of species, and returns a complete map of domain IDs->colors.
let dom_map (dcs:Hashtable.t<string,string>) species_list =
  let (_,taken_colours) = Hashtable.to_list dcs |> List.unzip
  let free_colours = palette |> List.filter (fun c -> List.exists ((=) c) taken_colours |> not) in
  let dom_counter = ref 0 in
  let ret = Hashtable.empty() in
  let add d =
    let name = (d |> from_dsd_domain Sequence.empty).name in
    match name |> Hashtable.tryFind ret with
    | None ->
      let colour = match name |> Hashtable.tryFind dcs with
                   | None ->
                     let c = free_colours.[!dom_counter] in
                     dom_counter := (!dom_counter + 1) % free_colours.Length;
                     c
                   | Some c -> c in
      Hashtable.add ret name colour
    | Some _ -> ()
  let xx = species_list
            |> List.collect Species.domains
            |> List.filter Domain.is_toehold
            |> Lib.remove_duplicates (=)
  List.iter add xx;
  ret
  
/// Returns the CSS style for the SVG representation of the given species. You can pass a partial map of domains->colors; the rest of the domains will get assigned colors automatically.
let species_style dc (sp:Species.t list) : string =
  let dc = dom_map dc sp
  dc |> Hashtable.to_list |> style

let species_to_svg_branches (arrange:arrange_mode) (mode:renderer_mode) (mapping:Sequence.mapping) (rotate_labels:bool) (style:string) (sp:Species.t) : string =
  let nucleotides = mode = renderer_mode.Nucleotides
  let g = match sp with
          | Species.STRAND strand ->
            let dstrand = from_dsd_strand mapping strand
            dstrand_to_group mode rotate_labels None false None false false false dstrand
          | Species.GATE gate ->
            let dgate = from_dsd_gate mapping gate
            dgate_to_group mode rotate_labels dgate
          | Species.ORIGAMI origami ->
            let dorigami = from_dsd_origami mapping origami
            dorigami_to_group arrange mode rotate_labels dorigami
          | Species.UNKNOWN unknown ->
            unknown_to_group unknown
          | Species.LogicDsdProcess logic ->
            let delement = from_logic_dsd logic
            match delement with
            | Strand strand -> dstrand_to_group mode rotate_labels None false None false false false strand
            | Branch branch -> dbranch_to_group arrange mode rotate_labels branch
  let box = Svg.emerge_groups [g]
  let ret = Svg.to_string_normalise (not rotate_labels && not nucleotides) style box
  ret