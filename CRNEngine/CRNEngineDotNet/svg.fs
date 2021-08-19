[<JavaScript>]
module Microsoft.Research.CRNEngine.Svg

(* Bits from Lib reimplemented *)
let on_option f d = function
  | None -> d
  | Some x -> f x

let string_of_list f sep = function
  | [] -> ""
  | xs -> xs |> List.map f |> List.reduce (fun a b -> a + sep + b)

type point = { x: float; y: float }

let display_float (f: float) = (f * 1000.0 |> floor) / 1000.0 |> string
let point_to_string p = display_float p.x + "," + display_float p.y

///Returns the length of the vector corresponding to the given point.
let norm v = (v.x * v.x + v.y * v.y) |> sqrt

///Scales a point by a given factor.
let scale_by s p = { x = s * p.x; y = s * p.y }
///Scales a point so that its norm becomes the specified number.
let scale_to s p = p |> scale_by (s / norm p)

let pi = 3.14159265359
///Returns the angle (in radians) of the vector corresponding to the given point.
let dir v =
  match v.x, v.y with
  | 0.0, 0.0 -> 0.0
  | x, 0.0 -> if x > 0.0 then 0.0 else pi
  | 0.0, y -> if y > 0.0 then pi / 2.0 else 3.0 * pi / 2.0
  | x, y ->
    let r = y / x |> atan in
    if x > 0.0 then r else r + pi
///Converts radians to degrees.
let rad_to_deg r = 180.0 * r / pi
///Returns the angle (in degrees) of the vector corresponding to the given point.
let ang v = dir v |> rad_to_deg
///The origin.
let origo = { x = 0.0; y = 0.0 }
///A "standard" radius for a drawn point. This is used to compute the boundaries of objects. Note that this is assuming a given stroke width.
let pointradius = 1.25
///A "standard" letter width. Note that this relies on assumptions on fonts, and will not work correctly in the general case.
let default_letter_width = 8.0
///A "standard" font size. Note that if the style then selects a different font size, this will not work correctly.
let fontsize = 15
let text_height = (float fontsize) + 2.0 (* allow for asterisk *)
let half_text_height = text_height / 2.0

///Vector sum of two points.
let translate_point_by t p = { x = t.x+p.x; y = t.y+p.y }
///Vector subtraction of two points.
let sub a b = { x = a.x-b.x; y = a.y-b.y }
///Rotates a point by a given angle (in degrees) around a pivot.
let rotate_point_around pivot angle p =
  let v = sub p pivot in
  let d = norm v in
  let a = dir v in
  let rads = a + pi * angle / 180.0 in
  let v_rotated = { x = d * cos rads; y = d * sin rads } in
  translate_point_by pivot v_rotated

///An anchor is a named point. This allows it to be given semantic value.
type anchor = { anchor_id: string; anchor_point: point }
with override this.ToString() = sprintf "%s:{%f,%f}" this.anchor_id this.anchor_point.x this.anchor_point.y end

///Applies a transform to an anchor.
let transform_anchor_by t a = { a with anchor_point = t a.anchor_point }
///Translates an anchor.
let translate_anchor_by = translate_point_by >> transform_anchor_by
///Rotates an anchor around a pivot. Angle is in degrees.
let rotate_anchor_around pivot angle = rotate_point_around pivot angle |> transform_anchor_by

///This represents an SVG arc. Good example for this at http://codepen.io/lingtalfi/pen/yaLWJG
type arc = { rx: float; ry: float; large_flag: bool; sweep_flag: bool; angle: float }

let flag_to_string f = if f then "1" else "0"
///Produces the parameters for the SVG arc command, given an arc object.
let arc_to_string a = display_float a.rx + "," + display_float a.ry + " " + display_float a.angle + " " + flag_to_string a.large_flag + "," + flag_to_string a.sweep_flag

///This represents one command of an SVG path (a segment).
type path_command =
  | MoveTo of point
  | LineTo of point
  | ArcTo of point * arc
  | ClosePath

///Produces the SVG command for a path_command.
let path_command_to_string = function
  | MoveTo p -> "M " + point_to_string p 
  | LineTo p -> "L " + point_to_string p
  | ArcTo (p, a) -> "A " + arc_to_string a + " " + point_to_string p
  | ClosePath -> "z"

///Transforms a path command .
let transform_path_command_by t = function
  | MoveTo p -> p |> t |> MoveTo
  | LineTo p -> p |> t |> LineTo
  | ArcTo (p, a) -> (p |> t, a) |> ArcTo
  | ClosePath -> ClosePath
///Translates a path command.
let translate_path_command_by = translate_point_by >> transform_path_command_by
///Rotates a path command around a pivot. Angle is in degrees.
let rotate_path_command_around pivot angle = rotate_point_around pivot angle |> transform_path_command_by

///Creates a list of path_commands that represents a rounded box. The upper_left and lower_right parameters are the corners of the "inner" box.
let rounded_box upper_left lower_right radius =
  let arc = { rx = radius; ry = radius; large_flag = false; sweep_flag = true; angle = 0.0 } in
  [ MoveTo { x = upper_left.x + radius; y = upper_left.y }
  ; LineTo { x = lower_right.x - radius; y = upper_left.y }
  ; ArcTo ( { x = lower_right.x; y = upper_left.y + radius }, arc)
  ; LineTo { x = lower_right.x; y = lower_right.y - radius }
  ; ArcTo ( { x = lower_right.x - radius; y = lower_right.y }, arc)
  ; LineTo { x = upper_left.x + radius; y = lower_right.y }
  ; ArcTo ( { x = upper_left.x; y = lower_right.y - radius }, arc)
  ; LineTo { x = upper_left.x; y = upper_left.y + radius }
  ; ArcTo ( { x = upper_left.x + radius; y = upper_left.y }, arc)
  ; ClosePath ]

///Represents a label. The label has an anchor point, which seems to be intended to be the middle of the label. Label_dir is the direction of rotation of the label (as a vector from the anchor).
type label =
  { label_text : string
  ; label_anchor : point
  ; label_class : string option
  ; letter_width : float
  ; label_dir : point }

///Retrieves the rotation angle of a label (in degrees).
let label_angle l = sub l.label_dir l.label_anchor |> ang

///This function rotates the label so that it is not upside-down (it leaves the label unchanged if it is not upside-down to begin with).
let normalise_label l =
  let d = sub l.label_dir l.label_anchor in
  let a = (ang d) % 360.0 in
  let flip = a < 270.0 && a > 90.0 in
  let nanch, ndir =
    if flip then
       let v = rotate_point_around origo -90.0 d |> scale_to half_text_height in
       let nd = rotate_point_around origo -180.0 d in
       let na = translate_point_by v l.label_anchor in
       na, translate_point_by nd na
    else
      l.label_anchor, l.label_dir
  { l with
        label_anchor = nanch
        label_dir = ndir }

///Produces the SVG element corresponding to a label.
let label_to_string normalise l =
  let l = if normalise then normalise_label l else l in
  "<text "+ (match l.label_class with None -> "" | Some c -> "class=\"" + c + "\" ")+" x=\"" + display_float l.label_anchor.x + "\" y=\"" + display_float l.label_anchor.y + "\" transform=\"rotate(" + (l |> label_angle |> display_float) + " " + point_to_string l.label_anchor + ")\">" + l.label_text + "</text>"

///I think this is supposed to produce a rectangle (in the form of a list of two points) that contains the label. However, it appears to be producing a square. If the label is long, this square can be substantially bigger than the label. I don't know whether this is intentional.
let points_in_label l =
  let len = float (String.length l.label_text) * l.letter_width in
  let r = max ((len |> float) / 2.0) text_height in
  [ { x = l.label_anchor.x - r
    ; y = l.label_anchor.y - r }
  ; { x = l.label_anchor.x + r
    ; y = l.label_anchor.y + r } ]

///Applies a transform to a label's position.
let transform_label_by t l = { l with
                                   label_anchor = l.label_anchor |> t
                                   label_dir = l.label_dir |> t }
///Translates a label.
let translate_label_by = translate_point_by >> transform_label_by
///Rotates a label around a pivot. Angle is in degrees.
let rotate_label_around pivot angle = rotate_point_around pivot angle |> transform_label_by

///Represents a path. In this context, a path is a series of path commands, plus a label.
type path =
  { path_class : string option
  ; commands : path_command list
  ; path_label : label option }

///Produces the SVG elements for the path. These, in the general case, include the path itself plus a text element for the label.
let path_to_string normalise p =
  let pathString = match p.commands with
                   | [] -> "" 
                   | commands -> "<path " + (match p.path_class with None -> "" | Some c -> "class=\"" + c + "\" ")+"d=\"" + string_of_list path_command_to_string "\r\n         " commands + "\" />"
  match p.path_label with
  | None -> pathString
  | Some label -> pathString + "\r\n" + label_to_string normalise label

///Produces a set of points that characterizes the path, in the sense that a rectangle that contains all of these points will contain the path.
let points_in_path path =
  let pathPoints =   List.collect
                       (function
                        | MoveTo p | LineTo p -> [ { x = p.x + pointradius
                                                   ; y = p.y + pointradius }
                                                 ; { x = p.x - pointradius
                                                   ; y = p.y - pointradius } ]
                        | ArcTo (p, a) -> [ { x = p.x + a.rx
                                            ; y = p.y + a.ry }
                                          ; { x = p.x - a.rx
                                            ; y = p.y - a.ry } ]
                        | ClosePath -> [])
                       path.commands
  match path.path_label with
  | None -> pathPoints
  | Some label -> points_in_label label @ pathPoints

///Applies a transform to the path and its label.
let transform_path_by t p = { p with
                                  commands = p.commands |> List.map (transform_path_command_by t)
                                  path_label = match p.path_label with None -> None | Some label -> label |> transform_label_by t |> Some }
///Translates a path and its label.
let translate_path_by = translate_point_by >> transform_path_by
///Rotates a path and its label around a pivot. Angle is in degrees.
let rotate_path_around pivot angle = rotate_point_around pivot angle |> transform_path_by

///Represents the content of a group; this can be either a series of paths, or a chunk of raw SVG code.
type content =
  | Paths of path list
  | Raw of string

///Produces the SVG string corresponding to a given content instance.
let content_to_string normalise =
  function
  | Paths paths -> string_of_list (path_to_string normalise) "\r\n  " paths
  | Raw svg -> svg

///Produces a list of points that characterize the content, in the sense that a rectangle containing these points will contain the content. Note however that this does *not* work if the content is raw SVG. This means that boxes containing raw SVG will need to have their size set explicitly.
let points_in_content =
  function
  | Paths paths -> List.collect points_in_path paths
  | Raw _ -> []

///Applies a transform to content. This does not work on raw SVG content. I wonder whether this should actually throw an exception on attempts to apply it to raw-SVG contents. It seems like it just shouldn't be done.
let transform_content_by t =
  function
  | Paths paths -> paths |> List.map (transform_path_by t) |> Paths
  | Raw svg -> Raw svg

///This represents an SVG group. The group can contain content (which may be a set of paths or a single chunk of raw SVG), and it can also contain other groups. It can have anchors; the semantics of the anchor are user-defined. Finally, it may have an offset; the offset is the position of the group itself, as opposed to the position of the group's content. For example, applying transforms to the group will not change the offset.
type group =
  { name : string
  ; content : content
  ; anchors : anchor list
  ; debug_anchors : bool
  ; sub_groups : group list
  ; offset : point option }

let content_group content = { name = ""; content = content; anchors = []; debug_anchors = false; sub_groups = []; offset = None }

///Turns a point option into an SVG translate transform (or an empty string, if None). Used for group offsets.
let translate_to_string =
  on_option (point_to_string >> (fun s -> " transform=\"translate(" + s + ")\"")) ""

let debug_anchor (anchor:anchor) : string =
  let circle = sprintf "<circle cx=\"%f\" cy=\"%f\" r=\"5\" stroke=\"#DDDDFF\" stroke-width=\"0.5\"/>" anchor.anchor_point.x anchor.anchor_point.y
  circle

///Produces the SVG string corresponding to a group.
let rec group_to_string normalise g =
  "<g" + (g.offset |> translate_to_string) + ">\r\n  " +
  content_to_string normalise g.content +
  (if g.debug_anchors then String.concat " " (g.anchors |> List.map debug_anchor) else "") +
  string_of_list (group_to_string normalise) "\r\n  " g.sub_groups +
  "\r\n</g>"

///Returns a set of point that characterizes the group, in the sense that a rectangle containing all of these points will contain the group. Note that this does not account for raw SVG content.
let rec points_in_group g =
  points_in_content g.content @ List.collect points_in_group g.sub_groups

///Applies a transform to the group's content.
let rec transform_group_by t g =
  { name = g.name
  ; content = g.content |> transform_content_by t
  ; anchors = g.anchors |> List.map (transform_anchor_by t)
  ; debug_anchors = g.debug_anchors
  ; sub_groups = g.sub_groups |> List.map (transform_group_by t)
  ; offset = g.offset }
///Translates the group's content.
let translate_group_by = translate_point_by >> transform_group_by
///Rotates the group's content around a pivot. Angle is in degrees.
let rotate_group_around pivot angle = rotate_point_around pivot angle |> transform_group_by

///Retrieves one of the group's anchors by anchor ID.
let get_anchor a_id g = List.find (fun ga -> ga.anchor_id = a_id) g.anchors
///Retrieves the point of one of the group's anchors by anchor ID.
let get_anchor_point a_id g = let a = get_anchor a_id g in a.anchor_point

///Returns the vector between two anchors (identified by group and ID). Note that this will give wrong results if the anchors belong to groups with different offsets!
let anchor_to_anchor (g1, a1) (g2, a2) =
  sub (get_anchor_point a2 g2) (get_anchor_point a1 g1)

///Rotates a group around one of its own anchors, identified by ID. Angle is in degrees
let rotate_group_around_anchor a_id angle g =
  let pivot = get_anchor_point a_id g in
  rotate_group_around pivot angle g

///Translates a group so that one of its anchors (identified by ID) ends up aligned on a given point.
let translate_group_to p (g, a) =
  let gp = (get_anchor a g).anchor_point in
  let t = sub p gp in
  translate_group_by t g

///Translates group g2 so that one of its anchors (identified by ID a2) ends up aligned on anchor a1 of group g1. Note that this will give wrong results if the anchors belong to groups with different offsets.
let attach_group_to (g1,a1) (g2,a2) =
  let p1 = (get_anchor a1 g1).anchor_point in
  translate_group_to p1 (g2,a2)

///A box represents a group with size.
type box =
  { box_group : group
  ; box_dim : point }

type t = box

///Helper function that produces a box which contains a single label as its content.
let label_box lclass letter_width text =
  let dim_x = text |> String.length |> float |> (*) letter_width in
  let label = { label_text = text
              ; label_anchor = { x = 0.0
                               ; y = text_height }
              ; label_class = lclass
              ; label_dir = { x = dim_x
                            ; y = text_height }
              ; letter_width = letter_width } |> Some in
  let path = { path_class = lclass
             ; commands = []
             ; path_label = label } in
  let group = { name = ""
              ; content = Paths [path]
              ; anchors = []
              ; debug_anchors = false
              ; sub_groups = []
              ; offset = None } in
  { box_group = group
  ; box_dim = { x = dim_x
              ; y = text_height } }

///Turns a list of groups into a box. Calculates the size of the box based on the points used in the groups (this will ignore raw SVG contents).
let emerge_groups gs =
  let points = List.collect points_in_group gs in
  let xs = points |> List.map (fun p -> p.x) in
  let ys = points |> List.map (fun p -> p.y) in
  let upper_left = { x = xs |> List.min
                   ; y = ys |> List.min } in
  let lower_right = { x = xs |> List.max
                    ; y = ys |> List.max } in
  let d = sub origo upper_left in
  { box_group = { name = gs |> List.fold (fun n g -> n+" "+g.name) ""
                ; content = Paths []
                ; anchors = []
                ; debug_anchors = false
                ; sub_groups = gs |> List.map (translate_group_by d)
                ; offset = None }
  ; box_dim = lower_right |> translate_point_by d }

///Stacks a set of boxes vertically. The margin is a margin around the stack, while vskip is the distance between elements of the stack. The boxes are aligned to the left.
let stackv margin vskip bs =
  let sgs, dim =
    bs |> List.fold
            (fun (acc_gs, acc_p) b -> ( { b.box_group with offset = Some acc_p }::acc_gs
                                      , { x = acc_p.x; y = acc_p.y + b.box_dim.y + vskip } ))
            ([], margin) in
  { box_group = { name = ""
                ; content = Paths []
                ; anchors = []
                ; debug_anchors = false
                ; sub_groups = sgs
                ; offset = None }
  ; box_dim = { x = (match bs with [] -> 0.0 | _ -> bs |> List.map (fun b -> b.box_dim.x) |> List.max) + 2.0 * margin.x
              ; y = dim.y - vskip + margin.y } }

///Stacks a set of boxes horizontally. The margin is a margin around the stack, while hskip is the distance between elements of the stack. The boxes are aligned to the bottom.
let stackh margin hskip bs =
  let sgs, dim =
    bs |> List.fold
            (fun (acc_gs, acc_p) b -> ( { b.box_group with offset = Some acc_p }::acc_gs
                                      , { x = acc_p.x + b.box_dim.x + hskip; y = acc_p.y } ))
            ([], margin) in
  { box_group = { name = ""
                ; content = Paths []
                ; anchors = []
                ; debug_anchors = false
                ; sub_groups = sgs
                ; offset = None }
  ; box_dim = { x = dim.x - hskip + margin.x
              ; y = (match bs with [] -> 0.0 | _ -> bs |> List.map (fun b -> b.box_dim.y) |> List.max) + 2.0 * margin.y } }

///Stacks a set of boxes horizontally. The margin is a margin around the stack, while hskip is the distance between elements of the stack. The boxes are aligned to the center.
let stackhc margin hskip bs =
  let height = bs |> List.map (fun b -> b.box_dim.y) |> List.max in
  let sgs, dim =
    bs |> List.fold
            (fun (acc_gs, acc_p) b -> ( { b.box_group with offset = Some { x = acc_p.x; y = acc_p.y + (height - b.box_dim.y)/2.0 } }::acc_gs
                                      , { x = acc_p.x + b.box_dim.x + hskip; y = acc_p.y } ))
            ([], margin) in
  { box_group = { name = ""
                ; content = Paths []
                ; anchors = []
                ; debug_anchors = false
                ; sub_groups = sgs
                ; offset = None }
  ; box_dim = { x = dim.x - hskip + margin.x
              ; y = height + 2.0 * margin.y } }

///Produces an SVG element for the given box, adding a style string. Note that embedded styles will leak on the rest of the page; try to always use unique CSS class names.
let to_string_normalise normalise style b =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"" + display_float b.box_dim.y + "\" width=\"" + display_float b.box_dim.x + "\">\r\n  <style>" + style + "</style>\r\n  " +
  group_to_string normalise b.box_group +
  "\r\n</svg>"
let to_string style b = to_string_normalise true style b

///Produces an SVG group for the given box.
let to_element_string_normalise normalise b =
  let group_to_string_with_size g =
    "<g width=\"" + display_float b.box_dim.x + "\" height=\"" + display_float b.box_dim.y + "\" " + (g.offset |> translate_to_string) + ">\r\n  " +
    content_to_string normalise g.content +
    string_of_list (group_to_string normalise) "\r\n  " g.sub_groups +
    "\r\n</g>" in
  group_to_string_with_size b.box_group
let to_element_string b = to_element_string_normalise true b