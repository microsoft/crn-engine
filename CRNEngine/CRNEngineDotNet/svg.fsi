// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Svg

type point = { x: float; y: float }

type anchor = { anchor_id: string; anchor_point: point }

type arc = { rx: float; ry: float; large_flag: bool; sweep_flag: bool; angle: float }

type path_command =
  | MoveTo of point
  | LineTo of point
  | ArcTo of point * arc
  | ClosePath

type label =
  { label_text : string
  ; label_anchor : point
  ; label_class : string option
  ; letter_width : float
  ; label_dir : point }

type path =
  { path_class : string option
  ; commands : path_command list
  ; path_label : label option }

type content =
  | Paths of path list
  | Raw of string

type group =
  { name : string
  ; content : content
  ; anchors : anchor list
  ; debug_anchors : bool
  ; sub_groups : group list
  ; offset : point option }
val content_group : content -> group

type box =
  { box_group : group
  ; box_dim : point }

type t = box

val anchor_to_anchor : (group*string) -> (group*string) -> point
val default_letter_width : float
val label_box : string option -> float -> string -> box
val display_float : float -> string
val pi : float
val origo : point
val dir : point -> float
val ang : point -> float
val rad_to_deg : float -> float
val norm : point -> float
val translate_point_by : point -> point -> point
val sub : point -> point -> point
val rotate_point_around : point -> float -> point -> point
val scale_by : float -> point -> point
val scale_to : float -> point -> point
val rotate_path_command_around : point -> float -> (path_command -> path_command)
val rounded_box : point -> point -> float -> path_command list
val rotate_path_around : point -> float -> (path -> path)
val transform_path_by : (point -> point) -> path -> path
val translate_path_by : (point -> path -> path)
val rotate_label_around : point -> float -> (label -> label)
val translate_group_by : (point -> group -> group)
val rotate_group_around_anchor : string -> float -> group -> group
val attach_group_to : (group * string) -> (group * string) -> group
val get_anchor_point : string -> group -> point
val emerge_groups : group list -> box
val stackv : point -> float -> box list -> box
val stackh : point -> float -> box list -> box
val stackhc : point -> float -> box list -> box

val to_string : string -> box -> string
val to_string_normalise : bool -> string -> box -> string
val to_element_string : box -> string
val to_element_string_normalise : bool -> box -> string