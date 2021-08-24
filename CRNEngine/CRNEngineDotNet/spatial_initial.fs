// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Spatial_initial
open Operators

let distance_from_xy (x,y) (xpos,ypos) = sqrt (((xpos - x)**2.0) + ((ypos - y)**2.0))

type core = { 
  inner: float
  outer: float
  width: float
}
with
  member c.to_string () = sprintf "centralcore = {inner=%1.3f; outer=%1.3f; width=%1.3f}" c.inner c.outer c.width
  member c.apply1d (pos,value) = if (abs (pos - 0.5) <= (c.width / 2.0)) then pos, value + c.inner else pos, value + c.outer
  member c.apply2d (pos,value) = 
        if (distance_from_xy (0.5,0.5) pos <= (c.width / 2.0)) 
        then pos, c.inner + value 
        else pos, c.outer + value

let default_core = {
    inner = 0.0
    outer = 0.0
    width = 0.0
  }

  
type point = { 
  x: float
  y: float
  width: float
  value: float
}
with 
  member p.to_string() = sprintf "{x=%1.3f; y=%1.3f; width=%1.3f; value=%1.3f}" p.x p.y p.width p.value
  member p.apply1d (pos, value) = if abs (p.x - pos) <= (p.width / 2.0) then pos, value + p.value else pos, value
  member p.apply2d (pos,value) = if distance_from_xy (p.x,p.y) pos <= (p.width / 2.0) then pos, p.value + value else pos, value

let default_point = {
  x     = 0.0
  y     = 0.0
  width = 0.0
  value = 0.0
}

type rectangle = {
  xmin: float
  xmax: float
  ymin: float
  ymax: float
  value: float
}
with  
  member r.to_string () = sprintf "{xmin=%1.3f; xmax=%1.3f; ymin = %1.3f; ymax = %1.3f; value=%1.3f}" r.xmin r.xmax r.ymin r.ymax r.value
  member r.apply1d (pos,value) = if (pos >= r.xmin) && (pos <= r.xmax) then pos, value + r.value else pos, value
  member r.apply2d ((xpos,ypos),value) = if (xpos >= r.xmin && xpos <= r.xmax && ypos >= r.ymin && ypos <= r.ymax) then (xpos,ypos), r.value + value else (xpos,ypos), value

let default_rectangle = {
  xmin = 0.0
  xmax = 1.0
  ymin = 0.0
  ymax = 1.0
  value = 0.0
}

type t = { 
    random : float
    core : core option
    points : point list
    rectangles : rectangle list
  }

let defaults = {
    random = 0.0
    core   = None
    points = []
    rectangles = []
  }

let to_string (sp:t) =
  let randomText =
    if sp.random <> defaults.random
      then sprintf "random = %1.3f" sp.random
      else ""
  let coreText = match sp.core with None -> "" | Some core -> core.to_string()
  let pointsText = 
    if sp.points <> defaults.points
      then "points = [" + (sp.points |> List.map (fun p -> p.to_string()) |> String.concat "; ") + "]"
      else ""  
  let rectanglesText = 
    if sp.rectangles <> defaults.rectangles
    then "rectangles = [" + (sp.rectangles |> List.map (fun r -> r.to_string()) |> String.concat "; ") + "]"
    else ""

  "{ "+ 
  ([ randomText
     coreText
     pointsText 
     rectanglesText ]
   |> List.filter (fun s -> s <> "")
   |> String.concat "; ")
  + " }"


let CONSTANT = "constant" 
let INIT     = "init"
let SPATIAL  = "spatial"

let parser = 
  let pcore = 
    Parser.record default_core [
      "inner", Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with inner = d }
      "outer", Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with outer = d }
      "width", Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with width = d }
    ]
  
  let ppoint = 
    Parser.record default_point [ 
      "x",      Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with x      = d }
      "y",      Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with y      = d }
      "width",  Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with width  = d }
      "value",  Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with value  = d }
    ]
  
  let prectangle = 
    Parser.record default_rectangle [ 
      "xmin",   Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with xmin   = d }
      "xmax",   Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with xmax   = d }
      "ymin",   Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with ymin   = d }
      "ymax",   Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with ymax   = d }
      "value",  Parser.pfloat .>> Parser.spaces |>> fun d s -> { s with value  = d }
    ]
  
  Parser.record defaults [
    "random",       Parser.pfloat             .>> Parser.spaces |>> fun d s -> { s with random = d }
    "centralcore",  pcore                     .>> Parser.spaces |>> fun d s -> { s with core   = Some d }
    "points",       Parser.list_of ppoint     .>> Parser.spaces |>> fun d s -> { s with points = d }
    "rectangles",   Parser.list_of prectangle .>> Parser.spaces |>> fun d s -> { s with rectangles = d }
  ]