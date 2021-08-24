// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

open Operators
open System.Diagnostics
open System.Diagnostics.Tracing

// Note (FP): some of these fields would be better as F# option fields. They were not option fields when initially designed, in order to avoid serialisation issues due to ambiguity in the JSON representation of option fields. After serialisation ambiguity has been resolved, they may be replaced with option fields.
[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Plot_settings<'e> = 
  {
    /// The label of the horizontal axis. If empty, it will be determined automatically based on the type of simulation.
    x_label: string;
    /// The label of the vertical axis. If empty, it will be determined automatically based on the type of simulation.
    y_label: string;
    /// The title of the chart.
    title: string;
    /// The font size for labels.
    label_font_size: int;
    /// The font size for axis ticks.
    tick_font_size: int;
    /// A set of positions where ticks will be placed on the horizontal axis. If empty, ticks will be determined automatically.
    x_ticks: float list;
    /// A set of positions where ticks will be placed on the vertical axis. If empty, ticks will be determined automatically.
    y_ticks: float list;
    /// The initial left coordinate of the viewing window. If none, it will be determined automatically.
    x_min: float option;
    /// The initial right coordinate of the viewing window. If none, it will be determined automatically.
    x_max: float option;
    /// The initial bottom coordinate of the viewing window. If none, it will be determined automatically.
    y_min: float option;
    /// The initial top coordinate of the viewing window. If none, it will be determined automatically.
    y_max: float option;
    /// A set of X coordinates which will be used to draw vertical lines on the plot.
    v_boundaries: 'e list;
    /// A set of Y coordinates which will be usde to draw horizontal lines on the plot.
    h_boundaries: 'e list;
  }
  member s.update_empty_labels (x_label:string) (y_label:string) =
    let s:Plot_settings<'e> = if s.x_label = "" then {s with x_label = x_label} else s
    if s.y_label = "" then {s with y_label = y_label} else s
  static member defaults = {
    x_label = ""
    y_label = ""
    title = ""
    label_font_size = 16
    tick_font_size = 16
    x_ticks = []
    y_ticks = []
    x_min = None
    x_max = None
    y_min = None
    y_max = None
    v_boundaries = []
    h_boundaries = []
  }
  static member parse_defaults (defaults:Plot_settings<'e>) (pe:Parser.t<'e>) =
    let parseAnyChar = Parser.satisfy (fun c -> c <> '\"')
    Parser.record defaults [
      "x_label", Parser.between (Parser.pchar '\"') (Parser.pchar '\"') (Parser.manyChars parseAnyChar) |>> fun v (s:Plot_settings<'e>) -> { s with x_label = v }
      "y_label", Parser.between (Parser.pchar '\"') (Parser.pchar '\"') (Parser.manyChars parseAnyChar) |>> fun v (s:Plot_settings<'e>) -> { s with y_label = v }
      "title", Parser.between (Parser.pchar '\"') (Parser.pchar '\"') (Parser.manyChars parseAnyChar) |>> fun v (s:Plot_settings<'e>) -> { s with title = v }
      "label_font_size", Parser.pint32 |>> fun v (s:Plot_settings<'e>) -> { s with label_font_size = v }
      "tick_font_size", Parser.pint32 |>> fun v (s:Plot_settings<'e>) -> { s with tick_font_size = v }
      "x_ticks", Parser.list_of Parser.pfloat |>> fun v (s:Plot_settings<'e>) -> { s with x_ticks = v }
      "y_ticks", Parser.list_of Parser.pfloat |>> fun v (s:Plot_settings<'e>) -> { s with y_ticks = v }
      "x_min", Parser.pfloat |>> fun v (s:Plot_settings<'e>) -> { s with x_min = Some v }
      "x_max", Parser.pfloat |>> fun v (s:Plot_settings<'e>) -> { s with x_max = Some v }
      "y_min", Parser.pfloat |>> fun v (s:Plot_settings<'e>) -> { s with y_min = Some v }
      "y_max", Parser.pfloat |>> fun v (s:Plot_settings<'e>) -> { s with y_max = Some v }
      "v_boundaries", Parser.list_of pe |>> fun v (s:Plot_settings<'e>) -> { s with v_boundaries = v }
      "h_boundaries", Parser.list_of pe |>> fun v (s:Plot_settings<'e>) -> { s with h_boundaries = v }
    ]
  static member parse pe = Plot_settings<'e>.parse_defaults (Plot_settings<'e>.defaults) pe
  member s.map (f:'e -> 'e2) = {
    x_label = s.x_label
    y_label = s.y_label
    title = s.title
    label_font_size = s.label_font_size
    tick_font_size = s.tick_font_size
    x_ticks = s.x_ticks
    y_ticks = s.y_ticks
    x_min = s.x_min
    x_max = s.x_max
    y_min = s.y_min
    y_max = s.y_max
    v_boundaries = List.map f s.v_boundaries
    h_boundaries = List.map f s.h_boundaries
  }