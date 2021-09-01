// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace ReactIDD

(*
This is the Fable wrapper for ReactIDD. The correct way to use this is to create Plot objects, then use Helpers.convert to make an IDDProps object, and pass that to ReactIDD. The F# declaration for ReactIDD is:

let inline iddChart (props : IDDProps list) (elems : ReactElement list) : ReactElement =
    ofImport "Chart" "reactidd" (keyValueList CaseRules.LowerFirst props) elems

This is not declared in this file, in order to avoid acquiring a dependency on Fable.Import.React. It should be declared in the client, but this file can also be used in a server.
*)

type MarkerShape = Box | Circle | Cross | Diamond | Triangle
type Quantiles = {
    median: float[];
    lower68: float[];
    upper68: float[];
    lower95: float[];
    upper95: float[];
}

type Polyline = {
    id: string;
    x: float[];
    y: float[];
    stroke: string;
    thickness: int;
}

type Markers = {
    id: string;
    x: float[];
    y: float[];
    size: float;
    color: string;
    shape: MarkerShape
}

type SizedMarkers = {
    id: string;
    x: float[];
    y: float[];
    sizes: float[];
    color: string;
    shape: MarkerShape
}

type Bars = {
    id: string;
    x: float[];
    y: float[];
    barWidth: float;
    color: string;
}

type BoxAndWhiskers = {
    id: string;
    x: float[];
    q: Quantiles;
    size: float;
    color: string;
}

type Heatmap = {
    id: string;
    x: float[];
    y: float[];
    values: float[][];
    colorPalette: string;
}

/// This (and the various union case types) is the F#-style type that can be used to manipulate plots.
type Plot = Polyline of Polyline | Markers of Markers | SizedMarkers of SizedMarkers | Bars of Bars | BoxAndWhiskers of BoxAndWhiskers | Heatmap of Heatmap

/// This is the type that can be sent to ReactIDD. It closely matches the type of objects that IDD understands, except for ensuring that any given field is of one specific type only (which is not the case in JS). The user doesn't need to worry about this. The intended usage pattern is for the user to work on Plot instances, and then call Helpers.convert (see below) to get IDDPlot instances.
type IDDPlot = {
    kind: string;
    id: string;
    x: float[] option;
    y: float[] option;
    stroke: string option;
    thickness: int option;
    size: float option;
    sizes: float[] option;
    color: string option;
    shape: string option;
    barWidth: float option;
    q: Quantiles option;
    values: float[][] option;
    colorPalette: string option;
}

type AxisPosition = Bottom | Left | Top | Right
// For now, only numeric axes are supported
type AxisKind = Numeric

type Axis = {
    position: AxisPosition;
    kind: AxisKind
}

type IDDAxis = {
    position: string;
    kind: string
}

/// This is the type of the props for the ReactIDD component. Pass this to ReactIDD to show a chart on screen.
type IDDProps =
| Width of float
| Height of float
// If None, the default axes are used.
| Axes of IDDAxis[] option
| Plots of IDDPlot[]

/// This module contains a few functions that make it easy to deal with the structural differences between IDD-like plot objects and F#-like plot objects.
module Helpers =
    /// Use this to get the ID of a Plot.
    let getid plot =
        match plot with
        | Polyline polyline -> polyline.id
        | Markers markers -> markers.id
        | SizedMarkers markers -> markers.id
        | Bars bars -> bars.id
        | BoxAndWhiskers boxAndWhiskers -> boxAndWhiskers.id
        | Heatmap heatmap -> heatmap.id

    let private blankPlot = {
        kind = "";
        id = "";
        x = None;
        y = None;
        stroke = None;
        thickness = None;
        size = None;
        sizes = None;
        color = None;
        shape = None;
        barWidth = None;
        q = None;
        values = None;
        colorPalette = None;
    }

    let private convertPolyline (polyline:Polyline) = {
        blankPlot with
            kind = "polyline";
            id = polyline.id;
            x = Some polyline.x;
            y = Some polyline.y;
            stroke = Some polyline.stroke;
            thickness = Some polyline.thickness
    }

    let private convertMarkers (markers:Markers) = {
        blankPlot with
            kind = "markers";
            id = markers.id;
            x = Some markers.x;
            y = Some markers.y;
            size = Some markers.size;
            color = Some markers.color;
            shape = Some (markers.shape.ToString().ToLower())
    }

    let private convertSizedMarkers (sizedMarkers:SizedMarkers) = {
        blankPlot with
            kind = "sizedmarkers";
            id = sizedMarkers.id;
            x = Some sizedMarkers.x;
            y = Some sizedMarkers.y;
            sizes = Some sizedMarkers.sizes;
            color = Some sizedMarkers.color;
            shape = Some (sizedMarkers.shape.ToString().ToLower())
    }

    let private convertBars (bars:Bars) = {
        blankPlot with
            kind = "bars";
            id = bars.id;
            x = Some bars.x;
            y = Some bars.y;
            barWidth = Some bars.barWidth;
            color = Some bars.color;
    }

    let private convertBoxAndWhiskers (boxAndWhiskers:BoxAndWhiskers) = {
        blankPlot with
            kind = "boxandwhiskers";
            id = boxAndWhiskers.id;
            x = Some boxAndWhiskers.x;
            q = Some boxAndWhiskers.q;
            size = Some boxAndWhiskers.size;
            color = Some boxAndWhiskers.color;
    }

    let private convertHeatmap (heatmap:Heatmap) = {
        blankPlot with
            kind = "heatmap";
            id = heatmap.id;
            x = Some heatmap.x;
            y = Some heatmap.y;
            values = Some heatmap.values;
            colorPalette = Some heatmap.colorPalette;
    }

    /// Use this to convert a Plot to an IDDPlot. The usage pattern is that you create and manipulate Plot instances, and then use this function to convert them to IDDPlot instances right before creating a Chart.
    let convertPlot (plot:Plot) =
        match plot with
        | Polyline polyline -> convertPolyline polyline
        | Markers markers -> convertMarkers markers
        | SizedMarkers sizedMarkers -> convertSizedMarkers sizedMarkers
        | Bars bars -> convertBars bars
        | BoxAndWhiskers boxAndWhiskers -> convertBoxAndWhiskers boxAndWhiskers
        | Heatmap heatmap -> convertHeatmap heatmap
    
    let convertAxis (axis:Axis) = {
        position = match axis.position with
                   | Bottom -> "bottom"
                   | Top -> "top"
                   | Right -> "right"
                   | Left -> "left"
        kind = match axis.kind with
               | Numeric -> "numeric"
    }

/// This type is strictly for the Fable sample project.
type ParseResponse = Plot of Plot | Delete of string

/// This type is strictly for the Fable sample project.
type IPlotParseApi = {
    parse : string -> Async<ParseResponse>
}
