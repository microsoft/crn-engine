// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

(* A data structure that represents a simple graph, in a form that's reasonably easy to manipulate, but also contains some simple rendering hints. Can be used as a rendering-agnostic intermediate for graph representations. This is supposed to be easy to convert into a MSAGL graph, so it helps if we use the same names as MSAGL shapes, styles, colors etc. *)
[<JavaScript>]
module Microsoft.Research.CRNEngine.Graph

type LineStyle = Dashed
type Shape = Ellipse | Box

type Node = { id: string; label: string option; stroke: string option; fill: string option; shape: Shape option }
type Edge = { source: string; destination: string; label: string option; style: LineStyle option; stroke: string option }

type Graph =
    {
        nodes: Node list
        edges: Edge list
    }
