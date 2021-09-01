// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace ReactMSAGL

(*
This is the Fable wrapper for ReactMSAGL. The correct way to use this is to create a Graph object, then put it in a MSAGLProps object, and pass that to ReactMSAGL. The F# declaration for ReactMSAGL is:

let inline msaglGraph (props : MSAGLProps list) (elems : ReactElement list) : ReactElement =
    ofImport "Graph" "reactmsagl" (keyValueList CaseRules.LowerFirst props) elems

This is not declared in this file, in order to avoid acquiring a dependency on Fable.Import.React. It should be declared in the client, but this file can also be used in a server.
*)

type Point =
    {
        x: float;
        y: float;
    }
    static member Origin = {x=0.0;y=0.0}

type Rect =
    {
        x: float;
        y: float;
        width: float;
        height: float;
    }
    
type Label =
    {
        bounds: Rect option;
        content: string;
        fill: string option;
    }
    static member OfString(content:string) = {bounds=None;content=content;fill=None}

type Node =
    {
        id: string;
        label: Label option;
        labelMargin: float option;
        thickness: float option;
        fill: string option;
        stroke: string option;
    }
    static member OfID(id:string) = {id=id;label=None;labelMargin=None;thickness=None;fill=None;stroke=None}
    static member OfIDLabel(id:string,label:string) = { Node.OfID(id) with label = Label.OfString(label) |> Some }

type ArrowHead =
    {
        closed: bool;
        fill: bool;
        // standard, tee or diamond
        style: string;
    }
    static member Standard = {closed=true;fill=false;style="standard"}

type Edge =
    {
        id: string;
        source: string;
        target: string;
        label: Label option;
        arrowHeadAtTarget: ArrowHead option;
        arrowHeadAtSource: ArrowHead option;
        thickness: float option;
        stroke: string option;
    }
    static member OfIDSourceTarget(id:string,source:string,target:string) = {id=id;source=source;target=target;label=None;arrowHeadAtTarget=Some ArrowHead.Standard;arrowHeadAtSource=None;thickness=None;stroke=None}
    static member OfIDSourceTargetLabel(id:string,source:string,target:string,label:string) = {Edge.OfIDSourceTarget(id,source,target) with label = Label.OfString(label) |> Some}

type Graph =
    {
        nodes: Node[];
        edges: Edge[];
    }

type MSAGLProps =
| Width of float
| Height of float
| Graph of Graph
