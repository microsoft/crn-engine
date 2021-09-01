// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as MSAGL from "../../../MSAGL_JS/Scripts/msagl";
import "./styles.css";
import "../samples.css";

$(window).on('load', function () {
    var graphView = document.getElementById("graphView");
    var graphControl = new MSAGL.IDDSVGGraph(graphView);
    var graph: MSAGL.GGraph = null;

    var statusText = document.getElementById("status");
    var mousePointText = document.getElementById("mousePoint");
    var objectUnderCursorText = document.getElementById("objectUnderCursor");
    var dragObjectText = document.getElementById("dragObject");

    function appendStatus(text: string) {
        var p = document.createElement("p");
        p.appendChild(document.createTextNode(text));
        statusText.appendChild(p);
    }

    function makeInitialGraph() {
        var graph = new MSAGL.GGraph();
        graphControl.setGraph(graph);
        graphControl.onNodeClick = (n => appendStatus("Clicked " + n.id));
        graphControl.onEdgeClick = (e => appendStatus("Clicked " + e.id));
        graph.settings.aspectRatio = graphView.offsetWidth / graphView.offsetHeight;

        graph.addNode(new MSAGL.GNode({ id: "node1", label: "Node 1", fill: "white" }));
        graph.addNode(new MSAGL.GNode({ id: "node2", label: "Node 2", fill: "white" }));
        graph.addNode(new MSAGL.GNode({ id: "node3", label: "Node 3", fill: "white" }));

        graph.addEdge(new MSAGL.GEdge({ id: "edge13", label: "Edge 1-3", source: "node1", target: "node3" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge31", label: "Edge 3-1", source: "node3", target: "node1" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge23", label: "Edge 2-3", source: "node2", target: "node3" }));

        graph.createNodeBoundariesForSVGInContainer(graphView);
        graph.layoutCallbacks.add(() => { graphControl.drawGraph(); updateTexts(); setInterval(updateTexts, 100); });
        graph.beginLayoutGraph();
    }

    makeInitialGraph();

    function updateTexts() {
        mousePointText.textContent = JSON.stringify(graphControl.getMousePoint());
        objectUnderCursorText.textContent = JSON.stringify(graphControl.getObjectUnderMouseCursor());
        dragObjectText.textContent = JSON.stringify(graphControl.getDragObject());
    }
});
