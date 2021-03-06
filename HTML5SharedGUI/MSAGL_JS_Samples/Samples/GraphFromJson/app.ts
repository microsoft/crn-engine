// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as MSAGL from "../../../MSAGL_JS/Scripts/msagl";
import "./styles.css";
import "../samples.css";
import * as sample from "raw-loader!./samplegraph.txt";

$(window).on('load', function () {

    var graphView = document.getElementById("graphView");
    var graphControl = new MSAGL.SVGGraph(graphView);
    var graph: MSAGL.GGraph = null;

    function draw() {
        var jsonOutputArea = document.getElementById("jsonOutput");
        var graphText = graph.getJSON();
        jsonOutputArea.textContent = graphText;
        graphControl.drawGraph();
    }

    function parse() {
        var jsonTextArea = <HTMLTextAreaElement>document.getElementById("jsonInput");
        var jsonText = jsonTextArea.value;
        graph = MSAGL.GGraph.ofJSON(jsonText);
        graphControl.setGraph(graph);
        graph.createNodeBoundariesForSVGInContainer(graphView);
        graph.layoutCallbacks.add(draw);
        graph.edgeRoutingCallbacks.add(draw);
    }

    function loadFromOutput() {
        var jsonOutputArea = <HTMLTextAreaElement>document.getElementById("jsonOutput");
        var jsonText = jsonOutputArea.value;
        graph = MSAGL.GGraph.ofJSON(jsonText);
        graphControl.setGraph(graph);
    }

    function parseJsonClicked() {
        parse();
        graph.beginLayoutGraph();
    }

    function loadClicked() {
        parse();
        draw();
    }

    function routeEdgesClicked() {
        graph.beginEdgeRouting();
    }

    function renderClicked() {
        loadFromOutput();
        graphControl.drawGraph();
    }

    document.getElementById("parseButton").onclick = parseJsonClicked;
    document.getElementById("loadButton").onclick = loadClicked;
    document.getElementById("routeButton").onclick = routeEdgesClicked;
    document.getElementById("renderButton").onclick = renderClicked;

    document.getElementById("jsonInput").textContent = sample;
});
