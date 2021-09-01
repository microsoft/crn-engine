// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as MSAGL from "../../../MSAGL_JS/Scripts/msagl";
import "./styles.css";
import "../samples.css";

$(window).on('load', function () {

    var graphView = document.getElementById("graphView");
    var graphControl = new MSAGL.IDDSVGGraph(graphView);
    var graph: MSAGL.GGraph = null;

    var nodeCountControl = <HTMLInputElement>document.getElementById("nodeCount");
    var edgeCountControl = <HTMLInputElement>document.getElementById("edgeCount");
    var startButton = <HTMLButtonElement>document.getElementById("startButton");
    var stopButton = <HTMLButtonElement>document.getElementById("stopButton");
    var working = document.getElementById("working");
    var elapsed = document.getElementById("elapsed");
    var edgeRoutingSelect = <HTMLSelectElement>document.getElementById("edgeRoutingSelect");

    function setGUIToRunning() {
        working.style.display = "inline";
        startButton.disabled = true;
        stopButton.disabled = false;
        elapsed.textContent = "";
    }

    function setGUIToNotRunning() {
        working.style.display = "none";
        startButton.disabled = false;
        stopButton.disabled = true;
    }

    function run(nodeCount: number, edgeCount: number) {
        graph = new MSAGL.GGraph();
        graphControl.setGraph(graph);
        graph.settings.aspectRatio = graphView.offsetWidth / graphView.offsetHeight;
        graph.settings.routing = edgeRoutingSelect.value;
        for (var i = 1; i <= nodeCount; i++)
            graph.addNode(new MSAGL.GNode({ id: "node" + i, label: "Node " + i }));
        var randomness = new Uint32Array(edgeCount * 2);
        if (typeof (crypto) != "undefined")
            crypto.getRandomValues(randomness);
        else
            (<any>window).msCrypto.getRandomValues(randomness);
        for (var i = 1; i <= edgeCount; i++)
            graph.addEdge(new MSAGL.GEdge({
                id: "edge" + i, source: "node" + (randomness[(i - 1) * 2] % nodeCount + 1),
                target: "node" + (randomness[(i - 1) * 2 + 1] % nodeCount + 1)
            }));

        var startTime = new Date();
        graph.createNodeBoundariesForSVGInContainer(graphView);
        graph.layoutCallbacks.add(() => {
            graphControl.drawGraph();
            setGUIToNotRunning();
            var endTime = new Date();
            var diff = endTime.getTime() - startTime.getTime();
            elapsed.textContent = diff + " msecs";
        });
        graph.beginLayoutGraph();
    }

    function startButtonClicked() {
        var nodeCount = parseInt(nodeCountControl.value);
        var edgeCount = parseInt(edgeCountControl.value);
        setGUIToRunning();
        run(nodeCount, edgeCount);
    }

    function stopButtonClicked() {
        graph.stopLayoutGraph();
        setGUIToNotRunning();
    }

    setGUIToNotRunning();

    startButton.onclick = startButtonClicked;
    stopButton.onclick = stopButtonClicked;
});
