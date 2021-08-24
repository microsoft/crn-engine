import * as MSAGL from "../../../MSAGL_JS/Scripts/msagl";
import "./styles.css";
import "../samples.css";

$(window).on('load', function () {
    var graphContainer = document.getElementById("graphContainer");
    var graphView = document.getElementById("graphView");
    var graphControl = new MSAGL.IDDSVGGraph(graphView);
    var graph: MSAGL.GGraph = null;

    var showButton = <HTMLButtonElement>document.getElementById("showButton");
    var hideButton = <HTMLButtonElement>document.getElementById("hideButton");
    var renderButton = <HTMLButtonElement>document.getElementById("renderButton");

    function showButtonClicked() {
        graphContainer.style.display = "block";
    }

    function hideButtonClicked() {
        graphContainer.style.display = "none";
    }

    function renderButtonClicked() {
        graph = new MSAGL.GGraph;
        graphControl.setGraph(graph);
        graph.addNode(new MSAGL.GNode({ id: "node1", label: "Node 1" }));
        graph.addNode(new MSAGL.GNode({ id: "node2", label: "Node 2" }));
        graph.addNode(new MSAGL.GNode({ id: "node3", label: "Node 3" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge12", source: "node1", target: "node2" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge13", source: "node1", target: "node3" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge23", source: "node2", target: "node3" }));
        graph.createNodeBoundariesFromSVG();
        graph.layoutCallbacks.add(() => graphControl.drawGraph());
        graph.beginLayoutGraph();
    }

    showButton.onclick = showButtonClicked;
    hideButton.onclick = hideButtonClicked;
    renderButton.onclick = renderButtonClicked;
});