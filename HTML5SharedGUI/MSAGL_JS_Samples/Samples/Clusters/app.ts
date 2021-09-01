// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as MSAGL from "../../../MSAGL_JS/Scripts/msagl";
import "./styles.css";
import "../samples.css";

$(window).on('load', function () {
    var graphView = document.getElementById("graphView");
    var graphControl = new MSAGL.IDDSVGGraph(graphView);
    graphControl.allowEditing = false;
    var graph: MSAGL.GGraph = null;

    function generateGraph() {
        graph = new MSAGL.GGraph();
        graphControl.setGraph(graph);
        // Note that I'm setting a left-margin, but the label will be drawn on top, because I'm also setting a planar rotation.
        graph.addNode(new MSAGL.GCluster({
            id: "clusterA", label: "Cluster A", children: [
                new MSAGL.GNode({ id: "node1", label: "Node 1" }),
                new MSAGL.GNode({ id: "node2", label: "Node 2" }),
                new MSAGL.GCluster({
                    id: "clusterB", label: "Cluster B", children: [
                        new MSAGL.GNode({ id: "node3", label: "Node Three (long name)", shape: "roundedrect" }),
                        new MSAGL.GNode({ id: "node4", label: "Node 4" }),
                    ],
                    margin: { left: 16 }
                })
            ],
            margin: { left: 16 }
        }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge12", source: "node1", target: "node2" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge2B", source: "node2", target: "clusterB" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge34", source: "node3", target: "node4" }));
        graph.addEdge(new MSAGL.GEdge({ id: "edge13", source: "node1", target: "node3" }));
        graph.settings.transformation = MSAGL.GPlaneTransformation.ninetyDegreesTransformation;
        graph.createNodeBoundariesForSVGInContainer(graphView, MSAGL.GGraph.defaultStyle);

        graph.layoutCallbacks.add(() => graphControl.drawGraph());
        graph.beginLayoutGraph();
    }

    generateGraph();
});
