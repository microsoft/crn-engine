// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as MSAGL from "../../../MSAGL_JS/Scripts/msagl";
import * as ko from "knockout";
import "./styles.css";
import "../samples.css";

$(window).on('load', function () {
    var graphView = document.getElementById("graphView");
    var graphControl = new MSAGL.IDDSVGGraph(graphView);
    var graph: MSAGL.GGraph = null;

    var working = document.getElementById("working");
    var reinitButton = document.getElementById("reinitButton");

    function setGUIToRunning() {
        working.style.display = "inline";
    }

    function setGUIToNotRunning() {
        working.style.display = "none";
    }

    class ZoomViewModel {
        public zoomLevel: KnockoutObservable<string>;

        /** If swithced to false, altering the ko value zoomLevel does not pass the assigned value down to iddSvgGraph  */
        private koUpdatesScale = true;

        public constructor(svgGraph: MSAGL.IDDSVGGraph) {
            const zoomStep: number = 1.1;

            var that = this;

            this.zoomLevel = ko.observable("0");
            this.zoomLevel.subscribe(function (newVal) {
                if (that.koUpdatesScale) {
                    var fValue: number = parseFloat(newVal);
                    if (isNaN(fValue))
                        return;
                    var v = Math.exp(Math.log(zoomStep) * fValue);
                    console.log('pushing to iddsvggrah zoom level change ' + fValue + '(' + v.toFixed(3) + ')');
                    svgGraph.setZoomLevel(v);
                }
            });

            svgGraph.zoomLevelChangeCallback = function (reportedZoomLevel) {
                var v = (Math.log(reportedZoomLevel) / Math.log(zoomStep)).toFixed(0);

                // This brakes the updates loop
                that.koUpdatesScale = false;
                console.log('reported zoom level change from ' + that.zoomLevel() + ' to ' + v + ' (' + reportedZoomLevel.toFixed(3) + ')');
                that.zoomLevel(v.toString());
                that.koUpdatesScale = true;
            }
        }
    }

    var vm: ZoomViewModel = undefined;

    function run(nodeCount: number, edgeCount: number) {
        graph = new MSAGL.GGraph();
        graphControl.setGraph(graph);
        vm = new ZoomViewModel(graphControl);
        ko.applyBindings(vm);
        console.log('VM iunitialized');
        graph.settings.aspectRatio = graphView.offsetWidth / graphView.offsetHeight;
        graph.settings.routing = 'sugiyamasplines';
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
        });
        graph.beginLayoutGraph();
    }

    reinitButton.onclick = function () {
        graphControl.resetZoomLevel();
        console.log('zoom level is reset');
    }


    setGUIToRunning();
    run(100, 100);
});
