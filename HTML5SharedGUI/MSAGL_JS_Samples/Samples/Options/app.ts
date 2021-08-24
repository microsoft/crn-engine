import * as MSAGL from "../../../MSAGL_JS/Scripts/msagl";
import "./styles.css";
import "../samples.css";
import * as sampleGraph1 from 'raw-loader!./samplegraph1.txt';
import * as sampleGraph2 from 'raw-loader!./samplegraph2.txt';

$(window).on('load', function () {
    var graphView = document.getElementById("graphView");
    var graphControl = new MSAGL.IDDSVGGraph(graphView);
    var graph: MSAGL.GGraph = null;

    var layeredLayoutCheckBox = <HTMLInputElement>document.getElementById("layeredLayoutCheckBox");
    var horizontalLayoutCheckBox = <HTMLInputElement>document.getElementById("horizontalLayoutCheckBox");
    var aspectRatioTextBox = <HTMLInputElement>document.getElementById("aspectRatioTextBox");
    var edgeRoutingSelect = <HTMLSelectElement>document.getElementById("edgeRoutingSelect");
    var workingIndicator = <HTMLDivElement>document.getElementById("workingIndicator");

    var jsonGraph = "";

    function showWorking(show: boolean) {
        if (show) {
            workingIndicator.style.display = "inherit";
            graphView.style.display = "none";
        }
        else {
            workingIndicator.style.display = "none";
            graphView.style.display = "";
        }
    }

    function stop() {
        if (graph != null && graph.working)
            graph.stopLayoutGraph();
        showWorking(false);
    }

    function copySettingsToGraph() {
        graph.settings.layout = layeredLayoutCheckBox.checked ? MSAGL.GSettings.sugiyamaLayout : MSAGL.GSettings.mdsLayout;
        graph.settings.transformation = horizontalLayoutCheckBox.checked ? MSAGL.GPlaneTransformation.ninetyDegreesTransformation : MSAGL.GPlaneTransformation.defaultTransformation;
        if (aspectRatioTextBox.value != null && aspectRatioTextBox.value != "")
            graph.settings.aspectRatio = parseFloat(aspectRatioTextBox.value);
        graph.settings.routing = edgeRoutingSelect.value;
    }

    function layoutClicked() {
        stop();
        copySettingsToGraph();
        graph.createNodeBoundariesForSVGInContainer(graphView);
        showWorking(true);
        graph.beginLayoutGraph();
    }

    function routeClicked() {
        stop();
        copySettingsToGraph();
        showWorking(true);
        graph.beginEdgeRouting();
    }

    function stopClicked() {
        stop();
    }

    function loadGraph(json: string) {
        jsonGraph = json;
        graph = MSAGL.GGraph.ofJSON(jsonGraph);
        graphControl.setGraph(graph);
        graphControl.allowEditing = false;
        graph.workStoppedCallbacks.add(() => {
            showWorking(false);
            graphControl.drawGraph();
        });
        layoutClicked();
    }

    function loadGraph1() {
        loadGraph(sampleGraph1);
    }

    function loadGraph2() {
        loadGraph(sampleGraph2);
    }

    document.getElementById("layoutButton").onclick = layoutClicked;
    document.getElementById("routeButton").onclick = routeClicked;
    document.getElementById("stopButton").onclick = stopClicked;
    document.getElementById("loadGraph1Button").onclick = loadGraph1;
    document.getElementById("loadGraph2Button").onclick = loadGraph2;

    loadGraph1();
});