// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Implements an inference graph viewer that has a MSAGL/JS graph plus some controls to operate on it.

import "./../../GenericComponents/Styles/shared.css";
import "./../Styles/crn.css";
import * as $ from 'jquery';
import * as CRNvm from './crnVM';
import * as MSAGL from './../../../HTML5SharedGUI/MSAGL_JS/Scripts/msagl';
import * as Utils from './Utils';
import * as ko from 'knockout';
import "../../GenericComponents/Scripts/Dropdown";
import ZoomSlider from './../../GenericComponents/Scripts/ZoomSlider';

interface InferenceNode {
    model: CRNvm.Model;
}

class SystemNode extends MSAGL.GNode implements InferenceNode {
    constructor(public model: CRNvm.Model, public system: CRNvm.CRN) {
        super({ id: model.NodeID() + "." + system.name(), label: system.name(), shape: 'roundedrect' });
    }
}

class ModelNode extends MSAGL.GCluster implements InferenceNode {
    constructor(public horizontal: boolean, public model: CRNvm.Model, private systemNodes: SystemNode[]) {
        super({ id: model.NodeID(), label: model.NodeID(), shape: 'roundedrect', margin: { top: horizontal ? 0 : 16, left: horizontal ? 16 : 0 } });
        for (let i in systemNodes)
            this.addChild(systemNodes[i]);
    }
}

class EmptyModelNode extends MSAGL.GNode implements InferenceNode {
    constructor(public horizontal: boolean, public model: CRNvm.Model) {
        super({ id: model.NodeID(), label: model.NodeID(), shape: 'roundedrect', margin: { top: horizontal ? 0 : 16, left: horizontal ? 16 : 0 } });
    }
}

export class InferenceGraphViewerVM {
    constructor(public ig: CRNvm.InferenceGraph) { }

    public zoom: ZoomSlider = new ZoomSlider();

    // UI states.
    public horizontalLayout = ko.observable<boolean>(true);
    public aspectRatio = ko.observable<boolean>(false);
    public routing = ko.observable<string>(MSAGL.GSettings.sugiyamaSplinesRouting);

    public saveAsSVG = () => { };
}

export class InferenceGraphViewer {
    // Instantiate this with a div that will contain the graph. The div needs to have an ID; I'll use it later. I should probably come up with a design that doesn't require this.
    constructor(private container: HTMLElement) { }

    // The MSAGL graph.
    public iddgraph: MSAGL.IDDSVGGraph;

    private zoom_noRecurse = false;

    private vm: InferenceGraphViewerVM;
    public setVM(vm: InferenceGraphViewerVM) {
        var that = this;
        if (this.vm !== vm) {
            if (this.vm != null) {
                console.log("Warning: VM changed in InferenceGraphViewer. This will leak KO subscriptions. If this is happening for a good reason, dispose logic needs to be implemented.");
            }
            this.vm = vm;

            vm.saveAsSVG = () => that.iddgraph.saveAsSVG();

            this.iddgraph = new MSAGL.IDDSVGGraph(this.container);

            vm.zoom.Value.subscribe(function (p) {
                if (!that.zoom_noRecurse)
                    that.iddgraph.setZoomLevel(p);
            });
            this.iddgraph.zoomLevelChangeCallback = function (v) {
                // Prevent recursion.
                that.zoom_noRecurse = true;
                vm.zoom.Value(v);
                that.zoom_noRecurse = false;
            }

            this.iddgraph.onNodeClick = n => that.onNodeClick(n);
            this.iddgraph.allowEditing = false;
            // Subscribe to the UI toggles that can cause a re-render.
            this.vm.horizontalLayout.subscribe(val => this.prepareUpdate());
            this.vm.aspectRatio.subscribe(val => this.prepareUpdate());
            this.vm.routing.subscribe(val => this.prepareUpdate());
            this.vm.ig.Nodes.subscribe(val => this.prepareUpdate());
            this.vm.ig.Edges.subscribe(val => this.prepareUpdate());
            this.vm.ig.SelectedCRN.subscribe(v => that.resetSelectionState());
            this.prepareUpdate();
        }
    }

    private resetSelectionState() {
        let graph = this.iddgraph.getGraph();
        for (let node of graph.nodes) {
            var modelNode = <ModelNode | EmptyModelNode>node;
            var modelSelected = this.vm.ig.SelectedCRN() == modelNode.model.Top();
            modelNode.stroke = modelSelected ? "blue" : "black";
            modelNode.thickness = modelSelected ? 2 : 1;
            if (modelNode.isCluster())
                for (let subnode of (<ModelNode>modelNode).children) {
                    var systemNode = <SystemNode>subnode;
                    var systemSelected = this.vm.ig.SelectedCRN() == systemNode.system;
                    systemNode.stroke = systemSelected ? "blue" : "black";
                    systemNode.thickness = systemSelected ? 2 : 1;
                }
        }
        this.iddgraph.drawGraph();
    }

    private onNodeClick(n: MSAGL.GNode): void {
        var node = <InferenceNode><any>n;

        // Switch to the clicked model.
        var model: CRNvm.Model = null;
        for (let m of this.vm.ig.Nodes())
            if (m.NodeID() == node.model.NodeID())
                model = m;
        if (model != null) {
            var crn: CRNvm.CRN = model.Top();
            var systemNode = <SystemNode>node;
            if (systemNode.system != null)
                for (let c of model.AllCRNs())
                    if (c.name() == systemNode.system.name())
                        crn = c;

            this.vm.ig.SelectedNode(model);
            model.SelectedCRN(crn);
        }
    }

    // Requests an update. This can be called multiple times and only one update will be performed.
    private prepareUpdate() {
        var that = this;
        if (this.timeoutKey != null)
            clearTimeout(this.timeoutKey);
        this.timeoutKey = setTimeout(function () { that.timeoutKey = null; that.render(); });
    }
    // This is the ID of the timeout that's currently being used to delay the call to render().
    private timeoutKey: number;

    // This function turns the InferenceGraph into a GGraph.
    private makeGraph(): MSAGL.GGraph {
        var ret: MSAGL.GGraph = new MSAGL.GGraph();

        // Create nodes. Models translate to clusters, systems translate to regular nodes.
        for (let model of this.vm.ig.Nodes()) {
            let systemNodes: SystemNode[] = [];
            for (let system of model.Systems()) {
                let systemNode = new SystemNode(model, system);
                if (this.vm.ig.SelectedCRN() == system) {
                    systemNode.thickness = 2;
                    systemNode.stroke = "blue";
                }
                systemNodes.push(systemNode);
            }
            let modelNode = systemNodes.length == 0 ? new EmptyModelNode(this.vm.horizontalLayout(), model) : new ModelNode(this.vm.horizontalLayout(), model, systemNodes);
            if (this.vm.ig.SelectedCRN() == model.Top()) {
                modelNode.thickness = 2;
                modelNode.stroke = "blue";
            }
            ret.addNode(modelNode);
        }
        // Create edges.
        for (let sourceName in this.vm.ig.Edges()) {
            // Edges are in the VM in the form of an array for each node, with the node being the source.
            let edges = this.vm.ig.Edges()[sourceName];
            for (let edge of edges) {
                if (edge.location.NodeLoc != null) {
                    // This edge targets a node (a model).
                    let targetName = edge.location.NodeLoc;
                    let gedge = new MSAGL.GEdge({
                        id: (sourceName + "_to_" + targetName),
                        source: sourceName,
                        target: targetName
                    });
                    ret.addEdge(gedge);
                }
                else {
                    // This edge targets a system within a node.
                    var systemLoc: string[] = <string[]>edge.location.SystemLoc;
                    let targetName = systemLoc[0];
                    let systemName = systemLoc[1];
                    let gedge = new MSAGL.GEdge({
                        id: (sourceName + "_to_" + targetName),
                        source: sourceName,
                        target: targetName + "." + systemName
                    });
                    ret.addEdge(gedge);
                }
            }
        }

        // Deal with settings.
        // Plane transformation.
        ret.settings.transformation = this.vm.horizontalLayout() ? MSAGL.GPlaneTransformation.ninetyDegreesTransformation : MSAGL.GPlaneTransformation.defaultTransformation;
        // Enforcing aspect ratio.
        if (this.vm.aspectRatio()) {
            var w = this.container.offsetWidth;
            var h = this.container.offsetHeight;
            if (w != null && w > 0 && h != null && h > 0)
                ret.settings.aspectRatio = this.vm.horizontalLayout() ? h / w : w / h;
        }
        ret.settings.routing = this.vm.routing();

        // Create the node boundaries.
        ret.createNodeBoundariesFromSVG();

        return ret;
    }

    // The graph currently being laid out.
    private graphInLayout: MSAGL.GGraph = null;

    // Update the graph.
    private render() {
        // If a previous graph is still being laid out, abort it.
        if (this.graphInLayout != null)
            this.graphInLayout.stopLayoutGraph();

        // Make the GGraph.
        this.graphInLayout = this.makeGraph();
        // Put the graph in the IDDSVGGraph instance.
        this.iddgraph.setGraph(this.graphInLayout);

        // Prepare a layout callback.
        var that = this;
        var graphLayoutCallback = function () {
            // Draw it.
            that.iddgraph.drawGraph();
        }
        this.graphInLayout.layoutCallbacks.add(graphLayoutCallback);

        // Begin layout.
        this.graphInLayout.beginLayoutGraph();
    }
}
