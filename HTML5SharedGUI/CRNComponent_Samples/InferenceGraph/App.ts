// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as crnKO from "../../CRNComponent/Scripts/crnKO";
import * as crnVM from "../../CRNComponent/Scripts/crnVM";
import * as InferenceGraphViewer from "../../CRNComponent/Scripts/inferenceGraphViewer";
import CRNSelector from "../../CRNComponent/Scripts/CRNSelector";
import "../samples.css";
import "./styles.css";
import sampleCRN from "../SampleCRN";

// Grab the JSON for the first node of the sample model.
var node = sampleCRN.Nodes()[0];
var json = node.getSerializableForm();
// Deserialise it twice, to make two nodes.
var firstNode = new crnVM.Model();
firstNode.fromSerializableForm(json);
firstNode.Top().name("first");
// Make it have two systems.
var firstNodeA = new crnVM.CRN();
firstNodeA.fromSerializableForm(json.top);
firstNodeA.name("first.A");
var firstNodeB = new crnVM.CRN();
firstNodeB.fromSerializableForm(json.top);
firstNodeB.name("first.B");
firstNode.Systems([firstNodeA, firstNodeB]);
// Make the second node.
var secondNode = new crnVM.Model();
secondNode.fromSerializableForm(json);
secondNode.Top().name("second");
// Make it have two systems.
var secondNodeA = new crnVM.CRN();
secondNodeA.fromSerializableForm(json.top);
secondNodeA.name("second.A");
var secondNodeB = new crnVM.CRN();
secondNodeB.fromSerializableForm(json.top);
secondNodeB.name("second.B");
secondNode.Systems([secondNodeA, secondNodeB]);
// Make an IG with the two nodes.
var ig = sampleCRN.getSerializableForm();
delete ig.nodes[""];
ig.nodes["first"] = firstNode.getSerializableForm();
ig.nodes["second"] = secondNode.getSerializableForm();
// Add an edge from the first node to the first system of the second node.
ig.edges["first"] = [{
    location: { SystemLoc: ["second", "second.A"] },
    parameters: []
}];
// Make a VM for it.
sampleCRN.fromSerializableForm(ig);

$(window).on('load', function () {
    var div = document.getElementById('graph');
    var model = new InferenceGraphViewer.InferenceGraphViewerVM(sampleCRN);
    crnKO.bindInferenceGraph(div, model);

    var selector = document.getElementById('CRNSelector');
    var crnSelector = new CRNSelector(sampleCRN);
    crnSelector.Bind(selector);
});
