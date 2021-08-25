// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import StrandGraphs from "../../Scripts/StrandGraphs";
import * as CRNInterfaces from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var parseStatus = document.getElementById("parseStatus");
var simulateStatus = document.getElementById("simulateStatus");

var me = new StrandGraphs(false);

function appendParseStatus(text: string) {
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(text));
    parseStatus.appendChild(p);
}

function appendSimulateStatus(text: string) {
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(text));
    simulateStatus.appendChild(p);
}

function clearParseStatus() {
    while (parseStatus.firstChild)
        parseStatus.removeChild(parseStatus.firstChild);
}

function clearSimulateStatus() {
    while (simulateStatus.firstChild)
        simulateStatus.removeChild(simulateStatus.firstChild);
}

function onParseFail(t: string, result: CRNInterfaces.Error) {
    appendParseStatus("fail (" + t + ") (" + JSON.stringify(result));
}

function onSimulateFail(t: string, result: CRNInterfaces.Error) {
    appendSimulateStatus("fail (" + t + ") (" + JSON.stringify(result) + ")");
}

var currentModel: CRNInterfaces.IG = null;

function parseClicked() {
    clearParseStatus();
    clearSimulateStatus();
    appendParseStatus("start");
    var code = codeBox.value;
    var observables = me.UserCompile(code);
    observables.result.subscribe(result => { currentModel = result.model; appendParseStatus("progress (result): " + JSON.stringify(result)); }, error => onParseFail("result", error), () => appendParseStatus("success (result)"));
}

function simulateClicked() {
    if (currentModel == null)
        return;
    clearSimulateStatus();
    appendSimulateStatus("start");
    var observables = me.UserSimulateGui(currentModel, "");
    observables.node.subscribe(node => appendSimulateStatus("progress (node): " + JSON.stringify(node)), error => onSimulateFail("node", error), () => appendSimulateStatus("completed (node)"));
    observables.exports.subscribe(exports => appendSimulateStatus("progress (exports): " + JSON.stringify(exports)), error => onSimulateFail("exports", error), () => appendSimulateStatus("completed (exports)"));
    observables.instances.subscribe(instances => appendSimulateStatus("progress (instances): " + JSON.stringify(instances)), error => onSimulateFail("instances", error), () => appendSimulateStatus("completed (instances)"));
    observables.simrun.subscribe(rundef => {
        appendSimulateStatus("progress (simrun): " + JSON.stringify(rundef));
        rundef.values.subscribe(simdata => appendSimulateStatus("progress (simdata) " + simdata.instance + " " + simdata.time + " " + simdata.values), error => onSimulateFail("simdata", error), () => appendSimulateStatus("completed (simdata)"));
    }, error => onSimulateFail("simrun", error), () => appendSimulateStatus("completed (simrun)"));
}

document.getElementById("parseButton").onclick = parseClicked;
document.getElementById("simulateButton").onclick = simulateClicked;

require(["text!Samples/test.txt"], (sample:string) => {
    codeBox.value = sample;
});