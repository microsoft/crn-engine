// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import ClassicDSD from "../../ClassicDSDTSWrapper/Scripts/ClassicDSD";
import * as Interfaces from "../../ClassicDSDTSWrapper/Scripts/Interfaces";
import * as CRNInterfaces from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import "../samples.css"

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");

var me = new ClassicDSD(false);

function appendStatus(header: string, text?: string) {
    var p = document.createElement("p");
    var h = document.createElement("b");
    h.appendChild(document.createTextNode(header));
    p.appendChild(h);
    if (text != null)
        p.appendChild(document.createTextNode(text));
    status.appendChild(p);
}

function clearStatus() {
    while (status.firstChild)
        status.removeChild(status.firstChild);
}

function onFail(process: string, t: string, result: CRNInterfaces.Error) {
    appendStatus(process + " fail (" + t + ")", JSON.stringify(result));
}

function onSimRun(simrun: CRNInterfaces.SimRunDefinition) {
    simrun.values.subscribe(value => appendStatus("simrun value", JSON.stringify(value)), error => appendStatus("simrun values error", JSON.stringify(error)), () => appendStatus("simrun values success"));
    simrun.newplottables.subscribe(value => appendStatus("simrun newplottable", JSON.stringify(value)), error => appendStatus("simrun newplottables error", JSON.stringify(error)), () => appendStatus("simrun newplottables success"));
}

function onParseSuccess(result: Interfaces.ParseResult) {
    var observables = me.UserSimulateGui(result.model, "");
    observables.node.subscribe(node => appendStatus("simulate progress (node)", JSON.stringify(node)), error => onFail("simulate", "node", error), () => appendStatus("simulate success (node)"));
    observables.exports.subscribe(exports => appendStatus("simulate progress (exports)", JSON.stringify(exports)), error => onFail("simulate", "exports", error), () => appendStatus("simulate success (exports)"));
    observables.instances.subscribe(instances => appendStatus("simulate progress (instances)", JSON.stringify(instances)), error => onFail("simulate", "instances", error), () => appendStatus("simulate success (instances)"));
    observables.probabilities.subscribe(probabilities => appendStatus("simulate progress (probabilities)", JSON.stringify(probabilities)), error => onFail("simulate", "probabilities", error), () => appendStatus("simulate success (probabilities)"));
    observables.simrun.subscribe(simrun => { appendStatus("simulate progress (simrun)", JSON.stringify(simrun)); onSimRun(simrun); }, error => onFail("simulate", "simrun", error), () => appendStatus("simulate success (simrun)"));
}

function parseRunClicked() {
    clearStatus();
    appendStatus("parse start");
    var code = codeBox.value;
    var observables = me.UserParse(code, false);
    observables.result.subscribe(result => { appendStatus("parse progress (result)", JSON.stringify(result)); onParseSuccess(result); }, error => onFail("parse", "result", error), () => appendStatus("parse success (result)"));
}

document.getElementById("parseRunButton").onclick = parseRunClicked;

import sample from "raw-loader!./jittest.txt";

codeBox.value = sample;