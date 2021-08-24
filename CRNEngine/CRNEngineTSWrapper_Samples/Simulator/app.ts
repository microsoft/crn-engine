// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");
var serverCB = <HTMLInputElement>document.getElementById("serverCheckBox");

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

function onFail(t: string, result: Interfaces.Error) {
    appendStatus("fail (" + t + ") ", result.message + (new Date()).toString());
}

function parseClicked() {
    var server = serverCB.checked;
    var me = new CRNEngine(server);
    clearStatus();
    appendStatus("start ", (new Date()).toString());
    var code = codeBox.value;
    var parseObservables: Interfaces.ParseCodeObservables = me.UserParseCode(code);
    parseObservables.model.subscribe(model => {
        var observables: Interfaces.SimulatorObservables = null;
        observables = me.UserSimulateGui(model, "");
        observables.node.subscribe(model => appendStatus("progress (model): ", JSON.stringify(model)), error => onFail("crn", error), () => appendStatus("completed (model)"));
        observables.exports.subscribe(exports => appendStatus("progress (exports): ", JSON.stringify(exports)), error => onFail("exports", error), () => appendStatus("completed (exports)"));
        observables.instances.subscribe(instances => appendStatus("progress (instances): ", JSON.stringify(instances)), error => onFail("instances", error), () => appendStatus("completed (instances)"));
        observables.simrun.subscribe(rundef => {
            appendStatus("progress (simrun): " + JSON.stringify(rundef));
            rundef.values.subscribe(value => appendStatus("progress (values) ", JSON.stringify(value)), error => onFail("values", error), () => appendStatus("completed (values)"));
            rundef.tables.subscribe(table => appendStatus("progress (tables) ", JSON.stringify(table)), error => onFail("tables", error), () => appendStatus("completed (tables)"));
        }, error => onFail("simrun", error), () => appendStatus("completed (simrun)"));
        observables.statespace.subscribe(ss => appendStatus("progress (statespace): ", JSON.stringify(ss)), error => onFail("statespace", error), () => appendStatus("completed (statespace)"));
        observables.probabilities.subscribe(prob => appendStatus("progress (probabilities): ", JSON.stringify(prob)), error => onFail("probabilities", error), () => appendStatus("completed (probabilities)"));
    });
}

import sample1 from "raw-loader!../test.txt";
import sample2 from "raw-loader!../test_lna.txt";
import sample3 from "raw-loader!../test_cme.txt";
import sample4 from "raw-loader!../test_instances.txt";
import sample5 from "raw-loader!../test_systems.txt";
import sample6 from "raw-loader!../test_trajectories.txt";

function loadTest() {
    codeBox.value = sample1;
}

function loadTestLna() {
    codeBox.value = sample2;
}

function loadTestCme() {
    codeBox.value = sample3;
}

function loadTestInstances() {
    codeBox.value = sample4;
}

function loadTestSystems() {
    codeBox.value = sample5;
}

function loadTestTrajectories() {
    codeBox.value = sample6;
}

document.getElementById("parseButton").onclick = parseClicked;
document.getElementById("loadTestButton").onclick = loadTest;
document.getElementById("loadTestLnaButton").onclick = loadTestLna;
document.getElementById("loadTestCmeButton").onclick = loadTestCme;
document.getElementById("loadTestInstancesButton").onclick = loadTestInstances;
document.getElementById("loadTestSystemsButton").onclick = loadTestSystems;
document.getElementById("loadTestTrajectoriesButton").onclick = loadTestTrajectories;

loadTest();