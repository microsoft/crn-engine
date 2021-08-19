import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");

var me = new CRNEngine(false);

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

function getProbs(token: Interfaces.Probabilities, name: string) {
    var probObservables = me.GetProbabilityMap(token, name, 1e-8);
    probObservables.subscribe(map => appendStatus("progress (map " + name + "): ", JSON.stringify(map)), error => onFail("map " + name, error), () => appendStatus("completed (map " + name + ")"));
}

function onProbs(probs: Interfaces.InstanceProbabilities) {
    appendStatus("progress (probabilities): ", JSON.stringify(probs));

    var species: string[] = Object.getOwnPropertyNames(probs.probabilities.species);
    for (var i = 0; i < species.length; i++) {
        var name = species[i];
        getProbs(probs.probabilities, name);
    }
}

function parseClicked() {
    clearStatus();
    appendStatus("start " + (new Date()).toString());
    var code = codeBox.value;
    var parseObservables: Interfaces.ParseCodeObservables = me.UserParseCode(code);
    parseObservables.model.subscribe(model => {
        var observables: Interfaces.SimulatorObservables = null;
        observables = me.UserSimulateGui(model, "");
        observables.node.subscribe(model => appendStatus("progress (model): ", JSON.stringify(model)), error => onFail("model", error), () => appendStatus("completed (model)"));
        observables.exports.subscribe(exports => appendStatus("progress (exports): ", JSON.stringify(exports)), error => onFail("exports", error), () => appendStatus("completed (exports)"));
        observables.instances.subscribe(instances => appendStatus("progress (instances): ", JSON.stringify(instances)), error => onFail("instances", error), () => appendStatus("completed (instances)"));
        observables.simrun.subscribe(rundef => {
            appendStatus("progress (simrun): ", JSON.stringify(rundef));
            rundef.values.subscribe(simdata => appendStatus("progress (simdata) ", JSON.stringify(simdata)), error => onFail("simdata", error), () => appendStatus("completed (simdata)"));
        }, error => onFail("simrun", error), () => appendStatus("completed (simrun)"));
        observables.probabilities.subscribe(prob => onProbs(prob), error => onFail("probabilities", error), () => appendStatus("completed (probabilities)"));
    });
}

import sample from "raw-loader!../test_cme.txt";

function loadTest() {
    codeBox.value = sample;
}

document.getElementById("parseButton").onclick = parseClicked;

loadTest();