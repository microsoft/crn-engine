import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");
var serverCB = <HTMLInputElement>document.getElementById("serverCheckBox");

function appendStatus(text: string) {
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(text));
    status.appendChild(p);
}

function clearStatus() {
    while (status.firstChild)
        status.removeChild(status.firstChild);
}

function onParseFail(t: string, result: Interfaces.Error) {
    appendStatus("fail (" + t + ") (" + JSON.stringify(result));
}

function parseClicked() {
    var server = serverCB.checked;
    var me = new CRNEngine(server);
    clearStatus();
    appendStatus("start");
    var code = codeBox.value;
    var observables = me.UserParseCode(code);
    observables.model.subscribe(model => appendStatus("progress (model): " + JSON.stringify(model)), error => onParseFail("model", error), () => appendStatus("success (model)"));
}

import sample1 from "raw-loader!../test.txt";
import sample2 from "raw-loader!../test_instances.txt";
import sample3 from "raw-loader!../test_systems.txt";
import sample4 from "raw-loader!../test_error.txt";

function loadTest() {
    codeBox.value = sample1;
}

function loadTestInstances() {
    codeBox.value = sample2;
}

function loadTestSystems() {
    codeBox.value = sample3;
}

function loadTestError() {
    codeBox.value = sample4;
}

document.getElementById("parseButton").onclick = parseClicked;
document.getElementById("loadTestButton").onclick = loadTest;
document.getElementById("loadTestInstancesButton").onclick = loadTestInstances;
document.getElementById("loadTestSystemsButton").onclick = loadTestSystems;
document.getElementById("loadTestErrorButton").onclick = loadTestError;

loadTest();