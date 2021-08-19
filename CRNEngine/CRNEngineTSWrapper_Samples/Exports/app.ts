import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");

var me = new CRNEngine(false);
var model: Interfaces.IG = null;

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
    clearStatus();
    appendStatus("start");
    var code = codeBox.value;
    var observables = me.UserParseCode(code);
    observables.model.subscribe(lmodel => { model = lmodel; appendStatus("progress (model): " + JSON.stringify(model)) }, error => onParseFail("model", error), () => appendStatus("success (model)"));
}

function exportClicked() {
    if (model == null)
        return;
    var nodeId = null;
    for (var n in model.nodes) {
        nodeId = n;
        break;
    }
    var observables = me.UserGenerateExports(model, nodeId);
    observables.exports.subscribe(exportgui => appendStatus("progress (exports): " + JSON.stringify(exportgui)), error => onParseFail("exports", error), () => appendStatus("success (exports)"));
}

import sample1 from "raw-loader!../test.txt";
import sample2 from "raw-loader!../test_instances.txt";

function loadTest() {
    codeBox.value = sample1;
}

function loadTestInstances() {
    codeBox.value = sample2;
}

document.getElementById("parseButton").onclick = parseClicked;
document.getElementById("loadTestButton").onclick = loadTest;
document.getElementById("loadTestInstancesButton").onclick = loadTestInstances;
document.getElementById("exportButton").onclick = exportClicked;

loadTest();