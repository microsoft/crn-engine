import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");

var me = new CRNEngine(false);

function appendStatus(text: string) {
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(text));
    status.appendChild(p);
}

function clearStatus() {
    while (status.firstChild)
        status.removeChild(status.firstChild);
}

function onFail(t: string, result: Interfaces.Error) {
    appendStatus("fail (" + t + ") (" + result.message + ")");
}

function parseClicked() {
    clearStatus();
    appendStatus("start");
    var code = codeBox.value;
    var gui = <Interfaces.CRN>JSON.parse(code);
    var lmodel: Interfaces.Model = { top: gui, systems: [] };
    var ig: Interfaces.IG = { nodes: { "": lmodel }, edges: {}, expanded: false };
    var observables = me.UserSimulateGui(ig, "");
    observables.node.subscribe(model => appendStatus("progress (model): " + JSON.stringify(model)), error => onFail("model", error), () => appendStatus("completed (model)"));
    observables.exports.subscribe(exports => appendStatus("progress (exports): " + JSON.stringify(exports)), error => onFail("exports", error), () => appendStatus("completed (exports)"));
    observables.instances.subscribe(instances => appendStatus("progress (sweepdef): " + JSON.stringify(instances)), error => onFail("sweepdef", error), () => appendStatus("completed (sweepdef)"));
    observables.simrun.subscribe(rundef => {
        appendStatus("progress (simrun): " + JSON.stringify(rundef));
        rundef.values.subscribe(simdata => appendStatus("progress (simdata) " + simdata.instance + " " + simdata.time + " " + simdata.values), error => onFail("simdata", error), () => appendStatus("completed (simdata)"));
    }, error => onFail("simrun", error), () => appendStatus("completed (simrun)"));
}

document.getElementById("parseButton").onclick = parseClicked;

import sample from "raw-loader!./testjson.txt";

codeBox.value = sample;