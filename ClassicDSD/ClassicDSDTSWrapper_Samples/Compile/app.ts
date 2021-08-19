import ClassicDSD from "../../ClassicDSDTSWrapper/Scripts/ClassicDSD";
import * as DSDInterfaces from "../../ClassicDSDTSWrapper/Scripts/Interfaces";
import * as CRNInterfaces from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import "../samples.css"

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");

var me = new ClassicDSD(false);

function appendStatus(header: string, text?: string) {
    var p = document.createElement("p");
    var b = document.createElement("b");
    b.appendChild(document.createTextNode(header));
    p.appendChild(b);
    if (text != null)
        p.appendChild(document.createTextNode(text));
    status.appendChild(p);
}

function clearStatus() {
    while (status.firstChild)
        status.removeChild(status.firstChild);
}

function onFail(process: string, t: string, result: CRNInterfaces.Error) {
    appendStatus(process + " fail (" + t + ")", JSON.stringify(result) + ")");
}

var localResult: DSDInterfaces.ParseResult = null;

function parseClicked() {
    clearStatus();
    appendStatus("parse start");
    var code = codeBox.value;
    var observables = me.UserParse(code, false);
    observables.result.subscribe(result => { localResult = result; appendStatus("parse progress (result): ", JSON.stringify(result)); }, error => onFail("parse", "result", error), () => appendStatus("parse success (result)"));
}

function expandClicked() {
    if (localResult == null)
        return;
    appendStatus("expand start");
    var code = codeBox.value;
    var observables = me.UserExpand(code, false, localResult.model, localResult.settings);
    observables.result.subscribe(result => appendStatus("expand progress (result): ", JSON.stringify(result)), error => onFail("expand", "result", error), () => appendStatus("expand success (result)"));
}

document.getElementById("parseButton").onclick = parseClicked;
document.getElementById("expandButton").onclick = expandClicked;

import sample from "raw-loader!../test.txt";

codeBox.value = sample;