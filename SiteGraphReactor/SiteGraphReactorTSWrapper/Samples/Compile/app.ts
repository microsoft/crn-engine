// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import StrandGraphs from "../../Scripts/StrandGraphs";
import * as CRNInterfaces from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var status = document.getElementById("status");

var me = new StrandGraphs(false);

function appendStatus(text: string) {
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(text));
    status.appendChild(p);
}

function clearStatus() {
    while (status.firstChild)
        status.removeChild(status.firstChild);
}

function onFail(process: string, type: string, result: CRNInterfaces.Error) {
    appendStatus(process + " fail (" + type + ") (" + JSON.stringify(result));
}

function parseClicked() {
    clearStatus();
    appendStatus("parse start");
    var code = codeBox.value;
    var observables = me.UserParse(code);
    observables.result.subscribe(result => appendStatus("parse progress (result): " + JSON.stringify(result)), error => onFail("parse", "result", error), () => appendStatus("parse success (result)"));
}

function expandClicked() {
    appendStatus("expand start");
    var code = codeBox.value;
    var observables = me.UserExpand(code);
    observables.result.subscribe(result => appendStatus("expand progress (result): " + JSON.stringify(result)), error => onFail("expand", "result", error), () => appendStatus("expand success (result)"));
}

document.getElementById("parseButton").onclick = parseClicked;
document.getElementById("expandButton").onclick = expandClicked;

require(["text!Samples/test.txt"], (sample: string) => {
    codeBox.value = sample;
});