// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var parallelCheckbox = <HTMLInputElement>document.getElementById("parallelCheckbox");
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

function runStateSpace(model: Interfaces.IG) {
    var ssObservable = me.UserStateSpace(model, false);
    ssObservable.subscribe(ss => appendStatus("state space: ", JSON.stringify(ss)), error => onFail("state space", error), () => appendStatus("state space completed"));
}

function parseClicked() {
    clearStatus();
    appendStatus("start " + (new Date()).toString());
    var code = codeBox.value;
    var parseObservables = me.UserParseCode(code);
    parseObservables.model.subscribe(model => runStateSpace(model), error => onFail("parse", error), () => appendStatus("parse completed"));
}

import sample from "raw-loader!./test.txt";

function loadTest() {
    codeBox.value = sample;
}

document.getElementById("parseButton").onclick = parseClicked;

loadTest();