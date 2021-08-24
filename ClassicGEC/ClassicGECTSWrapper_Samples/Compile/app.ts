// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import ClassicGEC from "../../ClassicGECTSWrapper/Scripts/ClassicGEC";
import * as CRNInterfaces from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import "../samples.css"

var codeBox = <HTMLTextAreaElement>document.getElementById("codeBox");
var partsBox = <HTMLTextAreaElement>document.getElementById("partsBox");
var reactionsBox = <HTMLTextAreaElement>document.getElementById("reactionsBox");
var status = document.getElementById("status");

var me = new ClassicGEC();

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

function onCompileDone() {
    var observables = me.UserGetSolution(0);
    observables.solution.subscribe(solution => appendStatus("getsolution progress (solution): ", JSON.stringify(solution)), error => onFail("getsolution", "solution", error), () => appendStatus("getsolution success (solution)"));
    observables.jsbol.subscribe(jsbol => appendStatus("getsolution progress (jsbol): ", JSON.stringify(jsbol)), error => onFail("getsolution", "jsbol", error), () => appendStatus("getsolution success (jsbol)"));
    observables.exports.subscribe(expdef => appendStatus("getsolution progress (exports): ", JSON.stringify(expdef)), error => onFail("getsolution", "exports", error), () => appendStatus("getsolution success (exports)"));
}

function compileClicked() {
    clearStatus();
    appendStatus("parse start");
    var code = codeBox.value;
    me.Parts = partsBox.value;
    me.Reactions = reactionsBox.value;
    var observables = me.UserCompile(code);
    observables.solution_count.subscribe(count => appendStatus("compile progress (solution_count): ", JSON.stringify(count)), error => onFail("compile", "solution_count", error), () => { appendStatus("compile success (solution_count)"); onCompileDone(); });
    observables.jsbol.subscribe(jsbol => appendStatus("compile progress (jsbol): ", JSON.stringify(jsbol)), error => onFail("compile", "jsbol", error), () => appendStatus("compile success (jsbol)"));
    observables.exports.subscribe(expdef => appendStatus("compile progress (exports): ", JSON.stringify(expdef)), error => onFail("compile", "exports", error), () => appendStatus("compile success (exports)"));
}

document.getElementById("compileButton").onclick = compileClicked;

import sample from "raw-loader!../test.txt";

codeBox.value = sample;
partsBox.value = me.Parts;
reactionsBox.value = me.Reactions;
