// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Interfaces from './Interfaces';
import * as Internals from './InternalInterfaces';

importScripts("WebWorkerSafeChecks.js");
// Unpleasant workaround, current 4.x WebSharper needs a "window" variable
// https://github.com/intellifactory/websharper/issues/641#issuecomment-289730809
let selfLocal: any = self;
selfLocal.window = self;
// Load the transcompiled F# code.
//importScripts("CRNEngineJS.js");
importScripts("CRNEngineJS.min.js");
delete selfLocal.window;

declare var Microsoft: any;
declare var Parser: any;
declare var IntelliFactory: any;

function handleParseExpressionRequest(str: string) {
    var expression = Microsoft.Research.CRNEngine.Expression.from_string(Parser.name(), str);
    var jexpression: Interfaces.Expression = Microsoft.Research.CRNEngine.WebSharperSerialization.to_JSON_expression(expression);
    self.postMessage({ type: "expression", object: jexpression }, undefined);
}

function handleParseTimeUnit(str: string) {
    var timeUnit = Microsoft.Research.CRNEngine.Units.time_from_string(str);
    var jTimeUnit: Interfaces.TimeUnit = Microsoft.Research.CRNEngine.WebSharperSerialization.to_JSON_timeUnit(timeUnit);
    self.postMessage({ type: "timeUnit", object: jTimeUnit }, undefined);
}

function handleParseSpaceUnit(str: string) {
    var spaceUnit = Microsoft.Research.CRNEngine.Units.space_from_string(str);
    var jSpaceUnit: Interfaces.SpaceUnit = Microsoft.Research.CRNEngine.WebSharperSerialization.to_JSON_spaceUnit(spaceUnit);
    self.postMessage({ type: "spaceUnit", object: jSpaceUnit }, undefined);
}

function handleParseConcentrationUnit(str: string) {
    var concentrationUnit = Microsoft.Research.CRNEngine.Units.concentration_from_string(str);
    var jConcentrationUnit: Interfaces.ConcentrationUnit = Microsoft.Research.CRNEngine.WebSharperSerialization.to_JSON_concentrationUnit(concentrationUnit);
    self.postMessage({ type: "concentrationUnit", object: jConcentrationUnit }, undefined);
}

function handleStringifyTimeUnit(jTimeUnit: Interfaces.TimeUnit) {
    var timeUnit = Microsoft.Research.CRNEngine.WebSharperSerialization.of_JSON_timeUnit(jTimeUnit);
    var str = Microsoft.Research.CRNEngine.Units.time_to_string(timeUnit);
    self.postMessage({ type: "string", object: str }, undefined);
}

function handleStringifySpaceUnit(jSpaceUnit: Interfaces.SpaceUnit) {
    var spaceUnit = Microsoft.Research.CRNEngine.WebSharperSerialization.of_JSON_spaceUnit(jSpaceUnit);
    var str = Microsoft.Research.CRNEngine.Units.space_to_string(spaceUnit);
    self.postMessage({ type: "string", object: str }, undefined);
}

function handleStringifyConcentrationUnit(jConcentrationUnit: Interfaces.ConcentrationUnit) {
    var concentrationUnit = Microsoft.Research.CRNEngine.WebSharperSerialization.of_JSON_concentrationUnit(jConcentrationUnit);
    var str = Microsoft.Research.CRNEngine.Units.concentration_to_string(concentrationUnit);
    self.postMessage({ type: "string", object: str }, undefined);
}

function processFastTaskMessage(e: any) {
    var req: Internals.FastWorkerRequest = e.data;
    console.log("fast task request: " + req.mtype);
    try {
        if (e.data.type == "parseexpression")
            handleParseExpressionRequest((<Internals.FastWorkerRequest_ParseExpression>req).expression);
        else if (e.data.type == "parseTimeUnit")
            handleParseTimeUnit((<Internals.FastWorkerRequest_ParseTimeUnit>req).unittext);
        else if (e.data.type == "parseSpaceUnit")
            handleParseSpaceUnit((<Internals.FastWorkerRequest_ParseSpaceUnit>req).unittext);
        else if (e.data.type == "parseConcentrationUnit")
            handleParseConcentrationUnit((<Internals.FastWorkerRequest_ParseConcentrationUnit>req).unittext);
        else if (e.data.type == "stringifyTimeUnit")
            handleStringifyTimeUnit((<Internals.FastWorkerRequest_StringifyTimeUnit>req).unit);
        else if (e.data.type == "stringifySpaceUnit")
            handleStringifySpaceUnit((<Internals.FastWorkerRequest_StringifySpaceUnit>req).unit);
        else if (e.data.type == "stringifyConcentrationUnit")
            handleStringifyConcentrationUnit((<Internals.FastWorkerRequest_StringifyConcentrationUnit>req).unit);
    }
    catch (exc) {
        // This might be a parsing error in F# format. That's what these '$' fields are about.
        if (exc.$0 != null && exc.$0.Message != null && exc.$0.Errors != null) {
            var perr: Interfaces.ParsingError = { message: exc.$0.Message, positions: exc.$0.Errors, extra: exc.extra };
            var msg: Internals.FastWorkerResponse_Error = { mtype: "error", error: perr };
            self.postMessage(msg, undefined);
        }
        else {
            var err: Interfaces.Error = { message: "" + exc, extra: exc.extra };
            var msg: Internals.FastWorkerResponse_Error = { mtype: "error", error: err };
            self.postMessage(msg, undefined);
        }
    }
};

self.addEventListener('message', processFastTaskMessage);

export function processMessages(e: any[]) {
    for (var i in e)
        processFastTaskMessage(e[i]);
}