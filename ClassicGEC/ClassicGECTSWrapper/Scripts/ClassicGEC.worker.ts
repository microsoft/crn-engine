// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Import the generic Worker. Note that here I'm importing it as a regular module, rather than as a web worker constructor (i.e. via worker-loader).
import * as CRNEngineWorker from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine.worker";
import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from "./Interfaces";
import * as CRNInterfaces from "./../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces";
import { jsbolToExport } from "./JSBOL";

// Configure the main worker.
CRNEngineWorker.config.loader = () => {
    var classicGECJS = require("raw-loader!./../../ClassicGECJS/Content/ClassicGECJS.min.js");
    classicGECJS = classicGECJS.replace(/('|")use strict('|");?/, '');
    eval.call(null, classicGECJS);
    compile = Microsoft.Research.GEC.JSONAPI.compile;
    get_solution = Microsoft.Research.GEC.JSONAPI.get_solution
}
CRNEngineWorker.config.processMessage = processMessageGEC;

// Declare some shortcuts.
declare var Microsoft: any;
var compile: WGI.compile;
var get_solution: WGI.get_solution;

var local_result: WGI.Microsoft.Research.GEC.JSAPI.solve_result;
var local_solution: WGI.Microsoft.Research.GEC.JSAPI.solution_result;

function handleCompile(code: string, parts: string, reactions: string) {
    if (code == null) code = "";
    if (parts == null) parts = "";
    if (reactions == null) reactions = "";
    // Invoke compilation on the given code.
    local_result = compile(code, parts, reactions);
    local_solution = null;
    // Send the solution count to the main thread.
    var count = (<any>local_result.solution).solution[1].numSolutions;
    var solsmsg: Interfaces.WorkerResponse_GECSolutions = { mtype: "gec.solutions", count: count };
    self.postMessage(solsmsg, undefined);
    var jsbolmsg: Interfaces.WorkerResponse_JSBOL = { mtype: "gec.jsbol", document: local_result.jsbol };
    self.postMessage(jsbolmsg, undefined);
}

function handleGetSolution(idx: number) {
    local_solution = get_solution(local_result, idx);
    var solmsg: Interfaces.WorkerResponse_GECSolution = { mtype: "gec.solution", solution: { model: local_solution.model, code: local_solution.crnstring } };
    self.postMessage(solmsg, undefined);
    var jsbolmsg: Interfaces.WorkerResponse_JSBOL = { mtype: "gec.jsbol", document: local_solution.jsbol };
    self.postMessage(jsbolmsg, undefined);
}

/** Processes a task coming from the main thread. Calculus-specific tasks will be handled here, CRN generic tasks will be passed to the processMessage function in CRNEngine/Worker. */
function processMessageGEC(e: any) {
    var req = <Interfaces.WorkerRequest>e.data;
    console.log("worker request: " + req.mtype);
    try {
        switch (req.mtype) {
            case "generateexports":
                // I want to send the normal exports, and then send the XML SBOL export.
                CRNEngineWorker.handleGenerateExports((<CRNInterfaces.WorkerRequest_GenerateExports>req).model, (<CRNInterfaces.WorkerRequest_GenerateExports>req).nodeId);
                var exportmsg: CRNInterfaces.WorkerResponse_Export = null;
                if (local_solution != null)
                    exportmsg = jsbolToExport(local_solution.jsbol);
                else if (local_result != null)
                    exportmsg = jsbolToExport(local_result.jsbol);
                if (exportmsg != null)
                    self.postMessage(exportmsg, undefined);
                break;
            case "gec.compile":
                handleCompile((<Interfaces.WorkerRequest_GECCompile>req).code, (<Interfaces.WorkerRequest_GECCompile>req).parts, (<Interfaces.WorkerRequest_GECCompile>req).reactions);
                break;
            case "gec.getsolution":
                handleGetSolution((<Interfaces.WorkerRequest_GECGetSolution>req).idx);
                break;
            default:
                // Let the generic worker handle this. Note that it will also send the finished signal, so I don't need to.
                CRNEngineWorker.processMessage(e);
                return;
        }
        CRNEngineWorker.sendFinished();
    }
    catch (exc) {
        if (exc.Data0 == null)
            CRNEngineWorker.handleException(exc);
        else {
            exc.Data1.extra = exc.Data0;
            exc.Data1.message = "Error while parsing " + exc.Data0 + ": " + exc.Data1.message;
            CRNEngineWorker.handleException(exc.Data1);
        }
    }
};