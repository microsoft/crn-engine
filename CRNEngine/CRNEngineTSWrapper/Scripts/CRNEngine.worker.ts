// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from './Interfaces';
import * as Internals from './InternalInterfaces';

// This is for the benefit of the testing environment. If there's a more specific worker file, that's where it should be loaded.
var wwsafe = require("raw-loader!./WebWorkerSafeChecks.js");
eval.call(null, wwsafe);
/* This is the main web worker for the CRN Engine. F# code is loaded and invoked here. This worker will accept messages, execute a
task, and answer with one or more messages. */

type InferenceResult = WGI.Microsoft.Research.CRNEngine.JSAPI.inference_result;
type Probabilities = WGI.Microsoft.Research.CRNEngine.Probabilities;

import CRNEngine = WGI.Microsoft.Research.CRNEngine;

import sundials from '../../../Lib/Sundials_v3.1.1_WebAssembly/SundialsSolver.js';
// Note that the following import statement will produce the URL of the WASM file.
import sundialsWasm from '../../../Lib/Sundials_v3.1.1_WebAssembly/SundialsSolver.wasm';
import WsEmGlue from './WsEmGlue';

// This is a cancel token I can pass to methods. Note that this will never be set, because of how web workers work; cancellation is done by killing the worker. Effectively, this token is just used to fill a parameter.
var cancel = { contents: false };

export interface WorkerConfig {
    loader: () => void;
    isJITfunc: isJITType;
    getJITfunc: getJITType;
    processMessage: (e: any) => void;
}

// Other calculi can change this.
export var config: WorkerConfig = {
    loader: () => {
        var crnEngineJS = require("raw-loader!./../../CRNEngineJS/Content/CRNEngineJS.min.js");
        crnEngineJS = crnEngineJS.replace(/('|")use strict('|");?/, '');
        eval.call(null, crnEngineJS);
    },
    isJITfunc: null,
    getJITfunc: null,
    processMessage: processMessage
};

// Load the WASM transpiled Sundials. This is an asynchronous operation. The web worker will not be ready before it's completed.
sundials({
    locateFile(path: string) {
        // This is necessary to allow the WASM loader (in SundialsSolver.js) to get the WASM file from a webpack-processed folder structure.
        if (path == 'SundialsSolver.wasm')
            return sundialsWasm;
        return path;
    }
}).then((Module: any) => {
    // Now that the WASM module has been loaded, I can create the WebSharper/Emscripten glue module and complete initialization of the worker.
    var WS_EM_GLUE = new WsEmGlue(Module);
    completeInitialization(config, WS_EM_GLUE);
});

// This will be the main entry point for WebSharper-generated functions.
declare var Microsoft: any;

export var user_parse_code: WGI.user_parse_code;
export var user_get_exports: WGI.user_get_exports;
export var user_get_export: WGI.user_get_export;
export var user_infer_gui: WGI.user_infer_gui;
export var user_get_sim_runs: WGI.user_get_sim_runs;
var simulateFloat: WGI.simulateFloat;
var simulateFloatJIT: WGI.simulateFloatJIT<any>;
var simulateMeanStdev: WGI.simulateMeanStdev;
var simulateMeanStdevProbabilities: WGI.simulateMeanStdevProbabilities;
var simulateSpatial1D: WGI.simulateSpatial1D;
var simulateSpatial2D: WGI.simulateSpatial2D;
var simulateMeanStdevTable: WGI.simulateMeanStdevTable;
var getProbabilityMap: WGI.getProbabilityMap;
export var user_state_space: WGI.user_state_space;
export var user_state_space_jit: WGI.user_state_space_jit<any>;
export var expression_to_string: WGI.expression_to_string;
export var mcplot_to_string: WGI.mcplot_to_string;
export type getJITType = (model: CRNEngine.GuiModel, instance: Interfaces.SimulationInstance) => CRNEngine.JSAPI.jit<any>;
export type isJITType = (model: CRNEngine.GuiModel) => boolean;
var getJIT: getJITType;
var isJIT: isJITType;

var performance: Performance;
if (typeof performance == "undefined")
    (<any>performance) = { now: () => 0 };

/** This is where we'll store messages that arrive before initialization is complete. After initialization is done, this variable will be set to null. */
var messageQueue: any[] = [];

/**
 * At web worker startup, there are asynchronous operations that need to be completed before the worker is able to process requests. Call this function after these operations are done.
 * @param loader A function that synchronously loads the WebSharper-built F# bundle.
 * @param WS_EM_GLUE The async-loaded WebSharper-Emscripten "glue" code.
 * @param isJITFunc A function that determines whether a model is JIT. Null if the calculus does not support JIT.
 * @param getJITfunc A function that constructs a JIT object out of a model. Null if the calculus does not support JIT.
 */
export function completeInitialization(config: WorkerConfig, WS_EM_GLUE: WsEmGlue) {
    initializeWebSharper(config.loader, WS_EM_GLUE);
    loadFSharpFunctions(config.isJITfunc, config.getJITfunc);
    var q = messageQueue;
    messageQueue = null;
    for (var msg of q)
        handleMessage(msg);
}

function initializeWebSharper(loader: () => void, WS_EM_GLUE: WsEmGlue) {
    // Unpleasant workaround, current 4.x WebSharper needs a "window" variable
    // https://github.com/intellifactory/websharper/issues/641#issuecomment-289730809
    let selfLocal: any = self;
    selfLocal.window = self;
    selfLocal.window.WS_EM_GLUE = WS_EM_GLUE;
    loader();
    delete selfLocal.window;
}

/** Loads the F# functions into typed references. This needs to happen *after* the importScripts calls. */
function loadFSharpFunctions(isJITfunc: isJITType, getJITfunc: getJITType) {
    if (typeof Microsoft == "undefined")
        console.log("Worker initialization failed: Microsoft namespace not defined. Bad F# bundle import?");

    user_parse_code = Microsoft.Research.CRNEngine.JSONAPI.user_parse_code;
    user_get_exports = Microsoft.Research.CRNEngine.JSONAPI.user_get_exports;
    user_get_export = Microsoft.Research.CRNEngine.JSONAPI.user_get_export;
    user_infer_gui = Microsoft.Research.CRNEngine.JSONAPI.user_infer_gui;
    user_get_sim_runs = Microsoft.Research.CRNEngine.JSONAPI.user_get_sim_runs;
    simulateFloat = Microsoft.Research.CRNEngine.JSONAPI.simulateFloat;
    simulateFloatJIT = Microsoft.Research.CRNEngine.JSONAPI.simulateFloatJIT;
    simulateMeanStdev = Microsoft.Research.CRNEngine.JSONAPI.simulateMeanStdev;
    simulateMeanStdevProbabilities = Microsoft.Research.CRNEngine.JSONAPI.simulateMeanStdevProbabilities;
    simulateSpatial1D = Microsoft.Research.CRNEngine.JSONAPI.simulateSpatial1D;
    simulateSpatial2D = Microsoft.Research.CRNEngine.JSONAPI.simulateSpatial2D;
    simulateMeanStdevTable = Microsoft.Research.CRNEngine.JSONAPI.simulateMeanStdevTable;
    getProbabilityMap = Microsoft.Research.CRNEngine.JSONAPI.getProbabilityMap;
    user_state_space = Microsoft.Research.CRNEngine.JSONAPI.user_state_space;
    user_state_space_jit = Microsoft.Research.CRNEngine.JSONAPI.user_state_space_jit;
    expression_to_string = Microsoft.Research.CRNEngine.JSONAPI.expression_to_string;
    mcplot_to_string = Microsoft.Research.CRNEngine.JSONAPI.mcplot_to_string;
    isJIT = isJITfunc == null ? gui => false : isJITfunc;
    getJIT = getJITfunc;
}

/** Sends the "finished" message to the main thread. After sending this, the task should be considered completed. */
export function sendFinished() {
    var msg: Internals.WorkerResponse_Finished = { mtype: "finished" };
    self.postMessage(msg, undefined);
}

function getPlottables(gui: Interfaces.CRN, settings: Interfaces.SimSettings): string[] {
    var stringPlots = settings.plots;
    var mcplots = gui.settings.moment_closure.plots;
    var stringMCPlots = mcplots.map(mcplot_to_string);
    var allStringPlots = stringPlots.concat(stringMCPlots);
    return allStringPlots;
}

/** Sends a Model to the main thread. */
export function sendIG(model: Interfaces.IG) {
    var msg: Internals.WorkerResponse_Model = { mtype: "model", model: model };
    self.postMessage(msg, undefined);
    return model;
}

/** Sends an export definition to the main thread. */
export function sendExport(e: Interfaces.ExportDef) {
    var msg: Internals.WorkerResponse_Export = { mtype: "export", export: e };
    self.postMessage(msg, undefined);
}

/** Generates the exports for the given CRNGUI, and sends them to the main thread. */
export function generateAndSendExports(final: boolean, model: Interfaces.IG, nodeId: string) {
    var exps = user_get_exports(final, model, nodeId);
    for (let exp of exps)
        sendExport(exp);
}

/** Returns an appropriate simulation results callback, for a given instance. */
function getSimResultsCallback(id: number) {
    return (row: any) => {
        var msg: Internals.WorkerResponse_SimResult = { mtype: "simresult", row: { instance: id, time: row.time, values: row.values } };
        self.postMessage(msg, undefined);
    };
}

/** Returns an appropriate simulation results callback, for a given instance. */
function getSimTablesCallback(id: number) {
    return (table: any) => {
        var msg: Internals.WorkerResponse_SimTable = { mtype: "simtable", table: { instance: id, time: table.times, values: table.columns.map((c: any) => c.values) } };
        self.postMessage(msg, undefined);
    };
}

/** Returns an appropriate new plottable callback, for a given instance. */
function getNewPlottableCallback(id: number) {
    return (plottable: Interfaces.NewPlottable) => {
        var msg: Internals.WorkerResponse_NewPlottable = { mtype: "newplottable", plottable: { instance: id, plottable: plottable } };
        self.postMessage(msg, undefined);
    }
}

/** Produces a callback to return a state space. Note: multiple instances are not currently supported. */
function getStateSpaceCallback(id: number) {
    return (ss: CRNEngine.JSAPI.state_space) => {
        var msg: Internals.WorkerResponse_StateSpace = { mtype: "statespace", statespace: ss };
        self.postMessage(msg, undefined);
    }
}

/** Sends inference parameters to the main thread. */
function inferenceParametersCallback(result: Interfaces.InferenceParameters) {
    var parmsg: Internals.WorkerResponse_ParameterDefinitions = { mtype: "parameterdefinitions", nodeId: result.nodeId, parameters: result.parameters };
    self.postMessage(parmsg, undefined);
}

/** Sends inference results to the main thread. This includes summary, parameters and species values. */
function inferenceResultsCallback(result: InferenceResult) {
    var summsg: Internals.WorkerResponse_Summary = { mtype: "summary", nodeId: result.nodeId, summary: <string>result.summary };
    self.postMessage(summsg, undefined);
    var progressmsg: Internals.WorkerResponse_InferenceProgress = { mtype: "inferenceprogress", nodeId: result.nodeId, progress: result.iteration };
    self.postMessage(progressmsg, undefined);
    var state: CRNEngine.JSAPI.inference_evaluated_values = undefined;
    var mleLglk = -Infinity;
    if (result.state.BurninPhase != null) {
        var bphase = result.state.BurninPhase;
        state = bphase.mle;
        mleLglk = bphase.mle.lglk;
    }
    else {
        var sphase = result.state.SamplingPhase;
        state = sphase.mle;
        mleLglk = sphase.mle.lglk;
        var posteriorTableUpdate = sphase.chain[0]; //we are interested in list head
        if (posteriorTableUpdate && sphase.thinningSkippedCount == 0) { //if list is not empty && posterior table is just appended
            var postTableUpdateMsg: Internals.WorkerResponse_InferenceChain = { mtype: "inferencechainupdate", nodeId: result.nodeId, update: posteriorTableUpdate };
            self.postMessage(postTableUpdateMsg, undefined);
        }
    }
    if (result.lkincreased) {
        var parmsg: Internals.WorkerResponse_ParameterResult = { mtype: "parameterresult", nodeId: result.nodeId, values: state };
        self.postMessage(parmsg, undefined);
    }
    if (result.mlesims != null) {
        var mlesims = result.mlesims;
        console.log("MLE lglk increased to " + mleLglk + ". sending " + mlesims.length + " simulations");
        for (var i = 0; i < mlesims.length; i++) {
            var sim = mlesims[i];
            var res: Interfaces.InferenceResult = { result: sim, sim_id: i };
            var resmsg: Internals.WorkerResponse_InferenceResult = { mtype: "inferenceresult", nodeId: result.nodeId, result: res };
            self.postMessage(resmsg, undefined);
        }
    }
}

/** Handles a request to parse code. */
function handleUserParseCode(code: string) {
    if (code == null) code = "";
    var crn = user_parse_code(code);
    sendIG(crn);
}

/** Handles a request to generate exports. */
export function handleGenerateExports(ig: Interfaces.IG, nodeId: string) {
    var exps = user_get_exports(false, ig, nodeId);
    for (let exp of exps)
        sendExport(exp);
}

function handleGenerateExport(ig: Interfaces.IG, nodeId: string, id: string) {
    var exp = user_get_export(ig, nodeId, id);
    sendExport(exp);
}

/** Handles a request to "parse" and run inference on a GUI CRN with attached settings. */
function handleUserInferGui(ig: Interfaces.IG) {
    if (ig.task != null && ig.task.task_type == "Parse")
        throw "this model is designed for parsing only";
    for (var node in ig.nodes)
        sendSimRuns(ig, node);
    user_infer_gui(ig, sendExport, inferenceParametersCallback, inferenceResultsCallback, cancel);
}

/** Runs all of the simulations in the provided model sequentially. Sends the simulation type, the instance definitions, and then the actual results, to the main thread. */
function runAllSimulations(ig: Interfaces.IG, nodeId: string) {
    sendIG(ig);
    var model = ig.nodes[nodeId];
    var modelmsg: Internals.WorkerResponse_Node = { mtype: "node", node: model };
    self.postMessage(modelmsg, undefined);

    // Get all of the simulation run definitions.
    var simRuns = user_get_sim_runs(ig, model.top.name);

    // Send the result type to the main thread.
    var msg: Internals.WorkerResponse_SimType = { mtype: "simtype", simtype: simRuns.value_type };
    self.postMessage(msg, undefined);

    // Create the instance definitions. Note that these are simply the sim instances, wrapped in an object that attaches an ID to them. The ID will later be used to generate a callback that will, in turn, append the ID to the simulation results. In this way, the main thread can discriminate messages.
    var definitions: Interfaces.SimulationInstanceDefinition[] = simRuns.instances.map((e, i) => {
        return { id: i, crnName: model.top.name, instance: e, };
    });

    // I'll send the instance definitions first, so I can let the main thread know all about what instances I'll be running without having to wait for them to actually start.
    var defsmsg: Internals.WorkerResponse_SimInstanceDefinitions = { mtype: "instancedefinitions", nodeId: nodeId, definitions: definitions };
    self.postMessage(defsmsg, undefined);

    // Run all of the simulations sequentially.
    for (var i = 0; i < definitions.length; i++) {
        var definition = definitions[i];
        var instance = definition.instance;
        // Get the callback. Each simulation has its own callback closure, which encapsulates the simulation instance ID.
        var callback = getSimResultsCallback(definition.id);
        var callbackTable = getSimTablesCallback(definition.id);
        var startTime = performance.now()
        switch (simRuns.value_type) {
            case "Float":
                if (isJIT(model)) {
                    var callbackplottable = getNewPlottableCallback(definition.id);
                    var jit = getJIT(model, instance);
                    simulateFloatJIT(model, jit, callback, callbackplottable, sendExport, sendIG, cancel);
                }
                else
                    simulateFloat(ig, model.top.name, instance, callback, sendExport, cancel);
                break;
            case "MeanStdev":
                simulateMeanStdev(ig, model.top.name, instance, callback, sendExport, cancel);
                break;
            case "MeanStdevProbabilities":
                var statespace_callback = getStateSpaceCallback(definition.id);
                var probabilities = simulateMeanStdevProbabilities(ig, model.top.name, instance, statespace_callback, callback, sendExport, cancel);
                var instanceProbabilities = { instance: definition.instance, probabilities: probabilities };
                var probabilitiesMsg: Internals.WorkerResponse_Probabilities = { mtype: "probabilities", probabilities: instanceProbabilities };
                self.postMessage(probabilitiesMsg, undefined);
                break;
            case "Spatial1D":
                simulateSpatial1D(ig, model.top.name, instance, callback, sendExport, cancel);
                break;
            case "Spatial2D":
                simulateSpatial2D(ig, model.top.name, instance, callback, sendExport, cancel);
                break;
            case "MeanStdevTable":
                simulateMeanStdevTable(ig, model.top.name, instance, callbackTable, sendExport, cancel);
                break;
        }
        var endTime = performance.now()
        console.log("Simulation took " + (endTime - startTime) + " milliseconds");
    }
}

/** Handles a request to simulate a program string without expanding the instances. This is used as one instance of a multi-threaded simulation. */
function handleUserSimulateSingle(ig: Interfaces.IG, nodeId: string, definition: Interfaces.SimulationInstanceDefinition) {
    // Get all of the simulation run definitions. In this case, I really only need the result type.
    var simRuns = user_get_sim_runs(ig, nodeId);

    // Run the simulation.
    var instance = definition.instance;
    var callback = getSimResultsCallback(definition.id);
    var callbackTable = getSimTablesCallback(definition.id);
    var startTime = performance.now()
    switch (simRuns.value_type) {
        case "Float":
            let model = ig.nodes[nodeId];
            if (isJIT(model)) {
                var callbackplottable = getNewPlottableCallback(definition.id);
                var jit = getJIT(model, instance);
                simulateFloatJIT(model, jit, callback, callbackplottable, sendExport, sendIG, cancel);
            }
            else
                simulateFloat(ig, nodeId, instance, callback, sendExport, cancel);
            break;
        case "MeanStdev":
            simulateMeanStdev(ig, nodeId, instance, callback, sendExport, cancel);
            break;
        case "MeanStdevProbabilities":
            var statespace_callback = getStateSpaceCallback(definition.id);
            var probabilities = simulateMeanStdevProbabilities(ig, nodeId, instance, statespace_callback, callback, sendExport, cancel);
            var instanceProbabilities = { instance: definition.instance, probabilities: probabilities };
            var probabilitiesMsg: Internals.WorkerResponse_Probabilities = { mtype: "probabilities", probabilities: instanceProbabilities };
            self.postMessage(probabilitiesMsg, undefined);
            break;
        case "Spatial1D":
            simulateSpatial1D(ig, nodeId, instance, callback, sendExport, cancel);
            break;
        case "Spatial2D":
            simulateSpatial2D(ig, nodeId, instance, callback, sendExport, cancel);
            break;
        case "MeanStdevTable":
            simulateMeanStdevTable(ig, nodeId, instance, callbackTable, sendExport, cancel);
            break;
    }
    var endTime = performance.now()
    console.log("Simulation took " + (endTime - startTime) + " milliseconds");
}

/** Handles a request to "parse" and simulate a GUI CRN. This is single-threaded simulation. */
function handleUserSimulateGui(ig: Interfaces.IG, nodeId: string) {
    if (ig.task != null && ig.task.task_type == "Parse")
        throw "this model is designed for parsing only";
    runAllSimulations(ig, nodeId);
}

/** Provides the simulation instance definitions for the given CRN. This is in preparation for multi-threaded simulation. */
function sendSimRuns(ig: Interfaces.IG, nodeId: string) {
    var simRuns = user_get_sim_runs(ig, nodeId);
    var msg: Internals.WorkerResponse_SimType = { mtype: "simtype", simtype: simRuns.value_type };
    self.postMessage(msg, undefined);
    var definitions: Interfaces.SimulationInstanceDefinition[] = simRuns.instances.map((e, i) => { return { id: i, instance: e }; });
    var defsmsg: Internals.WorkerResponse_SimInstanceDefinitions = { mtype: "instancedefinitions", nodeId: nodeId, definitions: definitions };
    self.postMessage(defsmsg, undefined);
}

/** Handles a request to provide instances for simulation. This is in preparation for multi-threaded simulation. */
function handleGetSimRunsGui(ig: Interfaces.IG, nodeId: string) {
    if (ig.task != null && ig.task.task_type == "Parse")
        throw "this model is designed for parsing only";
    var model = ig.nodes[nodeId];
    var msg: Internals.WorkerResponse_Node = { mtype: "node", node: model };
    self.postMessage(msg, undefined);
    sendSimRuns(ig, nodeId);
}

/** Handles a request to generate the state space for the CRN. Depending on the CRN, this may require excessive amounts of resources, or even be infinite. */
function handleUserStateSpace(ig: Interfaces.IG, jit: boolean) {
    if (ig.task != null && ig.task.task_type == "Parse")
        throw "this model is designed for parsing only";
    // Retrieve the first model of the IG. Running state space analysis on an IG with more than one node is currently an undefined operation.
    let model = null;
    for (let nodeId in ig.nodes) {
        model = ig.nodes[nodeId];
        break;
    }
    // The following call may not complete, or it may run out of resources.
    if (jit) {
        var simRuns = user_get_sim_runs(ig, model.top.name);
        var instance = simRuns.instances[0];
        var jitobj = getJIT(model, instance);
        var ss = user_state_space_jit(jitobj);
        getStateSpaceCallback(0)(ss);
    }
    else {
        var ss = user_state_space(ig);
        getStateSpaceCallback(0)(ss);
    }
}

function handleGetProbabilityMap(probabilities: Interfaces.Probabilities, species: string, lowerBound: number) {
    var map: Interfaces.ProbabilityMap = getProbabilityMap(probabilities, species, lowerBound);
    var msg: Internals.WorkerResponse_ProbabilityMap = { mtype: "probabilitymap", map: map };
    self.postMessage(msg, undefined);
}

export function handleException(exc: any) {
    // This might be an F# Parser.Exception, or something that overrides Exception.Message.
    var serr = typeof exc.get_Message == "function" ? exc.get_Message() : exc.message == null ? ("" + exc) : exc.message;
    if (exc.errors != null) {
        var perr: Interfaces.ParsingError = { message: serr, positions: exc.errors };
        var msg: Internals.WorkerResponse_Error = { mtype: "error", error: perr };
        self.postMessage(msg, undefined);
    }
    else {
        var err: Interfaces.Error = { message: serr };
        var msg: Internals.WorkerResponse_Error = { mtype: "error", error: err };
        self.postMessage(msg, undefined);
    }
}

export function processMessage(e: any) {
    var req = <Internals.WorkerRequest>e.data;
    console.log("worker request: " + req.mtype);
    try {
        switch (req.mtype) {
            case "parsecode":
                handleUserParseCode((<Internals.WorkerRequest_ParseCode>req).code);
                break;
            case "generateexports":
                handleGenerateExports((<Internals.WorkerRequest_GenerateExports>req).model, (<Internals.WorkerRequest_GenerateExports>req).nodeId);
                break;
            case "generateexport":
                handleGenerateExport((<Internals.WorkerRequest_GenerateExport>req).model, (<Internals.WorkerRequest_GenerateExport>req).nodeId, (<Internals.WorkerRequest_GenerateExport>req).id);
                break;
            case "simulategui":
                handleUserSimulateGui((<Internals.WorkerRequest_SimulateGUI>req).model, (<Internals.WorkerRequest_SimulateGUI>req).nodeId);
                break;
            case "simulatesingle":
                handleUserSimulateSingle((<Internals.WorkerRequest_SimulateSingle>req).model, (<Internals.WorkerRequest_SimulateSingle>req).nodeId, (<Internals.WorkerRequest_SimulateSingle>req).definition);
                break;
            case "infergui":
                handleUserInferGui((<Internals.WorkerRequest_InferGUI>req).model);
                break;
            case "getsimrunsgui":
                handleGetSimRunsGui((<Internals.WorkerRequest_GetSimRunsGUI>req).model, (<Internals.WorkerRequest_GetSimRunsGUI>req).nodeId);
                break;
            case "statespace":
                handleUserStateSpace((<Internals.WorkerRequest_StateSpace>req).model, (<Internals.WorkerRequest_StateSpace>req).jit);
                break;
            case "getprobabilitymap":
                handleGetProbabilityMap((<Internals.WorkerRequest_GetProbabilityMap>req).probabilities, (<Internals.WorkerRequest_GetProbabilityMap>req).species, (<Internals.WorkerRequest_GetProbabilityMap>req).lowerBound);
                break;
            case "getcloudcapabilities":
                self.postMessage({ mtype: "pools", account: "", pools: [] }, undefined);
                break;
            case "synthesis":
                self.postMessage({ mtype: "error", error: { message: "Synthesis not available for run-in-browser" } }, undefined);
                break;
        }
        sendFinished();
    }
    catch (exc) {
        handleException(exc);
    }
}

/** Receives and executes a task coming from the main thread. */
function handleMessage(e: any) {
    // If the message queue is still available, it means that initialization is not complete yet. I need to set aside this message in the queue, to be processed after initialization is complete.
    if (messageQueue != null) {
        messageQueue.push(e);
        return;
    }
    config.processMessage(e);
};

self.addEventListener('message', handleMessage);