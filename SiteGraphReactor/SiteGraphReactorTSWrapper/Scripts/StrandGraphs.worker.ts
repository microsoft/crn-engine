// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Import the generic Worker. Note that here I'm importing it as a regular module, rather than as a web worker constructor (i.e. via worker-loader).
import * as CRNEngineWorker from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine.worker";
import * as CRNInterfaces from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from "./Interfaces";

// Configure the main worker.
CRNEngineWorker.config.loader = () => {
    require("script-loader!./../../SiteGraphCRNJS/Content/SiteGraphCRNJS.js");
    user_parse = SiteGraphReactor.JSONAPI.user_parse;
    user_expand = SiteGraphReactor.JSONAPI.user_expand;
    user_compile = SiteGraphReactor.JSONAPI.user_compile;
}
CRNEngineWorker.config.processMessage = processMessageStrandGraphs;

// Declare some shortcuts.
declare var SiteGraphReactor: any;
var user_parse: WGI.user_parse;
var user_expand: WGI.user_expand;
var user_compile: WGI.user_compile;

function handleUserCompile(code: string) {
    // Invoke compilation on the given code.
    var gui: CRNInterfaces.CRN = user_compile(code);
    var model: CRNInterfaces.Model = { top: gui, systems: [] };
    var ig: CRNInterfaces.IG = { nodes: { "": model }, edges: {}, expanded: false };
    var crncode = CRNEngineWorker.user_get_export(ig, "", "code").content;
    self.postMessage({ mtype: "sg.parseresult", result: { crn: gui, code: crncode } }, undefined);
}

// The code that generated the currently cached bundle.
var currentCode: string = null;
// The cached bundle from the last parse request. Note that the type is fully opaque. Only the F# code cares about
// the contents of the bundle.
var currentBundle: any = null;

/** Parses code and caches the resulting bundle. Returns the unexpanded CRN. */
function runParseAndCache(code: string): CRNInterfaces.CRN {
    // Clear the current cache. I do this at this point in case parsing throws an exception.
    currentCode = null;
    currentBundle = null;
    // Run parse.
    var result: WGI.SiteGraphReactor.JSAPI.ParseResult = user_parse(code);
    // Cache the bundle.
    currentCode = code;
    currentBundle = result.bundle;
    return result.unexpanded;
}

function handleUserParse(code: string) {
    // Parse, caching the bundle.
    var gui = runParseAndCache(code);
    var model: CRNInterfaces.Model = { top: gui, systems: [] };
    var ig: CRNInterfaces.IG = { nodes: { "": model }, edges: {}, expanded: false };
    var crncode = CRNEngineWorker.user_get_export(ig, "", "code").content;
    self.postMessage({ mtype: "sg.parseresult", result: { model: model, code: crncode } }, undefined);
}

function handleUserExpand(code: string) {
    // If the request for expansion is dealing with a different code than the one that generated the currently cached bundle,
    // or if no bundle is currently cached, I'll need to run parse at this point.
    if (code != currentCode || currentBundle == null)
        runParseAndCache(code);
    // At this point, I am guaranteed to have the correct bundle. I will run expansion on it.
    var gui: CRNInterfaces.CRN = user_expand(currentBundle);
    var model: CRNInterfaces.Model = { top: gui, systems: [] };
    var ig: CRNInterfaces.IG = { nodes: { "": model }, edges: {}, expanded: false };
    var crncode = CRNEngineWorker.user_get_export(ig, "", "code").content;
    self.postMessage({ mtype: "sg.parseresult", result: { model: model, code: crncode } }, undefined);
    // Note that at this point I still have the original bundle. If, for whatever reason, the user wants to run
    // expansion again, I will not need to run parse again.
}

/** Processes a task coming from the main thread. Calculus-specific tasks will be handled here, CRN generic tasks will be passed
to the processMessage function in CRNEngine/Worker. */
function processMessageStrandGraphs(e: any) {
    var req = <Interfaces.WorkerRequest>e.data;
    console.log("worker request: " + req.mtype);
    try {
        if (req.mtype == "sg.compile")
            handleUserCompile((<Interfaces.WorkerRequest_Compile>req).code);
        else if (req.mtype == "sg.parse")
            handleUserParse((<Interfaces.WorkerRequest_Parse>req).code);
        else if (req.mtype == "sg.expand")
            handleUserExpand((<Interfaces.WorkerRequest_Expand>req).code);
        else {
            // Let the generic worker handle this. Note that it will also send the finished signal, so I don't need to.
            CRNEngineWorker.processMessage(e);
            return;
        }
        CRNEngineWorker.sendFinished();
    }
    catch (exc) {
        CRNEngineWorker.handleException(exc);
    }
};