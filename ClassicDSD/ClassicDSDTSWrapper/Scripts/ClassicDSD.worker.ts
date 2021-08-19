// Import the generic Worker. Note that here I'm importing it as a regular module, rather than as a web worker constructor (i.e. via worker-loader).
import * as CRNEngineWorker from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine.worker";
import * as CRNInterfaces from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from "./Interfaces";

// Note that DSD has the concept of an unexpanded CRN versus an expanded CRN. "Parse" operations generate an unexpanded CRN, while "Compile" operations generate an expanded CRN. "Expand" operations generate an expanded CRN out of an unexpanded CRN. Because expand operations require data structures that cannot be serialized (a "bundle"), the outside-facing interface is different. A parse operation generates the bundle and the unexpanded CRN. The unexpanded CRN is returned to the user program, while the bundle is stored here in the web worker. The user can request an expansion of the bundle by invoking an expand operation on the same code that was used to generate the bundle. In this case, the provided code is ignored altogether, the bundle is expanded, and the resulting CRN is sent to the user. This allows me to skip the parsing phase.

// Configure the main worker.
CRNEngineWorker.config.loader = () => {
    var classicDSDJS = require("raw-loader!./../../ClassicDSDJS/Content/ClassicDSDJS.min.js");
    classicDSDJS = classicDSDJS.replace(/('|")use strict('|");?/, '');
    eval.call(null, classicDSDJS);
    user_parse = Microsoft.Research.DNA.JSONAPI.user_parse;
    user_parse_oldsyntax = Microsoft.Research.DNA.JSONAPI.user_parse_oldsyntax
    user_expand = Microsoft.Research.DNA.JSONAPI.user_expand;
    user_compile = Microsoft.Research.DNA.JSONAPI.user_compile;
    user_compile_oldsyntax = Microsoft.Research.DNA.JSONAPI.user_compile_oldsyntax
    is_jit = Microsoft.Research.DNA.JSAPI.is_jit;
    is_classic = Microsoft.Research.DNA.JSAPI.is_classic;
    get_jit_classic = Microsoft.Research.DNA.JSAPI.get_jit_classic;
    get_jit_rules = Microsoft.Research.DNA.JSAPI.get_jit_rules;
    set_options = Microsoft.Research.DNA.JSONAPI.set_options;
}
CRNEngineWorker.config.isJITfunc = isJIT;
CRNEngineWorker.config.getJITfunc = getJIT;
CRNEngineWorker.config.processMessage = processMessageDSD;

// Declare some shortcuts.
declare var Microsoft: any;
var user_parse: WGI.user_parse;
var user_parse_oldsyntax: WGI.user_parse_oldsyntax;
var user_expand: WGI.user_expand;
var user_compile: WGI.user_compile;
var user_compile_oldsyntax: WGI.user_compile_oldsyntax;
var is_jit: WGI.is_jit;
var is_classic: WGI.is_classic;
var get_jit_classic: WGI.get_jit_classic;
var get_jit_rules: WGI.get_jit_rules;
var set_options: WGI.set_options;

// The code that generated the currently cached bundle.
var currentCode: Interfaces.ParseObject = null;
// The cached result from the last parse request. Note that the type of the bundle is fully opaque. Only the F# code cares about the contents of the bundle.
var currentResult: WGI.Microsoft.Research.DNA.JSAPI.ParseResult = null;

function isJIT(model: CRNInterfaces.Model): boolean {
    return currentResult == null ? false : is_jit(currentResult.bundle);
}
function getJIT(model: CRNInterfaces.Model, instance: CRNInterfaces.SimulationInstance) {
    var jit = is_classic(currentResult.bundle) ? get_jit_classic(currentResult.bundle) : get_jit_rules(currentResult.bundle);
    return jit;
}

function getCrnCode(gui: CRNInterfaces.IG): string {
    var exportDef = CRNEngineWorker.user_get_export(gui, "", "code");
    return exportDef.content[0];
}

function handleUserCompile(code: Interfaces.ParseObject, oldSyntax: boolean) {
    // Invoke compilation on the given code.
    var gui: CRNInterfaces.Model = oldSyntax ? user_compile_oldsyntax(code) : user_compile(code);
    var ig = { nodes: { "": gui }, edges: {}, expanded: false };
    // Send the JIT flag (false, in this case, because this includes expansion).
    self.postMessage({ mtype: "dsd.isjit", isJIT: false }, undefined);
    var crnCode: string = getCrnCode(ig);
    self.postMessage({ mtype: "dsd.parseresult", result: { crn: gui, settings: currentResult.settings, code: crnCode } }, undefined);
}

function isCurrent(code: Interfaces.ParseObject): boolean {
    return currentCode != null && code.code == currentCode.code && code.specificities == currentCode.specificities && code.toeholds == currentCode.toeholds;
}

/** Parses code and caches the resulting bundle. Returns the unexpanded CRN. */
function runParseAndCache(code: Interfaces.ParseObject, oldSyntax: boolean): CRNInterfaces.IG {
    // Clear the current cache. I do this at this point in case parsing throws an exception.
    currentCode = null;
    currentResult = null;
    // Run parse; if successful, cache the result.
    currentResult = oldSyntax ? user_parse_oldsyntax(code) : user_parse(code);
    currentCode = code;
    return currentResult.unexpanded;
}

function handleUserParse(code: Interfaces.ParseObject, oldSyntax: boolean) {
    // Parse, caching the bundle.
    var model: CRNInterfaces.IG = runParseAndCache(code, oldSyntax);
    // Send the JIT flag.
    self.postMessage({ mtype: "dsd.isjit", isJIT: is_jit(currentResult.bundle) }, undefined);
    let crnCode = getCrnCode(model);
    self.postMessage({ mtype: "dsd.parseresult", result: { model: model, settings: currentResult.settings, code: crnCode } }, undefined);
}

function arraysAreEqual(a: string[], b: string[]): boolean {
    if (a.length != b.length)
        return false;
    for (var i = 0; i < a.length; i++)
        if (a[i] != b[i])
            return false;
    return true;
}

function handleUserExpand(code: Interfaces.ParseObject, userIG: CRNInterfaces.IG, options: Interfaces.DsdSettings, oldSyntax: boolean) {
    // If the request for expansion is dealing with a different code than the one that generated the currently cached bundle,
    // or if no bundle is currently cached, I'll need to run parse at this point.
    if (!isCurrent(code) || currentResult == null)
        runParseAndCache(code, oldSyntax);
    // Replace the DSD settings with the provided DSD settings, except for the rules program.
    if (currentResult.settings.rulesProgram !== undefined)
        options.rulesProgram = currentResult.settings.rulesProgram;
    currentResult = set_options(currentResult, options);
    // At this point, I am guaranteed to have the correct bundle. I will run expansion on it.
    var ig: CRNInterfaces.IG = is_jit(currentResult.bundle) ? currentResult.unexpanded : { nodes: { "": user_expand(currentResult.bundle) }, edges: {}, expanded: false };
    if (userIG.task != null)
        ig.task = userIG.task;
    var model: CRNInterfaces.Model = null;
    for (var n in ig.nodes) {
        model = ig.nodes[n];
        break;
    }
    // Replace the settings with the ones from the original model. This won't work with multi-CRN models.
    // If the user-modified plots are unmodified or empty, then I should use the plots from the result. Otherwise, I should use the user-modified plots.
    var userModel: CRNInterfaces.Model = null;
    for (var n in userIG.nodes) {
        userModel = userIG.nodes[n];
        break;
    }
    var unexpandedModel: CRNInterfaces.Model = null;
    for (var n in currentResult.unexpanded.nodes) {
        unexpandedModel = currentResult.unexpanded.nodes[n];
        break;
    }
    var userPlots = userModel.top.settings.simulation.plots;
    var unexpandedPlots = unexpandedModel.top.settings.simulation.plots;
    var modified = !arraysAreEqual(userPlots, unexpandedPlots) && userPlots.length > 0;
    if (!modified) {
        userModel.top.settings.simulation.plots = model.top.settings.simulation.plots;
        for (let i in userModel.top.settings.simulations)
            userModel.top.settings.simulations[i].plots = model.top.settings.simulations[i].plots;
    }
    model.top.settings = userModel.top.settings;
    // Send the JIT flag.
    self.postMessage({ mtype: "dsd.isjit", isJIT: is_jit(currentResult.bundle) }, undefined);
    let crnCode = getCrnCode(ig);
    self.postMessage({ mtype: "dsd.parseresult", result: { model: ig, settings: currentResult.settings, code: crnCode } }, undefined);
    // Note that at this point I still have the original bundle. If, for whatever reason, the user wants to run expansion again, I will not need to run parse again.
}

/** Processes a task coming from the main thread. Calculus-specific tasks will be handled here, CRN generic tasks will be passed
to the processMessage function in CRNEngine/Worker. */
function processMessageDSD(e: any) {
    var req = <Interfaces.WorkerRequest>e.data;
    console.log("worker request: " + req.mtype);
    try {
        switch (req.mtype) {
            case "dsd.compile":
                handleUserCompile((<Interfaces.WorkerRequest_Compile>req).code, (<Interfaces.WorkerRequest_Compile>req).oldSyntax);
                break;
            case "dsd.parse":
                handleUserParse((<Interfaces.WorkerRequest_Parse>req).code, (<Interfaces.WorkerRequest_Parse>req).oldSyntax);
                break;
            case "dsd.expand":
                handleUserExpand((<Interfaces.WorkerRequest_Expand>req).code, (<Interfaces.WorkerRequest_Expand>req).model, (<Interfaces.WorkerRequest_Expand>req).settings, (<Interfaces.WorkerRequest_Expand>req).oldSyntax);
                break;
            default:
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