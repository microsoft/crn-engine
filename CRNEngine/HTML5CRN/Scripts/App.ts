/*import { ServiceWorker } from './Scripts/ServiceWorker';
let serviceWorker = new ServiceWorker();*/

import * as $ from 'jquery';
//import "../../../node_modules/jqueryui/jquery-ui.min.css"
import "../../../node_modules/jquery-ui/themes/base/all.css"
import "../../../node_modules/katex/dist/katex.min.css"
import "../../../HTML5SharedGUI/KnockoutGrid/table.css"
// We override IDD styles with General.css, so we require IDD styles to be loaded prior to General.css.
import "../../../node_modules/interactive-data-display/dist/idd.css"
import "../../../HTML5SharedGUI/CodeEditor/Styles/codepad.css"
import "../../../HTML5SharedGUI/SimulationViewer/Styles/simulation.css"
import "../../../HTML5SharedGUI/InferenceViewer/Styles/inference.css"
import "../../../HTML5SharedGUI/CRNComponent/Styles/crn.css"
import "idd";
declare var InteractiveDataDisplay: any;
import * as mousetrap from 'mousetrap';
import * as Utils from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Utils';
import * as CRNvm from '../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';
import * as CodeEditor from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNCodeEditor';
import * as CodeStorage from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/CodeEditorStorageDecorator';
import * as CodeParser from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineCRNParser';
import * as CrnEditor from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNViewer';
import * as CrnExport from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNExport';
import * as SimRunner from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSimulationRunner';
import * as SimRunnerSpat1D from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSpatial1DSimulationRunner';
import * as SimRunnerSpat2D from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSpatial2DSimulationRunner';
import * as InferenceRunner from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineInferenceRunner';
import { Adapter as SSARunner } from "../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSSA";
import { Viewer as SSASummaryViewer } from "../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedSSASummaryViewer";
import * as SimViewer from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/SimViewerAdapter';
import { StoredSettings as SpatialViewerSettings } from "../../../HTML5SharedGUI/SimulationViewer/Scripts/SpatialViewerSettings";
import SpatialViewer1D from "../../../HTML5SharedGUI/SimulationViewer/Scripts/Spatial1DViewer";
import SpatialViewer2D from "../../../HTML5SharedGUI/SimulationViewer/Scripts/Spatial2DViewer";
import * as HintScreen from '../../../HTML5SharedGUI/GenericComponents/Scripts/HintScreen';
import * as ModificationIndicator from '../../../HTML5SharedGUI/GenericComponents/Scripts/ModificationIndicators';
import * as SVF from '../../../HTML5SharedGUI/SimulationViewer/Scripts/SimulationViewerFramework';
import CRNEngine from './../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as InferenceParametersViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/InferredParametersViewer';
import * as InferenceModelDynamicsViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/ModelDataDynamics';
import * as InferencePosteriorViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/PosteriorViewer';
import * as InferenceSummary from '../../../HTML5SharedGUI/InferenceViewer/Scripts/InferenceSummary';
import * as InferenceCompositeViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/CompositeViewer';
import { Viewer as SSATextViewer } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/SSATextViewer';
import SSAGraphAdapter from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/SSAGraphAdapter';
import { Viewer as SSACompoundViewer } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/SSACompoundViewer';
import CRNViewer from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/CRNCompoundViewer';
import CRNGraphAdapter from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/CRNGraphAdapter';
import InferenceGraphAdapter from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/InferenceGraphAdapter';
import SynthesisAdapter from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSynthesis';
import CRNSelector from '../../../HTML5SharedGUI/CRNComponent/Scripts/CRNSelector';
import Options from '../../../HTML5SharedGUI/GenericComponents/Scripts/Options';
import "../../../HTML5SharedGUI/GenericComponents/Scripts/Dropdown";
import * as SumToVum from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/SumToVum';
import { Viewer as PMViewer } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ProbabilityMapsAdapter';
import { SynthesisViewer } from '../../../HTML5SharedGUI/SimulationViewer/Scripts/SynthesisViewer';

import * as LongOperations from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/LongOperations';
import * as LongOperationsKO from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/LongOperationsKO';
import * as ParseOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/ParseCodeFillCRN';
import * as ExportOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/ModellingEngineExporter';
import * as SimulateOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/SimulateParsedCRN';
import * as Spatial1DSimulation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Spatial1DSimulation';
import * as Spatial2DSimulation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Spatial2DSimulation';
import { SimulationType, Operation as DispatchedSimulateOperation } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/AutoChoiceSimulation';
import * as InferOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Inference';
import * as TabSelectOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/TabSelect';
import { Operation as SSAOperation } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/StateSpaceAnalysis';
import { Operation as SynthesisOperation } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Synthesis';

import { KnockoutBasedDataSetsList as DataSetList } from '../../../HTML5SharedGUI/CRNComponent/Scripts/crnDataSets';
import { KnockoutGridDataSetViewer as DataSetViewer } from '../../../HTML5SharedGUI/CRNComponent/Scripts/crnDataSets';
import { ObservationsSource } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/MultipleDataSetObsSource';
import { MemoryDataSetStorage, LocalStorageDataSetStorage, IndexedDBDataSetStorage } from '../../../HTML5SharedGUI/CRNComponent/Scripts/DataSetStorage';
import IDDTabs from "../../../HTML5SharedGUI/GenericComponents/Scripts/IDDTabs";

//import { CRNSettingsVM } from '../../../HTML5SharedGUI/CRNComponent/Scripts/src/Scripts/crnSettings';
//Phase 1. Creating the functional components
let dataSetStorage = (() => {
    var ret: IndexedDBDataSetStorage | MemoryDataSetStorage = null;
    if (window.indexedDB !== undefined)
        ret = new IndexedDBDataSetStorage("CRNdb");
    else
        ret = new MemoryDataSetStorage(); //e.g. Edge InPrivate
    return ret;
})();
var dataSetList = new DataSetList(dataSetStorage);
var longOperationsManager = new LongOperationsKO.LongOperationsKO();
var codeModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-code-tab-header"));
var crnModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-crn-tab-header"));
var crnEditor: CrnEditor.CRNEditor<void> = new CrnEditor.CRNEditor(dataSetList, crnModificationIndicator);
var crnVM: CRNvm.InferenceGraph = crnEditor.GetVM();
var crnEngine = new CRNEngine(Options.Server());
var options = new Options(crnEngine);
var codeSource = new CodeEditor.KOCRNCodeEditor(codeModificationIndicator);
var codeStorage = new CodeStorage.CodeEditorStorageDecorator(codeSource, "CRN");
var crnGraphViewer = new CRNGraphAdapter<void>(crnVM);
var inferenceGraphViewer = new InferenceGraphAdapter<void>(crnVM);
var synthesisAdapter = new SynthesisAdapter(crnEngine);
var crnSelector = new CRNSelector(crnVM);
var crnExport = new CrnExport.CRNExport(crnEngine);
var codeParser = new CodeParser.Parser(crnEngine);
var currentlySelectedDataSetSource = new ObservationsSource(dataSetStorage);
var infRunner = new InferenceRunner.Adapter(crnEngine, currentlySelectedDataSetSource);
var ssaRunner = new SSARunner(crnEngine);
var ssaSummaryViewer = new SSASummaryViewer();
var ssaGraphViewer = new SSAGraphAdapter(crnVM);
var simViewer = new SimViewer.Viewer();
var spatialViewerSettings = new SpatialViewerSettings("black,green");
var spatialViewer1D = new SpatialViewer1D(spatialViewerSettings);
var spatialViewer2D = new SpatialViewer2D(spatialViewerSettings);
var dataSetViewer = new DataSetViewer();
var probabilityMapsViewer = new PMViewer(crnEngine, spatialViewerSettings);
var simRunner = new SimRunner.SimulationRunner(crnEngine, currentlySelectedDataSetSource);
var spat1DSimRunner = new SimRunnerSpat1D.SimulationRunner(crnEngine);
var spat2DSimRunner = new SimRunnerSpat2D.SimulationRunner(crnEngine);

//var crnSettings = new CRNSettingsVM();
//Viewers:
var infParamViewer = new InferenceParametersViewer.InferredParametersViewer(crnSelector);
var infDynamicsViewer = new InferenceModelDynamicsViewer.ModelDataDynamics(crnSelector);
var infPosteriorViewer = new InferencePosteriorViewer.PosteriorViewer(crnSelector);
var infSummary = new InferenceSummary.InferenceSummary(crnSelector);
var infViewer = new InferenceCompositeViewer.Viewer([infParamViewer, infDynamicsViewer, infSummary, infPosteriorViewer]);
var ssaTextViewer = new SSATextViewer();
var ctmcCompoundViewer = new SSACompoundViewer(ssaGraphViewer, ssaTextViewer, ssaSummaryViewer);
var crnCompoundViewer = new CRNViewer(crnEditor);
var synthesisViewer = new SynthesisViewer(crnEngine);

var unitsExtractor = function (): SVF.IVisualizationUpdateMessage {
    return {
        MessageType: SVF.VisualizationUpdateMessageType.UnitsInformation,
        EncapsulatedUpdate: Utils.ExponentToConcentrationString(crnEditor.GetConcentrationUnits())
    }
};
var plotSettingsExtractor = function (): SVF.IVisualizationUpdateMessage {
    return {
        MessageType: SVF.VisualizationUpdateMessageType.PlotSettingsInfo,
        EncapsulatedUpdate: crnEditor.GetPlotSettings()
    }
};

var plotSettingsExtractorForInference = function (): InferOperation.IPlotSettingsValues {
    return crnEditor.GetPlotSettings();
};

// Reset the CRN when a new file is loaded in the editor.
codeSource.EditorLoadEvents.subscribe(next => {
    crnCompoundViewer.UpdateValuesWith(null, null, false);
    //crnExport.Reset();
});

var currentCRNSimulationType = function () {
    var ig = crnEditor.getModel();
    var model = null;
    for (var k in ig.nodes) {
        model = ig.nodes[k];
        break;
    }
    if (model.top.settings.simulator != "PDE")
        return SimulationType.nonspatial;
    if (model.top.settings.spatial.dimensions == 1)
        return SimulationType.spatial1D;
    if (model.top.settings.spatial.dimensions == 2)
        return SimulationType.spatial2D;
    throw "unsupported operation type";
}

//Phase 2. Assembling higher level functionality from the components above

var parseOperation = new ParseOperation.Operation<void, void>(codeStorage, codeParser, crnCompoundViewer, codeSource);
var exportOperation = new ExportOperation.Exporter(crnEngine, crnEditor, crnSelector, crnExport);
var simulateOperation = new SimulateOperation.Operation<SimRunner.ISimulationMessage, SVF.IVisualizationUpdateMessage, void>(crnEditor, crnSelector, simRunner, simViewer, ctmcCompoundViewer, probabilityMapsViewer, crnCompoundViewer, crnExport, unitsExtractor, plotSettingsExtractor, SumToVum.getModelfromSim, SumToVum.getExportsFromSim, SumToVum.getStateSpaceFromSim, SumToVum.Convert);
var simulateSpatial1DOperation = new Spatial1DSimulation.Operation(crnEditor, crnSelector, spat1DSimRunner, spatialViewer1D);
var simulateSpatial2DOperation = new Spatial2DSimulation.Operation(crnEditor, crnSelector, spat2DSimRunner, spatialViewer2D);

var autoChoiceSimulationOperation = new DispatchedSimulateOperation(
    currentCRNSimulationType,
    simulateSpatial1DOperation, $('#spatial1dViewer'),
    simulateSpatial2DOperation, $('#spatial2dViewer'),
    simulateOperation, $('#simulationViewer'));

var inferOperation = new InferOperation.Operation(crnEditor, currentlySelectedDataSetSource, infRunner, infViewer, crnExport, plotSettingsExtractorForInference);
var synthesisOperation = new SynthesisOperation(crnEditor, synthesisAdapter, synthesisViewer);
var ssaOperation = new SSAOperation(crnEditor, ssaRunner, ctmcCompoundViewer, false);

var selectCRNDirectivesTab = new TabSelectOperation.Operation($('#inputTabs'), "#crnDirectives");
var selectInferTab = new TabSelectOperation.Operation($('#resultsTabs'), "#inferenceViewer");
var selectModelDynamicsTab = new TabSelectOperation.Operation($('#inferenceViewer'), "#inferenceDynamics");
var selectSimulationTab = new TabSelectOperation.Operation($('#resultsTabs'), "#simulationViewerTab");
//var selectSimulationViewerTab = new TabSelectOperation.Operation($('#simulationViewerTab'), "#simulationViewerTab");
var selectSSATab = new TabSelectOperation.Operation($('#resultsTabs'), "#StatesViewerTab");
var selectSSATextTab = new TabSelectOperation.Operation($('#StatesViewerTab'), "#ssaTextTab");
var selectSynthesisTab = new TabSelectOperation.Operation($('#resultsTabs'), "#synthesisTab");

//var simHintScreen = new HintScreen.HintScreen(autoChoiceSimulationOperation);
//var graphHintScreen = new HintScreen.HintScreen(parseOperation);


//Phase 3. Setting up the layout
// Turn the main areas into tabs.
IDDTabs();
var codeTabIdx = $("#inputTabs").find("#crnCode").index() - 1;
$("#inputTabs").tabs("option", "active", codeTabIdx);

function parseOnLeavingCodeTab() {
    if (longOperationsManager.CanStartNewAction())
        longOperationsManager.EnqueueOperations([parseOperation]);
    else {
        if (confirm("Cannot parse while an operation is in progress. Press OK to abort the operation and parse, or press Cancel to revert your code changes.")) {
            longOperationsManager.Stop();
            setTimeout(() => longOperationsManager.EnqueueOperations([parseOperation]));
        }
        else {
            codeSource.ResetToUnmodified();
            $("#inputTabs").tabs("option", "active", codeTabIdx);
        }
    }
}

$("#inputTabs").on("tabsactivate", (event, ui) => {
    if (ui.oldPanel[0] == $("#crnCode")[0] && codeModificationIndicator.getIsSet())
        parseOnLeavingCodeTab();
});

//bind functional components
codeSource.AutoBind();
crnEditor.Bind($("#inputTabs")[0]);
crnGraphViewer.Bind(document.getElementById('crnGraph'));
inferenceGraphViewer.Bind(document.getElementById('inferenceGraph'));
var exportAreas = $(".c-export");
for (var c = 0; c < exportAreas.length; c++)
    crnExport.Bind(exportAreas[c]);
crnExport.BindToTabs(document.getElementById("crnExport"));
dataSetList.Bind($("#datasets-viewer")[0]);
dataSetViewer.Bind($("#dataset-viewer")[0]);
//simHintScreen.Bind(document.getElementById('simulationHintScreen'));
//graphHintScreen.Bind(document.getElementById('graphHintScreen'));
simViewer.Bind(document.getElementById('simulationViewer'));
spatialViewer1D.bind(document.getElementById('spatial1dViewer'));
spatialViewer2D.bind(document.getElementById('spatial2dViewer'));
ssaSummaryViewer.Bind(document.getElementById('ssaSummary'));
infDynamicsViewer.Bind(document.getElementById('inferenceDynamics'));
infParamViewer.Bind(document.getElementById('inferenceParameters'));
infPosteriorViewer.Bind(document.getElementById('inferencePosterior'));
infSummary.Bind(document.getElementById('inferenceSummary'));
ssaTextViewer.Bind(document.getElementById('ssaTextTab'));
probabilityMapsViewer.Bind(document.getElementById('probabilitiesTab'));
ssaGraphViewer.Bind(document.getElementById('ssaGraph'));
synthesisViewer.Bind(document.getElementById('synthesisTab'));
longOperationsManager.Bind(document.getElementById('toolbar'));
crnSelector.Bind(document.getElementById('CRNSelector'));
ko.applyBindings(crnVM, document.getElementById('crnInferenceGraphTab'));
options.bind(document.getElementById('options'));

dataSetList.getSelectedObservable().subscribe((val) => {
    val ? dataSetViewer.Show(val) : dataSetViewer.Show(null);
});

var files = ["TestInference.txt", "Join_Simulated.txt", "Join_data.csv", "AM_obs.csv", "AM_obs_noised.csv",
    "Rep_Simulated.csv"];
dataSetList.Initialize("Examples/Observations/", files);

dataSetStorage.GetNames().done((names: any) => {
    if (!names.length)
        files.forEach((val) => {
            var url = "Examples/Observations/" + val;
            var oReq = new XMLHttpRequest();
            oReq.onload = () => {
                if (oReq.status % 100 == 4)
                    console.log("Warning: attempting to load " + url + " resulted in status " + oReq.status + ".");
                else
                    dataSetList.LoadFromString(oReq.responseText, val);
            }
            oReq.open("GET", url);
            oReq.send();
        });
});

function getActiveTabID(tabName: string) {
    return $("#" + tabName + " .ui-tabs-panel:visible").attr("id");
}
function getActiveInputTabID() {
    return getActiveTabID("inputTabs");
}

$("#separator").draggable({
    axis: "x",
    containment: "parent",
    scroll: false,
    drag: (event, ui) => {
        var pos = ui.position.left;
        var percLeft = 100 * pos / ui.helper.parent().width();
        var percRight = 100 - percLeft;
        $("#input-area").css({ width: "calc(" + percLeft + "% - 5px)" });
        $("#output-area").css({ width: "calc(" + percRight + "% - 15px)" });
        InteractiveDataDisplay.updateLayouts($("#output-area"));
    },
    stop: (event, ui) => {
        let left = 100 * ui.position.left / ui.helper.parent().width();
        $("#separator").css({ left: left + "%" });
    }
});

$("#dataset-separator").draggable({
    axis: "y",
    containment: "parent",
    cursorAt: { top: 4 },
    scroll: false,
    drag: function (event, ui) {
        var pos = ui.position.top;
        var percTop = 100 * pos / ui.helper.parent().height();
        var percBottom = 100 - percTop;
        $("#datasets-viewer").css({ height: "calc(" + percTop + "% - 4px)" });
        $("#dataset-viewer").css({ height: "calc(" + percBottom + "% - 4px)" });
    }
});
$("#dataset-separator").css({ position: "static" }); // jqueryui draggable sets position to relative automatically

//Phase 4. Setting up button handlers
function exportCommand() {
    if (!longOperationsManager.CanStartNewAction())
        return;
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "crnData":
        case "crnCode"://code
            operations = [parseOperation, exportOperation];
            break;
        case "crnDirectives"://directives
        case "crnParameters"://parameters
        case "crnSpecies"://species
        case "crnReactions"://reactions
        case "crnInferenceGraph"://inference graph
            operations = [exportOperation];
            break;
        default:
            throw "not implemented yet";
    }
    longOperationsManager.EnqueueOperations(operations);
}

function simulateCommand() {
    if (!longOperationsManager.CanStartNewAction())
        return;
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "crnData":
        case "crnCode"://code
            operations = [parseOperation, exportOperation, selectSimulationTab, autoChoiceSimulationOperation];
            break;
        case "crnDirectives"://directives
        case "crnParameters"://parameters
        case "crnSpecies"://species
        case "crnReactions"://reactions
        case "crnInferenceGraph"://inference graph
            operations = [exportOperation, selectSimulationTab, autoChoiceSimulationOperation];
            break;
        default:
            throw "not implemented yet";
    }
    longOperationsManager.EnqueueOperations(operations);
}

function statesCommand() {
    if (!longOperationsManager.CanStartNewAction())
        return;
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "crnData":
        case "crnCode"://code
            operations = [parseOperation, exportOperation, selectSSATab, selectSSATextTab, ssaOperation];
            break;
        case "crnDirectives"://directives
        case "crnParameters"://parameters
        case "crnSpecies"://species
        case "crnReactions"://reactions
        case "crnInferenceGraph"://inference graph
            operations = [selectSSATextTab, selectSSATab, ssaOperation];
            break;
        default:
            throw "not implemented yet";
    }
    longOperationsManager.EnqueueOperations(operations);
}

function synthesisCommand() {
    if (!longOperationsManager.CanStartNewAction())
        return;
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "crnData":
        case "crnCode"://code
            operations = [parseOperation, exportOperation, selectSynthesisTab, synthesisOperation];
            break;
        case "crnDirectives"://directives
        case "crnParameters"://parameters
        case "crnSpecies"://species
        case "crnReactions"://reactions
        case "crnInferenceGraph"://inference graph
            operations = [selectSynthesisTab, synthesisOperation];
            break;
        default:
            throw "not implemented yet";
    }
    longOperationsManager.EnqueueOperations(operations);
}

function inferCommand() {
    if (!longOperationsManager.CanStartNewAction())
        return;
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "crnData":
        case "crnCode"://code            
            operations = [parseOperation, exportOperation, selectInferTab, selectModelDynamicsTab, inferOperation];
            break;
        case "crnDirectives"://directives
        case "crnParameters"://parameters
        case "crnSpecies"://species
        case "crnReactions"://reactions
        case "crnInferenceGraph"://inference graph
            operations = [selectInferTab, selectModelDynamicsTab, inferOperation];
            break;
        default:
            throw "not implemented yet";
    }
    longOperationsManager.EnqueueOperations(operations);
}

function stopCommand() {
    if (!longOperationsManager.CanStopOngoingAction())
        return;
    longOperationsManager.Stop();
}

document.getElementById('exportButton').onclick = exportCommand;
document.getElementById('simulateButton').onclick = simulateCommand;
document.getElementById('statesButton').onclick = statesCommand;
document.getElementById('synthesisButton').onclick = synthesisCommand;
document.getElementById('inferButton').onclick = inferCommand;
document.getElementById('stopButton').onclick = stopCommand;

/* FP: removing these; mousetrap works, but finding shortcuts that make sense and aren't already used by any browser or Monaco is harder than I thought. If shortcuts are required, we can use mousetraps, but we need to plan them carefully.

function selectTab(tabControlID: string, tabID: string) {
    var tabControl = $('#' + tabControlID);
    var idx = tabControl.find('#' + tabID).index() - 1;
    tabControl.tabs("option", "active", idx)
}
function mousetrapPreventDefault(f: () => void): (e: any) => boolean {
    return function (e) {
        f();
        return false;
    }
}
mousetrap.prototype.stopCallback = function (e: any, element: HTMLElement, combo: any) { return false; }
mousetrap.bind('ctrl+e', mousetrapPreventDefault(exportCommand));
mousetrap.bind('ctrl+s', mousetrapPreventDefault(simulateCommand));
mousetrap.bind('ctrl+d', mousetrapPreventDefault(statesCommand));
mousetrap.bind('ctrl+f', mousetrapPreventDefault(inferCommand));
mousetrap.bind('ctrl+x', mousetrapPreventDefault(stopCommand));*/

//serviceWorker.Bind(document.getElementById('worker'));

Utils.RemoveLoadingOverlay();

console.log("HTML5CRN loaded");