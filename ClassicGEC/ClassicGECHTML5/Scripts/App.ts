// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/*import { ServiceWorker } from './Scripts/ServiceWorker';
let serviceWorker = new ServiceWorker();*/

import "jqueryui";
import * as $ from 'jquery';
import "../Styles/App.css";
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
import * as visbol from '../../../node_modules/visbol/index.js';
import * as visbolfont from '../../../node_modules/visbol/font/sbolv/main.js';
import * as Utils from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Utils';
import "pathseg";
import { jsbolToObject } from "../../ClassicGECTSWrapper/Scripts/JSBOL";

// import convertJsonToXml from './json2xml';
import GECLanguage from './GECLang';
import PartsLanguage from './PartsLang';
import ReactionsLanguage from './ReactionsLang';
import Options from '../../../HTML5SharedGUI/GenericComponents/Scripts/Options';
import "../../../HTML5SharedGUI/GenericComponents/Scripts/Dropdown";
import * as CodeEditor from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNCodeEditor';
import { CodeEditor as GECCodeEditor } from './Adapters/GECCodeSource';
import { CodeEditor as PartsDBCodeEditor } from './Adapters/PartsDBCodeSource';
import { CodeEditor as ReactionsDBCodeEditor } from './Adapters/ReactionsDBCodeSource';
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
import CRNEngine from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import ClassicGEC from '../../ClassicGECTSWrapper/Scripts/ClassicGEC';
import * as InferenceParametersViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/InferredParametersViewer';
import * as InferenceModelDynamicsViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/ModelDataDynamics';
import * as InferencePosteriorViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/PosteriorViewer';
import * as InferenceSummary from '../../../HTML5SharedGUI/InferenceViewer/Scripts/InferenceSummary';
import * as InferenceCompositeViewer from '../../../HTML5SharedGUI/InferenceViewer/Scripts/CompositeViewer';
import { Viewer as SSATextViewer } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/SSATextViewer';
import { Viewer as SSACompoundViewer } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/SSACompoundViewer';
import * as SumToVum from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/SumToVum';
import { Viewer as PMViewer } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ProbabilityMapsAdapter';
import SSAGraphViewer from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/SSAGraphAdapter';
import CRNCompoundViewer from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/CRNCompoundViewer';
import CRNGraphViewer from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/CRNGraphAdapter';
import * as LongOperations from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/LongOperations';
import * as LongOperationsKO from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/LongOperationsKO';
import * as ParseOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/ParseCodeFillCRN';
import * as ExportOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/ModellingEngineExporter';
import * as GecParseOperation from './Operations/GecParseOperation';
import * as GetSolutionOperation from './Operations/GetSolutionOperation';
import * as SimulateOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/SimulateParsedCRN';
import * as Spatial1DSimulation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Spatial1DSimulation';
import * as Spatial2DSimulation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Spatial2DSimulation';
import { SimulationType, Operation as DispatchedSimulateOperation } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/AutoChoiceSimulation';
import * as InferOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Inference';
import * as TabSelectOperation from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/TabSelect';
import { Operation as SSAOperation } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/StateSpaceAnalysis';
import { Operation as SynthesisOperation } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Synthesis';
import CRNSelector from '../../../HTML5SharedGUI/CRNComponent/Scripts/CRNSelector';
import InferenceGraphAdapter from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/InferenceGraphAdapter';
import SynthesisAdapter from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSynthesis';
import { SynthesisViewer } from '../../../HTML5SharedGUI/SimulationViewer/Scripts/SynthesisViewer';

import { KnockoutBasedDataSetsList as DataSetList } from '../../../HTML5SharedGUI/CRNComponent/Scripts/crnDataSets';
import { KnockoutGridDataSetViewer as DataSetViewer } from '../../../HTML5SharedGUI/CRNComponent/Scripts/crnDataSets';
import { ObservationsSource } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/MultipleDataSetObsSource';
import { MemoryDataSetStorage, LocalStorageDataSetStorage, IndexedDBDataSetStorage } from '../../../HTML5SharedGUI/CRNComponent/Scripts/DataSetStorage';

import * as GECInterfaces from '../../ClassicGECTSWrapper/Scripts/Interfaces';
import IDDTabs from "../../../HTML5SharedGUI/GenericComponents/Scripts/IDDTabs";

var examplesPath = "./Examples/GECModels/";
var examples: ExamplesGroup[] = [
    {
        // NOTE: the URLs here are supposed to be resolved relativly to the HTML page that runs the script. Usually it is ($app_root)/index.html.
        // As the path to the code is $app_root/CodeExamples/* the relatice url here is CodeExamples/*
        Name: "Examples",
        Correspondence: {
            "Basic": examplesPath + "basic.txt",
            "Repressilator Similar": examplesPath + "repressilator_similar.txt",
            "Repressilator Modules": examplesPath + "repressilator_modules.txt",
            "Repressilator Modules Similar": examplesPath + "repressilator_modules_similar.txt",
            "Receiver Device": examplesPath + "receiver_device.txt",
            "Predator-Prey": examplesPath + "predator_prey.txt",
            "Band Detector": examplesPath + "band_detector.txt"
        }
    }
];

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
var gecCodeModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-gec-tab-header"));
var codeModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-code-tab-header"));
var crnModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-crn-tab-header"));
var classicGEC = new ClassicGEC(Options.Server());
var gecCodeSource = new GECCodeEditor(<any>GECLanguage, examples, gecCodeModificationIndicator, {});
var partsDBCodeSource = new PartsDBCodeEditor(<any>PartsLanguage, [], gecCodeModificationIndicator, {});
var reactionsDBCodeSource = new ReactionsDBCodeEditor(<any>ReactionsLanguage, [], gecCodeModificationIndicator, {});
var codeSource = new CodeEditor.KOCRNCodeEditor(codeModificationIndicator);
var codeStorage = new CodeStorage.CodeEditorStorageDecorator(codeSource, "CRN");
var gecCodeStorage = new CodeStorage.CodeEditorStorageDecorator(<any>gecCodeSource, "GEC");
var partsCodeStorage = new CodeStorage.CodeEditorStorageDecorator(<any>partsDBCodeSource, "GECParts");
var reactionsCodeStorage = new CodeStorage.CodeEditorStorageDecorator(<any>reactionsDBCodeSource, "GECReactions");
var crnExport = new CrnExport.CRNExport(classicGEC);
var options = new Options(classicGEC);
var codeParser = new CodeParser.Parser(classicGEC);
var crnEditor = new CrnEditor.CRNEditor<void>(dataSetList, crnModificationIndicator);
var currentlySelectedDataSetSource = new ObservationsSource(dataSetStorage);
var infRunner = new InferenceRunner.Adapter(classicGEC, currentlySelectedDataSetSource);
var ssaRunner = new SSARunner(classicGEC);
var ssaSummaryViewer = new SSASummaryViewer();
var crnVM = crnEditor.GetVM();
var inferenceGraphViewer = new InferenceGraphAdapter<void>(crnVM);
var ssaGraphViewer = new SSAGraphViewer(crnVM);
var crnGraphViewer = new CRNGraphViewer<void>(crnVM);
var simViewer = new SimViewer.Viewer();
var spatialViewerSettings = new SpatialViewerSettings("black,green");
var spatialViewer1D = new SpatialViewer1D(spatialViewerSettings);
var spatialViewer2D = new SpatialViewer2D(spatialViewerSettings);
var dataSetViewer = new DataSetViewer();
var probabilityMapsViewer = new PMViewer(classicGEC, spatialViewerSettings);
var simRunner = new SimRunner.SimulationRunner(classicGEC, currentlySelectedDataSetSource);
var spat1DSimRunner = new SimRunnerSpat1D.SimulationRunner(classicGEC);
var spat2DSimRunner = new SimRunnerSpat2D.SimulationRunner(classicGEC);
var crnSelector = new CRNSelector(crnVM);
var synthesisAdapter = new SynthesisAdapter(classicGEC);

//var crnSettings = new CRNSettingsVM();
//inference viewers:
var infParamViewer = new InferenceParametersViewer.InferredParametersViewer(crnSelector);
var infDynamicsViewer = new InferenceModelDynamicsViewer.ModelDataDynamics(crnSelector);
var infPosteriorViewer = new InferencePosteriorViewer.PosteriorViewer(crnSelector);
var infSummary = new InferenceSummary.InferenceSummary(crnSelector);
var infViewer = new InferenceCompositeViewer.Viewer([infParamViewer, infDynamicsViewer, infSummary, infPosteriorViewer]);
var synthesisViewer = new SynthesisViewer(classicGEC);

//SSA viewers
var ssaTextViewer = new SSATextViewer();
var ctmcCompoundViewer = new SSACompoundViewer(ssaGraphViewer, ssaTextViewer, ssaSummaryViewer);
var crnCompoundViewer = new CRNCompoundViewer(crnEditor);

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

var currentCRNSimulationType = function () {
    var ig = crnEditor.getModel();
    var model = null;
    for (var n in ig.nodes) {
        model = ig.nodes[n];
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

var solutionSelector: HTMLSelectElement = <HTMLSelectElement>$("#solution-selector")[0];
function setSolutionsCount(count: number) {
    while (solutionSelector.hasChildNodes())
        solutionSelector.removeChild(solutionSelector.firstChild);
    for (var i = 1; i <= count; i++) {
        var opt = document.createElement("option");
        opt.text = i.toString();
        opt.value = i.toString();
        solutionSelector.appendChild(opt);
    }
}
var getSolutionIdx = () => {
    var idx = solutionSelector.selectedIndex;
    if (idx < 0)
        return -1;
    return parseInt(solutionSelector.options[idx].value);
}

//================================================
//Start JSBOL -> SBOLJS Conversion

var visbolDesign: any;

function showSBOL(jsbol: GECInterfaces.jSBOLDocument) {


    try {
        //console.log(JSON.stringify(jsbol));

        var sbolobj = jsbolToObject(jsbol);

        var visbolDiv = $("#visbolContainer")[0];
        while (visbolDiv.children.length > 0)
            visbolDiv.removeChild(visbolDiv.firstChild);

        //var index = $("#resultsTabs").find("#visbolTab").index() - 1;
        //$("#resultsTabs").tabs("option", "active", index);

        //var xmlsbol = convertJsonToXml(jsbol);

        //console.log("Start printing XML version");
        //console.dirxml(xmlsbol);
        //console.log("Finished printing XML version of SBOL")

        var component: any = {
            segments: []
        }

        //console.log(JSON.stringify(jsbol));

        //console.log("About to print the display list for each component definition...");
        //sbolobj.componentDefinitions.forEach( (componentDefinition:any) =>  console.log(visbol.getDisplayList(componentDefinition)));
        //console.log("All display lists printed.");



        sbolobj.componentDefinitions.forEach((componentDefinition: any) => {
            if (componentDefinition.name == 'device') {

                var fullseq = visbol.getDisplayList.getDisplayList(componentDefinition).components[0].segments[0].sequence
                var firsthalf = fullseq.slice(0, fullseq.length / 2)
                var halfseg = visbol.getDisplayList.getDisplayList(componentDefinition).components[0].segments[0]
                halfseg.sequence = firsthalf
                component.segments = component.segments.concat(halfseg)
                //component.segments = component.segments.concat(visbol.getDisplayList.getDisplayList(componentDefinition).components[0].segments[0])
            }

        });
        //xmlsbol.componentDefinitions.forEach(componentDefinition => component.segments = component.segments.concat(visbol.getDisplayList(componentDefinition).components[0].segments[0]));

        var displayList: any = { version: 1, components: [component] };
        var font: any = visbolfont;
        visbolDesign = new visbol.Design({ element: visbolDiv, font: font });
        visbolDesign.setDisplayList(displayList);
        visbolDesign.redraw();

        /*var xml = json2xml(jsbol);
        console.log(visbol != null);
        var xmlstring = JSON.stringify(xml);
        visbolDiv.text(xmlstring);*/

    } catch (e) {
        console.log(e)
    }
}

//Phase 2. Assembling higher level functionality from the components above

var parseCrnOperation = new ParseOperation.Operation<void, void>(codeStorage, codeParser, crnCompoundViewer, codeSource);
var parseGecOperation = new GecParseOperation.Operation<void>(classicGEC, gecCodeStorage, partsCodeStorage, reactionsCodeStorage, setSolutionsCount, showSBOL, crnExport, gecCodeSource, partsDBCodeSource, reactionsDBCodeSource);
var selectSolutionOperation = new GetSolutionOperation.Operation(getSolutionIdx, classicGEC, codeSource, crnCompoundViewer, showSBOL, crnExport);
var exportOperation = new ExportOperation.Exporter(classicGEC, crnEditor, crnSelector, crnExport);
var simulateOperation = new SimulateOperation.Operation<SimRunner.ISimulationMessage, SVF.IVisualizationUpdateMessage, void>(crnEditor, crnSelector, simRunner, simViewer, ctmcCompoundViewer, probabilityMapsViewer, crnCompoundViewer, crnExport, unitsExtractor, plotSettingsExtractor, SumToVum.getModelfromSim, SumToVum.getExportsFromSim, SumToVum.getStateSpaceFromSim, SumToVum.Convert);
var simulateSpatial1DOperation = new Spatial1DSimulation.Operation(crnEditor, crnSelector, spat1DSimRunner, spatialViewer1D);
var simulateSpatial2DOperation = new Spatial2DSimulation.Operation(crnEditor, crnSelector, spat2DSimRunner, spatialViewer2D);
var synthesisOperation = new SynthesisOperation(crnEditor, synthesisAdapter, synthesisViewer);

var autoChoiceSimulationOperation = new DispatchedSimulateOperation(
    currentCRNSimulationType,
    simulateSpatial1DOperation, $('#spatial1dViewer'),
    simulateSpatial2DOperation, $('#spatial2dViewer'),
    simulateOperation, $('#simulationViewer'));

var inferOperation = new InferOperation.Operation(crnEditor, currentlySelectedDataSetSource, infRunner, infViewer, crnExport, plotSettingsExtractorForInference);

var ssaOperation = new SSAOperation(crnEditor, ssaRunner, ctmcCompoundViewer, false);

var selectGECCodeTab = new TabSelectOperation.Operation($("#gecTabs"), "#gecCode");
var selectCRNDirectivesTab = new TabSelectOperation.Operation($('#inputTabs'), "#crnDirectives");
var selectInferTab = new TabSelectOperation.Operation($('#resultsTabs'), "#inferenceViewer");
var selectModelDynamicsTab = new TabSelectOperation.Operation($('#inferenceViewer'), "#inferenceDynamics");
var selectSimulationTab = new TabSelectOperation.Operation($('#resultsTabs'), "#simulationViewerTab");
var selectSSATab = new TabSelectOperation.Operation($('#resultsTabs'), "#StatesViewerTab");
var selectSSATextTab = new TabSelectOperation.Operation($('#StatesViewerTab'), "#ssaTextTab");
var selectSBOLTab = new TabSelectOperation.Operation($('#resultsTabs'), '#visbolTab');
var selectSynthesisTab = new TabSelectOperation.Operation($('#resultsTabs'), "#synthesisTab");

//var simHintScreen = new HintScreen.HintScreen(autoChoiceSimulationOperation);
//var graphHintScreen = new HintScreen.HintScreen(new HintScreen.CombinedHintRemoveNotifier([parseCrnOperation]));


//Phase 3. Setting up the layout
// Turn the main areas into tabs.

IDDTabs(null, (newPanel => {
    // Force redraw of visbol when the relevant tab is focused (workaround for issue where visbol doesn't draw on hidden tabs).
    if (newPanel[0].id == 'visbolTab')
        visbolDesign.redraw();
}));

//bind functional components
codeSource.AutoBind();
gecCodeSource.AutoBind();
partsDBCodeSource.AutoBind();
reactionsDBCodeSource.AutoBind();
crnEditor.Bind(document.getElementById('crnTabs'));
inferenceGraphViewer.Bind(document.getElementById('inferenceGraph'));
var exportAreas = $(".c-export").get();
for (var c in exportAreas)
    crnExport.Bind(exportAreas[c]);
crnExport.BindToTabs(document.getElementById("crnExport"));
dataSetList.Bind($("#datasets-viewer").get()[0]);
dataSetViewer.Bind($("#dataset-viewer").get()[0]);
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
crnGraphViewer.Bind(document.getElementById('crnGraph'));
longOperationsManager.Bind(document.getElementById('toolbar'));
crnSelector.Bind(document.getElementById('CRNSelector'));
ko.applyBindings(crnVM, document.getElementById('crnInferenceGraphTab'));
options.bind(document.getElementById('options'));
synthesisViewer.Bind(document.getElementById('synthesisTab'));

dataSetList.getSelectedObservable().subscribe((val) => {
    val ? dataSetViewer.Show(val) : dataSetViewer.Show(null);
});

var files = ["TestInference.txt", "Join_Simulated.txt", "AM_obs.csv", "AM_obs_noised.csv",
    "Rep_Simulated.csv"];
dataSetList.Initialize("Examples/Observations/", files);

partsDBCodeSource.ShowCode(classicGEC.Parts);
reactionsDBCodeSource.ShowCode(classicGEC.Reactions);
$("#gecTabs").tabs("option", "active", 2);

function getActiveTabID(tabName: string) {
    return $("#" + tabName + " .ui-tabs-panel:visible").attr("id");
}
function getActiveInputTabID() {
    return getActiveTabID("inputTabs");
}
function getActiveCrnTabID() {
    return getActiveTabID("crnTabs");
}
function getActiveGECTabID() {
    return getActiveTabID("gecTabs");
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
    containment: "#crnData",
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

var isSelectingSolution = false;
longOperationsManager.CanStartNewAction.subscribe(val => {
    if (isSelectingSolution && val) {
        setTimeout(() => solutionSelector.focus());
        isSelectingSolution = false;
    }
});
solutionSelector.onchange = () => {
    var operations = [selectSolutionOperation, exportOperation];
    isSelectingSolution = true;
    longOperationsManager.EnqueueOperations(operations);
}

document.getElementById('parseButton').onclick = function (e) {
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "gecTabs":
            tabID = getActiveGECTabID();
            switch (tabID) {
                case "gecParts":
                case "gecReactions":
                case "gecCode":
                    operations = [parseGecOperation, selectGECCodeTab, selectSBOLTab, selectSolutionOperation, exportOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
        case "crnTabs":
            tabID = getActiveCrnTabID();
            switch (tabID) {
                case "crnData":
                case "crnCode"://code
                    operations = [parseCrnOperation, exportOperation, selectCRNDirectivesTab];
                    break;
                case "crnDirectives"://directives
                case "crnParameters"://parameters
                case "crnSpecies"://species
                case "crnReactions"://reactions
                case "crnInferenceGraph": // inference graph
                    operations = [parseCrnOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
    }
    longOperationsManager.EnqueueOperations(operations);
}

document.getElementById('exportButton').onclick = function (e) {
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = [];
    switch (tabID) {
        case "gecTabs":
            switch (getActiveGECTabID()) {
                case "gecParts":
                case "gecReactions":
                case "gecCode":
                    operations = [parseGecOperation, selectGECCodeTab, selectSolutionOperation, exportOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
        case "crnTabs":
            switch (getActiveCrnTabID()) {
                case "crnData":
                case "crnCode"://crn code
                    operations = [parseCrnOperation, exportOperation];
                    break;
                case "crnDirectives"://directives
                case "crnParameters"://parameters
                case "crnSpecies"://species
                case "crnReactions"://reactions
                case "crnInferenceGraph": // inference graph
                    operations = [exportOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
    }
    longOperationsManager.EnqueueOperations(operations);
}

document.getElementById('simulateButton').onclick = function (e) {
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "gecTabs":
            tabID = getActiveGECTabID();
            switch (tabID) {
                case "gecParts":
                case "gecReactions":
                case "gecCode":
                    operations = [parseGecOperation, selectGECCodeTab, selectSolutionOperation, parseCrnOperation, selectSimulationTab, autoChoiceSimulationOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
        case "crnTabs":
            tabID = getActiveCrnTabID();
            switch (tabID) {
                case "crnData":
                case "crnCode"://code
                    operations = [parseCrnOperation, exportOperation, selectSimulationTab, autoChoiceSimulationOperation];
                    break;
                case "crnDirectives"://directives
                case "crnParameters"://parameters
                case "crnSpecies"://species
                case "crnReactions"://reactions
                case "crnInferenceGraph": // inference graph
                    operations = [exportOperation, selectSimulationTab, autoChoiceSimulationOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
    }
    longOperationsManager.EnqueueOperations(operations);
}

document.getElementById('synthesisButton').onclick = function (e) {
    if (!longOperationsManager.CanStartNewAction())
        return;
    var tabIndex = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabIndex) {
        case "sgTab":
            switch (getActiveGECTabID()) {
                case "gecParts":
                case "gecReactions":
                case "gecCode":
                    operations = [parseGecOperation, selectGECCodeTab, selectSolutionOperation, parseCrnOperation, selectSynthesisTab, synthesisOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
        case "crnTabs":
            switch (getActiveCrnTabID()) {
                case "crnData":
                case "crnCode"://code
                    operations = [parseCrnOperation, exportOperation, selectSynthesisTab, synthesisOperation];
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
    }
    longOperationsManager.EnqueueOperations(operations);
}

document.getElementById('statesButton').onclick = function (e) {
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "gecTabs":
            tabID = getActiveGECTabID();
            switch (tabID) {
                case "gecParts":
                case "gecReactions":
                case "gecCode":
                    operations = [parseGecOperation, selectGECCodeTab, selectSolutionOperation, parseCrnOperation, selectSSATab, selectSSATextTab, ssaOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
        case "crnTabs":
            tabID = getActiveCrnTabID();
            switch (tabID) {
                case "crnData":
                case "crnCode"://code
                    operations = [parseCrnOperation, exportOperation, selectSSATab, selectSSATextTab, ssaOperation];
                    break;
                case "crnDirectives"://directives
                case "crnParameters"://parameters
                case "crnSpecies"://species
                case "crnReactions"://reactions
                case "crnInferenceGraph": // inference graph
                    operations = [exportOperation, selectSSATab, selectSSATextTab, ssaOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
    }
    longOperationsManager.EnqueueOperations(operations);
}

document.getElementById('inferButton').onclick = function (e) {
    var tabID = getActiveInputTabID();
    var operations: Array<LongOperations.IOperation> = []
    switch (tabID) {
        case "gecTabs":
            tabID = getActiveGECTabID();
            switch (tabID) {
                case "gecParts":
                case "gecReactions":
                case "gecCode":
                    operations = [parseGecOperation, selectGECCodeTab, selectSolutionOperation, parseCrnOperation, selectInferTab, selectModelDynamicsTab, inferOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
        case "crnTabs":
            tabID = getActiveCrnTabID();
            switch (tabID) {
                case "crnData":
                case "crnCode"://code            
                    operations = [parseCrnOperation, exportOperation, selectInferTab, selectModelDynamicsTab, inferOperation];
                    break;
                case "crnDirectives"://directives
                case "crnParameters"://parameters
                case "crnSpecies"://species
                case "crnReactions"://reactions
                case "crnInferenceGraph": // inference graph
                    operations = [exportOperation, selectInferTab, selectModelDynamicsTab, inferOperation];
                    break;
                default:
                    throw "not implemented yet";
            }
            break;
    }
    longOperationsManager.EnqueueOperations(operations);
}

document.getElementById('stopButton').onclick = function (e) {
    longOperationsManager.Stop();
}

//serviceWorker.Bind(document.getElementById('worker'));

Utils.RemoveLoadingOverlay();

console.log("ClassicGEC loaded");