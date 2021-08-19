/*import { ServiceWorker } from '../../HTML5CRN_Lib/Scripts/ServiceWorker';
let serviceWorker = new ServiceWorker();*/

import * as $ from 'jquery';
import "../Styles/App.css";
import "../../../node_modules/jquery-ui/themes/base/all.css"
import "../../../node_modules/katex/dist/katex.min.css"
import "../../KnockoutGrid/table.css"
// We override IDD styles with shared.css, so we require IDD styles to be loaded prior to General.css.
import "../../../node_modules/interactive-data-display/dist/idd.css"
import "../../CodeEditor/Styles/codepad.css"
import "../../SimulationViewer/Styles/simulation.css"
import "../../InferenceViewer/Styles/inference.css"
import "../../CRNComponent/Styles/crn.css"
import "idd";
declare var InteractiveDataDisplay: any;
import Options from '../../GenericComponents/Scripts/Options';
import "../../GenericComponents/Scripts/Dropdown";
import * as Utils from '../../HTML5CRN_Lib/Scripts/Utils';
import * as LongOperations from '../../HTML5CRN_Lib/Scripts/Operations/LongOperations';
import * as LongOperationsKO from './Operations/ButtonsAvailabilityKO';
import * as CrnParseOperation from '../../HTML5CRN_Lib/Scripts/Operations/ParseCodeFillCRN';
import * as ExportOperation from '../../HTML5CRN_Lib/Scripts/Operations/ModellingEngineExporter';
import * as GenericParseOperation from '../../HTML5CRN_Lib/Scripts/Operations/GenericCodeParsing';
import * as SgParseOperation from './Operations/ParseSgCode';
import * as SgExpandOperation from './Operations/ExpandSgCode';
import * as SimulateParsedCRNOperation from '../../HTML5CRN_Lib/Scripts/Operations/SimulateParsedCRN';
import * as Spatial1DSimulation from '../../HTML5CRN_Lib/Scripts/Operations/Spatial1DSimulation';
import * as Spatial2DSimulation from '../../HTML5CRN_Lib/Scripts/Operations/Spatial2DSimulation';
import * as TabSelectOperation from '../../HTML5CRN_Lib/Scripts/Operations/TabSelect';
import { Operation as SSAOperation } from '../../HTML5CRN_Lib/Scripts/Operations/StateSpaceAnalysis';
import * as InferOperation from '../../HTML5CRN_Lib/Scripts/Operations/Inference';
import { SimulationType, Operation as DispatchedSimulateOperation } from '../../HTML5CRN_Lib/Scripts/Operations/AutoChoiceSimulation';
import { Operation as SynthesisOperation } from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/Synthesis';
import StateSpaceViewer from '../../HTML5CRN_Lib/Scripts/Adapters/SSAGraphAdapter';
import CRNGraphViewer from '../../HTML5CRN_Lib/Scripts/Adapters/CRNGraphAdapter';
import * as ModificationIndicator from "../../GenericComponents/Scripts/ModificationIndicators";
import * as CrnCodeEditor from '../../HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNCodeEditor';
import { CodeEditor as DsdCodeEditor } from './Adapters/KnockoutBasedDSDCodeEditor';
import * as CodeStorage from '../../HTML5CRN_Lib/Scripts/Adapters/CodeEditorStorageDecorator';
import * as MECodeParser from '../../HTML5CRN_Lib/Scripts/Adapters/ModellingEngineCRNParser';
import * as CrnEditor from '../../HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNViewer';
import * as CrnExport from '../../HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNExport';
import * as SimRunner from '../../HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSimulationRunner';
import * as SimRunnerSpat1D from '../../HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSpatial1DSimulationRunner';
import * as SimRunnerSpat2D from '../../HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSpatial2DSimulationRunner';
import InferenceGraphAdapter from '../../HTML5CRN_Lib/Scripts/Adapters/InferenceGraphAdapter';
import * as InferenceRunner from '../../HTML5CRN_Lib/Scripts/Adapters/ModellingEngineInferenceRunner';
import { Adapter as SSARunner } from "../../HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSSA";
import { Viewer as SSASummaryViewer } from "../../HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedSSASummaryViewer";
import * as SimViewer from '../../HTML5CRN_Lib/Scripts/Adapters/SimViewerAdapter';
import { StoredSettings as SpatialViewerSettings } from "../../SimulationViewer/Scripts/SpatialViewerSettings";
import Spatial1DViewer from "../../SimulationViewer/Scripts/Spatial1DViewer";
import Spatial2DViewer from "../../SimulationViewer/Scripts/Spatial2DViewer";
//import * as t HintScreen from '../../HTML5CRN_Lib/Scripts/HintScreen';
import * as InferenceParametersViewer from '../../InferenceViewer/Scripts/InferredParametersViewer';
import * as InferenceModelDynamicsViewer from '../../InferenceViewer/Scripts/ModelDataDynamics';
import * as InferencePosteriorViewer from '../../InferenceViewer/Scripts/PosteriorViewer';
import * as InferenceSummary from '../../InferenceViewer/Scripts/InferenceSummary';
import * as InferenceCompositeViewer from '../../InferenceViewer/Scripts/CompositeViewer';
import { Viewer as SSATextViewer } from '../../HTML5CRN_Lib/Scripts/Components/SSATextViewer';
import { Viewer as SSACompoundViewer } from '../../HTML5CRN_Lib/Scripts/Components/SSACompoundViewer';
import CRNCompoundViewer from '../../HTML5CRN_Lib/Scripts/Components/CRNCompoundViewer';
import * as SumToVum from '../../HTML5CRN_Lib/Scripts/Adapters/SumToVum';
import { Viewer as ProbabilityMapViewer } from '../../HTML5CRN_Lib/Scripts/Adapters/ProbabilityMapsAdapter';
import SynthesisAdapter from '../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/ModellingEngineSynthesis';
import { SynthesisViewer } from '../../../HTML5SharedGUI/SimulationViewer/Scripts/SynthesisViewer';
import * as SVGStructuralCache from './Adapters/SVGStructuralCache';
import * as CachingDecorator from './Adapters/CachingDecorator';
import CRNEngine from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as SVF from '../../SimulationViewer/Scripts/SimulationViewerFramework';
import { KnockoutBasedDataSetsList as DataSetList } from '../../CRNComponent/Scripts/crnDataSets';
import { KnockoutGridDataSetViewer as DataSetViewer } from '../../CRNComponent/Scripts/crnDataSets';
import { ExternalSetting as ExternalSetting, CRN as CRN } from '../../CRNComponent/Scripts/crnVM';
import { ObservationsSource } from '../../HTML5CRN_Lib/Scripts/Adapters/MultipleDataSetObsSource';
import { MemoryDataSetStorage, LocalStorageDataSetStorage, IndexedDBDataSetStorage } from '../../CRNComponent/Scripts/DataSetStorage';
import { INamedMonarchLanguage } from '../../CodeEditor/Scripts/CodeEditor';
import * as GenericDSDParser from './Adapters/GenericDSDParser';
import * as GenericExpandingParser from './Adapters/GenericExpandingParser';
import * as template from 'raw-loader!../Templates/template.html';
import CRNSelector from '../../CRNComponent/Scripts/CRNSelector';
import IDDTabs from "../../GenericComponents/Scripts/IDDTabs";

document.body.innerHTML = template;

// (FP) Launches a DSD-type tool. The parameters provide the calculus language, code examples, and the parser. Note that there are actually two parsers; one is supposed to generate an unexpanded CRN, while the other is supposed to generate an expanded CRN.
// Design note 1: this bakes in the assumption that the calculus has the notion of expansion of a CRN, which is a DSD-specific concept. Could this be more abstract, and be used as a generic calculus tools.
// Design note 2: the expanded CRN appears to be a result of parsing. In theory, it could be generated from an unexpanded CRN even without the code (however, actually doing so would require serialization of calculus species). In practice, the back-end caches the parsing results, so parsing is not actually run twice.
export function Launch<TOptions extends Options, TCustomSettings>(
    language: INamedMonarchLanguage,
    codeExamples: Array<ExamplesGroup>,
    parser: GenericDSDParser.Parser<TCustomSettings, TOptions>,
    expandCrnParser: GenericExpandingParser.ExpandingParser<TCustomSettings, TOptions>,
    crnEngine: CRNEngine,
    options: TOptions,
    customSettingsConverter: CrnEditor.CustomSettingsConverter<TCustomSettings>) {

    // Phase 1. Creating the functional components. (FP) This is where we create all of the separate bits and pieces. Some of these perform computation, some others are bits of GUI code. At this point, they are all disconnected.
    let dataSetStorage = new IndexedDBDataSetStorage("DSDdb");
    var dataSetList = new DataSetList(dataSetStorage);
    //let serviceWorker = new ServiceWorker();
    var longOperationsManager = new LongOperationsKO.SgOperationButtonsAvailability();
    var dsdCodeModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-dsd-code-tab-header"));
    var crnCodeModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-code-tab-header"));
    var crnModificationIndicator = new ModificationIndicator.TabSuffixIndicator($(".j-crn-tab-header"));
    var codeSource = new DsdCodeEditor<TOptions>(language, codeExamples, dsdCodeModificationIndicator, options);
    var dsdCodeStorage = new CodeStorage.CodeEditorStorageDecorator<TOptions>(codeSource, "DSD");
    var meCodeEditor = new CrnCodeEditor.KOCRNCodeEditor(crnCodeModificationIndicator);
    var meCodeStorage = new CodeStorage.CodeEditorStorageDecorator<void>(meCodeEditor, "CRN");
    var meCodeParser = new MECodeParser.Parser(crnEngine);
    var svgCache = new SVGStructuralCache.SVGStructuralCache();
    var crnEditor = new CrnEditor.CRNEditor<TCustomSettings>(dataSetList, crnModificationIndicator);
    var crnCacheViewer = new CachingDecorator.CachingDecorator<TCustomSettings | void>(svgCache, crnEditor);
    var sgEditor = new CrnEditor.CRNEditor<TCustomSettings>(dataSetList, crnModificationIndicator, customSettingsConverter);
    var sgCacheViewer = new CachingDecorator.CachingDecorator<TCustomSettings | void>(svgCache, sgEditor);
    var synthesisAdapter = new SynthesisAdapter(crnEngine);

    var crnDataSetSource = new ObservationsSource(dataSetStorage);
    var crnExport = new CrnExport.CRNExport(crnEngine);
    var infRunner = new InferenceRunner.Adapter(crnEngine, crnDataSetSource);
    var ssaRunner = new SSARunner(crnEngine);
    var ssaSummaryViewer = new SSASummaryViewer();
    var sgCrnVM = sgEditor.GetVM();
    var crnVM = crnEditor.GetVM();
    var inferenceGraphViewer = new InferenceGraphAdapter<void>(crnVM);
    var ssaViewer = new StateSpaceViewer(sgCrnVM);
    var simViewer = new SimViewer.Viewer();
    var spatialViewerSettings = new SpatialViewerSettings("black,green");
    var spatialViewer1D = new Spatial1DViewer(spatialViewerSettings);
    var spatialViewer2D = new Spatial2DViewer(spatialViewerSettings);
    var dataSetViewer = new DataSetViewer();
    var crnGraphViewer = new CRNGraphViewer<TCustomSettings | void>(crnEditor.GetVM());
    var crnSelector = new CRNSelector(crnEditor.GetVM());
    var infParamViewer = new InferenceParametersViewer.InferredParametersViewer(crnSelector);
    var infDynamicsViewer = new InferenceModelDynamicsViewer.ModelDataDynamics(crnSelector);
    var infPosteriorViewer = new InferencePosteriorViewer.PosteriorViewer(crnSelector);
    var infSummary = new InferenceSummary.InferenceSummary(crnSelector);
    var infViewer = new InferenceCompositeViewer.Viewer([infParamViewer, infDynamicsViewer, infSummary, infPosteriorViewer]);
    var pmViewer = new ProbabilityMapViewer(crnEngine, spatialViewerSettings);
    var synthesisViewer = new SynthesisViewer(crnEngine);

    // SSA (Space State Analysis) viewers
    var ssaTextViewer = new SSATextViewer();
    var ctmcCompoundViewer = new SSACompoundViewer(ssaViewer, ssaTextViewer, ssaSummaryViewer);
    var sgCompoundViewer = new SSACompoundViewer(ssaViewer, ssaTextViewer, ssaSummaryViewer);

    var crnCompoundViewer = new CRNCompoundViewer(crnCacheViewer);
    var sgCrnCompoundViewer = new CRNCompoundViewer(sgCacheViewer);

    var simRunner = new SimRunner.SimulationRunner(crnEngine, crnDataSetSource);
    var spat1DSimRunner = new SimRunnerSpat1D.SimulationRunner(crnEngine);
    var spat2DSimRunner = new SimRunnerSpat2D.SimulationRunner(crnEngine);

    var knownSweepNames: Array<string> = [];

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
    meCodeEditor.EditorLoadEvents.subscribe(next => {
        //dsdCodeStorage.ShowCode("");
        crnCompoundViewer.UpdateValuesWith(null, null, false);
        sgCrnCompoundViewer.UpdateValuesWith(null, null, false);
        //crnExport.Reset();
    });
    codeSource.EditorLoadEvents.subscribe(next => {
        //meCodeStorage.ShowCode("");
        sgEditor.UpdateValuesWith(null, null, false);
        crnCompoundViewer.UpdateValuesWith(null, null, false);
        sgCrnCompoundViewer.UpdateValuesWith(null, null, false);
        //crnExport.Reset()
    });

    var currentCRNSimulationType = function () {
        var ig = crnEditor.getModel();
        var model = null;
        for (let n in ig.nodes) {
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


    // Phase 2. Assembling higher level functionality from the components above. (FP) These "Operations" can be thought of as "things that happen when the user clicks something" and are compositions of the elements declared above. For example, a parsing operation makes use of the code editor, the parser and the crn viewers, plus some secondary aspects such as an exports viewer and an errors viewer. Note that operations can be further composed in a sequence of operations; see below for what happens in the actual click handlers.

    // This operation expands the current CRN.
    var expandCRNOperation = new SgExpandOperation.Operation<TCustomSettings, TOptions>(dsdCodeStorage, meCodeEditor, sgEditor, expandCrnParser, crnCompoundViewer, codeSource);
    // This operation parses DSD with no expansion.
    var parseDSDOperation = new SgParseOperation.Operation<TCustomSettings, TOptions>(dsdCodeStorage, meCodeEditor, parser, sgCrnCompoundViewer, codeSource);
    // This operation parses CRN.
    var parseCRNOperation = new CrnParseOperation.Operation<void, void>(meCodeStorage, meCodeParser, crnCompoundViewer, meCodeEditor);
    var exportOperation = new ExportOperation.Exporter(crnEngine, crnEditor, crnSelector, crnExport);
    //var exportUnexpandedOperation = new ExportOperation.Exporter(crnEngine, sgEditor, crnExport);
    var simulateParsedCRNOperation =
        new SimulateParsedCRNOperation.Operation<SimRunner.ISimulationMessage, SVF.IVisualizationUpdateMessage, TCustomSettings>(
            crnEditor, crnSelector, simRunner, simViewer, ctmcCompoundViewer, pmViewer, crnCompoundViewer, crnExport, unitsExtractor, plotSettingsExtractor, SumToVum.getModelfromSim, SumToVum.getExportsFromSim, SumToVum.getStateSpaceFromSim, SumToVum.Convert);
    var simulateSpatial1DOperation = new Spatial1DSimulation.Operation(crnEditor, crnSelector, spat1DSimRunner, spatialViewer1D);
    var simulateSpatial2DOperation = new Spatial2DSimulation.Operation(crnEditor, crnSelector, spat2DSimRunner, spatialViewer2D);
    var synthesisOperation = new SynthesisOperation(crnEditor, synthesisAdapter, synthesisViewer);

    var autoChoiceSimulationOperation = new DispatchedSimulateOperation(
        currentCRNSimulationType,
        simulateSpatial1DOperation, $('#spatial1dViewer'),
        simulateSpatial2DOperation, $('#spatial2dViewer'),
        simulateParsedCRNOperation, $('#simulationViewer'));


    var crnInferOperation = new InferOperation.Operation(crnEditor, crnDataSetSource, infRunner, infViewer, crnExport, plotSettingsExtractorForInference);
    var crn_statesOperation = new SSAOperation(crnEditor, ssaRunner, ctmcCompoundViewer, false);
    var sg_statesOperation = new SSAOperation(sgEditor, ssaRunner, sgCompoundViewer, true);

    var selectCRNTab = new TabSelectOperation.Operation($('#inputTabs'), "#crnTab");
    var selectCRNCodeTab = new TabSelectOperation.Operation($('#crnTab'), "#crnCode");
    var selectCRNDirectivesTab = new TabSelectOperation.Operation($('#crnTab'), "#crnDirectives");
    var selectSGDirectivesTab = new TabSelectOperation.Operation($('#sgTab'), "#sgDirectives");
    var selectSimulationTab = new TabSelectOperation.Operation($('#resultsTabs'), "#simulationViewerTab");
    var selectStatesTab = new TabSelectOperation.Operation($('#resultsTabs'), "#StatesViewerTab");
    var selectStatesTextTab = new TabSelectOperation.Operation($('#StatesViewerTab'), "#ssaTextTab");
    var selectInferTab = new TabSelectOperation.Operation($('#resultsTabs'), "#inferenceViewer");
    var selectModelDynamicsTab = new TabSelectOperation.Operation($('#inferenceViewer'), "#inferenceDynamics");
    var selectExportTab = new TabSelectOperation.Operation($('#resultsTabs'), "#crnExport");
    var selectSynthesisTab = new TabSelectOperation.Operation($('#resultsTabs'), "#synthesisTab");
    //var selectExportInitialsTab = new TabSelectOperation.Operation($('#crnExport'), "#export-initials");

    // (FP) The hint screen is the control that initially covers the simulation area with a "no simulation" message. The following code declares a set of operations that, when executed, cause the hint screen to be removed.
    //var simHintScreen = new HintScreen.HintScreen(autoChoiceSimulationOperation);
    //var graphHintScreen = new HintScreen.HintScreen(new HintScreen.CombinedHintRemoveNotifier(showGraphOperations));

    //var parseOperations: HintScreen.IHintRemoveNotifier[] = [expandCRNOperation, parseCRNOperation, parseDSDOperation];
    //var showGraphOperations = parseOperations.concat([simulateParsedCRNOperation]);

    // Phase 3. Setting up the layout. Turn the main areas into tabs. (FP) Here, some parts of the HTML GUI are made to behave like tabs. This happens in JS because HTML5 does not have a native notion of tabs.
    IDDTabs();
    var sgTabIdx = $("#inputTabs").find("#sgTab").index() - 1;
    $("#inputTabs").tabs("option", "active", sgTabIdx);
    var sgCodeTabIdx = $("#sgTab").find("#dsdCode").index() - 1;

    // Parse DSD when leaving the DSD tab.
    function parseDSDOnLeavingCodeTab() {
        if (longOperationsManager.CanStartNewAction())
            longOperationsManager.EnqueueOperations([parseDSDOperation]);
        else {
            if (confirm("Cannot parse while an operation is in progress. Press OK to abort the operation and parse, or press Cancel to revert your code changes.")) {
                longOperationsManager.Stop();
                setTimeout(() => longOperationsManager.EnqueueOperations([parseDSDOperation]));
            }
            else {
                codeSource.ResetToUnmodified();
                $("#inputTabs").tabs("option", "active", sgTabIdx);
                $("#sgTab").tabs("option", "active", sgCodeTabIdx);
            }
        }
    }

    // Parse DSD when leaving the DSD Code tab.
    $("#sgTab").on("tabsactivate", (event, ui) => {
        if (ui.oldPanel[0] == $("#dsdCode")[0] && dsdCodeModificationIndicator.getIsSet())
            parseDSDOnLeavingCodeTab();
    });
    // Parse DSD when leaving the DSD overall tab.
    /*$("#inputTabs").on("tabsactivate", (event, ui) => {
        if (ui.oldPanel[0] == $("#sgTab")[0] && dsdCodeModificationIndicator.getIsSet()) {
            var activeTabIndex = $('#sgTab').tabs("option", "active");
            if (activeTabIndex == 0)
                parseDSDOnLeavingCodeTab();
        }
    });*/

    var crnTabIdx = $("#inputTabs").find("#crnTab").index() - 1;
    var crnCodeTabIdx = $("#crnTab").find("#crnCode").index() - 1;

    // Parse CRN when leaving the CRN tab.
    function parseCRNOnLeavingCodeTab() {
        if (longOperationsManager.CanStartNewAction())
            longOperationsManager.EnqueueOperations([parseCRNOperation]);
        else {
            if (confirm("Cannot parse while an operation is in progress. Press OK to abort the operation and parse, or press Cancel to revert your code changes.")) {
                longOperationsManager.Stop();
                setTimeout(() => longOperationsManager.EnqueueOperations([parseCRNOperation]));
            }
            else {
                meCodeEditor.ResetToUnmodified();
                $("#inputTabs").tabs("option", "active", crnTabIdx);
                $("#crnTab").tabs("option", "active", crnCodeTabIdx);
            }
        }
    }

    // Parse CRN when leaving the CRN Code tab.
    $("#crnTab").on("tabsactivate", (event, ui) => {
        if (ui.oldPanel[0] == $("#crnCode")[0] && crnCodeModificationIndicator.getIsSet())
            parseCRNOnLeavingCodeTab();
    });
    // Parse CRN when leaving the CRN overall tab.
    /*$("#inputTabs").on("tabsactivate", (event, ui) => {
        if (ui.oldPanel[0] == $("#crnTab")[0] && crnCodeModificationIndicator.getIsSet()) {
            var activeTabIndex = $('#crnTab').tabs("option", "active");
            if (activeTabIndex == 0)
                parseCRNOnLeavingCodeTab();
        }
    });*/

    $("#inputTabs").on("tabsactivate", () => {
        //notifing the tabsAvailability manager that calculus tab is active
        var activeTabIndex = $('#inputTabs').tabs("option", "active");
        longOperationsManager.CRNTabActiveChanged(activeTabIndex == 1, activeTabIndex == 2);
    });

    // Bind functional components. (FP) This refers to KO binding. We are telling the components where they go on the screen.
    meCodeEditor.AutoBind();
    codeSource.AutoBind();
    crnEditor.Bind(document.getElementById('crnTab'));
    sgEditor.Bind(document.getElementById('sgTab'));
    dataSetViewer.Bind($("#dataset-viewer")[0]);
    dataSetList.Bind($("#datasets-viewer")[0]);
    inferenceGraphViewer.Bind(document.getElementById('inferenceGraph'));
    var exportAreas = $(".c-export");
    for (var c = 0; c < exportAreas.length; c++)
        crnExport.Bind(exportAreas[c]);
    crnExport.BindToTabs(document.getElementById("crnExport"));
    //simHintScreen.Bind(document.getElementById('simulationHintScreen'));
    //graphHintScreen.Bind(document.getElementById('graphHintScreen'));
    simViewer.Bind(document.getElementById('simulationViewer'));
    spatialViewer1D.bind(document.getElementById('spatial1dViewer'));
    spatialViewer2D.bind(document.getElementById('spatial2dViewer'));
    ssaSummaryViewer.Bind(document.getElementById('ssaSummary'));
    longOperationsManager.Bind(document.getElementById('toolbar'));
    infDynamicsViewer.Bind(document.getElementById('inferenceDynamics'));
    infParamViewer.Bind(document.getElementById('inferenceParameters'));
    infSummary.Bind(document.getElementById('inferenceSummary'));
    infPosteriorViewer.Bind(document.getElementById('inferencePosterior'));
    ssaTextViewer.Bind(document.getElementById('ssaTextTab'));
    pmViewer.Bind(document.getElementById('probabilitiesTab'));
    ssaViewer.Bind(document.getElementById('ssaGraph'));
    crnGraphViewer.Bind(document.getElementById('crnGraph'));
    crnSelector.Bind(document.getElementById('CRNSelector'));
    synthesisViewer.Bind(document.getElementById('synthesisTab'));
    ko.applyBindings(crnVM, document.getElementById('crnInferenceGraphTab'));

    dataSetList.getSelectedObservable().subscribe((val) => {
        val ? dataSetViewer.Show(val) : dataSetViewer.Show(null);
    });

    // (FP) Load some observations. Design note: this list should probably not be hardcoded.
    var files = ["Join_data.csv", "TestInference.txt", "Join_Simulated.txt", "AM_obs.csv", "AM_obs_noised.csv",
        "Rep_Simulated.csv", "AM80_87.txt", "AM80_87_XYB.txt", "AM80_96.txt", "AM80_96_XYB.txt", "BX2X.txt", "BY2Y.txt", "Fork2B_AddH1.txt", "Fork2B_AddH2.txt", "Fork2B_AddR.txt", "Fork2X_AddH1.txt", "Fork2X_AddH2.txt", "Fork2X_AddR.txt", "Fork2Y_AddH1.txt", "Fork2Y_AddH2.txt", "Fork2Y_AddR.txt", "JoinBX_AddB.txt", "JoinBX_AddH.txt", "JoinBX_AddRev1.txt", "JoinBX_AddRev2.txt", "JoinBX_AddRev3.txt", "JoinBX_AddX.txt", "JoinBY_AddB.txt", "JoinBY_AddH.txt", "JoinBY_AddRev1.txt", "JoinBY_AddRev2.txt", "JoinBY_AddRev3.txt", "JoinBY_AddY.txt", "JoinXY_AddH.txt", "JoinXY_AddRev1.txt", "JoinXY_AddRev2.txt", "JoinXY_AddRev3.txt", "JoinXY_AddX.txt", "JoinXY_AddY.txt", "Rep_rbxu1.txt", "Rep_rbyu1.txt", "Rep_rxyu1.txt", "Rep_tb.txt", "Rep_tx.txt", "Rep_u3pb.txt", "Rep_u3px.txt", "Rep_u3py.txt", "XY2B.txt"];
    dataSetList.Initialize("Examples/Observations/", files);

    function getActiveTabID(tabName: string) {
        return $("#" + tabName + " .ui-tabs-panel:visible").attr("id");
    }
    function getActiveInputTabID() {
        return getActiveTabID("inputTabs");
    }
    function getActiveSGTabID() {
        return getActiveTabID("sgTab");
    }
    function getActiveCRNTabID() {
        return getActiveTabID("crnTab");
    }

    // (FP) Setup some draggable separators.

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
        containment: "#inferenceDataSets",
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

    // Phase 4. Setting up button handlers. (FP) This is where we declare what happens when the user clicks one of the buttons. These are sequences of operations (for example, doing something and moving to the right tab). These sequences get posted to the "long operations manager", which is a class that knows how to concatenate operations and keep track of their state.
    document.getElementById('exportButton').onclick = function (e) {
        var tabID = getActiveInputTabID();
        var operations: Array<LongOperations.IOperation> = [];
        switch (tabID) {
            case "sgTab":
                switch (getActiveSGTabID()) {
                    case "dsdCode"://Sg code
                        operations = [parseDSDOperation];
                        break;
                    case "sgDirectives"://directives
                    case "sgParameters"://parameters
                    case "sgSpecies"://species
                    case "sgReactions"://reactions
                        console.warn("TODO: what to parse if we are on parsed CRN screen?");
                        break;
                    default:
                        throw "not implemented yet";
                }
                break;
            case "crnTab":
                switch (getActiveCRNTabID()) {
                    case "crnCode"://crn code
                        operations = [parseCRNOperation, exportOperation];
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

    document.getElementById('crnButton').onclick = function (e) {
        var operations: Array<LongOperations.IOperation> = [];
        //switch ($('#sgTab').tabs("option", "active")) {
        switch (getActiveSGTabID()) {
            case "dsdCode"://Sg code
                operations = [parseDSDOperation, expandCRNOperation, exportOperation, selectExportTab];
                break;
            case "sgDirectives"://directives
            case "sgParameters"://parameters
            case "sgSpecies"://species
            case "sgReactions"://reactions
            default:
                operations = [expandCRNOperation, exportOperation, selectExportTab];
                break;
        }
        longOperationsManager.EnqueueOperations(operations);
    }

    document.getElementById('simulateButton').onclick = function (e) {
        var tabIndex = getActiveInputTabID();
        var operations: Array<LongOperations.IOperation> = [];
        switch (tabIndex) {
            case "sgTab":
                switch (getActiveSGTabID()) {
                    case "dsdCode"://Sg code
                        operations = [parseDSDOperation, expandCRNOperation, exportOperation, selectSimulationTab, autoChoiceSimulationOperation];
                        break;
                    case "sgDirectives"://directives
                    case "sgParameters"://parameters
                    case "sgSpecies"://species
                    case "sgReactions"://reactions
                        operations = [expandCRNOperation, exportOperation, selectSimulationTab, autoChoiceSimulationOperation];
                        break;
                    default:
                        throw "not implemented yet";
                }
                break;
            case "crnTab":
                switch (getActiveCRNTabID()) {
                    case "crnCode"://Crn code
                        operations = [parseCRNOperation, exportOperation, selectSimulationTab, autoChoiceSimulationOperation];
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
        }
        longOperationsManager.EnqueueOperations(operations);
    }

    document.getElementById('statesButton').onclick = function (e) {
        var tabIndex = getActiveInputTabID();
        var operations: Array<LongOperations.IOperation> = [];
        switch (tabIndex) {
            case "sgTab":
                switch (getActiveSGTabID()) {
                    case "dsdCode"://code
                        operations = [parseDSDOperation, expandCRNOperation, exportOperation, sg_statesOperation, selectStatesTab, selectStatesTextTab];
                        break;
                    case "sgDirectives"://directives
                    case "sgParameters"://parameters
                    case "sgSpecies"://species
                    case "sgReactions"://reactions
                        operations = [expandCRNOperation, exportOperation, sg_statesOperation, selectStatesTab, selectStatesTextTab];
                        break;
                    default:
                        throw "not implemented yet";
                }
                break;
            case "crnTab":
                switch (getActiveCRNTabID()) {
                    case "crnCode"://code
                        operations = [parseCRNOperation, exportOperation, crn_statesOperation, selectStatesTab, selectStatesTextTab];
                        break;
                    case "crnDirectives"://directives
                    case "crnParameters"://parameters
                    case "crnSpecies"://species
                    case "crnReactions"://reactions
                    case "crnInferenceGraph": // inference graph
                        operations = [exportOperation, crn_statesOperation, selectStatesTab, selectStatesTextTab];
                        break;
                    default:
                        throw "not implemented yet";
                }
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
                switch (getActiveSGTabID()) {
                    case "dsdCode"://Sg code
                        operations = [parseDSDOperation, expandCRNOperation, exportOperation, selectSynthesisTab, synthesisOperation];
                        break;
                    case "sgDirectives"://directives
                    case "sgParameters"://parameters
                    case "sgSpecies"://species
                    case "sgReactions"://reactions
                        break;
                    default:
                        throw "not implemented yet";
                }
                break;
            case "crnTab":
                switch (getActiveCRNTabID()) {
                    case "crnData":
                    case "crnCode"://code
                        operations = [parseCRNOperation, exportOperation, selectSynthesisTab, synthesisOperation];
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


    document.getElementById('inferButton').onclick = function (e) {
        var tabIndex = getActiveInputTabID();
        var operations: Array<LongOperations.IOperation> = [];
        switch (tabIndex) {
            case "sgTab":
                switch (getActiveSGTabID()) {
                    case "dsdCode"://Sg code
                        operations = [parseDSDOperation, expandCRNOperation, exportOperation, selectInferTab, selectModelDynamicsTab, crnInferOperation];
                        break;
                    case "sgDirectives"://directives
                    case "sgParameters"://parameters
                    case "sgSpecies"://species
                    case "sgReactions"://reactions
                        break;
                    default:
                        throw "not implemented yet";
                }
                break;
            case "crnTab":
                switch (getActiveCRNTabID()) {
                    case "crnCode"://Crn code
                        operations = [parseCRNOperation, exportOperation, selectInferTab, selectModelDynamicsTab, crnInferOperation];
                        break;
                    case "crnDirectives"://directives
                    case "crnParameters"://parameters
                    case "crnSpecies"://species
                    case "crnReactions"://reactions
                    case "crnInferenceGraph": // inference graph
                        operations = [exportOperation, selectInferTab, selectModelDynamicsTab, crnInferOperation];
                        break;
                    default:
                        throw "not implemented yet";
                }
        }
        longOperationsManager.EnqueueOperations(operations);
    }

    document.getElementById('stopButton').onclick = function (e) {
        longOperationsManager.Stop();
    }

    //serviceWorker.Bind(document.getElementById('worker'));

    Utils.RemoveLoadingOverlay();

    console.log("App loaded");
}