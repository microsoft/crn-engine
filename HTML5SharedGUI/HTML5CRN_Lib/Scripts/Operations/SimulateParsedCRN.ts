// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//Feature descriotion:
//Gets CRN entities from the CRN Editor, passes them to the simulator, passes simulation info back to the Simulation viewer

import * as Rx from "rx";
import * as crn from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as HintScreen from "../../../GenericComponents/Scripts/HintScreen";
import * as Operations from "./LongOperations";

//Dependencies to be injected

//provides the CRN entities
export interface IModelSource {
    getModel(): crn.IG;
}

export interface INodeSelector {
    getSelectedNode(): string;
}

export interface ISimulationRunner<SimulationUpdateMessage> {
    SimulateModel(model: crn.IG, nodeId: string): Rx.Observable<SimulationUpdateMessage>;
    Abort(): void;
}

export interface ISimulationViewer<VisualizationUpdateMessage> {
    Display(visualization: Rx.Observable<VisualizationUpdateMessage>): void;
}

/** Displays the CRN entities with GUI */
export interface ICRNViewer<TCustomSettings> {
    UpdateValuesWith(update: crn.IG, customSettings: TCustomSettings, fromJIT: boolean): void;
}

/** Displays the CRN exports with GUI */
export interface IExportsViewer {
    ShowExport(update: crn.ExportDef): void;
}

export interface ISSAViewer {
    Show(stateSpace: crn.StateSpace): void;
}

//end of dependencies to be injected

export class Operation<SimulationUpdateMessage, VisualizationUpdateMessage, TCustomSettings>
    implements Operations.IOperation, HintScreen.IHintRemoveNotifier {

    private ongoingPromise: JQueryDeferred<void> = undefined;
    private exitMessage = "";
    constructor(
        private modelSource: IModelSource,
        private nodeSelector: INodeSelector,
        private simRunner: ISimulationRunner<SimulationUpdateMessage>,
        private simViewer: ISimulationViewer<VisualizationUpdateMessage>,
        private ssViewer: ISSAViewer,
        private probViewer: ISimulationViewer<VisualizationUpdateMessage>,
        private crnViewer: ICRNViewer<TCustomSettings>,
        private exports: IExportsViewer,
        private unitsExtractor: () => VisualizationUpdateMessage,
        private plotSettingsExtractor: () => VisualizationUpdateMessage,
        private SUMtoModel: ((sum: SimulationUpdateMessage) => crn.Model),
        private SUMtoExport: ((sum: SimulationUpdateMessage) => crn.ExportDef),
        private SUMtoStateSpace: ((sum: SimulationUpdateMessage) => crn.StateSpace),
        private SUMtoVUM: ((sum: SimulationUpdateMessage) => VisualizationUpdateMessage)
    ) { }

    //Operations.IOperation implementation
    public GetName() {
        return "Simulation";
    }

    public GetExitMessage() {
        return this.exitMessage;
    }

    public GetViewer(): ISimulationViewer<VisualizationUpdateMessage> {
        return this.simViewer;
    }

    public Initiate(): JQueryPromise<any> {
        var promise = jQuery.Deferred<void>();
        this.ongoingPromise = promise;

        var model = this.modelSource.getModel();
        var nodeId = this.nodeSelector.getSelectedNode();
        var simulationUpdates = this.simRunner.SimulateModel(model, nodeId);

        simulationUpdates.subscribe(update => {
            var node = this.SUMtoModel(update);
            if (node != null) {
                model.nodes[nodeId] = node;
                this.crnViewer.UpdateValuesWith(model, null, true);
            }
            var exp = this.SUMtoExport(update);
            if (exp != null)
                this.exports.ShowExport(exp);
            var ss = this.SUMtoStateSpace(update);
            if (ss != null)
                this.ssViewer.Show(ss);
        }, error => {
            this.exitMessage = error.message == null ? JSON.stringify(error) : error.message;
            this.ongoingPromise.reject(error);
            this.ongoingPromise = undefined;
        }, () => {
            this.exitMessage = "completed successfully";
            this.ongoingPromise.resolve();
            this.ongoingPromise = undefined;
        });

        simulationUpdates.take(1).subscribe(update => { //first update hides the hint screens
            this.notificationCallbacks.forEach(c => { c(); });
        }, error => { }, () => { });

        var unitsMessage = Rx.Observable.return<VisualizationUpdateMessage>(this.unitsExtractor());
        var plotSettingsMessage = Rx.Observable.return<VisualizationUpdateMessage>(this.plotSettingsExtractor());

        var visualizationUpdates =
            unitsMessage
                .concat(plotSettingsMessage)
                .concat(simulationUpdates.map<VisualizationUpdateMessage>(update => this.SUMtoVUM(update)).where(update => update != null));

        this.simViewer.Display(visualizationUpdates);
        if (this.probViewer != null) {
            this.probViewer.Display(visualizationUpdates);
        }

        return promise;
    }

    public Abort() {
        this.simRunner.Abort();
        if (this.ongoingPromise) {
            this.ongoingPromise.reject("Simulation aborted");
            this.ongoingPromise = undefined;
        }
    }

    //HintScreen.IHintRemoveNotifier implementation
    private notificationCallbacks: Array<() => void> = [];
    public SubscribeRemoveHint(callback: () => void) {
        this.notificationCallbacks.push(callback);
    }
}
