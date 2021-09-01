// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//Feature descriotion:
//Gets CRN entities from the CRN Editor, passes them to the simulator, passes simulation info back to the Simulation viewer

import * as Rx from "rx";
import * as crn from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as HintScreen from "../../../GenericComponents/Scripts/HintScreen";
import * as Operations from "./LongOperations";
import * as SimViewer from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/SimulationViewerFramework';
import * as SpatialRunner from '../Adapters/ModellingEngineSpatial1DSimulationRunner';
import SpatialViewer from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/Spatial1DViewer';

//provides the CRN entities
export interface IModelSource {
    getModel(): crn.IG;
}

export interface INodeSelector {
    getSelectedNode(): string;
}

export interface ISimulationRunner {
    SimulateModel(model: crn.IG, nodeId: string, server: boolean): Rx.Observable<SimViewer.IVisualizationUpdateMessage>;
    Abort(): void;
}

export class Operation implements Operations.IOperation, HintScreen.IHintRemoveNotifier {
    private ongoingPromise: JQueryDeferred<void> = undefined;
    private exitMessage = "";
    constructor(
        private crnSource: IModelSource,
        private nodeSelector: INodeSelector,
        private simRunner: ISimulationRunner,
        private simViewer: SpatialViewer
    ) { }

    //Operations.IOperation implementation
    public GetName() {
        return "Spatial1DSimulation";
    }

    public GetExitMessage() {
        return this.exitMessage;
    }

    public GetViewer() {
        return this.simViewer;
    }

    public Initiate(server?: boolean): JQueryPromise<any> {
        this.ongoingPromise = jQuery.Deferred<void>();

        var model = this.crnSource.getModel();
        var nodeId = this.nodeSelector.getSelectedNode();
        var simulationUpdates = this.simRunner.SimulateModel(model, nodeId, server);
        var first = true;
        simulationUpdates.subscribe(next => {
            if (first) {
                first = false;
                for (let cb of this.notificationCallbacks)
                    cb();
            }
            this.simViewer.Post(next);
        }, error => {
            this.exitMessage = error.message == null ? JSON.stringify(error) : error.message;
            this.ongoingPromise.reject(error);
            this.ongoingPromise = undefined;
        }, () => {
            this.exitMessage = "completed successfully";
            this.ongoingPromise.resolve();
            this.ongoingPromise = undefined;
        });
        return this.ongoingPromise;
    }

    public Abort() {
        this.simRunner.Abort();
        this.ongoingPromise = undefined;
    }

    //HintScreen.IHintRemoveNotifier implementation
    private notificationCallbacks: Array<() => void> = [];
    public SubscribeRemoveHint(callback: () => void) {
        this.notificationCallbacks.push(callback);
    }
}
