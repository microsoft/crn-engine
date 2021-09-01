// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as HintScreen from "../../../GenericComponents/Scripts/HintScreen";
import * as Operations from "./LongOperations";
import "idd";
declare var InteractiveDataDisplay: any;

export enum SimulationType {
    nonspatial,
    spatial1D,
    spatial2D
}

/**
 * This operation looks into CRN object upon initiation to check whether the required simulation is spatial or not and determines dimension if spatial.
 * Upon the check the call is delegated either to spatial simulation or non-spatial simulation operation. 
 */
export class Operation implements Operations.IOperation, HintScreen.IHintRemoveNotifier {
    constructor(
        private method: () => SimulationType,
        private spatial1DSimulationOperation: Operations.IOperation,
        private spatial1DSimulationView: JQuery,
        private spatial2DSimulationOperation: Operations.IOperation,
        private spatial2DSimulationView: JQuery,
        private nonspatialSimulationOperation: Operations.IOperation,
        private nonspatialSimulationView: JQuery
    ) { }

    private ongoingOperation: Operations.IOperation = null;

    //Operations.IOperation implementation
    public GetName() {
        if (this.ongoingOperation)
            return this.ongoingOperation.GetName();
        else
            return "";
    }

    public GetExitMessage() {
        if (this.ongoingOperation)
            return this.ongoingOperation.GetExitMessage();
        else
            return "";
    }

    public Initiate(): JQueryPromise<any> {
        this.notificationCallbacks.forEach(c => { c(); });
        var toRefresh: JQuery;
        switch (this.method()) {
            case SimulationType.spatial1D:
                console.log("Initiation 1D spatial simulation");
                this.ongoingOperation = this.spatial1DSimulationOperation;
                this.spatial1DSimulationView.show();
                this.spatial2DSimulationView.hide();
                this.nonspatialSimulationView.hide();
                toRefresh = this.spatial1DSimulationView;
                break;
            case SimulationType.spatial2D:
                console.log("Initiation 2D spatial simulation");
                this.ongoingOperation = this.spatial2DSimulationOperation;
                this.spatial1DSimulationView.hide();
                this.spatial2DSimulationView.show();
                this.nonspatialSimulationView.hide();
                toRefresh = this.spatial2DSimulationView;
                break;
            case SimulationType.nonspatial:
                console.log("Initiating non-spatial simulation");
                this.ongoingOperation = this.nonspatialSimulationOperation;
                this.spatial1DSimulationView.hide();
                this.spatial2DSimulationView.hide();
                this.nonspatialSimulationView.show();
                toRefresh = this.nonspatialSimulationView;
                break;
            default:
                throw "there is no corresponding operation for such simulation type";
        }

        // Enforce refresh of tabs.
        toRefresh.parents(".j-has-tabs").tabs("refresh");
        $(".j-has-tabs", toRefresh).tabs("refresh");
        InteractiveDataDisplay.updateLayouts($(".j-has-tabs", toRefresh));

        return this.ongoingOperation.Initiate();
    }

    public Abort() {
        if (this.ongoingOperation)
            this.ongoingOperation.Abort();
    }

    //HintScreen.IHintRemoveNotifier implementation
    private notificationCallbacks: Array<() => void> = [];
    public SubscribeRemoveHint(callback: () => void) {
        this.notificationCallbacks.push(callback);
    }
}
