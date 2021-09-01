// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Rx from 'rx';
import * as $ from 'jquery';
import * as Operations from "./LongOperations";
import * as crn from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";

export interface IModelSource {
    getModel(): crn.IG;
}

export interface ISSAEngine {
    Analyse(model: crn.IG, jit: boolean): JQueryPromise<crn.StateSpace>;
    Abort(): void;
}

export interface ISSAViewer {
    Show(stateSpace: crn.StateSpace): void;
}

/**
 * Long operation of State Space Analysis.
 * Takes the CRN object form ICRNSource, passes it to ISSAEngine, transmits the results to ISSAViewer
 */
export class Operation implements Operations.IOperation {
    private ongoingPromise: JQueryDeferred<void> = undefined;
    private exitMessage = "";

    /**
     * @param modelSource CRN entities with model specification     
     * @param engine The actual runner of state space analysis operation.
     * @param viewer Represents the results of State Space Analysis graph     
     */
    constructor(private modelSource: IModelSource, private engine: ISSAEngine, private viewer: ISSAViewer, private jit: boolean) {
    }

    //Operations.IOperation implementation
    public GetName() {
        return "State space analysis";
    }

    public GetExitMessage() {
        return this.exitMessage;
    }

    public Initiate(): JQueryPromise<any> {
        this.ongoingPromise = jQuery.Deferred<void>();

        var model = this.modelSource.getModel();

        //submiting observations to the viewer
        var run = this.engine.Analyse(model, this.jit);

        run.done((stateSpace: crn.StateSpace) => {
            this.viewer.Show(stateSpace);
            this.exitMessage = "completed successfully";
            this.ongoingPromise.resolve();
            this.ongoingPromise = undefined;
        }).fail((error:any) => {
            this.exitMessage = error;
            this.ongoingPromise.reject(error);
            this.ongoingPromise = undefined;
        });

        return this.ongoingPromise;
    }

    public Abort() {
        this.engine.Abort();
        this.ongoingPromise = undefined;
    }
}
