import * as Rx from 'rx';
import * as $ from 'jquery';
import * as Operations from "./LongOperations";
import * as crn from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";

export interface ICRNSource {
    getModel(): crn.IG;
    getNodeID(): string;
    getCRNID(): string;
}

export interface ISynthesisEngine {
    Synthesize(ig: crn.IG, nodeId: string, crnId: string): JQueryPromise<crn.SynthesisResult>;
    Abort(): void;
}

export interface ISynthesisViewer {
    Show(result: crn.SynthesisResult): void;
}

/**
 * Long operation of Z3 Synthesis.
 * Takes the CRN object form ICRNSource, passes it to ISynthesisEngine, transmits the results to ISynthesisViewer
 */
export class Operation implements Operations.IOperation {
    private ongoingPromise: JQueryDeferred<void> = undefined;
    private exitMessage = "";

    /**
     * @param crnSource provider of a CRN to execute the method on.
     * @param engine The actual runner of the synthesis operation.
     * @param viewer Shows the results of the synthesis operation.
     */
    constructor(private crnSource: ICRNSource, private engine: ISynthesisEngine, private viewer: ISynthesisViewer) {
    }

    //Operations.IOperation implementation
    public GetName() {
        return "Z3 Synthesis";
    }

    public GetExitMessage() {
        return this.exitMessage;
    }

    public Initiate(): JQueryPromise<any> {
        this.ongoingPromise = jQuery.Deferred<void>();

        var model = this.crnSource.getModel();
        var nodeId = this.crnSource.getNodeID();
        var crnId = this.crnSource.getCRNID();

        //submiting observations to the viewer
        var run = this.engine.Synthesize(model, nodeId, crnId);

        run.done((synthesisResult: crn.SynthesisResult) => {
            this.viewer.Show(synthesisResult);
            this.exitMessage = "completed with: " + synthesisResult.message;
            this.ongoingPromise.resolve();
            this.ongoingPromise = undefined;
        }).fail((error: any) => {
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