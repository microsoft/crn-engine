import * as GenericCRNParser from '../Adapters/GenericCRNParser';
import ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as Interfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import CRNSelector from '../../../CRNComponent/Scripts/CRNSelector';
import * as Operations from "./LongOperations";
import * as jQuery from 'jquery';

export interface IModelSource {
    getModel(): Interfaces.IG;
}

/** Displays the CRN exports with GUI */
export interface IExportsViewer {
    SetModel(crn: Interfaces.IG): void;
    SetNodeID(nodeId: string): void;
    ShowExport(update: Interfaces.ExportDef): void;
    Reset(): void;
}

/** Uses ModellingEngine.ts layer to generate exports from a CRN */
export class Exporter implements Operations.IOperation {
    constructor(private me: ME, private crnsource: IModelSource, private nodeselector: CRNSelector, private exportViewer: IExportsViewer) {
        ko.computed(() => this.exportViewer.SetNodeID(this.nodeselector.SelectedNode().Top().name()));
    };

    Initiate(): JQueryPromise<any> {
        this.exportViewer.Reset();
        var dfd = jQuery.Deferred<void>();
        var model: Interfaces.IG = this.crnsource.getModel();
        var nodeId: string = this.nodeselector.getSelectedNode();
        this.exportViewer.SetModel(model);
        var observables = this.me.UserGenerateExports(model, nodeId);
        observables.exports.subscribe(exp => {
            this.exportViewer.ShowExport(exp);
        }, error => {
            if (error.message == null)
                this.exitMessage = "failed";
            else
                this.exitMessage = "failed: " + error.message;
            dfd.reject(error);
        }, () => {
            this.exitMessage = "completed successfully";
            dfd.resolve();
        });
        return dfd;
    }

    Abort(): void {
        this.me.Abort();
    }

    GetName(): string {
        return "Generating exports";
    }

    private exitMessage: string;

    GetExitMessage(): string {
        return this.exitMessage;
    }
}