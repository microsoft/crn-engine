// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as $ from 'jquery';
import GEC from '../../../ClassicGECTSWrapper/Scripts/ClassicGEC';
import * as Interfaces from '../../../ClassicGECTSWrapper/Scripts/Interfaces';
import * as CRN from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as Operations from "../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/LongOperations";
import * as HintScreen from "../../../../HTML5SharedGUI/GenericComponents/Scripts/HintScreen";

//Dependencies to be injected

export interface ICodeViewer {
    ShowCode(code: string): void;
}

/** Displays the CRN entities with GUI */
export interface ICRNViewer {
    UpdateValuesWith(update: CRN.IG, customSettings: void, fromJIT: boolean): void;
}

export interface IExportsViewer {
    ShowExport(update: CRN.ExportDef): void;
}

/**
 * Gets the code from the CodeEditor, passes it to the parser, receives CRN entities, updates CRN viewer with them, updates data files selecting control with parsed "data directive" files.
 * If any parsing error occurs, displays it with errorDisplay
 */
export class Operation implements Operations.IOperation, HintScreen.IHintRemoveNotifier {
    constructor(
        private getIdx: () => number,
        private gec: GEC,
        private crnCodeViewer: ICodeViewer,
        private crnViewer: ICRNViewer,
        private showSBOL: (jsbol: Interfaces.jSBOLDocument) => void,
        private exports: IExportsViewer) {
    }

    public Initiate(): JQueryPromise<any> {
        var deferred = $.Deferred();
        var idx = this.getIdx();
        if (idx == -1) {
            this.m_ExitMessage = "no solutions found";
            deferred.reject();
        }
        else {
            var observables = this.gec.UserGetSolution(idx);
            var error = (err: any) => {
                this.m_ExitMessage = JSON.stringify(err);
                deferred.reject();
            }
            observables.solution.subscribe(solution => {
                this.m_ExitMessage = "Solution " + idx + " selected";
                this.InterpretSuccessfulResults(solution);
                deferred.resolve();
            }, error, () => { });
            observables.jsbol.subscribe(jsbol => this.showSBOL(jsbol), err => { }, () => { });
            observables.exports.subscribe(exp => this.exports.ShowExport(exp), err => { }, () => { });
        }
        return deferred;
    }

    public Abort() {
        this.gec.Abort();
        this.m_ExitMessage = "Aborted";
    }

    //defining abstract functions
    public GetName() {
        return "Retrieving GEC solution";
    }

    private m_ExitMessage: string;

    public GetExitMessage() {
        return this.m_ExitMessage;
    }

    protected InterpretSuccessfulResults(solution: Interfaces.GECSolution) {
        this.crnViewer.UpdateValuesWith(solution.model, null, false);
        this.crnCodeViewer.ShowCode(solution.code);
        this.notificationCallbacks.forEach(c => { c() }); // notifing that hints now can be removed
    }

    //HintScreen.IHintRemoveNotifier implementation
    private notificationCallbacks: Array<() => void> = [];

    public SubscribeRemoveHint(callback: () => void) {
        this.notificationCallbacks.push(callback);
    }
}

