// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as $ from 'jquery';
import GEC from '../../../ClassicGECTSWrapper/Scripts/ClassicGEC';
import * as Interfaces from '../../../ClassicGECTSWrapper/Scripts/Interfaces';
import * as CRN from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as Operations from "../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/LongOperations";
import * as GenericParsing from "../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/GenericCodeParsing";

export interface IExportsViewer {
    ShowExport(update: CRN.ExportDef): void;
}

export class Operation<TOptions> implements Operations.IOperation {
    constructor(
        private gec: GEC,
        private codeSource: GenericParsing.ICodeSource<void>,
        private partsSource: GenericParsing.ICodeSource<void>,
        private reactionsSource: GenericParsing.ICodeSource<void>,
        private showSolutionsCount: (count: number) => void,
        private showSBOL: (jsbol: Interfaces.jSBOLDocument) => void,
        private exports: IExportsViewer,
        private codeErrorDisplay: GenericParsing.IErrorDisplay,
        private partsErrorDisplay: GenericParsing.IErrorDisplay,
        private reactionsErrorDisplay: GenericParsing.IErrorDisplay) {
    }

    public Initiate(): JQueryPromise<any> {
        this.gec.Parts = this.partsSource.GetCode();
        this.gec.Reactions = this.reactionsSource.GetCode();
        var code = this.codeSource.GetCode();
        var observables = this.gec.UserCompile(code);
        var deferred = jQuery.Deferred<any>();
        observables.solution_count.subscribe(next => {
            this.showSolutionsCount(next);
            this.exitMessage = "completed with " + (next == 0 ? "no" : next.toString()) + " solution" + (next == 1 ? "" : "s");
            this.codeErrorDisplay.ClearErrors();
            this.partsErrorDisplay.ClearErrors();
            this.reactionsErrorDisplay.ClearErrors();
            deferred.resolve(next);
        }, error => {
            this.showSolutionsCount(0);
            var errorString: string = error.message == null ? JSON.stringify(error) : error.message;
            this.exitMessage = "failed with " + errorString;
            var errorObj: GenericParsing.IError = { text: errorString };
            if (error.positions != null && error.positions.length > 0) {
                var row = error.positions[0].row;
                var column = error.positions[0].column + 1;
                errorObj.text += " at row " + row + ", column " + column;
                var len = error.positions[0].text.length;
                errorObj.location = { rowStart: row, rowEnd: row, colStart: column, colEnd: column };
            }
            if (errorString.substr(0, 5) == "parts")
                this.partsErrorDisplay.ShowErrors([errorObj]);
            else if (errorString.substr(0, 9) == "reactions")
                this.reactionsErrorDisplay.ShowErrors([errorObj]);
            else
                this.codeErrorDisplay.ShowErrors([errorObj]);
            deferred.reject(error);
        }, () => { });
        observables.jsbol.subscribeOnNext(jsbol => this.showSBOL(jsbol));
        observables.exports.subscribe(exp => this.exports.ShowExport(exp), err => { }, () => { });
        return deferred;
    }

    public Abort(): void {
        this.gec.Abort();
    }

    public GetName(): string {
        return "GEC parsing";
    }

    private exitMessage: string;
    public GetExitMessage(): string {
        return this.exitMessage;
    }
}