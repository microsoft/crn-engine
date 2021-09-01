// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as I from '../../../GenericComponents/Scripts/Interfaces';
import * as ModificationIndicator from '../../../GenericComponents/Scripts/ModificationIndicators';
import * as GenericParseOperation from '../Operations/GenericCodeParsing';
import * as ko from 'knockout';
import * as Rx from 'rx';

export interface IHighlight {
    rowStart: number;
    colStart: number;
    rowEnd: number;
    colEnd: number;
}

export interface IError {
    location: IHighlight;
    text: string;
}

export class CodeVM<TOptions> {
    public editorText = ko.observable("");
    public options = ko.observable<TOptions>();
    public errors = ko.observableArray<IError>([]);
    public editorLoadEvents = new Rx.Subject<string>();
}

export abstract class CodeEditorBasedCodeSource<TOptions> implements GenericParseOperation.ICodeSource<TOptions>, GenericParseOperation.IErrorDisplay, I.IUIAutoBindable {
    protected vm: CodeVM<TOptions> = new CodeVM<TOptions>();

    constructor(private modifIndicator: ModificationIndicator.IModificationIndicator, options: TOptions) {
        this.vm.options(options);
        this.vm.editorText.subscribe((dummy) => {
            modifIndicator.SetModified();
        });
        this.EditorLoadEvents = this.vm.editorLoadEvents;
    }
    
    public EditorLoadEvents: Rx.Observable<string>;

    private lastUnmodifiedText: string = "";

    //ParseFeature.ICRNCodeSource implementation
    public GetCode() {
        this.ClearModificationIndicator();
        return this.vm.editorText();
    }

    // Removes the modification indicator.
    public ClearModificationIndicator() {
        this.lastUnmodifiedText = this.vm.editorText();
        this.modifIndicator.Clear();
    }

    // Resets text to the last time the modification indicator was cleared.
    public ResetToUnmodified() {
        this.vm.editorText(this.lastUnmodifiedText);
        this.ClearModificationIndicator();
    }

    public GetParserOptions() {
        return this.vm.options();
    }

    //ParseOperation.IErrorDisplay implementation
    public ShowErrors(errors: Array<IError>) {
        this.vm.errors(errors);
    }
    public ClearErrors() {
        this.vm.errors.removeAll();
    }

    //Features.IUIAutoBindable implementation
    public abstract AutoBind(): void;
}
