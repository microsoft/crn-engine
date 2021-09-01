// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as GenericParseOperation from '../Operations/GenericCodeParsing';
import * as KOCodeEditor from './KnockoutBasedCodeEditor';
import * as ko from 'knockout';
import * as Rx from 'rx';

export class CodeEditorStorageDecorator<TOptions> implements GenericParseOperation.ICodeSource<TOptions>, KOCodeEditor.ICodeViewer{
    private static storage = window.localStorage;

    constructor(private codeEditor: GenericParseOperation.ICodeSource<TOptions> & KOCodeEditor.ICodeViewer, private codeType: string) {
        let code = CodeEditorStorageDecorator.storage.getItem(this.codeType + "PersistedCode");
        this.ShowCode(code);
        this.EditorLoadEvents = codeEditor.EditorLoadEvents;
    }

    //ParseFeature.ICRNCodeSource implementation
    public GetCode(): string {
        let code = this.codeEditor.GetCode();
        CodeEditorStorageDecorator.storage.setItem(this.codeType + "PersistedCode", code);
        return code;
    }

    public GetParserOptions(): TOptions {
        return this.codeEditor.GetParserOptions();
    }

    public ShowCode(code: string): void {
        this.codeEditor.ShowCode(code);
    }

    public EditorLoadEvents: Rx.Observable<string>;
}
