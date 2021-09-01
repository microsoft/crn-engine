// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as CodeSource from "./CodeEditorCodeSource";
import * as I from '../../../GenericComponents/Scripts/Interfaces';
import * as GenericParseOperation from '../Operations/GenericCodeParsing';
import * as ModificationIndicator from "../../../GenericComponents/Scripts/ModificationIndicators";
import * as ko from "knockout";

//shows the crn code that is produced by StrandGraphs translation
export interface ICodeViewer {
    ShowCode(code: string): void; 
} 

//extends the CEN code source to be avle to show CRN code as well
export abstract class KOCodeEditor<TOptions> extends CodeSource.CodeEditorBasedCodeSource<TOptions> implements GenericParseOperation.ICodeSource<TOptions>, GenericParseOperation.IErrorDisplay, ICodeViewer, I.IUIAutoBindable {
    constructor(modifIndicator: ModificationIndicator.IModificationIndicator, options: TOptions) {
        super(modifIndicator, options);
    }

    //SgParseOp.ICodeViewer implementation
    ShowCode(code: string) {
        this.vm.editorText(code);
    }
    
    //other interfaces implemented in the base class
}
