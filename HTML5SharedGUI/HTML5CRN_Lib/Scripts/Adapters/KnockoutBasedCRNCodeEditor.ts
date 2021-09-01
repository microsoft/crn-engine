// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import CRNEditor from '../Components/CRNCodeEditor';
import * as I from '../../../GenericComponents/Scripts/Interfaces';
import * as GenericParseOperation from '../Operations/GenericCodeParsing';
import * as KOCodeEditor from "./KnockoutBasedCodeEditor";
import * as ModificationIndicator from "../../../GenericComponents/Scripts/ModificationIndicators";
import * as ko from "knockout";

export class KOCRNCodeEditor extends KOCodeEditor.KOCodeEditor<void> implements GenericParseOperation.ICodeSource<void>, GenericParseOperation.IErrorDisplay, I.IUIAutoBindable {
    constructor(modifIndicator: ModificationIndicator.IModificationIndicator) {
        super(modifIndicator, null);
    }

    //Features.IUIAutoBindable implementation
    public AutoBind() {
        //CodeEditor create
        var codeEditor = new CRNEditor();
        ko.applyBindings(this.vm, document.getElementById('crnCode'));
    }
}
