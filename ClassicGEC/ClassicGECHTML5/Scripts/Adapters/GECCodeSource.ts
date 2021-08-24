// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import Editor from '../GECCodeEditor';
import * as ModificationIndicator from '../../../../HTML5SharedGUI/GenericComponents/Scripts/ModificationIndicators';
import * as Features from '../Features';
import * as KOCodeEditor from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCodeEditor';
import * as GenericParseOperation from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/GenericCodeParsing';
import { INamedMonarchLanguage } from '../../../../HTML5SharedGUI/CodeEditor/Scripts/CodeEditor';
import * as ko from 'knockout';

export class CodeEditor extends KOCodeEditor.KOCodeEditor<{}> implements GenericParseOperation.ICodeSource<{}>, GenericParseOperation.IErrorDisplay, Features.IUIAutoBindable {
    constructor(private language: INamedMonarchLanguage, private examples: Array<ExamplesGroup>, modifIndicator: ModificationIndicator.IModificationIndicator, options: {}) {
        super(modifIndicator, options);
    }

    //Features.IUIAutoBindable implementation
    public AutoBind() {
        //CodeEditor create        
        var codeEditor = new Editor(this.language, this.examples);
        ko.applyBindings(this.vm, document.getElementById('gecCode'));
    }
}