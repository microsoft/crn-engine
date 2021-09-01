// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import Editor from '../DsdGenericCodeEditor';
import * as ModificationIndicator from '../../../../HTML5SharedGUI/GenericComponents/Scripts/ModificationIndicators';
import * as Features from '../Features';
import * as KOCodeEditor from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCodeEditor';
import * as GenericParseOperation from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/GenericCodeParsing';
import { INamedMonarchLanguage } from '../../../CodeEditor/Scripts/CodeEditor';
import * as ko from 'knockout';

export class CodeEditor<TOptions> extends KOCodeEditor.KOCodeEditor<TOptions> implements GenericParseOperation.ICodeSource<TOptions>, GenericParseOperation.IErrorDisplay, Features.IUIAutoBindable {
    constructor(private language: INamedMonarchLanguage, private examples: Array<ExamplesGroup>, modifIndicator: ModificationIndicator.IModificationIndicator, options: TOptions) {
        super(modifIndicator, options);
    }

    //Features.IUIAutoBindable implementation
    public AutoBind() {
        //CodeEditor create        
        var codeEditor = new Editor(this.language, this.examples);
        ko.applyBindings(this.vm, document.getElementById('dsdCode'));
    }
}
