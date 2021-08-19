///<reference path="./Interfaces.ts"/>

// (FP) This file contains the logic for the full-fledged editor (i.e. including buttons and stuff).

import * as CodeEditor from "./CodeEditor";
import * as $ from "jquery";
import * as ko from "knockout";
import * as Rx from "rx";
import { saveAs } from "file-saver";
import "./CodeExamples";

// The HTML template for the editor.
import * as padTemplate from "raw-loader!./../Templates/code-pad-template.html";

interface IError {
    location: CodeEditor.IHighlight;
    text: string;
}

/** (FP) This class handles the binding between the HTML elements and the back-end. Note that the actual event handlers are in the CodeEditor file. The code in that file is invoked by the creation of a code-editor-widget in the template. The event handlers are passed to it in the "params" field. Note that this class respects the ICodeEditorParams interface, but does not technically implement it. This because it is not this class instance that gets passed to the code-editor-widget, but rather a params object of which every field is bound to the observables in this class. */
class CodePadViewModel {
    editorText: KnockoutObservable<string>;
    cutEvent: KnockoutObservable<any> = ko.observable();
    copyEvent: KnockoutObservable<any> = ko.observable();
    pasteEvent: KnockoutObservable<any> = ko.observable();
    filesInput: HTMLInputElement;
    undoEvent: KnockoutObservable<any> = ko.observable();
    redoEvent: KnockoutObservable<any> = ko.observable();
    findEvent: KnockoutObservable<any> = ko.observable();
    replaceEvent: KnockoutObservable<any> = ko.observable();
    outdentEvent: KnockoutObservable<any> = ko.observable();
    indentEvent: KnockoutObservable<any> = ko.observable();
    lineNumbersEvent: KnockoutObservable<any> = ko.observable();
    examplesCorrespondence: ExamplesGroup[];
    examplesVisibility = ko.observable(true);
    errors: KnockoutObservableArray<IError>;
    public highlight: KnockoutComputed<CodeEditor.IHighlight> = ko.pureComputed(() => this.errors().length == 0 ? null : this.errors()[0].location);
    constructor(
        externalText: KnockoutObservable<string>,
        public language: CodeEditor.INamedMonarchLanguage,
        examplesCorrespondence: ExamplesGroup[],
        errors: KnockoutObservableArray<IError>,
        public editorLoadEvents: Rx.IObserver<string>
    ) {
        // (FP) Hide the examples if there are no examples.
        if (Object.keys(examplesCorrespondence).length == 0) {
            this.examplesVisibility(false);
        }
        this.examplesCorrespondence = examplesCorrespondence;
        // (FP) Initialize the editor with some text, if required.
        if (externalText == undefined) {
            this.editorText = ko.observable("");
        } else {
            this.editorText = externalText;
        }
        // (FP) Initialize the editor with some errors, if required.
        this.errors = errors ? errors : ko.observableArray([]);
        // (FP) The "load" button is not an actual HTML input element; it's a button. It's easier to control the aspect this way. However, we want it to behave like an input element, so we'll create an input element that's not on the DOM, and map clicks on the button to clicks on the input.
        this.filesInput = document.createElement("input");
        this.filesInput.type = "file";
        var that = this;
        $(this.filesInput).change(function () {
            // (FP) Load the file; this is an asynchronous operation.
            var file = that.filesInput.files[0];
            var reader = new FileReader();
            reader.onload = function (event) {
                var text = (<any>event.target).result;
                that.editorText(text);
                if (that.editorLoadEvents != null)
                    that.editorLoadEvents.onNext(file.name);
            }
            reader.readAsText(file);
        });
    }
    // (FP) Some actions are handled here directly. These include new, load and save. The rest of the actions are handled in CodeEditor.ts.
    newText(): void {
        this.editorText("");
    }
    loadText(): void {
        this.filesInput.click();
    }
    saveText(): void {
        var text = this.editorText();
        var blob = new Blob([text], { type: "text/txt" });
        saveAs(blob, "code.txt");
    }
}

// (FP) This class tells KO how to build a code editor (for a specific language).
class PadViewerConfig implements KnockoutComponentTypes.Config {
    examplesCorrespondence: ExamplesGroup[]
    constructor(
        examplesCorrespondence: ExamplesGroup[],
        private language: any
    ) {
        this.examplesCorrespondence = examplesCorrespondence;
    }
    get template(): KnockoutComponentTypes.AMDModule {
        return padTemplate;
    }
    get viewModel(): KnockoutComponentTypes.ViewModelFactoryFunction {
        var that = this;
        return {
            createViewModel: function (params, componentInfo) {
                // (FP) Setup the toolbar icons. Not sure why this can't be done in HTML alone.
                var toolbar = $(componentInfo.element).children(".c-codepad").children(".c-codepad__toolbar");
                $(".btn--new", toolbar).button({
                    icons: {
                        primary: "codepad-button-clear"
                    },
                    text: false
                });
                $(".btn--load", toolbar).button({
                    icons: {
                        primary: "codepad-button-load"
                    },
                    text: false
                });
                $(".btn--save", toolbar).button({
                    icons: {
                        primary: "codepad-button-save"
                    },
                    text: false
                });
                $(".btn--undo", toolbar).button({
                    icons: {
                        primary: "codepad-button-undo"
                    },
                    text: false
                });
                $(".btn--redo", toolbar).button({
                    icons: {
                        primary: "codepad-button-redo"
                    },
                    text: false
                });
                $(".btn--cut", toolbar).button({
                    icons: {
                        primary: "codepad-button-cut"
                    },
                    text: false
                });
                $(".btn--copy", toolbar).button({
                    icons: {
                        primary: "codepad-button-copy"
                    },
                    text: false
                });
                $(".btn--paste", toolbar).button({
                    icons: {
                        primary: "codepad-button-paste"
                    },
                    text: false
                });
                $(".btn--find", toolbar).button({
                    icons: {
                        primary: "codepad-button-find"
                    },
                    text: false
                });
                $(".btn--replace", toolbar).button({
                    icons: {
                        primary: "codepad-button-replace"
                    },
                    text: false
                });
                $(".btn--outdent", toolbar).button({
                    icons: {
                        primary: "codepad-button-outdent"
                    },
                    text: false
                });
                $(".btn--indent", toolbar).button({
                    icons: {
                        primary: "codepad-button-indent"
                    },
                    text: false
                });
                $(".btn--line-numbers", toolbar).button({
                    icons: {
                        primary: "codepad-button-line-numbers"
                    },
                    text: false
                });
                var vm = new CodePadViewModel(params.editorText, that.language, that.examplesCorrespondence, params.errors, params.editorLoadEvents);
                return vm;
            }
        }
    }
}

// (FP) The constructor for this class registers the component with KO.
class CodePad extends CodeEditor.Editor {
    constructor(language: any, componentName: string, examplesCorrespondence: ExamplesGroup[] = []) {
        // (FP) The superclass constructor will register the text editor. Each language has its own editor as a separate component, so the component name is provided. Apart from that, the caller needs to provide the language, and a set of examples.
        super();
        if (!ko.components.isRegistered(componentName)) {
            ko.components.register(componentName, new PadViewerConfig(examplesCorrespondence, language));
        }
    }
}

monaco.editor.defineTheme('crn', {
    base: 'vs',
    inherit: true,
    rules: [
        { token: 'number', foreground: '800080' },
        { token: 'operator', foreground: 'B22222' },
        { token: 'delimiter', foreground: 'B22222' },
    ],
    colors: {}
});

export default CodePad;