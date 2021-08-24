///<reference path="./Interfaces.ts"/>

// (FP) The code in this file represents the bare editor, by which I mean the text box alone. This has its own KO component, called code-editor-widget. The full editor, which is in CodePad.ts, makes use of it.

///<reference path="../../../node_modules/monaco-editor/monaco.d.ts"/>
import * as monaco from '../../../node_modules/monaco-editor/esm/vs/editor/editor.api';
import "jqueryui";
import * as $ from "jquery";
import * as ko from "knockout";
// The HTML template for the editor.
import * as editorTemplate from "raw-loader!./../Templates/code-editor-template.html";

// (FP) This interface represents the language (mostly, this is from Monaco).
export interface INamedMonarchLanguage extends monaco.languages.IMonarchLanguage {
    name: string;
}

export interface IHighlight {
    rowStart: number;
    colStart: number;
    rowEnd: number;
    colEnd: number;
}

// (FP) This interface is how the bare editor talks to the outside world (including e.g. the text editor buttons). Note that there are no objects that implement this interface in JS code, because it is implemented by the "params" property of the code-editor-widget instances declared in HTML code. This is why the interface can be private.
interface ICodeEditorParams {
    language: INamedMonarchLanguage;
    undoEvent: KnockoutObservable<any>;
    redoEvent: KnockoutObservable<any>;
    findEvent: KnockoutObservable<any>;
    replaceEvent: KnockoutObservable<any>;
    cutEvent: KnockoutObservable<any>;
    copyEvent: KnockoutObservable<any>;
    pasteEvent: KnockoutObservable<any>;
    outdentEvent: KnockoutObservable<any>;
    indentEvent: KnockoutObservable<any>;
    lineNumbersEvent: KnockoutObservable<any>
    editorText: KnockoutObservable<string>;
    highlight: KnockoutObservable<IHighlight>;
}


// (FP) This class handles connecting the Monaco editor to the KO binding for the text. Note that the HTML template for the code editor is not using KO anywhere; this is because Monaco and KO do not natively talk to each other, so the binding is implemented here in an explicit fashion. Note that updates can come from both directions: the user can type in the editor, or the text can change due to program logic actions. As usual, this situation needs to be handled with care. Two flags are used to enforce the direction of propagation of changes. Note that there are some bits here that are closely related to how Monaco works, and that I don't understand fully at this moment.
class CodeEditorViewModel {
    editor: any;
    editorText: KnockoutObservable<string>;
    highlight: KnockoutObservable<IHighlight>;
    isEditorChanging: boolean = false;
    isModelChanging: boolean = false;
    // An identifier for an edit operation, it's used for the undo buffer.
    i = 0;
    private decorations: string[] = [];
    constructor(externalText: KnockoutObservable<string>, externalHighlight: KnockoutObservable<IHighlight>, editor: monaco.editor.IStandaloneCodeEditor) {
        this.editor = editor;
        this.editorText = externalText;
        this.highlight = externalHighlight;
        let model = editor.getModel();
        model.setValue(externalText());
        this.editorText.subscribe((value) => {
            // This is what happens when the text is changed from the outside (i.e. not by the user typing).
            if (!this.isModelChanging) {
                this.isEditorChanging = true;
                // The following code is used to push the change as an edit into the undo buffer.
                var endLine = model.getLineCount();
                var endColumn = model.getLineMaxColumn(endLine);
                model.pushEditOperations(editor.getSelections(), [{
                    range: new monaco.Range(1, 1, endLine, endColumn),
                    text: value,
                    forceMoveMarkers: false
                }], function () {
                    return editor.getSelections();
                });
                this.isEditorChanging = false;
            }
        });

        model.onDidChangeContent(() => {
            // (FP) This is what happens when the text is changed by the user typing.
            if (!this.isEditorChanging) {
                this.isModelChanging = true;
                this.editorText(model.getValue());
                this.isModelChanging = false;
            }
        });

        externalHighlight.subscribe(hl => {
            this.decorations = this.editor.deltaDecorations(this.decorations, hl == null ? [] : [{ range: new monaco.Range(hl.rowStart, hl.colStart, hl.rowEnd, hl.colEnd + 1), options: { inlineClassName: 'j-codepad-error-highlight' } }]);
        });
    }
}

// (FP) This class tells KO how to build a code-editor-widget.
class EditorViewerConfig implements KnockoutComponentTypes.Config {
    get template(): KnockoutComponentTypes.AMDModule {
        return editorTemplate;
    }
    get viewModel(): KnockoutComponentTypes.ViewModelFactoryFunction {
        var that = this;
        return {
            createViewModel: function (params: ICodeEditorParams, componentInfo: KnockoutComponentTypes.ComponentInfo) {
                // (FP) Here, we are receiving an ICodeEditorParams from the outside world. That contains our sources of user actions besides typing, such as undo/redo operations, cut/copy/paste operations, and so forth. We are going to subscribe to all of those, so that we can pass them to Monaco.

                // (FP) Register the language with Monaco. This actually happens every time a code editor is created, so it can theorically happen twice for the same language. I don't think this is a problem.
                monaco.languages.register({ id: params.language.name });
                monaco.languages.setMonarchTokensProvider(params.language.name, params.language);

                var editorDiv = $(componentInfo.element).children(".c-code-editor");

                var editor = monaco.editor.create(editorDiv[0], {
                    mouseWheelZoom: true,
                    automaticLayout: true,
                    language: params.language.name,
                    lineNumbers: "off",
                    value: "",
                    minimap: { enabled: false },
                    theme: "crn"
                }, {});

                // Prepare the cut-copy-paste dialog. This dialog will appear when a paste operation fails, which can happen due to the browser blocking it, because of security concerns. In this case, the user has to explicitly invoke a paste command from the browser, typically via CTRL-V. The dialog informs the user of this.
                var cpdialog = $(componentInfo.element).children(".c-cpdialog");
                cpdialog.dialog({ autoOpen: false });

                params.undoEvent.subscribe(function () {
                    editor.trigger("toolbar", "undo", {});
                });
                params.redoEvent.subscribe(function () {
                    editor.trigger("toolbar", "redo", {});
                });
                params.cutEvent.subscribe(function () {
                    editor.focus();
                    document.execCommand('cut');
                });
                params.copyEvent.subscribe(function () {
                    editor.focus();
                    document.execCommand('copy');
                });
                params.pasteEvent.subscribe(function () {
                    editor.focus();
                    if (!document.execCommand('paste')) {
                        cpdialog.dialog("open").parent()
                            .position({ my: 'center', at: 'center', of: $(componentInfo.element) });
                    };
                });
                params.findEvent.subscribe(function () {
                    let find = editor.getAction('actions.find');
                    find.run();
                });
                params.replaceEvent.subscribe(function () {
                    let findAndReplace = editor.getAction('editor.action.startFindReplaceAction');
                    findAndReplace.run();
                });
                params.outdentEvent.subscribe(function () {
                    editor.trigger("toolbar", "outdent", {});
                });

                params.indentEvent.subscribe(function () {
                    editor.trigger("toolbar", "tab", {});
                });
                params.lineNumbersEvent.subscribe(function () {
                    editor.updateOptions({
                        lineNumbers: editor.getConfiguration().viewInfo.renderLineNumbers == monaco.editor.RenderLineNumbersType.On ? "off" : "on"
                    });
                });
                // (FP) Create and return a CodeEditorViewModel; this handles the binding for the text.
                // Design note: at this point, the constructor for CodeEditorViewModel happens. Note that it wouldn't make any difference if we handled the events binding there as well.
                var vm = new CodeEditorViewModel(params.editorText, params.highlight, editor);
                return vm;
            }
        }
    }
}

export class Editor {
    constructor() {
        ko.bindingHandlers["editorText"] = {
            update: function (element, valueAccessor, allBindings) {
                // First get the latest data that we're bound to
                var value = valueAccessor();
                // Next, whether or not the supplied model property is observable, get its current value
                var valueUnwrapped = ko.unwrap(value);
                element.setValue(valueUnwrapped)
            }
        };
        ko.bindingHandlers["highlight"] = {
            update: function (element, valueAccessor, allBindings) {
                // First get the latest data that we're bound to
                var value = valueAccessor();
                // Next, whether or not the supplied model property is observable, get its current value
                var valueUnwrapped = ko.unwrap(value);
                element.setValue(valueUnwrapped)
            }
        };
    }
}

if (!ko.components.isRegistered("code-editor-widget"))
    ko.components.register("code-editor-widget", new EditorViewerConfig());