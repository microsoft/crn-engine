import * as monaco from "monaco-editor";
import MonacoEditor from 'react-monaco-editor';
import React from 'react';
import "../src/reactcodepad.css";

// This file defines a React component that wraps a Monaco editor with buttons that allow the user to perform some basic text editor functionality.

export interface ICodePadProps {
    width?: string | number | undefined;
    height?: string | number | undefined;
    language?: string;
    text?: string;
    theme?: string;
    onTextChange?: (tex: string) => void;
    defaultFileName?: string;
}

interface ICodePadState {
    text: string;
}

// Registers a Monarch language definition for syntax highlighting.
export function RegisterLanguage(id: string, language: monaco.languages.IMonarchLanguage | monaco.Thenable<monaco.languages.IMonarchLanguage>) {
    (window as any).monaco.languages.register({ id: id });
    (window as any).monaco.languages.setMonarchTokensProvider(id, language);
}

// The CodePad component.
export class CodePad extends React.Component<ICodePadProps, ICodePadState> {
    constructor(props: ICodePadProps) {
        super(props);
    }

    fireTextChange(v: string) {
        if (this.props.onTextChange != null)
            this.props.onTextChange(v);
    }

    private editor?: monaco.editor.IStandaloneCodeEditor;

    private editorWillMount = (editor: typeof monaco) => {
    }

    private editorDidMount = (editor: monaco.editor.IStandaloneCodeEditor) => {
        this.editor = editor;
    }

    private onNew = () => {
        this.fireTextChange("");
    }

    private onOpen = () => {
        var that = this;
        var filesInput = document.createElement("input");
        filesInput.type = "file";
        filesInput.onchange = () => {
            if (filesInput.files != null) {
                var file = filesInput.files[0];
                var reader = new FileReader();
                reader.onload = function (event) {
                    if (event.target != null) {
                        var text = event.target.result == null ? "" : event.target.result.toString();
                        that.fireTextChange(text);
                    }
                }
                reader.readAsText(file);
            }
        }
        filesInput.click();
    }

    private onSave = () => {
        var text = this.props.text == null ? "" : this.props.text;
        var blob = new Blob([text], { type: "text/txt" });
        saveAs(blob, this.props.defaultFileName == null ? "code.txt" : this.props.defaultFileName);
    }

    private onFind = () => {
        if (this.editor == null)
            return;
        let find = this.editor.getAction('actions.find');
        find.run();
    }

    private onReplace = () => {
        if (this.editor == null)
            return;
        let findAndReplace = this.editor.getAction('editor.action.startFindReplaceAction');
        findAndReplace.run();
    }

    private onCut = () => {
        if (this.editor == null)
            return;
        this.editor.focus();
        document.execCommand('cut');
    }

    private onCopy = () => {
        if (this.editor == null)
            return;
        this.editor.focus();
        document.execCommand('copy');
    }

    private onPaste = () => {
        if (this.editor == null)
            return;
        this.editor.focus();
        document.execCommand('paste');
    }

    private onUndo = () => {
        if (this.editor == null)
            return;
        this.editor.trigger("toolbar", "undo", {});
    }

    private onRedo = () => {
        if (this.editor == null)
            return;
        this.editor.trigger("toolbar", "redo", {});
    }

    private onOutdent = () => {
        if (this.editor == null)
            return;
        this.editor.trigger("toolbar", "outdent", {});
    }

    private onIndent = () => {
        if (this.editor == null)
            return;
        this.editor.trigger("toolbar", "tab", {});
    }

    private onLineNumbers = () => {
        if (this.editor == null)
            return;
        this.editor.updateOptions({
            lineNumbers: this.editor.getOption(monaco.editor.EditorOption.lineNumbers).renderType == monaco.editor.RenderLineNumbersType.On ? "off" : "on"
        });
    }

    render() {
        var that = this;
        var onChangeHandler = (v: string, e: any) => this.fireTextChange(v);
        return (
            <div className="c-reactcodepad" style={{ width: this.props.width, height: this.props.height }}>
                <div className="c-reactcodepad__toolbar">
                    <div className="c-reactcodepad__toolbar-button-group">
                        <button className="c-reactcodepad__toolbar-button btn--new">
                            <span className="c-reactcodepad__toolbar-button btn--new" title="New" onClick={that.onNew}></span>
                        </button>
                        <button className="c-reactcodepad__toolbar-button btn--open">
                            <span className="c-reactcodepad__toolbar-button btn--open" title="Open" onClick={that.onOpen}></span>
                        </button>
                        <button className="c-reactcodepad__toolbar-button btn--save">
                            <span className="c-reactcodepad__toolbar-button btn--save" title="Save" onClick={that.onSave}></span>
                        </button>
                    </div>
                    <div className="c-reactcodepad__toolbar-button-group">
                        <button className="c-reactcodepad__toolbar-button btn--find">
                            <span className="c-reactcodepad__toolbar-button btn--find" title="Find" onClick={that.onFind}></span>
                        </button>
                        <button className="c-reactcodepad__toolbar-button btn--replace">
                            <span className="c-reactcodepad__toolbar-button btn--replace" title="Find and Replace" onClick={that.onReplace}></span>
                        </button>
                    </div>
                    <div className="c-reactcodepad__toolbar-button-group">
                        <button className="c-reactcodepad__toolbar-button btn--cut">
                            <span className="c-reactcodepad__toolbar-button btn--cut" title="Cut" onClick={that.onCut}></span>
                        </button>
                        <button className="c-reactcodepad__toolbar-button btn--copy">
                            <span className="c-reactcodepad__toolbar-button btn--copy" title="Copy" onClick={that.onCopy}></span>
                        </button>
                        <button className="c-reactcodepad__toolbar-button btn--paste">
                            <span className="c-reactcodepad__toolbar-button btn--paste" title="Paste" onClick={that.onPaste}></span>
                        </button>
                    </div>
                    <div className="c-reactcodepad__toolbar-button-group">
                        <button className="c-reactcodepad__toolbar-button btn--undo">
                            <span className="c-reactcodepad__toolbar-button btn--undo" title="Undo" onClick={that.onUndo}></span>
                        </button>
                        <button className="c-reactcodepad__toolbar-button btn--redo">
                            <span className="c-reactcodepad__toolbar-button btn--redo" title="Redo" onClick={that.onRedo}></span>
                        </button>
                    </div>
                    <div className="c-reactcodepad__toolbar-button-group">
                        <button className="c-reactcodepad__toolbar-button btn--outdent">
                            <span className="c-reactcodepad__toolbar-button btn--outdent" title="Outdent" onClick={that.onOutdent}></span>
                        </button>
                        <button className="c-reactcodepad__toolbar-button btn--indent">
                            <span className="c-reactcodepad__toolbar-button btn--indent" title="Indent" onClick={that.onIndent}></span>
                        </button>
                    </div>
                    <div className="c-reactcodepad__toolbar-button-group">
                        <button className="c-reactcodepad__toolbar-button btn--line-numbers">
                            <span className="c-reactcodepad__toolbar-button btn--line-numbers" title="Line numbers" onClick={that.onLineNumbers}></span>
                        </button>
                    </div>
                </div>
                <div className="c-reactcodepad__editor">
                    <MonacoEditor width='100%' height='100%' theme={this.props.theme} value={this.props.text} language={this.props.language} onChange={onChangeHandler} editorWillMount={this.editorWillMount} editorDidMount={this.editorDidMount} />
                </div>
            </div>
        );
    }
}