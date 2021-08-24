import CodePad from "../../CodeEditor/Scripts/CodePad";
import { INamedMonarchLanguage } from "../../CodeEditor/Scripts/CodeEditor";

class DsdGenericCodeEditor extends CodePad {
    constructor(language: INamedMonarchLanguage, examples: Array<ExamplesGroup>) {
        super(language, "dsd-generic-editor-widget", examples);
    }
}

export default DsdGenericCodeEditor;