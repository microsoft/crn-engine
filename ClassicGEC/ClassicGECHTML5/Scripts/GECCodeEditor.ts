import CodePad from "../../../HTML5SharedGUI/CodeEditor/Scripts/CodePad";
import { INamedMonarchLanguage } from '../../../HTML5SharedGUI/CodeEditor/Scripts/CodeEditor';

class GECCodeEditor extends CodePad {
    constructor(language: INamedMonarchLanguage, examples: Array<ExamplesGroup>) {
        super(language, "gec-editor-widget", examples);
    }
}

export default GECCodeEditor;