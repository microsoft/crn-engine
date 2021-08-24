import CodePad from "../CodeEditor/Scripts/CodePad";
import language from "./crnlang";

class CRNEditor extends CodePad {
    constructor() {
        var examplesCorrespondence: ExamplesGroup[] = [
            {
                Name: "AM",
                Correspondence: {
                    "AM": "/samples/CRNData/AM.txt",
                    "AM - Stochastic": "/samples/CRNData/AM-Stochastic.txt",
                    "AM - CME": "/samples/CRNData/AM-CME.txt",
                    "AM - CME Large": "/samples/CRNData/AM-CME-Large.txt",
                    "AM - LNA": "/samples/CRNData/AM-LNA.txt",
                    "AM - Z3": "/samples/CRNData/AM-Z3.txt",
                    "AM - Z3 Full-Full": "/samples/CRNData/AM-Z3.txt",
                    "AM - ODE": "/samples/CRNData/AM-ODE.txt",
                    "AM - PDE (1d)": "/samples/CRNData/AM-PDE-1d.txt",
                    "AM - PDE (2d)": "/samples/CRNData/AM-PDE-2d.txt"
                }
            }
        ];

        super(language, "crn-editor-widget", examplesCorrespondence);
    }
}

export default CRNEditor;