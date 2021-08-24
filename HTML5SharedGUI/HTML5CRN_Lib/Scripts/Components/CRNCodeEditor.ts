import CodePad from "../../../../HTML5SharedGUI/CodeEditor/Scripts/CodePad";
import language from "./CrnLang";

var path = "./Examples/CRNModels/";
class CRNEditor extends CodePad {
    constructor() {
        // NOTE: the URLs here are supposed to be resolved relativly to the HTML page that runs the CodeEditor/CodePad.js script. Usually it is ($app_root)/index.html.
        // As the path to the code is $app_root/Models/* the relatice url here is Models/*
        var examplesCorrespondence: ExamplesGroup[] = [
            /*{
                Name: "Test",
                Correspondence: {
                    "Test": path+"test.txt",
                    "Test sweeps": path+"test_sweeps.txt",
                    "No-Directives": path+"No-Directives.txt"
                }
            },*/
            {
                Name: "Manual",
                Correspondence: {
                    "Join": path + "Manual-Join.txt",
                    "Join - Modules": path + "Manual-Join-Modules.txt",
                    "Join - Initials": path + "Manual-Join-Initials.txt",
                    "Join - Stochastic": path + "Manual-Join-Stochastic.txt",
                    "Join - LNA": path + "Manual-Join-LNA.txt",
                    "Join - CME": path + "Manual-Join-CME.txt",
                    "Join - Spatial": path + "Manual-Join-Spatial.txt",
                    "Join - Inference": path + "Manual-Join-Inference.txt"

                }
            },
            {
                Name: "Approximate Majority",
                Correspondence: {
                    //"AM": path+"AM.txt",
                    "AM - Stochastic": path + "AM-Stochastic.txt",
                    "AM - ODE": path + "AM-ODE.txt",
                    "AM - CME": path + "AM-CME.txt",
                    "AM - CME Large": path + "AM-CME-Large.txt",
                    "AM - LNA": path + "AM-LNA.txt",
                    //"AM - Z3": path+"AM-Z3.txt",
                    //"AM - Z3 Full-Full": path+"AM-Z3.txt",
                    "AM - PDE (1d)": path + "AM-PDE-1d.txt",
                    "AM - PDE (2d)": path + "AM-PDE-2d.txt",
                    "AM - Moment closure": path + "AM-MomentClosure.txt"
                }
            },
            {
                Name: "Non-spatial",
                Correspondence: {
                    "Pulses": path + "pulses.crn",
                    "Sin/Cos": path + "sincos.crn",
                    "Tanh": path + "tanh.crn",
                    "Lorenz": path + "lorenz.crn",
                    "Lotka": path + "lotka.crn",
                    "MHC class I": path + "MHC.crn",
                    "KaiC": path + "mc_kaiC.txt",
                    "Schlogl": path + "schlogl.crn",
                    "Toggle (2-species)": path + "toggle2.crn",
                    "Toggle (4-species)": path + "toggle4.crn"
                }
            },
            {
                Name: "Spatial",
                Correspondence: {
                    "Degradation": path + "spatial_degradation.txt",
                    "Autocatalytic": path + "Autocatalytic.txt",
                    "Predator-Prey (synthetic gene circuit)": path + "spatial_predator_prey.crn",
                    "Brusselator": path + "Brusselator.crn",
                    "Gierer-Meinhardt": path + "gierer.txt"
                }
            },
            {
                Name: "Inference",
                Correspondence: {
                    "AM": path + "inference-AM.txt",
                    "Reporter": path + "inference-Reporter.txt",
                    "Join Circuit": path + "inference-Join.crn"
                }
            }/*,
            {
                Name: "State Space Analysis",
                Correspondence: {
                    //"Sample 1": path+"test_SSA.txt",                 
                    "AM": path+"ssa_AM.txt",
                }
            }*/
        ];

        super(language, "crn-editor-widget", examplesCorrespondence);
    }
}

export default CRNEditor;