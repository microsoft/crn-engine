import * as ko from "knockout";
import { InferredParametersViewer } from "../../InferenceViewer/Scripts/InferredParametersViewer";
import * as Inference from "../../InferenceViewer/Scripts/InferenceViewer";
import * as Rx from "rx";
import "../../InferenceViewer/Styles/inference.css";
import "../../../HTML5SharedGUI/KnockoutGrid/table.css"

var selectorVM = {
    Nodes: ["NodeA", "NodeB"],
    SelectedNodeID: ko.observable("NodeA")
};
ko.applyBindings(selectorVM, document.getElementById("CRNSelector"));

var param = new InferredParametersViewer(selectorVM, true);
param.Bind(document.getElementById("ParametersViewer"));

function run(): Inference.IInferenceRun {
    var progress: Rx.Observable<Inference.IProgress> = Rx.Observable.interval(100).take(600).map(i => {
        return {
            NodeID: i < 300 ? "NodeA" : "NodeB",
            CurrentIteration: i % 300,
            BurnInLength: 30,
            SamplingLength: 300
        };
    });
    var def: Rx.Observable<Inference.IParameterDefinitions> = Rx.Observable.from(
        [{
            NodeID: "NodeA",
            Definitions:
                [{
                    Name: "aaa",
                    Type: Inference.ParameterType.Log,
                    Variability: Inference.ParameterVariability.Random,
                    LowerBound: 4,
                    UpperBound: 6
                },
                {
                    Name: "bbb",
                    Type: Inference.ParameterType.Real,
                    Variability: Inference.ParameterVariability.Random,
                    LowerBound: 2,
                    UpperBound: 4
                }]
        }, {
            NodeID: "NodeB",
            Definitions:
                [{
                    Name: "foo",
                    Type: Inference.ParameterType.Log,
                    Variability: Inference.ParameterVariability.Random,
                    LowerBound: -1,
                    UpperBound: 1
                },
                {
                    Name: "bar",
                    Type: Inference.ParameterType.Real,
                    Variability: Inference.ParameterVariability.Random,
                    LowerBound: 0,
                    UpperBound: 100
                }]
        }]
    );
    var upd: Rx.Observable<Inference.IRecentParametersValues> = Rx.Observable.interval(100).take(600).map(i => {
        return {
            NodeID: i < 300 ? "NodeA" : "NodeB",
            values: i < 300 ? [5 + Math.sin(i * 0.01), 3 - Math.cos(i * 0.02)] : [-1 + Math.sin(i), (i - 300) / 3],
            iteration: i % 300,
            lglk: 0
        };
    });
    return {
        progress: progress,
        paramDefinitions: def,
        summary: null,
        paramUpdates: upd,
        posteriorTableUpdates: upd,
        traceDefinitions: null,
        simulationUpdates: null,
        plotSettingsUpdates: null
    };
}

param.show(run());