import * as ko from "knockout";
import { PosteriorViewer } from "../../InferenceViewer/Scripts/PosteriorViewer";
import * as Inference from "../../InferenceViewer/Scripts/InferenceViewer";
import * as Rx from "rx";
import "../../InferenceViewer/Styles/inference.css";
import "../../../node_modules/interactive-data-display/dist/idd.css";

var selectorVM = {
    Nodes: ["NodeA", "NodeB"],
    SelectedNodeID: ko.observable("NodeA")
};
ko.applyBindings(selectorVM, document.getElementById("CRNSelector"));

var model = new PosteriorViewer(selectorVM, true);
model.Bind(document.getElementById("DensityPlot"));

function run(): Inference.IInferenceRun {
    var def: Rx.Observable<Inference.IParameterDefinitions> = Rx.Observable.from(
        [{
            NodeID: "NodeA",
            Definitions:
                [{
                    Name: "A (log)",
                    Type: Inference.ParameterType.Log,
                    Variability: Inference.ParameterVariability.Random,
                    LowerBound: 4,
                    UpperBound: 5
                }, {
                    Name: "B (real)",
                    Type: Inference.ParameterType.Real,
                    Variability: Inference.ParameterVariability.Fixed,
                    LowerBound: 1.0,
                    UpperBound: 10000
                }]
        }, {
            NodeID: "NodeB",
            Definitions:
                [{
                    Name: "X (log)",
                    Type: Inference.ParameterType.Log,
                    Variability: Inference.ParameterVariability.Random,
                    LowerBound: -1,
                    UpperBound: 1
                }, {
                    Name: "Y (real)",
                    Type: Inference.ParameterType.Real,
                    Variability: Inference.ParameterVariability.Fixed,
                    LowerBound: 0,
                    UpperBound: 100
                }]
        }]
    );

    var BvaluesArray = [1];
    while (BvaluesArray.length < 10) BvaluesArray.push(5);
    while (BvaluesArray.length < 20) BvaluesArray.push(50);
    while (BvaluesArray.length < 30) BvaluesArray.push(500);
    while (BvaluesArray.length < 39) BvaluesArray.push(5000);
    BvaluesArray.push(10000);

    var upd: Rx.Observable<Inference.IRecentParametersValues> = Rx.Observable.interval(30).take(2000).map(i => {
        return {
            NodeID: i < 1000 ? "NodeA" : "NodeB",
            values: i < 1000 ? [5 + Math.sin(i * 0.01), BvaluesArray[i % BvaluesArray.length]] : [Math.cos(i * 0.1), BvaluesArray[i % BvaluesArray.length] / 100],
            iteration: i,
            lglk: 0
        };
    });
    var settings: Rx.Observable<Inference.IPlotSettingsValues> = Rx.Observable.from(
        [{
            NodeID: "NodeA",
            XLabel: "",
            YLabel: "",
            Title: "",
            LabelFontSize: 16,
            TickFontSize: 15,
            XTicks: [],
            YTicks: [],
            BurnInThin: 100,
            XMin: null,
            XMax: null,
            YMin: null,
            YMax: null,
            HBoundaries: [],
            VBoundaries: []
        }, {
            NodeID: "NodeB",
            XLabel: "",
            YLabel: "",
            Title: "",
            LabelFontSize: 16,
            TickFontSize: 15,
            XTicks: [],
            YTicks: [],
            BurnInThin: 100,
            XMin: null,
            XMax: null,
            YMin: null,
            YMax: null,
            HBoundaries: [],
            VBoundaries: []
        }]);
    var progress: Rx.Observable<Inference.IProgress> = Rx.Observable.interval(30).take(2000).map(i => {
        return {
            NodeID: i < 1000 ? "NodeA" : "NodeB",
            CurrentIteration: i % 1000,
            BurnInLength: 100,
            SamplingLength: 900
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
        plotSettingsUpdates: settings
    };
}

model.show(run());