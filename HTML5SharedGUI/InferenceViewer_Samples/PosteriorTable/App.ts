// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from "knockout";
import { PosteriorViewer } from "../../InferenceViewer/Scripts/PosteriorViewer";
import * as Inference from "../../InferenceViewer/Scripts/InferenceViewer";
import * as Rx from "rx";
import "../../InferenceViewer/Styles/inference.css";
import "../../../HTML5SharedGUI/KnockoutGrid/table.css"

var selectorVM = {
    Nodes: ["NodeA", "NodeB"],
    SelectedNodeID: ko.observable("NodeA")
};
ko.applyBindings(selectorVM, document.getElementById("CRNSelector"));

var table = new PosteriorViewer(selectorVM, true);
table.Bind(document.getElementById("PosteriorViewer"));

function run(): Inference.IInferenceRun {
    var progress: Rx.Observable<Inference.IProgress> = Rx.Observable.interval(300).take(200).map(i => {
        return {
            NodeID: i < 100 ? "NodeA" : "NodeB",
            CurrentIteration: i % 100,
            BurnInLength: 30,
            SamplingLength: 70
        };
    });
    var def: Rx.Observable<Inference.IParameterDefinitions> = Rx.Observable.from(
        [{
            NodeID: "NodeA",
            Definitions: [{
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
            Definitions: [{
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
    var upd: Rx.Observable<Inference.IRecentParametersValues> = Rx.Observable.interval(300).take(200).map(i => {
        return {
            NodeID: i < 100 ? "NodeA" : "NodeB",
            values: i < 100 ? [5 + Math.sin(i * 0.01), 3 - Math.cos(i * 0.02)] : [Math.sin(i), (i - 100) / 3],
            iteration: i % 100,
            lglk: 0
        };
    });
    var settingsValues: Rx.Observable<Inference.IPlotSettingsValues> = Rx.Observable.from(
        [{
            NodeID: "NodeA",
            XLabel: "Time",
            YLabel: "Concentration (nM)",
            Title: "",
            LabelFontSize: 16,
            TickFontSize: 8,
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
            XLabel: "Time",
            YLabel: "Concentration (nM)",
            Title: "",
            LabelFontSize: 16,
            TickFontSize: 8,
            XTicks: [],
            YTicks: [],
            BurnInThin: 100,
            XMin: null,
            XMax: null,
            YMin: null,
            YMax: null,
            HBoundaries: [],
            VBoundaries: []
        }]
    );
    return {
        progress: progress,
        paramDefinitions: def,
        summary: null,
        paramUpdates: upd,
        posteriorTableUpdates: upd,
        traceDefinitions: null,
        simulationUpdates: null,
        plotSettingsUpdates: settingsValues
    };
}

table.show(run());
