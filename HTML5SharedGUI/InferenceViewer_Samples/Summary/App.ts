// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from "knockout";
import { InferenceSummary } from "../../InferenceViewer/Scripts/InferenceSummary";
import * as Inference from "../../InferenceViewer/Scripts/InferenceViewer";
import * as Rx from "rx";
import "../../InferenceViewer/Styles/inference.css";
import "../../../node_modules/interactive-data-display/dist/idd.css";

var selectorVM = {
    Nodes: ["NodeA", "NodeB"],
    SelectedNodeID: ko.observable("NodeA")
};
ko.applyBindings(selectorVM, document.getElementById("CRNSelector"));

var model = new InferenceSummary(selectorVM, true);
model.Bind(document.getElementById("InferenceSummary"));

function run(): Inference.IInferenceRun {
    var progress: Rx.Observable<Inference.IProgress> = Rx.Observable.interval(300).take(600).map(i => {
        return {
            NodeID: i < 300 ? "NodeA" : "NodeB",
            CurrentIteration: i % 300,
            BurnInLength: 30,
            SamplingLength: 300
        };
    });
    var paramUpdates: Rx.Observable<Inference.IRecentParametersValues> = Rx.Observable.interval(300).take(600).map(i => {
        if (i < 300)
            return {
                NodeID: "NodeA",
                values: [],
                lglk: 0,
                iteration: i
            };
        else
            return {
                NodeID: "NodeB",
                values: [],
                lglk: 0,
                iteration: (i - 300)
            };
    });
    var summary: Rx.Observable<Inference.ISummary> = Rx.Observable.interval(300).take(600).map(i => {
        if (i < 300)
            return {
                NodeID: "NodeA",
                Summary: "summary NodeA " + i
            };
        else
            return {
                NodeID: "NodeB",
                Summary: "summary NodeB " + (i - 300)
            };
    });
    return {
        progress: progress,
        paramDefinitions: null,
        summary: summary,
        paramUpdates: paramUpdates,
        posteriorTableUpdates: null,
        traceDefinitions: null,
        simulationUpdates: null,
        plotSettingsUpdates: null
    };
}

model.show(run());
