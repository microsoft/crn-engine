import * as ko from "knockout";
import { ModelDataDynamics } from "../../InferenceViewer/Scripts/ModelDataDynamics";
import * as Inference from "../../InferenceViewer/Scripts/InferenceViewer";
import * as Rx from "rx";
import "../../InferenceViewer/Styles/inference.css";
import "../../../node_modules/interactive-data-display/dist/idd.css";

var selectorVM = {
    Nodes: ["NodeA", "NodeB"],
    SelectedNodeID: ko.observable("NodeA")
};
ko.applyBindings(selectorVM, document.getElementById("CRNSelector"));

var model = new ModelDataDynamics(selectorVM, true);
model.Bind(document.getElementById("ModelDynamics"));

const uint32_max = 4294967295;

function run(): Inference.IInferenceRun {
    var plottables: Rx.Observable<Inference.ITraceDefinitions> = Rx.Observable.from([{
        NodeID: "NodeA",
        CRNs: [{
            Name: "", Settings: [{
                Name: "",
                Sweeps: [{
                    Name: "Sweep1",
                    Instances: [{
                        ID: 1,
                        Name: "A",
                        Plottables: [{ Name: "Sin" }, { Name: "Cos" }]
                    }, {
                        ID: 2,
                        Name: "B",
                        Plottables: [{ Name: "Sin" }, { Name: "Cos" }]
                    }]
                }, {
                    Name: "Sweep2",
                    Instances: [{
                        ID: 3,
                        Name: "A",
                        Plottables: [{ Name: "Sin" }, { Name: "Cos" }]
                    }, {
                        ID: 4,
                        Name: "B",
                        Plottables: [{ Name: "Sin" }, { Name: "Cos" }]
                    }]
                }]
            }]
        }]
    }, {
        NodeID: "NodeB",
        CRNs: [{
            Name: "", Settings: [{
                Name: "",
                Sweeps: [{
                    Name: "Sweep1",
                    Instances: [{
                        ID: 1,
                        Name: "Foo",
                        Plottables: [{ Name: "X" }, { Name: "Y" }]
                    }, {
                        ID: 2,
                        Name: "Bar",
                        Plottables: [{ Name: "X" }, { Name: "Y" }]
                    }]
                }, {
                    Name: "Sweep2",
                    Instances: [{
                        ID: 3,
                        Name: "Foo",
                        Plottables: [{ Name: "X" }, { Name: "Y" }]
                    }, {
                        ID: 4,
                        Name: "Bar",
                        Plottables: [{ Name: "X" }, { Name: "Y" }]
                    }]
                }]
            }]
        }]
    }]);
    var simValues: Rx.Observable<Inference.IPlottableValues> = Rx.Observable.interval(100).take(600).map(i => {
        if (i < 300) {
            var instance = i % 4 + 1;
            var k = i + 1;
            var t = new Array<number>();
            var v = new Array<number[]>();
            v.push(new Array<number>());
            v.push(new Array<number>());
            let randomness = new Uint32Array(1000 * 2);
            crypto.getRandomValues(randomness);
            for (var j = 0; j < 1000; j++) {
                t.push(j * 0.1 + instance * 2);
                v[0].push(Math.sin(j * 0.1) + ((randomness[j] / uint32_max) / k - 0.5 / k));
                v[1].push(Math.cos(j * 0.05) + 2 + ((randomness[j + 1] / uint32_max) / k - 0.5 / k));
            }
            return {
                NodeID: "NodeA",
                Times: t,
                Values: v,
                InstanceID: instance
            };
        }
        else {
            var instance = i % 4 + 1;
            var k = i + 1;
            var t = new Array<number>();
            var v = new Array<number[]>();
            v.push(new Array<number>());
            v.push(new Array<number>());
            let randomness = new Uint32Array(1000 * 2);
            crypto.getRandomValues(randomness);
            for (var j = 0; j < 1000; j++) {
                t.push(j * 0.1 + instance * 2);
                v[0].push(j * 0.2 + ((randomness[j] / uint32_max) / k - 0.5 / k));
                v[1].push(j * 1.3 + 2 + ((randomness[j + 1] / uint32_max) / k - 0.5 / k));
            }
            return {
                NodeID: "NodeB",
                Times: t,
                Values: v,
                InstanceID: instance
            };
        }
    });
    var progress: Rx.Observable<Inference.IProgress> = Rx.Observable.interval(100).take(600).map(i => {
        if (i < 300)
            return {
                NodeID: "NodeA",
                CurrentIteration: i,
                BurnInLength: 30,
                SamplingLength: 300
            };
        else
            return {
                NodeID: "NodeB",
                CurrentIteration: (i - 300),
                BurnInLength: 30,
                SamplingLength: 300
            };
    });
    var paramUpdates: Rx.Observable<Inference.IRecentParametersValues> = Rx.Observable.interval(100).take(600).map(i => {
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
        paramDefinitions: null,
        summary: null,
        paramUpdates: paramUpdates,
        posteriorTableUpdates: null,
        traceDefinitions: plottables,
        simulationUpdates: simValues,
        plotSettingsUpdates: settingsValues
    };
}

model.show(run());