// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as SimViewer from "../SimulationViewer/Scripts/Viewer";
import * as SimViewerFramework from "../SimulationViewer/Scripts/SimulationViewerFramework";
import * as Rx from "rx";
import "./table.css";
import "./DummySimulation.css";
import "../SimulationViewer/Styles/simulation.css";
import "../../node_modules/interactive-data-display/dist/idd.css";

//Setting up the page
var viewer = new SimViewer.Viewer();


// FP: prevent this from attempting to bind to null
var viewerDivDefault = document.getElementById('defaultLayout');
if (viewerDivDefault)
    viewer.bind(viewerDivDefault);

var viewerDivAutobind = document.getElementById('autoBind');
if (viewerDivAutobind)
    viewer.autoBind(viewerDivAutobind);

(<any>$(".SimulationViewer_tabs")).tabs({ //transforming lists to tabs
    heightStyle: "fill"
});


//sending data to the simulation viewer

var withCI = true;

export class Simulator {
    public Simulate() {
        //initial map of the species names. The further simulation updates will rely on the order of the species name here
        var instancesUpdate = Rx.Observable.return<SimViewerFramework.IVisualizationUpdateMessage>(
            {
                MessageType: SimViewerFramework.VisualizationUpdateMessageType.TraceDefinitions,
                EncapsulatedUpdate: {
                    ModelName: "",
                    CRNs: [{
                        Name: "",
                        Settings: [{
                            Name: "",
                            Sweeps: [{
                                Name: "",
                                Instances: [{ ID: 0, Name: "", Plottables: [{ Name: "species A", Structural: "species A structure" }, { Name: "species B", Structural: "species B structure" }, { Name: "species C", Structural: "species C structure" }], MaxTime: 1000, MaxTrajectories: 1 }]
                            }]
                        }]
                    }]
                }
            });
        var unitsUpdate = Rx.Observable.return<SimViewerFramework.IVisualizationUpdateMessage>(
            {
                MessageType: SimViewerFramework.VisualizationUpdateMessageType.UnitsInformation,
                EncapsulatedUpdate: "Units of simulation"
            }
        );
        var labelsUpdate = Rx.Observable.return<SimViewerFramework.IVisualizationUpdateMessage>(
            {
                MessageType: SimViewerFramework.VisualizationUpdateMessageType.PlotSettingsInfo,
                EncapsulatedUpdate: {
                    XLabel: "Time",
                    YLabel: "Concentration (nM)",
                    Title: "",
                    LabelFontSize: 14,
                    TickFontSize: 12,
                    XTicks: [], //[10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400],
                    YTicks: [], //[0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20 ]
                    VBoundaries: [],
                    HBoundaries: [],
                    XMin: 5.0,
                    XMax: 15.0,
                    YMin: 0.0,
                    YMax: 0.2
                }
            }
        );
        var countsUpdates = Rx.Observable.interval(20).takeWhile(idx => idx <= 1000).map((idx) => { //composing VUM for each timer tick
            var amp = 0 + idx * 0.004;
            var a = Math.cos(0.04 * idx * Math.PI) * amp; //2 Pi in one second in case of 50 sim steps per secons
            var b = Math.cos(0.04 * idx * Math.PI - Math.PI * 0.5) * amp; //delayed
            var c = Math.cos(0.04 * idx * Math.PI - Math.PI) * amp;

            var update: SimViewerFramework.ISimStepData = {
                Time: idx,
                Counts: [ //making oscilations positive
                    a + amp,
                    b + amp,
                    c + amp,
                ],
                Instance: 0
            };

            var varRate = 0.1;

            if (withCI) {
                update.Bounds = [
                    { lower: Math.max(0, (a + amp) * (1.0 - varRate)), upper: (a + amp) * (1.0 + varRate) },
                    { lower: Math.max(0, (b + amp) * (1.0 - varRate)), upper: (b + amp) * (1.0 + varRate) },
                    { lower: Math.max(0, (c + amp) * (1.0 - varRate)), upper: (c + amp) * (1.0 + varRate) }
                ];
            }

            return {
                MessageType: SimViewerFramework.VisualizationUpdateMessageType.SimulationStep,
                EncapsulatedUpdate: update
            };
        });

        return instancesUpdate.concat(unitsUpdate).concat(labelsUpdate).concat(countsUpdates);
    }
}

var simulator = new Simulator();
function run() {
    simulator.Simulate().subscribeOnNext((update) => { viewer.Post(update); }); //sending VUM sequence to the viewer
}
setTimeout(run);
