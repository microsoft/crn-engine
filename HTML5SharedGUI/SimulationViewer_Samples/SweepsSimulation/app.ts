// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as SimViewer from "../../SimulationViewer/Scripts/Viewer";
import * as SimViewerFramework from "../../SimulationViewer/Scripts/SimulationViewerFramework";
import * as Rx from "rx";
import "./index.css";
import "../../SimulationViewer/Styles/simulation.css";
import "../../../node_modules/interactive-data-display/dist/idd.css";

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
                                Name: "SweepA",
                                Instances: [{ ID: 0, Name: "A1", Plottables: [{ Name: "species A" }, { Name: "species B" }, { Name: "species C" }], MaxTime: 1000, MaxTrajectories: 1 },
                                { ID: 1, Name: "A2", Plottables: [{ Name: "species A" }, { Name: "species B" }, { Name: "species C" }], MaxTime: 1000, MaxTrajectories: 1 }]
                            }, {
                                Name: "SweepB",
                                Instances: [{ ID: 2, Name: "B1", Plottables: [{ Name: "species A" }, { Name: "species B" }, { Name: "species C" }], MaxTime: 1000, MaxTrajectories: 1 },
                                { ID: 3, Name: "B2", Plottables: [{ Name: "species A" }, { Name: "species B" }, { Name: "species C" }], MaxTime: 1000, MaxTrajectories: 1 }]
                            }]
                        }]
                    }]
                }
            });

        var counter = 0;
        var countsUpdates = Rx.Observable.interval(20).take(1000).map((idx) => { //composing VUM for each timer tick
            var amp = 0 + idx * 0.004;
            amp = (counter++ % 2 == 0) ? (amp * 2.0) : (amp);
            var a = Math.cos(0.04 * idx * Math.PI) * amp; //2 Pi in one second in case of 50 sim steps per seconds
            var b = Math.cos(0.04 * idx * Math.PI - Math.PI * 0.5) * amp; //delayed
            var c = Math.cos(0.04 * idx * Math.PI - Math.PI) * amp;

            var update: SimViewerFramework.ISimStepData = {
                Time: idx,
                Counts: [ //making oscilations positive
                    a + amp,
                    b + amp,
                    c + amp,
                ],
                Instance: counter % 4
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

        return instancesUpdate.concat(countsUpdates);
    }
}

var simulator = new Simulator();
simulator.Simulate().subscribeOnNext((update) => { viewer.Post(update); }); //sending VUM sequence to the viewer    
(<any>$(".SimulationViewer_tabs")).tabs("refresh");
