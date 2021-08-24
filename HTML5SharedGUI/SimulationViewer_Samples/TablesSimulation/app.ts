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
                                Instances: [{ ID: 0, Name: "1", Plottables: [{ Name: "species A" }, { Name: "species B" }, { Name: "species C" }], MaxTime: 1000, MaxTrajectories: 1 }]
                            }]
                        }]
                    }]
                }
            });

        var counter = 0;
        var times:number[] = [];
        for (var i = 0; i < 100; i++)
            times[i] = i;
        var countsUpdates = Rx.Observable.interval(1000).map((idx) => { //composing VUM for each timer tick

            var a = [];
            var b = [];
            var c = [];
            for (var i = 0; i < 100; i++) {
                var amp = 0 + i * 0.004;
                amp = (counter++ % 2 == 0) ? (amp * 2.0) : (amp);
                a.push(amp + Math.cos(0.04 * idx * Math.PI) * amp); //2 Pi in one second in case of 50 sim steps per seconds
                b.push(amp + Math.cos(0.04 * idx * Math.PI - Math.PI * 0.5) * amp); //delayed
                c.push(amp + Math.cos(0.04 * idx * Math.PI - Math.PI) * amp);
            }

            function makeBounds(value: number, index: number) {
                var k = (index / 100) * (1 - 1 / idx);
                return { lower: value - k, upper: value + k };
            }

            var update: SimViewerFramework.ISimTable = {
                Instance: 0,
                Time: times,
                Counts: [a, b, c,],
                Bounds: [a.map(makeBounds), b.map(makeBounds), c.map(makeBounds)]
            };

            return {
                MessageType: SimViewerFramework.VisualizationUpdateMessageType.SimulationTable,
                EncapsulatedUpdate: update
            };
        });

        return instancesUpdate.concat(countsUpdates);
    }
}

var simulator = new Simulator();
simulator.Simulate().subscribeOnNext((update) => { viewer.Post(update); }); //sending VUM sequence to the viewer    
(<any>$(".SimulationViewer_tabs")).tabs("refresh");