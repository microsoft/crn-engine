import * as FW from '../../SimulationViewer/Scripts/SimulationViewerFramework';
import { Settings as SpatialViewerSettings } from '../../SimulationViewer/Scripts/SpatialViewerSettings';
import Viewer1D from '../../SimulationViewer/Scripts/Spatial1DViewer';
import Viewer2D from '../../SimulationViewer/Scripts/Spatial2DViewer';
import "../../SimulationViewer/Styles/simulation.css";
import "../../../node_modules/interactive-data-display/dist/idd.css";
import "../table.css";
import "../DummySimulation.css";

var spatialSettings = new SpatialViewerSettings("blue,green,red", false);

var s1D = new Viewer1D(spatialSettings);
s1D.bind($('#spatial-1d-viewer').get(0));
var s2D = new Viewer2D(spatialSettings);
s2D.bind($('#spatial-2d-viewer').get(0));

function getEvents1D(): Rx.Observable<FW.IVisualizationUpdateMessage> {
    var x = new Array(100);
    var t = 0;
    for (var i = 0; i < 100; i++) {
        x[i] = i / 25;
    }

    var traces = {
        ModelName: "",
        CRNs: [{
            Name: "",
            Settings: [{
                Name: "",
                Sweeps: [{
                    Name: "",
                    Instances: [{
                        ID: 0,
                        Name: "",
                        Plottables: [{ Name: "SingleSpecies" }],
                        MaxTime: 100,
                        MaxTrajectories: 1
                    }]
                }]
            }]
        }]
    };

    var data: Rx.Observable<FW.IVisualizationUpdateMessage> = Rx.Observable.interval(250)
        .take(200)
        .map((i) => {
            var data = new Array(100);
            for (var k = 0; k < 100; k++) {
                data[k] = Math.sin(k * 0.04) * Math.cos(i * 0.02);
            }
            return {
                MessageType: FW.VisualizationUpdateMessageType.SimulationStep1D,
                EncapsulatedUpdate: {
                    Instance: 0,
                    Time: t + 0.01 * i,
                    Data: [data]
                }
            };
        });

    var events = Rx.Observable.fromArray<FW.IVisualizationUpdateMessage>([
        {
            MessageType: FW.VisualizationUpdateMessageType.SpatialSpace,
            EncapsulatedUpdate: { XAxis: x }
        }, {
            MessageType: FW.VisualizationUpdateMessageType.TraceDefinitions,
            EncapsulatedUpdate: traces
        }
    ]);
    return events.concat(data);
}

var events1D = getEvents1D();
events1D.subscribe(next => s1D.Post(next), error => { }, () => { });



function getEvents2D(): Rx.Observable<FW.IVisualizationUpdateMessage> {
    var x = new Array(100);
    var t = 0;
    for (var i = 0; i < 100; i++) {
        x[i] = i / 25;
    }

    var traces = {
        ModelName: "",
        CRNs: [{
            Name: "",
            Settings: [{
                Name: "",
                Sweeps: [{
                    Name: "",
                    Instances: [{
                        ID: 1,
                        Name: "",
                        Plottables: [{ Name: "SingleSpecies" }],
                        MaxTime: 100,
                        MaxTrajectories: 1
                    }]
                }]
            }]
        }]
    };

    var data: Rx.Observable<FW.IVisualizationUpdateMessage> = Rx.Observable.interval(250)
        .take(200)
        .map((i) => {
            var data: number[][] = new Array(100);
            for (var j = 0; j < 100; j++) {
                data[j] = new Array(100);
                for (var k = 0; k < 100; k++) {
                    data[j][k] = Math.sin(k * 0.04) * Math.cos(j * 0.02 + i);
                }
            }
            return {
                MessageType: FW.VisualizationUpdateMessageType.SimulationStep2D,
                EncapsulatedUpdate: {
                    Instance: 1,
                    Time: i,
                    Data: [data]
                }
            };
        });

    var events = Rx.Observable.fromArray<FW.IVisualizationUpdateMessage>([
        {
            MessageType: FW.VisualizationUpdateMessageType.SpatialSpace,
            EncapsulatedUpdate: { XAxis: x, YAxis: x }
        }, {
            MessageType: FW.VisualizationUpdateMessageType.TraceDefinitions,
            EncapsulatedUpdate: traces
        }
    ]);
    return events.concat(data);
}

var events2D = getEvents2D().share();
events2D.subscribe(next => s2D.Post(next), error => { }, () => { });