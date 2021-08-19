import * as PM from '../../SimulationViewer/Scripts/ProbabilityMapView';
import * as Framework from "../../SimulationViewer/Scripts/SimulationViewerFramework";
import { Settings as SpatialViewerSettings } from '../../SimulationViewer/Scripts/SpatialViewerSettings';
import "../../SimulationViewer/Styles/simulation.css";
import "../../../node_modules/interactive-data-display/dist/idd.css";

var info1: Framework.IProbabilitiesInfo = {
    InstanceName: "1",
    Names: ["Species 1", "Species 2"],
    ProbabilitiesToken: "1"
};
var info2: Framework.IProbabilitiesInfo = {
    InstanceName: "2",
    Names: ["Species 1", "Species 2"],
    ProbabilitiesToken: "2"
};

class FakeProvider implements PM.IProbabilityMapProvider {
    constructor() {
    }

    private GetProbabilityMap11() {
        var pm = new PM.ProbabilityMap();
        var timesCount = 10, valuesCount = 2;
        pm.times = new Array(timesCount);
        pm.values = new Array(valuesCount);
        pm.probabilities = new Array(timesCount);
        for (var j = 0; j < valuesCount; j++)
            pm.values[j] = j;
        for (var i = 0; i < timesCount; i++)
            pm.times[i] = i;
        for (var i = 0; i < timesCount; i++) {
            pm.probabilities[i] = new Array(valuesCount);
            pm.probabilities[i][0] = Math.abs(Math.sin(i / 3));
            pm.probabilities[i][1] = 1 - pm.probabilities[i][0];
        }
        return pm;
    }

    private GetProbabilityMap12() {
        var pm = new PM.ProbabilityMap();
        var timesCount = 10, valuesCount = 2;
        pm.times = new Array(timesCount);
        pm.values = new Array(valuesCount);
        pm.probabilities = new Array(timesCount);
        for (var j = 0; j < valuesCount; j++)
            pm.values[j] = j;
        for (var i = 0; i < timesCount; i++)
            pm.times[i] = i;
        for (var i = 0; i < timesCount; i++) {
            pm.probabilities[i] = new Array(valuesCount);
            pm.probabilities[i][0] = Math.abs(Math.cos(i / 3));
            pm.probabilities[i][1] = 1 - pm.probabilities[i][0];
        }
        return pm;
    }

    private GetProbabilityMap21() {
        var pm = new PM.ProbabilityMap();
        var timesCount = 50, valuesCount = 2;
        pm.times = new Array(timesCount);
        pm.values = new Array(valuesCount);
        pm.probabilities = new Array(timesCount);
        for (var j = 0; j < valuesCount; j++)
            pm.values[j] = j;
        for (var i = 0; i < timesCount; i++)
            pm.times[i] = i;
        for (var i = 0; i < timesCount; i++) {
            pm.probabilities[i] = new Array(valuesCount);
            pm.probabilities[i][0] = Math.abs(Math.sin(i / 3));
            pm.probabilities[i][1] = 1 - pm.probabilities[i][0];
        }
        return pm;
    }

    private GetProbabilityMap22() {
        var pm = new PM.ProbabilityMap();
        var timesCount = 20, valuesCount = 2;
        pm.times = new Array(timesCount);
        pm.values = new Array(valuesCount);
        pm.probabilities = new Array(timesCount);
        for (var j = 0; j < valuesCount; j++)
            pm.values[j] = j;
        for (var i = 0; i < timesCount; i++)
            pm.times[i] = i;
        for (var i = 0; i < timesCount; i++) {
            pm.probabilities[i] = new Array(valuesCount);
            pm.probabilities[i][0] = Math.abs(Math.cos(i / 3));
            pm.probabilities[i][1] = 1 - pm.probabilities[i][0];
        }
        return pm;
    }

    GetProbabilityMap(probabilityToken: any, species: string): Rx.Observable<PM.ProbabilityMap> {
        if (probabilityToken == info1.ProbabilitiesToken && species == "Species 1")
            return Rx.Observable.from([this.GetProbabilityMap11()]);
        else if (probabilityToken == info1.ProbabilitiesToken && species == "Species 2")
            return Rx.Observable.from([this.GetProbabilityMap12()]);
        else if (probabilityToken == info2.ProbabilitiesToken && species == "Species 1")
            return Rx.Observable.from([this.GetProbabilityMap21()]);
        else if (probabilityToken == info2.ProbabilitiesToken && species == "Species 2")
            return Rx.Observable.from([this.GetProbabilityMap22()]);
        else
            throw new Error("unknown species");
    }
}

var provider = new FakeProvider();
var spatialSettings = new SpatialViewerSettings("blue,green,red", false);
var sample = new PM.ProbabilityMapView(provider, spatialSettings);
sample.Bind($('#probabilityMap').get(0));
sample.onNext({
    MessageType: Framework.VisualizationUpdateMessageType.PlotSettingsInfo,
    EncapsulatedUpdate: {
        XLabel: "Time",
        YLabel: "Marginal Probability",
        Title: "",
        LabelFontSize: 16,
        TickFontSize: 12,
        XTicks: [],
        YTicks: [],
        XMin: null,
        XMax: null,
        YMin: null,
        YMax: null,
        HBoundaries: [],
        VBoundaries: []
    }
});
sample.onNext({
    MessageType: Framework.VisualizationUpdateMessageType.Probabilities,
    EncapsulatedUpdate: info1
});
sample.onNext({
    MessageType: Framework.VisualizationUpdateMessageType.Probabilities,
    EncapsulatedUpdate: info2
});