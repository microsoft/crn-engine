declare var SVG: any;
import "svg";
import * as ko from "knockout";
import "idd";
declare var InteractiveDataDisplay: any;
import * as $ from "jquery";
import GetExporter from "../../GenericComponents/Scripts/SVGExporter";
import * as Framework from "./SimulationViewerFramework";
import * as SpatialViewerSettings from "./SpatialViewerSettings";
import { saveAs } from "file-saver";
import "jquery-mousewheel";
import * as ProgressView from "./ProgressView";
import * as SingleSpeciesFilter from "../../GenericComponents/Scripts/SingleFilter";
import PaletteEditor from "../../GenericComponents/Scripts/PaletteEditor";
import * as I from "../../GenericComponents/Scripts/Interfaces";
InteractiveDataDisplay.HeatmapworkerPath = "scripts/IDD/";

import * as template from 'raw-loader!../html/spatial-simulation-1d.html';
ko.components.register("spatial-simulation-1d", {
    template: template,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            if (componentInfo.element == null)
                throw "Attempt to create a Spatial1D VM for null";
            var $_elem = $(componentInfo.element);
            var context = ko.contextFor(componentInfo.element);
            var vm = context == null ? {} : context.$data;

            // Setup the IDD box.
            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $_elem));
            var gestureSource = InteractiveDataDisplay.Gestures.getGesturesStream(plot.centralPart);
            var bottomAxisGestures = InteractiveDataDisplay.Gestures.applyHorizontalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-bottom-axis", $_elem)));
            var leftAxisGestures = InteractiveDataDisplay.Gestures.applyVerticalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-left-axis", $_elem)));
            plot.navigation.gestureSource = gestureSource.merge(bottomAxisGestures.merge(leftAxisGestures));
            var legend = $(".idd-legend-cover", $_elem);
            legend.draggable({ containment: $_elem.children("div[data-idd-plot='figure']") });
            new InteractiveDataDisplay.Legend(plot, legend, true);
            return vm;
        }
    }
});

export type IddAxisLabels = {
    type: string;
    ticks: Array<number>;
    labels: Array<string>;
    attachGrid: boolean;
    rotate?: boolean;
    fontSize?: number;
};

class InstanceData {
    constructor(public ID: number) { }
    public Time: KnockoutObservableArray<number> = ko.observableArray([]).extend({ rateLimit: 1000 });
    public RawData: KnockoutObservableArray<number[][]> = ko.observableArray([]).extend({ rateLimit: 1000 });
}

// (FP) The VM for this control; it's fairly straightforward. Note that it supports SVG export. Also, it's important to note that in "data" here, there is an array with as many elements as the values of x-axis, and each of those has as many elements as the values of time. Therefore, data coming in the form of Increment1D will need to be reshaped.
class ViewModel {
    public Filter: SingleSpeciesFilter.View = new SingleSpeciesFilter.View();
    public Progress: ProgressView.ProgressView = new ProgressView.ProgressView();

    xAxis: KnockoutObservableArray<number> = ko.observableArray([]);
    instances: KnockoutObservable<InstanceData[]> = ko.observable([]);
    tAxis: KnockoutComputed<number[]> = ko.pureComputed(() => {
        let instance = this.currentInstance();
        let instanceData = this.instances()[instance];
        if (instanceData == null)
            return [];
        return instanceData.Time();
    })
    currentInstance: KnockoutObservable<number> = ko.observable(0);
    currentSpecies: KnockoutObservable<number> = ko.observable(0);

    // The data for the selected species of the selected instance.
    data: KnockoutComputed<number[][]> = ko.pureComputed(() => {
        let xLen = this.xAxis().length;
        let instance = this.currentInstance();
        let instanceData = this.instances()[instance];
        if (instanceData == null)
            return [];
        var rawData = instanceData.RawData();
        var species = this.currentSpecies();
        var ret: number[][] = [];
        if (species >= 0)
            for (var x = 0; x < xLen; x++) {
                ret[x] = [];
                for (var y = 0; y < rawData.length; y++) {
                    ret[x][y] = rawData[y][species][x];
                }
            }
        return ret;
    });
    XLabel: KnockoutObservable<string> = ko.observable("");
    YLabel: KnockoutObservable<string> = ko.observable("");
    XTicks: KnockoutObservable<IddAxisLabels> = ko.observable({
        type: "numeric",
        ticks: [],
        labels: [],
        attachGrid: true,
        fontSize: 16
    });
    YTicks: KnockoutObservable<IddAxisLabels> = ko.observable({
        type: "numeric",
        ticks: [],
        labels: [],
        attachGrid: true,
        rotate: true,
        fontSize: 16
    });
    labelSize: KnockoutObservable<string> = ko.observable("16px");
    IsLegendVisible = ko.observable(true);

    palette: PaletteEditor = new PaletteEditor();

    public CaptureSVG: () => void;
}

// (FP) This class implements the control. Note that it supports SVG exports.
export class Viewer implements Rx.IObserver<Framework.IVisualizationUpdateMessage> {
    private vm: ViewModel;

    private Reset() {
        this.vm.Filter.SetAvailableTraces({ ModelName: "", CRNs: [] });
        this.vm.instances([]);
        this.vm.xAxis([]);
        this.vm.tAxis([]);
    }

    constructor(settings: Framework.ISpatialViewerSettings) {
        var vm = new ViewModel();
        vm.palette.IDDPalette = settings.colorPalette;
        vm.palette.LogColors = settings.isLogPalette;
        this.vm = vm;
    }

    public ShowInstance(id: number) {
        this.vm.currentInstance(id);
    }

    public ShowSpecies(idx: number) {
        this.vm.currentSpecies(idx);
    }

    // (FP) Construct the control inside the specified element.
    Bind(div: HTMLElement) {
        if (div == null)
            throw "Attempt to bind Spatial1DTimeseriesView to null";
        ko.cleanNode(div);
        ko.applyBindings(this.vm, div);

        this.vm.Filter.GetSelectedSpecies().subscribe(names => {
            this.ShowSpecies(names);
        });
        this.vm.Filter.GetSelectedInstance().subscribe(instance => {
            this.ShowInstance(instance);
        });

        this.vm.CaptureSVG = GetExporter("simulation", div);
    }

    // (FP) The caller needs to use this function to provide a source of simulation data.
    public onNext(update: Framework.IVisualizationUpdateMessage) {
        var vm = this.vm;
        vm.Progress.onNext(update);
        switch (update.MessageType) {
            case Framework.VisualizationUpdateMessageType.TraceDefinitions:
                var traces = <I.ITraceDefinitions>update.EncapsulatedUpdate;
                vm.Filter.SetAvailableTraces(traces);
                // Create the observable arrays for each instance.
                var instances: InstanceData[] = [];
                for (let crn of traces.CRNs)
                    for (let settings of crn.Settings)
                        for (let sweep of settings.Sweeps)
                            for (let instance of sweep.Instances)
                                instances[instance.ID] = new InstanceData(instance.ID);
                this.vm.instances(instances);
                break;
            case Framework.VisualizationUpdateMessageType.SpatialSpace:
                var sp = <Framework.ISpatialSpace>update.EncapsulatedUpdate;
                vm.xAxis(sp.XAxis);
                break;
            case Framework.VisualizationUpdateMessageType.AdditionalPlottable:
                var newPlottable = <I.IAdditionalPlottable>update.EncapsulatedUpdate;
                vm.Filter.AddSpeciesToInstance(newPlottable);
                console.log("JIT plots not currently supported in spatial view");
                break;
            case Framework.VisualizationUpdateMessageType.SimulationStep1D:
                var data = <Framework.I1DSimStepData>update.EncapsulatedUpdate;
                let instanceData = vm.instances()[data.Instance];
                instanceData.Time.push(data.Time);
                instanceData.RawData.push(data.Data);
                break;
            case Framework.VisualizationUpdateMessageType.PlotSettingsInfo:
                var settings = <Framework.PlotSettings>update.EncapsulatedUpdate;
                vm.XLabel(settings.XLabel);
                vm.YLabel(settings.YLabel);
                vm.XTicks({
                    type: settings.XTicks.length == 0 ? "numeric" : "labels",
                    ticks: settings.XTicks,
                    labels: settings.XTicks.map(value => value.toString()),
                    attachGrid: true,
                    fontSize: settings.TickFontSize
                });
                vm.YTicks({
                    type: settings.YTicks.length == 0 ? "numeric" : "labels",
                    ticks: settings.YTicks,
                    labels: settings.YTicks.map(value => value.toString()),
                    attachGrid: true,
                    rotate: true,
                    fontSize: settings.TickFontSize
                });
                vm.labelSize(settings.LabelFontSize + "px");
                break;
            case Framework.VisualizationUpdateMessageType.Reset:
                this.Reset();
                break;
            default:
                break;
        }
    }

    public onError(exception: any) {
        console.log("Spatial1DTimeseriesView exception: " + JSON.stringify(exception));
        throw exception;
    }

    public onCompleted() {
    }
}