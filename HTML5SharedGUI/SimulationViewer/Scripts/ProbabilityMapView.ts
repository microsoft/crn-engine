// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// (FP) This file contains the implementation for the probability map control. Note that I made this myself, so it might be a bit different from the other designs in this project. The overall design assumes that probability maps are generated on-demand and asynchronously. Therefore, the higher layer needs to provide this control with the data required to generate probability maps, the list of species for which a probability map can be generated, and the function that asynchronously generates a probability map. The control will invoke this function with that data when the user requires it.

declare var SVG: any;
import "svg";

import * as ko from "knockout";
import "idd";
declare var InteractiveDataDisplay: any;
import * as $ from "jquery";
import "jqueryui";
import { saveAs } from "file-saver";
import GetExporter from "../../GenericComponents/Scripts/SVGExporter";
import * as Framework from "./SimulationViewerFramework";
import * as SpatialViewerSettings from "./SpatialViewerSettings";
import PaletteEditor from "../../GenericComponents/Scripts/PaletteEditor";
import * as I from "../../GenericComponents/Scripts/Interfaces";
InteractiveDataDisplay.HeatmapworkerPath = "scripts/IDD/";

import * as template from 'raw-loader!../html/ProbabilityMapView.html';
ko.components.register("probability-map", {
    template: template,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            var $_elem = $(componentInfo.element);//? add plot which contains map and bars to export svg
            var context = ko.contextFor(componentInfo.element);
            var vm = context == null ? {} : context.$data;

            // Configure the IDD box.                        
            var heatMap = InteractiveDataDisplay.asPlot($("div.j-probability-map", $_elem));
            heatMap.navigation.gestureSource = undefined;
            heatMap.padding = 1;
            var barChart = InteractiveDataDisplay.asPlot($("div.j-probability-bars", $_elem));
            barChart.navigation.gestureSource = undefined;
            barChart.padding = 1;
            heatMap.centralPart.click((event: JQueryEventObject) => vm.onClick(heatMap, event));
            var legend = $(".idd-legend-cover", $_elem);
            legend.draggable({ containment: $_elem.find("div.j-probability-map") });

            return context == null ? {} : context.$data;
        }
    }
});

// (FP) This class represents a probability map, in the form used by this control. This is the shape that the higher layers need to provide.
export class ProbabilityMap {
    times: number[];
    values: number[];
    probabilities: number[][];
}
export type IddAxisLabels = {
    type: string;
    attachGrid: boolean;
    ticks?: Array<number>;
    labels?: Array<string>;
    rotate?: boolean;
    fontSize?: number;
};
// (FP) This interface represents an object that is capable of asynchronously producing a probability map for a given species. A "token" is provided; this is an arbitrary object instance used by the probability map provider.
export interface IProbabilityMapProvider {
    GetProbabilityMap(token: any, species: string, lowerBound: number): Rx.Observable<ProbabilityMap>;
}

// This is the viewmodel for a species' mean plot.
class MeanPlotVM {
    constructor(public InstanceName: string, public Name: string, public Counts: number[], public Times: number[]) { }
}

type MeanPlotStorage = { [instanceName: string]: { [speciesName: string]: MeanPlotVM } };

// This is the viewmodel for the control.
class ViewModel {
    // (FP) The following three observables contain the data for the currently loaded probability map, and will be bound to IDD.
    public times: KnockoutObservableArray<number> = ko.observableArray([]);
    public srcValues: KnockoutObservableArray<number> = ko.observableArray([]);
    public values: KnockoutComputed<number[]> = ko.pureComputed(() => {
        var values = this.srcValues();
        return values.length == 0 ? values : [values[0] - 0.5].concat(values, [values[values.length - 1] + 0.5])
    });
    public srcProbabilities: KnockoutObservableArray<number[]> = ko.observableArray([]);
    public probabilities: KnockoutComputed<number[][]> = ko.pureComputed(() => {
        var probabilities = this.srcProbabilities();
        var values = this.srcValues();
        var lowerBound = this.LowerBound();
        var probs = probabilities.map(value => values.length == 0 ? value : [0.0].concat(value, [0.0]));
        probs = probs.map(p => p.map(pp => pp < lowerBound ? lowerBound : pp));
        return probs;
    });

    public IsLegendVisible = ko.observable(true);

    // (FP) The following three observables contain the available sets of probability data (usually one, but there may be more in case of sweeps), and the currently selected set. The second observable is a computed that provides the same set, only sorted.
    public infos: KnockoutObservableArray<Framework.IProbabilitiesInfo> = ko.observableArray([]);
    public sortedInfos: KnockoutComputed<Framework.IProbabilitiesInfo[]> = ko.computed(() => {
        return this.infos().sort((a, b) => a.InstanceName > b.InstanceName ? 1 : a.InstanceName < b.InstanceName ? -1 : 0);
    });
    public currentInfo: KnockoutObservable<Framework.IProbabilitiesInfo> = ko.observable(null);

    // (FP) The following two observables contain the set of available species (for the currently selected set of probability data), and the currently selected species.
    public speciesNames: KnockoutComputed<string[]> = ko.computed(() => this.currentInfo() == null ? [] : this.currentInfo().Names);
    public currentSpecies: KnockoutObservable<string> = ko.observable("");

    // This contains the mean plot data.
    public MeanPlots: MeanPlotStorage = {};

    // (FP) The following two observables are used with probability bars.
    public BarWidths: KnockoutObservableArray<number> = ko.observableArray([]);
    public BarLocations: KnockoutObservableArray<number> = ko.observableArray([]);

    public KnownSpeciesNames: Array<string> = [];
    // (FP) These two observables contain the data for the mean plot.
    public Counts: KnockoutObservableArray<number> = ko.observableArray([]);
    public Times: KnockoutObservableArray<number> = ko.observableArray([]);

    public ChosenTime: number = 0.0;
    public ChosenTimeIndex: KnockoutObservable<number> = ko.observable(0);
    public ChosenTimeLocation: KnockoutObservableArray<number> = ko.observableArray([]);
    public BarsLabelText: KnockoutComputed<string> = ko.computed(() => "Marginal Probability (T = " + (this.times().length != 0 ? this.ChosenTime.toFixed(2) : 0.0) + ")");

    public labelSize: KnockoutObservable<string> = ko.observable("16px");
    public XLabel: KnockoutObservable<string> = ko.observable("Time");
    public YLabel: KnockoutObservable<string> = ko.observable("");
    public XTicks: KnockoutObservable<IddAxisLabels> = ko.observable({
        type: "numeric",
        attachGrid: true,
        ticks: [],
        labels: [],
        fontSize: 16
    });
    public YTicks: KnockoutObservable<IddAxisLabels> = ko.observable({
        type: "numeric",
        attachGrid: true,
        rotate: true,
        fontSize: 16
    });
    public ProbabilityTicks: KnockoutObservable<IddAxisLabels> = ko.observable({
        type: "numeric",
        attachGrid: true,
        fontSize: 16
    });

    public LowerBound = ko.observable(1e-8);

    palette: PaletteEditor = new PaletteEditor();

    public CaptureSVG: () => void;

    constructor(public maps: IProbabilityMapProvider) {
        var that = this;
        // (FP) I'm subscribing to my own user-bound observables, so that when the user changes them I can start the asynchronous calculation of a new probability map.
        this.currentInfo.subscribe(() => that.requestUpdate());
        this.currentSpecies.subscribe(() => that.requestUpdate());
    }

    private getNearestPoint(x: number): number {
        var length = this.times().length;
        if (length != 0) {
            if (x < this.times()[0]) return 0;
            if (x > this.times()[length - 1]) return length - 1;
            for (var i = 0; i < this.times().length - 1; i++) {
                if (x > this.times()[i] && x < this.times()[i + 1]) {
                    return Math.abs(x - this.times()[i]) > Math.abs(x - this.times()[i + 1]) ? i : i + 1;
                }
            }
        }
        return 0;
    }

    onClick(heatMap: any, event: JQueryEventObject) {
        var originHost = InteractiveDataDisplay.Gestures.getXBrowserMouseOrigin(heatMap.host, event);
        var originCentralPart = InteractiveDataDisplay.Gestures.getXBrowserMouseOrigin(heatMap.centralPart, event);
        var ct = heatMap.coordinateTransform;
        var px = ct.screenToPlotX(originCentralPart.x);
        this.ChosenTime = px;
        var nearest = this.getNearestPoint(px);
        this.ChosenTimeIndex(nearest);
        this.updateBars();
    }

    updateBars() {
        var atLastTime = this.ChosenTimeIndex() < 0 || this.ChosenTimeIndex() >= this.times().length - 1;
        var values = this.srcValues();
        var times = this.times();
        var probabilities = this.srcProbabilities();
        this.BarLocations(values);
        if (atLastTime) {
            // Set the current time for the bar chart to the last available time.
            this.ChosenTime = times[times.length - 1];
            this.ChosenTimeIndex(times.length - 1);
        }
        this.BarWidths(probabilities[this.ChosenTimeIndex()]);
        var linePoints: number[] = new Array<number>(values.length);
        for (var i = 0; i < values.length; i++) linePoints[i] = this.ChosenTime;
        if (values.length != 0) linePoints = [this.ChosenTime, this.ChosenTime].concat(linePoints);
        this.ChosenTimeLocation(linePoints);
    }

    private currentUpdateHandle: number = 0;

    private requestUpdate() {
        if (this.currentUpdateHandle != 0)
            clearTimeout(this.currentUpdateHandle);
        var that = this;
        this.currentUpdateHandle = setTimeout(() => that.runUpdate());
    }

    // This function starts generating a probability map (or clears the view, if no species is selected).
    private runUpdate() {
        var lowerBound = this.LowerBound();
        var species = this.currentSpecies();
        var that = this;
        if (this.speciesNames().indexOf(species) < 0)
            this.blank();
        else
            // Start asynchronous generation; also prepare to put the resulting data in this VM.
            this.maps.GetProbabilityMap(this.currentInfo().ProbabilitiesToken, species, lowerBound).subscribeOnNext(map => {
                that.times(map.times);
                that.srcValues(map.values);
                that.srcProbabilities(map.probabilities);
                that.updateBars();

                var meanPlot = null;
                var instanceName = that.currentInfo().InstanceName;
                var meanPlotInstance = that.MeanPlots[instanceName];
                if (meanPlotInstance != null)
                    meanPlot = meanPlotInstance[that.currentSpecies()];
                if (meanPlot == null) {
                    that.Times([]);
                    that.Counts([]);
                }
                else {
                    that.Times(meanPlot.Times);
                    that.Counts(meanPlot.Counts);
                }
            });
    }

    // (FP) This function resets the control; it clears both the current selection, and the available data.
    reset() {
        this.currentInfo(null);
        this.infos([]);
        this.currentSpecies("");
        this.MeanPlots = {};
        this.blank();
    }

    // (FP) This function clears the current selection.
    blank() {
        this.times([]);
        this.srcValues([]);
        this.srcProbabilities([]);
        this.KnownSpeciesNames = [];
        this.ChosenTime = 0.0;
        this.ChosenTimeIndex(-1);
        this.ChosenTimeLocation([]);
        this.Times([]);
        this.Counts([]);
    }
}

// (FP) This class implements the control. Note that it extends SvgExportableView, because it can generate SVG.
// Design note: although this implements IViewObserver, I'm not actually using this as part of the larger simulation Viewer. Maybe this should change.
export class ProbabilityMapView implements Rx.IObserver<Framework.IVisualizationUpdateMessage> {
    private vm: ViewModel;
    private bufferIsUpdating: boolean;
    private buffer: MeanPlotStorage;
    private instanceNames: { [instanceName: string]: string };

    public Reset() {
        this.vm.reset();
        this.instanceNames = {};
    }

    pushBuffer() {
        for (let instanceName in this.buffer) {
            var sweepBuffer = this.buffer[instanceName];
            for (var sn in sweepBuffer) {
                var speciesBuffer = sweepBuffer[sn];
                var meanPlotSweep = this.vm.MeanPlots[instanceName];
                if (meanPlotSweep == null)
                    meanPlotSweep = this.vm.MeanPlots[instanceName] = {};
                var meanPlot = meanPlotSweep[sn];
                if (meanPlot == null)
                    meanPlot = meanPlotSweep[sn] = new MeanPlotVM(instanceName, sn, [], []);
                ko.utils.arrayPushAll<number>(meanPlot.Counts, speciesBuffer.Counts);
                ko.utils.arrayPushAll<number>(meanPlot.Times, speciesBuffer.Times);
            }
        }
        this.buffer = {};
        this.bufferIsUpdating = false;
    }

    public onNext(update: Framework.IVisualizationUpdateMessage) {
        switch (update.MessageType) {
            case Framework.VisualizationUpdateMessageType.Probabilities:
                this.vm.infos.push(<Framework.IProbabilitiesInfo>update.EncapsulatedUpdate);
                // If this is the first set, select it.
                if (this.vm.infos().length == 1)
                    this.vm.currentInfo(<Framework.IProbabilitiesInfo>update.EncapsulatedUpdate);
                break;
            case Framework.VisualizationUpdateMessageType.PlotSettingsInfo:
                this.vm.XLabel((<Framework.PlotSettings>update.EncapsulatedUpdate).XLabel);
                this.vm.YLabel((<Framework.PlotSettings>update.EncapsulatedUpdate).YLabel);
                this.vm.labelSize((<Framework.PlotSettings>update.EncapsulatedUpdate).LabelFontSize + "px");
                this.vm.XTicks({
                    type: (<Framework.PlotSettings>update.EncapsulatedUpdate).XTicks.length == 0 ? "numeric" : "labels",
                    attachGrid: true,
                    ticks: (<Framework.PlotSettings>update.EncapsulatedUpdate).XTicks,
                    labels: (<Framework.PlotSettings>update.EncapsulatedUpdate).XTicks.map(value => value.toString()),
                    fontSize: (<Framework.PlotSettings>update.EncapsulatedUpdate).TickFontSize
                });
                this.vm.YTicks({
                    type: "numeric",
                    attachGrid: true,
                    fontSize: (<Framework.PlotSettings>update.EncapsulatedUpdate).TickFontSize
                });
                this.vm.ProbabilityTicks({
                    type: "numeric",
                    attachGrid: true,
                    fontSize: (<Framework.PlotSettings>update.EncapsulatedUpdate).TickFontSize
                });
                break;
            case Framework.VisualizationUpdateMessageType.TraceDefinitions:
                var traces = <I.ITraceDefinitions>update.EncapsulatedUpdate;
                var instances = traces.CRNs[0].Settings[0].Sweeps[0].Instances;
                for (var i = 0; i < instances.length; i++)
                    this.instanceNames[instances[i].ID] = instances[i].Name;
                var species = instances[0].Plottables;
                for (var i = 0; i < species.length; i++)
                    this.vm.KnownSpeciesNames.push(species[i].Name);
                break;
            case Framework.VisualizationUpdateMessageType.SimulationStep:
                var step = (<Framework.ISimStepData>update.EncapsulatedUpdate);

                // Copy the sim step to the buffer.
                var sweepBuffer = this.buffer[step.Instance];
                if (sweepBuffer == null)
                    sweepBuffer = this.buffer[step.Instance] = {};
                var speciesNames = this.vm.KnownSpeciesNames;
                for (var i = 0; i < speciesNames.length; i++) {
                    var speciesName = speciesNames[i];
                    var speciesBuffer = sweepBuffer[speciesName];
                    if (speciesBuffer == null)
                        speciesBuffer = sweepBuffer[speciesName] = new MeanPlotVM(this.instanceNames[step.Instance], speciesName, [], []);
                    speciesBuffer.Counts.push(step.Counts[i]);
                    speciesBuffer.Times.push(step.Time);
                    if (!this.bufferIsUpdating) {
                        this.bufferIsUpdating = true;
                        setTimeout(() => { this.pushBuffer(); });
                    }
                }
                break;
            case Framework.VisualizationUpdateMessageType.Reset:
                this.Reset();
                break;
        }
    }

    public onError(error: any) {
        console.log("ProbabilityMapView: " + JSON.stringify(error));
    }

    public onCompleted() { }

    constructor(maps: IProbabilityMapProvider, settings: SpatialViewerSettings.StoredSettings) {
        var vm = new ViewModel(maps);
        vm.palette.IDDPalette = settings.colorPalette;
        vm.palette.LogColors = settings.isLogPalette;

        this.vm = vm;
        this.bufferIsUpdating = false;
        this.buffer = {};
    }

    // Construct the control in the given element.
    Bind(div: HTMLElement) {
        if (div == null)
            throw "Attempt to bind ProbabilityMapView to null";
        var that = this;
        ko.cleanNode(div);
        ko.applyBindings(that.vm, div);

        this.vm.CaptureSVG = GetExporter("probability-map", div, (el: HTMLElement) => {
            // Export the heatmap, but first hide the time selector if present.
            var heatmapPlot = InteractiveDataDisplay.asPlot($("div.j-probability-map", el)[0]);
            var timeSelector = InteractiveDataDisplay.asPlot($('.j-time-selector', el)[0]);
            timeSelector.children.forEach(function (child: any) { child.isVisible = false; });
            var heatmapSvg = heatmapPlot.exportToSvg();
            timeSelector.children.forEach(function (child: any) { child.isVisible = true; });
            // Export the bars.
            var barsPlot = InteractiveDataDisplay.asPlot($("div.j-probability-bars", el)[0]);
            var barsSvg = barsPlot.exportToSvg();
            // Merge the heatmap and bars.
            var svgHost = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            var exportSvg = SVG(svgHost).size(heatmapSvg.width() + barsSvg.width(), heatmapSvg.height());
            exportSvg.nested().add(heatmapSvg);
            exportSvg.nested().add(barsSvg).x(heatmapSvg.width());
            return exportSvg.svg();
        });
    }
}
