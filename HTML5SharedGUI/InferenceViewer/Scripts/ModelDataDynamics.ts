// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// (FP) This file contains the implementation of the dynamics control (the inference plotter).

declare var SVG: any;
import "svg";

import * as ko from "knockout";
import * as $ from "jquery";
import "jquery-mousewheel";
import "idd";
import "jqueryui";
declare var InteractiveDataDisplay: any;
import { saveAs } from "file-saver";
import GetExporter from "../../GenericComponents/Scripts/SVGExporter";
import * as Inference from "./InferenceViewer";
import { InferenceProgress } from "./InferenceProgress";
import "../../GenericComponents/Scripts/Dropdown";
import TraceColourEditor from "../../GenericComponents/Scripts/TraceColourEditor";
import * as I from "../../GenericComponents/Scripts/Interfaces";
import * as MultiFilter from "../../GenericComponents/Scripts/MultiFilter";
import * as template from "raw-loader!../html/model-data-dynamics.html";

ko.components.register("model-data-dynamics", {
    template: template,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            if (componentInfo.element == null)
                throw "Attempt to create a ModelDataDynamics VM for null";
            // Setup IDD.
            var $_elem = $(componentInfo.element);
            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $_elem));
            var gestureSource = InteractiveDataDisplay.Gestures.getGesturesStream(plot.centralPart);
            plot.navigation.gestureSource = gestureSource;
            var bottomAxisGestures = InteractiveDataDisplay.Gestures.applyHorizontalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-mdd__bottomaxis", $_elem)));
            var leftAxisGestures = InteractiveDataDisplay.Gestures.applyVerticalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-mdd__leftaxis", $_elem)));
            plot.navigation.gestureSource = gestureSource.merge(bottomAxisGestures.merge(leftAxisGestures));

            var legend = $(".idd-legend-cover", $_elem);
            legend.draggable({ containment: $_elem.children("div[data-idd-plot='figure']") });
            new InteractiveDataDisplay.Legend(plot, legend, true);

            // Finally, pass the parent VM as this component's VM.
            var context = ko.contextFor(componentInfo.element);
            return context == null ? {} : context.$data;
        }
    }
});

function getOriginalTarget(event: any): Element {
    if (event.originalEvent.srcElement != null)
        return event.originalEvent.srcElement;
    return event.originalEvent.originalTarget;
}

// (FP) This is the VM for the data for one plottable. It contains all of the observed values for that plottable, and all of the simulated values for that plottable.
class PlottableVM {
    public SimulationCounts: KnockoutObservable<number[]>;
    public SimulationTimes: KnockoutObservable<number[]>;
    public IsVisible: KnockoutObservable<boolean>;
    public DisplayName: KnockoutComputed<string>;
    public DisplayStructural: KnockoutComputed<string>;
    public VisibleName: KnockoutComputed<string>;

    public Colour: TraceColourEditor;

    // (FP) Constructs a new plot VM. Note that you should provide initial values. They will be copied to the internal buffers.
    constructor(owner: ModelDataVM, public Name: string, public StructuralString: string, public InstanceID: number, public InstanceName: string, public System: string, public Settings: string, public Sweep: string, public ObservationName: string, public ObservationTimes: number[], public ObservationCounts: number[], originalColour: string) {
        this.IsVisible = ko.observable(true);
        var speciesPostfix = (() => {
            let tokens = [this.System, this.Settings, this.InstanceName].filter(t => t != "");
            if (tokens.length == 0)
                return "";
            return "(" + tokens.join('/') + ")";
        })();
        this.DisplayName = ko.computed(() => this.Name + speciesPostfix);
        this.DisplayStructural = ko.computed(() => this.StructuralString + speciesPostfix);
        this.VisibleName = ko.computed(() => owner.AreNamesEnabled() ? this.DisplayName() : this.DisplayStructural());
        this.SimulationCounts = ko.observable([]);
        this.SimulationTimes = ko.observable([]);

        this.Colour = new TraceColourEditor(this.VisibleName, originalColour == null ? "black" : originalColour);
    }
}

// (FP) This is the VM for the control. It contains the Plottable instances representing all available data, plus other observables that control which ones are being displayed. Note that the choice of which species are plotted is hierarchical, i.e. the user can choose a set of sweeps and a set of species, and this determines a set of plots. Then, the user can choose within that set of plots (using the Display property of Plottable above).
// Design note: selection here is handled in the form of a separate array of strings, with selection being defined as the string being in that array. This seems somewhat inefficient in that all tests require iteration on this array. That said, the array is never going to be very long, so it doesn't matter much. The alternative would be to have species and sweep instances be arrays of an object more complex than a plain string, that includes a boolean flag for selection.
class ModelDataVM {

    public Filter: MultiFilter.View;

    public IsLegendVisible = ko.observable(true);
    public IsGridVisible = ko.observable(true);

    // Here, by "allowed", we mean "selected by the user for displaying".
    public AllowedSpeciesNames: KnockoutObservableArray<string> = ko.observableArray([]);
    public AllowedInstanceNames: KnockoutObservableArray<string> = ko.observableArray([]);
    public AllowedSweepNames: KnockoutObservableArray<string> = ko.observableArray([]);
    public AllowedSettingsNames: KnockoutObservableArray<string> = ko.observableArray([]);
    public AllowedSystemNames: KnockoutObservableArray<string> = ko.observableArray([]);

    public AllPlottables: KnockoutObservableArray<PlottableVM>;
    public FilteredPlottables: KnockoutComputed<PlottableVM[]>;
    public VisiblePlottables: KnockoutComputed<PlottableVM[]>;
    public SelectedAllVisible: KnockoutComputed<boolean>;

    public PlotCountCap = ko.observable(15);
    public PlotCountCapExceeded = ko.observable(false);

    public AreNamesEnabled: KnockoutObservable<boolean>;
    public AllSpeciesHaveStructural: KnockoutComputed<boolean>;

    constructor() {
        this.Filter = new MultiFilter.View();
        this.AreNamesEnabled = ko.observable(true);
        this.AllPlottables = ko.observableArray([]);

        this.FilteredPlottables = ko.pureComputed<PlottableVM[]>(() => { //external filter applied
            var all = this.AllPlottables();
            // The reason we are peeking at Allowed[...] here, without actually doing anything with them, is to establish in KO that this computed has a dependency on them. There are paths in this code that can get out of the function without ever calling them.
            this.AllowedInstanceNames.peek();
            this.AllowedSettingsNames.peek();
            this.AllowedSweepNames.peek();
            this.AllowedSpeciesNames.peek();
            this.AllowedSystemNames.peek();
            var max = this.PlotCountCap();
            var c = 0;
            return all.filter((sp => {
                var spName = sp.Name;
                var instanceID = sp.InstanceID;
                var instance = sp.InstanceName;
                var sweep = sp.Sweep;
                var settings = sp.Settings;
                var system = sp.System;
                var res = ((this.AllowedSpeciesNames().indexOf(spName) > -1) && (!this.AllowedInstanceNames().every(allowed => allowed != instance)) && (!this.AllowedSettingsNames().every(allowed => allowed != settings)) && (!this.AllowedSweepNames().every(allowed => allowed != sweep)) && (!this.AllowedSystemNames().every(allowed => allowed != system)));
                if (!res)
                    return false;
                c++;
                this.PlotCountCapExceeded(c > max);
                return c <= max;
            }));
        });
        this.VisiblePlottables = ko.pureComputed<PlottableVM[]>(() => this.FilteredPlottables().filter(p => p.IsVisible()));

        this.SelectedAllVisible = ko.pureComputed<boolean>(
            {
                read: () => {
                    return this.FilteredPlottables().every((sp) => { return sp.IsVisible(); });
                },
                write: (value) => {
                    this.FilteredPlottables().forEach((sp) => {
                        sp.IsVisible(value);
                    });
                }
            });

        this.AllSpeciesHaveStructural = ko.pureComputed<boolean>(() => this.AllPlottables().length > 0 && this.AllPlottables().every(vm => vm.DisplayStructural() != null && vm.DisplayStructural() != ""));

        this.Filter.GetSelectedSpecies().subscribe(names => {
            this.AllowedSpeciesNames(names);
        });
        this.Filter.GetSelectedInstances().subscribe(instances => {
            this.AllowedInstanceNames(instances);
        });
        this.Filter.GetSelectedSweeps().subscribe(sweeps => {
            this.AllowedSweepNames(sweeps);
        })
        this.Filter.GetSelectedSettings().subscribe(settings => {
            this.AllowedSettingsNames(settings);
        });
        this.Filter.GetSelectedSystems().subscribe(systems => {
            this.AllowedSystemNames(systems);
        });

    }

    Units = ko.observable("");
    XLabel = ko.observable("");
    YLabel = ko.observable("");
    XTicks = ko.observable({
        type: "numeric",
        ticks: [],
        labels: [],
        attachGrid: true,
        fontSize: 16
    });
    YTicks = ko.observable({
        type: "numeric",
        ticks: [],
        labels: [],
        attachGrid: true,
        rotate: true,
        fontSize: 16
    });
    labelSize = ko.observable("16px");

    public AutoFitEnabled: KnockoutObservable<boolean> = ko.observable(true);
    public VisibleRegion: KnockoutObservable<{ x_min?: number; x_max?: number; y_min?: number; y_max?: number }> = ko.observable({});
    public AutoFitString = ko.pureComputed<string>(() => {
        if (!this.AutoFitEnabled())
            return "false";
        var v = this.VisibleRegion();
        if (v.x_min == null && v.x_max == null && v.y_min == null && v.y_max == null)
            return "true";
        return "bounds(" + (v.x_min == null ? "auto" : v.x_min) + "," + (v.x_max == null ? "auto" : v.x_max) + "," + (v.y_min == null ? "auto" : v.y_min) + "," + (v.y_max == null ? "auto" : v.y_max) + ")";
    });

    public VBoundaries: KnockoutObservable<number[]> = ko.observable([]);
    public XMin: KnockoutComputed<number> = ko.pureComputed<number>(() => Math.min(...this.VisiblePlottables().filter(p => p.SimulationTimes().length > 0).map(p => Math.min(...p.SimulationTimes()))));
    public XMax: KnockoutComputed<number> = ko.pureComputed<number>(() => Math.max(...this.VisiblePlottables().filter(p => p.SimulationTimes().length > 0).map(p => Math.max(...p.SimulationTimes()))));
    public HBoundaries: KnockoutObservable<number[]> = ko.observable([]);
    public YMin: KnockoutComputed<number> = ko.pureComputed<number>(() => Math.min(...this.VisiblePlottables().filter(p => p.SimulationTimes().length > 0).map(p => Math.min(...p.SimulationCounts()))));
    public YMax: KnockoutComputed<number> = ko.pureComputed<number>(() => Math.max(...this.VisiblePlottables().filter(p => p.SimulationTimes().length > 0).map(p => Math.max(...p.SimulationCounts()))));

    public Reset() {
        this.colorIndex = 0;
        this.Filter.SetAvailableTraces({ ModelName: "", CRNs: [] });
        this.AllPlottables.removeAll();
    }

    colorIndex = 0;

    private static palette: string[] = ["#FF0000", "#00AA00", "#0000FF", "#FFA500", "#FF00FF", "#00FFFF", "#551A8B", "#F4A460", "A9A9A9", "D3D3D3", "#FF00FF", "#000000", "#DA70D6", "#7CFC00", "#B0C4DE", "#E9967A", "#008080", "#BC8F8F", "#FFDEAD", "#808000", "#000080"];
    getNewColor = () => {
        // FP: replacing this with a fixed palette.
        //return tinycolor("#ff0000").spin(83 * this.colorIndex++).toHexString();
        return ModelDataVM.palette[this.colorIndex++ % ModelDataVM.palette.length];
    };
}

class GraphDataVM {
    constructor(private nodeSelector: Inference.INodeSelector) {
    }

    public Nodes: KnockoutObservable<{ [nodeId: string]: ModelDataVM }> = ko.observable({ "": new ModelDataVM() });
    public CurrentNode = ko.pureComputed(() => this.GetModelDataVM(this.nodeSelector.SelectedNodeID()));

    public CaptureSVG: () => void;

    public onChildrenChanged: () => void = () => { }; // to be set externally
    public onFrameRendered: () => void = () => { }; // to be set externally

    // (FP) An inference progress control VM (for the progress control).
    Progress = new InferenceProgress(this.nodeSelector);

    public Reset() { this.Nodes({ "": new ModelDataVM() }); }

    // Returns the model VM for the given node, or creates one if it doesn't exist yet.
    public GetModelDataVM(nodeId: string): ModelDataVM {
        var nodes = this.Nodes();
        if (nodes[nodeId] == null)
            nodes[nodeId] = new ModelDataVM();
        return nodes[nodeId];
    }
}

// (FP) This class implements the actual control. It's an IInferenceViewer because it can receive inference runs.
export class ModelDataDynamics implements Inference.IInferenceViewer {
    private vm = new GraphDataVM(this.nodeSelector);

    private container: HTMLElement;

    private isActive = false;

    constructor(private nodeSelector: Inference.INodeSelector, isActive?: boolean) {
        this.isActive = isActive == true;
    }

    // (FP) Call this to create the component in the specified element.
    Bind(div: HTMLElement) {
        if (div == null)
            throw "Attempt to bind ModelDataDynamics to null";
        var that = this;
        this.container = div;
        ko.cleanNode(div);
        ko.applyBindings(this.vm, div);
        this.vm.CaptureSVG = GetExporter("model-data-dynamics", div);

        $(div).on("activate", () => {
            that.isActive = true;
        });
        $(div).on("deactivate", () => {
            that.isActive = false;
        });
    }

    // (FP) The upper layer calls this to pass the data for an inference run. This implies resetting any current data. It is important to note that, in this design, the data is not being put in the VM immediately when it becomes available. Rather, it is placed in a buffer, and from there it gets copied to the VM in one big chunk within a setTimeout handler (i.e. when the main thread is idle). This is an optimization to avoid unnecessary updates.
    // Design note: I wonder whether this could be obtained in a more elegant fashion by using an Rx throttled observable?
    show(run: Inference.IInferenceRun) {
        var sampleFreq = 1000; // [ms] Update plots and parameters not more than once per sampleFreq
        // Decorates given set of observables by adding sampling to some of them to improve performance of this component.
        var sampledRun =
        {
            progress: run.progress,
            paramUpdates: run.paramUpdates.sample(sampleFreq), // <-- sampling
            simulationUpdates: run.simulationUpdates, // <-- it will be sampled below independently for each of the sweep key
            paramDefinitions: run.paramDefinitions,
            summary: run.summary,
            posteriorTableUpdates: run.posteriorTableUpdates,
            traceDefinitions: run.traceDefinitions,
            plotSettingsUpdates: run.plotSettingsUpdates
        };

        this.vm.Progress.show(sampledRun);
        this.vm.Reset();

        sampledRun.traceDefinitions.subscribe(
            data => {
                var vm = this.vm.GetModelDataVM(data.NodeID);
                var traces = <I.ITraceDefinitions><any>data;
                vm.Filter.SetAvailableTraces(traces);
                var vms: PlottableVM[] = [];
                for (let crn of traces.CRNs)
                    for (let settings of crn.Settings)
                        for (let sweep of settings.Sweeps)
                            for (let instance of sweep.Instances) {
                                var idx = vms.length;
                                for (let plot of instance.Plottables) {
                                    var color = (plot.Colour == null || plot.Colour == "") ? vm.getNewColor() : plot.Colour;
                                    var spVM = new PlottableVM(vm, plot.Name, plot.Structural, instance.ID, instance.Name, crn.Name, settings.Name, sweep.Name, plot.ObservationName, plot.ObservationTimes, plot.ObservationCounts, color);
                                    vms.push(spVM);
                                }
                            }
                vm.AllPlottables(vms);
            },
            (err) => { console.log(err); },
            () => { }
        );

        sampledRun.plotSettingsUpdates.subscribe(
            settings => {
                var vm = this.vm.GetModelDataVM(settings.NodeID);
                vm.XLabel(settings.XLabel);
                vm.YLabel(settings.YLabel);
                vm.XTicks({
                    type: settings.XTicks.length === 0 ? "numeric" : "labels",
                    ticks: settings.XTicks,
                    labels: settings.XTicks.map(value => value.toString()),
                    attachGrid: true,
                    fontSize: settings.TickFontSize
                });
                vm.YTicks({
                    type: settings.YTicks.length === 0 ? "numeric" : "labels",
                    ticks: settings.YTicks,
                    labels: settings.YTicks.map(value => value.toString()),
                    attachGrid: true,
                    rotate: true,
                    fontSize: settings.TickFontSize
                });
                vm.VBoundaries(settings.VBoundaries);
                vm.HBoundaries(settings.HBoundaries);
                vm.labelSize(settings.LabelFontSize + "px");

                vm.VisibleRegion({ x_min: settings.XMin, x_max: settings.XMax, y_min: settings.YMin, y_max: settings.YMax });
            }, err => console.log(err), () => { }
        );

        var isRendering: { [nodeId: string]: { [instance: number]: boolean } } = {}; // nodeId -> instance -> flag that is true if a rendering update is already in the pipeline for this instance
        var that = this;

        this.vm.onFrameRendered = () => {
            for (let node in isRendering)
                for (let instance in isRendering[node])
                    isRendering[node][instance] = false;
        };

        // We do sampling of update independently for each of the instances, so that plots for each of the instance are updated with given frequency. This allows to reduce CPU load caused by IDD.

        // Moves the data to the actual observables.
        let updateInstance = (val: Inference.IPlottableValues) => {
            var vm = this.vm.GetModelDataVM(val.NodeID);
            var instancePlottables = vm.AllPlottables().filter(plottable => plottable.InstanceID === val.InstanceID);
            // Expected: instancePlottables.length == val.Values.length && val.Times.length == val.Values[x].length
            for (var i = 0; i < instancePlottables.length; i++) {
                var plottable = instancePlottables[i];
                var values = val.Values[i];
                plottable.SimulationTimes(val.Times);
                plottable.SimulationCounts(values);
            }
        };

        run.simulationUpdates
            .groupBy(val => val.NodeID)
            .subscribe(nodes => {
                var nodeID = nodes.key;
                if (isRendering[nodeID] == null)
                    isRendering[nodeID] = {};
                nodes.groupBy(val => val.InstanceID)
                    .subscribe(obsInstances => { // onNext instance group
                        var instanceID = obsInstances.key;
                        isRendering[nodeID][instanceID] = false;
                        obsInstances
                            .sample(20)
                            .filter(val => that.isActive)
                            .subscribe(val => { // onNext for an instance name
                                if (!isRendering[nodeID][instanceID]) {
                                    isRendering[nodeID][instanceID] = true;
                                    setTimeout(() => updateInstance(val), 20);
                                }
                            }, err => console.log(err), () => { });
                        obsInstances // we always draw the last message for each of the instances
                            .last()
                            .subscribe(val => { setTimeout(() => updateInstance(val), 20); }, err => { }, () => { });
                    }, err => console.log(err), () => { });
            }, err => console.log(err), () => { });
    }
}
