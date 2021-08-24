// (FP) This file contains the code for the time series plotter. Note that in the context of this file, "CI" typically refers to species concentration intervals.

import * as $ from "jquery";
import * as ko from "knockout";
import "jquery-mousewheel";
import "idd";
declare var InteractiveDataDisplay: any;
import * as fileSaver from "file-saver";
import GetExporter from "../../GenericComponents/Scripts/SVGExporter";
import * as Framework from "./SimulationViewerFramework";
import "../../GenericComponents/Scripts/Dropdown";
import * as MultiInstanceFilter from "../../GenericComponents/Scripts/MultiFilter";
import * as ProgressView from "./ProgressView";
import TraceColourEditor from "../../GenericComponents/Scripts/TraceColourEditor";
import * as I from "../../GenericComponents/Scripts/Interfaces";
export type IddAxisLabels = {
    type: string;
    attachGrid: boolean;
    ticks: Array<number>;
    labels: Array<string>;
    rotate?: boolean;
    fontSize?: number;
};

// Register the KO component.
import * as template from 'raw-loader!../html/TimeseriesView.html';
ko.components.register("species-timeseries-view", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            // Setup IDD.
            var $_elem = $(componentInfo.element);
            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $_elem));
            var gestureSource = InteractiveDataDisplay.Gestures.getGesturesStream(plot.centralPart);
            var bottomAxisGestures = InteractiveDataDisplay.Gestures.applyHorizontalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-bottom-axis", $_elem)));
            var leftAxisGestures = InteractiveDataDisplay.Gestures.applyVerticalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-left-axis", $_elem)));
            plot.navigation.gestureSource = gestureSource.merge(bottomAxisGestures.merge(leftAxisGestures));

            var legend = $(".idd-legend-cover", $_elem);
            legend.draggable({ containment: $_elem.children("div[data-idd-plot='figure']") });
            new InteractiveDataDisplay.Legend(plot, legend, true);

            // Finally, pass the parent VM as this component's VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

// (FP) This is the VM for a single species. This is the object to which the IDD plot for a species gets bound. As such, it will contain several KO observables, each of which will be bound to some attribute of the plot.
// Design note: note that the Y-axis values (Counts, UpperCounts, LowerCounts) are non-observable arrays. They do get changed, but KO does not get notified when they do. The reason for this is that if every array was observable, the plot would be updated every time one of them changes. So, it would be updated when the Concentration change, and when the Times change (and, if intervals are available, when the Upper changes and when the Lower changes). This is a problem for two reasons. First, when we are going to update all of those in sequence, the  length of the arrays will be different until all of the arrays have been updated. Second, doing such multiple updates is a waste of time. The solution here is to have only one actual observable (Times). The update of the plots will only trigger when Times is updated. As long as we remember to update Times last, this guarantees that when we add a point to the content, there will be only one update, and only when all of the arrays have the correct length. There are other ways to solve this problem, but this is probably the most efficient, and  performance of this part of the logic is actually quite important.
class PlottableVM {
    public Counts: Array<number>;
    public UpperCounts: Array<number>;
    public LowerCounts: Array<number>;
    public Times: KnockoutObservableArray<number>;
    public IsVisible: KnockoutObservable<boolean>;
    public DisplayName: KnockoutComputed<string>;
    public DisplayStructural: KnockoutComputed<string>;
    public VisibleName: KnockoutComputed<string>;
    public BoundsAvailable: KnockoutComputed<boolean>;
    public HasObservations = ko.pureComputed(() => this.ObservationCounts != null && this.ObservationCounts != [] && this.ObservationTimes != null && this.ObservationTimes != []);

    public Colour: TraceColourEditor;

    // (FP) Constructs a new plot VM. Note that you should provide initial values. They will be copied to the internal buffers.
    constructor(owner: TimeSeriesVM, public Name: string, public StructuralString: string, public InstanceID: number, public InstanceName: string, public Sweep: string, public Settings: string, public System: string, public originalColour: string, public ObservationName: string, public ObservationCounts: number[], public ObservationTimes: number[]) {
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
        this.Counts = [];
        this.LowerCounts = [];
        this.UpperCounts = [];
        this.Times = ko.observableArray([]);

        // (FP) This computed is true if the current data has intervals.
        this.BoundsAvailable = ko.computed(() => {
            // (FP) We are calling Times() and throwing away the value. This lets KO know that the value of this computed depends on Times. But it does not actually depend on Times; the reason we are doing this is that Times is used as the trigger for all updates.
            this.Times(); //this.Times.peek() does not work here (does not make BoundsAvailable refresh upon Times refresh). Thus just getting Times() value
            // (FP) Intervals are considered to be available if UpperCounts has values (could just as well be LowerCounts).
            return (this.UpperCounts.length > 0);
        });
        this.Colour = new TraceColourEditor(this.VisibleName, originalColour == null ? "black" : originalColour);
    }
}

// (FP) This is the viewmodel for the entire set of series. Note that it extends SvgCapturingVM, because it can be used to generate SVG.
class TimeSeriesVM {

    //are filled up upon receiving simulation updates (as new sweeps can come)
    public AllPlottables: KnockoutObservableArray<PlottableVM>;

    // Here, by "allowed", we mean "selected by the user for displaying".
    public AllowedSpeciesNames: KnockoutObservableArray<string>;
    public AllowedInstanceNames: KnockoutObservableArray<string>;
    public AllowedSweepNames: KnockoutObservableArray<string>;
    public AllowedSettingsNames: KnockoutObservableArray<string>;
    public AllowedSystemNames: KnockoutObservableArray<string>;

    //AllSpecies with external filters applied. (FP) This will be a computed; it's AllSpecies restricted to the ones that are in AllowedSpeciesNames and AllowedSweeps.
    public FilteredPlottables: KnockoutComputed<Array<PlottableVM>>;

    //filled up upon units message receiving
    public Units: KnockoutObservable<Framework.Units>;

    //Display control read-only hooks
    public AllSpeciesAreBoundsAvailable: KnockoutComputed<boolean>;
    public AllSpeciesHaveStructural: KnockoutComputed<boolean>;

    //Display control assignable hooks
    public SelectedAllVisible: KnockoutComputed<boolean>;
    public PlotCountCap = ko.observable(30);
    public PlotCountCapExceeded = ko.observable(false);
    public AreBoundsEnabled: KnockoutObservable<boolean>;
    public AreNamesEnabled: KnockoutObservable<boolean>;

    public XLabel: KnockoutObservable<string>;
    public YLabel: KnockoutObservable<string>;
    public XTicks: KnockoutObservable<IddAxisLabels>;
    public YTicks: KnockoutObservable<IddAxisLabels>;
    public labelSize: KnockoutObservable<string>;
    public PlotTitle: KnockoutObservable<string>;

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

    public VBoundaries: KnockoutObservable<number[]>;
    public HBoundaries: KnockoutObservable<number[]>;
    public XMin: KnockoutComputed<number>;
    public XMax: KnockoutComputed<number>;
    public YMin: KnockoutComputed<number>;
    public YMax: KnockoutComputed<number>;

    // Users' desire to see the legend on timeseries
    public IsLegendVisible = ko.observable(true);
    public IsGridVisible = ko.observable(true);

    public Filter: MultiInstanceFilter.View;
    public Progress: ProgressView.ProgressView;

    public CaptureSVG: () => void;

    constructor() {
        this.Filter = new MultiInstanceFilter.View();
        this.Progress = new ProgressView.ProgressView();
        this.Units = ko.observable("");
        this.XLabel = ko.observable("");
        this.YLabel = ko.observable("");
        this.XTicks = ko.observable({
            type: "numeric",
            attachGrid: true,
            attachNavigation: true,
            ticks: [],
            labels: [],
            fontSize: 16
        });
        this.YTicks = ko.observable({
            type: "numeric",
            attachGrid: true,
            ticks: [],
            labels: [],
            rotate: true,
            fontSize: 16
        });
        this.VBoundaries = ko.observable([]);
        this.HBoundaries = ko.observable([]);
        this.PlotTitle = ko.observable("");
        this.labelSize = ko.observable("10px");
        this.AllPlottables = ko.observableArray([]);
        this.AllowedSpeciesNames = ko.observableArray([]); // external filter settings
        this.AllowedInstanceNames = ko.observableArray([]); // external filter settings
        this.AllowedSweepNames = ko.observableArray([]); // external filter settings
        this.AllowedSettingsNames = ko.observableArray([]); // external filter settings
        this.AllowedSystemNames = ko.observableArray([]); // external filter settings
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
        this.XMin = ko.pureComputed<number>(() => Math.min(...this.FilteredPlottables().filter(p => p.Times().length > 0).map(p => Math.min(...p.Times()))));
        this.XMax = ko.pureComputed<number>(() => Math.max(...this.FilteredPlottables().filter(p => p.Times().length > 0).map(p => Math.max(...p.Times()))));
        this.YMin = ko.pureComputed<number>(() => Math.min(...this.FilteredPlottables().filter(p => p.Times().length > 0).map(p => Math.min(...p.Counts))));
        this.YMax = ko.pureComputed<number>(() => Math.max(...this.FilteredPlottables().filter(p => p.Times().length > 0).map(p => Math.max(...p.Counts))));

        this.AreBoundsEnabled = ko.observable(true);
        this.AreNamesEnabled = ko.observable(true);
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
        this.AllSpeciesAreBoundsAvailable = ko.pureComputed<boolean>({ read: () => this.FilteredPlottables().length > 0 && this.FilteredPlottables().every(sp => sp.BoundsAvailable()) });
        this.AllSpeciesHaveStructural = ko.pureComputed<boolean>({
            read: () => this.FilteredPlottables().length > 0 && this.FilteredPlottables().every(sp => sp.StructuralString != null && sp.StructuralString != "")
        });
    }
}

// (FP) This class represents the time series plotter component. Note that it derives from SvgExportableView (because the component can be exported to  SVG) and it implements IViewObserver (this makes it a part of the larger simulation viewer component), specifying the type of the update messages it expects.
export class View implements Rx.IObserver<Framework.IVisualizationUpdateMessage> {
    private vm: TimeSeriesVM;
    // (FP) There is a mechanism where incoming time series updates are put in a buffer, which is then pushed into the VM (and from there to the screen) all at once. I assume this is for performance reason (i.e. to avoid updating IDD at every single update). The delay is minimal, i.e. the buffer gets pushed every time the main thread is idle. The following flag ensures that no update requests are queued while there is one pending already.
    private bufferIsUpdating: boolean;
    // The buffer maps the index of one specific species within the global species array, to the values that need to be added for that species.
    private buffer: { [index: number]: { time: number[], counts: number[], lower?: number[], upper?: number[] } };

    constructor() {
        this.vm = new TimeSeriesVM();
        this.bufferIsUpdating = false;
        this.buffer = {};
        this.instances = {};
    }

    private container: HTMLElement;

    // (FP) Builds this component in the given div. It will also load any necessary CSS.
    bind(div: HTMLElement) {
        if (div == null)
            throw "Attempt to bind TimeseriesView to null";
        this.container = div;
        var capturedVm = this.vm;
        ko.applyBindings(capturedVm, div);

        capturedVm.Filter.GetSelectedSpecies().subscribe(names => {
            this.ShowSpecies(names);
        });
        capturedVm.Filter.GetSelectedInstances().subscribe(instances => {
            this.ShowInstances(instances);
        });
        capturedVm.Filter.GetSelectedSweeps().subscribe(sweeps => {
            this.ShowSweeps(sweeps);
        })
        capturedVm.Filter.GetSelectedSettings().subscribe(settings => {
            this.ShowSettings(settings);
        });
        capturedVm.Filter.GetSelectedSystems().subscribe(systems => {
            this.ShowSystems(systems);
        });

        capturedVm.CaptureSVG = GetExporter("simulation", div);
    }

    // (FP) Empties the updates buffer into the VM, causing KO bindings to update. Note that Times is pushed last. This is important; Times is the actual observable, so after pushing it all of the data need to be ready.
    pushBuffer() {
        let len = this.vm.AllPlottables().length;
        for (let i = 0; i < len; i++) {
            var buffer = this.buffer[i];
            if (buffer) {
                let species = this.vm.AllPlottables()[i];
                ko.utils.arrayPushAll(species.Counts, buffer.counts);
                if (buffer.upper) {
                    ko.utils.arrayPushAll(species.UpperCounts, buffer.upper);
                    ko.utils.arrayPushAll(species.LowerCounts, buffer.lower);
                }
                ko.utils.arrayPushAll<number>(species.Times, buffer.time);
            }
        }
        this.buffer = {};
        this.bufferIsUpdating = false;
    }

    private instances: { [id: number]: { Name: string, Sweep: string, Settings: string, System: string, Plots: { Name: string, Structural?: string }[], Index: number } };

    //Framework.IView<IUpdateMessage> implementation
    // (FP) This is the function the higher level will call in order to provide data.
    public onNext(update: Framework.IVisualizationUpdateMessage) {
        var vm = this.vm;
        vm.Progress.onNext(update);
        switch (update.MessageType) {
            case Framework.VisualizationUpdateMessageType.TraceDefinitions:
                var traces = <I.ITraceDefinitions>update.EncapsulatedUpdate;
                vm.Filter.SetAvailableTraces(traces);
                this.instances = {};
                var vms: PlottableVM[] = [];
                for (let crn of traces.CRNs)
                    for (let settings of crn.Settings)
                        for (let sweep of settings.Sweeps)
                            for (let instance of sweep.Instances) {
                                var idx = vms.length;
                                for (let plot of instance.Plottables) {
                                    var color = (plot.Colour == null || plot.Colour == "") ? Framework.Palette[vms.length % Framework.Palette.length] : plot.Colour;
                                    var spVM = new PlottableVM(this.vm, plot.Name, plot.Structural, instance.ID, instance.Name, sweep.Name, settings.Name, crn.Name, color, plot.ObservationName, plot.ObservationCounts, plot.ObservationTimes);
                                    vms.push(spVM);
                                }
                                this.instances[instance.ID] = { Name: instance.Name, Sweep: sweep.Name, Settings: settings.Name, System: crn.Name, Plots: instance.Plottables.slice(), Index: idx };
                            }
                vm.AllPlottables(vms);
                break;
            case Framework.VisualizationUpdateMessageType.AdditionalPlottable:
                var newPlottable = <I.IAdditionalPlottable>update.EncapsulatedUpdate;
                vm.Filter.AddSpeciesToInstance(newPlottable);
                var instance = this.instances[newPlottable.Instance];
                var vms: PlottableVM[] = this.vm.AllPlottables();
                // Create a new VM for the species.
                var newVM = new PlottableVM(this.vm, newPlottable.Name, newPlottable.Structural, newPlottable.Instance, instance.Name, instance.Sweep, instance.Settings, instance.System, Framework.Palette[vms.length % Framework.Palette.length], null, null, null);
                // Insert the VM in the correct position.
                vms.splice(instance.Index + instance.Plots.length, 0, newVM);
                // Insert the species in the instance.
                instance.Plots.push(newPlottable);
                // Increase the global index for all instances that have a larger index, because they get pushed forward.
                for (let i in this.instances)
                    if (this.instances[i].Index > instance.Index)
                        this.instances[i].Index++;
                this.vm.AllPlottables(vms);
                break;
            case Framework.VisualizationUpdateMessageType.SimulationTable:
                // This update replaces the entire current table.
                var table = <Framework.ISimTable>update.EncapsulatedUpdate;
                var instanceID = table.Instance;
                var instance = this.instances[instanceID];
                var spCount = instance.Plots.length;
                var idx = instance.Index;
                var isBoundsAvailable = (typeof table.Bounds !== 'undefined');

                var vms = this.vm.AllPlottables().filter(vm => vm.InstanceID == instanceID);
                for (var i = 0; i < vms.length; i++) {
                    vms[i].Counts = table.Counts[i];
                    if (isBoundsAvailable) {
                        vms[i].LowerCounts = table.Bounds[i].map(b => b.lower);
                        vms[i].UpperCounts = table.Bounds[i].map(b => b.upper);
                    }
                    // Note that it's important to set Times last, because this is what triggers updates.
                    vms[i].Times(table.Time);
                }

                break;
            case Framework.VisualizationUpdateMessageType.SimulationStep:
                // (FP) Some simulation data has been received. The following code puts it into the buffer, and then uses setTimeout to tell JS that the buffer should be copied as soon as the main thread is idle again (unless another such operation has already been scheduled).
                var step = <Framework.ISimStepData>update.EncapsulatedUpdate;
                var instanceID = step.Instance;
                var instance = this.instances[instanceID];
                var spCount = instance.Plots.length;

                // Within the SpeciesVM array, instances are packed back-to-back. Species belonging to the same instance are contiguous, and in the same order as in sim updates.
                var idx = instance.Index;

                var isBoundsAvailable = (typeof step.Bounds !== 'undefined');
                for (let i = 0; i < spCount; i++) {
                    var globalIndex = idx + i;
                    var buffer = this.buffer[globalIndex] = this.buffer[globalIndex] || { time: [], counts: [], lower: isBoundsAvailable ? [] : undefined, upper: isBoundsAvailable ? [] : undefined };
                    buffer.counts = buffer.counts || [];
                    buffer.counts.push(step.Counts[i]);

                    if (isBoundsAvailable) {
                        buffer.lower = buffer.lower || [];
                        buffer.upper = buffer.upper || [];
                        buffer.lower.push(step.Bounds[i].lower);
                        buffer.upper.push(step.Bounds[i].upper);
                    }
                    buffer.time = buffer.time || [];
                    buffer.time.push(step.Time);
                }
                if (!this.bufferIsUpdating) {
                    this.bufferIsUpdating = true;
                    setTimeout(() => { this.pushBuffer(); });
                }
                break;
            case Framework.VisualizationUpdateMessageType.UnitsInformation:
                // (FP) Unit of measure data has been received. Just pass it to the VM.
                vm.Units(<Framework.Units>update.EncapsulatedUpdate);
                break;
            case Framework.VisualizationUpdateMessageType.PlotSettingsInfo:
                var settings = <Framework.PlotSettings>update.EncapsulatedUpdate;
                vm.XLabel(settings.XLabel);
                vm.YLabel(settings.YLabel);

                var xTicksObj = {
                    type: settings.XTicks.length == 0 ? "numeric" : "labels",
                    attachGrid: true,
                    attachNavigation: true,
                    ticks: settings.XTicks,
                    labels: settings.XTicks.map(value => value.toString()),
                    fontSize: settings.TickFontSize,
                    debugTag: 'xTicksObj'
                };
                vm.XTicks(xTicksObj);

                var yTicksObj = {
                    type: settings.YTicks.length == 0 ? "numeric" : "labels",
                    attachGrid: true,
                    attachNavigation: true,
                    ticks: settings.YTicks,
                    labels: settings.YTicks.map(value => value.toString()),
                    rotate: true,
                    fontSize: settings.TickFontSize,
                    debugTag: 'yTicksObj'
                };
                vm.YTicks(yTicksObj);

                vm.VBoundaries(settings.VBoundaries);
                vm.HBoundaries(settings.HBoundaries);

                vm.PlotTitle(settings.Title);
                vm.labelSize(settings.LabelFontSize + "px");

                vm.VisibleRegion({ x_min: settings.XMin, x_max: settings.XMax, y_min: settings.YMin, y_max: settings.YMax });

                break;
            case Framework.VisualizationUpdateMessageType.Reset:
                this.Reset();
                break;
            default:
                break;
        }
    }

    public onError(exception: any) {
        console.log("SpeciesCountsView Exception: " + JSON.stringify(exception));
        throw exception;
    }

    public onCompleted() {
    }

    private ResetColors() {
        var species = this.vm.FilteredPlottables();
        for (let i = 0; i < species.length; i++) {
            var colour = species[i].originalColour;
            if (colour == null || colour == "")
                colour = Framework.Palette[i % Framework.Palette.length];
            species[i].Colour.Text(colour);
        }
    }

    public ShowSystems(names: Array<string>) {
        this.vm.AllowedSystemNames(names);
        this.ResetColors();
    }

    public ShowSettings(names: Array<string>) {
        this.vm.AllowedSettingsNames(names);
        this.ResetColors();
    }

    public ShowSweeps(names: Array<string>) {
        this.vm.AllowedSweepNames(names);
        this.ResetColors();
    }

    // (FP) Makes a set of instances selected. This is a hook used by the higher level to set the sweeps when the user selects sweeps in some other tab.
    public ShowInstances(names: Array<string>) {
        this.vm.AllowedInstanceNames(names);
        this.ResetColors();
    }

    // (FP) Makes a set of species selected. This is a hook used by the higher level to set the species when the user selects sweeps in some other tab.
    public ShowSpecies(names: Array<string>) {
        this.vm.AllowedSpeciesNames(names);
        this.ResetColors();
    }

    // (FP) Clears the simulation. Note that all of the data is held in the single species viewmodels; therefore, it does not need to be cleared explicitly (it'll get garbage collected some time after clearing AllSpecies).
    Reset() {
        this.vm.Filter.SetAvailableTraces({ ModelName: "", CRNs: [] });
        this.instances = {};
        this.vm.AllPlottables.removeAll();
        this.vm.VisibleRegion({});
        // Toggling AutoFitEnabled because the user might have changed it using the mouse. If that happens, then autofit is disabled but AutoFitEnabled is true. Because just setting it to true would have no effect (it's the same value), I need to set it to false and then to true, so that I am certain a change notification is sent to IDD.
        this.vm.AutoFitEnabled(false);
        this.vm.AutoFitEnabled(true);
    }
}