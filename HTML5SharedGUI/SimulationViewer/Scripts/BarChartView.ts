// (PF) This file contains the implementation of the bar chart view (for simulations).
// A lot of this uses exactly the same designs as TimeseriesView.ts. Look at that file for explanations.
// Design note: arguably, there is code duplication here. On the other hand, though, it's nice to keep things independant.

import * as ko from "knockout";
import "idd";
declare var InteractiveDataDisplay: any;
import * as $ from "jquery";
import * as fileSaver from "file-saver";
import GetExporter from "../../GenericComponents/Scripts/SVGExporter";
import * as Framework from "./SimulationViewerFramework";
import "../../GenericComponents/Scripts/Dropdown";
import * as I from "../../GenericComponents/Scripts/Interfaces";
import * as MultiFilter from "../../GenericComponents/Scripts/MultiFilter";
import * as ProgressView from "./ProgressView";
import TraceColourEditor from "../../GenericComponents/Scripts/TraceColourEditor";

import * as template from 'raw-loader!../HTML/BarChartView.html';
ko.components.register("species-barchart-view", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            // Configure the tabs and plot.
            var $_elem = $(componentInfo.element);

            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $_elem));
            let barPlots = InteractiveDataDisplay.asPlot($("div.j-bar-plots", $_elem));
            var legend = $(".idd-legend-cover", $_elem);
            legend.draggable({ containment: $_elem.children("div[data-idd-plot='figure']") });
            new InteractiveDataDisplay.Legend(barPlots, legend, true);

            // Finally, pass the parent VM as this component's VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

// (FP) This class is the VM for a single species. Note that the count is actually a single number here, as the bar chart is built to 
// display the situation at a single point in time.
class SpeciesVM {
    public IsVisible: KnockoutObservable<boolean>;
    public Counts: KnockoutObservable<number>;
    public LowerCounts: KnockoutObservable<number>;
    public UpperCounts: KnockoutObservable<number>;
    public BoundsAvailable: KnockoutComputed<boolean>;
    public DisplayName: KnockoutComputed<string>;

    public GetSpeciesPostfix() {
        let tokens = [this.SystemName, this.SettingsName, this.InstanceName].filter(t => t != "");
        if (tokens.length == 0)
            return "";
        return "(" + tokens.join('/') + ")";
    }

    /** A textual identifier of Species Name including groups */
    public GetDisplayName() {
        return this.SpeciesName + this.GetSpeciesPostfix();
    }

    /** A textual identifier of species structutal string including groups */
    public GetDisplayStructuralString() {
        return this.StructuralString + this.GetSpeciesPostfix();
    }

    constructor(owner: BarChartVM, public SpeciesName: string, public StructuralString: string, public InstanceID: number, public InstanceName: string, public SweepName: string, public SettingsName: string, public SystemName: string) {
        this.IsVisible = ko.observable(true);
        this.Counts = ko.observable(0.0);
        this.LowerCounts = ko.observable(undefined);
        this.UpperCounts = ko.observable(undefined);
        this.DisplayName = ko.pureComputed(() => owner.AreNamesEnabled() ? SpeciesName : StructuralString);
        this.BoundsAvailable = ko.computed(() => {
            this.Counts();
            return (this.UpperCounts() != undefined && this.LowerCounts() != undefined);
        });
    }
}

/** Represents a series of SpeciesVM that should be displayed with same color. */
class SingleColorBars {
    /** The heights of the bars on the figure */
    public BarHeights: KnockoutComputed<Array<number>>;
    public BarLowerHeights: KnockoutComputed<Array<number>>;
    public BarUpperHeights: KnockoutComputed<Array<number>>;

    public BarLocations: KnockoutObservable<Array<number>>;

    public IsCIEnabled: KnockoutObservable<boolean>;

    private obsCounts: KnockoutObservable<Array<KnockoutObservable<number>>>;
    private obsLowerCounts: KnockoutObservable<Array<KnockoutObservable<number>>>;
    private obsUpperCounts: KnockoutObservable<Array<KnockoutObservable<number>>>;

    public Colour: TraceColourEditor;

    constructor(public PlotName: string, color: string, counts: Array<KnockoutObservable<number>>, lower: Array<KnockoutObservable<number>>, upper: Array<KnockoutObservable<number>>, locations: Array<number>, public Order: number) {
        this.obsCounts = ko.observable(counts);
        this.obsLowerCounts = ko.observable(lower);
        this.obsUpperCounts = ko.observable(upper);

        this.IsCIEnabled = ko.observable(false);

        this.BarHeights = ko.computed(() => {
            return this.obsCounts().map(q => q());
        });
        this.BarLowerHeights = ko.computed(() => {
            var counts = this.obsLowerCounts();
            return counts != undefined ? counts.map(q => q()) : [];
        });
        this.BarUpperHeights = ko.computed(() => {
            var counts = this.obsUpperCounts();
            return counts != undefined ? counts.map(q => q()) : [];
        });
        this.BarLocations = ko.observable(locations);

        this.Colour = new TraceColourEditor(ko.observable(PlotName), color);
    }

    public Update(counts: Array<KnockoutObservable<number>>, lower: Array<KnockoutObservable<number>>, upper: Array<KnockoutObservable<number>>, locations: Array<number>) {
        this.BarLocations(locations);
        this.obsCounts(counts);
        this.obsLowerCounts(lower);
        this.obsUpperCounts(upper);
        this.IsCIEnabled = ko.observable(lower != undefined && upper != undefined);
    }
}

type SpeciesGrouping = {
    // This is an array of SpeciesVM's Counts in order as they are in VisibleSpeciesVMs.
    // Element is observable since count for each element can change individually without changing number of species.
    Counts: Array<KnockoutObservable<number>>;
    LowerCounts: Array<KnockoutObservable<number>>;
    UpperCounts: Array<KnockoutObservable<number>>;

    // Contains position on the x-axis for each of the SpeciesVM items,
    // so that the items are grouped on the x-axis by the PositionalKeys (see below).
    // Index of this array is same as for Counts.
    // Positions are re-computed atomically as new SpeciesVM comes.
    Positions: Array<number>;

    // Contains ticks on x-axis that divide the axis into positional groups.
    Ticks: Array<number>;

    // Contains keys for positional grouping (e.g. species names).
    PositionalKeys: Array<string>;

    // Maps positional key to an array of indexes of the array `Counts`, i.e. 
    // contains collections of SpeciesVM counts grouped by the positional key.
    PositionalGroup: { [posKey: string]: Array<number>; } // number is an index in Input

    // Contains keys for color grouping (e.g. instance names).
    ColorKeys: Array<string>;

    // Maps color key to an array of indexes of the array `Counts`, i.e. 
    // contains collections of SpeciesVM counts grouped by the color key.
    ColorGroup: { [colorKey: string]: Array<number>; } // number is an index in Input
};

type IddAxisLabels = {
    type: string;
    ticks: Array<number>;
    labels: Array<string>;
    attachGrid: boolean;
    rotate?: boolean;
    fontSize?: number;
    rotateAngle?: number;
};

// This is the VM for the chart as a whole. The overall design is much like the VM for TimeseriesView.
class BarChartVM {
    public CaptureSVG: () => void;
    public SpeciesVMs: KnockoutObservableArray<SpeciesVM>; //species of all the instances          

    public CurrentSystems: KnockoutObservableArray<string>;
    public CurrentSettings: KnockoutObservableArray<string>;
    public CurrentSweeps: KnockoutObservableArray<string>;
    public CurrentInstances: KnockoutObservableArray<string>;
    public CurrentVisibleSpecies: KnockoutObservableArray<string>;

    public FilteredSpeciesVMs: KnockoutComputed<Array<SpeciesVM>>; //filtered by constraints applied from outside (CurrentInstances & CurrentVisibleSpecies).
    public VisibleSpeciesVMs: KnockoutComputed<Array<SpeciesVM>>; //even further filtering. by checkboxes in bar char view

    public GroupBy: KnockoutObservable<string>;
    public ShowGroupBy: KnockoutObservable<boolean>;

    private SpeciesGroups: KnockoutComputed<SpeciesGrouping>;
    public BarPlots: KnockoutComputed<Array<SingleColorBars>>;
    public XLabels: KnockoutComputed<IddAxisLabels>;

    public IsBoundsEnabled: KnockoutComputed<boolean>;
    public IsLegendVisible = ko.observable(true);
    /**
    * x coordinates of the bars on the figure
    */
    //public BarLocations: KnockoutComputed<Array<number>>;
    public Units: KnockoutObservable<string>;
    public YLabel: KnockoutObservable<string>;
    public labelSize: KnockoutObservable<string>;
    public YTicks: KnockoutObservable<IddAxisLabels>;
    //readonly UI hooks
    public AllSpeciesHaveStructural: KnockoutComputed<boolean>;

    //writable UI hooks
    public SelectedAllVisible: KnockoutComputed<boolean>;

    public AreNamesEnabled: KnockoutObservable<boolean>;

    // Color cache so there would be same colors for same names.
    private colors: { [name: string]: string; } = {};

    private GetColor(name: string) {
        if (this.colors[name] !== undefined) return this.colors[name];

        var n = Object.keys(this.colors).length;
        var color = Framework.Palette[n % Framework.Palette.length];
        this.colors[name] = color;
        return color;
    }

    public Filter: MultiFilter.View;
    public Progress: ProgressView.ProgressView;

    constructor() {
        this.Filter = new MultiFilter.View();
        this.Progress = new ProgressView.ProgressView();
        this.SpeciesVMs = ko.observableArray([]);

        this.CurrentSystems = ko.observableArray([]);
        this.CurrentSettings = ko.observableArray([]);
        this.CurrentSweeps = ko.observableArray([]);
        this.CurrentInstances = ko.observableArray([]);
        this.CurrentVisibleSpecies = ko.observableArray([]);

        this.Units = ko.observable("");
        this.YLabel = ko.observable("");
        this.labelSize = ko.observable("16px");
        this.YTicks = ko.observable({
            type: "labels",
            ticks: [],
            labels: [],
            attachGrid: true,
            rotate: true,
            fontSize: 16
        });
        this.AreNamesEnabled = ko.observable(true);

        this.GroupBy = ko.observable("instances");

        this.FilteredSpeciesVMs = ko.pureComputed<Array<SpeciesVM>>(() => {
            var allowedSystems = this.CurrentSystems();
            var allowedSettings = this.CurrentSettings();
            var allowedSweeps = this.CurrentSweeps();
            var allowedInstances = this.CurrentInstances();
            var allowedSpeciesNames = this.CurrentVisibleSpecies();
            var filtered = this.SpeciesVMs();
            if (allowedSystems)
                filtered = filtered.filter(vm => allowedSystems.some(name => name == vm.SystemName));
            if (allowedSettings)
                filtered = filtered.filter(vm => allowedSettings.some(name => name == vm.SettingsName));
            if (allowedSweeps)
                filtered = filtered.filter(vm => allowedSweeps.some(name => name == vm.SweepName));
            if (allowedInstances)
                filtered = filtered.filter(vm => allowedInstances.some(name => name == vm.InstanceName));
            if (allowedSpeciesNames)
                filtered = filtered.filter(vm => allowedSpeciesNames.some(sn => sn === vm.SpeciesName));
            return filtered;
        });

        this.AllSpeciesHaveStructural = ko.computed(() => {
            return this.FilteredSpeciesVMs().length > 0 && this.FilteredSpeciesVMs().every(vm => vm.StructuralString != null && vm.StructuralString != "");
        });

        this.VisibleSpeciesVMs = ko.pureComputed<Array<SpeciesVM>>(() => {
            var result = this.FilteredSpeciesVMs().filter(vm => vm.IsVisible());
            return result;
        });

        this.ShowGroupBy = ko.pureComputed<boolean>(() => {
            var instances = this.CurrentInstances();
            return instances.length > 0;
        });

        var sortByKeys = function (arr: string[], keys: string[]) {
            var n = arr.length;
            var b = new Array<{ v: string, k: string }>(n);
            for (var i = 0; i < n; i++) {
                b[i] = { v: arr[i], k: keys[i] };
            }
            b.sort(function (a, b) {
                if (typeof a.k === "undefined" || a.k == null) {
                    if (typeof b.k === "undefined" || b.k == null) {
                        return 0;
                    }
                    return -1;
                }
                return a.k.localeCompare(b.k);
            });
            for (var i = 0; i < n; i++) {
                arr[i] = b[i].v;
            }
        };

        this.SpeciesGroups = ko.pureComputed<SpeciesGrouping>(() => {
            var groupBy = this.CurrentInstances().length == 0 ? "species" : this.GroupBy();
            var input = this.VisibleSpeciesVMs();
            var useNames = this.AreNamesEnabled();
            var n = input.length;

            var groupSpecies: { [speciesName: string]: Array<number>; } = {};
            var species = new Array<string>();
            var speciesNames = new Array<string>();

            var groupInstances: { [instanceName: string]: Array<number>; } = {};
            var instances = new Array<string>();

            for (var i = 0; i < n; i++) {
                var item = input[i];

                var sn = useNames ? item.SpeciesName : item.StructuralString;

                speciesNames.push(item.SpeciesName);

                if (groupSpecies[sn] == undefined) {
                    groupSpecies[sn] = [i];
                    species.push(sn);
                } else {
                    groupSpecies[sn].push(i);
                }

                if (groupInstances[item.GetSpeciesPostfix()] == undefined) {
                    groupInstances[item.GetSpeciesPostfix()] = [i];
                    instances.push(item.GetSpeciesPostfix());
                } else {
                    groupInstances[item.GetSpeciesPostfix()].push(i);
                }
            }

            var posKeys: string[], posGroup: { [key: string]: Array<number>; };
            var colKeys: string[], colGroup: { [key: string]: Array<number>; };
            if (groupBy == "species") {
                posKeys = species;
                colKeys = instances;
                posGroup = groupSpecies;
                colGroup = groupInstances;

                colKeys.sort();
                sortByKeys(posKeys, speciesNames);
            }
            else {
                posKeys = instances;
                colKeys = species;
                posGroup = groupInstances;
                colGroup = groupSpecies;

                sortByKeys(colKeys, speciesNames);
                posKeys.sort();
            }

            var positions = new Array<number>(n);
            var ticks = new Array<number>(posKeys.length + 1);
            ticks[0] = 0;
            for (var i = 0, k = 0, pos = ticks[0] + 1; i < posKeys.length; i++) {
                var sn = posKeys[i];
                var groupItems = posGroup[sn];
                for (var j = 0; j < groupItems.length; j++) {
                    positions[groupItems[j]] = pos++;
                }
                ticks[i + 1] = pos++;
            }

            var hasBounds = this.IsBoundsEnabled();

            var g: SpeciesGrouping = {
                Counts: input.map(sp => sp.Counts),
                LowerCounts: hasBounds ? input.map(sp => sp.LowerCounts) : undefined,
                UpperCounts: hasBounds ? input.map(sp => sp.UpperCounts) : undefined,
                Positions: positions,
                Ticks: ticks,
                PositionalKeys: posKeys,
                PositionalGroup: posGroup,
                ColorKeys: colKeys,
                ColorGroup: colGroup,
            };
            return g;
        });

        this.XLabels = ko.pureComputed<IddAxisLabels>(() => {
            var groups = this.SpeciesGroups();
            return {
                type: "labels",
                ticks: groups.Ticks,
                labels: groups.PositionalKeys,
                fontSize: this.YTicks().fontSize,
                attachGrid: false,
                rotateAngle: 15
            };
        });

        // The cache allows to keep same insances of SingleColorBars to prevent removing corresponding plots when new SpeciesVM appear.
        // This prevents flickering when new group appears.
        var cacheByColor: { [colorKey: string]: SingleColorBars; } = {};

        this.BarPlots = ko.pureComputed<Array<SingleColorBars>>(() => {
            var groups = this.SpeciesGroups();

            var barPlots = new Array<SingleColorBars>(groups.ColorKeys.length);
            for (var i = 0; i < barPlots.length; i++) {
                var colorKey = groups.ColorKeys[i];
                var colorGroup = groups.ColorGroup[colorKey];
                var counts = colorGroup.map(idx => groups.Counts[idx]);
                var hasCI = groups.LowerCounts !== undefined;
                var lower = hasCI ? colorGroup.map(idx => groups.LowerCounts[idx]) : undefined;
                var upper = hasCI ? colorGroup.map(idx => groups.UpperCounts[idx]) : undefined;
                var locations = colorGroup.map(idx => groups.Positions[idx]);

                if (typeof cacheByColor[colorKey] == 'undefined') {
                    var color = this.GetColor(colorKey);
                    cacheByColor[colorKey] = barPlots[i] = new SingleColorBars(colorKey, color, counts, lower, upper, locations, barPlots.length - i);
                } else {
                    // update
                    barPlots[i] = cacheByColor[colorKey];
                    barPlots[i].Update(counts, lower, upper, locations);
                }
            }

            // Removing obsolete cached instances
            var cachedKeys = Object.keys(cacheByColor);
            for (var j = 0; j < cachedKeys.length; j++) {
                var colorKey = cachedKeys[j];
                if (typeof groups.ColorGroup[colorKey] == 'undefined') {
                    delete cacheByColor[colorKey];
                }
            }

            return barPlots;
        });

        this.SelectedAllVisible = ko.pureComputed<boolean>(
            {
                read: () => {
                    return this.FilteredSpeciesVMs().every((sp) => { return sp.IsVisible(); });
                },
                write: (value) => {
                    this.FilteredSpeciesVMs().forEach((sp) => {
                        sp.IsVisible(value);
                    });
                }
            });

        this.IsBoundsEnabled = ko.computed(() => {
            return this.VisibleSpeciesVMs().length > 0 && this.VisibleSpeciesVMs().every(vm => vm.BoundsAvailable() == true);
        });
    }
}

// The class that represents the component. Same design as in TimeseriesView.ts.
export class View implements Rx.IObserver<Framework.IVisualizationUpdateMessage> {
    private vm: BarChartVM;

    private bufferIsUpdating: boolean;
    private buffer: any;

    constructor() {
        this.vm = new BarChartVM();
        this.instances = {};
    }

    bind(elementToBindTo: HTMLElement) {
        if (elementToBindTo == null)
            throw "Attempt to bind BarChartView to null";
        var capturedVm = this.vm;
        ko.applyBindings(capturedVm, elementToBindTo);

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
        capturedVm.CaptureSVG = GetExporter("bar-chart", elementToBindTo);
    }

    private instances: { [id: number]: { Name: string, Sweep: string, Settings: string, System: string, Plots: { Name: string, Structural?: string }[], Index: number } };

    //Framework.IView<IUpdateMessage> implementation
    public onNext(update: Framework.IVisualizationUpdateMessage) {
        var vm = this.vm;
        vm.Progress.onNext(update);
        switch (update.MessageType) {
            case Framework.VisualizationUpdateMessageType.TraceDefinitions:
                var traces = <I.ITraceDefinitions>update.EncapsulatedUpdate;
                vm.Filter.SetAvailableTraces(traces);
                this.instances = {};
                var vms: SpeciesVM[] = [];
                for (let crn of traces.CRNs)
                    for (let settings of crn.Settings)
                        for (let sweep of settings.Sweeps)
                            for (let instance of sweep.Instances) {
                                var index = vms.length;
                                for (let plot of instance.Plottables) {
                                    var spVM = new SpeciesVM(vm, plot.Name, plot.Structural, instance.ID, instance.Name, sweep.Name, settings.Name, crn.Name);
                                    vms.push(spVM);
                                }
                                this.instances[instance.ID] = { Name: instance.Name, Sweep: sweep.Name, Settings: settings.Name, System: crn.Name, Plots: instance.Plottables.slice(), Index: index };
                            }
                vm.SpeciesVMs(vms);
                break;
            case Framework.VisualizationUpdateMessageType.AdditionalPlottable:
                var newPlottable = <I.IAdditionalPlottable>update.EncapsulatedUpdate;
                vm.Filter.AddSpeciesToInstance(newPlottable);
                var instance = this.instances[newPlottable.Instance];
                var vms: SpeciesVM[] = this.vm.SpeciesVMs();
                // Create a new VM for the species.
                var newVM = new SpeciesVM(vm, newPlottable.Name, newPlottable.Structural, newPlottable.Instance, instance.Name, instance.Sweep, instance.Settings, instance.System);
                // Insert the VM in the correct position.
                vms.splice(instance.Index + instance.Plots.length, 0, newVM);
                // Insert the species in the instance.
                instance.Plots.push(newPlottable);
                // Increase the global index for all instances that have a larger index, because they get pushed forward.
                for (let i in this.instances)
                    if (this.instances[i].Index > instance.Index)
                        this.instances[i].Index++;
                this.vm.SpeciesVMs(vms);
                break;
            case Framework.VisualizationUpdateMessageType.SimulationStep:
                var step = <Framework.ISimStepData>update.EncapsulatedUpdate;
                var instanceID = step.Instance;
                var instance = this.instances[instanceID];
                var instanceName = instance.Name;
                var settingsName = instance.Settings;
                var spCount = instance.Plots.length;
                var counts = step.Counts;
                var bounds = step.Bounds;

                // Within the SpeciesVM array, instances are packed back-to-back. Species belonging to the same instance are contiguous, and in the same order as in sim updates. Here, I'm searching for the index of the first plot of this instance within the array. I expect the appropriate number of species VMs starting at this index.
                var idx = instance.Index;
                var VMs = vm.SpeciesVMs();

                for (var i = 0; i < spCount; i++) {
                    var spVM = VMs[idx + i];
                    if (bounds != null && bounds.length > 0) {
                        spVM.LowerCounts(bounds[i].lower);
                        spVM.UpperCounts(bounds[i].upper);
                    }
                    spVM.Counts(counts[i]);
                }
                break;
            case Framework.VisualizationUpdateMessageType.UnitsInformation:
                vm.Units(<Framework.Units>update.EncapsulatedUpdate);
                break;
            case Framework.VisualizationUpdateMessageType.PlotSettingsInfo:
                var settings = <Framework.PlotSettings>update.EncapsulatedUpdate;
                vm.YLabel(settings.YLabel);
                vm.labelSize(settings.LabelFontSize + "px");
                vm.YTicks({
                    type: settings.YTicks.length == 0 ? "numeric" : "labels",
                    ticks: settings.YTicks,
                    labels: settings.YTicks.map(value => value.toString()),
                    attachGrid: true,
                    rotate: true,
                    fontSize: settings.TickFontSize
                });
                break;
            case Framework.VisualizationUpdateMessageType.Reset:
                this.Reset();
                break;
            default:
                break;
        }
    }

    public onError(exception: any) {
        console.log("SpeciesCountsView Exception: " + exception);
        throw exception;
    }

    public onCompleted() {
    }

    public ShowSystems(names: Array<string>) {
        this.vm.CurrentSystems(names);
    }

    public ShowSettings(names: Array<string>) {
        this.vm.CurrentSettings(names);
    }

    public ShowSweeps(names: Array<string>) {
        this.vm.CurrentSweeps(names);
    }

    public ShowInstances(names: Array<string>) {
        this.vm.CurrentInstances(names);
    }

    public ShowSpecies(names: Array<string>) {
        this.vm.CurrentVisibleSpecies(names);
    }

    Reset() {
        this.vm.Filter.SetAvailableTraces({ ModelName: "", CRNs: [] });
        this.vm.SpeciesVMs.removeAll();
        this.instances = {};
    }
}