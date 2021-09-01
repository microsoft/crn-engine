// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// (FP) This file implements the table view (specifically for simulation results). Note that this does not implement the actual table: it makes use of the table component that's implemented in KnockoutGrid.js (found in HTML5SharedGUI). Also note that the internal logic has to be different here compared to TimeseriesView, because the data needs to be used by-row rather than by-column.

import "../../KnockoutGrid/knockoutgrid";
import * as ko from "knockout";
import * as $ from "jquery";
import { saveAs } from "file-saver";
import * as Papa from "papaparse";
import * as Framework from "./SimulationViewerFramework";
import * as MultiInstanceFilter from "../../GenericComponents/Scripts/MultiFilter";
import * as ProgressView from "./ProgressView";
import * as I from "../../GenericComponents/Scripts/Interfaces";
// Register the KO component.
import * as tableTemplate from 'raw-loader!../html/TableView.html';
import * as rowsTemplate from 'raw-loader!../html/TableRows.html';
// Store the rows template in the DOM head.
$('head').append(rowsTemplate);
ko.components.register("species-table-view", {
    template: tableTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

// (FP) This is a generic viewmodel provider for KO that just passes the viewmodel that's already present. This is used for when the VM is provided by the higher layer and should not be generated.
var vmp = {
    createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
        var context = ko.contextFor(componentInfo.element);
        return context === null ? {} : context.$data;
    }
};

// A cell can actually have multiple sub-values, i.e. bounds.
interface CellData {
    Values: number[];
}

interface RowData {
    Time: number;
    Data: CellData[];
}

interface Species {
    Name: string;
    Structural?: string;
}

interface LocalInstance {
    System: string;
    Settings: string;
    Sweep: string;
    ID: number;
    Name: string;
    Species: KnockoutObservableArray<Species>;
    Rows: KnockoutObservableArray<RowData>;
}

// A header can have multiple sub-columns, i.e. bounds.
interface Header {
    Instance: LocalInstance;
    SpeciesIndex: number;
    Columns: string[];
}

// Headers are grouped by instance. Each group has a position. This way, I can insert new columns at the correct position.
interface HeaderGroup {
    Instance: LocalInstance;
    Position: number;
    Headers: Header[];
}

// (FP) This class is the VM for the table viewer. It contains the data that gets displayed and any relative configuration (names, choice of sim, etc).
class TableVM {
    public CurrentSystems: KnockoutObservableArray<string> = ko.observableArray();
    public CurrentSettings: KnockoutObservableArray<string> = ko.observableArray();
    public CurrentSweeps: KnockoutObservableArray<string> = ko.observableArray();
    public CurrentInstances: KnockoutObservableArray<string> = ko.observableArray();
    public CurrentSpecies: KnockoutObservableArray<string> = ko.observableArray();

    Instances: KnockoutObservableArray<LocalInstance> = ko.observableArray();

    HeaderGroups: KnockoutComputed<HeaderGroup[]> = ko.pureComputed(() => {
        var ret = [];
        var c = 0;
        for (let instance of this.Instances())
            if (this.CurrentSystems().indexOf(instance.System) >= 0 && this.CurrentSettings().indexOf(instance.Settings) >= 0 && this.CurrentInstances().indexOf(instance.Name) >= 0) {
                let species = instance.Species();
                let hs: Header[] = [];
                for (let idx = 0; idx < species.length; idx++) {
                    if (this.CurrentSpecies().indexOf(species[idx].Name) >= 0) {
                        var display = this.AreNamesEnabled() ? species[idx].Name : species[idx].Structural;
                        var columns = [display];
                        if (this.SupportsBounds() && this.AreBoundsEnabled()) {
                            columns.push(display + " (low)");
                            columns.push(display + " (high)");
                        }
                        hs.push({
                            Instance: instance, SpeciesIndex: idx,
                            Columns: columns
                        });
                    }
                }
                ret[instance.ID] = { Instance: instance, Headers: hs, Position: c };
                c += hs.length + 1;
            }
        return ret;
    });
    Headers: KnockoutComputed<Header[]> = ko.pureComputed(() => {
        var ret: Header[] = [];
        for (let hg of this.HeaderGroups()) {
            if (hg == null)
                continue;
            ret.push({ Instance: hg.Instance, SpeciesIndex: -1, Columns: [hg.Instance.Name == "" ? "Time" : ("Time " + hg.Instance.Name)] });
            for (let h of hg.Headers)
                ret.push(h);
        }
        return ret;
    });
    public Rows: KnockoutObservableArray<CellData[]> = <KnockoutObservableArray<CellData[]>>ko.observableArray().extend({ rateLimit: 1000 });

    public VisibleHeaders: KnockoutComputed<string[]> = ko.pureComputed(() => {
        var ret: string[] = [];
        for (let h of this.Headers()) {
            if (this.SupportsBounds() && this.AreBoundsEnabled())
                Array.prototype.push.apply(ret, h.Columns);
            else
                ret.push(h.Columns[0]);
        }
        return ret;
    });
    private GetValuesWithNulls(cd: CellData, col: number): number[] {
        if (cd == null) {
            var ret: number[] = [];
            let headers = this.Headers();
            if (headers.length > col)
                ret.length = headers[col].Columns.length;
            return ret;
        }
        else
            return cd.Values;
    }
    public VisibleRows: KnockoutComputed<number[][]> = ko.pureComputed(() => {
        var ret: number[][] = [];
        for (let h of this.Rows()) {
            if (this.SupportsBounds() && this.AreBoundsEnabled())
                ret.push([].concat.apply([], h.map((cd, col) => this.GetValuesWithNulls(cd, col))));
            else
                ret.push(h.map((cd, col) => this.GetValuesWithNulls(cd, col)[0]));
        }
        return ret;
    });

    public ResetRows() {
        var rows: CellData[][] = [];
        var headers = this.Headers();
        for (var c = 0; ; c++) {
            var hasData = false;
            var row: CellData[] = headers.map(h => {
                if (h.Instance.Rows().length <= c)
                    return null;
                hasData = true;
                if (h.SpeciesIndex < 0)
                    return { Values: [h.Instance.Rows()[c].Time] };
                return h.Instance.Rows()[c].Data[h.SpeciesIndex];
            });
            if (!hasData)
                break;
            rows.push(row);
        }
        this.Rows(rows);
    }

    public AddInstanceRows(instanceID: number, time: number[], data: CellData[][]) {
        var instance = this.Instances()[instanceID];
        var rowIdx = instance.Rows().length;
        // Put the data in the instance table.
        ko.utils.arrayPushAll(instance.Rows, time.map((v, i) => { return { Time: v, Data: data[i] }; }));
        // Put the data in the global table: get the header group for this instance...
        var hg = this.HeaderGroups()[instanceID];
        if (hg == null)
            return;
        var currData = this.Rows();
        var allHeaders = this.Headers();
        for (var r = 0; r < time.length; r++) {
            // Get the row at the correct index...
            var row = currData[rowIdx + r];
            if (row == null) {
                // Make a new row.
                row = currData[rowIdx + r] = [];
                // Make sure the new row contains elements (even if they're null) at each position before the current one. Otherwise, subsequent calls to Array.map may ignore those positions altogether. That would be bad, because if bounds are enabled, those positions need to map to three positions in the visible array.
                for (var i = 0; i < hg.Position; i++)
                    row[i] = null;
            }
            // Copy the data to the correct columns of the global table.
            row[hg.Position] = { Values: [time[r]] };
            for (var i = hg.Position + 1; i < allHeaders.length && allHeaders[i].Instance.ID == instance.ID; i++)
                row[i] = data[r][allHeaders[i].SpeciesIndex];
        }
        this.Rows(currData);
    }

    public AddInstanceRow(instanceID: number, time: number, data: CellData[]) {
        this.AddInstanceRows(instanceID, [time], [data]);
    }

    AreNamesEnabled: KnockoutObservable<boolean> = ko.observable(true);
    SupportsStructural = ko.pureComputed(() => {
        return this.Instances().length > 0 && this.Instances().every(instance => instance.Species().every(species => species.Structural != null && species.Structural != ""));
    });
    AreBoundsEnabled: KnockoutObservable<boolean> = ko.observable(true);
    SupportsBounds = ko.pureComputed(() => {
        return this.Instances().length > 0 && this.Instances().every(instance => {
            var rows = instance.Rows();
            return rows != null && rows.length > 0 && rows[0].Data.length > 0 && rows[0].Data[0].Values.length > 1;
        });
    });

    tableConfig: any = {
        data: this.VisibleRows,
        headerTemplate: 'table-view-header',
        columnTemplate: 'table-view-template',
        ViewModel: this,
        last: true,
    };

    public Filter: MultiInstanceFilter.View;
    public Progress: ProgressView.ProgressView;

    constructor() {
        this.Filter = new MultiInstanceFilter.View();
        this.Progress = new ProgressView.ProgressView();
    }

    // This function generates a CSV file with the current data.
    Save() {
        var csv = Papa.unparse({
            data: this.VisibleRows(),
            fields: this.VisibleHeaders()
        },
            {
                quotes: false,
                delimiter: ",",
                newline: "\r\n"
            });
        var blob = new Blob([csv], { type: "text/csv" });
        saveAs(blob, "SimulationResult.csv");
    }
}

// (FP) This class implements the control. It implements IViewObserver because it's part of the larger Viewer control (the corresponding transform is further down).
export class View implements Rx.IObserver<Framework.IVisualizationUpdateMessage> {
    private vm: TableVM;

    constructor() {
        this.vm = new TableVM();
    }

    // (FP) Constructs the component in the provided element.
    bind = (elementToBindTo: HTMLElement) => {
        if (elementToBindTo == null)
            throw "Attempt to bind TableView to null";
        var vm = this.vm;
        ko.applyBindings(vm, elementToBindTo);

        vm.Filter.GetSelectedSpecies().subscribe(names => {
            this.ShowSpecies(names);
        });
        vm.Filter.GetSelectedInstances().subscribe(instances => {
            this.ShowInstances(instances);
        });
        vm.Filter.GetSelectedSweeps().subscribe(sweeps => {
            this.ShowSweeps(sweeps);
        })
        vm.Filter.GetSelectedSettings().subscribe(settings => {
            this.ShowSettings(settings);
        });
        vm.Filter.GetSelectedSystems().subscribe(systems => {
            this.ShowSystems(systems);
        });
    }

    //Framework.IView<IUpdateMessage> implementation
    // (FP) The higher layer will call this to send a message to this control.
    public onNext = (update: Framework.IVisualizationUpdateMessage) => {
        this.vm.Progress.onNext(update);
        switch (update.MessageType) {
            case Framework.VisualizationUpdateMessageType.TraceDefinitions:
                var traces = <I.ITraceDefinitions>update.EncapsulatedUpdate;
                this.vm.Filter.SetAvailableTraces(traces);
                var instances: LocalInstance[] = [];
                for (let crn of traces.CRNs)
                    for (let settings of crn.Settings)
                        for (let sweep of settings.Sweeps)
                            for (let instance of sweep.Instances)
                                instances[instance.ID] = { Settings: settings.Name, Sweep: sweep.Name, ID: instance.ID, Name: instance.Name, System: crn.Name, Species: ko.observableArray(instance.Plottables.slice()), Rows: ko.observableArray() };
                this.vm.Instances(instances);
                break;
            case Framework.VisualizationUpdateMessageType.AdditionalPlottable:
                var newPlottable = <I.IAdditionalPlottable>update.EncapsulatedUpdate;
                this.vm.Filter.AddSpeciesToInstance(newPlottable);
                var instance: LocalInstance = this.vm.Instances()[newPlottable.Instance];
                instance.Species.push(newPlottable);
                break;
            case Framework.VisualizationUpdateMessageType.SimulationTable:
                var table = <Framework.ISimTable>update.EncapsulatedUpdate;
                for (var instance of this.vm.Instances())
                    instance.Rows([]);
                this.vm.ResetRows();
                var tdata: CellData[][] = [];
                for (var c = 0; c < table.Time.length; c++) {
                    var rdata: CellData[] = [];
                    for (var d = 0; d < table.Counts.length; d++) {
                        var values = [table.Counts[d][c]];
                        if (table.Bounds != null) {
                            values.push(table.Bounds[d][c].lower);
                            values.push(table.Bounds[d][c].upper);
                        }
                        rdata.push({ Values: values });
                    }
                    tdata.push(rdata);
                };
                this.vm.AddInstanceRows(table.Instance, table.Time, tdata);
                break;
            case Framework.VisualizationUpdateMessageType.SimulationStep:
                // (FP) A simulation step. Push it into the correct observable array in the VM (there may be more than one, it depends on the sweep key).
                var step = <Framework.ISimStepData>update.EncapsulatedUpdate;
                var data = [];
                for (var c = 0; c < step.Counts.length; c++) {
                    var values = [step.Counts[c]];
                    if (step.Bounds != null) {
                        values.push(step.Bounds[c].lower);
                        values.push(step.Bounds[c].upper);
                    }
                    data.push({ Values: values });
                }
                this.vm.AddInstanceRow(step.Instance, step.Time, data);
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
        this.vm.ResetRows();
    }

    public ShowSettings(names: Array<string>) {
        this.vm.CurrentSettings(names);
        this.vm.ResetRows();
    }

    public ShowSweeps(names: Array<string>) {
        this.vm.CurrentSweeps(names);
        this.vm.ResetRows();
    }

    public ShowInstances(names: Array<string>) {
        this.vm.CurrentInstances(names);
        this.vm.ResetRows();
    }

    public ShowSpecies(names: Array<string>) {
        this.vm.CurrentSpecies(names);
        this.vm.ResetRows();
    }

    Reset() {
        this.vm.Filter.SetAvailableTraces({ ModelName: "", CRNs: [] });
        this.vm.Instances.removeAll();
        this.vm.ResetRows();
    }
}
