// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// (FP) This file implements the table view (specifically for simulation results). Note that this does not implement the actual table: it makes use of the table component that's implemented in KnockoutGrid.js (found in HTML5SharedGUI).

import "../../KnockoutGrid/knockoutgrid";
import * as ko from "knockout";
import * as $ from "jquery";
import { saveAs } from "file-saver";
import * as Papa from "papaparse";
import * as Framework from "./SimulationViewerFramework";
import * as ProgressView from "./ProgressView";
import * as tableTemplate from 'raw-loader!../html/TableView.html';
import * as rowsTemplate from 'raw-loader!../html/Spatial1DTableRows.html';
import * as I from "../../GenericComponents/Scripts/Interfaces";
$('head').append(rowsTemplate);
ko.components.register("spatial-1d-table-view", {
    template: tableTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

interface TableRow {
    t: number;
    data: number[];
}

class InstanceData {
    constructor(public ID: number) { }
    public Time: KnockoutObservableArray<number> = ko.observableArray([]).extend({ rateLimit: 1000 });
    public RawData: KnockoutObservableArray<number[][]> = ko.observableArray([]).extend({ rateLimit: 1000 });
}

// (FP) This class is the VM for the table viewer. It contains the data that gets displayed and any relative configuration (names, choice of sim, etc).
class TableVM {
    public Progress: ProgressView.ProgressView = new ProgressView.ProgressView();

    xAxis: KnockoutObservableArray<number> = ko.observableArray([]);
    instances: KnockoutObservable<InstanceData[]> = ko.observable([]);
    tAxis: KnockoutComputed<number[]> = ko.pureComputed(() => {
        let instance = this.currentInstance();
        let instanceData = this.instances.get(instance.toString())();
        if (instanceData == null)
            return [];
        return instanceData.Time();
    })
    currentInstance: KnockoutObservable<number> = ko.observable(0);
    currentSpecies: KnockoutObservable<number> = ko.observable(0);
    data: KnockoutComputed<TableRow[]> = ko.pureComputed(() => {
        let instance = this.currentInstance();
        let instanceData = this.instances()[instance];
        if (instanceData == null)
            return [];
        let rawData = instanceData.RawData();
        let time = instanceData.Time();
        var species = this.currentSpecies();
        var ret: TableRow[] = [];
        if (species >= 0)
            for (var i = 0; i < rawData.length; i++) {
                ret.push({
                    t: time[i],
                    data: rawData[i][species]
                });
            }
        return ret;
    });
    SupportsStructural = ko.observable(false);
    SupportsBounds = ko.observable(false);
    AreBoundsEnabled = ko.observable(false);

    tableConfig: any;

    constructor() {
        // (FP) The table config is the object that gets used by the KnockoutGrid component to know what to show.
        this.tableConfig = {
            data: this.data,
            headerTemplate: 'spatial-1d-table-view-header',
            columnTemplate: 'spatial-1d-table-view-template',
            ViewModel: this,
            last: true,
        };
    }

    // (FP) This function generates a CSV file with the current data.
    Save() {
        var data = this.data;
        var names = new Array();
        names.push("Time");
        this.xAxis().forEach(val => names.push(val));
        var csv = Papa.unparse({
            data: data().map((val) => {
                var arr = new Array();
                arr.push(val.t);
                val.data.forEach(val => arr.push(val));
                return arr;
            }),
            fields: names
        },
            {
                quotes: false,
                delimiter: ",",
                newline: "\r\n"
            });
        var blob = new Blob([csv], { type: "text/csv" });
        saveAs(blob, "Spatial1DSimulationResult.csv");
    }

    public Filter: any = null;
}

export class View implements Rx.IObserver<Framework.IVisualizationUpdateMessage> {
    private vm: TableVM;

    constructor() {
        this.vm = new TableVM();
    }

    public ShowInstance(id: number) {
        this.vm.currentInstance(id);
    }

    public ShowSpecies(idx: number) {
        this.vm.currentSpecies(idx);
    }

    // (FP) Constructs the component in the provided element.
    public Bind(elementToBindTo: HTMLElement) {
        if (elementToBindTo == null)
            throw "attempt to bind Spatial1DTableView to null";
        ko.applyBindings(this.vm, elementToBindTo)
    };

    // (FP) The caller needs to use this function to provide a source of simulation data.
    public onNext(update: Framework.IVisualizationUpdateMessage) {
        var vm = this.vm;
        vm.Progress.onNext(update);
        switch (update.MessageType) {
            case Framework.VisualizationUpdateMessageType.TraceDefinitions:
                var traces = <I.ITraceDefinitions>update.EncapsulatedUpdate;
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
                // This should work in theory, but is untested.
                break;
            case Framework.VisualizationUpdateMessageType.SimulationStep1D:
                var data = <Framework.I1DSimStepData>update.EncapsulatedUpdate;
                var instance = this.vm.instances()[data.Instance];
                instance.Time.push(data.Time);
                instance.RawData.push(data.Data);
                break;
            case Framework.VisualizationUpdateMessageType.PlotSettingsInfo:
                var settings = <Framework.PlotSettings>update.EncapsulatedUpdate;
                break;
            case Framework.VisualizationUpdateMessageType.Reset:
                this.Reset();
                break;
            default:
                break;
        }
    }

    public onError(exception: any) {
        console.log("Spatial1DTableView exception: " + JSON.stringify(exception));
        throw exception;
    }

    public onCompleted() {
    }

    private Reset() {
        this.vm.instances([]);
        this.vm.currentInstance(0);
        this.vm.currentSpecies(0);
    }
}
