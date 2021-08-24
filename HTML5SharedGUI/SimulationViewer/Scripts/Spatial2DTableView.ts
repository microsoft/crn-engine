// (FP) This file implements the table view (specifically for simulation results). Note that this does not implement the actual table: it makes use of the table component that's implemented in KnockoutGrid.js (found in HTML5SharedGUI).

import "../../KnockoutGrid/knockoutgrid";
import * as ko from "knockout";
import * as $ from "jquery";
import { saveAs } from "file-saver";
import * as Papa from "papaparse";
import * as Framework from "./SimulationViewerFramework";
import * as ProgressView from "./ProgressView";
import * as tableTemplate from 'raw-loader!../html/TableView.html';
import * as rowsTemplate from 'raw-loader!../html/Spatial2DTableRows.html';
import * as I from "../../GenericComponents/Scripts/Interfaces";
$('head').append(rowsTemplate);
ko.components.register("spatial-2d-table-view", {
    template: tableTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

interface Record {
    y: number;
    values: number[];
}

function transposeArray(array: number[][]): number[][] {
    let newArray: number[][] = [];
    for (let j in array[0]) {
        newArray.push([]);
    }
    for (let i in array) {
        for (let j in array[i]) {
            newArray[j].push(array[i][j]);
        }
    }
    return newArray;
}

class InstanceData {
    constructor(public ID: number) { }
    public RawData: KnockoutObservable<number[][][]> = ko.observableArray([]);
}

// (FP) This class is the VM for the table viewer. It contains the data that gets displayed and any relative configuration (names, choice of sim, etc).
class TableVM {
    public Progress: ProgressView.ProgressView = new ProgressView.ProgressView();

    xAxis: KnockoutObservableArray<number> = ko.observableArray([]);
    yAxis: KnockoutObservableArray<number> = ko.observableArray([]);
    instances: KnockoutObservable<InstanceData[]> = ko.observable([]);
    currentInstance: KnockoutObservable<number> = ko.observable(0);
    currentSpecies: KnockoutObservable<number> = ko.observable(0);
    records: KnockoutComputed<Record[]> = ko.pureComputed(() => {
        let instance = this.currentInstance();
        let instanceData = this.instances()[instance];
        if (instanceData == null)
            return [];
        let arr: Record[] = [];
        let xAxis = this.xAxis();
        let yAxis = this.yAxis();
        let species = this.currentSpecies();
        let rawData = instanceData.RawData();
        let spData = rawData[species];
        if (spData == null)
            return [];
        let data = transposeArray(spData);
        for (let it in data) {
            let record: Record = {
                y: yAxis[it] ? yAxis[it] : NaN,
                values: data[it]
            };
            arr.push(record);
        }
        return arr;
    }).extend({ rateLimit: 1000 });
    SupportsStructural = ko.observable(false);
    SupportsBounds = ko.observable(false);
    AreBoundsEnabled = ko.observable(false);

    tableConfig: any;

    constructor() {
        // (FP) The table config is the object that gets used by the KnockoutGrid component to know what to show.
        this.tableConfig = {
            data: this.records,
            headerTemplate: 'spatial-2d-table-view-header',
            columnTemplate: 'spatial-2d-table-view-template',
            ViewModel: this,
            last: false,
        };
    }

    // (FP) This function generates a CSV file with the current data.
    Save() {
        let results = this.records;
        let xAxis: string[] = [];
        xAxis.push("Y\\X");
        this.xAxis().forEach(val => xAxis.push(val.toString()));
        var csv = Papa.unparse({
            data: results().map((val) => {
                var arr = new Array();
                arr.push(val.y);
                val.values.forEach(val => arr.push(val));
                return arr;
            }),
            fields: xAxis
        },
            {
                quotes: false,
                delimiter: ",",
                newline: "\r\n"
            });
        var blob = new Blob([csv], { type: "text/csv" });
        saveAs(blob, "Spatial2DSimulationResult.csv");
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
    // (FP) Constructs the component in the provided element.
    public Bind(elementToBindTo: HTMLElement) {
        if (elementToBindTo == null)
            throw "attempt to bind Spatial2DTableView to null";
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
                vm.yAxis(sp.YAxis);
                break;
            case Framework.VisualizationUpdateMessageType.AdditionalPlottable:
                console.log("JIT plots not currently supported in spatial view");
                break;
            case Framework.VisualizationUpdateMessageType.SimulationStep2D:
                var data = <Framework.I2DSimStepData>update.EncapsulatedUpdate;
                var instance = this.vm.instances()[data.Instance];
                instance.RawData(data.Data);
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
        console.log("Spatial2DTableView exception: " + JSON.stringify(exception));
        throw exception;
    }

    public onCompleted() {
    }

    private Reset() {
        this.vm.instances([]);
        this.vm.xAxis([]);
        this.vm.yAxis([]);
    }
}