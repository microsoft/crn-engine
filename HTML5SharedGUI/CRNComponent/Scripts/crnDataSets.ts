// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// (FP) This file contains code that handles observation data sets for inference.

import "../../KnockoutGrid/knockoutgrid";
import * as Papa from "papaparse";
import * as $ from 'jquery';
import * as ko from 'knockout';
import * as datasetTemplate from 'raw-loader!../html/dataset-viewer.html';
import * as datasetsTemplate from 'raw-loader!../html/datasets-viewer.html';

ko.components.register("dataset-viewer", {
    viewModel: {
        createViewModel: (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) => {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    },
    template: datasetTemplate
});
ko.components.register("datasets-viewer", {
    viewModel: {
        createViewModel: (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) => {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    },
    template: datasetsTemplate
});


// (FP) This interface represents a place that can store data sets. There are several different methods for a web application to store data; they can
// all be used, providing a suitable implementation of this interface.
export interface IDataSetStorage {
    Add(name: string, data: IDataSet): JQueryPromise<void>;
    Remove(name: string): JQueryPromise<void>;
    Get(name: string): JQueryPromise<IDataSet>;
    GetNames(): JQueryPromise<string[]>;
}

// (FP) These interfaces represent data sets.
export type DataRow = number[];
export interface IDataSet {
    ColumnNames: string[];
    Data: DataRow[];
}

/**
 * Removes the extension (includeing dot (".")) from the filename.
 * @param name Filename to remove extension from.
 */
export function TruncateExt(name: string) {
    var dot_pos = name.lastIndexOf(".");
    if (dot_pos === -1)
        return name; //extension is already absent
    else
        return name.substring(0, dot_pos);
}

// (FP) Parses the content of a file. This works on CSV files. This is an asynchronous operation; PapaParse is designed that way (I don't know
// whether it runs chunks of work on a timer, or runs a web worker).
// Design note: the type of data here is "File | string". However, if I changed it to declare as such, the call to Papa.parse would not build
// because the TS declaration file declares it as two overrides (rather than one signature with "File | string").
export function ParseFile(data: any): JQueryPromise<IDataSet> {
    var dfd = $.Deferred<IDataSet>();
    Papa.parse(data, {
        complete: (results: PapaParse.ParseResult) => {
            var fields: string[] = results.data[0];
            // Eliminate the results of parsing blank rows.
            var sdata: string[][] = results.data.slice(1).filter(arr => arr != null && arr.length > 0 && arr[0] != null && arr[0] != "");
            // Convert values from string to number.
            var data: DataRow[] = sdata.map(d => d.map(parseFloat));
            dfd.resolve({
                ColumnNames: fields,
                Data: data
            });
        },
        error: (err: PapaParse.ParseError) => {
            console.warn(err.message);
            dfd.reject();
        },
        header: false,
        dynamicTyping: false
    });
    return dfd.promise();
}

// (FP) This class is the VM that's bound to the template in datasets-viewer.html. Also, it will get passed to a TableViewer, and act as a ViewModel for that
// too.
class DataSetsVM {
    container: HTMLElement;

    files: KnockoutObservableArray<string> = ko.observableArray([]);

    // (FP) Design note: I wonder whether selectedDataSetName could be a writable computed observable on selectedDataSet.
    selectedDataSetName = ko.observable<string>();
    selectedDataSet = ko.observable<IDataSet>();
    dragClass = ko.observable<string>();

    // (FP) inferenceConfig will become the parameters to the table component. This is declared in the HTML template. Note how some of the observables of
    // DataSetsVM get passed to the table component through this object. This is the outer table, the one that lists the datasets. Note the names of the
    // templates. Those will get loaded into HTML 
    inferenceConfig: any = {
        data: this.files,
        showAll: true,
        headerTemplate: 'data-sets-header',
        columnTemplate: 'data-sets-template',
        selected: this.selectedDataSetName,
        ViewModel: this,
    };

    // (FP) Bound to the drag event of the data sets component. This bit of code prevents other parts from handling dragging.
    drag = (data: any, e: Event) => {
        e.stopPropagation();
        e.preventDefault();
        this.dragClass("c-datasets__over");
    }

    dragleave = (data: any, e: Event) => {
        this.dragClass("");
    }

    // (FP) Bound to the drop event of the data sets component. This bit of code attempts to load dragged CSV file(s).
    drop = (data: any, e: Event) => {
        e.stopPropagation();
        e.preventDefault();
        var dt = (<any>e).originalEvent.dataTransfer;
        var files: FileList = dt.files;
        this.load(files);
        this.dragClass("");
    }

    // (FP) Bound to the change event of the load file button (that's the event that fires when the user selects files with it).
    listener = (data: any, evt: any) => {
        var files: FileList = evt.target.files;
        this.load(files);
    }

    // (FP) Handles a file list (passes them to addFile).
    private load = (files: FileList) => {
        for (let i = 0; i < files.length; i++) {
            let file = files.item(i);
            var filename = TruncateExt(file.name);
            this.addFile(file, filename);
        }
    }

    // (FP) Parses a file and puts the data it contains in the storage (passes it to Persist).
    private addFile = (data: File, name: string) => {
        var fieldsPromise = ParseFile(data).done((result) => {
            this.Persist(result.Data, name, result.ColumnNames).fail(() => {
                console.warn("A listener can't add the file " + name);
            });
        }).fail(() => {
            console.warn("A listener can't parse the file" + name);
        });
    }

    // (FP) Puts a file's content into storage.
    Persist(data: any, name: string, fields: string[]): JQueryPromise<void> {
        var dfd = $.Deferred<void>();
        this.storage.Add(name, { ColumnNames: fields, Data: data }).done(() => {
            console.log("file \"" + name + "\" persisted in the file storage");
            this.files.push(name);
            dfd.resolve();
        }).fail(() => {
            console.warn("Can't persist the file \"" + name + "\" in the file storage");
            dfd.reject();
        });

        return dfd.promise();
    }

    /* Load a new dataset. This emulates a click on the hidden browse file button. */
    LoadDataSet() {
        $(".j-csv-load", this.container).click();
    }

    // (FP) Removes the selected data set.
    RemoveDataSet(): JQueryPromise<void> {
        let name = this.selectedDataSetName();
        let promise = this.storage.Remove(name);
        promise.done(() => {
            this.files.remove(name);
        });
        return promise;
    }

    constructor(private storage: IDataSetStorage) {
        // (FP) Start by getting all currently known files from storage.
        storage.GetNames().done((result) => {
            this.files(result);
        }).fail(() => {
            console.warn("DataSets: Can't get file names");
        });

        //This computed is equal to this.selectedDataSetName.subscribe(...)
        // (FP) Design note: This is to connect the selection of a name with the loading of its dataset. We are making a computed observable
        // that is not actually observed by anyone; we just want it to get updated. This works, but I wonder why not using subscribe. Also, 
        // maybe this could be dealt with by making selectedDataSetName a writable computed observable instead.
        ko.computed(() => {
            let name = this.selectedDataSetName();
            if (name) {
                this.storage.Get(name).done((result) => {
                    this.selectedDataSet(result);
                }).fail(() => {
                    console.warn("Can't get the file " + this.selectedDataSetName() + " corresponding the selected dataset name");
                });
            }
        });
    }

}

// (FP) This class represents the data sets list component.
export class KnockoutBasedDataSetsList {
    private dataSets: DataSetsVM;

    getSelectedObservable() {
        return this.dataSets.selectedDataSet;
    }

    getFullList() {
        return this.dataSets.files;
    }

    constructor(private storage: IDataSetStorage) {
        this.dataSets = new DataSetsVM(storage);
    }

    Initialize(baseFolder: string, files: string[]) {
        var that = this;
        var loadDefaults = () => {
            files.forEach((val) => {
                var url = baseFolder + val;
                var oReq = new XMLHttpRequest();
                oReq.onload = () => {
                    if (oReq.status % 100 == 4)
                        console.log("Warning: attempting to load " + url + " resulted in status " + oReq.status + ".");
                    else
                        that.LoadFromString(oReq.responseText, val);
                }
                oReq.open("GET", url);
                oReq.send();
            })
        }
        this.storage.GetNames().done((names: string[]) => { if (!names.length) loadDefaults(); }).fail(err => { console.log(err); loadDefaults(); });
    }

    //The following method is used for programmatic file loading (e.g. via require text plugin)
    LoadFromString(data: string, name: string): JQueryPromise<void> {
        name = TruncateExt(name);
        let dfd = $.Deferred<void>();
        ParseFile(data).done((result) => {
            this.dataSets.Persist(result.Data, name, result.ColumnNames).done(() => {
                dfd.resolve();
            }).fail(() => {
                console.warn("Can't add the file " + name + " when loading from string");
                dfd.reject();
            });
        }).fail(() => {
            console.warn("Can't parse the file " + name + " when loading from string");
            dfd.reject()
        });
        return dfd.promise();
    }

    Bind(div: HTMLElement) {
        ko.cleanNode(div);
        this.dataSets.container = div;
        // (FP) Ensures that the row templates (for the list view) are present in the current HTML document.
        if (!$('#data-set-template').length)
            $('head').append(datasetRowsTemplate);
        ko.applyBindings(this.dataSets, div);
    }
}

// (FP) This is the VM for a single dataset, i.e. what shows up when you select a dataset in the list.
class DataSetVM {
    Data = ko.observableArray<Array<number>>([]);
    Names = ko.observableArray<string>([]);

    // (FP) dataSetConfig is the object that gets passed to the table component (in HTML). This is the inner table, the one that shows the
    // actual data.
    dataSetConfig: any = {
        data: this.Data,
        headerTemplate: 'data-set-header',
        columnTemplate: 'data-set-template',
        ViewModel: this,
    };

    // (FP) This loads a specific dataset into the VM.
    Show(data: IDataSet) {
        if (data) {
            // Replacing null values (which could come out of a corrupted database) with NaNs (which at least won't crash the program, while still remaining obviously wrong). I'm also emitting a console warning, but only once as when this happens, it can impact a large number of values.
            var warningEmitted = false;
            this.Data(data.Data.map(col => col.map(n => {
                if (n == null) {
                    if (!warningEmitted)
                        console.log("Warning: null value found in a dataset. Column names:" + data.ColumnNames.join(","));
                    warningEmitted = true;
                    return NaN;
                }
                return n;
            })));
            this.Names(data.ColumnNames);
        } else {
            this.Data([]);
            this.Names([]);
        }
    }
}

import * as datasetRowsTemplate from 'raw-loader!../html/dataset-rows.html';

// (FP) This class represents the single dataset component.
export class KnockoutGridDataSetViewer {
    private dataSet = new DataSetVM();

    constructor() { }

    Bind(div: HTMLElement) {
        ko.cleanNode(div);

        // (FP) Ensures that the row templates (for the list view) are present in the current HTML document.
        if (!$('#data-set-template').length)
            $('head').append(datasetRowsTemplate);
        ko.applyBindings(this.dataSet, div);
    }

    Show(data: IDataSet) {
        this.dataSet.Show(data);
    }
}
