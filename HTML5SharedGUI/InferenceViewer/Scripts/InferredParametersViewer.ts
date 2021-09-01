// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// (FP) This file contains the implementation of the parameters viewer. This viewer uses a table-viewer control internally (a KnockoutGrid).

import "../../KnockoutGrid/knockoutgrid";
import * as ko from "knockout";
import * as $ from "jquery";
import { saveAs } from "file-saver";
import * as Rx from "rx";
import * as Inference from "./InferenceViewer";
import { InferenceProgress } from "./InferenceProgress";
import * as template from "raw-loader!../html/inferred-parameters.html";

ko.components.register("inferred-parameters-viewer", {
    template: template,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            if (componentInfo.element == null)
                throw "Attempt to get an InferredParametersViewer VM for null";
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context == null ? {} : context.$data;
        }
    }
});

// (FP) The VM for the control. It contains a VM for the progress control located at the top, plus the data to be displayed in the table. Note that the information for a given row is not all in the same object; there is an array for parameters, and a separate one for values. This is because the values are the changing part, and it makes sense to update them separately. The separate parts are then related in the HTML binding through the currIndex function, i.e. the context is the IParameterDefinition instance, and the binding for the values retrives its index and then uses it to access the values array for the corresponding value.
// Design note: alternatively, we could store it all in the same object, and ensure that the value is observable separately.
class InferredParametersViewerVM {
    Progress = new InferenceProgress(this.nodeSelector);

    parametersStorage = ko.observable<{ [nodeId: string]: KnockoutObservableArray<Inference.IParameterDefinition> }>({});
    valuesStorage = ko.observable<{ [nodeId: string]: KnockoutObservableArray<number> }>({});

    parameters = ko.pureComputed(() => {
        var curr = this.parametersStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr();
    })
    values = ko.pureComputed(() => {
        var curr = this.valuesStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr();
    });

    constructor(private nodeSelector: Inference.INodeSelector) { }

    // (FP) The function returning the index of a parameter definition, which is the same index as its value in the values array.
    currIndex(data: Inference.IParameterDefinition) {
        return this.parameters().indexOf(data);
    }

    // (FP) The parameter type is an enumeration; this function converts it to a string. It is referred to in the HTML binding.
    convertType(val: Inference.ParameterType) {
        return Inference.ParameterType[val];
    }

    // (FP) The parameter variability is an enumeration; this function converts it to a string. It is referred to in the HTML binding.
    convertVar(val: Inference.ParameterVariability) {
        return Inference.ParameterVariability[val];
    }

    parametersConfig: any = {
        data: this.parameters,
        headerTemplate: 'inferred-parameters-viewer-header',
        columnTemplate: 'inferred-parameters-viewer-template',
        ViewModel: this,
        showAll: true
    };

    public Save() {
        var csv = "Name,Lower Bound,Upper Bound,Value,Type,Var\r\n";
        let parameters = this.parameters();
        let values = this.values();
        for (let i = 0; i < parameters.length; i++) {
            let p = parameters[i];
            let value = values[i];
            csv += p.Name + "," + p.LowerBound + "," + p.UpperBound + "," + value + "," + this.convertType(p.Type) + "," + this.convertVar(p.Variability) + "\r\n";
        }
        var blob = new Blob([csv], { type: "text/csv" });
        saveAs(blob, "InferredParameters.csv");
    }
}

import * as inferredParametersRowsTemplate from 'raw-loader!../html/inferred-parameters-rows.html';

// (FP) This class implements the control. It is an IInferenceViewer because it can receive a set of inference run observables.
export class InferredParametersViewer implements Inference.IInferenceViewer {
    private vm = new InferredParametersViewerVM(this.nodeSelector);

    private isActive = false;

    constructor(private nodeSelector: Inference.INodeSelector, isActive?: boolean) {
        this.isActive = isActive == true;
    }

    private buffer: { [nodeId: string]: Inference.IRecentParametersValues } = {};
    // (FP) This flag is used to avoid enqueuing redundant updates.
    private updateRequest = false;

    // Requests to schedule an update to the observables.
    private scheduleUpdate() {
        if (this.isActive && !this.updateRequest) {
            // (FP) The updateRequest flag guards against scheduling an update while an update is already scheduled.
            this.updateRequest = true;
            setTimeout(() => {
                // Copy data from the buffer to the observables.
                for (let nodeId in this.buffer) {
                    let values = this.buffer[nodeId];
                    let storage = this.vm.valuesStorage();
                    if (storage[nodeId] == null) {
                        storage[nodeId] = ko.observableArray(values.values);
                        this.vm.valuesStorage(storage);
                    }
                    else
                        storage[nodeId](values.values);
                }
                this.buffer = {};
                this.updateRequest = false;
            });
        }
    }

    // (FP) This function constructs the component in the given element.
    Bind(div: HTMLElement) {
        if (div == null)
            throw "Attempt to bind a ParametersViewer to null";
        var that = this;

        ko.cleanNode(div);
        // (FP) Ensure that the templates for the table viewer are loaded into the document.
        if (!$('#parameters-viewer-template').length) $('head').append(inferredParametersRowsTemplate);
        ko.applyBindings(this.vm, div);

        $(div).on("activate", () => {
            that.isActive = true;
            that.scheduleUpdate();
        });
        $(div).on("deactivate", () => {
            that.isActive = false;
            that.scheduleUpdate();
        });
    }

    // (FP) The higher layer calls this to provide an inference run. Note that the updates are buffered, to avoid unnecessarily frequent updates.
    show(run: Inference.IInferenceRun) {
        var sampleFreq = 250; // [ms] Update plots and parameters not more than once per sampleFreq
        // Decorates given set of observables by adding sampling to some of them to improve performance of this component.
        var sampledRun =
        {
            progress: run.progress,
            paramUpdates: run.paramUpdates.sample(sampleFreq), // <-- sampling
            simulationUpdates: run.simulationUpdates,
            paramDefinitions: run.paramDefinitions,
            summary: run.summary,
            posteriorTableUpdates: run.posteriorTableUpdates,
            traceDefinitions: run.traceDefinitions,
            plotSettingsUpdates: run.plotSettingsUpdates
        };


        // (FP) Pass it to the progress viewer too.
        this.vm.Progress.show(sampledRun);
        this.vm.valuesStorage({});
        this.vm.parametersStorage({});

        var that = this;

        // (FP) The parameter definitions are fixed, and get set at the first update.
        sampledRun.paramDefinitions.subscribe(
            val => {
                let storage = this.vm.parametersStorage();
                storage[val.NodeID] = ko.observableArray(val.Definitions);
                this.vm.parametersStorage(storage);
            },
            err => { console.log(err.message); },
            () => { }
        );
        // (FP) The parameter updates can be frequent. They will be copied to the buffer, and then an update will be scheduled with a setTimeout. This will cause the update to happen as soon as the main thread is idle.
        sampledRun.paramUpdates.subscribe(
            val => {
                that.buffer[val.NodeID] = val;
                that.scheduleUpdate();
            },
            err => { console.log(err.message); },
            () => { that.updateRequest = false; }
        );
    }
}
