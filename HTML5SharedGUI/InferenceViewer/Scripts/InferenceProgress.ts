// (FP) This is a component that shows the progress of an inference run. It has a progress bar, plus some text fields.

import * as ko from "knockout";

import * as Inference from "./InferenceViewer";

import * as template from 'raw-loader!../html/inference-progress.html';
ko.components.register("inference-progress", {
    template: template,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            // Return the parent VM.
            if (componentInfo.element == null)
                throw "Attempt to get an InferenceProgress VM for null";
            var context = ko.contextFor(componentInfo.element);
            return context == null ? {} : context.$data;
        }
    }
});

// (FP) This class implements the component. Note that, unlike other designs in the simulation viewer library, this covers the roles of both View and ViewModel (it would be more accurate to say that this is the ViewModel, and the View is pure HTML). Note that it is an IInferenceViewer, which means it can receive an inference run.
export class InferenceProgress implements Inference.IInferenceViewer {
    private progressStorage: KnockoutObservable<{ [nodeId: string]: Inference.IProgress }> = ko.observable({});
    private paramStorage: KnockoutObservable<{ [nodeId: string]: Inference.IRecentParametersValues }> = ko.observable({});
    private maxStorage: KnockoutObservable<{ [nodeId: string]: number }> = ko.observable({});

    private currentProgress = ko.pureComputed(() => this.progressStorage()[this.nodeSelector.SelectedNodeID()]);
    private currentParams = ko.pureComputed(() => this.paramStorage()[this.nodeSelector.SelectedNodeID()]);
    private currentMax = ko.pureComputed(() => this.maxStorage()[this.nodeSelector.SelectedNodeID()]);

    max = ko.pureComputed(() => this.currentMax() == null ? -Infinity : this.currentMax());
    progress = ko.pureComputed(() => Math.round(this.currentProgress() == null ? 0 : (this.currentProgress().CurrentIteration * 100 / (this.currentProgress().BurnInLength + this.currentProgress().SamplingLength))));
    lglk = ko.pureComputed(() => this.currentParams() == null ? -Infinity : this.currentParams().lglk);
    iteration = ko.pureComputed(() => this.currentProgress() == null ? 0 : this.currentProgress().CurrentIteration);

    constructor(private nodeSelector: Inference.INodeSelector) {
    }

    show(run: Inference.IInferenceRun) {
        this.progressStorage({});
        this.paramStorage({});
        this.maxStorage({});

        // (FP) Subscribe to the observables in the run that contain interesting data (iteration and log likelihood). Note that I'm grouping by NodeID, otherwise the sampling will miss some data from the end of each node but the last.
        run.progress.groupBy(val => val.NodeID).subscribe(
            kval => {
                kval.sample(250).subscribe(val => {
                    var storage = this.progressStorage();
                    storage[val.NodeID] = val;
                    this.progressStorage(storage);
                }, err => console.log(err), () => { });
            }, err => console.log(err), () => { });
        run.paramUpdates.groupBy(val => val.NodeID).subscribe(
            kval => {
                kval.sample(250).subscribe(val => {
                    var storage = this.paramStorage();
                    var maxStorage = this.maxStorage();
                    storage[val.NodeID] = val;
                    if (val.lglk > maxStorage[val.NodeID] || maxStorage[val.NodeID] == null) {
                        maxStorage[val.NodeID] = val.lglk;
                        this.maxStorage(maxStorage);
                    }
                    this.paramStorage(storage);
                }, err => console.log(err), () => { });
            }, err => console.log(err), () => { });
    }
}