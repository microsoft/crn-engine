// (FP) This file contains the implementation of the inference summary control, which is basically a text area showing the summary text (plus a progress control).

import * as ko from "knockout";
import * as $ from "jquery";
import * as Rx from "rx";
import { saveAs } from "file-saver";
import * as Inference from "./InferenceViewer";
import { InferenceProgress } from "./InferenceProgress";

import * as template from 'raw-loader!../html/inference-summary.html';
ko.components.register("inference-summary-viewer", {
    template: template,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            // Return the parent VM.
            if (componentInfo.element == null)
                throw "Attempt to get an InferenceSummary VM for null";
            return ko.contextFor(componentInfo.element).$data;
        }
    }
});

// (FP) This class implements the control. It's an IInferenceViewer, so it can receive an inference run.
export class InferenceSummary implements Inference.IInferenceViewer {
    private storage: KnockoutObservable<{ [nodeId: string]: Inference.ISummary }> = ko.observable({});
    private active = ko.observable(false);
    private summary = ko.pureComputed(() => {
        if (!this.active())
            return "";
        var nodeId = this.nodeSelector.SelectedNodeID();
        var storage = this.storage();
        if (storage[nodeId] == null)
            return "";
        return storage[nodeId].Summary;
    });
    private Progress: InferenceProgress;

    constructor(private nodeSelector: Inference.INodeSelector, isActive?: boolean) {
        this.Progress = new InferenceProgress(nodeSelector);
        this.active(isActive !== undefined && isActive);
    }

    public saveAs() {
        var blob = new Blob([this.summary()], { type: "text/txt" });
        saveAs(blob, "inferencesummary.txt");
    }

    // (FP) Call this to create this control in the specified element.
    Bind(div: HTMLElement) {
        if (div == null)
            throw "Attempt to bind an InferenceSummary to null";
        ko.cleanNode(div);
        ko.applyBindings(this, div);

        var that = this;
        $(div).on("activate", () => {
            that.active(true);
        });
        $(div).on("deactivate", () => {
            that.active(false);
        });
    }

    show(run: Inference.IInferenceRun) {
        var sampleFreq = 250; // [ms] Update plots and parameters not more than once per sampleFreq
        // Decorates given set of observables by adding sampling to some of them to improve performance of this component.
        var sampledRun =
        {
            progress: run.progress,
            paramUpdates: run.paramUpdates.sample(sampleFreq), // <-- sampling
            summary: run.summary.sample(sampleFreq), // <-- sampling
            simulationUpdates: run.simulationUpdates,
            paramDefinitions: run.paramDefinitions,
            posteriorTableUpdates: run.posteriorTableUpdates,
            traceDefinitions: run.traceDefinitions,
            plotSettingsUpdates: run.plotSettingsUpdates
        };

        this.storage({});
        this.Progress.show(sampledRun);
        sampledRun.summary.subscribe(summary => {
            var storage = this.storage();
            storage[summary.NodeID] = summary;
            this.storage(storage);
        }, error => {
            console.log(error);
        }, () => { });
    }
}