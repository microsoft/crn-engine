// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from "knockout";
import * as Framework from "./SimulationViewerFramework";
import * as I from "../../GenericComponents/Scripts/Interfaces";
import * as template from 'raw-loader!../html/ProgressViewer.html';
ko.components.register("sim-progress-view", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    },
});

class InstanceInfo {
    ID: number;
    MaxTime: number;
    CurrentTime: number;
    MaxTrajectories: number;
    Trajectories: number;
}

export class ProgressView implements Rx.IObserver<Framework.IVisualizationUpdateMessage> {
    public Progress = ko.observable<number>(null);
    public Percentage = ko.computed<number>(() => this.Progress() == null ? null : Math.round(this.Progress() * 100));

    private infos: { [id: number]: InstanceInfo } = {};

    public onNext = (update: Framework.IVisualizationUpdateMessage) => {
        if (update.MessageType == Framework.VisualizationUpdateMessageType.SimulationStep || update.MessageType == Framework.VisualizationUpdateMessageType.SimulationStep1D || update.MessageType == Framework.VisualizationUpdateMessageType.SimulationStep2D) {
            var step = <Framework.ISimStep>update.EncapsulatedUpdate;
            this.infos[step.Instance].CurrentTime = step.Time;
            var tot = 0, count = 0;
            for (let id in this.infos) {
                let info = this.infos[id];
                tot += (info.CurrentTime / info.MaxTime);
                count++;
            }
            this.Progress(tot / count);
        }
        else if (update.MessageType == Framework.VisualizationUpdateMessageType.SimulationTable) {
            var table = <Framework.ISimTable>update.EncapsulatedUpdate;
            this.infos[table.Instance].Trajectories++;
            var tot = 0, count = 0;
            for (let id in this.infos) {
                let info = this.infos[id];
                tot += (info.Trajectories / info.MaxTrajectories);
                count++;
            }
            this.Progress(tot / count);
        }
        else if (update.MessageType == Framework.VisualizationUpdateMessageType.TraceDefinitions) {
            this.infos = {};
            var tr = <I.ITraceDefinitions>update.EncapsulatedUpdate;
            for (let crn of tr.CRNs)
                for (let settings of crn.Settings)
                    for (let sweep of settings.Sweeps)
                        for (let instance of sweep.Instances)
                            this.infos[instance.ID] = { ID: instance.ID, MaxTime: instance.MaxTime, CurrentTime: 0, MaxTrajectories: instance.MaxTrajectories, Trajectories: 0 };
            this.Progress(null);
        }
        else if (update.MessageType == Framework.VisualizationUpdateMessageType.Reset)
            this.Reset();
    }

    public onError(exception: any) {
        this.Reset();
    }

    public onCompleted() {
        this.Reset();
    }

    public Reset() {
        this.Progress(null);
    }

    public bind(elementToBindTo: HTMLElement) {
        if (elementToBindTo == null)
            throw "Attempt to bind ProgressView to null";
        var that = this;
        ko.applyBindings(that, elementToBindTo);
    }
}
