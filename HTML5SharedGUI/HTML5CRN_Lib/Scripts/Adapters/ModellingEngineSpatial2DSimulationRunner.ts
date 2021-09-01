// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as SimulateOperation from '../Operations/Spatial1DSimulation';
import ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as CRNInterfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as SimViewer from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/SimulationViewerFramework';
import * as Rx from 'rx';
import * as I from "../../../GenericComponents/Scripts/Interfaces";
import * as Utils from "../Utils";

function axisDescriptionToValues(num: number, maxCoord: number): number[] {
    let values: number[] = [];
    let step = maxCoord / (num - 1);
    for (let i = 0; i < num; i++)
        values.push(i * step);
    return values;
}

//Uses ModellingEngine.ts layer to run simulation.
export class SimulationRunner implements
    SimulateOperation.ISimulationRunner {
    constructor(private me: ME) { }

    //SimulateOperation.ISimulationRunner<ISimulationMessage> implementation
    public SimulateModel(ig: CRNInterfaces.IG, nodeId: string): Rx.Observable<SimViewer.IVisualizationUpdateMessage> {
        var model: CRNInterfaces.Model = ig.nodes[nodeId];
        var observables = this.me.UserSimulateGui(ig, nodeId);
        let updates = new Rx.ReplaySubject<SimViewer.IVisualizationUpdateMessage>();

        let attributes = model.top.attributes;

        let space = {
            XAxis: axisDescriptionToValues(model.top.settings.spatial.nx, model.top.settings.spatial.xmax),
            YAxis: axisDescriptionToValues(model.top.settings.spatial.nx, model.top.settings.spatial.xmax),
        };
        updates.onNext({ MessageType: SimViewer.VisualizationUpdateMessageType.SpatialSpace, EncapsulatedUpdate: space });

        let plotSettings = {
            XLabel: model.top.settings.plot.x_label,
            YLabel: model.top.settings.plot.y_label,
            Title: model.top.settings.plot.title,
            LabelFontSize: model.top.settings.plot.label_font_size,
            TickFontSize: model.top.settings.plot.tick_font_size,
            XTicks: model.top.settings.plot.x_ticks,
            YTicks: model.top.settings.plot.y_ticks,
            MaxTime: model.top.settings.simulation.final,
            XMin: model.top.settings.plot.x_min,
            XMax: model.top.settings.plot.x_max,
            YMin: model.top.settings.plot.y_min,
            YMax: model.top.settings.plot.y_max,
            VBoundaries: model.top.settings.plot.v_boundaries.map(Utils.parseFloatExp),
            HBoundaries: model.top.settings.plot.h_boundaries.map(Utils.parseFloatExp)
        };
        updates.onNext({ MessageType: SimViewer.VisualizationUpdateMessageType.PlotSettingsInfo, EncapsulatedUpdate: plotSettings });

        observables.instances.subscribe((instanceDefs: CRNInterfaces.SimulationInstanceDefinition[]) => {
            var traces: I.ITraceDefinitions = { ModelName: "", CRNs: [] };

            // Group the instances by crn and settings, into an ITracesDefinition structure.
            for (let instanceDef of instanceDefs) {
                // Generate a name for the instance, if there is no name defined in the message.
                var instanceName = instanceDef.instance.assignment;
                if (instanceName == null || instanceName == '')
                    // The generated string is a representation of the environment.
                    instanceName = Object.getOwnPropertyNames(instanceDef.instance.environment).map(p => p + "=" + instanceDef.instance.environment[p]).join(",");
                /*if (instanceDefs.length > 1)
                    // If there are multiple instances, I attach the ID to the name.
                    instanceName = instanceDef.id.toString() + ":" + instanceName;*/
                instanceDef.instance.name = instanceName;

                var plottables = instanceDef.instance.settings.plots.map(p => {
                    return {
                        Name: p,
                        Structural: attributes[p] == null ? null : attributes[p].structure,
                    }
                });

                var instance: I.ITraceDefinitionsInstance = { ID: instanceDef.id, Name: instanceDef.instance.name, Plottables: plottables, MaxTime: instanceDef.instance.settings.final, MaxTrajectories: 1 };
                var crn: I.ITraceDefinitionsCRN = null;
                for (let i in traces.CRNs)
                    if (traces.CRNs[i].Name == instanceDef.instance.model) {
                        crn = traces.CRNs[i];
                        break;
                    }
                if (crn == null) {
                    crn = { Name: instanceDef.instance.model, Settings: [] };
                    traces.CRNs.push(crn);
                }
                var settings: I.ITraceDefinitionsSetting = null;
                for (let i in crn.Settings)
                    if (crn.Settings[i].Name == instanceDef.instance.settings.name) {
                        settings = crn.Settings[i];
                        break;
                    }
                if (settings == null) {
                    settings = { Name: instanceDef.instance.settings.name, Sweeps: [] };
                    crn.Settings.push(settings);
                }
                var sweep: I.ITraceDefinitionsSweep = null;
                for (let i in settings.Sweeps)
                    if (settings.Sweeps[i].Name == instanceDef.instance.sweep) {
                        sweep = settings.Sweeps[i];
                    }
                if (sweep == null) {
                    sweep = { Name: instanceDef.instance.sweep, Instances: [] };
                    settings.Sweeps.push(sweep);
                }
                sweep.Instances.push(instance);
            };
            updates.onNext({ MessageType: SimViewer.VisualizationUpdateMessageType.TraceDefinitions, EncapsulatedUpdate: traces });
        }, error => { }, () => { });

        //simrun returns only run observables. 
        observables.simrun.subscribe(rundef => {
            // runs per se ends while corresponding rundef completes
            rundef.values.subscribe(simdata => {
                let update: SimViewer.I2DSimStepData = {
                    Instance: simdata.instance,
                    Time: simdata.time,
                    Data: (<number[][][]>simdata.values)
                }
                updates.onNext({ MessageType: SimViewer.VisualizationUpdateMessageType.SimulationStep2D, EncapsulatedUpdate: update });
            }, error => {
                console.error(JSON.stringify(error));
                updates.onError(error);
            }, () => updates.onCompleted()
            );
        }, error => {
            console.error(JSON.stringify(error));
            updates.onError(error);
        }, () => { });

        return updates;
    }

    public Abort() {
        this.me.Abort();
    }
}
