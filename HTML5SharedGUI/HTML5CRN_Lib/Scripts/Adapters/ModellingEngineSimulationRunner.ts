// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as SimulateOperation from '../Operations/SimulateParsedCRN';
import ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as MeInterfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as SimViewer from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/SimulationViewerFramework';
import * as Rx from 'rx';
import { WebSharperGeneratedInterfaces as WGI } from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces';
import CRNEngine = WGI.Microsoft.Research.CRNEngine;
import * as I from '../../../GenericComponents/Scripts/Interfaces';
import * as InferenceOp from '../Operations/Inference';

export enum SimulationMessageType { SimulationStep, TracesInfo, NewPlottable, StateSpace, Probabilities, JITModel, Export, SimulationTable }

export interface ISimulationMessage {
    Type: SimulationMessageType;
    Payload: SimViewer.ISimStepData | SimViewer.ISimTable | I.ITraceDefinitions | I.IAdditionalPlottable | MeInterfaces.StateSpace | MeInterfaces.InstanceProbabilities | MeInterfaces.Model | MeInterfaces.ExportDef;
}

export interface IObservationsSource {
    GetData(sets: InferenceOp.IObservationsDef[]): JQueryPromise<CRNEngine.Dataset[]>;
}

// The CRNEngine returns multiple typed streams, differentiated by type. Here they get merged into a single stream, differentiated by a Type field.
function MergeSimulationStreams(obsSource: IObservationsSource, streams: MeInterfaces.SimulatorObservables): Rx.Observable<ISimulationMessage> {
    var stream: Rx.Subject<ISimulationMessage> = new Rx.Subject<ISimulationMessage>();
    // It is possible for instance declarations to be delayed, because I'll be waiting for observation data to arrive. Simulation data may arrive before the observation data has been read. In this case, the simulation data should be placed in a queue, and only be sent after instances.
    var delayed: ISimulationMessage[] = [];

    function handleError(error: any) {
        console.log("error in merged simulation streams: " + JSON.stringify(error));
        stream.onError(error);
    }

    var currentModel: MeInterfaces.Model = null;
    streams.node.subscribe((model: MeInterfaces.Model) => {
        currentModel = model;
        var modelMessage = {
            Type: SimulationMessageType.JITModel,
            Payload: model
        }
        stream.onNext(modelMessage);
    }, handleError, () => { });
    streams.exports.subscribe((exports: MeInterfaces.ExportDef) => {
        var exportsMessage = {
            Type: SimulationMessageType.Export,
            Payload: exports
        }
        stream.onNext(exportsMessage);
    }, handleError, () => { });

    streams.instances.subscribe((instanceDefs: MeInterfaces.SimulationInstanceDefinition[]) => {
        var traces: I.ITraceDefinitions = { ModelName: "", CRNs: [] };
        var c = 0;
        // Group the instances by crn and settings, into an ITracesDefinition structure. I need to associate observation data columns to instance plots. This is done by counting, with each Settings object having its own data.
        var set = 0, file = 0, column = 0;
        var currentCrnName: string = null;
        var currentSettingsName: string = null;

        // This function converts one instance, then starts an asynchronous task to associate observations to that instance, and then calls itself on the next instance.
        function next() {
            let instanceDef = instanceDefs[c];
            // Get the CRN this instance belongs to.
            var crn = currentModel.top;
            if (instanceDef.instance.model != crn.name)
                for (let scrn of currentModel.systems)
                    if (scrn.name == instanceDef.instance.model)
                        crn = scrn;
            // Get the Settings this instance belongs to.
            var settings = crn.settings.simulation;
            if (instanceDef.instance.settings.name != settings.name)
                for (let ssettings of crn.settings.simulations)
                    if (ssettings.name == instanceDef.instance.settings.name)
                        settings = ssettings;
            // If the CRN or the Settings is different, reset the columns counters.
            if (crn.name != currentCrnName || settings.name != currentSettingsName) {
                set = 0;
                file = 0;
                column = 0;
            }
            currentCrnName = crn.name;
            currentSettingsName = settings.name;

            var dataNames = instanceDef.instance.settings.data;
            if (dataNames.length == 0)
                dataNames = crn.settings.data.map(d => d.file);
            var sets = dataNames.map(dn => {
                return { name: dn, plotsCount: instanceDef.instance.settings.plots.length };
            });

            // Asynchronously retrieve the data files.
            obsSource.GetData(sets).done((data: CRNEngine.Dataset[]) => {
                // Generate a name for the instance, if there is no name defined in the message.
                var instanceName = instanceDef.instance.assignment;
                if (instanceName == null || instanceName == '')
                    // The generated string is a representation of the environment.
                    instanceName = Object.getOwnPropertyNames(instanceDef.instance.environment).map(p => p + "=" + instanceDef.instance.environment[p]).join(",");
                instanceDef.instance.name = instanceName;
                // Convert the plots into ITraceDefinitionsPlottable instances. Decorate with the observations, following the column counters.
                var plottables: I.ITraceDefinitionsPlottable[] = instanceDef.instance.settings.plots.map((p, i) => {
                    var dataFile = (data == null || data.length <= set) ? null : data[set].data[file];
                    var colour = instanceDef.instance.settings.plotcolours.length > i ? instanceDef.instance.settings.plotcolours[i] : null;
                    var ret = {
                        Name: p,
                        Structural: crn.attributes[p] == null ? null : crn.attributes[p].structure,
                        ObservationName: dataFile == null ? null : dataFile.columns[column].name,
                        ObservationCounts: dataFile == null ? null : dataFile.columns[column].values,
                        ObservationTimes: dataFile == null ? null : dataFile.times,
                        Colour: colour
                    };
                    if (dataFile != null && ++column >= data[set].data[file].columns.length) {
                        column = 0;
                        if (++file >= data[set].data.length) {
                            file = 0;
                            set++;
                        }
                    }
                    return ret;
                });

                var tcrn: I.ITraceDefinitionsCRN = null;
                for (let i in traces.CRNs)
                    if (traces.CRNs[i].Name == instanceDef.instance.model) {
                        tcrn = traces.CRNs[i];
                        break;
                    }
                if (tcrn == null) {
                    tcrn = { Name: instanceDef.instance.model, Settings: [] };
                    traces.CRNs.push(tcrn);
                }
                var settings: I.ITraceDefinitionsSetting = null;
                for (let i in tcrn.Settings)
                    if (tcrn.Settings[i].Name == instanceDef.instance.settings.name) {
                        settings = tcrn.Settings[i];
                        break;
                    }
                if (settings == null) {
                    settings = { Name: instanceDef.instance.settings.name, Sweeps: [] };
                    tcrn.Settings.push(settings);
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
                var instance: I.ITraceDefinitionsInstance = { ID: instanceDef.id, Name: instanceDef.instance.name, Plottables: plottables, MaxTime: instanceDef.instance.settings.final, MaxTrajectories: crn.settings.stochastic.trajectories };
                sweep.Instances.push(instance);

                // Execute this after ALL of the instances have been handled.
                if (++c == instanceDefs.length) {
                    var instancesInfo: ISimulationMessage = {
                        Type: SimulationMessageType.TracesInfo,
                        Payload: traces
                    };
                    stream.onNext(instancesInfo);
                    for (var i = 0; i < delayed.length; i++) {
                        var msg = delayed[i];
                        if (msg == null)
                            stream.onCompleted();
                        else
                            stream.onNext(msg);
                    }
                    delayed = null;
                }
                else
                    next();
            }).fail((error: any) => { stream.onError(error); });
        };
        next();
    }, handleError, () => { });

    // If I still have to send instances, then put the message in a queue, to be sent after instances. If instances have already been sent, forward the message immediately.
    function delayMsg(message: ISimulationMessage) {
        if (delayed != null)
            delayed.push(message);
        else
            stream.onNext(message);
    }
    // If I still have to send instances, then put the completion signal in a queue, to be sent after instances and all sim updates. If instances have already been sent, forward the completion signal immediately.
    function delayComplete() {
        if (delayed != null)
            delayed.push(null);
        else
            stream.onCompleted();
    }

    streams.simrun.subscribe((simdef: MeInterfaces.SimRunDefinition) => {
        switch (simdef.simtype) {
            case "Float":
                var updates = <Rx.Observable<MeInterfaces.SimResult<MeInterfaces.SimResultValue>>>simdef.values;
                updates.subscribe((update: MeInterfaces.SimResult<MeInterfaces.SimResultValue>) => {
                    var time = <number>update.time;
                    var counts = <Array<number>>update.values;
                    var message = {
                        Type: SimulationMessageType.SimulationStep,
                        Payload: { Time: time, Counts: counts, Instance: update.instance }
                    };
                    delayMsg(message);
                },
                    error => { console.log("error in simulation updates stream:" + JSON.stringify(error)); stream.onError(error); },
                    () => delayComplete());
                break;
            case "MeanStdev":
            case "MeanStdevProbabilities":
                var updates = <Rx.Observable<MeInterfaces.SimResult<MeInterfaces.SimResultValue>>>simdef.values;
                updates.subscribe((update: MeInterfaces.SimResult<MeInterfaces.SimResultValue>) => {
                    var time = <number>update.time;
                    var counts = <number[]>update.values.map((val: MeInterfaces.SimResultMeanStdev) => val.mean);
                    var bounds = <{ lower: number, upper: number }[]>update.values.map((val: MeInterfaces.SimResultMeanStdev) => { return { lower: val.mean - val.stdev, upper: val.mean + val.stdev } });
                    var message: ISimulationMessage = {
                        Type: SimulationMessageType.SimulationStep,
                        Payload: { Instance: update.instance, Time: time, Counts: counts, Bounds: bounds }
                    };
                    delayMsg(message);
                },
                    error => { console.log("error in simulation updates stream:" + JSON.stringify(error)); stream.onError(error); },
                    () => delayComplete());
                break;
            case "MeanStdevTable":
                var tables = <Rx.Observable<MeInterfaces.SimTable<MeInterfaces.SimResultValue>>>simdef.tables;
                tables.subscribe((table: MeInterfaces.SimTable<MeInterfaces.SimResultValue>) => {
                    var counts = table.values.map(col => col.map((cell: MeInterfaces.SimResultMeanStdev) => cell.mean));
                    var bounds = table.values.map(col => col.map((cell: MeInterfaces.SimResultMeanStdev) => { return { lower: cell.mean - cell.stdev, upper: cell.mean + cell.stdev } }));
                    var message: ISimulationMessage = {
                        Type: SimulationMessageType.SimulationTable,
                        Payload: { Instance: table.instance, Time: table.time, Counts: counts, Bounds: bounds }
                    };
                    delayMsg(message);
                },
                    error => { console.log("error in simulation tables stream:" + JSON.stringify(error)); stream.onError(error); },
                    () => delayComplete());
                break;
            default:
                throw "unexpected sim step type " + simdef.simtype;
        }
        var newplottables = <Rx.Observable<MeInterfaces.NewPlottableDefinition>>simdef.newplottables;
        newplottables.subscribe(definition => {
            var message = {
                Type: SimulationMessageType.NewPlottable,
                Payload: { Instance: definition.instance, Name: definition.plottable.name, Structural: definition.plottable.structural }
            };
            if (delayed != null)
                delayed.push(message);
            else
                stream.onNext(message);
        }, error => { }, () => { });
    }, handleError, () => { });

    streams.statespace.subscribe(ss => {
        var probMessage = {
            Type: SimulationMessageType.StateSpace,
            Payload: ss
        };
        stream.onNext(probMessage);
    }, handleError, () => { });

    streams.probabilities.subscribe(probs => {
        var probMessage = {
            Type: SimulationMessageType.Probabilities,
            Payload: probs
        };
        stream.onNext(probMessage);
    }, handleError, () => { });

    /*streams.plottables.subscribeOnCompleted(() => {
        stream.onCompleted();
    });*/
    return stream.asObservable();
};

//Uses ModellingEngine.ts layer to run simulation.
export class SimulationRunner implements
    SimulateOperation.ISimulationRunner<ISimulationMessage> {
    constructor(private me: ME, private obsSource: IObservationsSource) { }

    //SimulateOperation.ISimulationRunner<ISimulationMessage> implementation
    public SimulateModel(model: MeInterfaces.IG, nodeId: string): Rx.Observable<ISimulationMessage> {
        var simulationStreams = this.me.UserSimulateGui(model, nodeId);
        return MergeSimulationStreams(this.obsSource, simulationStreams);
    }

    public Abort() {
        this.me.Abort();
    }
}
