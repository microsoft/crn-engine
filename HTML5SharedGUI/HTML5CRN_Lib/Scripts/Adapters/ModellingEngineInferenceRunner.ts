// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as InferOperation from '../Operations/Inference';
import ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as IV from '../../../../HTML5SharedGUI/InferenceViewer/Scripts/InferenceViewer';
import * as MeInterfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as Rx from 'rx';

const thin = 5;
const burnin = 200;

var printError = (error: any) => console.error(JSON.stringify(error));

export interface IObservationsSource {
    GetData(sets: InferOperation.IObservationsDef[]): JQueryPromise<MeInterfaces.Dataset[]>;
}

/**
 * Adapts CRNEngine to InferOperation.IInferenceEngine
 */
export class Adapter implements InferOperation.IInferenceEngine {
    constructor(private me: ME, private obsSource: IObservationsSource) { }

    public InferParameters(ig: MeInterfaces.IG): InferOperation.IInferenceRun {
        let model: MeInterfaces.Model = null;
        for (let k in ig.nodes) {
            model = ig.nodes[k];
            break;
        }
        var paramDefinitions = new Rx.Subject<IV.IParameterDefinitions>();
        var paramUpdates = new Rx.Subject<InferOperation.IRecentParamersValues>();
        var posteriorTableUpdates = new Rx.Subject<InferOperation.IRecentParamersValues>();
        var traceDefinitions = new Rx.Subject<IV.ITraceDefinitions>();
        var simulationUpdates = new Rx.Subject<InferOperation.IPlottableValues>();
        var iterationUpdates = new Rx.Subject<InferOperation.IIteration>();
        var summary = new Rx.Subject<IV.ISummary>();

        var observables = this.me.UserInferGui(ig);

        var plotSettingsUpdates = new Rx.Subject<InferOperation.IPlotSettingsValues>();
        var results: InferOperation.IInferenceRun = {
            iteration: iterationUpdates,
            paramDefinitions: paramDefinitions,
            paramUpdates: paramUpdates,
            posteriorTableUpdates: posteriorTableUpdates,
            traceDefinitions: traceDefinitions,
            simulationUpdates: simulationUpdates,
            plotSettingsUpdates: plotSettingsUpdates,
            summary: summary,
            exports: observables.exports
        }

        var parameterNamesMap: { [nodeId: string]: { [idx: string]: number } } = {}; // nodeId -> name -> idx
        var parameterDefReported: { [nodeId: string]: boolean } = {};
        var parameterUpdatesCounter: { [nodeId: string]: number } = {};
        var posteriorUpdatesCounter: { [nodeId: string]: number } = {};
        var instancesNames: { [idx: number]: string } = {};

        observables.progress.subscribe(next => {
            iterationUpdates.onNext({ NodeID: next.nodeId, Iteration: next.iteration });
        }, printError, () => { });

        //This forces having values which is annoying while experimenting

        observables.parameterdefinitions.subscribe(next => {
            var convertType = (t: MeInterfaces.InferenceParameterType) => {
                var res: IV.ParameterType;
                switch (t) {
                    case "Fixed":
                    case "Real": res = IV.ParameterType.Real; break;
                    case "Log": res = IV.ParameterType.Log; break;
                }
                return res;
            };

            var getVariability = (t: MeInterfaces.InferenceParameterType) => {
                if (t == "Fixed")
                    return IV.ParameterVariability.Fixed;
                else
                    return IV.ParameterVariability.Random;
            }

            var definitions = {
                NodeID: next.nodeId,
                Definitions: next.parameters.map(v => {
                    return <IV.IParameterDefinition>{
                        Name: v.name,
                        Type: convertType(v.range.pType),
                        LowerBound: v.range.lb,
                        UpperBound: v.range.ub,
                        Variability: getVariability(v.range.pType)
                    }
                })
            };
            parameterDefReported[next.nodeId] = true;
            if (parameterNamesMap[next.nodeId] == null)
                parameterNamesMap[next.nodeId] = {};
            for (var i = 0; i < definitions.Definitions.length; i++)
                parameterNamesMap[next.nodeId][definitions.Definitions[i].Name] = i;
            paramDefinitions.onNext(definitions);
        }, error => {
            printError(error);
            paramDefinitions.onError(error);
        }, () => paramDefinitions.onCompleted());

        var evaluatedValuesToInfViewerRecentValues = (nodeId: string, next: MeInterfaces.EvaluatedValues) => {
            var recentValues = next.values;
            var resultValues: Array<number> = [];
            for (var i in recentValues) {
                var idx = parameterNamesMap[nodeId][i];
                resultValues[idx] = recentValues[i];
            }
            return resultValues;
        }
        observables.parameterresults.subscribe(next => {
            if (parameterUpdatesCounter[next.nodeId] == null)
                parameterUpdatesCounter[next.nodeId] = 0;
            parameterUpdatesCounter[next.nodeId] += 1;
            if (parameterDefReported[next.nodeId] != true) //skipping parameters results as definitions are not yet receicved
                return;
            var resultValues = evaluatedValuesToInfViewerRecentValues(next.nodeId, next.values);
            paramUpdates.onNext({
                NodeID: next.nodeId,
                values: resultValues,
                lglk: next.values.lglk,
                iteration: parameterUpdatesCounter[next.nodeId]
            });
        }, error => {
            console.error(JSON.stringify(error));
            paramUpdates.onError(error);
            parameterNamesMap = {};
            parameterDefReported = {};
            parameterUpdatesCounter = {};
        }, () => {
            paramUpdates.onCompleted();
            parameterNamesMap = {};
            parameterDefReported = {};
            parameterUpdatesCounter = {};
        });
        observables.inferencechainupdate.subscribe(next => {
            if (posteriorUpdatesCounter[next.nodeId] == null)
                posteriorUpdatesCounter[next.nodeId] = 0;
            posteriorUpdatesCounter[next.nodeId] += 1;
            if (parameterDefReported[next.nodeId] != true) //skipping parameters results as definitions are not yet receicved
                return;
            var resultValues = evaluatedValuesToInfViewerRecentValues(next.nodeId, next.values);
            posteriorTableUpdates.onNext(
                {
                    NodeID: next.nodeId,
                    values: resultValues,
                    lglk: next.values.lglk,
                    iteration: model.top.settings.inference.burnin + ((posteriorUpdatesCounter[next.nodeId] - 1) * model.top.settings.inference.thin) + 1
                });
        },
            error => {
                console.error(JSON.stringify(error));
                posteriorTableUpdates.onError(error);
                posteriorUpdatesCounter = {};
            },
            () => {
                posteriorTableUpdates.onCompleted();
                posteriorUpdatesCounter = {};
            });

        var obsSource = this.obsSource;
        observables.instances.subscribe(defs => {
            var nodeId = defs.nodeId;
            var currentModel = ig.nodes[nodeId];
            var instanceDefs = defs.definitions;
            var traces: IV.ITraceDefinitions = { NodeID: nodeId, CRNs: [] };
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
                obsSource.GetData(sets).done((data: MeInterfaces.Dataset[]) => {
                    // Generate a name for the instance, if there is no name defined in the message.
                    var instanceName = instanceDef.instance.assignment;
                    if (instanceName == null || instanceName == '')
                        // The generated string is a representation of the environment.
                        instanceName = Object.getOwnPropertyNames(instanceDef.instance.environment).map(p => p + "=" + instanceDef.instance.environment[p]).join(",");
                    instanceDef.instance.name = instanceName;
                    // Convert the plots into ITraceDefinitionsPlottable instances. Decorate with the observations, following the column counters.
                    var plottables: IV.ITraceDefinitionsPlottable[] = instanceDef.instance.settings.plots.map((p, i) => {
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

                    var tcrn: IV.ITraceDefinitionsCRN = null;
                    for (let i in traces.CRNs)
                        if (traces.CRNs[i].Name == instanceDef.instance.model) {
                            tcrn = traces.CRNs[i];
                            break;
                        }
                    if (tcrn == null) {
                        tcrn = { Name: instanceDef.instance.model, Settings: [] };
                        traces.CRNs.push(tcrn);
                    }
                    var settings: IV.ITraceDefinitionsSetting = null;
                    for (let i in tcrn.Settings)
                        if (tcrn.Settings[i].Name == instanceDef.instance.settings.name) {
                            settings = tcrn.Settings[i];
                            break;
                        }
                    if (settings == null) {
                        settings = { Name: instanceDef.instance.settings.name, Sweeps: [] };
                        tcrn.Settings.push(settings);
                    }
                    var sweep: IV.ITraceDefinitionsSweep = null;
                    for (let i in settings.Sweeps)
                        if (settings.Sweeps[i].Name == instanceDef.instance.sweep) {
                            sweep = settings.Sweeps[i];
                        }
                    if (sweep == null) {
                        sweep = { Name: instanceDef.instance.sweep, Instances: [] };
                        settings.Sweeps.push(sweep);
                    }
                    var instance: IV.ITraceDefinitionsInstance = { ID: instanceDef.id, Name: instanceDef.instance.name, Plottables: plottables };
                    sweep.Instances.push(instance);

                    // Execute this after ALL of the instances have been handled.
                    if (++c == instanceDefs.length) {
                        traceDefinitions.onNext(traces);
                        traceDefinitions.onCompleted();
                    }
                    else
                        next();
                }).fail((error: any) => { traceDefinitions.onError(error); });
            };
            next();
        }, error => { }, () => { });

        observables.inferenceresults.subscribe(next => {
            var result: InferOperation.IPlottableValues = {
                NodeID: next.nodeId,
                Times: next.result.result.table.times,
                Values: next.result.result.table.columns.map(c => c.values),
                InstanceID: next.result.sim_id
            };
            simulationUpdates.onNext(result);
        },
            error => {
                console.error(JSON.stringify(error));
                simulationUpdates.onError(error);
            },
            () => simulationUpdates.onCompleted());

        observables.summary.subscribe(next => summary.onNext({ NodeID: next.nodeId, Summary: next.summary }),
            error => {
                console.error(JSON.stringify(error));
                summary.onError;
            },
            () => summary.onCompleted());

        return results;
    }

    public Abort(): void {
        this.me.Abort();
    }
}
