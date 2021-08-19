import * as Rx from 'rx';
import * as $ from 'jquery';
import * as Operations from "./LongOperations";
import * as crn from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as IV from "../../../../HTML5SharedGUI/InferenceViewer/Scripts/InferenceViewer";
import { WebSharperGeneratedInterfaces as WGI, WebSharperGeneratedInterfaces } from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces';
import CRNEngine = WGI.Microsoft.Research.CRNEngine;

export interface IRecentParamersValues {
    NodeID: string;
    values: number[];
    lglk: number;
    iteration: number;
}

export enum ParameterType { Log, Real }
export enum ParameterVariability { Random, Fixed }

export interface IParameterDefinition {
    Name: string;
    Type: ParameterType;
    LowerBound: number;
    UpperBound: number;
    Variability: ParameterVariability;
}

export interface IPlottableValues {
    NodeID: string;
    InstanceID: number;
    Times: number[];
    Values: number[][]; //outer array is species, inner array is species counts (matches length of Times)
}
export interface IPlotSettingsValues {
    NodeID: string;
    XLabel: string;
    YLabel: string;
    Title: string;
    LabelFontSize: number;
    TickFontSize: number;
    XTicks: number[];
    YTicks: number[];
    BurnInThin: number;
    MaxTime: number;
    XMin?: number;
    XMax?: number;
    YMin?: number;
    YMax?: number;
    VBoundaries: number[];
    HBoundaries: number[];
}
export interface IIteration {
    NodeID: string;
    Iteration: number;
}
export interface IInferenceRun {
    iteration: Rx.Observable<IIteration>;
    paramDefinitions: Rx.Observable<IV.IParameterDefinitions>;
    paramUpdates: Rx.Observable<IRecentParamersValues>;
    posteriorTableUpdates: Rx.Observable<IRecentParamersValues>;
    traceDefinitions: Rx.Observable<IV.ITraceDefinitions>;
    simulationUpdates: Rx.Observable<IPlottableValues>;
    plotSettingsUpdates: Rx.Observable<IPlotSettingsValues>;
    summary: Rx.Observable<IV.ISummary>;
    exports: Rx.Observable<crn.ExportDef>;
}

export interface IModelSource {
    getModel(): crn.IG;
}

export interface IInferenceEngine {
    InferParameters(crn: crn.IG): IInferenceRun;
    Abort(): void;
}

export interface IObservationsDef { name: string, plotsCount: number }

export interface IObservationsSource {
    GetData(sets: IObservationsDef[]): JQueryPromise<CRNEngine.Dataset[]>;
}

export interface IExportsViewer {
    ShowExport(update: crn.ExportDef): void;
}

function GetObservations(nodeId: string, data: crn.Table, instanceID: number): IPlottableValues {
    var results: IPlottableValues = {
        NodeID: nodeId,
        Times: data.times,
        Values: data.columns.map(column => column.values),
        InstanceID: instanceID
    };
    return results;
}

/** Inserts the observation values into the trace definitions that come from CRNEngine. This is all shifting references, so it should be fast enough. Note that the way observation traces are attached to simulation traces is fragile; it just assumes that each sim trace gets an observation trace. This ought to be rethought. */
function InjectObservations(run: IInferenceRun, modelToCrnToData: { [nodeId: string]: { [index: string]: CRNEngine.Dataset[] } }) {
    var res: IInferenceRun = {
        iteration: run.iteration,
        paramDefinitions: run.paramDefinitions,
        paramUpdates: run.paramUpdates,
        posteriorTableUpdates: run.posteriorTableUpdates,
        traceDefinitions: run.traceDefinitions.select(td => {
            let crnToData = modelToCrnToData[td.NodeID];
            var y = 0, z = 0;
            for (let crn of td.CRNs) {
                var obs = crnToData[crn.Name];
                var x = 0;
                for (let settings of crn.Settings)
                    for (let sweep of settings.Sweeps)
                        for (let instance of sweep.Instances)
                            for (let i = 0; i < instance.Plottables.length; i++) {
                                instance.Plottables[i].ObservationName = obs[x].data[y].columns[z].name;
                                instance.Plottables[i].ObservationCounts = obs[x].data[y].columns[z].values;
                                instance.Plottables[i].ObservationTimes = obs[x].data[y].times;
                                if (++z >= obs[x].data[y].columns.length) {
                                    z = 0;
                                    if (++y >= obs[x].data.length) {
                                        y = 0;
                                        x++;
                                    }
                                }
                            }
            }
            return td;
        }),
        simulationUpdates: run.simulationUpdates,
        plotSettingsUpdates: run.plotSettingsUpdates,
        summary: run.summary,
        exports: run.exports
    }

    return res;
}

function InjectPlotSettings(run: IInferenceRun, obs: IPlotSettingsValues) {
    var obsRx = Rx.Observable.return<IPlotSettingsValues>(obs);
    var res: IInferenceRun = {
        iteration: run.iteration,
        paramDefinitions: run.paramDefinitions,
        paramUpdates: run.paramUpdates,
        posteriorTableUpdates: run.posteriorTableUpdates,
        traceDefinitions: run.traceDefinitions,
        simulationUpdates: run.simulationUpdates,
        plotSettingsUpdates: obsRx.concat(run.plotSettingsUpdates),
        summary: run.summary,
        exports: run.exports
    }

    return res;
}

class PlottableCache {
    private cache: { [index: number]: IPlottableValues } = {};
    public Add(plottable: IPlottableValues) {
        this.cache[plottable.InstanceID] = plottable;
    }
    public GetAll() {
        var res: IPlottableValues[] = [];
        for (var idx in this.cache)
            res.push(this.cache[idx]);
        return res;
    }
}

/**
 * Long operation of parameter inference.
 * Takes the CRN object form ICRNSource, supplies it with data from IObseravationsSource, passes combination to IInferenceEngine, transmits the progress to IInferenceViewer
 */
export class Operation implements Operations.IOperation {
    private ongoingPromise: JQueryDeferred<void> = undefined;
    private exitMessage = "";
    /**
     * @param modelSource CRN entities with model specification
     * @param obsSource A source of table with observations. These observations are attached to CRN model so likelihood function can be constructed
     * @param engine The actual runner of inference operation.
     * @param viewer Represents the process of inference to the user
     * @param settings A source of inference settings
     */
    constructor(private modelSource: IModelSource, private obsSource: IObservationsSource, private engine: IInferenceEngine, private viewer: IV.IInferenceViewer, private exports: IExportsViewer, private plotSettingsExtractor: () => IPlotSettingsValues) {
    }

    //Operations.IOperation implementation
    public GetName() {
        return "Parameter inference";
    }

    public GetExitMessage() {
        return this.exitMessage;
    }

    public Initiate(): JQueryPromise<any> {
        this.ongoingPromise = jQuery.Deferred<void>();

        var ig = this.modelSource.getModel();

        // Load data for each CRN in each model.
        var getDataPromises: JQueryPromise<CRNEngine.Dataset[]>[] = [];
        var self = this;
        function loadData(crn: crn.CRN) {
            // Here, I need to figure out how to divide the columns in data files across instances (simulations). A data file with 12 columns could be used as sources for 4 instances with 3 plots each, or 12 instances with 1 plots each.
            var sets: IObservationsDef[] = [];
            var smap: { [idx: string]: IObservationsDef } = {};
            // First, generate the set of all data. These should all be in the base settings.data field. By default, I assume the base simulation plots as the column count for all datasets.
            var dataSetNames = crn.settings.data.map(d => d.file);
            for (var dataSetName of dataSetNames) {
                smap[dataSetName] = { name: dataSetName, plotsCount: crn.settings.simulation.plots.length };
                sets.push(smap[dataSetName]);
            }
            // Next, I'm overriding the columns count for datasets that are present in the specific simulations fields. They should use the specific simulation plots as the column count for all datasets used by that simulation.
            for (var sim of crn.settings.simulations) {
                for (var dsName of sim.data) {
                    if (smap[dsName] == null) {
                        // This should not happen, but if it does, I'm going to add the dataset to the list.
                        console.log("Warning: simulation " + sim.name + " contains dataset " + dsName + " which is not present in the CRN data list.");
                        smap[dsName] = { name: dsName, plotsCount: sim.plots.length };
                        sets.push(smap[dsName]);
                    }
                    else
                        smap[dsName].plotsCount = sim.plots.length;
                }
            }
            getDataPromises.push(self.obsSource.GetData(sets));
        }
        for (let nodeId in ig.nodes) {
            let model = ig.nodes[nodeId];
            loadData(model.top);
            for (let crn of model.systems)
                loadData(crn);
        }

        // Wait until all of the data has been loaded. This will result in the arguments array getting filled with the results, in the same order.
        jQuery.when.apply(jQuery, getDataPromises).done(function () {
            // Attach the data to each CRN object. Arguments is a special variable that contains the arguments of this function (in this case, the data sets).
            var c = 0;
            for (let nodeId in ig.nodes) {
                let model = ig.nodes[nodeId];
                model.top.settings.data = arguments[c];
                c++;
                for (let crn of model.systems) {
                    crn.settings.data = arguments[c];
                    c++;
                }
            }

            var inferenceRun = self.engine.InferParameters(ig);

            inferenceRun.exports.subscribe(exp => self.exports.ShowExport(exp), err => { }, () => { });

            var runWithPlotSettings: IInferenceRun = InjectPlotSettings(inferenceRun, self.plotSettingsExtractor());
            //converting IInferenceRun to IV.IInferenceRun
            var IVsimulationUpdates = new Rx.Subject<IV.IPlottableValues>();
            var IVProgress = new Rx.Subject<IV.IProgress>();
            var IvInferenceRun: IV.IInferenceRun = {
                progress: IVProgress,
                paramDefinitions: runWithPlotSettings.paramDefinitions,
                paramUpdates: runWithPlotSettings.paramUpdates,
                posteriorTableUpdates: runWithPlotSettings.posteriorTableUpdates,
                traceDefinitions: runWithPlotSettings.traceDefinitions,
                simulationUpdates: IVsimulationUpdates,
                plotSettingsUpdates: runWithPlotSettings.plotSettingsUpdates,
                summary: runWithPlotSettings.summary
            }
            self.viewer.show(IvInferenceRun);
            runWithPlotSettings.iteration.subscribe(i => {
                var progress = {
                    NodeID: i.NodeID,
                    CurrentIteration: i.Iteration,
                    BurnInLength: ig.nodes[i.NodeID].top.settings.inference.burnin,
                    SamplingLength: ig.nodes[i.NodeID].top.settings.inference.samples
                };
                IVProgress.onNext(progress);
            }, error => { }, () => { });

            runWithPlotSettings.simulationUpdates.subscribe(simulated => {
                IVsimulationUpdates.onNext({
                    NodeID: simulated.NodeID,
                    Times: simulated.Times,
                    Values: simulated.Values,
                    InstanceID: simulated.InstanceID
                });
            }, error => IVsimulationUpdates.onError(error), () => IVsimulationUpdates.onCompleted());

            //as all observables are either complete or produce error, subscribing to any of them
            inferenceRun.paramUpdates.subscribe((res) => { },
                error => {
                    self.exitMessage = error.message == null ? JSON.stringify(error) : error.message;
                    if (self.ongoingPromise) {
                        self.ongoingPromise.reject(error);
                        self.ongoingPromise = undefined;
                    }
                },
                () => {
                    self.exitMessage = "completed successfully";
                    if (self.ongoingPromise) {
                        self.ongoingPromise.resolve();
                        self.ongoingPromise = undefined;
                    }
                });

        }).fail((err: any) => {
            console.warn("Inference can't get data from the observations source: " + err);
            this.exitMessage = err;
            setTimeout(() => this.ongoingPromise.reject(err));
        });

        return this.ongoingPromise;
    }

    public Abort() {
        this.engine.Abort();
        if (this.ongoingPromise) {
            this.ongoingPromise.reject("Inference aborted");
            this.ongoingPromise = undefined;
        }
    }
}