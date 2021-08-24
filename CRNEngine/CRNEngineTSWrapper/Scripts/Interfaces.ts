// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
export type CRN = WGI.Microsoft.Research.CRNEngine.Gui;
export type Model = WGI.Microsoft.Research.CRNEngine.GuiModel;
export type IG = WGI.Microsoft.Research.CRNEngine.GuiIG;
export type Species = WGI.Microsoft.Research.CRNEngine.Species;
export type SpeciesAttributes = WGI.Microsoft.Research.CRNEngine.Attributes;
export type Initial = WGI.Microsoft.Research.CRNEngine.Initial<string, string>;
export type Settings = WGI.Microsoft.Research.CRNEngine.Crn_settings<string>;
export type ExportDef = WGI.Microsoft.Research.CRNEngine.JSAPI.export_def;
export type Expression = WGI.Microsoft.Research.CRNEngine.Expression.t<string>;
export type MultiSetElement = WGI.Microsoft.Research.CRNEngine.Mset.entry<string>;
export type Rate = WGI.Microsoft.Research.CRNEngine.Rate<string, string>;
export type Reaction = WGI.Microsoft.Research.CRNEngine.Reaction<string, string, string>;
export type Sweep = WGI.Microsoft.Research.CRNEngine.Sweep;
export type SimRun = WGI.Microsoft.Research.CRNEngine.JSAPI.sim_run;
export type Population = { [key: string]: number };
export type Result = WGI.Microsoft.Research.CRNEngine.Result<number>;
export type State = WGI.Microsoft.Research.CRNEngine.JSAPI.state;
export type StateSpace = WGI.Microsoft.Research.CRNEngine.JSAPI.state_space;
export type SimResultType = WGI.Microsoft.Research.CRNEngine.JSAPI.ValueType;
export type SimSettings = WGI.Microsoft.Research.CRNEngine.Simulation_settings<string>;
export type Plottable = Expression;
export type MCSettings = WGI.Microsoft.Research.CRNEngine.Moment_closure_settings.t<WGI.Microsoft.Research.CRNEngine.Expression.t<Array<WGI.Opaque.FSharpTuple>>>
export type MCPlottable = WGI.Microsoft.Research.CRNEngine.Expression.t<Array<WGI.Opaque.FSharpTuple>>;
export type SynthesisSettings = WGI.Microsoft.Research.CRNEngine.Synthesis_settings;
export type SimResultMeanStdev = WGI.Microsoft.Research.CRNEngine.Point;
export type SimResultValue = number | SimResultMeanStdev | number[] | number[][];
export type InferenceSettings = WGI.Microsoft.Research.CRNEngine.Inference_settings;
export type EvaluatedValues = WGI.Microsoft.Research.CRNEngine.JSAPI.inference_evaluated_values;
export type Column = WGI.Microsoft.Research.CRNEngine.Column<number>;
export type Table = WGI.Microsoft.Research.CRNEngine.Table<number>;
export type Dataset = WGI.Microsoft.Research.CRNEngine.Dataset;
export type PriorInterval = WGI.Microsoft.Research.CRNEngine.Interval;
export type PriorVariation = WGI.Microsoft.Research.CRNEngine.Variation;
export type InferenceParameter = WGI.Microsoft.Research.CRNEngine.JSAPI.InferenceParameter;
export type InferenceParameters = WGI.Microsoft.Research.CRNEngine.JSAPI.InferenceParameters;
export type InferenceParameterType = WGI.Microsoft.Research.CRNEngine.JSAPI.InferenceParameterType;
export type InferenceResult = { sim_id: number, result: Result }
export type SimulatorType = WGI.Microsoft.Research.CRNEngine.Simulator;
export type Kinetics = WGI.Microsoft.Research.CRNEngine.Kinetics;
export type Boundary = WGI.Microsoft.Research.CRNEngine.Boundary;
export type Parameter = WGI.Microsoft.Research.CRNEngine.Parameter;
export type Probabilities = WGI.Microsoft.Research.CRNEngine.Probabilities;
export type ProbabilityMap = WGI.Microsoft.Research.CRNEngine.JSAPI.probabilityMap;
export type SpatialInitial = WGI.Microsoft.Research.CRNEngine.Spatial_initial.t;
export type Functional = WGI.Microsoft.Research.CRNEngine.Expression.t<WGI.Microsoft.Research.CRNEngine.Key<WGI.Microsoft.Research.CRNEngine.Species>>;
export type SynthesisResult = WGI.Microsoft.Research.CRNEngine.JSAPI.SynthesisResult;
export type SynthesisValue = WGI.Microsoft.Research.CRNEngine.JSAPI.SynthesisValue;
export type BistabilityPlot = WGI.Microsoft.Research.CRNEngine.JSAPI.BistabilityPlot;

export interface CloudCapabilities {
    account: string;
    pools: string[];
}

export interface ParsingErrorPosition {
    row: number;
    column: number;
    text: string;
}

export interface Error {
    message: string;
    extra?: any;
}

export interface ParsingError extends Error {
    positions: ParsingErrorPosition[];
}

export type SimulationInstance = WGI.Microsoft.Research.CRNEngine.Instance<string>;
export type SimulationInstanceDefinition = { id: number, instance: SimulationInstance };
export type SimResult<T> = { instance: number, time: number, values: T[] };
export type SimTable<T> = { instance: number, time: number[], values: T[][] };
export type NewPlottable = WGI.Microsoft.Research.CRNEngine.Jit.newplottable;
export type NewPlottableDefinition = { instance: number, plottable: NewPlottable };

export type TimeUnit = WGI.Microsoft.Research.CRNEngine.Time;
export type SpaceUnit = WGI.Microsoft.Research.CRNEngine.Space;
export type ConcentrationUnit = WGI.Microsoft.Research.CRNEngine.Concentration;

export type SimRunDefinition = { simtype: SimResultType, values: Rx.Observable<SimResult<SimResultValue>>, tables: Rx.Observable<SimTable<SimResultValue>>, newplottables: Rx.Observable<NewPlottableDefinition> };
export type InstanceProbabilities = { instance: SimulationInstance, probabilities: Probabilities };

export type SimulatorObservables = {
    node: Rx.Observable<Model>,
    exports: Rx.Observable<ExportDef>,
    instances: Rx.Observable<SimulationInstanceDefinition[]>,
    statespace: Rx.Observable<StateSpace>,
    probabilities: Rx.Observable<InstanceProbabilities>,
    simrun: Rx.Observable<SimRunDefinition>,
}
export type InferenceObservables = {
    exports: Rx.Observable<ExportDef>,
    instances: Rx.Observable<{ nodeId: string; definitions: SimulationInstanceDefinition[] }>,
    inferencechainupdate: Rx.Observable<{ nodeId: string; values: EvaluatedValues }>,
    inferenceresults: Rx.Observable<{ nodeId: string; result: InferenceResult }>,
    parameterdefinitions: Rx.Observable<{ nodeId: string; parameters: InferenceParameter[] }>,
    parameterresults: Rx.Observable<{ nodeId: string; values: EvaluatedValues }>,
    summary: Rx.Observable<{ nodeId: string; summary: string }>,
    progress: Rx.Observable<{ nodeId: string; iteration: number }>
}
export type ParseCodeObservables = { model: Rx.Observable<IG> }
export type ExportObservables = { exports: Rx.Observable<ExportDef> }

export type JobState = "Waiting" | "Active" | "Completed" | "NoJob";
export type JobFile = { name: string; uri: string }
export type JobDescriptor = { id: string; state: JobState; verb: string; start: string; zipFile?: JobFile; files: JobFile[] }