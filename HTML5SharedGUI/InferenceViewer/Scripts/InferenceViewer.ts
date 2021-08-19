// (FP) This file does not contain any control; it only has some interfaces. These interfaces describe the shape of inference data, as expected by the various inference viewer controls contained in the library.

import * as Rx from "rx";
import * as ko from "knockout";

export interface IRecentParametersValues {
    NodeID: string;
    values: number[];
    lglk: number;
    iteration: number;
}

export enum ParameterType { Log, Real }
export enum ParameterVariability { Random, Fixed }

export interface INodeSelector {
    SelectedNodeID: KnockoutObservable<string>;
}

export interface IParameterDefinition {
    Name: string;
    Type: ParameterType;
    LowerBound: number;
    UpperBound: number;
    Variability: ParameterVariability;
}

export interface IParameterDefinitions {
    NodeID: string;
    Definitions: IParameterDefinition[];
}

export interface IProgress {
    NodeID: string;
    CurrentIteration: number;
    BurnInLength: number;
    SamplingLength: number;
}

export interface ITraceDefinitionsPlottable {
    Name: string;
    /** The structural string, if available. */
    Structural?: string;
    /** The name of the observations column, if available. */
    ObservationName?: string;
    /** The observation counts, if available. */
    ObservationCounts?: number[];
    /** The observation times, if available. */
    ObservationTimes?: number[];
    /** The colour for this trace, if available. */
    Colour?: string;
}

export interface ITraceDefinitionsInstance {
    /** The instance ID. This is unique across the entire model, and will identify simulation steps. */
    ID: number;
    /** The visible name of the instance. */
    Name: string;
    /** The plottables for this instance. */
    Plottables: ITraceDefinitionsPlottable[];
}

export interface ITraceDefinitionsSweep {
    Name: string;
    Instances: ITraceDefinitionsInstance[];
}

export interface ITraceDefinitionsSetting {
    Name: string;
    /** A setting can contain multiple sweeps. Note that *usually* all instances of a given setting will share the same plottables, except when running multiple instances in JIT. In that case, each instance will start out with the same plottables, but new plottables may appear independently. */
    Sweeps: ITraceDefinitionsSweep[];
}

export interface ITraceDefinitionsCRN {
    Name: string;
    /** A CRN can contain multiple settings. Each setting contains one or more instances. */
    Settings: ITraceDefinitionsSetting[];
}

/** This is the type of the definition of the traces that one simulation will produce. */
export interface ITraceDefinitions {
    NodeID: string;
    /** A model can contain multiple CRNs. Each CRN is entirely independent, from the point of view of traces. */
    CRNs: ITraceDefinitionsCRN[];
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
    XMin?: number;
    XMax?: number;
    YMin?: number;
    YMax?: number;
    VBoundaries: number[];
    HBoundaries: number[];
}

export interface ISummary {
    NodeID: string;
    Summary: string;
}

// (FP) This interface represents an inference run. It's a collection of observables, each of which provides a different type of data (inference generates many heterogenous types of data). The content and overall design is similar to the inference observables produced by CRNEngineTSWrapper, but the shape here is somewhat different. The user program will need an adapter.
export interface IInferenceRun {
    progress: Rx.Observable<IProgress>;
    paramDefinitions: Rx.Observable<IParameterDefinitions>;
    paramUpdates: Rx.Observable<IRecentParametersValues>;
    posteriorTableUpdates: Rx.Observable<IRecentParametersValues>;
    traceDefinitions: Rx.Observable<ITraceDefinitions>;
    simulationUpdates: Rx.Observable<IPlottableValues>;
    plotSettingsUpdates: Rx.Observable<IPlotSettingsValues>;
    summary: Rx.Observable<ISummary>;
}

// (FP) This interface represents a control that can make use of inference data. All the controls in this library implement it.
export interface IInferenceViewer {
    show(run: IInferenceRun): void;
}