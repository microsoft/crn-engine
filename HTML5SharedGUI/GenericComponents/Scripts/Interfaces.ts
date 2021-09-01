// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//bind to specific DOM element and occupy it
export interface IUIBindable {
    Bind(elem: HTMLElement): void;
}

//bind to the DOM, but automatically finds out a place to bind (e.g. by predefined element id)
export interface IUIAutoBindable {
    AutoBind(): void;
}


// Interface for a component that allows the user to select traces.
export interface ITracesFilter {
    SetAvailableTraces(data: ITraceDefinitions): void;
    AddSpeciesToInstance(data: IAdditionalPlottable): void;
}


export interface ITraceDefinitionsPlottable {
    Name: string;
    /** The structural string, if available. */
    Structural?: string;
    /** The name of the observations set for this trace, if available. */
    ObservationName?: string;
    /** The observation values for this trace, if available. */
    ObservationCounts?: number[];
    /** The observation times for this trace, if available. */
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
    /** The maximum expected simulation time for this instance. */
    MaxTime: number;
    /** The number of expected trajectories for this instance (usually 1). */
    MaxTrajectories: number;
}

export interface ITraceDefinitionsSweep {
    Name: string;
    /** A sweep can contain multiple instnaces. */
    Instances: ITraceDefinitionsInstance[];
}

export interface ITraceDefinitionsSetting {
    Name: string;
    /** A setting can contain multiple sweeps. Note that *usually* all sweeps of a given setting will share the same plottables, except when running multiple instances in JIT. In that case, each instance will start out with the same plottables, but new plottables may appear independently. */
    Sweeps: ITraceDefinitionsSweep[];
}

export interface ITraceDefinitionsCRN {
    Name: string;
    /** A CRN can contain multiple settings. Each setting contains one or more instances. */
    Settings: ITraceDefinitionsSetting[];
}

/** This is the type of the definition of the traces that one simulation will produce. */
export interface ITraceDefinitions {
    ModelName: string;
    /** A model can contain multiple CRNs. Each CRN is entirely independent, from the point of view of traces. */
    CRNs: ITraceDefinitionsCRN[];
}

/** This is the type of an additional plottable that appears in an instance during JIT simulation. */
export interface IAdditionalPlottable {
    Instance: number;
    Name: string;
    Structural?: string;
}

