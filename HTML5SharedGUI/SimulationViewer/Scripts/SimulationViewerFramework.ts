// (FP) The design of the SimulationViewer library is based on the notion that the outside world will send update messages to the library, and then the library will internally translate them and distribute them to the specific viewer (plotter, bar chart and table). This file implements the logic that allows distribution.

import * as Rx from "rx";
import * as I from "../../GenericComponents/Scripts/Interfaces";


// (FP) This is the type of updates coming from the outside world; it represents simulation results. MessageType is used to determine what type of object is contained in EncapsulatedUpdate. The following data structures are part of this.
export interface IVisualizationUpdateMessage {
    MessageType: VisualizationUpdateMessageType;
    EncapsulatedUpdate: ISimStepData | I1DSimStepData | I2DSimStepData | ISimTable | ISpatialSpace | I.ITraceDefinitions | I.IAdditionalPlottable | Units | PlotSettings | IProbabilitiesInfo;
}

export enum VisualizationUpdateMessageType { Reset, SimulationStep, SimulationStep1D, SimulationStep2D, SimulationTable, SpatialSpace, TraceDefinitions, AdditionalPlottable, UnitsInformation, PlotSettingsInfo, Probabilities }

export type Units = string;
// This is the type of plot settings, which represent directives from the model on how a chart should look like. They should be used when applicable.
export type PlotSettings = {
    XLabel: string;
    YLabel: string;
    Title: string;
    LabelFontSize: number;
    TickFontSize: number;
    XTicks: number[];
    YTicks: number[];
    MaxTime?: number;
    XMin?: number;
    XMax?: number;
    YMin?: number;
    YMax?: number;
    VBoundaries: number[];
    HBoundaries: number[];
};

export interface ISpatialSpace {
    XAxis: number[];
    YAxis?: number[];
}

export interface ISimStep {
    Instance: number;
    Time: number;
}

/** This is the type of one simulation step. */
export interface ISimStepData extends ISimStep {
    Counts: number[];
    // Lower/upper bounds, for simulations that support it.
    Bounds?: {
        lower: number;
        upper: number;
    }[];
}

export interface I1DSimStepData extends ISimStep {
    /** First index is on species, second index is on position. */
    Data: number[][];
}

export interface I2DSimStepData extends ISimStep {
    /** First index is on species, second and third indexes is on position. */
    Data: number[][][];
}

/** Type for simulations that emit entire time series tables at each step. */
export interface ISimTable {
    Instance: number;
    Time: number[];
    Counts: number[][];
    Bounds?: {
        lower: number;
        upper: number;
    }[][];
}

export interface IProbabilitiesInfo {
    InstanceName: string;
    Names: Array<string>;
    ProbabilitiesToken: any;
}

// (FP) This is the type of a function that transforms a generic simulation update into a view-specific update. The generic type parameter refers to the type of the view-spcific update.
export interface IVumToViewSpecificUpdate<T> {
    (vum: IVisualizationUpdateMessage): T;
}

// The default palette for components in the SimulationViewer library.
export var Palette: string[] = ["#FF0000", "#00AA00", "#0000FF", "#FFA500", "#FF00FF", "#00FFFF", "#551A8B", "#F4A460", "A9A9A9", "D3D3D3", "#FF00FF", "#000000", "#DA70D6", "#7CFC00", "#B0C4DE", "#E9967A", "#008080", "#BC8F8F", "#FFDEAD", "#808000", "#000080"];

export interface ISpatialViewerSettings {
    // This is an idd color palette.
    colorPalette: KnockoutObservable<any>;
    isLogPalette: KnockoutObservable<boolean>;
}

//Handles reset messages, scatteres the updates across registered views transforming the VUMs to the view specific update form
// (FP) The way this works is as follows. The higher layer calls AddView, passing a transform and an observer. The observer is subscribed to dataChannel, which runs the transform on it, plus some filtering, and puts the transformed and filtered  observer into an array of observers called dataObservers. When the higher layer posts a message, it gets routed to everyone in dataObservers.
// Design note: I have the feeling that there should be a way to do this in Rx alone.
export class ViewCollection {
    protected views: Array<Rx.IObserver<IVisualizationUpdateMessage>> = [];
    private dataObservers: Array<Rx.Observer<IVisualizationUpdateMessage>> = [];
    private dataChannel = Rx.Observable.create<IVisualizationUpdateMessage>((observer) => {
        // (FP) This happens whenever someone subscribes to dataChannel.
        this.dataObservers.push(observer);
    });
    protected AddView(view: Rx.IObserver<IVisualizationUpdateMessage>) {
        var capturedView = view;

        this.dataChannel.subscribe(<any>view);
        //propagating view specific updates to the view
        this.views.push(capturedView);
    }

    // (FP) This is what the higher layers call to send an update.
    public Post(update: IVisualizationUpdateMessage) {
        this.dataObservers.forEach((observer) => {
            observer.onNext(update);
        });
    }
}

//extracts information needed for InstanceFilters operation from the update message stream. Dirvert this info to the InstanceFilters
// (FP) This is an extension of the class above that adds handling of species and instance lists. When that information arrives, it causes invokation of SetAvailableInstances and/or SetAvailableNames. Note that this information will typically arrive at the beginning of a simulation (species may also arrive in the middle of a simulation).
export class FilteredViewCollection extends ViewCollection {
    protected TracesFilters: Array<I.ITracesFilter> = [];

    public RegisterTracesFilter(filter: I.ITracesFilter) {
        this.TracesFilters.push(filter);
    }

    public Post(update: IVisualizationUpdateMessage) {
        // Look at some of the messages and act on them.
        switch (update.MessageType) {
            case VisualizationUpdateMessageType.Reset:
                this.TracesFilters.forEach(f => { f.SetAvailableTraces({ ModelName: "", CRNs: [] }); });
                break;
            case VisualizationUpdateMessageType.TraceDefinitions:
                var definitions: I.ITraceDefinitions = <I.ITraceDefinitions>update.EncapsulatedUpdate;
                this.TracesFilters.forEach(f => f.SetAvailableTraces(definitions));
                break;
            case VisualizationUpdateMessageType.AdditionalPlottable:
                var newPlottable: I.IAdditionalPlottable = <I.IAdditionalPlottable>update.EncapsulatedUpdate;
                this.TracesFilters.forEach(f => { f.AddSpeciesToInstance(newPlottable); });
                break;
            default: //other message types are not interesting for this component
                break;
        }
        // Propagate all of the messages.
        super.Post(update);
    }
}