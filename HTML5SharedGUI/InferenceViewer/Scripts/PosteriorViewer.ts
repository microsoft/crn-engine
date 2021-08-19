// (FP) This file implements the posterior viewer. Like the simulation viewer, this actually encompasses three controls (the posterior table, the scatter plot and the density plot). Unlike the simulation viewer, here we don't have a template that organizes them in tabs; this is left to the higher layers. The control, however, will assume that all of the separate viewers are child of a given node (they can be organized in any way within that node). Note that there are three VMs, one for each type of control, but only one View class. Each of the VMs contains its own progress control, but they'll all bind to the same data.

import "../../KnockoutGrid/knockoutgrid";
import * as ko from "knockout";
import * as $ from "jquery";
import "jquery-mousewheel";
import "idd";
import "jqueryui";
import GetExporter from "../../GenericComponents/Scripts/SVGExporter";
declare var InteractiveDataDisplay: any;
import * as Papa from "papaparse";
import { saveAs } from "file-saver";
import * as Inference from "./InferenceViewer";
import { InferenceProgress } from "./InferenceProgress";

import * as posteriorTableViewerTemplate from 'raw-loader!../html/posterior-table-viewer.html';
ko.components.register("posterior-table-viewer", {
    template: posteriorTableViewerTemplate,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            if (componentInfo.element == null)
                throw "Attempt to get a PosteriorTable VM for null";
            return ko.contextFor(componentInfo.element).$data;
        }
    }
});
import * as scatterTemplate from 'raw-loader!../html/scatter-plot.html';
ko.components.register("posterior-scatter-plot", {
    template: scatterTemplate,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            if (componentInfo.element == null)
                throw "Attempt to get a PosteriorScatterPlot VM for null";
            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $(componentInfo.element)));
            var gestureSource = InteractiveDataDisplay.Gestures.getGesturesStream(plot.centralPart);
            var bottomAxisGestures = InteractiveDataDisplay.Gestures.applyHorizontalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-sp__bottomaxis", $(componentInfo.element))));
            var leftAxisGestures = InteractiveDataDisplay.Gestures.applyVerticalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-sp__leftaxis", $(componentInfo.element))));
            plot.navigation.gestureSource = gestureSource.merge(bottomAxisGestures.merge(leftAxisGestures));
            return ko.contextFor(componentInfo.element).$data;
        }
    },
});
import * as densityTemplate from 'raw-loader!../html/density-plot.html';
ko.components.register("density-plot", {
    template: densityTemplate,
    viewModel: {
        createViewModel: (params, componentInfo) => {
            if (componentInfo.element == null)
                throw "Attempt to get a PosteriorDensity VM for null";
            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $(componentInfo.element)));
            var gestureSource = InteractiveDataDisplay.Gestures.getGesturesStream(plot.centralPart);
            var bottomAxisGestures = InteractiveDataDisplay.Gestures.applyHorizontalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-dp__bottomaxis", $(componentInfo.element))));
            var leftAxisGestures = InteractiveDataDisplay.Gestures.applyVerticalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-dp__leftaxis", $(componentInfo.element))));
            plot.navigation.gestureSource = gestureSource.merge(bottomAxisGestures.merge(leftAxisGestures));

            return ko.contextFor(componentInfo.element).$data;
        }
    },
});

// (FP) This class is the VM for the posterior table. This is based on a table-viewer control from KnockoutGrid.
class PosteriorTableVM {
    Progress = new InferenceProgress(this.nodeSelector);

    data = ko.pureComputed<Inference.IRecentParametersValues[]>(() => {
        var curr = this.dataStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr();
    })
    names = ko.pureComputed(() => {
        var curr = this.namesStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr;
    })

    // (FP) A function that KO can bind a button to, that saves the current data as CSV.
    Save() {
        var data = this.data();
        var names = this.names();
        names.unshift("Iteration", "LogLk");
        var csv = Papa.unparse({
            data: data.map(val => {
                var arr = new Array();
                arr.push(val.iteration);
                arr.push(val.lglk);
                val.values.forEach(v => arr.push(v));
                return arr;
            }),
            fields: names
        },
            {
                quotes: false,
                delimiter: ",",
                newline: "\r\n"
            });
        var blob = new Blob([csv], { type: "text/csv" });
        saveAs(blob, "SimulationResult.csv");
    }

    // (FP) The data is passed to the VM in the constructor; it's in the form of observables, so it will be changed by the caller.
    constructor(private nodeSelector: Inference.INodeSelector,
        private dataStorage: KnockoutObservable<{ [nodeId: string]: KnockoutObservableArray<Inference.IRecentParametersValues> }>,
        private namesStorage: KnockoutObservable<{ [nodeId: string]: string[] }>) { }

    // (FP) The configuration object for the table-viewer control.
    posteriorConfig: any = {
        data: this.data,
        headerTemplate: 'posterior-table-header',
        columnTemplate: 'posterior-table-template',
        ViewModel: this
    };
}

export type IddAxisLabels = {
    type: string;
    attachGrid: boolean;
    ticks?: Array<number>;
    labels?: Array<string>;
    rotate?: boolean;
    fontSize?: number;
};
// (FP) This class is the VM for the scatter plot.
class ScatterPlotVM {
    Progress = new InferenceProgress(this.nodeSelector);

    // (FP) This computed is the set of selectable parameters. That's the set of parameters, plus the iteration and the log likelihood.
    options = ko.pureComputed(() => {
        return ['Iteration', 'Likelihood'].concat(this.names());
    });

    // (FP) These observables contain the selected parameters for the scatter plot.
    selectedX = ko.observable<string>('Iteration');
    selectedY = ko.observable<string>('Likelihood');


    data = ko.pureComputed<Inference.IRecentParametersValues[]>(() => {
        var curr = this.dataStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr();
    })

    burninData = ko.pureComputed<Inference.IRecentParametersValues[]>(() => {
        var curr = this.burninDataStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr();
    })

    names = ko.pureComputed(() => {
        var curr = this.namesStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr;
    })

    lowerParameterBounds = ko.pureComputed(() => {
        var curr = this.lowerParameterBoundsStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr;
    })

    upperParameterBounds = ko.pureComputed(() => {
        var curr = this.upperParameterBoundsStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr;
    })

    /// coresponds to two vertical lines
    xParameterBounds = ko.pureComputed(() => {
        var idx = this.names().indexOf(this.selectedX());
        if (idx === -1) { //not a parameter, possible logLikelihood or Iteration
            return []; //empty array means that the parameter bounds are not relevant and are not subject to be drawn            
        } else {
            var lower = this.lowerParameterBounds()[idx];
            var upper = this.upperParameterBounds()[idx];
            return [lower, upper];
        }
    });
    /// coresponds to two horizontal lines
    yParameterBounds = ko.pureComputed(() => {
        var idx = this.names().indexOf(this.selectedY());
        if (idx === -1) { //not a parameter, possible logLikelihood or Iteration
            return []; //empty array means that the parameter bounds are not relevant and are not subject to be drawn            
        } else {
            var lower = this.lowerParameterBounds()[idx];
            var upper = this.upperParameterBounds()[idx];
            return [lower, upper];
        }
    });

    // X series of polyline that depict the paramter bounds
    // It is either rectangular or two parallel lines or empty
    boundsX = ko.computed(() => {
        var xBounds = this.xParameterBounds();
        var yBounds = this.yParameterBounds();
        var max = Number.MAX_VALUE * 1e-20;
        var min = -max;
        if (xBounds.length == 0 && yBounds.length == 0) { //there are no bounds at all
            return [];
        } else if ((xBounds.length > 0) && (yBounds.length == 0)) { //only vertical lines (horizontal parameter bounds) are relevant
            return [xBounds[0], xBounds[0], NaN, xBounds[1], xBounds[1]];
        } else if (xBounds.length == 0 && yBounds.length > 0) { //only horizaontal lines (vertical parameter bounds) are relevant
            return [min, max, NaN, min, max];
        } else { //rectangular bounds (both horizontal and vertical  parameter bounds are relevant
            return [xBounds[0], xBounds[0], xBounds[1], xBounds[1], xBounds[0]];
        }
    });

    // Y series of polyline that depict the paramter bounds
    // It is either rectangular or two parallel lines or empty
    boundsY = ko.computed(() => {
        var xBounds = this.xParameterBounds();
        var yBounds = this.yParameterBounds();
        var max = Number.MAX_VALUE * 1e-20;
        var min = -max;
        if (xBounds.length == 0 && yBounds.length == 0) { //there are no bounds at all
            return [];
        } else if (xBounds.length > 0 && yBounds.length == 0) { //only vertical lines (horizontal parameter bounds) are relevant
            return [min, max, NaN, min, max];
        } else if (xBounds.length == 0 && yBounds.length > 0) { //only horizaontal lines (vertical parameter bounds) are relevant
            return [yBounds[0], yBounds[0], NaN, yBounds[1], yBounds[1]];
        } else { //rectangular bounds (both horizontal and vertical  parameter bounds are relevant
            return [yBounds[0], yBounds[1], yBounds[1], yBounds[0], yBounds[0]];
        }
    });

    // whether the log transform is applied to X axis
    logX = ko.observable<boolean>(false);
    // whether the log transform applicable (makes sense) with respect to the semantics of selectedX
    logXavailable = ko.pureComputed(() => this.names().indexOf(this.selectedX()) !== -1);

    // whether the log transform is applied to Y axis
    logY = ko.observable<boolean>(false);
    // whether the log transform applicable (makes sense) with respect to the semantics of selectedY
    logYavailable = ko.pureComputed(() => this.names().indexOf(this.selectedY()) !== -1);

    labelSize = ko.observable<string>('16px');
    XTicks = ko.observable<IddAxisLabels>({
        type: "numeric",
        attachGrid: true,
        fontSize: 16
    });
    YTicks = ko.observable<IddAxisLabels>({
        type: "numeric",
        attachGrid: true,
        rotate: true,
        fontSize: 16
    });

    public onFrameRendered: () => void = () => { }; //to be set externally
    public EnableAutoFit: () => void = () => { }; // to be set externally

    ShowBurnIn = ko.observable<boolean>(false);

    // (FP) This computed produces the X values; it basically looks at the selected parameter for the X axis, and returns the relevant data. Note that it will always depend on selectedX and on data, so it will get updated whenever the user changes the selected X parameter, and whenever the data changes.
    currentPlotX = ko.pureComputed(() => {
        var X: number[];
        switch (this.selectedX()) {
            case 'Iteration':
                X = this.data().map(val => val.iteration);
                break;
            case 'Likelihood':
                X = this.data().map(val => val.lglk);
                break;
            default:
                X = this.data().map(val => val.values[this.names().indexOf(this.selectedX())]);
        }
        return X;
    });

    // (FP) Same thing, for the Y values.
    currentPlotY = ko.pureComputed(() => {
        var Y: number[];
        switch (this.selectedY()) {
            case 'Iteration':
                Y = this.data().map(val => val.iteration);
                break;
            case 'Likelihood':
                Y = this.data().map(val => val.lglk);
                break;
            default:
                Y = this.data().map(val => val.values[this.names().indexOf(this.selectedY())]);
        }
        return Y;
    });

    // (FP) This computed produces the X values; it basically looks at the selected parameter for the X axis, and returns the relevant data. Note that it will always depend on selectedX and on data, so it will get updated whenever the user changes the selected X parameter, and whenever the data changes.
    currentPlotBurnInX = ko.pureComputed(() => {
        if (this.ShowBurnIn()) {
            var X: number[];
            switch (this.selectedX()) {
                case 'Iteration':
                    X = this.burninData().map(val => val.iteration);
                    break;
                case 'Likelihood':
                    X = this.burninData().map(val => val.lglk);
                    break;
                default:
                    X = this.burninData().map(val => val.values[this.names().indexOf(this.selectedX())]);
            }
            return X;
        } else return [];
    });

    // (FP) Same thing, for the Y values.
    currentPlotBurnInY = ko.pureComputed(() => {
        if (this.ShowBurnIn()) {
            var Y: number[];
            switch (this.selectedY()) {
                case 'Iteration':
                    Y = this.burninData().map(val => val.iteration);
                    break;
                case 'Likelihood':
                    Y = this.burninData().map(val => val.lglk);
                    break;
                default:
                    Y = this.burninData().map(val => val.values[this.names().indexOf(this.selectedY())]);
            }
            return Y;
        } else {
            return [];
        }
    });

    public CaptureSVG: () => void;

    // (FP) The data is passed to the VM in the constructor; it's in the form of observables, so it will be changed by the caller.
    constructor(private nodeSelector: Inference.INodeSelector,
        private dataStorage: KnockoutObservable<{ [nodeId: string]: KnockoutObservableArray<Inference.IRecentParametersValues> }>,
        private burninDataStorage: KnockoutObservable<{ [nodeId: string]: KnockoutObservableArray<Inference.IRecentParametersValues> }>,
        private namesStorage: KnockoutObservable<{ [nodeId: string]: string[] }>,
        private lowerParameterBoundsStorage: KnockoutObservable<{ [nodeId: string]: number[] }>,
        private upperParameterBoundsStorage: KnockoutObservable<{ [nodeId: string]: number[] }>) {

        this.logXavailable.subscribe((value) => {
            if (!value)
                this.logX(false);
        });

        this.logYavailable.subscribe((value) => {
            if (!value)
                this.logY(false);
        });
    }

    onSelectedXChanged() {
        this.EnableAutoFit();
    };

    onSelectedYChanged() {
        this.EnableAutoFit();
    };
}

function makeHistogram(bands: number, values: number[]): number[][] {
    if (values == null || values.length == 0)
        return [[], []];

    var min = values[0], max = values[0];
    values.forEach(v => {
        if (v < min) min = v;
        if (v > max) max = v;
    });

    if (min == max) {
        return [[], []];
    }

    var histV: number[] = [], histC: number[] = [];
    let bandwidth = (max - min) / bands;
    for (let c = 0; c < bands; c++) {
        histV.push(bandwidth * c + min + bandwidth / 2); // LM: if function should return coord-s of a band center
        histC.push(0);
    }
    for (let v of values) {
        let idx = Math.floor((v - min) / bandwidth);
        if (idx < 0) idx = 0;
        if (idx > bands - 1) idx = bands - 1;
        histC[idx]++;
    }
    return [histV, histC];
}

// (FP) This class is the VM for the density plot.
class DensityPlotVM {
    // (FP) This observable stores the currently selected parameter.
    selected = ko.observable("");

    // positions of vertical lines, that indecate parameter bounds
    parameterLower = ko.pureComputed(() => {
        var idx = this.names().indexOf(this.selected());
        this.lowerParameterBounds.peek();
        if (idx !== -1)
            return this.lowerParameterBounds()[idx];
        else
            return NaN;
    });

    parameterUpper = ko.pureComputed(() => {
        var idx = this.names().indexOf(this.selected());
        this.upperParameterBounds.peek();
        if (idx !== -1)
            return this.upperParameterBounds()[idx];
        else
            return NaN;
    });


    Progress = new InferenceProgress(this.nodeSelector);

    XTicks = ko.observable<IddAxisLabels>({
        type: "numeric",
        attachGrid: true,
        fontSize: 16
    });
    YTicks = ko.observable<IddAxisLabels>({
        type: "numeric",
        attachGrid: true,
        rotate: true,
        fontSize: 16
    });

    public EnableAutoFit: () => void = () => { }; // to be set externally

    // The array of the values of the currently selected parameter.
    Values = ko.pureComputed<number[]>(() => {
        let i = this.names().indexOf(this.selected());
        if (i === -1 || this.data().length === 0) return [];
        var l = this.data().map(val => val.values[i]);
        return l;
    });

    DataTransformed = ko.pureComputed<number[]>(() => {
        var values = this.Values();
        if (this.logX()) {
            return values.map(val => Math.log(val) / Math.LN10);
        }
        else {
            return values;
        }
    });

    // (FP) This computed observable returns the aggregate data, and it depends on the selected parameter as well as the data. The HTML bindings do not bind to this directly, but they bind to the following computed observables that in turn depend on this one. Note that this is called Data (capital D) and is different from data (the one from the constructor). This one performs statistics on the source data.
    Data = ko.pureComputed<number[][]>(() => {
        var values = this.DataTransformed();
        var bands = this.Bands();
        return makeHistogram(bands, values);
    });

    DataBackTransformed = ko.pureComputed(() => {
        var data = this.Data();
        if (this.logX()) {
            return [data[0].map(val => Math.pow(10, val)), data[1]];
        }
        else {
            return data;
        }
    });

    AvailableBands = ko.observable([4, 8, 16, 32, 64, 128, 256, 512]);
    Bands = ko.observable<number>(32);

    //UseKDE = ko.observable<boolean>(false);

    // The width (in model units) of the bars, for histograms in plot coordinates.
    BarWidth = ko.pureComputed(() => {
        var vs = this.DataBackTransformed()[0];

        if (this.logX()) {
            if (vs.length === 0)
                return 0;
            else
                if (vs.length === 1)
                    return Math.max(10e-8, Math.abs(vs[0] / 10));
            var histPlotLength = (Math.log(vs[vs.length - 1]) - Math.log(vs[0])) / Math.LN10;
            return histPlotLength / (vs.length - 1);
        }
        else {
            if (vs.length === 0)
                return 0;
            return vs[1] - vs[0];
        }
    });

    HistX = ko.pureComputed(() => {
        //if (this.UseKDE())
        //return [];
        return this.DataBackTransformed()[0];
    });
    HistY = ko.pureComputed(() => {
        //if (this.UseKDE())
        //return [];
        return this.DataBackTransformed()[1];
    });

    //the following will be used to show vertical lines of parameter bounds

    ParameterLowerX = ko.pureComputed(() => {
        let v = this.parameterLower();
        return [v, v];
    });

    ParameterUpperX = ko.pureComputed(() => {
        let v = this.parameterUpper();
        return [v, v];
    });

    ParameterBoundY = [0, 1000]; //intentionaly high values here, these vertical lines are not accounted in IDD autoFit plot rect calculation

    public CaptureSVG: () => void;

    data = ko.pureComputed<Inference.IRecentParametersValues[]>(() => {
        var curr = this.dataStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr();
    })

    names = ko.pureComputed(() => {
        var curr = this.namesStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr;
    })

    lowerParameterBounds = ko.pureComputed(() => {
        var curr = this.lowerParameterBoundsStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr;
    })

    upperParameterBounds = ko.pureComputed(() => {
        var curr = this.upperParameterBoundsStorage()[this.nodeSelector.SelectedNodeID()];
        if (curr == null)
            return [];
        return curr;
    })

    // (FP) The data is passed to the VM in the constructor; it's in the form of observables, so it will be changed by the caller.
    constructor(private nodeSelector: Inference.INodeSelector,
        private dataStorage: KnockoutObservable<{ [nodeId: string]: KnockoutObservableArray<Inference.IRecentParametersValues> }>,
        private namesStorage: KnockoutObservable<{ [nodeId: string]: string[] }>,
        private lowerParameterBoundsStorage: KnockoutObservable<{ [nodeId: string]: number[] }>,
        private upperParameterBoundsStorage: KnockoutObservable<{ [nodeId: string]: number[] }>) {
    }

    onSelectedNameChanged() {
        this.EnableAutoFit();
    }

    // whether the log transform is applied to X axis
    public logX = ko.observable<boolean>(false);
}

import * as posteriorTableTemplate from 'raw-loader!../html/posterior-table.html';

// (FP) This class implements the control. It is an IInferenceViewer, so it can receive an inference run.
export class PosteriorViewer implements Inference.IInferenceViewer {
    // (FP) This class stores a VM for each of the three different types of viewers. Note that they will only get instantiated if a viewer of the appropriate type is found in the DOM when the Bind function is called.
    private posteriorTable: PosteriorTableVM;
    private scatterPlot: ScatterPlotVM;
    private densityPlot: DensityPlotVM;

    // (FP) This observables will contain the sets of parameters progressively generated by the inference process.
    private data = ko.observable<{ [nodeId: string]: KnockoutObservableArray<Inference.IRecentParametersValues> }>({}).extend({ deferred: true });
    private burninData = ko.observable<{ [nodeId: string]: KnockoutObservableArray<Inference.IRecentParametersValues> }>({}).extend({ deferred: true });

    // (FP) This observable contains the parameter names. The size needs to be the same as the size of the values array in IRecentParametersValues.
    private names = ko.observable<{ [nodeId: string]: string[] }>({});
    private lowerParameterBounds = ko.observable<{ [nodeId: string]: number[] }>({});
    private upperParameterBounds = ko.observable<{ [nodeId: string]: number[] }>({});

    private isActive = false;
    private onActivate = () => { };

    constructor(private nodeSelector: Inference.INodeSelector, isActive?: boolean) {
        this.isActive = isActive == true;
    }

    // (FP) The higher layer calls this to create the controls in the provided element. Unlike other controls, the controls will not be created directly in the supplied Element; rather, this function will search the children for elements with a known CSS class and create the controls there.
    Bind(div: HTMLElement) {
        if (div == null)
            throw "Attempt to bind a PosteriorViewer to null";

        // (FP) For each type of control, search the DOM for its placement and call applyBindings there with the right VM. Note that the data and names observables from this object get passed directly to the VM. Every VM will be observing the same object, so update propagation will be implicit.
        var table = $("posterior-table-viewer", $(div)).get(0);
        if (table) {
            if (!$('#posterior-table-viewer').length) $('head').append(posteriorTableTemplate);
            this.posteriorTable = new PosteriorTableVM(this.nodeSelector, this.data, this.names);
            ko.cleanNode(table);
            ko.applyBindings(this.posteriorTable, table);
        }
        var scatter = $("posterior-scatter-plot", $(div)).get(0);
        if (scatter) {
            this.scatterPlot = new ScatterPlotVM(this.nodeSelector, this.data, this.burninData, this.names, this.lowerParameterBounds, this.upperParameterBounds);

            ko.cleanNode(scatter);
            ko.applyBindings(this.scatterPlot, scatter);
            this.scatterPlot.CaptureSVG = GetExporter("scatter-plot", scatter);
            this.scatterPlot.EnableAutoFit = function () {
                var plot = InteractiveDataDisplay.asPlot($("div.idd-plot-master", scatter)[0]);
                plot.isAutoFitEnabled = true;
            };
        }

        var density = $("density-plot", $(div)).get(0);
        if (density) {
            this.densityPlot = new DensityPlotVM(this.nodeSelector, this.data, this.names, this.lowerParameterBounds, this.upperParameterBounds);
            ko.cleanNode(density);
            ko.applyBindings(this.densityPlot, density);
            this.densityPlot.CaptureSVG = GetExporter("parameter-density", density);
            this.densityPlot.EnableAutoFit = function () {
                var plot = InteractiveDataDisplay.asPlot($("div.idd-plot-master", density)[0]);
                plot.isAutoFitEnabled = true;
            };
        }

        var that = this;
        this.onActivate = () => { };
        $(div).on("activate", () => {
            that.isActive = true;
            that.onActivate();
        });
        $(div).on("deactivate", () => {
            that.isActive = false;
        });
    }

    // (FP) This is the function that the upper layers need to call to set the inference run for all the controls managed by this class. Updates are buffered, to avoid pushing too many updates too quickly.
    show(run: Inference.IInferenceRun) {
        var that = this;
        this.data({});
        this.burninData({});
        this.names({});
        this.lowerParameterBounds({});
        this.upperParameterBounds({});
        var sampleFreq = 250; // [ms] Update plots and parameters not more than once per sampleFreq
        // Decorates given set of observables by adding sampling to some of them to improve performance of this component.
        var sampledRun =
        {
            progress: run.progress,
            paramUpdates: run.paramUpdates.sample(sampleFreq), // <-- sampling
            simulationUpdates: run.simulationUpdates,
            paramDefinitions: run.paramDefinitions,
            summary: run.summary,
            posteriorTableUpdates: run.posteriorTableUpdates,
            traceDefinitions: run.traceDefinitions,
            plotSettingsUpdates: run.plotSettingsUpdates
        };

        // (FP) Propagate the run to all of the various controls.
        if (this.densityPlot) this.densityPlot.Progress.show(sampledRun);
        if (this.scatterPlot) this.scatterPlot.Progress.show(sampledRun);
        if (this.posteriorTable) this.posteriorTable.Progress.show(sampledRun);

        // (FP) This is the buffer where data is stored waiting to be pushed.
        let buffer: { [nodeId: string]: Inference.IRecentParametersValues[] } = {};
        let burnInBuffer: { [nodeId: string]: Inference.IRecentParametersValues[] } = {};
        // (FP) This is the flag that guards against enqueueing two updates simultaneously.
        let updateRequest = false;

        // (FP) This function pushes the data to the observables. Note that, while in many other cases the VM data gets replaced by the buffer, in this case it gets added incrementally. Part of that design means that the buffer is cleared immediately afterwards.
        let updateObservables = () => {
            for (let nodeId in buffer) {
                var value = buffer[nodeId];
                var data = this.data();
                if (data[nodeId] == null) {
                    data[nodeId] = ko.observableArray(value);
                    this.data(data);
                }
                else
                    ko.utils.arrayPushAll<Inference.IRecentParametersValues>(data[nodeId], value);
                buffer[nodeId] = [];
            }
            updateRequest = false;
        };

        let updateBurnInObservables = () => {
            for (let nodeId in burnInBuffer) {
                var value = burnInBuffer[nodeId];
                var data = this.burninData();
                if (data[nodeId] == null) {
                    data[nodeId] = ko.observableArray(value);
                    this.burninData(data);
                }
                else
                    ko.utils.arrayPushAll<Inference.IRecentParametersValues>(data[nodeId], value);
                burnInBuffer[nodeId] = [];
            }
            updateRequest = false;
        };

        // The parameter definitions and plot settings typically arrive immediately and never change, so they get stored immediately.
        run.paramDefinitions.subscribe(
            val => {
                var names = this.names();
                names[val.NodeID] = val.Definitions.map(v => v.Name);
                this.names(names);
                var lowerParameterBounds = this.lowerParameterBounds();
                lowerParameterBounds[val.NodeID] = val.Definitions.map(v => v.LowerBound);
                this.lowerParameterBounds(lowerParameterBounds);
                var upperParameterBounds = this.upperParameterBounds();
                upperParameterBounds[val.NodeID] = val.Definitions.map(v => v.UpperBound);
                this.upperParameterBounds(upperParameterBounds);
            },
            err => {
                console.log(err.message);
            },
            () => { }
        );

        let burnInThin: number;
        run.plotSettingsUpdates.subscribe(
            (settings) => {
                burnInThin = settings.BurnInThin;
                if (that.scatterPlot) {
                    that.scatterPlot.labelSize(settings.LabelFontSize + 'px');
                    that.scatterPlot.XTicks({
                        type: "numeric",
                        attachGrid: true,
                        fontSize: settings.TickFontSize
                    });
                    that.scatterPlot.YTicks({
                        type: "numeric",
                        attachGrid: true,
                        rotate: true,
                        fontSize: settings.TickFontSize
                    });
                }
                if (that.densityPlot) {
                    that.densityPlot.XTicks({
                        type: "numeric",
                        attachGrid: true,
                        fontSize: settings.TickFontSize
                    });
                    that.densityPlot.YTicks({
                        type: "numeric",
                        attachGrid: true,
                        rotate: true,
                        fontSize: settings.TickFontSize
                    });
                }
            },
            (err) => {
                console.log(err.message);
            },
            () => { }
        );
        // (FP) The posteriors updates arrive in a stream. When a new update arrive, it gets buffered, and a request for an update is enqueued via setTimeout, so that it happens when the main thread is idle.
        run.posteriorTableUpdates.subscribe(
            val => {
                if (buffer[val.NodeID] == null)
                    buffer[val.NodeID] = [];
                buffer[val.NodeID].push(val);
                // (FP) The updateRequest flags prevents an update from being enqueued if there is already an update scheduled.
                if (!updateRequest && that.isActive) {
                    updateRequest = true;
                    setTimeout(() => { updateObservables(); }, 20);
                }
            },
            err => {
                console.log(err.message);
            },
            () => { updateRequest = false; }
        );
        run.posteriorTableUpdates
            .last()
            .subscribe(val => {
                setTimeout(() => updateObservables());
            },
                err => { /* already handled by general subscription. This error in addition happens if the input is empty but this is normal for us. */ });

        let burnInLength = 0;
        run.progress.subscribe((val) => {
            burnInLength = val.BurnInLength;
        });

        let paramUpdatesCounter = 0;
        let paramSubscription = run.paramUpdates.subscribe(
            val => {
                if (burnInBuffer[val.NodeID] == null)
                    burnInBuffer[val.NodeID] = [];
                if (paramUpdatesCounter > burnInLength) {
                    burnInBuffer[val.NodeID].push(val);
                    paramSubscription.dispose();
                    updateRequest = true;
                    setTimeout(() => { updateBurnInObservables(); updateObservables(); }, 20);
                } else if (paramUpdatesCounter % burnInThin === 0) {
                    burnInBuffer[val.NodeID].push(val);
                    // (FP) The updateRequest flags prevents an update from being enqueued if there is already an update scheduled.
                    if (!updateRequest && that.isActive) {
                        updateRequest = true;
                        setTimeout(() => { updateBurnInObservables(); }, 20);
                    }
                }
                paramUpdatesCounter++;
            },
            err => {
                console.log(err.message);
            },
            () => { updateRequest = false; }
        );


        this.onActivate = () => {
            updateRequest = true;
            setTimeout(() => {
                updateBurnInObservables();
                updateObservables();
            });
        };
    }
}