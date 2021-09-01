// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import "../../KnockoutGrid/knockoutgrid";
import * as ko from "knockout";
import "jquery-mousewheel";
import "idd";
declare var InteractiveDataDisplay: any;
import GetExporter from "../../GenericComponents/Scripts/SVGExporter";
import * as katex from "katex";
import { saveAs } from "file-saver";
import * as MSAGL from './../../../HTML5SharedGUI/MSAGL_JS/Scripts/msagl';
import ZoomSlider from './../../GenericComponents/Scripts/ZoomSlider';
import * as tableTemplate from 'raw-loader!../html/SynthesisTableTemplates.html';
import * as valuesTemplate from 'raw-loader!../html/SynthesisValues.html';
import * as equationsTemplate from 'raw-loader!../html/SynthesisEquations.html';
import * as dispersionTemplate from 'raw-loader!../html/SynthesisDispersion.html';
import * as jacobianTemplate from 'raw-loader!../html/SynthesisJacobian.html';
import * as codeTemplate from 'raw-loader!../html/SynthesisCode.html';
import * as bistabilityTemplate from 'raw-loader!../html/BistabilityViewer.html';
import { SynthesisResult, SynthesisValue, BistabilityPlot, CRN } from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
$('head').append(tableTemplate);

ko.components.register("bistability-viewer", {
    template: bistabilityTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var $_elem = $(componentInfo.element);
            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $_elem));
            var gestureSource = InteractiveDataDisplay.Gestures.getGesturesStream(plot.centralPart);
            var bottomAxisGestures = InteractiveDataDisplay.Gestures.applyHorizontalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-bottom-axis", $_elem)));
            var leftAxisGestures = InteractiveDataDisplay.Gestures.applyVerticalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-left-axis", $_elem)));
            plot.navigation.gestureSource = gestureSource.merge(bottomAxisGestures.merge(leftAxisGestures));

            var context = ko.contextFor(componentInfo.element);
            if (context != null && context.$data != null)
                (context.$data as SynthesisViewer).SaveBistability = GetExporter("bistability", <HTMLElement>componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

ko.components.register("synthesis-values", {
    template: valuesTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

ko.components.register("synthesis-equations", {
    template: equationsTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

ko.components.register("synthesis-code", {
    template: codeTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

ko.components.register("synthesis-dispersion", {
    template: dispersionTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var $_elem = $(componentInfo.element);
            var plot = InteractiveDataDisplay.asPlot($("div[data-idd-plot='figure']", $_elem));
            var gestureSource = InteractiveDataDisplay.Gestures.getGesturesStream(plot.centralPart);
            var bottomAxisGestures = InteractiveDataDisplay.Gestures.applyHorizontalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-bottom-axis", $_elem)));
            var leftAxisGestures = InteractiveDataDisplay.Gestures.applyVerticalBehavior(InteractiveDataDisplay.Gestures.getGesturesStream($("div.j-left-axis", $_elem)));
            plot.navigation.gestureSource = gestureSource.merge(bottomAxisGestures.merge(leftAxisGestures));

            var context = ko.contextFor(componentInfo.element);
            if (context != null && context.$data != null)
                (context.$data as SynthesisViewer).SaveDispersion = GetExporter("dispersion", <HTMLElement>componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

ko.components.register("synthesis-jacobian", {
    template: jacobianTemplate,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

export interface IBistabilityPlotProvider {
    GetBistabilityPlot(crn: CRN, solution: { [name: string]: number }, spx: string, spy: string, numPoints: number): Rx.Observable<BistabilityPlot>;
}

export class SynthesisViewer {
    private container: HTMLElement = null;

    constructor(bistability: IBistabilityPlotProvider) {
        var that = this;
        this.BistabilityProvider = bistability;
        // Setup the graph viewer.
        this.JacobianAspectRatio.subscribe(() => that.LayoutJacobian());
        this.JacobianHorizontalLayout.subscribe(() => that.LayoutJacobian());
        this.JacobianRouting.subscribe(() => that.LayoutJacobian());
        this.JacobianZoom.Value.subscribe(p => {
            if (!that.zoom_noRecurse && that.JacobianGraph != null)
                that.JacobianGraph.setZoomLevel(p);
        });
    }

    public Bind(elem: HTMLElement) {
        if (elem == null)
            throw "Attempt to bind SynthesisViewer to null.";
        this.container = elem;
        ko.applyBindings(this, elem);
    }

    public Result: KnockoutObservable<SynthesisResult> = ko.observable(null);

    public ResultValues: KnockoutComputed<{ name: string, v: SynthesisValue }[]> = ko.computed(() => {
        var ret: { name: string, v: SynthesisValue }[] = [];
        var result = this.Result();
        if (result == null)
            return ret;
        for (let res in result.values)
            ret.push({ name: res, v: result.values[res] });
        return ret;
    });

    public tableConfig: any = {
        data: this.ResultValues,
        headerTemplate: 'synthesis-table-header',
        columnTemplate: 'synthesis-table-row',
        ViewModel: this,
        last: true,
    };

    public Code = ko.computed(() => {
        var result = this.Result();
        if (result == null || result.code == null)
            return "";
        return result.code;
    });

    public LatexEquationsRates: KnockoutObservable<string> = ko.observable();
    public LatexEquationsJacobian: KnockoutObservable<string> = ko.observable();
    public LatexEquationsDiffusion: KnockoutObservable<string> = ko.observable();
    public LatexEquationsCsts: KnockoutObservable<string> = ko.observable();
    public LatexEquationsAll = ko.computed(() => this.LatexEquationsRates() + this.LatexEquationsJacobian() + this.LatexEquationsDiffusion() + this.LatexEquationsCsts());
    public LatexError: KnockoutObservable<string> = ko.observable();

    public DispersionMarkersX: KnockoutComputed<number[]> = ko.computed(() => {
        var result = this.Result();
        if (result == null || result.dispersion == null)
            return [];
        return result.dispersion.markersX;
    });
    public DispersionMarkersY: KnockoutComputed<number[]> = ko.computed(() => {
        var result = this.Result();
        if (result == null || result.dispersion == null)
            return [];
        return result.dispersion.markersY;
    });
    public DispersionPlotX: KnockoutComputed<number[]> = ko.computed(() => {
        var result = this.Result();
        if (result == null || result.dispersion == null)
            return [];
        return result.dispersion.plotX;
    });
    public DispersionPlotY: KnockoutComputed<number[]> = ko.computed(() => {
        var result = this.Result();
        if (result == null || result.dispersion == null)
            return [];
        return result.dispersion.plotY;
    });
    public DispersionAutoFitEnabled: KnockoutObservable<boolean> = ko.observable(true);
    public DispersionVisibleRegion: KnockoutObservable<{ x_min?: number; x_max?: number; y_min?: number; y_max?: number }> = ko.observable({});
    public DispersionAutoFitString = ko.pureComputed<string>(() => {
        if (!this.DispersionAutoFitEnabled())
            return "false";
        var v = this.DispersionVisibleRegion();
        if (v.x_min == null && v.x_max == null && v.y_min == null && v.y_max == null)
            return "true";
        return "bounds(" + (v.x_min == null ? "auto" : v.x_min) + "," + (v.x_max == null ? "auto" : v.x_max) + "," + (v.y_min == null ? "auto" : v.y_min) + "," + (v.y_max == null ? "auto" : v.y_max) + ")";
    });

    private static BistabilityDefaultSelectedPoints = 20;
    private BistabilityProvider: IBistabilityPlotProvider = null;
    public BistabilityResult: KnockoutObservable<BistabilityPlot> = ko.observable(null);
    public BistabilityXLabel = ko.computed(() => this.BistabilityResult() == null ? "" : this.BistabilityResult().speciesX);
    public BistabilityYLabel = ko.computed(() => this.BistabilityResult() == null ? "" : this.BistabilityResult().speciesY);
    public BistabilitySpeciesX = ko.computed(() => {
        var result = this.BistabilityResult();
        if (result == null) return [];
        return [result.state1x, result.state2x];
    });
    public BistabilitySpeciesY = ko.computed(() => {
        var result = this.BistabilityResult();
        if (result == null) return [];
        return [result.state1y, result.state2y];
    });
    public BistabilityStateColors = ['red', 'blue'];
    public BistabilityInitialsX = ko.computed(() => {
        var result = this.BistabilityResult();
        if (result == null) return [];
        return result.initialsX;
    });
    public BistabilityInitialsY = ko.computed(() => {
        var result = this.BistabilityResult();
        if (result == null) return [];
        return result.initialsY;
    });
    public BistabilityState1Traces = ko.computed(() => {
        var result = this.BistabilityResult();
        if (result == null) return [];
        var ret: { x: number[]; y: number[] }[] = [];
        for (var i = 0; i < result.state1simsX.length && i < result.state1simsY.length; i++)
            ret.push({ x: result.state1simsX[i], y: result.state1simsY[i] });
        return ret;
    });
    public BistabilityState2Traces = ko.computed(() => {
        var result = this.BistabilityResult();
        if (result == null) return [];
        var ret: { x: number[]; y: number[] }[] = [];
        for (var i = 0; i < result.state2simsX.length && i < result.state2simsY.length; i++)
            ret.push({ x: result.state2simsX[i], y: result.state2simsY[i] });
        return ret;
    });
    public BistabilitySpeciesNames = ko.computed(() => {
        var result = this.Result();
        if (result == null || result.crn == null) return null;
        var species = result.crn.initials.map(i => i.species);
        return species;
    });
    public BistabilitySelectedSpeciesX = ko.observable("");
    public BistabilitySelectedSpeciesY = ko.observable("");
    public BistabilitySelectedPoints = ko.observable(SynthesisViewer.BistabilityDefaultSelectedPoints);

    public OnBistabilityClick() {
        var result = this.Result();
        if (result == null || result.values == null || result.crn == null) return;
        var speciesX = this.BistabilitySelectedSpeciesX();
        var speciesY = this.BistabilitySelectedSpeciesY();
        var points = this.BistabilitySelectedPoints();
        // Not sure whether this is a browser issue or a KO issue, but this can turn out to be a string. This would cause problems with deserialization.
        if (typeof points == "string")
            points = parseInt(points);
        var values: { [name: string]: number } = {};
        for (let v in result.values)
            values[v] = result.values[v].value;
        var observable = this.BistabilityProvider.GetBistabilityPlot(result.crn, values, speciesX, speciesY, points);
        observable.subscribe(plot => {
            this.BistabilityResult(plot);
        }, e => console.log(e), () => { });
    }

    public JacobianGraph: MSAGL.IDDSVGGraph = null;

    public JacobianHorizontalLayout = ko.observable(false);
    public JacobianAspectRatio = ko.observable(false);
    public JacobianRouting = ko.observable<string>(MSAGL.GSettings.sugiyamaSplinesRouting);
    public JacobianZoom: ZoomSlider = new ZoomSlider();
    private zoom_noRecurse: boolean = false;

    private SetupJacobianGraph() {
        var that = this;
        var graphContainer = $(".c-graph__graph", this.container)[0];
        this.JacobianGraph = new MSAGL.IDDSVGGraph(graphContainer);
        this.JacobianGraph.zoomLevelChangeCallback = v => {
            that.zoom_noRecurse = true;
            that.JacobianZoom.Value(v);
            that.zoom_noRecurse = false;
        }
    }

    private LayoutJacobian() {
        var graphContainer = $(".c-graph__graph", this.container)[0];
        var graph = this.JacobianGraph == null ? null : this.JacobianGraph.getGraph();
        if (graph != null)
            graph.stopLayoutGraph();
        graph = new MSAGL.GGraph();
        var result = this.Result();
        if (result != null && result.jacobian != null) {
            for (let n of result.jacobian.nodes)
                graph.addNode(new MSAGL.GNode({
                    id: n.id,
                    fill: n.fill,
                    stroke: n.stroke,
                    label: n.label
                }));
            for (let e of result.jacobian.edges)
                graph.addEdge(new MSAGL.GEdge({
                    id: e.source + "-" + e.destination,
                    source: e.source,
                    target: e.destination,
                    stroke: e.stroke,
                    label: e.label,
                    arrowHeadAtTarget: MSAGL.GArrowHead.standard
                }));

            // Deal with settings.
            // Plane transformation.
            graph.settings.transformation = this.JacobianHorizontalLayout() ? MSAGL.GPlaneTransformation.ninetyDegreesTransformation : MSAGL.GPlaneTransformation.defaultTransformation;
            // Enforcing aspect ratio.
            if (this.JacobianAspectRatio()) {
                var w = graphContainer.offsetWidth;
                var h = graphContainer.offsetHeight;
                if (w != null && w > 0 && h != null && h > 0)
                    graph.settings.aspectRatio = this.JacobianHorizontalLayout() ? h / w : w / h;
            }
            graph.settings.routing = this.JacobianRouting();

            // Create the node boundaries.
            graph.createNodeBoundariesFromSVG();
        }
        if (this.JacobianGraph == null)
            this.SetupJacobianGraph();
        this.JacobianGraph.setGraph(graph);

        // Prepare a layout callback.
        var that = this;
        var graphLayoutCallback = () => that.JacobianGraph.drawGraph();
        graph.layoutCallbacks.add(graphLayoutCallback);
        // Begin layout.
        graph.beginLayoutGraph();
    }

    public Show(result: SynthesisResult): void {
        this.Result(result);

        if (result.dispersion != null) {
            this.DispersionVisibleRegion({ x_min: result.dispersion.xMin, x_max: result.dispersion.xMax, y_min: result.dispersion.yMin, y_max: result.dispersion.yMax });
            // Toggling AutoFitEnabled because the user might have changed it using the mouse. If that happens, then autofit is disabled but AutoFitEnabled is true. Because just setting it to true would have no effect (it's the same value), I need to set it to false and then to true, so that I am certain a change notification is sent to IDD.
            this.DispersionAutoFitEnabled(false);
            this.DispersionAutoFitEnabled(true);
        }

        var latexEquationsRates = "", latexEquationsJacobian = "", latexEquationsDiffusion = "", latexEquationsCsts = "";
        var latexError = "";
        var result = this.Result();
        if (result != null) {
            var makeEquation = (str: string) => {
                var ret = "";
                if (str == null || str == "")
                    return ret;
                // Tweak the string to make Katex accept it.
                var hack = str.replace(/{align}/g, "{aligned}");
                var lines = hack.split("$$").map(s => s.trim()).filter(s => s != "");
                ret = "<p>";
                try {
                    for (let line of lines)
                        ret += katex.renderToString(line) + "</p><p>";
                }
                catch (e) {
                    latexError += (e.message == null ? e.toString() : e.message.toString()) + "<br/>";
                }
                ret += "</p>";
                return ret;
            }
            latexEquationsRates = makeEquation(result.equations.rateEquations);
            latexEquationsDiffusion = makeEquation(result.equations.diffusion);
            latexEquationsJacobian = makeEquation(result.equations.jacobian);
            latexEquationsCsts = makeEquation(result.equations.csts);
        }
        this.LatexEquationsRates(latexEquationsRates);
        this.LatexEquationsJacobian(latexEquationsJacobian);
        this.LatexEquationsDiffusion(latexEquationsDiffusion);
        this.LatexEquationsCsts(latexEquationsCsts);
        this.LatexError(latexError);

        this.LayoutJacobian();

        this.BistabilityResult(null);
        this.BistabilitySelectedPoints(SynthesisViewer.BistabilityDefaultSelectedPoints);
        var bistabilitySpeciesNames = this.BistabilitySpeciesNames();
        if (bistabilitySpeciesNames != null && bistabilitySpeciesNames.length > 1) {
            this.BistabilitySelectedSpeciesX(bistabilitySpeciesNames[0]);
            this.BistabilitySelectedSpeciesY(bistabilitySpeciesNames[1]);
            this.OnBistabilityClick();
        }
    }

    // This function generates a CSV file with the current values.
    SaveValues() {
        var csv = Papa.unparse({
            data: this.ResultValues(),
            fields: ["Name", "Value", "Lower Bound", "Upper Bound"]
        },
            {
                quotes: false,
                delimiter: ",",
                newline: "\r\n"
            });
        var blob = new Blob([csv], { type: "text/csv" });
        saveAs(blob, "SynthesisValues.csv");
    }

    SaveEquations() {
        var eq = this.LatexEquationsAll();
        if (eq == null || eq == "")
            return;
        var blob = new Blob([eq], { type: "application/x-tex" });
        saveAs(blob, "SynthesisEquations.tex");
    }

    public SaveDispersion: () => void;
    public SaveBistability: () => void;

    public SaveJacobian() {
        this.JacobianGraph.saveAsSVG("jacobian.svg");
    }

    public SaveCode() {
        var code = this.Code();
        if (code == null || code == "")
            return;
        var blob = new Blob([code], { type: "text/plain" });
        saveAs(blob, "model.crn");
    }
}
