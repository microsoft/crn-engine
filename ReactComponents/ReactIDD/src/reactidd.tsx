import './iddimports';
import IDD from 'interactive-data-display';
import React from 'react';
const InteractiveDataDisplay = IDD.InteractiveDataDisplay;

/*
I'm exporting several different interfaces, each of which describes a kind of plot. Note that these objects do not map exactly to the corresponding IDD objects, for example both bars and box-and-whiskers plots are actually marker plots in IDD. I do this for two reasons.

The first is to constrain fields for each kind of plot, e.g. prevent the user from attempting to specify a shape for a box plot, or to provide non-quantile values for a box-and-whiskers plot.

The second is to prevent any given field from having a different type depending on which plot kind it's used for, e.g. the 'y' values object for a box-and-whiskers plot is actually named 'q' here, because it's not a number array. I do this to make it easy to define a type in F# that can represent any IDD plot without using union types. This object can then be serialised to and from an IPlot by automatically generated serialisers.
*/

export interface IPolyline {
    kind: "polyline";
    id: string;
    x: number[];
    y: number[];
    stroke?: string;
    thickness?: number;
}

export type MarkerShape = "box" | "circle" | "cross" | "diamond" | "triangle";

export interface IMarkers {
    kind: "markers";
    id: string;
    x: number[];
    y: number[];
    size?: number;
    color?: string;
    shape?: MarkerShape
}

export interface ISizedMarkers {
    kind: "sizedmarkers";
    id: string;
    x: number[];
    y: number[];
    sizes: number[];
    color?: string;
    shape?: MarkerShape
}

export interface IBars {
    kind: "bars";
    id: string;
    x: number[];
    y: number[];
    barWidth: number;
    color?: string;
}

export interface IBoxAndWhiskers {
    kind: "boxandwhiskers";
    id: string;
    x: number[];
    q: {
        median: number[];
        lower68: number[];
        upper68: number[];
        lower95: number[];
        upper95: number[];
    },
    size?: number;
    color?: string;
}

export interface IHeatmap {
    kind: "heatmap";
    id: string;
    x: number[];
    y: number[];
    values: number[][];
    colorPalette?: string;
}

export type IPlot = IPolyline | IMarkers | ISizedMarkers | IBars | IBoxAndWhiskers | IHeatmap

export interface IAxis {
    position: "left" | "bottom" | "right" | "top"
    kind: "numeric"
}

export interface IChartProps {
    width: string;
    height: string;
    /** If this is undefined, then leave the default axes. */
    axes?: IAxis[];
    plots: IPlot[];
}

export class Chart extends React.Component<IChartProps>{
    constructor(props: IChartProps) {
        super(props);
        this.myRef = React.createRef();
    }

    myRef: React.RefObject<HTMLDivElement>;

    render() {
        const style = {
            width: this.props.width,
            height: this.props.height
        };
        var that = this;
        // IDD requires running some JS after the chart div has been added to the DOM.
        setTimeout(() => that.chartUpdated());
        return (
            <div data-idd-plot="chart" ref={this.myRef} style={style}></div>
        );
    }

    /** I can't find a way to enumerate existing plots in IDD, so I'm going to store references to all of my plots here. I'll populate this array as I build plots. */
    existingPlots: any[] = [];

    /** This function gets invoked after the chart has been added to the DOM, and is responsible for populating it with plots, using IDD calls. Because I don't know which plots have changed, I will delete and recreate all of them every time this function gets invoked. There are performance implications with this approach, which may require attention later on. Note that it is also possible to construct plots in HTML, and this may allow React to do some significant optimization; however, on my first attempts I could not get IDD to update or remove existing plots using this approach, which appears to be intended for static plots (in which case the performance of the current approach should be identical anyway). */
    chartUpdated() {
        if (this.myRef.current == null)
            return;
        // First of all, I'm getting the IDD object corresponding to the chart. This will turn the div into a chart, if it isn't already.
        const chart = InteractiveDataDisplay.asPlot(this.myRef.current);
        // Next, I'm removing the existing plots.
        for (let plot of this.existingPlots) {
            plot.remove();
        }
        // Then, I'm clearing the plot references storage.
        this.existingPlots = [];
        // Next, I'm creating each plot.
        for (let plot of this.props.plots) {
            // Each plot should have an ID. If they don't, unpredictable behavior may result.
            if (plot.id === null || plot.id === "")
                console.log("IDD warning: no id set for a plot.");
            var plotObject: any;
            switch (plot.kind) {
                case "polyline":
                    // Polyline: my object has the same shape as the IDD object.
                    plotObject = chart.polyline(plot.id, plot);
                    break;
                case "markers":
                    // Markers: my object has the same shape as the IDD object.
                    plotObject = chart.markers(plot.id, plot);
                    break;
                case "sizedmarkers":
                    // Markers where the size is a series: my 'sizes' field needs to be renamed 'size' to match the IDD object.
                    var psizedmarkers = Object.assign({}, plot, { size: plot.sizes });
                    plotObject = chart.markers(plot.id, psizedmarkers);
                    break;
                case "bars":
                    // Bars: the IDD shape needs to be set to 'bars'.
                    var pbars = Object.assign({}, plot, { shape: "bars" });
                    plotObject = chart.markers(plot.id, pbars);
                    break;
                case "boxandwhiskers":
                    // Box-and-whiskers: the IDD shape needs to be set to 'boxwhisker', and the 'q' field needs to be renamed 'y'.
                    var pboxandwhiskers = Object.assign({}, plot, { shape: "boxwhisker", y: plot.q });
                    plotObject = chart.markers(plot.id, pboxandwhiskers);
                    break;
                case "heatmap":
                    // Heatmap: my object has the same shape as the IDD object.
                    plotObject = chart.heatmap(plot.id, plot);
                    break;
                default:
                    // This should never happen.
                    console.log("invalid plot kind '" + (plot as any).kind + "'.");
                    return;
            }
            // Store the plot in the references array.
            this.existingPlots.push(plotObject);
        }
        // Next, if the axes are non-default, I'm setting them.
        if (this.props.axes != null) {
            // Remove the default axes.
            var axes = chart.getAxes();
            if (axes != null)
                axes.forEach((axis:any) => axis.remove());
            // Add my own axes. I'll need to hook them up with the grid.
            var grid = null;
            this.myRef.current.childNodes.forEach(cn => {
                var div = cn as HTMLElement;
                if (div.getAttribute != null && div.getAttribute("data-idd-plot") == "grid")
                    grid = cn as HTMLElement;
            });
            for (let axis of this.props.axes) {
                let iddAxis = chart.addAxis(axis.position, axis.kind).axis;
                if (grid != null) {
                    if (axis.position == "bottom" || axis.position == "top")
                        chart.get(grid).xAxis = iddAxis;
                    if (axis.position == "left" || axis.position == "right")
                        chart.get(grid).yAxis = iddAxis;
                }
            }
        }
    }
}