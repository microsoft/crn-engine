import * as G from './ggraph';
import ContextGraph from './contextgraph';
import "jquery-mousewheel";
import { InteractiveDataDisplay, Plot } from "../node_modules/interactive-data-display/dist/idd.webpack";

/** Renderer that targets a Canvas inside IDD. */
class IDDGraph extends ContextGraph {
    graph?: G.GGraph;
    grid: boolean = false;
    gplot: any;

    private static msaglPlot: any = function (this: any, jqDiv: any, master: any) {
        this.base = InteractiveDataDisplay.CanvasPlot;
        this.base(jqDiv, master);
        this.aspectRatio = 1.0;

        var _graph: IDDGraph;

        this.renderCore = function (this: any, plotRect: any, screenSize: any) {
            if (_graph != null && _graph.graph != null && plotRect.width > 0 && plotRect.height > 0) {
                var bbox = _graph.graph.boundingBox;
                var context = this.getContext(true);
                context.save();

                var t = this.coordinateTransform;
                var offset = t.getOffset();
                var scale = t.getScale();

                context.translate(offset.x, offset.y);
                context.scale(scale.x, scale.y);
                context.translate(-bbox.x, -bbox.height - bbox.y);

                _graph.drawGraphFromPlot(context);
                context.restore();
            }
        }

        this.setGraph = function (g: IDDGraph) { _graph = g };

        this.computeLocalBounds = function (step: any, computedBounds: any) {
            return (_graph != null && _graph.graph != null) ? { x: 0, y: 0, width: _graph.graph.boundingBox.width, height: _graph.graph.boundingBox.height } : { x: 0, y: 0, width: 1, height: 1 };
        };
    };

    constructor(chartID: string, graph?: G.GGraph) {
        super();

        var plotContainerID = chartID + '-container';
        var plotID = chartID + '-plot';
        var container = document.getElementById(chartID);
        if (container == null) throw "Cannot find " + chartID;
        container.setAttribute('data-idd-plot', 'plot');
        var containerDiv = document.createElement('div');
        containerDiv.setAttribute('id', plotContainerID);
        containerDiv.setAttribute('data-idd-plot', plotID);
        container.appendChild(containerDiv);

        IDDGraph.msaglPlot.prototype = new (<any>InteractiveDataDisplay.CanvasPlot);
        this.graph = graph;

        InteractiveDataDisplay.register(plotID, function (jqDiv: any, master: any) { return new IDDGraph.msaglPlot(jqDiv, master); });
        var chart = InteractiveDataDisplay.asPlot(chartID);
        var gestureSource = (<any>InteractiveDataDisplay).Gestures.getGesturesStream($("#" + chartID));
        chart.navigation.gestureSource = gestureSource;
        container.ondblclick = function (ev: MouseEvent) {
            chart.fitToView();
        };
        this.gplot = chart.get(plotContainerID);
    }

    drawGraph(): void {
        this.gplot.setGraph(this);
        this.gplot.invalidateLocalBounds();
        this.gplot.requestNextFrameOrUpdate();
    }

    private drawGraphFromPlot(context: CanvasRenderingContext2D): void {
        if (this.grid)
            this.drawGrid(context);
        if (this.graph != null)
            this.drawGraphInternal(context, this.graph);
    }
}

export default IDDGraph