import './iddimports';
import * as Msagl from 'msagljs';
import React from 'react';

export interface IGraphProps {
    width: string;
    height: string;
    graph: Msagl.IGraph;
}

export class Graph extends React.Component<IGraphProps>{
    constructor(props: IGraphProps) {
        super(props);
        this.myRef = React.createRef();
    }

    private myRef: React.RefObject<HTMLDivElement>;
    private graphControl?: Msagl.IDDSVGGraph;

    render() {
        const style = {
            width: this.props.width,
            height: this.props.height
        }
        var that = this;
        setTimeout(() => that.chartUpdated());
        return (
            <div ref={this.myRef} style={style}></div>
        );
    }

    chartUpdated() {
        if (this.myRef.current == null || this.props.graph == null)
            return;
        this.disposeGraph();
        var gc = this.graphControl = new Msagl.IDDSVGGraph(this.myRef.current);
        var gg = new Msagl.GGraph(this.props.graph);
        gc.setGraph(gg);
        gg.createNodeBoundariesForSVGInContainer(this.myRef.current);
        gg.layoutCallbacks.add(() => gc.drawGraph());
        gg.beginLayoutGraph();
    }

    private disposeGraph() {
        if (this.graphControl != null && this.graphControl.getGraph() != null)
            this.graphControl.getGraph()?.stopLayoutGraph();
        this.graphControl = undefined;
    }

    componentWillUnmount() {
        this.disposeGraph();
    }

    shouldComponentUpdate(nextProps: IGraphProps): boolean {
        return JSON.stringify(this.props) != JSON.stringify(nextProps);
    }
}