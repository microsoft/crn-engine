/* tslint:disable:insecure-random */
import React, { ChangeEvent, FormEvent } from 'react';
import ReactDOM from 'react-dom';
import { Chart, IPlot, IAxis, MarkerShape } from 'reactidd';
import { Graph } from 'reactmsagl';
import * as Msagl from 'msagljs';
import { CodePad, RegisterLanguage } from 'reactcodepad';
import './index.css';

// This is how you set up a custom language for Monaco in JS. Language definitions in Monaco are global.
import crnLanguage from "./crnlang";
RegisterLanguage("crn", crnLanguage);

/*
This is a test app for our React libraries.

IDD is invoked by declaring a <Chart/> element in the render method. The only relevant property is "plots", which takes an array of IPlot objects. The rest of this code is used to show a GUI to manipulate IPlot objects. This is just for the purpose of providing a sample GUI: in user code, you would create IPlot objects in whichever way you see fit. This program violates the React best practice of immutability in several points, due to reluctance to duplicating plot objects; I consider this acceptable in the context of providing a sample.

MSAGL is invoked by declaring a <Graph/> element in the render method. The only relevant property is "graph", which takes a Msagl.GGraph object. Here, a simple graph is manually created. In user code, you would create the Msagl.GGraph object in whichever way you see fit.

Monaco is invoked via the react-monaco-editor package (https://github.com/react-monaco-editor/react-monaco-editor). Monaco needs a specific Webpack plugin (monaco-editor-webpack-plugin) to work correctly. Normally, you would load it in webpack.config.js (see documentation for monaco-editor-webpack-plugin). In this sample, we are using config-overrides.js instead, because this sample was created via create-react-app, which hides access to webpack.config.js.
*/

interface IReactTestState {
  plots: IPlot[];
  newPlotId: string;
  newPlotKind: "polyline" | "markers" | "sizedmarkers" | "bars" | "boxandwhiskers" | "heatmap";
  newPlotError: string;

  graph: Msagl.GGraph;

  json: string;
  crntext: string;
}

function makeMsaglGraph() {
  var graph = new Msagl.GGraph();
  graph.addNode(new Msagl.GNode({ id: "node1", label: "Node 1" }));
  graph.addNode(new Msagl.GNode({ id: "node2", label: "Node 2" }));
  graph.addNode(new Msagl.GNode({ id: "node3", label: "Node 3" }));
  graph.addEdge(new Msagl.GEdge({ id: "edge12", source: "node1", target: "node2" }));
  graph.addEdge(new Msagl.GEdge({ id: "edge13", source: "node1", target: "node3" }));
  graph.addEdge(new Msagl.GEdge({ id: "edge23", source: "node2", target: "node3" }));
  graph.createNodeBoundariesFromSVG();
  return graph;
}

class ReactTest extends React.Component<{}, IReactTestState> {
  constructor(props: {}) {
    super(props);
    var plots: IPlot[] = [{
      kind: "polyline",
      id: "myPlot",
      x: [0, 1, 2, 3, 4],
      y: [2, -1, 0, 1, -2],
      stroke: "black",
      thickness: 2
    }];
    this.state = {
      plots: plots,
      newPlotId: "myPlot1",
      newPlotKind: "polyline",
      newPlotError: "",
      graph: makeMsaglGraph(),
      json: JSON.stringify(plots, null, 2),
      crntext: "directive simulation { }\nX + Y ->{0.2} Z"
    };
  }

  private updateState(state: any) {
    if (state.plots != null && state.json == null)
      state.json = JSON.stringify(state.plots, null, 2);
    this.setState(state);
  }

  onAddPointClicked(plotId: string) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined) {
      if (plot.kind === "heatmap") {
        if (plot.y.length * 2 < plot.x.length) {
          plot.y.push(plot.y.length);
          for (var x = 0; x < plot.x.length; x++)
            plot.values[x].push(Math.random());
        }
        else {
          plot.x.push(plot.x.length);
          var newRow = [];
          for (var y = 0; y < plot.y.length; y++)
            newRow[y] = Math.random();
          plot.values.push(newRow);
        }
      }
      else {
        plot.x.push(plot.x.length + 1);
        if (plot.kind === "boxandwhiskers") {
          var median = Math.random() * 4 - 2;
          var lower68 = median - Math.random();
          var upper68 = median + Math.random();
          plot.q.median.push(median);
          plot.q.lower68.push(lower68);
          plot.q.upper68.push(upper68);
          plot.q.lower95.push(lower68 - Math.random());
          plot.q.upper95.push(upper68 + Math.random());
        }
        else
          plot.y.push(Math.random() * 4 + (plot.kind === "bars" ? 0 : -2));
        if (plot.kind === "sizedmarkers")
          plot.sizes.push(Math.random() * 20 + 5);
      }
    }
    this.updateState({ plots: plots });
  }

  onRemovePlotClicked(plotId: string) {
    this.updateState({ plots: this.state.plots.filter(p => p.id !== plotId) });
  }

  onAddPlotClicked(ev: FormEvent<HTMLFormElement>) {
    ev.preventDefault();
    if (this.state.newPlotId === null || this.state.newPlotId === "" || this.state.plots.find(p => p.id === this.state.newPlotId) !== undefined) {
      this.updateState({ newPlotError: "The new plot must have a valid and unique ID." });
      return;
    }
    var plots = this.state.plots.slice();
    var plot: IPlot;
    switch (this.state.newPlotKind) {
      case "markers":
        plot = {
          kind: "markers",
          id: this.state.newPlotId,
          x: [],
          y: [],
          color: "black",
          shape: "box",
          size: 5
        }
        break;
      case "sizedmarkers":
        plot = {
          kind: "sizedmarkers",
          id: this.state.newPlotId,
          x: [],
          y: [],
          color: "black",
          shape: "box",
          sizes: []
        }
        break;
      case "polyline":
        plot = {
          kind: "polyline",
          id: this.state.newPlotId,
          x: [],
          y: [],
          stroke: "black",
          thickness: 2
        }
        break;
      case "bars":
        plot = {
          kind: "bars",
          id: this.state.newPlotId,
          x: [],
          y: [],
          color: "black",
          barWidth: 1
        }
        break;
      case "boxandwhiskers":
        plot = {
          kind: "boxandwhiskers",
          id: this.state.newPlotId,
          x: [],
          q: { median: [], lower68: [], upper68: [], lower95: [], upper95: [] },
          size: 10,
          color: "black"
        }
        break;
      case "heatmap":
        plot = {
          kind: "heatmap",
          id: this.state.newPlotId,
          x: [0, 1],
          y: [0, 1],
          values: [[0.1, 0.2], [0.3, 0.4]],
          colorPalette: "green,red"
        }
        break;
      default:
        // This should never happen.
        this.updateState({ newPlotError: "Invalid plot kind '" + this.state.newPlotKind + "'." });
        return;
    }
    plots.push(plot);

    var c = 1;
    var newNewPlotId = "myPlot" + c++;
    /* eslint-disable-next-line */
    while (plots.find(p => p.id === newNewPlotId) !== undefined)
      newNewPlotId = "myPlot" + c++;

    this.updateState({ plots: plots, newPlotId: newNewPlotId, newPlotError: "" });
  }

  onNewPlotIdChanged(ev: ChangeEvent<HTMLInputElement>) {
    this.updateState({ newPlotId: ev.target.value });
  }

  onNewPlotKindChanged(ev: ChangeEvent<HTMLSelectElement>) {
    if (ev.target.value === "markers" || ev.target.value === "polyline" || ev.target.value === "heatmap")
      this.updateState({ newPlotKind: ev.target.value });
  }

  onPolylineStrokeChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "polyline")
      plot.stroke = ev.target.value;
    this.updateState(this.state);
  }

  onPolylineThicknessChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "polyline")
      plot.thickness = parseInt(ev.target.value);
    this.updateState(this.state);
  }

  onMarkersSizeChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "markers")
      plot.size = parseInt(ev.target.value);
    this.updateState(this.state);
  }

  onMarkersColorChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "markers")
      plot.color = ev.target.value;
    this.updateState(this.state);
  }

  onMarkersShapeChanged(plotId: string, ev: ChangeEvent<HTMLSelectElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "markers")
      plot.shape = ev.target.value as MarkerShape;
    this.updateState(this.state);
  }

  onBarsColorChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "bars")
      plot.color = ev.target.value;
    this.updateState(this.state);
  }

  onBarsWidthChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "bars")
      plot.barWidth = parseFloat(ev.target.value);
    this.updateState(this.state);
  }

  onBoxAndWhiskersColorChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "boxandwhiskers")
      plot.color = ev.target.value;
    this.updateState(this.state);
  }

  onBoxAndWhiskersSizeChanged(plotId: string, ev: ChangeEvent<HTMLInputElement>) {
    var plots = this.state.plots.slice();
    var plot = plots.find(p => p.id === plotId);
    if (plot !== undefined && plot.kind === "boxandwhiskers")
      plot.size = parseInt(ev.target.value);
    this.updateState(this.state);
  }

  renderIDD() {
    var errorStyle = { color: "red" };
    var plotRowStyle = { marginTop: 6 }
    var headerStyle = { marginLeft: 10 };
    var propStyle = { width: 50, marginLeft: 2 };
    var optStyle = { marginLeft: 2 };
    // Here I create the controls that can be used to manipulate a given plot.
    var plotControls = this.state.plots.map(plot => {
      switch (plot.kind) {
        case "polyline":
          return (
            <div style={plotRowStyle} key={plot.id}>
              <label>Polyline {plot.id}:</label>
              <label style={headerStyle}>Stroke:<input style={propStyle} type="text" value={plot.stroke} onChange={ev => this.onPolylineStrokeChanged(plot.id, ev)}></input></label>
              <label style={headerStyle}>Thickness:<input style={propStyle} type="number" value={plot.thickness} onChange={ev => this.onPolylineThicknessChanged(plot.id, ev)}></input></label>
              <button style={headerStyle} onClick={ev => this.onAddPointClicked(plot.id)}>Add point</button>
              <button style={headerStyle} onClick={ev => this.onRemovePlotClicked(plot.id)}>Remove this plot</button>
            </div>
          );
        case "markers":
          return (
            <div style={plotRowStyle} key={plot.id}>
              <label>Markers {plot.id}:</label>
              <label style={headerStyle}>Size:<input style={propStyle} type="number" value={plot.size} onChange={ev => this.onMarkersSizeChanged(plot.id, ev)}></input></label>
              <label style={headerStyle}>Color:<input style={propStyle} type="text" value={plot.color} onChange={ev => this.onMarkersColorChanged(plot.id, ev)}></input></label>
              <label style={headerStyle}>Shape:
                <select style={optStyle} value={plot.shape} onChange={ev => this.onMarkersShapeChanged(plot.id, ev)}>
                  <option value="box">Box</option>
                  <option value="circle">Circle</option>
                  <option value="cross">Cross</option>
                  <option value="diamond">Diamond</option>
                  <option value="triangle">Triangle</option>
                </select>
              </label>
              <button style={headerStyle} onClick={ev => this.onAddPointClicked(plot.id)}>Add point</button>
              <button style={headerStyle} onClick={ev => this.onRemovePlotClicked(plot.id)}>Remove this plot</button>
            </div>
          );
        case "sizedmarkers":
          return (
            <div style={plotRowStyle} key={plot.id}>
              <label>Markers {plot.id}:</label>
              <label style={headerStyle}>Color:<input style={propStyle} type="text" value={plot.color} onChange={ev => this.onMarkersColorChanged(plot.id, ev)}></input></label>
              <label style={headerStyle}>Shape:
                  <select style={optStyle} value={plot.shape} onChange={ev => this.onMarkersShapeChanged(plot.id, ev)}>
                  <option value="box">Box</option>
                  <option value="circle">Circle</option>
                  <option value="cross">Cross</option>
                  <option value="diamond">Diamond</option>
                  <option value="triangle">Triangle</option>
                </select>
              </label>
              <button style={headerStyle} onClick={ev => this.onAddPointClicked(plot.id)}>Add point</button>
              <button style={headerStyle} onClick={ev => this.onRemovePlotClicked(plot.id)}>Remove this plot</button>
            </div>
          );
        case "bars":
          return (
            <div style={plotRowStyle} key={plot.id}>
              <label>Bars {plot.id}:</label>
              <label style={headerStyle}>Color:<input style={propStyle} type="text" value={plot.color} onChange={ev => this.onBarsColorChanged(plot.id, ev)}></input></label>
              <label style={headerStyle}>Width:<input style={propStyle} type="number" step="0.1" value={plot.barWidth} onChange={ev => this.onBarsWidthChanged(plot.id, ev)}></input></label>
              <button style={headerStyle} onClick={ev => this.onAddPointClicked(plot.id)}>Add point</button>
              <button style={headerStyle} onClick={ev => this.onRemovePlotClicked(plot.id)}>Remove this plot</button>
            </div>
          );
        case "boxandwhiskers":
          return (
            <div style={plotRowStyle} key={plot.id}>
              <label>Box and whiskers {plot.id}:</label>
              <label style={headerStyle}>Color:<input style={propStyle} type="text" value={plot.color} onChange={ev => this.onBoxAndWhiskersColorChanged(plot.id, ev)}></input></label>
              <label style={headerStyle}>Size:<input style={propStyle} type="number" value={plot.size} onChange={ev => this.onBoxAndWhiskersSizeChanged(plot.id, ev)}></input></label>
              <button style={headerStyle} onClick={ev => this.onAddPointClicked(plot.id)}>Add point</button>
              <button style={headerStyle} onClick={ev => this.onRemovePlotClicked(plot.id)}>Remove this plot</button>
            </div>
          );
        case "heatmap":
          return (
            <div style={plotRowStyle} key={plot.id}>
              <label>Heatmap {plot.id}:</label>
              <button style={headerStyle} onClick={ev => this.onAddPointClicked(plot.id)}>Add point</button>
              <button style={headerStyle} onClick={ev => this.onRemovePlotClicked(plot.id)}>Remove this plot</button>
            </div>
          );
        default:
          return (<div></div>);
      }
    });
    var axes: IAxis[] = [{ position: "right", kind: "numeric" }, { position: "top", kind: "numeric" }];
    return (
      <div>
        <form onSubmit={ev => this.onAddPlotClicked(ev)}>
          <label>New plot ID:
              <input style={propStyle} type="text" value={this.state.newPlotId} onChange={ev => this.onNewPlotIdChanged(ev)}></input>
          </label>
          <label style={headerStyle}>Kind:
              <select style={optStyle} value={this.state.newPlotKind} onChange={ev => this.onNewPlotKindChanged(ev)}>
              <option value="polyline">Polyline</option>
              <option value="markers">Markers</option>
              <option value="sizedmarkers">Sized markers</option>
              <option value="bars">Bars</option>
              <option value="boxandwhiskers">Box and whiskers</option>
              <option value="heatmap">Heatmap</option>
            </select>
          </label>
          <input style={headerStyle} type="submit" value="Add"></input>
        </form>
        <div style={errorStyle}>{this.state.newPlotError}</div>
        {plotControls}
        <Chart width="600px" height="600px" plots={this.state.plots} axes={axes} />
      </div>
    );
  }

  renderMsagl() {
    return (
      <div>
        <Graph width="600px" height="600px" graph={this.state.graph} />
      </div>
    );
  }

  onMonacoTextChange(json: string) {
    // Example of how to consume changes in Monaco text: this handler will read changes made to the JSON and use them to re-render the plots.
    var plots = this.state.plots;
    try {
      plots = json === "" ? [] : JSON.parse(json);
    }
    catch (exc) { plots = []; }
    this.updateState({ plots: plots, json: json });
  }

  renderCodepadJson() {
    return (
      <div>
        <CodePad width="800px" height="600px" language="json" theme="vs-dark" text={this.state.json} onTextChange={text => this.onMonacoTextChange(text)} />
      </div>
    );
  }

  renderCodepadCrn() {
    return (
      <div>
        <CodePad width="800px" height="600px" language="crn" theme="vs-light" text={this.state.crntext} />
      </div>
    );
  }

  render() {
    var sectionStyle = { border: '1px solid black', padding: 5, margin: 5 };
    var idd = this.renderIDD();
    var msagl = this.renderMsagl();
    var codepadJson = this.renderCodepadJson();
    var codepadCrn = this.renderCodepadCrn();
    return (
      <div>
        <h1>React components testing area</h1>
        <div id="msagl" style={sectionStyle}>
          <h2>reactmsagl sample</h2>
          <div>
            {msagl}
          </div>
        </div>
        <div id="idd" style={sectionStyle}>
          <h2>reactidd sample</h2>
          <div>
            {idd}
          </div>
        </div>
        <div id="monacojson" style={sectionStyle}>
          <h2>reactcodepad (Json) sample</h2>
          <span>Changes to this text are bound to the IDD plot above.</span>
          <div>
            {codepadJson}
          </div>
        </div>
        <div id="monacocrn" style={sectionStyle}>
          <h2>reactcodepad (CRN) sample</h2>
          <span>Demonstrates the use of a custom language for highlighting.</span>
          <div>
            {codepadCrn}
          </div>
        </div>
      </div>
    );
  }
}

ReactDOM.render(
  <ReactTest></ReactTest>,
  document.getElementById('root')
);