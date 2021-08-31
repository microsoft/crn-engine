// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Interfaces from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../CRNEngineTSWrapper/Scripts/CRNEngine";
import "../samples.css";
import "./styles.css";

var status = document.getElementById("status");
var parsedRB = <HTMLInputElement>document.getElementById("parsedRadioButton");
var hardcodedRB = <HTMLInputElement>document.getElementById("hardcodedRadioButton");
var serverCB = <HTMLInputElement>document.getElementById("serverCheckBox");

var me: CRNEngine = null;

function appendStatus(header: string, text?: string) {
    var p = document.createElement("p");
    var h = document.createElement("b");
    h.appendChild(document.createTextNode(header));
    p.appendChild(h);
    if (text != null)
        p.appendChild(document.createTextNode(text));
    status.appendChild(p);
}

function clearStatus() {
    while (status.firstChild)
        status.removeChild(status.firstChild);
}

function onRunFail(result: any) {
    appendStatus("fail", result.error);
}

function getHardcoded() {
    var gui: Interfaces.CRN = {
        name: "test",
        settings: {
            stochastic: { scale: 1, steps: null, trajectories: 1 },
            deterministic: { stiff: false, abstolerance: 1E-06, reltolerance: 0.001 },
            spatial: {
                dimensions: 1,
                boundary: "Periodic",
                random: 0.1,
                nx: 31,
                xmax: 1,
                dt: 1.0,
                parameters: [],
                diffusibles: [],
                default_diffusion: 0
            },
            simulation: {
                name: "",
                points: 1000,
                initial: 0,
                final: 1000,
                plots: ["A"],
                plotcolours: [],
                seed: null,
                kinetics: "Contextual",
                times: [],
                multicore: false,
                data: [],
                sweeps: [],
                prune: true
            },
            simulations: [],
            data: [{
                file: "a",
                data: [{ times: [0, 1, 2, 3], columns: [{ name: "A", values: [1000, 910, 800, 600] }] }]
            }],
            units: {
                concentration: { Molar: -9 },
                time: { Seconds: 1 },
                space: { Metres: -3 }
            },
            inference: {
                name: "",
                burnin: 1000,
                samples: 1000,
                thin: 10,
                seed: 0,
                seeds: [],
                noise_parameter: "Random",
                noise_model: "Constant",
                prune: false,
                timer: false,
                partial_evaluation: false,
                print_console: false,
                print_summary: false
            },
            simulator: "Oslo",
            parameters: [{ name: "r", value: 0.1, prior: { interval: "Log", distribution: { Uniform: { min: 0.01, max: 1.0 } }, variation: "Random" } }],
            rates: {},
            sweeps: [{ name: "mySweep", assignments: [{ variables: ["r"], values: [[{ Float: 0.1 }]] }] }],
            plot: {
                x_label: "Time",
                y_label: "Concentration (nM)",
                title: "",
                label_font_size: 16,
                tick_font_size: 16,
                x_ticks: [],
                y_ticks: [],
                x_min: null,
                x_max: null,
                y_min: null,
                y_max: null,
                h_boundaries: [],
                v_boundaries: []
            },
            moment_closure: {
                order: 0,
                initial_minimum: 0,
                log_evaluation: false,
                plots: []
            },
            synthesis: {
                mode: "Multistability",
                solver: "NLSat"
            },
            quiet: false
        },
        reactions: [{
            reactants: [{ element: "A", multiplicity: 1 }],
            products: [],
            catalysts: [],
            rate: { MassAction: "r" }
        }],
        initials: [{ species: "A", value: "1000", constant: false, time: "0" }],
        attributes: {}
    };
    var model: Interfaces.Model = { top: gui, systems: [] };
    var ret: Interfaces.IG = { nodes: { "": model }, edges: {}, expanded: false };
    return ret;
}

function parseData(data: string): Interfaces.Table {
    var rows = data.split("\n").filter(row => row.trim() != "");
    var names = rows[0].split(",");
    var times: number[] = [];
    var columns: Interfaces.Column[] = names.filter((v, i) => i > 0).map(n => { return { name: n.replace("\"", ""), values: [] } });
    for (var i = 1; i < rows.length; i++) {
        var values = rows[i].split(",").map(v => parseFloat(v));
        times.push(values[0]);
        for (var j = 1; j < values.length; j++)
            columns[j - 1].values.push(values[j]);
    }
    return { times: times, columns: columns };
}

function setHeadered(container: HTMLElement, header: string, text: string) {
    while (container.childElementCount > 0)
        container.removeChild(container.firstChild);
    var hbold = document.createElement("b");
    hbold.appendChild(document.createTextNode(header));
    container.appendChild(hbold);
    container.appendChild(document.createTextNode(text));
}

function infer(gui: Interfaces.IG) {
    var server = serverCB.checked;
    me = new CRNEngine(server);
    var observables = me.UserInferGui(gui);
    observables.exports.subscribe(exports => appendStatus("progress (exports): ", JSON.stringify(exports)), onRunFail, () => appendStatus("completed (exports)"));
    observables.inferencechainupdate.subscribe(sampling => setHeadered(document.getElementById("posterior"), "progress (posterior): ", JSON.stringify(sampling)), onRunFail, () => appendStatus("completed (posterior)"));
    observables.summary.subscribe(summary => setHeadered(document.getElementById("summary"), "progress (summary): ", summary.summary), onRunFail, () => appendStatus("completed (summary)"));
    observables.instances.subscribe(instances => appendStatus("progress (instances): ", JSON.stringify(instances)), onRunFail, () => appendStatus("completed (instances)"));
    observables.inferenceresults.subscribe(results => setHeadered(document.getElementById("results"), "progress (results): ", JSON.stringify(results)), onRunFail, () => appendStatus("completed (results)"));
    observables.parameterresults.subscribe(results => setHeadered(document.getElementById("parameters"), "progress (parameters): ", JSON.stringify(results)), onRunFail, () => appendStatus("completed (parameters)"));
    observables.parameterdefinitions.subscribe(results => appendStatus("progress (parameterDefinitions): ", JSON.stringify(results)), onRunFail, () => appendStatus("completed (parameterDefinitions)"));
    observables.progress.subscribe(results => setHeadered(document.getElementById("progress"), "progress (progress): ", JSON.stringify(results)), onRunFail, () => appendStatus("completed (progress)"));
}

import sample from "raw-loader!../test_inference.txt";
import data from "raw-loader!./AM_obs_noised.csv";

function runClicked() {
    clearStatus();
    appendStatus("start");

    if (hardcodedRB.checked) {
        var gui = getHardcoded();
        infer(gui);
    }
    else {
        var server = serverCB.checked;
        me = new CRNEngine(server);
        var parseObs = me.UserParseCode(sample);
        parseObs.model.subscribe(model => {
            model.nodes[""].top.settings.data = [{ file: "AM_obs_noised", data: [parseData(data)] }];
            infer(model);
        });
    }
}

function stopClicked() {
    if (me != null) {
        me.Abort();
        me = null;
    }
}

document.getElementById("runButton").onclick = runClicked;
document.getElementById("stopButton").onclick = stopClicked;