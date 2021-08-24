import * as crnVM from "../CRNComponent/Scripts/crnVM";
import * as CRNSettings from '../CRNComponent/Scripts/crnSettings';
import { WebSharperGeneratedInterfaces as WGI } from '../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces';

var crn = new crnVM.CRN();

var settings: WGI.Microsoft.Research.CRNEngine.Crn_settings<string> = {
    "stochastic": {
        "scale": 1,
        "trajectories": 1
    },
    "deterministic": {
        "stiff": false,
        "abstolerance": 0.000001,
        "reltolerance": 0.001
    },
    "spatial": {
        "parameters": [],
        "dimensions": 1,
        "diffusibles": [],
        "boundary": "Periodic",
        "xmax": 0,
        "nx": 0,
        "dt": 0,
        "default_diffusion": 0,
        "random": 0
    },
    "simulation": {
        "name": "",
        "points": 1000,
        "initial": 0,
        "final": 1000,
        "plots": [
            "species_0",
            "species_1",
            "species_2",
            "species_3",
            "species_4",
            "species_5",
            "species_6",
            "species_7",
            "species_8",
            "species_9",
            "species_10"
        ],
        "plotcolours": [],
        "kinetics": "Contextual",
        "sweeps": [],
        "times": [],
        "multicore": false,
        "data": []
    },
    "simulations": [],
    "data": [],
    "units": {
        "concentration": { "Molar": -9 },
        "time": { "Seconds": 0 },
        "space": { "Metres": -3 }
    },
    "inference": {
        "name": "",
        "burnin": 100,
        "samples": 100,
        "thin": 10,
        "noise_model": "Constant",
        "noise_parameter": "Random",
        "prune": false,
        "seed": 0,
        "seeds": [],
        "timer": false,
        "partial_evaluation": false,
        "print_console": false,
        "print_summary": false
    },
    "moment_closure": {
        "order": 0,
        "initial_minimum": 0,
        "log_evaluation": false,
        "plots": []
    },
    synthesis: {
        mode: "Multistability",
        solver: "NLSat"
    },
    "simulator": "Oslo",
    "parameters": [],
    "sweeps": [],
    "rates": {},
    "plot": {
        "x_label": "",
        "y_label": "",
        "title": "",
        "label_font_size": 0,
        "tick_font_size": 0,
        "x_ticks": [],
        "y_ticks": [],
        "h_boundaries": [],
        "v_boundaries": []
    }
};

crn.settings.source = settings;

var sp1 = new crnVM.Initial(crn);
sp1.species("Species with a very long name");
sp1.structural("<a>");
sp1.svg("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"125.000000\" height=\"61.000000\" stroke=\"black\">\n<style> path { stroke: silver; stroke-width: 2.500000; stroke-linejoin: round; fill: none; } text { stroke: black; fill: black; stroke-width: 0; font-family: Verdana,Arial,sans-serif; font-size: 15px; text-anchor: middle; }  </style>\n<text x=\"62.000000\" y= \"42.000000\" transform=\"rotate(0.000000 62.000000,42.000000)\" dy=\"6\" class=\"normal_text\">a</text>\n<path d=\"M 30.000000 30.000000 L 94.000000 30.000000 L 85.431112 35.155014\" class=\"normal\"/>\n</svg>");
sp1.value("1");
var sp2 = new crnVM.Initial(crn);
sp2.species("SP2");
sp2.structural("<b>");
sp2.svg("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"125.000000\" height=\"61.000000\" stroke=\"black\">\n<style> path { stroke: silver; stroke-width: 2.500000; stroke-linejoin: round; fill: none; } text { stroke: black; fill: black; stroke-width: 0; font-family: Verdana,Arial,sans-serif; font-size: 15px; text-anchor: middle; }  </style>\n<text x=\"62.000000\" y= \"42.000000\" transform=\"rotate(0.000000 62.000000,42.000000)\" dy=\"6\" class=\"normal_text\">b</text>\n<path d=\"M 30.000000 30.000000 L 94.000000 30.000000 L 85.431112 35.155014\" class=\"normal\"/>\n</svg>");
sp2.value("1");
var sp3 = new crnVM.Initial(crn);
sp3.species("SP3");
sp3.structural("<b>");
sp3.svg("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"125.000000\" height=\"61.000000\" stroke=\"black\">\n<style> path { stroke: silver; stroke-width: 2.500000; stroke-linejoin: round; fill: none; } text { stroke: black; fill: black; stroke-width: 0; font-family: Verdana,Arial,sans-serif; font-size: 15px; text-anchor: middle; }  </style>\n<text x=\"62.000000\" y= \"42.000000\" transform=\"rotate(0.000000 62.000000,42.000000)\" dy=\"6\" class=\"normal_text\">c</text>\n<path d=\"M 30.000000 30.000000 L 94.000000 30.000000 L 85.431112 35.155014\" class=\"normal\"/>\n</svg>");
sp3.value("1");
var r1 = new crnVM.Reaction();
r1.reactants.push({ element: "Species with a very long name", multiplicity: 2 });
r1.products.push({ element: "SP2", multiplicity: 1 });
r1.rate("1");
r1.reverseRate("Long reverse rate value");
var r2 = new crnVM.Reaction();
r2.reactants.push({ element: "SP2", multiplicity: 1 });
r2.products.push({ element: "SP3", multiplicity: 2 });
r2.rate("2");
crn.initials.push(sp1);
crn.initials.push(sp2);
crn.initials.push(sp3);
crn.reactions.push(r1);
crn.reactions.push(r2);

sp1.plot(true);
sp3.plot(true);

var sweep1 = new CRNSettings.CRNSweep();
sweep1.Name("Sweep1");

var assignment11 = {
    variables: ["N"],
    values: [[{ Float: 0.0 }, { Float: 0.2 }]]
}
var assignment12 = {
    variables: ["T"],
    values: [[{ Float: 3800.0 }, { Key: "3800.0 + (100.0 * N))" }]]
}

var assignment13 = {
    variables: ["N1", "T1"],
    values: [[{ Float: 0.0 }, { Float: 0.2 }, { Float: 0.4 }, { Float: 0.6 }], [{ Float: 3800.0 }, { Float: 2700.0 }, { Float: 1600.0 }, { Float: 3700.0 }]]
}
sweep1.Assignments.push(assignment11, assignment12, assignment13);

var sweep2 = new CRNSettings.CRNSweep();
sweep2.Name("Sweep2");

var assignment21 = {
    variables: ["N"],
    values: [[{ Float: 1.0 }, { Float: 2.0 }, { Float: 3.0 }, { Float: 4.0 }]]
}
sweep2.Assignments.push(assignment21);

var sweep3 = new CRNSettings.CRNSweep();
sweep3.Name("Sweep3");

var assignment31 = {
    variables: ["T"],
    values: [[{ Float: 1000.0 }, { Float: 2000.0 }]]
}
sweep3.Assignments.push(assignment31);

crn.settings.Sweeps.push(sweep1, sweep2, sweep3);

let parameters = new CRNSettings.ParameterVM();
parameters.interval("Real");
parameters.intervalSelect = ["Real", "Log"];
parameters.max(100);
parameters.min(20);
parameters.name("Parameter #1");
parameters.value(100500);
parameters.variation("Random");
parameters.variationSelect = ["Random", "Fixed", "Initial", "Multiple"];

crn.settings.Parameters.push(parameters);

let model = new crnVM.Model();
model.Top(crn);
model.SelectedCRN(crn);

let graph = new crnVM.InferenceGraph(null);
graph.Nodes([model]);
graph.SelectedNode(model);

export default graph