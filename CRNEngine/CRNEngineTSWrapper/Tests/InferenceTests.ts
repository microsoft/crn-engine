// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/// <reference path="../Scripts/typings/jasmine/jasmine.d.ts" />
/// <reference path="../Scripts/typings/requirejs/require.d.ts" />

import Interfaces = require("../Scripts/src/Interfaces");
import CRNEngine = require("../Scripts/src/CRNEngine");

import Config = require('./Config');

jasmine.DEFAULT_TIMEOUT_INTERVAL = Config.Timeout; //async timeout is set this way

var gui: Interfaces.CRN = {
    name: "test",
    settings: {
        stochastic: { scale: 1, seed: null, steps: null, trajectories: 1 },
        deterministic: { stiff: false, abstolerance: 1E-06, reltolerance: 0.001 },
        spatial: {
            dimensions: 1,
            boundary: "Periodic",
            random: 0.1,
            nx: 31,
            xmax: 1,
            dt: 1,
            parameters: [],
            diffusibles: [],
            default_diffusion: 0
        },
        simulation: {
            points: 1000,
            initial: 0,
            final: 1000,
            plots: ["A"],
            kinetics: "Contextual",
            times: [],
            prune: false,
            multicore: false,
            data: [],
            sweeps: []
        },
        simulations: [],
        data: [
            {
                file: "a",
                data: [{ times: [0, 1, 2, 3], columns: [{ name: "A", values: [1000, 910, 800, 600] }] }]
            }
        ],
        units: {
            concentration: { Molar: -9 },
            time: { Seconds: 1 },
            space: { Metres: -3 }
        },
        inference: {
            name: "",
            burnin: 100,
            samples: 100,
            thin: 10,
            seed: 0,
            noise_parameter: "Random",
            noise_model: "Constant",
            prune: false,
            timer: false,
            partial_evaluation: false
        },
        simulator: "Oslo",
        parameters: [{ name: "r", value: 0.1, prior: { interval: "Log", distribution: { Uniform: { min: 0.01, max: 1.0 } }, variation: "Random" } }],
        sweeps: [{ name: "mySweep", assignments: [{ variables: ["r"], values: [[{ Float: 0.1 }]] }] }],
        rates: {},
        plot: {
            x_label: "Time",
            y_label: "Concentration (nM)",
            title: "",
            label_font_size: 16,
            tick_font_size: 16,
            x_ticks: [],
            y_ticks: []
        },
        moment_closure: {
            order: 0,
            initial_minimum: 0,
            log_evaluation: false,
            plots: []
        }
    },
    reactions: [{
        reactants: [{ element: "A", multiplicity: 1 }],
        products: [],
        catalysts: [],
        rate: { MassAction: "r" }
    }],
    initials: [{ species: "A", value: "1000", constant: false, time: "0" }],
    attributes: { "A": { name: "A", structure: "", svg: "" } }
};


describe("CRNEngineTsWrapper", () => {
    describe("ModellingEngineLayer - Inference", () => {
        it("UserInferGui promise resolves successfully", (done: DoneFn) => {
            var me = new CRNEngine();

            var promise = me.UserInferGui(gui);
            promise.inferenceresults.subscribe(data => console.log("Progress: " + JSON.stringify(data)),
                error => {
                    fail("Fail with: " + JSON.stringify(error))
                    done();
                },
                () => {
                    expect(true).toBe(true); //as the chutzpah fails the build if there is no expectations
                    console.log("Completed");
                    me.Dispose();
                    done();
                });
        }, Config.Timeout);

        /*it("progress reported", (done) => {
            var me = new CRNEngine();

            var promise = me.UserInferGui(gui);
            var counter = 0;
            promise.progress.subscribe(data => {
                expect(data).toBe(++counter);                
                },
                error => {
                    fail("Fail with: " + JSON.stringify(error))
                    done();
                },
                () => {
                    expect(true).toBe(true); //as the chutzpah fails the build if there is no expectations
                    console.log("Completed");
                    expect(counter).toBe(200); //as hardcoded in "gui" variable
                    done();
                });
        }, Config.Timeout);*/

    });
});