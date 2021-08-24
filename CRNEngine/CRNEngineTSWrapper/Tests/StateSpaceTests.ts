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
            xmax: 1,
            nx: 31,
            dt: 1,
            parameters: [],
            diffusibles: [],
            default_diffusion: 0
        },
        simulation: {
            points: 1000,
            initial: 0,
            final: 1000,
            plots: [],
            kinetics: "Contextual",
            times: [],
            prune: false,
            multicore: false,
            data: [],
            sweeps: []
        },
        simulations: [],
        data: [],
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
        parameters: [],
        sweeps: [],
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
        "reactants": [{ element: "species_1", multiplicity: 1 }, { element: "species_0", multiplicity: 1 }],
        "products": [{ element: "species_2", multiplicity: 1 }],
        "catalysts": [],
        "rate": { MassAction: "0.003" }
    }],
    initials: [{
        "species": "species_0",
        "value": "1.0",
        "constant": false,
        "time": "0.0"
    },
    {
        "species": "species_1",
        "value": "1.0",
        "constant": false,
        "time": "0.0"
    },
    {
        "species": "species_2",
        "value": "1.0",
        "constant": false,
        "time": "0.0"
    }],
    attributes: {}
};


describe("CRNEngineTsWrapper", () => {
    describe("ModellingEngineLayer", () => {
        it("UserStateSpace promise resolves successfully", (done: DoneFn) => {
            var me = new CRNEngine();

            var promise = me.UserStateSpace(gui);
            promise.subscribe(data => console.log("Progress: " + JSON.stringify(data)),
                error => {
                    fail("Fail with: " + JSON.stringify(error));
                    done();
                },
                () => {
                    expect(true).toBe(true); //as the chutzpah fails the build if there is no expectations
                    console.log("Completed");
                    done();
                });
        }, Config.Timeout);

        it("UserStateSpace general results structure", (done: DoneFn) => {
            //If this test fails most likely that the structure of the object that comes from backend is not longer correspond to interfaces in interfaces.ts
            var me = new CRNEngine();

            var promise = me.UserStateSpace(gui);
            promise.subscribe(data => {
                expect(data.states.length).toBeGreaterThan(0);
                expect(data.states[0].transitions.length).toBeGreaterThan(0);
                expect(data.states[0].transitions[0].target).toBeDefined();
                expect(data.states[0].transitions[0].propensity).toBeDefined();
                expect(data.states[0].species).toBeDefined();
            },
                error => {
                    fail("Fail with: " + JSON.stringify(error));
                    done();
                },
                () => {
                    expect(true).toBe(true); //as the chutzpah fails the build if there is no expectations
                    console.log("Completed");
                    done();
                });
        }, Config.Timeout);
    });
});