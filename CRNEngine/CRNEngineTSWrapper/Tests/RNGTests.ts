// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/// <reference path="../Scripts/typings/jasmine/jasmine.d.ts" />
/// <reference path="../Scripts/typings/requirejs/require.d.ts" />

import Config = require('./Config');

declare var Microsoft: any;

jasmine.DEFAULT_TIMEOUT_INTERVAL = Config.Timeout; //async timeout is set this way

var seed = 123;
var Rng: any;

describe("CRNEngineTsWrapper", () => {
    describe("RNG", () => {
        it("reproduce sequence", () => {
            let count = 1000;

            var rng = Microsoft.Research.CRNEngine.Randomise.get_seeded(seed);

            var obs1: number[] = [];
            for (var i = 0; i < count; i++)
                obs1.push(rng.Next())

            var rng2 = Microsoft.Research.CRNEngine.Randomise.get_seeded(seed);

            var obs2: number[] = [];
            for (var i = 0; i < count; i++)
                obs2.push(rng2.Next())

            for (var i = 0; i < count; i++)
                expect(obs1[i]).toBe(obs2[i]);
        }, Config.Timeout);
        it("JavaScript/.NET  identical sequnces(Next(max) call)", () => {
            //      !!! This test can give false positives  !!!
            //          It contains hardcoded values, if the RngAdpater was switched to another RNG
            //          Or RNG itself is updated, the hardcoded values bellow need to be updated as well
            //
            //          If you update the hardcoded values bellow please update the same values in
            //          corresponding .NET test in /ModellingEngine/CRNEngineTests/RNGTests.fs
            //
            let count = 1000;

            var seq = Microsoft.Research.CRNEngine.Rng.emitTestSequence();

            var expected = [
                2837,
                4351,
                386,
                2208,
                3594,
                5902,
                3612,
                3268,
                797,
                6479
            ];

            for (var i = 0; i < expected.length; i++)
                expect(seq[i]).toBe(expected[i]);
        }, Config.Timeout);
    });
});