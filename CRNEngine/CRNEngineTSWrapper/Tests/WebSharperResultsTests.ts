// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import Config = require('./Config');

declare var Microsoft: any;

jasmine.DEFAULT_TIMEOUT_INTERVAL = Config.Timeout; //async timeout is set this way

/*
 * These tests are for automated build debugging
 */

describe("WebSharper", () => {
    describe("output", () => {
        it("contains Microsoft.Research namespace", () => {
            expect(Microsoft.Research).toBeDefined("Web sharper skipped entirty Microsoft.Research namespace. It is absent in the output file.");
            console.log("Microsoft.Research namespace is present. It contains following objects/namespaces:");
            for (var prop in Microsoft.Research) {
                console.log(prop);
            }
            console.log("end of Microsoft.Research properties");
        });
        it("contains Microsoft.Research.CRNEngine namespace", () => {
            expect(Microsoft.Research.CRNEngine).toBeDefined("Web sharper skipped entirty Microsoft.Research.CRNEngine namespace. It is absent in the output file.");
            console.log("Microsoft.Research.CRNEngine namespace is present.");
            console.log("Type of Microsoft.Research.CRNEngine is " + typeof (Microsoft.Research.CRNEngine));
            console.log("It contains following objects/namespaces:");
            for (var prop in Microsoft.Research.CRNEngine) {
                console.log(prop);
            }
            console.log("end of Microsoft.Research.CRNEngine properties");
        });
        it("contains Microsoft.Research.CRNEngine.Randomise module", () => {
            expect(Microsoft.Research.CRNEngine.Randomise).toBeDefined();
        });
    });
});