// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/// <reference path="../Scripts/typings/jasmine/jasmine.d.ts" />
/// <reference path="../Scripts/typings/requirejs/require.d.ts" />

import Config = require('./Config');

jasmine.DEFAULT_TIMEOUT_INTERVAL = Config.Timeout; //async timeout is set this way

describe("CRN Component sample code", () => { 
    it("fills up the crnVM object", (done: DoneFn) => {
        require(["Scripts/CRN/Scripts/crnVM"], (crnVM:any) => {

            //the code below exactly the same as in the sample page App.ts
            //This tests ensure sample page at least assembles crnVM without exceptions

            var crn = new crnVM.CRN();

            var sp1 = new crnVM.Initial();
            sp1.species("SP1");
            sp1.value("1");
            var sp2 = new crnVM.Initial();
            sp2.species("SP2");
            sp2.value("1");
            var r1 = new crnVM.Reaction();
            r1.reactants.push({ species: "SP1", count: 1 });
            r1.products.push({ species: "SP2", count: 1 });
            r1.rate("1");
            crn.initials.push(sp1);
            crn.initials.push(sp2);
            crn.reactions.push(r1);

            expect(crn.reactions().length).toBe(1);

            done();
        });
    }, Config.Timeout);    
});
