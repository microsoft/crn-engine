/// <reference path="../Scripts/typings/jasmine/jasmine.d.ts" />
/// <reference path="../Scripts/typings/requirejs/require.d.ts" />

import CrnVM = require('../Scripts/src/Scripts/crnVM');
import Monitor = require('../Scripts/src/Scripts/modificationMonitor');
import Config = require('./Config');

jasmine.DEFAULT_TIMEOUT_INTERVAL = Config.Timeout; //async timeout is set this way

function getVMforTest() {
    var crn = new CrnVM.CRN();

    var sp1 = new CrnVM.Initial();
    sp1.species("SP1");
    sp1.value("1");
    var sp2 = new CrnVM.Initial();
    sp2.species("SP2");
    sp2.value("1");
    var r1 = new CrnVM.Reaction();
    r1.reactants.push({ element: "SP1", multiplicity: 1 });
    r1.products.push({ element: "SP2", multiplicity: 1 });
    r1.rate("1");
    crn.initials.push(sp1);
    crn.initials.push(sp2);
    crn.reactions.push(r1);

    expect(crn.reactions().length).toBe(1);

    return crn;
}

describe("CRN vm JSON snapshot based modification monitor", () => {
    it("reacts on settings change", (done: DoneFn) => {
        var mon = new Monitor.JsonSnapshotMonitor();
        var crn = getVMforTest();
        mon.Modified.subscribeOnNext(() => {
            done();
        });
        mon.StartMonitoring(crn);

        crn.settings.Sim.Prune(true);
    }, Config.Timeout);

    it("reacts on species change", (done: DoneFn) => {
        var mon = new Monitor.JsonSnapshotMonitor();
        var crn = getVMforTest();
        var counter = 0;
        mon.Modified.subscribeOnNext(() => {
            ++counter;
            if (counter == 2)
                done();
        });
        mon.StartMonitoring(crn);


        var sp3 = new CrnVM.Initial();
        sp3.species("SP3");
        sp3.value("5");
        crn.initials.push(sp3); //first change
        sp3.value("10"); //second change        
    }, Config.Timeout);

    it("reacts on reactions change", (done: DoneFn) => {
        var mon = new Monitor.JsonSnapshotMonitor();
        var crn = getVMforTest();        
        mon.Modified.subscribeOnNext(() => {           
            done();
        });
        mon.StartMonitoring(crn);

        crn.reactions()[0].rate("20")
    }, Config.Timeout);
});