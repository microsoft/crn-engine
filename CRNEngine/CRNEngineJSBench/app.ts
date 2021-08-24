// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

var Microsoft: any;

let CRNEngine = Microsoft.Research.CRNEngine;

//2016-09-30, 1000, Colin's machine - 4902.32, 4935.09, 4940.22

function benchmark() {
    let program = document.getElementById('program').textContent;

    let crn = CRNEngine.JSAPI.parse_code(program);
    let ode = crn.top.to_ode();
    let oslo = ode.to_oslo(ode);

    let t0 = performance.now();

    for (let i = 0; i < 1000; i++) {
        let results = oslo.simulate();
    }

    let t1 = performance.now();

    console.log(t1 - t0);
}