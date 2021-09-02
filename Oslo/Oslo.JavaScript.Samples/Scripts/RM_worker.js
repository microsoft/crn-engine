// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

importScripts("WebSharper/IntelliFactory.JavaScript/Runtime.js");
importScripts("WebSharper/IntelliFactory.WebSharper.js");
importScripts("WebSharper/Oslo.FSharp.WebSharper.js");
importScripts("WebSharper/Oslo.FSharp.Samples.js");

var IterationCount = 250; // Number of outputs from worker
var IterationSize = 100; // Number of points in each output

IntelliFactory.Runtime.Start();

var rm = Oslo.FSharp.Samples.RM_init();

for (var i = 0; i < IterationCount; i++) {
    var chunk = Oslo.FSharp.Samples.RM_solve(rm, IterationSize);

    var t = chunk[0];
    var step = new Array(t.length - 1);
    var ts = new Array(t.length - 1);
    for (var j = 0;j<t.length - 1;j++) {
        ts[j] = (t[j] + t[j+1])/2;
        step[j] = t[j+1] - t[j];
    }

    self.postMessage({
        t: t,
        x1: chunk[1],
        x2: chunk[2],
        ts: ts,
        step: step
    });

    rm = chunk[3];
}
           
