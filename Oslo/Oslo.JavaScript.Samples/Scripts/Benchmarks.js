// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

importScripts("WebSharper/IntelliFactory.JavaScript/Runtime.js");
importScripts("WebSharper/IntelliFactory.WebSharper.js");
importScripts("WebSharper/Oslo.FSharp.WebSharper.js");
importScripts("WebSharper/Oslo.FSharp.Samples.js");

var IterationCount = 60;
var IterationSize = 20;

IntelliFactory.Runtime.Start();

self.postMessage({ text: "Benchmarking OscDNA using GearBDF" });

var start = new Date().getTime();
var dna = Oslo.FSharp.GearSamples.DNA_init();
var count = 0;
for (var i = 0; i < 5; i++)
{
    dna = Oslo.FSharp.GearSamples.DNA_solve(dna, 250)[4];
    count += 250;
    var end = new Date().getTime();
    self.postMessage({ text: "OscDNA using GearBDF: " + count + " points in " + (end - start) + " ms" });
}

self.postMessage({ text: "Benchmarking RM using R-K" });

start = new Date().getTime();
var rm = Oslo.FSharp.Samples.RM_init();
count = 0;
for (var i = 0; i < 5; i++) {
    rm = Oslo.FSharp.Samples.RM_solve(rm, 20000)[3];
    count += 20000;
    var end = new Date().getTime();
    self.postMessage({ text: "RM using R-K: " + count + " points in " + (end - start) + " ms" });
}
