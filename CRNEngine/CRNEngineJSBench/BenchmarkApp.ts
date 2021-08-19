var Microsoft: any;

var Benchmark: any;
var CVode: any;
var Module: any;

let CrnEngine = Microsoft.Research.CRNEngine;

var resultsTable: any;

<any>$(document).ready(function () {
    resultsTable = (<any>$('table#benchmarkResultsTable')).DataTable({
        columns: [
            { title: "Name", data: "Name" },
            { title: "Min (sec)", data: "Min" },
            { title: "Mean (sec)", data: "Mean" },
            { title: "Median (sec)", data: "Median" },
            { title: "StdDev (sec)", data: "StdDev" },
            { title: "Max (sec)", data: "Max" },
            { title: "Measurement runs count", data: "SampleCount" }
        ]
    });

    (<any>$('#platform')).text(Benchmark.platform);
});



function benchmarkJS() {
    resultsTable.clear().draw();

    var consoleResultOutput = function (r: BenchmarkResult) {
        console.log(r.Name + ": Min " + r.Min.toPrecision(4) + '; Mean ' + r.Mean.toPrecision(4) + '; Median ' + r.Median.toPrecision(4) + '; Max ' + r.Max.toPrecision(4) + '; StdDev ' + r.StdDev.toPrecision(4) + '; Count ' + r.SampleCount);
    }

    var tableResultOutput = function (r: BenchmarkResult) {
        //console.log('table raw adding');
        resultsTable.row.add(r).draw(false);
        //console.log('table raw added');
    }

    var jointResultOutput = function (r: BenchmarkResult) {
        consoleResultOutput(r);
        tableResultOutput(r);
    }

    console.log("starting...");
    RunAll(jointResultOutput);
}

CVode().then(function (compiled: any) {
    console.log("Sundials wasm is ready (loaded and compiled)");
    (<any>window).Module = compiled; //saving to global var
});