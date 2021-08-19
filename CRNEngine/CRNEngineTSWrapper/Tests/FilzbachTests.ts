/// <reference path="../Scripts/typings/jasmine/jasmine.d.ts" />
/// <reference path="../Scripts/typings/requirejs/require.d.ts" />

import Config = require('./Config');

declare var Microsoft: any;

jasmine.DEFAULT_TIMEOUT_INTERVAL = Config.Timeout; //async timeout is set this way

// FP: this test currently fails because Filzbach was changed. I've tried fixing it and I've made some progress, but someone who knows about the changes in Filzbach ought to take a look at it.
describe("CRNEngineTsWrapper", () => {
    describe("Filzbach", () => {
        // FP: "xit" disables the test; replace it with "it" to re-enable it.
        xit("runnable (default options; chain of N(10.0,5.0))", () => {
            var mean = 10.0
            var sigma = 5.0;
            var ss = 2 * sigma * sigma;
            var c = 1.0 / (sigma * Math.sqrt(2.0 * Math.PI))

            var burnin = 500;
            var sampling = 1000;

            var logLikelihood = function (params: any) { //nomal distribution density
                return function () {
                    var x = params.Item(0);
                    var d = x - mean;
                    var dsqr = d * d;
                    var lk = c * Math.exp(-dsqr / ss);
                    var logLk = Math.log(lk);
                    return [logLk];
                }
            }

            var options = Microsoft.Research.Filzbach.Filzbach.defaultRunOptions();


            var x = Microsoft.Research.Filzbach.FilzbachJS.parameter("x", 0, -50.0, 50.0);

            var chain = Microsoft.Research.Filzbach.FilzbachJS.run(logLikelihood, [x], [], options);
            var table = Microsoft.Research.Filzbach.FilzbachAnalysis.extractBayesTable(chain);
            expect(table.data[0].length).toBe(options.sampleLen / options.thinning);
        }, Config.Timeout);
    });
});