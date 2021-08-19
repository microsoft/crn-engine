class BenchmarkResult {
    constructor(public Name: string, public Min: number, public Max: number, public Mean: number, public Median: number, public StdDev: number, public SampleCount: number) { };
}

function BenchmarkResultFactory(target: any) {
    var sample = target.stats.sample.slice();
    sample.sort();
    return new BenchmarkResult(
        target.name,
        sample[0],
        sample[sample.length - 1],
        target.stats.mean, sample[Math.floor(sample.length / 2)],
        target.stats.deviation,
        sample.length);
}

interface ITestStatusCallback {
    (result: BenchmarkResult): void
}

function RunAll(testCompleteCallback: ITestStatusCallback) {
    var suite = new Benchmark.Suite;
    suite
        //the list of tests
        .add("Simuate AM (OSLO)", Tests.Simulate_Oslo_AM)
        .add("Simuate AM (Sundials)", Tests.Simulate_Sundials_AM)
        .add("Simuate Waves (OSLO)", Tests.Simulate_Oslo_Waves)
        .add("Simuate Waves (Sundials)", Tests.Simulate_Sundials_Waves)
        .add("Simuate AM Functional (OSLO)", Tests.Simulate_Oslo_AM_functional)
        .add("Simuate AM Functional (Sundials)", Tests.Simulate_Sundials_AM_functional)
        .add("Infer AM (OSLO) 1000", Tests.Infer_Oslo_AM_1000)
        .add("Infer AM (Sundials) 1000", Tests.Infer_Sundials_AM_1000)
        .add("Infer AM (OSLO) 10000", Tests.Infer_Oslo_AM_10000)
        .add("Infer AM (Sundials) 10000", Tests.Infer_Sundials_AM_10000)
        //configuring callbacks
        .on('cycle', function (event: any) {
            if (!event.target.aborted) {
                console.log(event.target.toString());
                var result = BenchmarkResultFactory(event.target);
                testCompleteCallback(result);
            }
        })
        .on('error', function (event: any) { console.warn(event.target + " Error during execution \"" + event.target.error + "\""); })
        .on('complete', function (event: any) {
            console.log("Done");
        })
        //dispatching run
        .run({ async: 'true' });
}