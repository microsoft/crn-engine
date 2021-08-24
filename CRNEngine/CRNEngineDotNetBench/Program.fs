// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace CRNEngineDotNetBench

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Jobs

module runner = 
    open holder

    let runInferenceBenchmark() =
        //http://benchmarkdotnet.org/Configs/Jobs.htm#run
        //A default inference benchmark takes a very long time, increase uncertainity for faster runs locally
        let job =
            Job.Default
                .WithIterationCount(1)
                .WithUnrollFactor(2)
                .WithInvocationCount(4)
                .WithGcForce(true)

        let config =
            ManualConfig
                .Create(DefaultConfig.Instance)
                .With(job)

        BenchmarkRunner.Run<InferenceBenchmark>(config)
        //BenchmarkRunner.Run<CharacterizationBenchmark>(config)

    [<EntryPoint>]
    let main argv =

        //Benchmarks run in their own process which if they error makes using the debugger
        //more awkward. When creating a new benchmark it's helpful to run the code directly first (or as unit tests!)

        //let simulationBenchmark = SimulationBenchmark()
        //simulationBenchmark.Oslo() |> ignore

        let summarySimulation = BenchmarkRunner.Run<SimulationBenchmark>();
        //let lambdaSimulation = BenchmarkRunner.Run<LambdaBenchmark>();
        //let expressionSimulation = BenchmarkRunner.Run<ExpressionBenchmark>();
        //let functionalRate = BenchmarkRunner.Run<FunctionalRateBenchmark>();
        //let inference = BenchmarkRunner.Run<InferenceBenchmark>();
        //runInferenceBenchmark() |> ignore

        //ExpressionBenchmark().AsLambda()

        //let x = holder.LambdaBenchmark()
        //x.LambdaNewtDict() |> ignore

        //Inference benchmarking is time consuming
        //let x = CharacterizationBenchmark()
        //x.Run() |> ignore
        
        //runInferenceBenchmark()

        (*let x = FunctionalRateBenchmark() 
        x.SetupData()
        x.ByExpressionSystem()
        x.DirectFSharp()*)

        
        

        0
