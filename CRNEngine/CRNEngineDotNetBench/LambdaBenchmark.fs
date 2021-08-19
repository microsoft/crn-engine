namespace CRNEngineDotNetBench

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Toolchains.InProcess;
open BenchmarkDotNet.Environments
open BenchmarkDotNet.Toolchains.CsProj
open BenchmarkDotNet.Exporters

open Microsoft.Research
open Microsoft.Research.CRNEngine
open System.Runtime.Versioning
open BenchmarkDotNet.Environments

module holder =
    let dict = System.Collections.Generic.Dictionary<_,_>()
    let inline dictFunction name = dict.[name]

    let mappy = [(1, 5.0);(2, 3.4);(3, 7.3)] |> Map.ofList
    let inline mappyFunction name = mappy.[name]

    let inline testFunction lookup =
        lookup 1
    
    let sampleFunctionRaw = testFunction

    let sampleFunction = Lambda testFunction

    let refmapFunction = ref mappyFunction
    let refdictfunction = ref dictFunction

    [<MemoryDiagnoser>]
    type LambdaBenchmark() =

        do
            dict.[1] <- 5.0
            dict.[2] <- 3.4
            dict.[3] <- 7.3

        [<Benchmark>]
        member this.LambdaKeyMap() =
            sampleFunction.key mappyFunction
        
        [<Benchmark>]
        member this.LambdaKeyDict() =
            sampleFunction.key dictFunction

        [<Benchmark>]
        member this.LambdaKeyRefMap() =
            sampleFunction.keyref refmapFunction
        
        [<Benchmark>]
        member this.LambdaKeyRefDict() =
            sampleFunction.keyref refdictfunction

        [<Benchmark>]
        member this.LambdaDirectFunctionalMap() =
            testFunction mappyFunction
    
        [<Benchmark>]
        member this.LambdaDirectFunctionalDict() =
            testFunction dictFunction