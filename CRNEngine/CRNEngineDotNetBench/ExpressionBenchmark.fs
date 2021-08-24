// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

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


[<MemoryDiagnoser>]
type ExpressionBenchmark() =

    let dict = System.Collections.Generic.Dictionary<_,_>()
    let dictFunction name = dict.[name]

    let expression = Expression.Times([Expression.Float 0.1; Expression.Float 4.0; Expression.Float 2.1])

    let lambda = Expression.to_lambda expression
    let lambdaSimplied = expression |> Expression.simplify |> Expression.to_lambda

    let dictFunctionRef = ref dictFunction

    do
        dict.[1] <- 5.0
        dict.[2] <- 3.4
        dict.[3] <- 7.3

    [<Benchmark>]
    member ___.StraightEval() =
        Expression.eval dictFunction expression

    [<Benchmark>]
    member ___.AsLambda() =
        lambda.key dictFunction

    [<Benchmark>]
    member ___.AsLambdaRef() =
        lambda.keyref dictFunctionRef

    [<Benchmark>]
    member ___.AsLambdaSimplifiedRef() =
        lambdaSimplied.keyref dictFunctionRef