// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

declare class BenchmarkResult {
    Name: string;
    Min: number;
    Max: number;
    Mean: number;
    Median: number;
    StdDev: number;
    SampleCount: number;
    constructor(Name: string, Min: number, Max: number, Mean: number, Median: number, StdDev: number, SampleCount: number);
}
declare function BenchmarkResultFactory(target: any): BenchmarkResult;
interface ITestStatusCallback {
    (result: BenchmarkResult): void;
}
declare function RunAll(testCompleteCallback: ITestStatusCallback): void;
