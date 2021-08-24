// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.InferenceTest

open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine;
open Microsoft.Research.CRNEngine.Inference

[<Fact(DisplayName="Inference McmcSummary - serialization")>]
let mcmcSummarySerialization () = 
  let ms : McmcSummary = { seed = 12u; burnin = 120; samples = 150; thin = 10; mle = -26.001; aic = 23.0; bic = 340.0; dic = 123.0; numData = 40 }
  
  let expected   = ms
  let serialized = ms.to_string ()
  let got        = serialized |> McmcSummary.from_string 
  Assert.Equal<McmcSummary> (expected, got)

[<Fact(DisplayName="Inference Summary - serialization")>]
let inferenceSummarySerialization () = 
  let ms : McmcSummary = { seed = 14u; burnin = 120; samples = 150; thin = 10; mle = -26.001; aic = 23.0; bic = 340.0; dic = 123.0; numData = 40 }
  let ps1 = ParameterSummary.create("test1", 1.5, 2.5, 3.0, 4.3, 5.0, 6.5, 7.0, 8.0, 9.0, 10.0, Interval.Log)
  let ps2 = ParameterSummary.create("test2", 1.5, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, Interval.Real)
  let ps3 = ParameterSummary.create("test3", 1.5, 2.5, 3.0, 4.2, 5.0, 6.8, 7.0, 8.0, 9.0, 10.0, Interval.Real)
  
  let summary = { mcmc=ms; parameters= Map.ofList["test1",ps1; "test2",ps2; "test3",ps3] }

  let expected   = summary
  let serialized = summary.to_string ()
  let got        = serialized |> Summary.from_string 
  Assert.Equal<Summary> (expected, got)

