// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.ParameterSummaryTest

open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

[<Fact(DisplayName="Parameter summary - serialization")>]
let parameterSummarySerialization () = 
  let ps = ParameterSummary.create("ns.test", 1.5, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, Interval.Real)
  
  let expected   = ps
  let serialized = ps         |> ParameterSummary.to_string 
  let got        = serialized |> ParameterSummary.from_string
  Assert.Equal<ParameterSummary> (expected, got)