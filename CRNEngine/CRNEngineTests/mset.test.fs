// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.MsetTest
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Mset

//let mset = Mset.parse sp |> from_string
let from_string = Parser.from_string <| Mset.parse Parser.name

[<Fact(DisplayName="Mset - simple")>]
let mset_simple () =
  let expected = from_list ["X"; "Y"] in
  let got = "X + Y" |> from_string in
  Assert.Equal<string Mset.t>(expected, got)

[<Fact(DisplayName="Mset - integer product")>]
let mset_integer_product () =
  let expected = from_list ["X"; "X"; "Y"; "Y"; "Y"] in
  let got = "2X + 3Y" |> from_string in
  Assert.Equal<string Mset.t>(expected, got)

[<Fact(DisplayName="Mset - empty")>]
let mset_empty () =
  let expected = empty in
  let got = "" |> from_string in
  Assert.Equal<string Mset.t>(expected, got)