// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.Deterministic_settingsTest
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

[<Fact(DisplayName="Deterministic settings - ode settings")>]
let ode_settings () = 
  let expected = { Deterministic_settings.defaults with stiff = true } in
  let s:Deterministic_settings = Deterministic_settings.from_string "{ stiff=true }" in
  Assert.Equal<Deterministic_settings>(expected, s)

[<Fact(DisplayName="Deterministic settings - ode settings - multi")>]
let ode_settings_multi () = 
  let expected = { Deterministic_settings.defaults with stiff = true; reltolerance=1e-5 } in
  let s:Deterministic_settings = Deterministic_settings.from_string "{ stiff=true; reltolerance=1e-5 }" in
  Assert.Equal<Deterministic_settings>(expected, s)
