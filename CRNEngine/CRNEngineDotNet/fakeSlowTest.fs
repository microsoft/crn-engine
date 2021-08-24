// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.


//We have to use the slow attribute once in each test project or the test fail on CI
//(this likely to get fixed so periodically try removing this!)
//Related to:
//https://github.com/xunit/xunit/issues/180
(*open Xunit
open FsUnit.Xunit

[<Trait("Category", "Slow")>]
[<Fact(DisplayName="Fake slow test")>]
let fakeSlowTest() = 
  ()*)

module stub

let x = 2