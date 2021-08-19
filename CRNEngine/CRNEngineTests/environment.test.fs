module Microsoft.Research.CRNEngine.Tests.EnvironmentTest
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine.Environment
open Microsoft.Research.CRNEngine

[<Fact(DisplayName="Environment - extend, empty")>]
let extend_empty () = 
  let e:t = create ["A",1.0;"B",2.0] in
    Assert.Equal<t>(e,extend empty e)

[<Fact(DisplayName="Environment - extend, overwrite")>]
let extend_overwrite () = 
  let e1:t = create ["A",1.0;"B",2.0] in
  let e2:t = create ["B",3.0;"C",4.0] in
  let e3:t = create ["A",1.0;"B",3.0;"C",4.0] in
    Assert.Equal<t>(e3,extend e2 e1)

[<Fact(DisplayName="Environment - of_list, double entry")>]
let of_list_double_entry () = 
  let e1:t = create ["A",1.0;"A",2.0] in
  let e2:t = create ["A",2.0] in
    Assert.Equal<t>(e1,e2)

