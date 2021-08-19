module Microsoft.Research.CRNEngine.Tests.ResultTest
open Microsoft.Research.CRNEngine
open Xunit
open FsUnit.Xunit

[<Fact(DisplayName="Results - group_sweeps, two sweeps")>]
let group_sweeps_two_sweeps () = 
  let r1:Result<float> = Result<_>.create (Instance.create("c", "s1", Environment.create ["A",1.0;"B",2.0])) Table<float>.empty in
  let r2:Result<float> = Result<_>.create (Instance.create("c", "s2", Environment.create ["A",2.0;"B",4.0])) Table<float>.empty in
  let r3:Result<float> = Result<_>.create (Instance.create("c", "s1", Environment.create ["A",3.0;"B",6.0])) Table<float>.empty in
  let rs:Result<float> list = [r1;r2;r3] in
  let expected:Result<float> list list = [[r1;r3];[r2]] in
    Assert.Equal<Result<float> list list>(expected, Result<_>.group_sweeps rs)
