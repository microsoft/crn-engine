module Microsoft.Research.CRNEngine.Tests.ParameterTest
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

[<Fact(DisplayName="Parameters - merge")>]
let parameters_merge () = 
    let p1 = Parameter.create("p1", 1.0, None)
    let p2 = Parameter.create("p2", 1.0, None)
    let ps1 = [p1]
    
    let expected = [p1;p2]
    let got = Parameters.merge ps1 [p2]
    Assert.Equal<Parameter list> (expected, got)

[<Fact(DisplayName="Parameters - used parameters in reverse rates")>]
let parameters_reverse_unused () = 
  let fr = Rate.MassAction (Expression.Key "p1")
  let br = Rate.MassAction (Expression.Key "p2")
  let r  = Reaction.create ([Species.create"A"], (Some br, fr), [Species.create "B"])
  let crn = Crn.create "" Crn_settings.defaults [r] [] Stringmap.empty false
  let used_parameters = crn.all_used_parameters () 
  Assert.Equal(2, used_parameters.Length)