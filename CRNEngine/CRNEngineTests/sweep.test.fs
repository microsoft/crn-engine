module Microsoft.Research.CRNEngine.Tests.SweepTest
open Operators
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Api

let float (f:float) = Expression.Float f
let key (s:string) = Expression.Key s

[<Fact(DisplayName="Assignment - merge, two double assignments")>]
let merge_two_double_assignments () =
  let s = Sweep.create("", [ ["A";"B"] => [[0.0;0.0]; [1.0;1.0]]; ["C";"D"] => [[2.0;2.0]; [3.0;3.0]]; ])
  let expected =  Assignment.create(["A";"B";"C";"D"], [[0.0;0.0;2.0;2.0]; [1.0;1.0;2.0;2.0]; [0.0;0.0;3.0;3.0]; [1.0;1.0;3.0;3.0]])
  let e = Environment.empty
  Assert.Equal(expected, s.merge);
  Assert.Equal<Environment.t list>(expected.eval e , s.eval e)

[<Fact(DisplayName="Assignment - merge, double and single assignment")>]
let merge_double_and_single_assignment () = 
  let s = Sweep.create("", [ ["X";"Y"] => [[0.0;0.0]; [1.0;1.0]] ; ["N"] => [[0.0];[1.0]] ] )
  let expected = Assignment.create(["X";"Y";"N"], [[0.0;0.0;0.0]; [1.0;1.0;0.0]; [0.0;0.0;1.0]; [1.0;1.0;1.0]])
  let e = Environment.empty
  Assert.Equal(expected, s.merge );
  Assert.Equal<Environment.t list>(expected.eval e, s.eval e )

[<Fact(DisplayName="Assignment - merge, with local variables")>]
let merge_with_local_variables () =
  let a1 = Assignment.create(["X";"Y"], [[1.0;2.0]; [2.0;4.0]])
  let a2 = Assignment.create(["N"], [[(key "X") * 2.0];[(key "Y") * 2.0]])
  let s = Sweep.create("", [a1;a2])
  let evaluated = Assignment.create(["X";"Y";"N"], [[1.0;2.0;2.0]; [2.0;4.0;4.0]; [1.0;2.0;4.0]; [2.0;4.0;8.0]])
  let merged = Assignment.create(["X";"Y";"N"], [[float 1.0;float 2.0;float 1.0 * 2.0]; [float 2.0;float 4.0;float 2.0 * 2.0]; [float 1.0;float 2.0;float 2.0 * 2.0]; [float 2.0; float 4.0;float 4.0 * 2.0]])
  let e = Environment.empty
  Assert.Equal(merged, s.merge);
  Assert.Equal<Environment.t list>(evaluated.eval e, s.eval e)

[<Fact(DisplayName="Assignment - merge, with global and local variables")>]
let merge_with_global_and_local_variables () = 
  let e = Map.ofList ["A",1.0;"B",2.0;"C",4.0]
  let s = Sweep.create("", [ ["X";"Y"] => [[key "A";key "B"]; [key "B";key "C"]] ; ["N"] => [[(key "X") * 2.0];[(key "Y") * 2.0]] ])
  let evaluated = Assignment.create(["X";"Y";"N"], [[1.0;2.0;2.0]; [2.0;4.0;4.0]; [1.0;2.0;4.0]; [2.0;4.0;8.0]])
  let merged = Assignment.create(["X";"Y";"N"], [[key "A";key "B";(key "A") * 2.0]; [key "B";key "C";(key "B") * 2.0]; [key "A";key "B";(key "B") * 2.0]; [key "B";key "C";(key "C") * 2.0]])
  Assert.Equal(merged, s.merge);
  Assert.Equal<Environment.t list>(evaluated.eval e, s.eval e )