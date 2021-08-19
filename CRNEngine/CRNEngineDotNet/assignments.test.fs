module Microsoft.Research.CRNEngine.AssignmentsTest
open Top
open Xunit
open FsUnit.Xunit

let float (f:float) = Expression.Float f
let key (s:string) = Expression.Key s

[<Fact>] 
let ``merge, two double assignments`` () =
  let a = Assignments([ ["A";"B"] => [[0.0;0.0]; [1.0;1.0]]; ["C";"D"] => [[2.0;2.0]; [3.0;3.0]]; ])
  let expected =  Assignment(["A";"B";"C";"D"], [[0.0;0.0;2.0;2.0]; [1.0;1.0;2.0;2.0]; [0.0;0.0;3.0;3.0]; [1.0;1.0;3.0;3.0]])
  let e = Environment.empty
  Assert.Equal(expected, a.merge);
  Assert.Equal<Environment.t list>(expected.eval e , a.merge_eval e)

[<Fact>] 
let ``merge, double and single assignment`` () = 
  let a = Assignments([ ["X";"Y"] => [[0.0;0.0]; [1.0;1.0]] ; ["N"] => [[0.0];[1.0]] ] )
  let expected = Assignment(["X";"Y";"N"], [[0.0;0.0;0.0]; [1.0;1.0;0.0]; [0.0;0.0;1.0]; [1.0;1.0;1.0]])
  let e = Environment.empty
  Assert.Equal(expected, a.merge );
  Assert.Equal<Environment.t list>(expected.eval e, a.merge_eval e )

[<Fact>] 
let ``merge, with local variables`` () =
  let a1 = Assignment(["X";"Y"], [[1.0;2.0]; [2.0;4.0]])
  let a2 = Assignment(["N"], [[(key "X") * 2.0];[(key "Y") * 2.0]])
  let a = Assignments([a1;a2])
  let evaluated = Assignment(["X";"Y";"N"], [[1.0;2.0;2.0]; [2.0;4.0;4.0]; [1.0;2.0;4.0]; [2.0;4.0;8.0]])
  let merged = Assignment(["X";"Y";"N"], [[float 1.0;float 2.0;float 1.0 * 2.0]; [float 2.0;float 4.0;float 2.0 * 2.0]; [float 1.0;float 2.0;float 2.0 * 2.0]; [float 2.0; float 4.0;float 4.0 * 2.0]])
  let e = Environment.empty
  Assert.Equal(merged, a.merge);
  Assert.Equal<Environment.t list>(evaluated.eval e, a.merge_eval e)

[<Fact>] 
let ``merge, with global and local variables``() = 
  let e = Map.ofList ["A",1.0;"B",2.0;"C",4.0]
  let a = Assignments([ ["X";"Y"] => [[key "A";key "B"]; [key "B";key "C"]] ; ["N"] => [[(key "X") * 2.0];[(key "Y") * 2.0]] ])
  let evaluated = Assignment(["X";"Y";"N"], [[1.0;2.0;2.0]; [2.0;4.0;4.0]; [1.0;2.0;4.0]; [2.0;4.0;8.0]])
  let merged = Assignment(["X";"Y";"N"], [[key "A";key "B";(key "A") * 2.0]; [key "B";key "C";(key "B") * 2.0]; [key "A";key "B";(key "B") * 2.0]; [key "B";key "C";(key "C") * 2.0]])
  Assert.Equal(merged, a.merge);
  Assert.Equal<Environment.t list>(evaluated.eval e, a.merge_eval e )