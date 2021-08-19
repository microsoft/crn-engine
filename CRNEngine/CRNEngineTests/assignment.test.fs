module Microsoft.Research.CRNEngine.Tests.AssignmentTest
open Operators
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

let float (f:float) = Expression.Float f
let key (s:string) = Expression.Key s

[<Fact(DisplayName="Assignment - create_floats, single assignment")>]
let create_floats_single_assignment () = 
  let expected = Assignment.create(["N"], [[0.0]])
  Assert.Equal(expected,Assignment.create("N = [0.0]"))

[<Fact(DisplayName="Assignment - create_floats, double assignment")>]
let create_floats_double_assignment () = 
  let expected = Assignment.create(["N"; "M"], [[0.0; 1.0]])
  Assert.Equal(expected,Assignment.create("(N,M) = [(0.0, 1.0)]"))

[<Fact(DisplayName="Assignment - to_string_bindings, single assignment")>]
let to_string_bindings_single_assignment () = 
  let expected:string list = ["N = 0"; "N = 1"]
  Assert.Equal<string list>(expected, Assignment.create("N = [0.0; 1.0]").to_string_bindings)

[<Fact(DisplayName="Assignment - to_string, single assignment")>]
let to_string_single_assignment () = 
  let expected:string = "N = [0; 1]"
  Assert.Equal(expected, Assignment.create("N = [0.0; 1.0]").string)

[<Fact(DisplayName="Assignment - to_string, double assignment")>]
let to_string_double_assignment () = 
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let expected:string = "(N,M) = [(0,0.1); (1,1.1)]"
  let actual = Assignment.create("(N,M) = [(0.0,0.1); (1.0,1.1)]").string
  Assert.Equal(expected,actual)

[<Fact(DisplayName="Assignment - eval, double assignment")>]
let eval_double_assignment () = 
  let a = Assignment.create(["A";"B"], [[0.0;0.0]; [1.0;1.0]])
  let expected = List.map Map.ofList [["A",0.0;"B",0.0];["A",1.0;"B",1.0]]
  Assert.Equal<Environment.t list>(expected, a.eval Environment.empty)

[<Fact(DisplayName="Assignment - eval, cannot refer to variables in the same assignment")>]
let eval_cannot_refer_to_variables_in_the_same_assignment () = 
  let e = Map.ofList ["A",10.;"B",20.;"C",30.]
  let a = Assignment.create(["A";"B";"C"], [[float 1.; 2. * key "A"; float 4.]; [float 1.; 3. * key "A"; float 6.]])
  let expected = List.map Map.ofList [["A",1.;"B",20.;"C",4.];["A",1.;"B",30.;"C",6.]]
  Assert.Equal<Environment.t list>(expected, a.eval e)

[<Fact(DisplayName="Assignment - add_multiples")>]
let add_multiples() = 
  let a = Assignment.create(["a";"b"], [[0.;0.]; [0.;1.]; [1.;0.]; [1.;1.]])
  let expected_values:Value list list = [
      [float 0.;float 0.;key "x_1";key "y_1"]; 
      [float 0.;float 1.;key "x_2";key "y_2"]; 
      [float 1.;float 0.;key "x_3";key "y_3"]; 
      [float 1.;float 1.;key "x_4";key "y_4"];
    ]
  let expected_assignment= Assignment.create(["a";"b";"x";"y"], expected_values)
  let expected_fresh_names_list:string list list = [["x_1";"y_1"];["x_2";"y_2"];["x_3";"y_3"];["x_4";"y_4"]]
  let (result_fresh_names_list:string list list),result_assignment = a.add_multiples (ref 0) ["x";"y"]
  Assert.Equal(expected_assignment,result_assignment);
  Assert.Equal<string list list>(expected_fresh_names_list,result_fresh_names_list);
