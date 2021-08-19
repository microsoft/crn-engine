module Microsoft.Research.CRNEngine.Tests.SweepsTest
open Operators
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Api

let float (f:float) = Expression.Float f
let key (s:string) = Expression.Key s

[<Fact(DisplayName="Sweep - list_add_multiples")>]
let list_add_multiples () = 
  let s1 = Sweep.create("s1", [["a";"b"] => [[1.;2.]; [3.;4.]]])
  let s2 = Sweep.create("s2", [["c";"d"] => [[5.;6.]; [7.;8.]]])
  let s1' = Sweep.create("s1", [["a";"b";"x";"y"] => [[float 1.;float 2.;key "x_1";key "y_1"]; [float 3.;float 4.;key "x_2";key "y_2"]]] )
  let s2' = Sweep.create("s2", [["c";"d";"x";"y"] => [[float 5.;float 6.;key "x_3";key "y_3"]; [float 7.;float 8.;key "x_4";key "y_4"]]] )
  let names:string list list list = [ [ ["x_1";"y_1"]; ["x_2";"y_2"] ]; [ ["x_3";"y_3"]; ["x_4";"y_4"] ] ]
  let result = Sweeps.add_multiples (ref 0) ["x";"y"] [s1;s2]
  let expected = [s1';s2']
  Assert.Equal<Sweep list>(expected, snd result);
  Assert.Equal<string list list list>(names,fst result);



[<Fact(DisplayName="Sweep - No species allowed")>]
let sweep_no_species () = 
  let compile () = 
    """directive simulation { final=0.15; points=300 }
  directive parameters [r = 1.0]
  directive simulator deterministic
  directive deterministic { reltolerance=1e-5 }
  directive sweeps [test = [X = [1; 5; 10; 30]]]
  
  | X + Y ->{r} X + B
  | Y + X ->{r} Y + B
  | X + B ->{r} X + X
  | Y + B ->{r} Y + Y
  
  | 30 X
  | 20 Y""" |> Crn.from_string 
  Assert.Throws<System.Exception>(fun () -> compile () |> ignore)
  