module Microsoft.Research.CRNEngine.Tests.JitTest

open Xunit
open Microsoft.Research.CRNEngine

module Jit = Microsoft.Research.CRNEngine.Jit

let am_example_parsed () = Microsoft.Research.CRNEngine.Crn.from_string "
  directive simulation {final = 1e20}
  directive simulator stochastic
  | 30 X 
  | 20 Y 

  | X + Y ->{1e-6} X + B
  | Y + X ->{2e-6} Y + B 
  | X + B ->{3e-6} 2X 
  | Y + B ->{4e-6} 2Y
  " 

let emptyCalculus : Calculus<Species> = { react = fun _ _ -> []}
let printSpecies (x:Species) = x.name
let equalsSpecies (x:Species) (y:Species) = x.name = y.name

[<Trait("Category", "JIT")>]
[<Fact(DisplayName="JIT - simulate AM")>]
let jit_simulate_AM () =
  let crn = am_example_parsed()
  let jit = Jit.fromCRN crn
  let results = Jit.simulate jit emptyCalculus |> snd
  Assert.NotEmpty(results.times)