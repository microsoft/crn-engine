module Microsoft.Research.CRNEngine.Tests.Simulation_settingsTest
open Xunit
open FsUnit.Xunit
 open Microsoft.Research.CRNEngine
 
[<Fact(DisplayName="Simulation settings - record")>]
let record () =
  let expected = { Simulation_settings.defaults with final = 20.0 } in
  let got = Simulation_settings.from_string Functional2.parse_plot "{ final = 20.0 }"  in
  Assert.Equal(expected, got)

[<Fact(DisplayName="Simulation settings - record - styled")>]
let record_styled () =
  let expected = { Simulation_settings.defaults with final = 20.0; points = 10; plots = [Expression.Key(Key.Species (Species.from_string "X"))] } in
  let got = Simulation_settings.from_string Functional2.parse_plot "  { final = 20.0 ; points = 10; plots = [X] }"  in
  Assert.Equal(expected, got)

[<Fact(DisplayName="Simulation settings - record - plot expressions")>]
let plot_expression () =
  let expected = { 
    Simulation_settings.defaults with 
      plots = 
      [ 
        Expression.Plus [ 
          Expression.Times [ 
            Expression.Key (Key.Parameter "a")
            Expression.Key (Key.Species (Species.from_string "y"))
          ]
          Expression.Key (Key.Parameter "b") 
        ]
        Expression.Key(Key.Species (Species.from_string "X"))
     ] 
  }
  let test = "{ plots = [a*[y]+b; X]}"  
  let got = Simulation_settings.from_string Functional2.parse_plot test
  Assert.Equal(expected, got)