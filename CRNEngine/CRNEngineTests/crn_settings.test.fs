module Microsoft.Research.CRNEngine.Tests.Crn_settingsTest
open Operators
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Api
#nowarn "58"

let from_string_species = Crn_settings.from_string Functional2.parse Functional2.parse_plot

let defaults = Crn_settings.defaults
[<Fact(DisplayName="CRN settings - sweep")>]
let crn_sweep() =
  let expected_sweep = Sweep.create("s", [["N"] => [[Expression.Float 0.0]] ])
  let expected = { defaults with sweeps = [expected_sweep]; parameters = [Parameter.create(("N",0.0),None)] }
  let got = "directive sweeps [s = [N = [0.0]]]
  directive parameters [N=0.0]" |> from_string_species 
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - final")>]
let crn_final () =
  let expected = { defaults with simulation = { Simulation_settings.defaults with final = 20.0 } }
  let got = from_string_species "directive simulation { final = 20.0 }" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - points")>]
let crn_points () =
  let expected = { defaults with simulation = { Simulation_settings.defaults with points = 20 } }
  let got = from_string_species "directive simulation { points = 20 }" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - plots")>]
let crn_plots () =
  let x = Key.Species (Species.create "X") in
  let expected = { defaults with simulation = { Simulation_settings.defaults with plots = [Expression.Key x] } }
  let got = from_string_species "directive simulation { plots = [X] }" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - simulation")>]
let crn_simulation () =
  let expected = { defaults with simulation = { Simulation_settings.defaults with final = 20.0; points = 20 } }
  let got = from_string_species "directive simulation { final = 20.0; points = 20 }" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - simulation stochastic")>]
let crn_simulation_stochastic () =
  let expected =
    { defaults with
        simulation = { Simulation_settings.defaults with final = 20.0; points = 20}
        simulator = SSA
    }
  let got = from_string_species "directive simulation { final = 20.0; points = 20 } directive simulator stochastic" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - simulation LNA")>]
let crn_simulation_LNA () =
  let expected =
    { defaults with
        simulation = { Simulation_settings.defaults with final = 20.0; points = 20}
        simulator = LNA
    }
  let got = from_string_species "directive simulation { final = 20.0; points = 20 } directive simulator lna" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - Directive on multiple lines")>]
let directive_on_multiple_lines () =
  let expected = { defaults with simulation = { Simulation_settings.defaults with final = 20.0; points = 20 } }
  let got = from_string_species "directive simulation { final = 20.0;\n points = 20 }" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - Units")>]
let directive_units () =
  let cexpected = { defaults with units = { defaults.units with concentration = Molar -21 } }
  let cgot = from_string_species "directive units { concentration = zM }"
  Assert.Equal(cexpected, cgot)

  let texpected = { defaults with units = { defaults.units with time = Seconds 60.0 } }
  let tgot = from_string_species "directive units { time = min }"
  Assert.Equal(texpected, tgot)

  let sexpected = { defaults with units = { defaults.units with space = Metres -12 } }
  let sgot = from_string_species "directive units { space = pm }"
  Assert.Equal(sexpected, sgot)

  let expected = { defaults with units = { defaults.units with space          = Metres -12
                                                               time           = Seconds 60.0
                                                               concentration  = Molar -21    } }
  let got = from_string_species "directive units { space = pm; time = min; concentration = zM }"
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - parameters")>]
let crn_parameters () =
  let expected =
    { defaults with 
        parameters =
          [ 
            Parameter.create("N", 20.0, None)
            Parameter.create("rexp", 0.1, Some {interval=Interval.Log;  distribution=Distribution.Uniform{min=1e-3;max=1e0}; variation=Variation.Random})
          ]
    }
  let got = from_string_species "directive parameters [ N = 20; rexp = 0.1, {interval=Log;  distribution=Uniform(1e-3,1e0); variation=Random} ]" in
  Assert.Equal(expected, got)


[<Fact(DisplayName="CRN settings - parameters - to_string")>]
let crn_parameters_to_string () =
  let expected = """directive parameters [
N = 20;
rexp = 0.1, {interval=Log; distribution=Uniform(0.001,1); variation=Random};
]"""                .Replace("\r\n", "\n")

  let got = (from_string_species expected).to_string Functional2.to_string_plot
  ()
  //Assert.Equal<string>(expected, got)

[<Fact(DisplayName="CRN settings - spatial simulation")>]
let crn_spatial_simulation () =
  let expected =
    { defaults with
        spatial   = { Spatial_settings.defaults with dimensions = 2
                                                     ; random = 2.0
                                                     ; nx =  1
                                                     ; dt = 3.0
                                                     ; boundary = Boundary.ZeroFlux } }
  let got = from_string_species "directive spatial { random = 2.0
                                                   ; dimensions = 2 
                                                   ; nx =  1
                                                   ; dt = 3.0
                                                   ; boundary = ZeroFlux }"
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - spatial simulation - partial")>]
let crn_spatial_simulation_partial () =
  let expected =
    { defaults with
        spatial   = { Spatial_settings.defaults with dimensions = 2
                                                     ; random = 2.0 } }
  let got = from_string_species "directive spatial { random = 2.0
                                                   ; dimensions = 2 }"
  Assert.Equal(expected, got)


[<Fact(DisplayName="CRN settings - spatial - diffusibles")>]
let crn_spatial_simulation_diffusibles () =
  let diffusibles = [
    Functional2.create (Species.create "X"), Expression.Float 1.0;
    Functional2.create (Species.create "Y"), Expression.Key "DY";
  ]
  let expected = { defaults with spatial   = { Spatial_settings.defaults with dimensions = 2; diffusibles = diffusibles } }
  let got = from_string_species "directive spatial { diffusibles = [X=1.0; Y=DY]; dimensions = 2 }"
  Assert.Equal(expected, got)

[<Fact(DisplayName="CRN settings - to_simulation_runs")>]
let ``CRN settings - to_simulation_runs`` () =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let s:Crn_settings<Functional> = from_string_species "
    directive parameters [
      N = 0.0; 
      Z1 = 0.0; 
      Z2 = 0.0; 
      Z3 = 0.0;
    ]
    directive sweeps [
      Rep_rbxu1 = [ N = [0.2;0.4]; Z1 = [1] ];
      JoinBX_AddB = [ N = [0.0;0.2]; Z2 = [1] ];
      JoinBX_AddX = [ N = [0.0;0.2;0.4;0.6]; Z3 = [1] ];
    ]
    directive simulation {sweeps = [Rep_rbxu1]; plots = [Signal_JoinBX]; final = 7.1 }
    directive simulations [
      Rep_rbxu1 = {sweeps = [Rep_rbxu1]; plots = [Signal_JoinBX]; final = 1.1 };
      JoinBX_AddB = {sweeps = [JoinBX_AddB]; plots = [Signal_JoinBX]; final = 7.1 };
    ]  
  "
  let simulation1 = Parser.from_string (Simulation_settings.parse_named Functional2.parse_plot) "
    Rep_rbxu1 = {sweeps = [Rep_rbxu1]; plots = [Signal_JoinBX]; final = 1.1 }
  "
  let simulation2 = Parser.from_string (Simulation_settings.parse_named Functional2.parse_plot) "
    JoinBX_AddB = {sweeps = [JoinBX_AddB]; plots = [Signal_JoinBX]; final = 7.1 }
  "
  let Signal_JoinBX = Species.create "Signal_JoinBX"
  let defaults = Crn_settings.defaults
  let s_api:Crn_settings<Functional> = { defaults with
    parameters = Parameters.create [
      "N" => 0.0,None;
      "Z1" => 0.0,None;
      "Z2" => 0.0,None;
      "Z3" => 0.0,None;
    ];
    sweeps = Sweeps.create [
      "Rep_rbxu1" => [ "N" => [0.2;0.4]; "Z1" => [1.0] ]
      "JoinBX_AddB" => [ "N" => [0.0;0.2]; "Z2" => [1.0] ];
      "JoinBX_AddX" => [ "N" => [0.0;0.2;0.4;0.6]; "Z3" => [1.0] ];
    ]
    simulation = { defaults.simulation with 
      sweeps = ["Rep_rbxu1"]; plots = plots [Signal_JoinBX]; final = 7.1 
    }
    simulations = [
      { defaults.simulation with name = "Rep_rbxu1"; sweeps = ["Rep_rbxu1"]; plots = plots [Signal_JoinBX]; final = 1.1 };
      { defaults.simulation with name = "JoinBX_AddB"; sweeps = ["JoinBX_AddB"]; plots = plots [Signal_JoinBX]; final = 7.1 };
    ]
  }
  let (instances:Instance<Functional> list,tables:Table<float> list) = s.to_simulation_runs "crn" 
  let expected_instances:Instance<Functional> list = [
    Instance.create("crn", "Rep_rbxu1", Environment.create [("N",0.2);("Z1",1.0)], simulation1, "[N = 0.2]");
    Instance.create("crn", "Rep_rbxu1", Environment.create [("N",0.4);("Z1",1.0)], simulation1, "[N = 0.4]");
    Instance.create("crn", "JoinBX_AddB", Environment.create [("N",0.0);("Z2",1.0)], simulation2, "[N = 0]");
    Instance.create("crn", "JoinBX_AddB", Environment.create [("N",0.2);("Z2",1.0)], simulation2, "[N = 0.2]");
  ]
  Assert.Equal<Table<float> list>(tables,[]);
  Assert.Equal<Instance<Functional>>(instances,expected_instances)
  Assert.Equal<Crn_settings<Functional>>(s,s_api)

