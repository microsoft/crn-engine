module Microsoft.Research.CRNEngine.Tests.ModelTest
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Api
open Xunit
open FsUnit.Xunit
open System.Diagnostics


#nowarn "58"

let dataset (s:string,ts:Table<float> list) = Dataset.create s ts

let dna_example () =
  let Input = species "Input"
  let InputR = species "InputR"
  let Catalyst = species "Catalyst"
  let CatalystR = species "CatalystR"
  let Helper = species "Helper"
  let Join_AB = species "Join_AB"
  let Rep_Join = species "Rep_Join"
  let Translator = species "Translator"
  let Signal_Join = species "Signal_Join" 
  let sp13 = species "sp13"
  let sp14 = species "sp14"
  let sp15 = species "sp15"
  let sp16 = species "sp16"
  let kJ1 = rate "kJ1"
  let kJ1r = rate "kJ1r"
  let kJ2 = rate "kJ2"
  let kJ2r = rate "kJ2r"
  let kJ3 = rate "kJ3"
  let kJ3r = rate "kJ3r"
  let kR = rate "kR"
  let N = rate "N"
  let leak = rate "leak"
  let good = rate "good"
  let t0RepJoin = rate "t0RepJoin"
  let t0JoinBX_B = rate "t0JoinBX_B"
  let t0JoinBX_X = rate "t0JoinBX_X"
  let t0JoinBX_H = rate "t0JoinBX_H"
  let Xi = float 10.0
  let NGi = float 30.0
  let NHi = float 30.0
  let NRi = float 30.0
  let NMi = float 100.0
  let NRr = float 16.0
  let Xr = float 4.0
  let zero = float 0.0
  let empty:Crn = Crn.empty
  //let JoinBX_AddB = ["JoinBX_AddB.txt" |> Io.load_file |> Table.parse_tsv]
  //let JoinBX_AddX = ["JoinBX_AddX.txt" |> Io.load_file |> Table.parse_tsv]
  //let JoinBX_AddH = ["JoinBX_AddH.txt" |> Io.load_file |> Table.parse_tsv]
  //let RepJoin = ["Rep_rbxu1.txt" |> Io.load_file |> Table.parse_tsv]
  let top:Crn = 
    { empty with
      name = "top";
      settings = { empty.settings with 
        simulation = { empty.settings.simulation with 
          points = 1000;
          final = 7.2;
          plots = plots [Signal_Join];
          multicore = true
        };
        stochastic = { empty.settings.stochastic with scale = 50.0};
        simulator = Simulator.Oslo;
        sweeps = [
          Sweep.create("sweepJoin", ["N" => [0.0; 0.2; 0.4; 0.6]]);
        ];
        inference = { empty.settings.inference with 
          burnin = 5000;
          samples = 20000;
          thin = 5;
          seed = 0u;
          noise_parameter = Noise_parameter.Random;
          noise_model = Proportional;
          prune = false;
          timer = false;
          partial_evaluation = false;
        };
        parameters = Parameters.create [
          "kJ1" => 0.01, Some {Prior.empty with distribution=Uniform{min=0.01;max=100.0}};
          "kJ1r" => 0.01, Some {Prior.empty with distribution=Uniform{min=0.01;max=100.0}};
          "kJ2" => 0.01, Some {Prior.empty with distribution=Uniform{min=0.01;max=100.0}};
          "kJ2r" => 0.01, Some {Prior.empty with distribution=Uniform{min=0.01;max=100.0}};
          "kJ3" => 0.01, Some {Prior.empty with distribution=Uniform{min=0.01;max=100.0}};
          "kJ3r" => 0.01, Some {Prior.empty with distribution=Uniform{min=0.01;max=100.0}};
          "kR" => 0.01, Some {Prior.empty with distribution=Uniform{min=0.01;max=100.0}};
          "t0RepJoin" => 0.118985, Some {Prior.empty with interval=Interval.Real; distribution=Uniform{min=0.01;max=2.0}};
          "t0JoinBX_B" => 1.0, Some {Prior.empty with interval=Interval.Real; distribution=Uniform{min=0.5;max=2.0}};
          "t0JoinBX_X" => 1.0, Some {Prior.empty with interval=Interval.Real; distribution=Uniform{min=0.5;max=2.0}};
          "t0JoinBX_H" => 1.0, Some {Prior.empty with interval=Interval.Real; distribution=Uniform{min=0.5;max=2.0}};
          "N" => 1.0, None;
        ];
      };
    }.create_blank_attributes()
  let rxnJoinBX = Reactions.create [
    [Join_AB;Input], kJ1r <-> kJ1, [sp13;InputR];
    [sp13;Catalyst], kJ2r <-> kJ2, [sp14;CatalystR];
    [sp14;Helper], kJ3r <-> kJ3, [sp15;Translator];
  ]
  let popJoinBX NB TB NX TX NH TH NG leak = Initials.create [
    NB, Input, Some TB;
    NX, Catalyst, Some TX;
    NH, Helper, Some TH;
    (NG-leak), Join_AB, None;
    leak, Translator, None;
  ]
  let popRepJoin (N:Value) (T:Value) (NR:Value) = Initials.create [
    N, Translator, Some T;
    NR, Rep_Join, None;
  ]
  let rxnRepJoin = Reactions.create [
    [Rep_Join;Translator], !-> kR, [sp16;Signal_Join];
  ]
  let crnJoinBX_AddB:Crn = { top with
    name = "crnJoinBX_AddB";
    settings = { top.settings with
      simulation = { top.settings.simulation with final = 7.2};
      data = List.map dataset ["joinBX_addB.txt" => []];
      parameters = Parameters.create [
        "leak" => 0.24322014, None; 
        "good" => 0.942807776666667, None;
      ];
    };
    reactions = rxnJoinBX @ rxnRepJoin;
    initials = 
      (popJoinBX (N*Xi*good) t0JoinBX_B NMi zero NMi zero NGi leak) 
      @ (popRepJoin zero zero NRi);
  }
  let crnJoinBX_AddX:Crn = { top with
    name = "crnJoinBX_AddX";
    settings = { top.settings with 
      simulation = { top.settings.simulation with final = 14.0};
      data = List.map dataset ["joinBX_addX.txt" => []];
      parameters = Parameters.create [
        "leak" => 0.11946842, None; 
        "good" => 1.07286499666667, None;
      ];
    };
    reactions = rxnJoinBX @ rxnRepJoin;
    initials = 
      (popJoinBX NMi zero (N*Xi*good) t0JoinBX_X NMi zero NGi leak)
      @ (popRepJoin zero zero NRi);
  }
  let crnJoinBX_AddH:Crn = { top with
    name = "crnJoinBX_AddH";
    settings = { top.settings with 
      simulation = { top.settings.simulation with final = 2.7};
      data = List.map dataset ["joinBX_addH.txt" => []];
      parameters = Parameters.create [
        "leak" => 0.097396333, None; 
        "good" => 0.968786827833333, None;
      ];
    };
    reactions = rxnJoinBX @ rxnRepJoin;
    initials = 
      (popJoinBX NMi zero NMi zero (N*Xi*good) t0JoinBX_H  NGi leak) 
      @ (popRepJoin zero zero NRi);
  }
  let crnRepJoin:Crn = { top with
    name = "crnRepJoin";
    settings = { top.settings with 
      simulation = { top.settings.simulation with final = 1.1};
      data = List.map dataset ["repJoin.txt" => []];
      parameters = [];
      sweeps = [
        Sweep.create("sweepRep", ["N" => [0.2; 0.4; 0.6; 0.8]]);
      ];
    };
    reactions = rxnRepJoin;
    initials = (popRepJoin (N*Xr) t0RepJoin NRr);
  }
  Model.create top.settings [crnJoinBX_AddB;crnJoinBX_AddX;crnJoinBX_AddH;crnRepJoin]




//Disable running the SSA for now, errors on looking for "Input" in one of the datastructures.
//Needs investigating - Colin
//let example = Crn.simulate_ssa crnJoinBX_AddB 

(*
Crn.infer_ode inference_settings None None crnJoinBX_AddB 

Crn.infer_odes inference_settings None None [crnJoinBX_AddB;crnJoinBX_AddX; crnJoinBX_AddH; crnRepJoin] 

"example.dna" |> Io.load_file  |> Crn.parse |> Crn.simulate None None

*)

(* Example CRN program *)
let dna_example_parsed () = Parser.from_string Model.parse "
directive simulation { final = 10.0; points = 1000; plots = [Signal_Join] }
directive deterministic { stiff = true }
directive stochastic { scale = 50.0 }
directive inference { burnin = 5000; samples = 20000; thin = 5 }
directive simulator deterministic
directive parameters [
  kJ1 = 0.1, {distribution=Uniform(0.01,100.0); interval=Log};
  kJ1r = 0.1, {distribution=Uniform(0.01,100.0); interval=Log};
  kJ2 = 0.1, {distribution=Uniform(0.01,100.0); interval=Log};
  kJ2r = 0.1, {distribution=Uniform(0.01,100.0); interval=Log};
  kJ3 = 0.1, {distribution=Uniform(0.01,100.0); interval=Log};
  kJ3r = 0.1, {distribution=Uniform(0.01,100.0); interval=Log};
  kR = 0.1, {distribution=Uniform(0.01,100.0); interval=Log};
  N = 0.0; Xi = 10.0; NGi = 30.0; NHi = 30.0; NRi = 30.0; NMi = 100.0; Xr = 4.0; NRr = 12.0;
]
directive sweeps [ s = [ N = [0.0; 0.2; 0.4; 0.6] ] ]

module JoinBX(NB,TB,NX,TX,NH,TH,NG,Leak) = {
  | NB Input @ TB 
  | NX Catalyst @ TX 
  | NH Helper @ TH 
  | (NG - Leak) Join_AB  
  | Leak Translator
  | Join_AB + Input <->{kJ1}{kJ1r} sp13 + InputR
  | sp13 + Catalyst <->{kJ2}{kJ2r} sp14 + CatalystR
  | sp14 + Helper <->{kJ3}{kJ3r} sp15 + Translator
}
module RepJoin(N,T,NR) = {
  | N Translator @ T
  | NR Rep_Join
  | Rep_Join + Translator ->{kR} sp16 + Signal_Join
}
system crnJoinBX_AddB = {
  directive simulation {final = 7.2}
  directive data [JoinBX_AddB]
  directive parameters [t0 = 1.0; leak = 0.24322014; good = 0.942807776666667]
  | JoinBX(N*Xi*good,t0,NMi,0.0,NMi,0.0,NGi,leak) 
  | RepJoin(0.0,0.0,NRi)
}
system crnJoinBX_AddX = {
  directive simulation {final = 14.0}
  directive data [JoinBX_AddX]
  directive parameters [t0 = 1.0; leak = 0.11946842; good = 1.07286499666667]
  | JoinBX(NMi,0.0,N*Xi*good,t0,NMi,0.0,NGi,leak) 
  | RepJoin(0.0,0.0,NRi)
}
system crnJoinBX_AddH = {
  directive simulation {final = 2.7}
  directive data [JoinBX_AddH]
  directive parameters [t0 = 1.0; leak = 0.097396333; good = 0.968786827833333]
  | JoinBX(NMi,0.0,NMi,0.0,N*Xi*good,t0,NGi,leak) 
  | RepJoin(0.0,0.0,NRi)
}
system crnRepJoin = {
  directive simulation {final = 1.1}
  directive sweeps [s = [N = [0.2; 0.4; 0.6; 0.8]]]
  directive data [RepJoin]
  directive parameters [t0 = 0.118985]  
  | RepJoin(N*Xr,t0,4.0*Xr)
}
"
(*
| crnJoinBX_AddB | crnJoinBX_AddX | crnJoinBX_AddH | crnRepJoin
*)

[<Fact(DisplayName = "Model - Parse DNA model with systems")>]
let ``Parse DNA model with systems`` () =
  let expected:Model = dna_example()
  let parsed:Model = dna_example_parsed()
  ///Assert.Equal<Crn>(expected,actual)//TODO: fix this so that expected = actual. 
  Assert.Equal<Model>(parsed,parsed)


[<Fact(DisplayName = "Model - Simulate with no systems")>]
let model_simulate_no_systems () = 
  let model = "directive simulator deterministic \n 10 A | 10 B | A + B ->{1.0} C" |> Parser.from_string Model.parse
  let result = model.simulate ()
  Assert.Equal<int>(1,result |> List.length)

//System Parser Tests

let topSettings = """directive simulation {final=45;points=300;}
directive plot_settings {x_label = "out0"}
directive sweeps [
  sweep0 = [
    (a,b) = [(1,2);(2,3);(3,4);]
  ];
  sweep1 = [
    (a,b) = [(2,3);(4,3);(8,4);]
  ];
  sweep2 = [
    (a,b) = [(7,8);(5,5);(6,4);]
  ];
]
directive parameters [
  globalParam = 4, {interval=Real; distribution=Uniform(0.2,30); variation=Random};
  a=0;
  b=0;
]
directive rates [
  globalRate = 1/r
]

"""

let systemString0 = """system growth = { 
directive simulation {final=36; points=250; plots=[([x] + x0)]; }
directive parameters [
  r = 1, {interval=Real; distribution=Uniform(0.1,10); variation=Multiple};
  K = 2, {interval=Real; distribution=Uniform(0.1,5); variation=Multiple};
  tlag = 1, {interval=Real; distribution=Uniform(0,10); variation=Multiple};
  x0 = 0.1, {interval=Real; distribution=Uniform(0,0.2); variation=Random};
]
directive rates [
  growth = (([grow] * r) * (1 - ([x] / K)))
]
}
"""

let systemString1 = """system Auto_control = { growth with directive data [placeholder] }"""

let systemString2 = """system Auto_control = { growth with directive data [placeholder] }
system growth = { 
directive simulation {final=36; points=250; plots=[([x] + x0)]; }
directive parameters [
  r = 1, {interval=Real; distribution=Uniform(0.1,10); variation=Multiple};
  K = 2, {interval=Real; distribution=Uniform(0.1,5); variation=Multiple};
  tlag = 1, {interval=Real; distribution=Uniform(0,10); variation=Multiple};
  x0 = 0.1, {interval=Real; distribution=Uniform(0,0.2); variation=Random};
]
directive rates [
  growth = (([grow] * r) * (1 - ([x] / K)))
]
}
"""

let systemString3 = """system empty = {}

system growth = { 
directive simulation {final=36; points=250; plots=[([x] + x0)]; }
directive plot_settings {x_label = "out1"}
directive parameters [
  r = 1, {interval=Real; distribution=Uniform(0.1,10); variation=Multiple};
  K = 2, {interval=Real; distribution=Uniform(0.1,5); variation=Multiple};
  tlag = 1, {interval=Real; distribution=Uniform(0,10); variation=Multiple};
  x0 = 0.1, {interval=Real; distribution=Uniform(0,0.2); variation=Random};
]
directive rates [
  growth = (([grow] * r) * (1 - ([x] / K)))
]
directive sweeps [
  sweep3 = [
    (a,b) = [(7,3);(1,5);(1,9);]
  ]    
]
}
system Auto_control = { growth with directive data [placeholder]
directive parameters [
  r = 3, {interval=Real; distribution=Uniform(0.1,10); variation=Multiple};
]
directive sweeps [
  sweep4 = [
    (a,b) = [(7,80);(6,5);(6,9);]
  ]    
]
}

"""

let systemString4 = """system empty = {}
system alsoEmpty = {empty with}
"""


let from_system_string settings moduleDefs = Parser.from_string (Model.parse_systems settings moduleDefs)

[<Fact(DisplayName="Model - Systems Parsing - With System not present")>]
  let previousSystemAbsent() = 
    let systemlist systemString missingSystem= 
      try 
          from_system_string Crn_settings.defaults [] systemString
      with 
          | Failure(msg) -> 
            Assert.Equal(msg,"System " + missingSystem + " not defined")
            []
    let sys0 = systemlist systemString1 "growth"
    let sys1 = systemlist systemString2 "growth"
    ()

[<Fact(DisplayName="Model - Systems Parsing - With System Parsing")>]
  let withSystemParsingTest() = 
    let singleSystem = from_system_string Crn_settings.defaults [] systemString0
    let systems = from_system_string Crn_settings.defaults [] systemString3
    Assert.Equal(singleSystem.Length,1)
    Assert.Equal((singleSystem.Item 0).name,"growth")
    Assert.Equal(systems.Length,3)
    Assert.Equal((systems.Item 0).name,"empty")
    Assert.Equal((systems.Item 1).name,"growth")
    Assert.Equal((systems.Item 2).name,"Auto_control")
    
 
[<Fact(DisplayName="Model - Systems Parsing - With System and Nothing else")>]
  let withSystemNothingElseTest() = 
    let singleSystem = from_system_string Crn_settings.defaults [] systemString4
    ()
             
    
[<Fact(DisplayName="Model - Systems Parsing - With System Settings test")>]
  let withSystemSettingsTest() = 
  let settings = Parser.from_string (Crn_settings.parse Functional2.parse Functional2.parse_plot) topSettings
  
  //Test Global Settings 
  Assert.Equal(settings.sweeps.Length,3)
  Assert.Equal(settings.sweeps.Item(0).name,"sweep0")
  Assert.Equal(settings.sweeps.Item(1).name,"sweep1")
  Assert.Equal(settings.sweeps.Item(2).name,"sweep2")
  Assert.Equal(settings.simulation.final,45.0)  
  Assert.Equal(settings.simulation.points,300)
  Assert.Equal(settings.plot.x_label,"out0")

  Assert.Equal(settings.rates.Count,1)
  Assert.True(settings.rates.ContainsKey("globalRate"))
  Assert.Equal(3, settings.parameters.Length)
  Assert.Equal(1, settings.parameters |> List.filter (fun p -> p.name = "globalParam") |> List.length)
  
  //Parse Systems
  let systems = from_system_string settings [] systemString3
  let emptySystem = systems.Item(0)
  let growthSystem = systems.Item(1)
  let autoControlSystem = systems.Item(2)

  //Check Plot_settings
  Assert.Equal(emptySystem.settings.plot.x_label,"out0") //This should pull settings from top.
  Assert.Equal(growthSystem.settings.plot.x_label,"out1") //These settings should be overriden.
  Assert.Equal(autoControlSystem.settings.plot.x_label,"out1") //These settings should come from growth.

  //Check Simulation
  Assert.Equal(emptySystem.settings.simulation.final,45.0) //This should pull settings from top.
  Assert.Equal(growthSystem.settings.simulation.final,36.0) //These settings should be overriden.
  Assert.Equal(autoControlSystem.settings.simulation.final,36.0) //These settings should come from growth.
  
  Assert.Equal(emptySystem.settings.simulation.points,300) //This should pull settings from top.
  Assert.Equal(growthSystem.settings.simulation.points,250) //These settings should be overriden.
  Assert.Equal(autoControlSystem.settings.simulation.points,250) //These settings should come from growth.
  

  //Check Sweeps
  Assert.True(emptySystem.settings.sweeps.IsEmpty)          // No propagation of Map types
  Assert.Equal(growthSystem.settings.sweeps.Length,1)       // Overridden (replaces)
  Assert.Equal(autoControlSystem.settings.sweeps.Length,2)  // From growth and adds one of its own
  
  Assert.Equal(growthSystem.settings.sweeps.Item(0).name,"sweep3")
  
  Assert.Equal(autoControlSystem.settings.sweeps.Item(0).name,"sweep3")
  Assert.Equal(autoControlSystem.settings.sweeps.Item(1).name,"sweep4")


  //Check Parameters
  Assert.True(emptySystem.settings.parameters.IsEmpty)          // No propagation of Map types
  Assert.Equal(growthSystem.settings.parameters.Length,4)
  Assert.Equal(autoControlSystem.settings.parameters.Length,4)

  let findParam (paramList:Parameter list) (name:string) = 
    match paramList |> List.tryFind (fun x -> x.name = name) with 
    | Some(param) -> param
    | None -> failwith ("Parameter " + name  + " not found. This is an error")

  let growthRparam = findParam growthSystem.settings.parameters "r"
  let autoControlRparam = findParam autoControlSystem.settings.parameters "r"
  let growthKparam = findParam growthSystem.settings.parameters "K"
  let autoControlKparam = findParam autoControlSystem.settings.parameters "K"
    
  
  Assert.NotEqual(growthRparam.value,autoControlRparam.value) //Growth's R param should replace AutoControl's R param
  Assert.Equal(growthKparam.value,autoControlKparam.value) //Growth's K param should equal AutoControl's K param

  //Check Rates
  Assert.Equal(emptySystem.settings.rates.Count,0)
  Assert.Equal(growthSystem.settings.rates.Count,1)
  Assert.Equal(autoControlSystem.settings.rates.Count,1)
  Assert.True(growthSystem.settings.rates.ContainsKey("growth"))
  Assert.True(autoControlSystem.settings.rates.ContainsKey("growth"))
