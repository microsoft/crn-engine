module Microsoft.Research.CRNEngine.Tests.CrnTest
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Api
open Xunit
open FsUnit.Xunit

#nowarn "58"

let dataset (s:string,ts:Table<float> list) = Dataset.create s ts

let equal (tolerance:float) (x:float) (y:float) = abs(x-y) < tolerance
let equal_list (tolerance:float) (l1:float list) (l2:float list) = List.fold (&&) true (List.map2 (equal tolerance) l1 l2 )
let empty = Crn.empty
let incremental_test() = 
  let A = species "A"
  let crn:Crn = {
    name = "crn";
    settings = { empty.settings with 
      simulation = { 
        name = "";
        points = 1000;
        initial = 0.0;
        final = 1000.0;
        plots = [ Expression.Key (Key.Species A) ];
        plotcolours = [];
        seed = None;
        kinetics = Kinetics.Contextual;
        times = [];
        prune = false;
        multicore = false;
        data = [];
        sweeps = [];
      };
      units = { 
        concentration = Molar -9;
        time = Seconds 1.;
        space = Metres -3;
      };
      simulator = Simulator.Oslo;
      inference = { Inference_settings.defaults with  
        name = "default";
        burnin = 100;
        samples = 100;
        thin = 10;
        seed = 0u;
        noise_parameter = Noise_parameter.Random;
        noise_model = Constant;
        prune = false;
        timer = false;
        partial_evaluation = false;
      };
      parameters = [ 
        Parameter.create(
          name = "r",
          value = 0.1,
          prior = Some { 
            interval = Interval.Log;
            distribution = Distribution.Uniform{min=0.01;max=1.0};
            variation = Variation.Random;
          }
        )
      ];
      sweeps = Sweeps.create [ 
        "s" => [ "r" => [ Expression.Float 0.1 ] ] 
      ];
      data = [ 
        Dataset.create "d" [ 
          { times = [ 0.; 1.; 2.; 3. ];
            columns = [ { name = "A"; values = [ 1000.; 910.; 800.; 600. ] } ]; 
          };
        ];
      ];
    };
    reactions = 
      let entry : Mset.entry<Species> = { element = A; multiplicity = 1 } in
      [ 
      Reaction.create(
        [],
        [ entry ],
        None,
        Rate.MassAction(Expression.Key "r"),
        []
      )
    ];
    initials = [ 
      Initial.create(
        constant = false,
        value = Expression.Float 1000.,
        species = A,
        time = None,
        spatial = None
      )
    ];
    attributes = Stringmap.empty |> Stringmap.add "A" { name = "A"; structure = ""; svg = "" };
  }
  crn.to_ode().infer()

let catalytic_example () =
  let Catalyst = species "Catalyst" 
  let Input1 = species "Input1" 
  let Input2 = species "Input2" 
  let Intermediate1 = species "Intermediate1" 
  let Intermediate2 = species "Intermediate2" 
  let Intermediate3 = species "Intermediate3" 
  let Output1 = species "Output1" 
  let Output2 = species "Output2" 
  let Output3 = species "Output3" 
  let bind3   = float 0.00042 
  let unbind3 = float 0.04 
  let bind5   = float 0.00065 
  let unbind5 = float 0.004 
  { empty with
    settings = { empty.settings with 
      simulation = { empty.settings.simulation with
        points = 1000;
        initial = 0.0;
        final = 7000.0;
        plots = plots [Catalyst; Input1; Input2; Output1; Output2];
        kinetics = Kinetics.Contextual;
      };
      stochastic = { empty.settings.stochastic with scale = 500.0 };
    };
    initials = Initials.create [ 
      float 10.0, Catalyst;
      float 10.0, Input1;
      float 13.0, Input2;
    ];
    reactions = Reactions.create [
      [Catalyst;Input1], unbind5 <-> bind5, [Intermediate1];
      [Intermediate1], bind3 <-> unbind3, [Intermediate2; Output1];
      [Intermediate2; Input2], !-> bind3, [Intermediate3; Output2];
      [Intermediate3], bind5 <-> unbind5, [Catalyst; Output3];
    ];
 }

let binding_example (scale_setting:float) = 
  let A = species "A"
  let B = species "B"
  let C = species "C"
  let r = rate "r"
  { empty with
    settings = { empty.settings with 
      stochastic = { empty.settings.stochastic with 
        scale = scale_setting; 
      };
    };
    reactions = Reactions.create [ 
      [A; B], !-> r, [C];
    ];
    initials = Initials.create [ 
      float 10.0, A, None;
      float 10.0, B, None;
    ];
  }

let binding_example_scaled (scale:float) = 
  let A = species "A"
  let B = species "B"
  let C = species "C"
  let r = rate "r"
  { empty with
    reactions = Reactions.create [ 
      [A; B], !-> (r / float scale), [C];
    ];
    initials = Initials.create [ 
      float 10.0 * float scale, A;
      float 10.0 * float scale, B;
    ];
  }

[<Fact(DisplayName="CRN - scale factor - binding")>]
let crn_scale_factor_binding () =
  Assert.Equal<Crn>((binding_example 3.0).scale(), binding_example_scaled 3.0)

let binding_example_functional (scale_setting:float) = 
  let A = species "A"
  let B = species "B"
  let C = species "C"
  let expression = 
    Expression.Key(Key.Species A) *
    Expression.Key(Key.Species B) *
    Expression.Key(Key.Parameter "r")
  { empty with
    settings = { empty.settings with 
      stochastic = { empty.settings.stochastic with 
        scale = scale_setting; 
      };
    };
    reactions = Reactions.create [ 
      [A; B], !~> expression, [C];
    ];
    initials = Initials.create [ 
      float 10.0, A;
      float 10.0, B;
    ];
  }

let binding_example_functional_scaled (scale:float) = 
  let A = species "A"
  let B = species "B"
  let C = species "C"
  let expression = 
    (Expression.Key(Key.Species A) / float scale) *
    (Expression.Key(Key.Species B) / float scale) *
    Expression.Key(Key.Parameter "r")
  { empty with
    reactions = Reactions.create [ 
      [A; B], !~> (expression * float scale), [C];
    ];
    initials = Initials.create [ 
      float 10.0 * float scale, A;
      float 10.0 * float scale, B;
    ];
  }
[<Fact(DisplayName="CRN - scale factor - binding - functional")>]
let crn_scale_factor_binding_functional () =
  Assert.Equal<Crn>((binding_example_functional 3.0).scale(), binding_example_functional_scaled 3.0)

let degradation_example () = 
  let X = species "X" 
  let k = rate "k" 
  let a = rate "a" 
  { empty with
    settings = { empty.settings with 
      simulation = { empty.settings.simulation with 
        final = log 2.0; 
        plots = plots [X];
      };
      deterministic = { empty.settings.deterministic with 
        stiff=true; 
        reltolerance=1e-5; 
      };
      data = List.map dataset [ 
        "degradation_data" => [Table.create [log(2.0)] ["X1";"X2"] [[0.5; 1.0]]] 
      ];
      sweeps = Sweeps.create [ 
        "s" => ["a" => [1.0; 2.0]]; 
      ];
      parameters = Parameters.create [
        "k" => 1.0, Some {Prior.empty with distribution=Distribution.Uniform{min=1e-1;max=1e1}};
        "a" => 100.0, None;
      ];
    };
    reactions = [Reaction.create([X], !-> k, [])];
    initials = [Initial.create(a, X)];
  }

let degradation_example_functional () = 
  let crn = degradation_example () 
  let X = species "X" 
  let expression = Expression.Key(Key.Parameter "k") * Expression.Key(Key.Species X) 
  let reactions = Reactions.create [[X], !~> expression, [] ]
  {crn with reactions = reactions}

let activation_example () = 
  let X = species "X" 
  let Xp = species "Xp" 
  let Y = species "Y" 
  let Yp = species "Yp" 
  let activate = rate "activate" 
  let deactivate = rate "deactivate" 
  { empty with
    settings = { empty.settings with 
      simulation = { empty.settings.simulation with 
        final = 2.0; 
        plots = plots [X;Y];
      };
      deterministic = { empty.settings.deterministic with 
        stiff=true; 
        reltolerance=1e-5;
      };
      parameters = Parameters.create [ 
        "activate" => 0.1, None;
        "deactivate" => 0.01, None;
      ];
    };
    reactions = Reactions.create [ 
      [Xp; Y], !-> activate, [X; Yp];
      [X; Yp], !-> deactivate, [Xp; Y];
    ];
    initials = Initials.create [ 
      float 100.0, Xp;
      float 100.0, Y;
      float 0.0, X;
      float 0.0, Yp;
    ];
  }.create_blank_attributes()

let activation_example_parsed () = Crn.from_string "
  directive simulation{ final=2.0; plots = [X;Y]}
  directive deterministic{ stiff=true; reltolerance=1e-5}
  directive parameters[ activate=0.1; deactivate=0.01]

  | 100 Xp
  | 100 Y
  | 0 X
  | 0 Yp
  | Xp + Y ->{activate} X + Yp
  | X + Yp ->{deactivate} Xp + Y
  "
 
let am_example () =  
  let X = species "X" 
  let Y = species "Y" 
  let B = species "B" 
  { empty with
    settings = {empty.settings with 
      simulation = { empty.settings.simulation with 
        plots = plots [X;Y;B];
      };
    };
    reactions = Reactions.create [ 
      [X; Y], !-> (float 0.1), [X; B];
      [Y; X], !-> (float 0.2), [Y; B];
      [X; B], !-> (float 0.3), [X; X];
      [Y; B], !-> (float 0.4), [Y; Y];
    ];
    initials = Initials.create [ 
      float 30.0, X;
      float 20.0, Y;
      float 0.0, B;
    ];
  }.create_blank_attributes()

let am_example_parsed () = Crn.from_string "
  | 30 X 
  | 20 Y 
  | X + Y ->{0.1} X + B
  | Y + X ->{0.2} Y + B 
  | X + B ->{0.3} 2X 
  | Y + B ->{0.4} 2Y
  " 

[<Fact(DisplayName="CRN - simulation directive and reaction")>]
let crn_simulation_directive_and_reaction () =
  let x = Species.create "X"
  let expected = 
    { empty with
      settings = { empty.settings with
          simulation = { empty.settings.simulation with 
            final = 20.0; 
            points = 20; 
            plots = [plot x]
          };
      };
      initials = [Initial.create(float 0.0, x)];
      reactions = [Reaction.create([x], !-> (float 1.0), [])];
    }.create_blank_attributes()
  let got = Crn.from_string "directive simulation { final = 20.0; points = 20 } X ->{1.0}"
  Assert.Equal<Crn>(expected, got)
  //Assert.Equal(expected.to_string, got.to_string )

[<Fact(DisplayName="CRN - parse, am example, saturates initials and plots, preserves order")>]
let parse_am_example_saturates_initials_and_plots_preserves_order () =
  let expected:Crn = am_example()
  let actual:Crn = am_example_parsed()
  Assert.Equal<Crn>(expected, actual)
  //Assert.Equal(expected.to_string , actual.to_string )

[<Fact(DisplayName="CRN - parse, activation example")>]
let parse_activation_example () =
  let expected:Crn = activation_example()
  let actual:Crn = activation_example_parsed()
  Assert.Equal<Crn>(expected,actual)
  //Assert.Equal(expected.to_string, actual.to_string)

[<Fact(DisplayName="CRN - simulate_oslo_single, degradation example")>]
let simulate_oslo_single_degradation_example () = 
  let crn:Crn = degradation_example ()
  let table:Table<float> = crn.to_oslo().simulate()
  let final:float = Table.find_column_last "X" table
  Assert.Equal(1.0, 50.0/final, 3)

[<Fact(DisplayName="CRN - simulate_oslo_single, degradation example with functional rate")>]
let simulate_oslo_single_degradation_example_with_functional_rate () = 
  let crn:Crn = degradation_example_functional ()
  let table:Table<float> = crn.to_oslo().simulate()
  let final:float = Table.find_column_last "X" table
  Assert.Equal(1.0, 50.0/final, 3)

[<Fact(DisplayName="CRN - simulate_ssa_single, degradation")>]
let simulate_ssa_single_degradation () = 
  let crn:Crn = degradation_example () 
  let crn:Crn = { crn with settings = { crn.settings with simulation = { crn.settings.simulation with seed = Some 0 } }} 
  let table:Table<float> = crn.to_ssa().simulate()
  let final:float = Table.find_column_last "X" table 
  Assert.Equal(1.0, 57.0/final, 3)

[<Fact(DisplayName="CRN - simulate_oslo_single, activation example")>]
let simulate_oslo_single_activation_example () = 
  let crn:Crn = activation_example ()
  let table:Table<float> = crn.to_oslo().simulate()
  let final:float = Table.find_column_last "X" table
  Assert.Equal(76.0, final, 1)

[<Fact(DisplayName="CRN - test comparison")>]
let test_comparison () = equal_list 0.1 [1.0;2.0;3.0] [1.0;2.0;3.0]

[<Fact(DisplayName="CRN - simulate_lna_single, degradation example")>]
let simulate_lna_single_degradation_example () =
  let crn = degradation_example ()
  let expected = 50.0
  let sim_data = crn.to_lna().simulate()
  let got = Table.find_column_last "X" sim_data |> (fun x -> x.mean)
  Assert.Equal(expected, got, 1)

[<Fact(DisplayName="CRN - simulate_lna_single AM example")>]
let simulate_lna_single_AM_example () =
  let am_example = Crn.from_string "
    directive simulation { final=1.0; points=10000 }
    directive parameters [r = 1.0]

    | X + Y ->{r} X + B
    | Y + X ->{r} Y + B
    | X + B ->{r} X + X
    | Y + B ->{r} Y + Y
    | 30 X
    | 10 Y
    "
  let expectedX = 40.0
  let expectedY = 0.0
  let sim_data = am_example.to_lna().simulate()
  let gotX = Table.find_column_last "X" sim_data |> fun x -> x.mean
  let gotY = Table.find_column_last "Y" sim_data |> fun x -> x.mean
  Assert.Equal(expectedX, gotX, 1); Assert.Equal(expectedY, gotY, 1)

[<Fact(DisplayName="CRN - simulate_cme, degradation example")>]
let simulate_cme_degradation_example () =
  let degradation = degradation_example ()   
  let env = Environment.empty
  let base_environment = Parameters.to_env degradation.settings.parameters
  let full_env = Environment.extend env base_environment
  let populations, events = degradation.initials |> Initial<Species,Value>.to_initialpops_events full_env degradation.settings.simulation.initial
  let cme = Cme.create populations events degradation.settings.deterministic degradation.settings.simulation
  (*
  let expected = 50.0
  let sim_data = Cme.integrate_CME env cme 
  let got = Array.last sim_data.data.["X"] |> (fun x -> x.mean)

  Assert.Equal(expected, got, 1)
  *)
  ()

[<Fact(Skip="Scale not implemented for CME yet", DisplayName="CRN - simulate CME with scale")>]
let simulate_cme_scaled () =
  let scale = 0.5
  let crn = Crn.from_string (sprintf "
    directive simulation { final=1.0; points=200 }
    directive stochastic { scale=%1.1f }
    directive parameters [r = 1.0]

    | X + Y ->{r} X + B
    | Y + X ->{r} Y + B
    | X + B ->{r} X + X
    | Y + B ->{r} Y + Y
    | 30 X
    | 20 Y
    " scale)
  let ub = 50.0*scale
  let lb = ub*0.5
  let sim_data = crn.simulate_cme ()
  let got = sim_data.columns.Head.values |> List.last |> fun x -> x.mean

  Assert.InRange(got, lb, ub)

[<Fact(DisplayName="CRN - compare parse and to_string, round-trip")>]
let compare_parse_and_to_string_round_trip () =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let am_text = """directive simulation {final=1; points=100; plots=[B; X; Y]; }
directive simulator deterministic
directive deterministic {stiff=true; reltolerance=1E-06; abstolerance=1E-08}
directive parameters [
  N = 20;
  rexp = 0.1, {interval=Log; distribution=Uniform(0.001,1); variation=Random};
  c = 0;
]
directive sweeps [
  mysweep = [c = [0; 0.01; 0.05; 0.1; 0.5; 1; 5; 10]];
]

| 30 X
| 20 Y
| X + B ->{r} X
| X + Y ->{r} X + B
| Y + B ->{r} Y
| Y + X ->{r} Y + B""".Replace("\r\n", "\n")
  let crn' = Crn.from_string am_text
  let string = crn'.to_string()
  Assert.Equal(am_text, string)

[<Fact(DisplayName="CRN - Crn.to_string elides default settings")>]
let crn_to_string_elides_default_settings() =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let input_text = """directive simulation {final=1; points=100; plots=[B; X; Y]}
directive deterministic {stiff=true; reltolerance=1E-06; abstolerance=1E-08}
directive simulator stochastic
directive parameters [
  N = 20;
  rexp = 0.1, {interval=Log; distribution=Uniform(0.001,1); variation=Random};
  c = 0;
]
directive sweeps [
  mysweep = [c = [0; 0.01; 0.05; 0.1; 0.5; 1; 5; 10]];
]

| 30 X
| 20 Y
| 0 B
| X + B ->{r} X
| X + Y ->{r} X + B
| Y + B ->{r} Y
| Y + X ->{r} Y + B""".Replace("\r\n", "\n")
  
  let expected = """directive simulation {final=1; points=100; plots=[B; X; Y]; }
directive deterministic {stiff=true; reltolerance=1E-06; abstolerance=1E-08}
directive parameters [
  N = 20;
  rexp = 0.1, {interval=Log; distribution=Uniform(0.001,1); variation=Random};
  c = 0;
]
directive sweeps [
  mysweep = [c = [0; 0.01; 0.05; 0.1; 0.5; 1; 5; 10]];
]

| 30 X
| 20 Y
| X + B ->{r} X
| X + Y ->{r} X + B
| Y + B ->{r} Y
| Y + X ->{r} Y + B""".Replace("\r\n", "\n")

  let crn = Crn.from_string input_text
  let code = crn.to_string()
  Assert.Equal(expected, code)


[<Fact(DisplayName="CRN - time species and conditional plot test")>]
let time_species_and_conditional_plot_test () = 
  let text = """directive simulation { final=10.0; points=300; plots=[(if [X] >= 10 then 10 else [X]); X] }
directive deterministic {stiff=true}
directive simulator deterministic
| 100 X
| X ->[[X]*[time]]"""
  (Crn.from_string text).simulate_case |> ignore

[<Fact(DisplayName="CRN - conditional time test")>]
let conditional_time_test () =
  let text = """
{plots = [ (if [time] < tlag then c0 else (K*c0)/(c0+(K-c0)*(-r*([time]-tlag)))) * [fp] + f0 ]}"""
  let res = Simulation_settings.from_string Functional2.parse_plot text
  ()

[<Fact(DisplayName="CRN - time species and conditional plot test 2")>]
let time_species_and_conditional_plot_test_2 () = 
  let text = """directive simulation { final=20.0; points=1000; plots=[( if [time] < tlag then c0 else K*c0/(c0+(K-c0)*2.718^(-r*([time]-tlag))))*[fp]+f0]   }
directive parameters [ 
  rc=1e4,   { interval=Log; distribution=Uniform(1e0,1e5); variation=Multiple };
 f0=5000.0,{ interval=Real; distribution=Uniform(0.0,10000.0); variation=Random };
 c0=0.002; r=1.0; K=2.0; mat=1.0; tlag=5;
]
directive simulator deterministic 

| (rc/(r+mat)) ifp 
| (mat*rc/(r+mat)/r) fp
| fp ->[[fp]* r*(1- ( if [time] < tlag then c0 else K*c0/(c0+(K-c0)*2.718^(-r*([time]-tlag)))))/K] 
| ifp ->[[ifp]* r*(1- ( if [time] < tlag then c0 else K*c0/(c0+(K-c0)*2.718^(-r*([time]-tlag)))))/K]
| ->{rc} ifp 
| ifp ->{mat} fp"""
  Crn.from_string text |> ignore

[<Fact(DisplayName="CRN - directive rates test")>]
let directive_rates_test () = 
  let text = """
    directive simulation { 
      final=20.0; 
      points=1000; 
      plots=[plot]   
    }
    directive parameters [ 
      rc=1e4,   { interval=Log; distribution=Uniform(1e0,1e5); variation=Multiple };
      f0=5000.0,{ interval=Real; distribution=Uniform(0.0,10000.0); variation=Random };
      c0=0.002; r=1.0; K=2.0; mat=1.0; tlag=5;
    ]
    directive rates [
      rate         = r*(1- [commonSubExp])/K;
      plot         = [commonSubExp]*[fp]+f0;
      commonSubExp = if [time] < tlag then c0 else K*c0/(c0+(K-c0)*2.718^(-r*([time]-tlag)));
    ]
    directive simulator deterministic 

    | (rc/(r+mat)) ifp
    | (mat*rc/(r+mat)/r) fp
    //  
    | fp -> [[fp]*[rate]] 
    | ifp ->[[ifp]*[rate]] 
    | ->{rc} ifp 
    | ifp ->{mat} fp"""

  let crn = Crn.from_string text 
  let setSimulator s = { crn with settings = {crn.settings with simulator = s}}
  
  // test Oslo
  (setSimulator Simulator.Oslo).simulate_case |> ignore
  
  // test Sundials
  (setSimulator Simulator.Sundials).simulate_case |> ignore


[<Fact(DisplayName="CRN - Modules - receiver")>]
let modules_receiver () =
  let code = """
directive parameters [
    tlag = 1.0, { interval=Log; distribution=Uniform(1e-7,10.0); variation=Multiple };
    rs=1e3,     { interval=Log; distribution=Uniform(1e-2,1e4); variation=Random };
    Kc=0.9,     { interval=Real; distribution=Uniform(0.5,1.0); variation=Random };
    nc=1.0,     { interval=Real; distribution=Uniform(1.0,100.0); variation=Random };
    rc = 1e4,   { interval=Log; distribution=Uniform(1e0,1e5); variation=Multiple };
    eps=0.0,    { interval=Real; distribution=Uniform(0.0,0.2); variation=Random };
    n=0.8,      { interval=Real; distribution=Uniform(0.6,2.0); variation=Random };
    aR=1e0,   	{ interval=Log; distribution=Uniform(1e-12,1e-3); variation=Random };
    dyfp=0.1,   { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random };
    aY=1e0, 	  { interval=Log; distribution=Uniform(1e-3,1e3); variation=Random };
    dR=0.1,     { interval=Log; distribution=Uniform(1e-3,1e1); variation=Random };
    matY=1.0,   { interval=Log; distribution=Uniform(1e-1,1e1); variation=Random };
    C6 = 0.0; 
    autoY=0.1; 
  ]

directive rates [ 
  x                = if [time] < tlag then c0 else (K*c0/(c0+(K-c0)^(-r*([time]-tlag))));
  specificLogistic = r*(1-[x]/K);
  switch           = (rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc);
  ]

module matY(gamma) = {
  | yfp ->{matY} myfp 
  | myfp ->[[myfp]*[gamma]]  
  | myfp ->{dyfp} 
}
module control(produce, gamma, degradation) = {
  | ->[[produce]] fp 
  | fp ->[[fp]*[gamma]] 
  | fp ->{degradation}
}

module receiver(produce, gamma) = {
  | ->[[produce]*aR] R
  | R ->[[R]*[gamma]]
  | R ->{dR}
  | ->[[produce]*autoY] yfp
  | yfp ->[[yfp]*[gamma]]
  | yfp ->{dyfp} 
  | ->[[produce]*aY*(C6^n*[R]^n + eps) / (C6^n*[R]^n + 1.0)] yfp 
}

// receiver; switch; specificLogistic; lag; matY; 
receiver(switch, specificLogistic)
| matY(specificLogistic)
| control(switch, specificLogistic, 0.0)
"""

  let crn  = Crn.from_string code
  let text = crn.to_string
  ()

[<Fact(DisplayName="CRN - Modules - digital logic")>]
  let digital_logic () =
  let code = """(* modular square root circuit, catalytic with fault tolerance *)

  directive simulation { final = 0.2; points = 1000; plots = [X11; X21; X31; X41; Y10; Y11; Y20; Y21] }
  directive simulator deterministic
  directive parameters [k = 0.01]

  module RESTORE(a0,a1,b0,b1) = {
    | 50 b0 | 50 b1 
    | a0 + a0 + b1 ->{k} a0 + a0 + b0 
    | a1 + a1 + b0 ->{k} a1 + a1 + b1 
  }

  module AND(a0,a1,b0,b1,c0,c1,c0int,c1int) = { 
    | 50 c0int | 50 c1int 
    | a0 + b0 + c1int ->{k} a0 + b0 + c0int 
    | a0 + b1 + c1int ->{k} a0 + b1 + c0int 
    | a1 + b0 + c1int ->{k} a1 + b0 + c0int 
    | a1 + b1 + c0int ->{k} a1 + b1 + c1int
    | RESTORE(c0int,c1int,c0,c1) 
  }
  module NAND(a0,a1,b0,b1,c0,c1,c0int,c1int) = { AND(a0,a1,b0,b1,c1,c0,c1int,c0int) }
  module OR(a0,a1,b0,b1,c0,c1,c0int,c1int) = { AND(a1,a0,b1,b0,c1,c0,c1int,c0int) }
  module NOR(a0,a1,b0,b1,c0,c1,c0int,c1int) = { AND(a1,a0,b1,b0,c0,c1,c0int,c1int) }
  module ANDNOT(a0,a1,b0,b1,c0,c1,c0int,c1int) = { AND(a0,a1,b1,b0,c0,c1,c0int,c1int) }
  module NAND3(a0,a1,b0,b1,c0,c1,d0,d1,d0int,d1int,c0inta,c1inta,c0intb,c1intb) = {
    | AND(a0,a1,b0,b1,d0int,d1int,c0inta,c1inta) 
    | NAND(c0,c1,d0int,d1int,d0,d1,c0intb,c1intb) 
  }

  | 90 X10 | 10 X11
  | 10 X20 | 90 X21
  | 90 X30 | 10 X31
  | 10 X40 | 90 X41

  | NOR(X10,X11,X20,X21,Z10,Z11,c0int1,c1int1) 
  | ANDNOT(X30,X31,X40,X41,Z20,Z21,c0int2,c1int2)
  | OR(X30,X31,X40,X41,Y20,Y21,c0int3,c1int3)
  | NAND3(X30,X31,X40,X41,Z10,Z11,Z40,Z41,d0int2,d1int2,c0int4,c1int4,c0int5,c1int5)
  | OR(Z10,Z11,Z20,Z21,Z30,Z31,c0int6,c1int6)
  | NAND(Z40,Z41,Z30,Z31,Y10,Y11,c0int7,c1int7)"""
  let crn  = Crn.from_string code
  let text = crn.to_string ()
  Assert.Equal(None, crn.initials |> List.tryFind (fun i -> i.species.name = "c0"))



[<Fact(DisplayName="CRN - Parsing - Lorentz, plot expressions")>]
  let lorentz () =
  let code = """(* Lorenz attractor as a dual-rail CRN analog dynamical system *)

  directive simulation { final = 30.0; points = 1000; plots = [ [Xp]-[Xm]; [Yp]-[Ym]; [Zp]-[Zm] ]} 
  directive simulator deterministic
  directive parameters [unity=1.0; sigma=10.0; r=28.0; b=2.667; fast=10.0]

  | 5 Xp
  | 0 Xm
  | 5 Yp
  | 0 Ym
  | 2 Zp
  | 0 Zm
  | Xp + Xm ->{fast} 
  | Yp + Ym ->{fast} 
  | Zp + Zm ->{fast} 
  | Yp ->{sigma} Yp + Xp 
  | Xm ->{sigma} 
  | Ym ->{sigma} Ym + Xm 
  | Xp ->{sigma} 
  | Xp ->{r} Xp + Yp 
  | Ym ->{unity} 
  | Xm + Zp ->{unity} Xm + Zp + Yp 
  | Xp + Zm ->{unity} Xp + Zm + Yp 
  | Xm ->{r} Xm + Ym 
  | Yp ->{unity} 
  | Xp + Zp ->{unity} Xp + Zp + Ym 
  | Xm + Zm ->{unity} Xm + Zm + Ym 
  | Xp + Yp ->{unity} Xp + Yp + Zp 
  | Xm + Ym ->{unity} Xm + Ym + Zp 
  | Zm ->{b} 
  | Xm + Yp ->{unity} Xm + Yp + Zm 
  | Xp + Ym ->{unity} Xp + Ym + Zm 
  | Zp ->{b} """
  let crn  = Crn.from_string code
  let text = crn.to_string ()
  ()
//[<Fact>] 
//let ``module fail 1`` () =
//  let code = "module m(a,b,c) = {} m(1.0, 2+2)"
//  Assert.ThrowsAnyAsync<System.Exception> (System.Func (fun () -> code |> Crn.from_string))

  // test CME - takes a very long time
  //  setSimulator Microsoft.Research.CRNEngine.Crn_settings.CME |> Crn.simulate |> ignore
  
  // test LNA - takes a very long time
  //  setSimulator Microsoft.Research.CRNEngine.Crn_settings.LNA |> Crn.simulate |> ignore

  // test SSA - SSA fails because it doesn't support time dependent rates
  //setSimulator Microsoft.Research.CRNEngine.Crn_settings.SSA |> Crn.simulate |> ignore