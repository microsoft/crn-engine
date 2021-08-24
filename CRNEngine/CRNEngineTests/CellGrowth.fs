// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.CellGrowth

open Xunit
open FsUnit
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Api
module CliInference = Microsoft.Research.CliLibrary.Inference
open Microsoft.Research.Filzbach

type species = Species
type value = Expression.t<string>
type key = Key<species>
type reaction = Reaction<species, value, Expression.t<key>>
type plottable = Expression.t<species>
type crn = Crn
type model = Model
type table = Table<float>

let species = Species.create
let float = Expression.Float
let value = Expression.Key
let init v s = Initial.create(false, v, s, None, None)
let event v s = Initial.create(false, v, s, None, None)


let sweep_test = Sweep.create( "mysweep", ["c" => [0.0;0.01;0.05;0.1;0.5;1.0]]) 
let sweep_single = Sweep.create("mysweep" , ["c" => [0.0;0.01;0.05;0.1;0.5;1.0;5.0;10.0]])

let crn varyN0 : crn = 
  let N0_variation = if varyN0 then "Multiple" else "Random"
  let prog_text = "
    directive parameters [
      c = 0.0;
      K = 100.0;
      r = 0.1, {interval = Log; distribution = Uniform(1e-3,1e0); variation = Random};
      IC50 = 1.0, {interval = Log; distribution = Uniform(1e-5,1e1); variation = Random};
      n = 1.0, {interval = Log; distribution = Uniform(1e-5,1e1); variation = Random};
      N0 = 8.0, {interval = Real; distribution = Uniform(5.0,15.0); variation = " + N0_variation + "};
    ]
    directive simulation {points = 1000; final = 140.0; plots = [N]}
    directive deterministic {stiff = true}
    directive sweeps [sweep_single = [c = [0.0;0.01;0.05;0.1;0.5;1.0;5.0;10.0]]]
    init N N0 | 
    ->[r*[N]*(1.0-[N]/K)/(1.0+(c/IC50)^n)] N
    " 
  in    
  Parser.from_string Crn.parse prog_text

let crn_lag_parse () : crn = 
    "directive simulation {final = 140.0; points = 1000; plots = [N]}
    directive deterministic {method = stiff}
    directive parameters [ 
      rlin = 0.1, {interval = Log; distribution = Uniform(1e-3,1e0); variation = Random};
      rexp = 0.1, {interval = Log; distribution = Uniform(1e-3,1e0); variation = Random};
      lag = 24.0, {interval = Real; distribution = Uniform(0.0,48.0); variation = Random};
      c = 0.0;
      K = 100.0;
    ]
    directive sweeps [mysweep = [c = [0.0; 0.01; 0.05; 0.1; 0.5; 1.0; 5.0; 10.0]]]
    directive inference {burnin = 5000; samples = 5000; simulation_choice = sundials}

    module Cell(IC50,n,N0) = {
      directive parameters [ 
        IC50 = 1.0, {interval = Log; distribution = Uniform(1e-5,1e1); variation = Random};
        n = 1.0, {interval = Log; distribution = Uniform(1e-5,1e1); variation = Random};
        N0 = 8.0, {interval = Real; distribution = Uniform(1.0,10.0); variation  =Random};
      ]
      init N N0 |
      ->[rlin*[i1]/(1.0+(c/IC50)^n)] N |
      ->[rexp*[i2]*[N]*(1.0-[N]/100.0)/(1.0 + (c/IC50)^n)] N 
    }
    
    system Compound1 = { Cell(IC50_C1,n_C1,N0_C1) | directive data [dat1] }
    system Compound2 = { Cell(IC50_C2,n_C2,N0_C2) | directive data [dat2] }"
    |> Parser.from_string Crn.parse


let crn_lag strIC50 strn strN0 : crn = 
  let prog_text = 
    "directive simulation {plots = [N]; points = 100; final = 140.0}
    directive simulator deterministic
    directive deterministic {stiff=true; reltolerance = 1e-5}
    directive sweeps [sweep_single = [c = [0.0;0.01;0.05;0.1;0.5;1.0;5.0;10.0]]]
    directive parameters [ 
      rlin = 0.1, {interval = Log; distribution=Uniform(1e-5,1e1); variation = Random};
      rexp = 0.1, {interval = Log; distribution=Uniform(1e-5,1e1); variation = Random};
      " + strIC50 + " = 1.0, {interval = Log;  distribution = Uniform(1e-5,1e1); variation = Random};
      " + strn + " = 1.0, {interval = Log;  distribution = Uniform(1e-5,1e1); variation = Random};
      " + strN0 + " = 8.0, {interval = Real; distribution = Uniform(1.0,20.0); variation = Random};
      lag = 24.0 , {interval = Real; distribution = Uniform(0.0,48.0); variation = Random};
      c = 0.0;
      K = 100.0;
    ]
    init i1 1.0 |
    init i1 -1.0 @ lag |
    init i2 1.0 @ lag |
    init i2 0.0 | 
    init N " + strN0 + " |
    ->[rlin*[i1] / (1.0 + (c/" + strIC50 + ")^" + strn + ")] N |
    ->[rexp*[i2]*[N]*(1.0-[N]/K) / (1.0 + (c/" + strIC50 + ")^" + strn + ")] N
    " 
  in
  Parser.from_string Crn.parse prog_text


/// Simulation test does 24 h of linear growth, followed by 24 h of exponential growth. We extract the time-point at 48 h for comparison.
[<Fact(DisplayName = "Cell growth - Oslo Simulation")>]
let oslo_simulation () = 
    let crn = crn_lag "IC50" "n" "N0" in
    let updated_settings = { crn.settings with sweeps = [sweep_test]; simulation = { crn.settings.simulation with final = 48.0 }} in
    let crn = { crn with settings = updated_settings } in
    let final = 
      crn.simulate_case () //(fun crn -> crn.to_oslo().simulate())
      |> List.map (fun r -> Table.find_column_last "N" r.table)

    // ND: I entered these by hand using reltol = 1e-3. The Test passes when changing to reltol=1e-5, which demonstrates some level of robustness to the algorithm.
    let expected = [56.11469; 55.46581; 52.98476; 50.11816;34.49525; 25.19413;] in    

    // Tolerate 1% error
    List.map2 (fun (x:float) (y:float) -> Assert.Equal(1.0, x/y, 2)) expected final

[<Fact(DisplayName = "Cell growth - Sundials Simulation")>]
let sundials_simulation () =
    Lib.check64bit()
    let crn = crn_lag "IC50" "n" "N0" in
    let updated_settings = { crn.settings with sweeps = [sweep_test]; simulation = { crn.settings.simulation with final = 48.0 }; simulator = Sundials } in
    let crn = { crn with settings = updated_settings } in
    let final = 
      crn.simulate_case() //(fun crn -> crn.to_oslo().simulate())
      |> List.map (fun r -> Table.find_column_last "N" r.table )

    // CG: this is a straight copy of the Oslo test
    // ND: I entered these by hand using reltol = 1e-3. The Test passes when changing to reltol=1e-5, which demonstrates some level of robustness to the algorithm.
    let expected = [56.11469; 55.46581; 52.98476; 50.11816;34.49525; 25.19413;] in    
    
    // Tolerate 1% error
    List.map2 (fun (x:float) (y:float) -> Assert.Equal(1.0, x/y, 2)) expected final

let inference_test nsamples fixed_parameters varyN0 simulator =
    let crn0 = crn varyN0
    let data = Dataset.create "data1_1rep" ("dat1_1rep.txt" |> Io.load_file |> Table<float>.parse_multiple_tsv 1) in  
    let parameters = 
        if fixed_parameters 
        then Parameters.fix crn0.settings.parameters 
        else crn0.settings.parameters 
    

    let inf_settings =
        let settings = { Inference_settings.defaults with burnin = nsamples; samples = nsamples; print_summary = false; print_console = false } in
        if fixed_parameters 
        then { settings with thin = 1; noise_parameter = Noise_parameter.Fixed 1.0 }
        else settings

    crn0.update_settings { crn0.settings with data = [ data ]
                                              parameters = parameters
                                              simulator = simulator
                                              inference = inf_settings }
    

// This is a more real example use case
(*let inference_replicates nsamples simulator = 
  let dat1 = Dataset.create "dat1" ("dat1.txt" |> Io.load_file |> Table.parse_multiple_tsv 1) in
  let dat2 = Dataset.create "dat2" ("dat2.txt" |> Io.load_file |> Table.parse_multiple_tsv 1) in 
  let crn = crn_lag2 () in
  let crn = { crn with settings = { crn.settings with data = [dat1; dat2]; simulator = simulator; inference = {crn.settings.inference with burnin = nsamples; samples = nsamples; parallel_computation = true} } } in
  Crn.infer crn*)

[<Fact(DisplayName="Cell growth - likelihood calculation")>]
let likelihood_calculation () = 
    let expected_lglk = -89700.0
    
    let inf = inference_test 1 true false Simulator.Oslo
    let lglk_serial = inf.infer().posterior.Head.["logLikelihood"]
    Assert.Equal(1.0, lglk_serial / expected_lglk, 1)

    let inf_parallel = inf.update_settings {inf.settings with simulation = {inf.settings.simulation with multicore=true}}
    let lglk_parallel = inf_parallel.infer().posterior.Head.["logLikelihood"]
    Assert.Equal(1.0, lglk_parallel / expected_lglk, 1)


[<Trait("Category", "Slow")>]
[<Fact(DisplayName="(slow) Cell growth - Oslo inference")>]
let ``Oslo Inference test``() = 
  
    let inf = inference_test 5000 false false Simulator.Oslo
    let sampling = inf.infer().posterior
    //let expected = [("r", 0.046); ("IC50", 3.24); ("n", 2.99); ("N0", 5.63)]
    let expected = [("r", 0.047); ("IC50", 3.26); ("n", 2.92); ("N0", 5.29)]
    let samples (p:string) = sampling |> List.averageBy (fun s -> s.[p]) 

    let sampled = expected |> List.map (fun (parameter_name,_) -> parameter_name, samples parameter_name)

    List.map2
        (fun (_, expected_value) (_, sampled_value) -> Assert.Equal (1.0, sampled_value / expected_value, 1))
        expected
        sampled

[<Trait("Category", "Slow")>]
[<Fact(DisplayName="Cell growth - Sundials inference")>]
let ``Sundials Inference test``() = 
  
    let inf = inference_test 5000 false false Simulator.Sundials
    let sampling = inf.infer().posterior
    //let expected = [("r", 0.046); ("IC50", 3.24); ("n", 2.99); ("N0", 5.63)]
    let expected = [("r", 0.047); ("IC50", 3.26); ("n", 2.92); ("N0", 5.29)]
    let samples (p:string) = sampling |> List.averageBy (fun s -> s.[p]) 
    let sampled = expected |> List.map (fun (parameter_name,_) -> parameter_name, samples parameter_name)

    List.map2
        (fun (_, expected_value) (_, sampled_value) -> Assert.Equal (1.0, sampled_value / expected_value, 1))
        expected
        sampled


(*[<Fact>]
let ``Parse CRN``() =     
    let crn = parse_example () in
    let exp_initials = [] in
    ()
    //Assert.Equal()*)