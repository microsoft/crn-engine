// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.InferenceTests

open Xunit
open FsUnit
open FsUnit.Xunit
module CliInference = Microsoft.Research.CliLibrary.Inference
open Microsoft.Research.Filzbach
open System.IO
open Microsoft.Research.CRNEngine

// Use a fixed noise model to prevent the likelihood scores drifting around during the burn-in
// Tolerate 1% error in the posterior mean. Pretty loose test, but reasonable for a fixed noise model

let (+/) path1 path2 = Path.Combine(path1, path2)
let DATA = "testData"
let MODELS = "Models"
let INFERENCE = "inference"
let projectDir = Directory.GetParent(System.IO.Directory.GetCurrentDirectory()).Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]

let degradation_example () = 
  let crn = 
    "directive simulation { final=2.0; plots=[X] }
    directive deterministic { stiff=true; reltolerance=1e-6 }
    directive parameters [ 
      k=1.0, { interval=Log; distribution=Uniform(1e-1,1e1); variation=Random };
      a=100.0;
    ]
    directive sweeps [ s = [ a = [1.0; 2.0] ] ]
    init X a |
    X ->{k}"
    |> Parser.from_string Crn.parse 
  let data = Dataset.create "degradation" [Table.create [log(2.0)] ["X1"] [[0.5]] ; Table.create [log(2.0)] ["X2"] [[1.0]]]
  { crn with settings = { crn.settings with data = [data] } }


let thalf () = Dataset.create "degradation" [Table.create [log 2.0; log 4.0] ["X1"] [[0.5];[0.25]] ; Table.create [log 2.0; log 4.0] ["X2"] [[1.0];[0.5]]]

let degradation_multiple_example () = 
  let crn = 
    "directive simulation { final=2.0; plots=[X]; multicore=true }
    directive simulator sundials
    directive parameters [ 
      k=1.0;
      a=100.0;
      k_1=1.0, { interval=Log; distribution=Uniform(0.1,10.0); variation=Random };
      k_2=1.0, { interval=Log; distribution=Uniform(0.1,10.0); variation=Random }; 
    ]
    directive sweeps [ s = [ (a,k) = [(1,k_1); (2,k_2)] ] ]
    directive inference { burnin = 5000; samples = 5000; partial=true }
    init X a |
    X ->{k}"
    |> Parser.from_string Crn.parse 
  let data = thalf ()
  { crn with settings = { crn.settings with data = [data] } }


let degradation_multiple_priors () = 
  let crn = 
    "directive simulation { final=2.0; plots=[X]; multicore=true }
    directive simulator sundials
    directive parameters [ 
      a=100.0;
      k=1.0, { interval=Log; distribution=Uniform(0.25,4.0); variation=Multiple }; 
    ]
    directive sweeps [ s = [ a = [1; 2] ] ]
    directive inference { burnin = 1000; samples = 1000; partial = true; seed=2 }
    init X a |
    X ->{k}"
    |> Parser.from_string Crn.parse 
  let data = thalf ()
  { crn with settings = { crn.settings with data = [data] } }

let simple_inference_test inf_settings simulator = 
  let crn = degradation_example ()
  let inf_settings = { inf_settings with print_console = false; print_summary = false }
  let crn = crn.update_settings {crn.settings with inference = inf_settings; simulator=simulator}
  let result = crn.infer()
  let ks = result.posterior |> List.map (fun ev -> ev.["k"], ev.["logLikelihood"])
  Seq.averageBy fst ks

[<Trait("Category", "Slow")>]
[<Fact(DisplayName="(slow) Inference - Simple inference (Oslo)")>]
let simple_inference_Oslo () =   
  let inf_settings = { Inference_settings.defaults with burnin = 2000; samples = 2000 }
  let simulator = Simulator.Oslo
  let kmean = simple_inference_test inf_settings simulator
  Assert.Equal(1.0, kmean, 2)

[<Trait("Category", "Slow")>]
[<Fact(DisplayName="Inference - Simple inference (Sundials)")>]
let simple_inference_Sundials () =   
  Lib.check64bit()
  let inf_settings = { Inference_settings.defaults with burnin = 2000; samples = 2000}
  let simulator = Simulator.Sundials
  let kmean = simple_inference_test inf_settings simulator
  Assert.Equal(1.0, kmean, 2)

let degradation_multiple prior = 
  let crn = 
    if prior 
    then degradation_multiple_priors ()
    else degradation_multiple_example ()
  in
  let crn = { crn with settings = { crn.settings with inference = { crn.settings.inference with print_console = false; print_summary = false }}}
  let result = crn.infer()
  let k1 = result.posterior |> List.map (fun k -> k.["k_1"])
  let k2 = result.posterior |> List.map (fun k -> k.["k_2"])
  List.average k1, List.average k2
  
[<Trait("Category", "Slow")>]
[<Fact(DisplayName="Inference - Multiple parameters")>]
let multiple_parameters () = 
  let k1, k2 = degradation_multiple false
  Assert.Equal(1.0, k1, 1);
  Assert.Equal(1.0, k2, 1)

[<Trait("Category", "Slow")>]
[<Fact(DisplayName="Inference - Multiple prior parameters")>]
let multiple_prior_parameters() = 
  let k1, k2 = degradation_multiple true
  Assert.Equal(1.0, k1, 1);
  Assert.Equal(1.0, k2, 1)

// Beware: This test has been known to fail for specific values of 'seed' (directive inference). Would be more robust to run multiple chains, when this feature is available (Neil)
[<Trait("Category", "Slow")>]
[<Fact(DisplayName="Inference - Multiple prior parameters over multiple CRNs")>]
let multiple_prior_parameters_over_multiple_CRNs () = 
  let crn = degradation_multiple_priors ()
  let crn = {crn with settings={crn.settings with inference={crn.settings.inference with print_summary=false;print_console=false}}}
  let model = { systems = [crn]; top = crn(*; inference = []*)}
  let result = model.infer ()
  let k1 = result.posterior |> List.averageBy (fun k -> k.["k_1"])
  let k2 = result.posterior |> List.averageBy (fun k -> k.["k_2"])
  let k3 = result.posterior |> List.averageBy (fun k -> k.["k_3"])
  let k4 = result.posterior |> List.averageBy (fun k -> k.["k_4"])
  Assert.Equal(1.0, k1, 1);
  Assert.Equal(1.0, k2, 1);
  Assert.Equal(1.0, k3, 1);
  Assert.Equal(1.0, k4, 1)


(* Test assignment of MLE simulations to named sweeps *)
let multisweep_example () = 
  let model = 
    "directive sweeps [ s1 = [ a = [1.0; 2.0] ]; s2 = [ a = [3.0; 4.0] ] ]
    directive simulation { final=1.0; plots=[X]; multicore=true }
    directive deterministic { stiff=true; reltolerance=1e-6 }
    directive parameters [ k=1.0, { interval=Log; distribution=Uniform(1e-1,1e1); variation=Random }
                         ; a=100.0 ]
    init X a |
    X ->{k}"
    |> Parser.from_string Model.parse
  let data = Dataset.create "test" [Table.create [log(2.0)] ["X1"] [[0.5]] ; Table.create [log(2.0)] ["X2"] [[1.0]]]
  model.update_settings { model.top.settings with data = [data;data] }

[<Fact(DisplayName="Inference - Multi-sweep inference")>]
let multi_sweep_inference () =
  Lib.check64bit()
  let model = multisweep_example ()
  let inf_settings = {Inference_settings.defaults with burnin = 100; samples = 100; print_console = false; print_summary = false}
  let simulator = Simulator.Sundials
  let model = model.update_settings { model.top.settings with inference = inf_settings; simulator = simulator }
  let result = model.infer ()
  let mlesims = Result<_>.group_sweeps result.mlesims
  let Ns = List.length mlesims
  Assert.Equal(2, Ns);   // Two sweeps
  mlesims |> List.map (fun rs -> 
  Assert.Equal (2, List.length rs))    // Two sweep simulations per sweep


(* Simple AM example *)
let am () = 
  let crn = 
    "directive inference  { burnin = 1000; samples = 1000; thin = 100 }
    directive simulation { final=1.0; points=1000; plots=[B; Y; X] }
    directive simulator deterministic
    directive deterministic { reltolerance=1e-5 }
    directive parameters [r = 0.2,  { interval=Log; distribution=Uniform(1e-2,1e2); variation=Random }]

    | X + Y ->{r} X + B
    | Y + X ->{r} Y + B
    | X + B ->{r} X + X
    | Y + B ->{r} Y + Y
    
    | 30 X
    | 20 Y 
    | 0 B"
    |> Parser.from_string Crn.parse
  let data = Dataset.create "AM_obs_noised" ["AM_obs_noised.csv" |> Io.load_file |> Table<float>.parse_csv]
  crn.update_settings { crn.settings with data = [data] }




[<Trait("Category", "Slow")>]
[<Fact(DisplayName="Inference - sequential")>]
let inference_sequential () =
  let crn = am ()
  let model = { Model.top = crn; Model.systems = [] }
  let (results, parameters) = model.infer_seq (fun _ -> ())
  results |> Seq.iter (fun res -> ())

(* Some Parser tests in the context of inference *)
[<Fact(DisplayName="Inference - parse inference settings")>]
let parse_Inference_settings () = 
  let burnin = 1234
  let samples = 2345
  let thin = 10
  let nm = Constant
  let prune = true
  let seed = 14u
  let inf_text = sprintf "{ burnin=%d; samples=%d; thin=%d; noise_model=constant; prune=%s }" burnin samples thin (string prune)
  let inf_settings = Parser.from_string Inference_settings.parse inf_text
  Assert.Equal (burnin, inf_settings.burnin)
  Assert.Equal (samples, inf_settings.samples)
  Assert.Equal (thin, inf_settings.thin)
  Assert.Equal (nm, inf_settings.noise_model)
  Assert.Equal (prune, inf_settings.prune)
  //Assert.Equal (seed, inf_settings.seed)

[<Fact(DisplayName="Inference - Parse inference with CRN")>]
let parse_inference_with_CRN () =   
  let burnin = 1234
  let samples = 2345
  let inf_text = sprintf """directive inference { burnin=%d; samples=%d }""" burnin samples
  let crn_text = "
  directive deterministic {stiff=true}
  X->{1.0}"
  in
  let crn = Parser.from_string Crn.parse (inf_text+crn_text)
  Assert.Equal (burnin, crn.settings.inference.burnin)
  Assert.Equal (samples, crn.settings.inference.samples)

[<Fact(DisplayName="Inference - Parse inference with systems")>]
let parse_inference_with_systems () =   
  let burnin = 1234
  let samples = 2345
  let inf_text = sprintf "directive inference { burnin=%d; samples=%d } system mycrn = { X->{1.0} }" burnin samples
  let model = Parser.from_string Model.parse inf_text in
  Assert.Equal (burnin, model.top.settings.inference.burnin)
  Assert.Equal (samples, model.top.settings.inference.samples)

[<Fact(DisplayName="Inference - Gaussian prior")>]
let gaussian_prior () = 
  let crn = 
    "directive simulation { final=1.0; plots=[X] }
    directive deterministic { stiff=true; reltolerance=1e-6 }
    directive simulator sundials
    directive parameters [ 
      k=1.0, { interval=Log; distribution=Normal(1.0,0.05); variation=Random };
      a=100.0;
    ]
    directive sweeps [ s = [ a = [1.0; 2.0] ] ]
    init X a |
    X ->{k}"
    |> Parser.from_string Crn.parse 
  in
  let data = Dataset.create "degradation" [Table.create [log(2.0)] ["X1"] [[0.5]] ; Table.create [log(2.0)] ["X2"] [[1.0]]]
  let result = { crn with settings = { crn.settings with data = [data] } }.infer()
  let ks = result.posterior |> Seq.map (fun ev -> ev.["k"], ev.["logLikelihood"])
  let kmean = Seq.averageBy fst ks
  Assert.Equal(1.0, kmean, 2)

let runTest name (multiples:Map<string,string list>) = 
  let dataDir = projectDir +/ DATA +/ INFERENCE
  let modelsDir = projectDir +/ MODELS

  let code = (projectDir +/ MODELS +/ name + ".crn") |> File.ReadAllText
  // let expected = failwith ""
  let parsed   = InferenceSiteGraph.from_string code
  let make_quiet (model:Model) = model.map_crns (fun crn -> {crn with settings = {crn.settings with inference = {crn.settings.inference with print_console=false;print_summary=false}}})
  let parsed = InferenceSiteGraph.mapNodes (fun _ model -> make_quiet model) parsed
  let igraph = InferenceSiteGraph.expandAndLift parsed
  let debug = InferenceSiteGraph.debugString igraph
  
  // Load the datasets that have been specified
  let loadedGraph = InferenceSiteGraph.load_data dataDir igraph

  // run inference and print HTML files
  let res = 
      loadedGraph 
      |> InferenceSiteGraph.inferWith (fun model ->
          let result = model.infer()
          let outdir = (modelsDir +/ "Test_" + name +/ model.top.name)
          let html_results = Html.mcmc_to_results model.top.settings model result
          Assert.Equal<string list>(multiples.[model.top.name], html_results.multiple_paras)
          CliInference.process_mcmc outdir true model result
          result.to_summary()
      )
  () 

[<Fact(DisplayName = "Inference Graph - Characterization control phase")>]
let inferenceGraph () = 
  ["Receivers_control", ["rc"]] 
  |> Map.ofList
  |> runTest "Control"

[<Trait("Category", "Slow")>]
[<Fact(DisplayName = "Inference Graph - Autofluorescence")>]
let autofluorescence () = 
  ["Auto", []; "Auto_control", ["rc"]; "Auto_growth", ["r"; "K"; "tlag"]] 
  |> Map.ofList
  |> runTest "Autofluorescence"

[<Trait("Category", "Slow")>]
[<Fact(DisplayName = "Inference Graph - Reduced double receiver")>]
let double_receiver () = 
  ["Receivers", []; "Receivers_control", ["rc"]; "Receivers_growth", ["r"; "K"; "tlag"]] 
  |> Map.ofList
  |> runTest "GoalReceivers"