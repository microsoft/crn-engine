// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.Program

open Microsoft.Research.Filzbach
open Microsoft.Research.CRNEngine
module CliInference = Microsoft.Research.CliLibrary.Inference

let inference_test_postprocess fname (inference_output:Crn * Filzbach.Sampling * (string * float Table) []) = 
  let crn, sampling, sims = inference_output in
  
  // Write posterior to file
  let pnames = List.ofArray sampling.space.Names in
  let header = "Iteration,logLikelihood," + (String.concat "," pnames) in
  let strsamples = List.mapi (fun i (s:Parameters.EvaluatedValues) -> string i + "," + (String.concat "," (List.map string (s.logLikelihood :: (List.map (fun (name:string) -> s.values.[name]) pnames))))) (List.rev sampling.chain) in
  Io.write_file (fname + ".csv") (String.concat "\r\n" (header :: strsamples));
  
  printfn "log-Likelihood = %f" sampling.chain.Head.logLikelihood



[<EntryPoint>]
let main(args) = 

  // Run the degradation simulation test
  //SimpleTests.simple_ODE_test () |> ignore;
  //InferenceTests.simple_inference_test { Inference.default_settings with burnin = 2000; samples = 2000 } |> ignore;

  // Run the cell growth simulation test
  //CellGrowth.oslo_simulation_test () |> ignore;

  // Run the cell growth inference test
  //CellGrowth.inference_test 1 true Crn.Oslo |> inference_test_postprocess "likelihoodCalculation";
  CellGrowth.inference_test 5000 false true Simulator.Oslo |> ignore;

  // Run the multiple dataset cell growth model
  //CellGrowth.inference_replicates 10000 Crn.Oslo;
  //CellGrowth.inference_replicates 5000 Crn_settings.Sundials |> ignore;
  //InferenceTests.``Multi-sweep inference`` () |> ignore;

  // Run an example from dynamic characterization
  //CharacterizationTests.simulation_test ();
  //CliInference.do_inference CliInference.MCMC (CharacterizationTests.inference_test 150) "characterization_test" "test" |> ignore;

  // Run the incremental inference test
  //CrnTest.incremental_test () |> Seq.iter (fun result -> printfn "lglk: %f" result.posterior.mle.lglk);

  // Run the multiple CRNs test
  //InferenceTests.degradation_multiple_example () |> ignore;

  // Run the AM test
  (*let am = InferenceTests.am()
  let i = am |> Crn.to_inference
  i |> Inference.run_mcmc_seq |> Seq.toArray |> ignore;
  InferenceTests.infer_am () |> ignore;*)

  //let crn = model_lag_parse () in
  printfn "Done";
  0