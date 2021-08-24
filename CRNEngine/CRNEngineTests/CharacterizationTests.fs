// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.CharacterizationTests

module FBParameters = Microsoft.Research.Filzbach.Parameters

open Xunit
open FsUnit
open Microsoft.Research.CRNEngine

type crn = Crn

let control () : Crn = 
  let prog_text = "
    directive inference { burnin = 1000; samples = 10000; thin=20; partial = true; seed = 0 }
    directive simulator sundials
    directive parameters [ 
      rs=1e3, { interval=Log; distribution=Uniform(1e0,1e4); variation=Random };
      rc=1e4, { interval=Log; distribution=Uniform(1e3,1e4); variation=Multiple };
      Kc=0.9;
      nc=40.0;
      f0=5000.0,{ interval=Real; distribution=Uniform(0.0,10000.0); variation=Random };
      c0 = 0.001; r = 1.0; K = 2.0; C6=0.0;
    ]
    
    directive sweeps [
      sweep_130607_Pcat_R123 = [ 
        (r,K,c0,C6) = [
          (0.7102,1.879,0.006692,1e+06); 
          (0.5727,2.099,0.02208,3.333e+05); 
          (0.5976,2.22,0.01858,1.111e+05); 
          (0.5766,2.068,0.01932,3.704e+04); 
          (0.6025,2.089,0.01442,1.235e+04); 
          (0.6588,2.146,0.01193,4115); 
          (0.6642,1.907,0.01219,1372); 
          (0.801,1.902,0.003548,457.2); 
          (0.9116,1.815,0.001758,152.4); 
          (1.046,1.687,0.0005095,50.81); 
          (1.117,1.737,0.0003199,16.94); 
          (1.161,1.427,0.0001771,0);
        ]
        //C6 = [1e+06; 3.333e+05; 1.111e+05; 3.704e+04; 1.235e+04; 4115; 1372; 457.2; 152.4; 50.81; 16.94; 0];
      ]
    ]
    directive simulation { final=20.0; points=1000; plots=[[x]*[fp]+f0]; multicore=true }

    init x c0 |
    ->[r*(1 - [x] / K)*[x]] x |
    fp ->[[fp]*r*(1 - [x] / K)] |
    ifp ->[[ifp]*r*(1 - [x] / K)] |

    ->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)] ifp |
    ifp ->{1.0} fp
    "

  Parser.from_string Crn.parse prog_text 

(*TODO
let simulation_test () = 
    let crn = control
    let sim_data = Crn.simulate_sundials_single crn
    Microsoft.Research.CliLibrary.Io.string_of_table_unordered "characterization_simtest.tsv" sim_data
   
[<Fact>]
let ``Simulation Test``() = 
    simulation_test ()
*)

let inference_test nb ns multicore partial : Crn =
    let data : Dataset list = [{file="Pcat_R123_cfp_130607"; data="Pcat_R123_cfp_130607.txt" |> Io.load_file |> Table<float>.parse_multiple_tsv 1}] in
    let crn = control ()
    let inference_settings = { crn.settings.inference with burnin=nb; samples=ns; partial_evaluation=partial }
    let sim_settings = {crn.settings.simulation with multicore=multicore}
    { crn with settings = {crn.settings with data=data; inference=inference_settings; simulation=sim_settings} }

[<Fact(DisplayName="Characterization - Likelihood robust to multicore")>]
let like_char_multicore () =
  let ode_serial = (inference_test 100 100 false true).to_ode()
  let inf_serial = Ode.list_to_inference ode_serial.settings [ode_serial]
  let ode_parallel = (inference_test 100 100 true true).to_ode()
  let inf_parallel = Ode.list_to_inference ode_parallel.settings [ode_parallel]
  
  let inf_combined = 
    match (inf_serial,inf_parallel) with 
    | (Inference.Full ser, Inference.Full par) -> 
      let likefn p = 
        let lser = ser.likefn p
        let lpar = par.likefn p
        Assert.Equal(lser,lpar)
        lser
      Inference.Full { ser with likefn = likefn }
    | (Inference.Partial ser, Inference.Partial par) -> 
      let likefn p subset = 
        let lser = ser.likefn p subset 
        let lpar = par.likefn p subset 
        (lser,lpar) ||> List.zip |> List.iter Assert.Equal
        lser
      Inference.Partial { ser with likefn = likefn }
    | _ -> failwith "Ignore for now"
  
  let _ = Inference.run_mcmc inf_combined
  ()

// ND: I don't understand why, but this test always seems to fail the first time. Then, when you try again, it works!
[<Fact(DisplayName="Characterization - Likelihood robust to partial")>]
let like_char_partial () =
  let ode_full = (inference_test 100 100 false false).to_ode()
  let inf_full = Ode.list_to_inference ode_full.settings [ode_full]
  let res_full = Inference.run_mcmc inf_full
  let ode_partial = (inference_test 100 100 false true).to_ode()
  let inf_partial = Ode.list_to_inference ode_partial.settings [ode_partial]
  let res_partial = Inference.run_mcmc inf_partial

  (res_full.posterior, res_partial.posterior)
  ||> List.map2 (fun f p ->
    let iteration = int f.["Iteration"]
    let vf = f.["logLikelihood"]
    let vp = p.["logLikelihood"]
    let text = sprintf "Iteration %d: Full = %f, Partial = %f" iteration vf vp
    System.Diagnostics.Trace.WriteLine text
    Assert.Equal(vf, vp)
  ) 
 
// ND: I don't understand why, but this test always seems to fail the first time. Then, when you try again, it works!
[<Trait("Category", "Slow")>]
[<Fact(DisplayName="(slow) Characterization - Inference")>]
let inference_characterization () =
    //Lib.check64bit()
    let ode = (inference_test 1500 500 true true).to_ode() 
    let inf = Ode.list_to_inference ode.settings [ode]
    let expected_dependencies = List.init 5 (fun i -> FBParameters.All) @ List.init 12 (fun i -> FBParameters.Subset [i])
    match inf with
    | Inference.Partial inf_partial ->  
        List.iter2  (fun exp obs -> 
            match (exp,obs) with
            | (FBParameters.All, FBParameters.All) -> ()
            | (FBParameters.Subset eids, FBParameters.Subset oids) -> (eids,oids) ||> List.zip |> List.iter Assert.Equal 
            | _ -> failwith "Cannot reconcile All with Subset"
        ) expected_dependencies inf_partial.dependencies
    | Inference.Full _ -> () //failwith "Shouldn't be a Full inference. Should be Partial in this test."
    
    let result = Inference.run_mcmc inf   // If the inference is working, the log-likelihood score should eventually reach around -10600
    let mle = result.mle.["logLikelihood"]
    Assert.InRange(mle,-11000.0,-1000.0)