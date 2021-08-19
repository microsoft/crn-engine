open Expecto

open Microsoft.Research.CRNEngine
open System

type crn = Crn

let control () : Crn = 
  let prog_text = "
    directive inference { burnin = 1000; samples = 10000; thin=50; partial = true }
    directive simulator deterministic
    directive parameters [ 
      rs=1e3, { interval=Log; distribution=Uniform(1e0,1e4); variation=Random };
      rc=1e4, { interval=Log; distribution=Uniform(1e3,1e4); variation=Multiple };
      Kc=0.9;
      nc=40.0;
      f0=5000.0,{ interval=Real; distribution=Uniform(0.0,10000.0); variation=Random };
      c0 = 0.001; r = 1.0; K = 2.0; 
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

let inference_test nb ns multicore partial : Crn =
    let data : Dataset list = [{file="Pcat_R123_cfp_130607"; data="Pcat_R123_cfp_130607.txt" |> Io.load_file |> Table<float>.parse_multiple_tsv 1}] in
    let crn = control ()
    let inference_settings = { crn.settings.inference with burnin=nb; samples=ns; partial_evaluation=partial; print_console = false; print_summary=false}
    let sim_settings = {crn.settings.simulation with multicore=multicore}
    { crn with settings = {crn.settings with data=data; inference=inference_settings; simulation=sim_settings} }

let tests =
    testList "Stuff" [
        test "Other test" {

              let ode_full = (inference_test 200 200 false false).to_ode()
              let inf_full = Ode.list_to_inference ode_full.settings [ode_full]
              let res_full = Inference.run_mcmc inf_full
              let ode_partial = (inference_test 200 200 false true).to_ode()
              let inf_partial = Ode.list_to_inference ode_partial.settings [ode_partial]
              let res_partial = Inference.run_mcmc inf_partial

              (res_full.posterior, res_partial.posterior)
              ||> List.map2 (fun f p ->
                Expect.equal(f.["logLikelihood"], p.["logLikelihood"])
              ) |> ignore
        }

     ]

[<EntryPoint>]
let main args =
    let config =
        { defaultConfig with
            ``parallel`` = true
            stress = Some (TimeSpan.FromHours 0.2)
            }
    runTestsWithArgs config args tests