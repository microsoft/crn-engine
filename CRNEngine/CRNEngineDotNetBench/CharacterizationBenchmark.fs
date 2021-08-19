namespace CRNEngineDotNetBench

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Jobs
open Microsoft.Research
open Microsoft.Research.CRNEngine
open Microsoft.Research.CliLibrary

type CharacterizationBenchmark() = 

    let program = """directive inference { name=target; burnin=500; samples=0; thin=10; seed=1; noise_model=proportional; partial=false; timer=false }
directive sweeps [
	sweep_proc141021_R33S175_Y81C76 = [ (r,K,tlag,rc,C6,C12) = [(1.08,2.835,0.2449,58.66,2.5e+04,0); (1.164,2.72,1.251,70.19,8333,0); (1.197,2.635,1.418,74.85,2778,0); (1.046,2.504,0.7702,74.87,925.9,0); (1.241,2.478,1.603,82.46,308.6,0); (1.356,2.443,1.859,88.82,102.9,0); (1.431,2.414,1.775,90.61,34.29,0); (1.5,2.355,2.114,94.22,11.43,0); (1.48,2.322,2.016,94.15,3.81,0); (1.484,2.39,1.709,88.24,1.27,0); (1.126,2.391,1.968e-05,73.53,0.4234,0); (1.126,2.37,1.86e-05,76.18,0,0); (1.095,2.473,0.911,56.81,0,2.5e+04); (1.094,2.413,0.7769,57.75,0,8333); (1.241,2.333,1.848,65.77,0,2778); (1.197,2.27,1.745,69.75,0,925.9); (1.374,2.258,2.254,78.93,0,308.6); (1.172,2.264,0.6931,70.42,0,102.9); (1.361,2.199,1.651,82.36,0,34.29); (1.5,2.25,1.923,86.66,0,11.43); (1.169,2.133,0.4027,76.82,0,3.81); (1.161,2.234,0.0002391,72.26,0,1.27); (1.098,2.192,0.0009652,73.48,0,0.4234); (1.091,2.219,1.468e-05,75.46,0,0)]; c0 = [0.002]; rs = [16.23]; Kc = [0.8927]; nc = [21.56]; f0 = [77.37]; aS = [aS175]; aR = [aR33] ]
]
directive simulation { final=20.0; points=1000; plots=[[x]*[yfp]+yb0; [x]*[cfp]+cb0] }
directive simulator sundials
directive parameters 
[ a0_76=0.086, { interval=Real; distribution=Uniform(0.0,10.0); variation=Random }
; a0_81=0.264, { interval=Real; distribution=Uniform(0.0,10.0); variation=Random }
; a1R=18.47,   { interval=Real; distribution=Uniform(0.0,1000.0); variation=Random }
; a1S=8.24,    { interval=Real; distribution=Uniform(0.0,1000.0); variation=Random }
; KGR_76=8.657e-2, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; KGS_76=4.788e-4, { interval=Log; distribution=Uniform(1e-5,1e-2); variation=Random }
; KGR_81=3.329e-3, { interval=Log; distribution=Uniform(1e-5,1e-2); variation=Random }
; KGS_81=4.249e-1, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; KR6=2.076e-4,  { interval=Log; distribution=Uniform(1e-4,1e0); variation=Random }
; KS6=1.71e-8,   { interval=Log; distribution=Uniform(1e-8,1e0); variation=Random }
; KR12=4.937e-7, { interval=Log; distribution=Uniform(1e-8,1e0); variation=Random }
; KS12=8.827e-3, { interval=Log; distribution=Uniform(1e-4,1e0); variation=Random }
; n=0.797,  { interval=Real; distribution=Uniform(0.5,2.0); variation=Random }
; aRS100=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; aR33=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; aS32=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; aS175=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; rho=1e0, 	{ interval=Log; distribution=Uniform(1e-3,1e3); variation=Random }
; dR=0.1,   { interval=Log; distribution=Uniform(1e-3,1e1); variation=Random }
; dS=0.1,   { interval=Log; distribution=Uniform(1e-3,1e1); variation=Random }
; dyfp=0.0//, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; dcfp=0.0//, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; dC=0.1,   { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; yb0=1e3,  { interval=Real; distribution=Uniform(0.0,5e3); variation=Random }
; cb0=1e3,  { interval=Real; distribution=Uniform(0.0,1e4); variation=Random }
; C6 = 0.0; C12 = 0.0; c0 = 0.001; r = 1.0; K = 2.0; rc = 1000.0; Kc=0.9; nc=10.0; rs=1.0; tlag=0.0; autoY=0.1; autoC=3.31; aR=1.0; aS=1.0 ]

init x c0 | init c12 C12 | init c6 C6  | init grow 1 @ tlag |

// Cell growth
->[[grow]*r*(1 - [x] / K)*[x]] x |
yfp ->[[grow]*[yfp]*r*(1 - [x] / K)] |
cfp ->[[grow]*[cfp]*r*(1 - [x] / K)] |
luxR ->[[grow]*[luxR]*r*(1 - [x] / K)] |
lasR ->[[grow]*[lasR]*r*(1 - [x] / K)] |

// "Total" LuxR and LasR (constitutive)
->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aR] luxR | luxR ->{dR} |
->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aS] lasR | lasR ->{dS} |

// CFP (P76)
->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*rho*(a0_76 + a1R*KGR_76*[luxR]^2*(((KR6^n)*([c6]^n) + (KR12^n)*([c12]^n))/((1.0 + KR6*[c6] + KR12*[c12])^n)) + a1S*KGS_76*[lasR]^2*(((KS6^n)*([c6]^n) + (KS12^n)*([c12]^n)) / ((1.0 + KS6*[c6] + KS12*[c12])^n)))/(1.0 + KGR_76*[luxR]^2*(((KR6^n)*([c6]^n) + (KR12^n)*([c12]^n))/((1.0 + KR6*[c6] + KR12*[c12])^n)) + KGS_76*[lasR]^2*(((KS6^n)*([c6]^n) + (KS12^n)*([c12]^n))/((1.0 + KS6*[c6] + KS12*[c12])^n)))] cfp |

// HSL degradation
c12 ->{dC} | c6 ->{dC} |

// YFP 
->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*(a0_81 + a1R*KGR_81*[luxR]^2*(((KR6^n)*([c6]^n) + (KR12^n)*([c12]^n))/((1.0 + KR6*[c6] + KR12*[c12])^n)) + a1S*KGS_81*[lasR]^2*(((KS6^n)*([c6]^n) + (KS12^n)*([c12]^n)) / ((1.0 + KS6*[c6] + KS12*[c12])^n)))/(1.0 + KGR_81*[luxR]^2*(((KR6^n)*([c6]^n) + (KR12^n)*([c12]^n))/((1.0 + KR6*[c6] + KR12*[c12])^n)) + KGS_81*[lasR]^2*(((KS6^n)*([c6]^n) + (KS12^n)*([c12]^n))/((1.0 + KS6*[c6] + KS12*[c12])^n)))] yfp | 

// FP Degradation
cfp ->{dcfp} |
yfp ->{dyfp} |

// Autofluorescence
->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*autoC] cfp |
->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*autoY] yfp"""

    let program_rates = """directive inference { name=target; burnin=500; samples=0; thin=10; seed=1; noise_model=proportional; partial=false; timer=false }
directive sweeps [
	sweep_proc141021_R33S175_Y81C76 = [ (r,K,tlag,rc,C6,C12) = [(1.08,2.835,0.2449,58.66,2.5e+04,0); (1.164,2.72,1.251,70.19,8333,0); (1.197,2.635,1.418,74.85,2778,0); (1.046,2.504,0.7702,74.87,925.9,0); (1.241,2.478,1.603,82.46,308.6,0); (1.356,2.443,1.859,88.82,102.9,0); (1.431,2.414,1.775,90.61,34.29,0); (1.5,2.355,2.114,94.22,11.43,0); (1.48,2.322,2.016,94.15,3.81,0); (1.484,2.39,1.709,88.24,1.27,0); (1.126,2.391,1.968e-05,73.53,0.4234,0); (1.126,2.37,1.86e-05,76.18,0,0); (1.095,2.473,0.911,56.81,0,2.5e+04); (1.094,2.413,0.7769,57.75,0,8333); (1.241,2.333,1.848,65.77,0,2778); (1.197,2.27,1.745,69.75,0,925.9); (1.374,2.258,2.254,78.93,0,308.6); (1.172,2.264,0.6931,70.42,0,102.9); (1.361,2.199,1.651,82.36,0,34.29); (1.5,2.25,1.923,86.66,0,11.43); (1.169,2.133,0.4027,76.82,0,3.81); (1.161,2.234,0.0002391,72.26,0,1.27); (1.098,2.192,0.0009652,73.48,0,0.4234); (1.091,2.219,1.468e-05,75.46,0,0)]; c0 = [0.002]; rs = [16.23]; Kc = [0.8927]; nc = [21.56]; f0 = [77.37]; aS = [aS175]; aR = [aR33] ]
]
directive simulation { final=20.0; points=1000; plots=[[x]*[yfp]+yb0; [x]*[cfp]+cb0] }
directive simulator sundials
directive parameters 
[ a0_76=0.086, { interval=Real; distribution=Uniform(0.0,10.0); variation=Random }
; a0_81=0.264, { interval=Real; distribution=Uniform(0.0,10.0); variation=Random }
; a1R=18.47,   { interval=Real; distribution=Uniform(0.0,1000.0); variation=Random }
; a1S=8.24,    { interval=Real; distribution=Uniform(0.0,1000.0); variation=Random }
; KGR_76=8.657e-2, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; KGS_76=4.788e-4, { interval=Log; distribution=Uniform(1e-5,1e-2); variation=Random }
; KGR_81=3.329e-3, { interval=Log; distribution=Uniform(1e-5,1e-2); variation=Random }
; KGS_81=4.249e-1, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; KR6=2.076e-4,  { interval=Log; distribution=Uniform(1e-4,1e0); variation=Random }
; KS6=1.71e-8,   { interval=Log; distribution=Uniform(1e-8,1e0); variation=Random }
; KR12=4.937e-7, { interval=Log; distribution=Uniform(1e-8,1e0); variation=Random }
; KS12=8.827e-3, { interval=Log; distribution=Uniform(1e-4,1e0); variation=Random }
; n=0.797,  { interval=Real; distribution=Uniform(0.5,2.0); variation=Random }
; aRS100=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; aR33=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; aS32=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; aS175=1.0, 	{ interval=Log; distribution=Uniform(1e-6,1e3); variation=Random }
; rho=1e0, 	{ interval=Log; distribution=Uniform(1e-3,1e3); variation=Random }
; dR=0.1,   { interval=Log; distribution=Uniform(1e-3,1e1); variation=Random }
; dS=0.1,   { interval=Log; distribution=Uniform(1e-3,1e1); variation=Random }
; dyfp=0.0//, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; dcfp=0.0//, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; dC=0.1,   { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; yb0=1e3,  { interval=Real; distribution=Uniform(0.0,5e3); variation=Random }
; cb0=1e3,  { interval=Real; distribution=Uniform(0.0,1e4); variation=Random }
; C6 = 0.0; C12 = 0.0; c0 = 0.001; r = 1.0; K = 2.0; rc = 1000.0; Kc=0.9; nc=10.0; rs=1.0; tlag=0.0; autoY=0.1; autoC=3.31; aR=1.0; aS=1.0 ]

directive rates [
    growth =  [grow]*r*(1 - [x] / K);
	capacity = (rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc);
	boundLuxR = [luxR]^2 * ((KR6*[c6])^n + (KR12*[c12])^n) / ((1.0 + KR6*[c6] + KR12*[c12])^n);
	boundLasR = [lasR]^2 * ((KS6*[c6])^n + (KS12*[c12])^n) / ((1.0 + KS6*[c6] + KS12*[c12])^n);
	P76 = (a0_76 + a1R*KGR_76*[boundLuxR] + a1S*KGS_76*[boundLasR]) / (1.0 + KGR_76*[boundLuxR] + KGS_76*[boundLasR]);
	P81 = (a0_81 + a1R*KGR_81*[boundLuxR] + a1S*KGS_81*[boundLasR]) / (1.0 + KGR_81*[boundLuxR] + KGS_81*[boundLasR])
  ]
init x c0 | init c12 C12 | init c6 C6  | init grow 1 @ tlag |

// Cell growth
     ->[[growth]*[x]] x |
yfp  ->[[growth]*[yfp]] |
cfp  ->[[growth]*[cfp]] |
luxR ->[[growth]*[luxR]] |
lasR ->[[growth]*[lasR]] |

// "Total" LuxR and LasR (constitutive)
->[[capacity]*aR] luxR | luxR ->{dR} |
->[[capacity]*aS] lasR | lasR ->{dS} |

// YFP (P81)
->[[capacity]*[P81]] yfp | 

// CFP (P76)
->[[capacity]*rho*[P76]] cfp |

// HSL degradation
c12 ->{dC} | c6 ->{dC} |

// FP Degradation
yfp ->{dyfp} |
cfp ->{dcfp} |

// Autofluorescence
->[[capacity]*autoY] yfp |
->[[capacity]*autoC] cfp"""



    //let parsed = program |> Parser.from_string Crn.parse
    let parsed =
        let possiblyParsed, errors = program_rates |> Parser.from_string_find_errors Crn.parse    // Program with directive rates
        match possiblyParsed with
        | Some parsed -> parsed
        | None ->
            let errorsString = errors |> Seq.map (fun error -> sprintf "Line %i, column %i: %s" error.row error.column error.text) |> (String.concat System.Environment.NewLine)
            failwithf "Failed to parse: %s%s" System.Environment.NewLine errorsString
    let num_cols = parsed.settings.simulation.plots.Length
    let crn = 
      let data = 
        //["Pcat_Y81C76_EYFP_ECFP_proc141006.txt"; "RS100S32_Y81C76_EYFP_ECFP_proc140916.txt"; "R33S32_Y81C76_EYFP_ECFP_proc140916.txt"; "R33S175_Y81C76_EYFP_ECFP_proc141021.txt"]
        ["R33S175_Y81C76_EYFP_ECFP_proc141021.txt"]
        |> List.map (fun s -> 
          let data = s |> System.IO.File.ReadAllText |> Table<float>.parse_multiple_tsv num_cols
          Dataset.create "AM_obs_noised" data
        )

      { parsed with settings = { parsed.settings with data = data; inference = {parsed.settings.inference with burnin=2500; samples=2500} } }
    
    [<Benchmark>]
    member this.Run() = crn.infer()