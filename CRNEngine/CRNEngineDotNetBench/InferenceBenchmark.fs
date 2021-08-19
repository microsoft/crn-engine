namespace CRNEngineDotNetBench

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Jobs

open Microsoft.Research
open Microsoft.Research.CRNEngine

type InferenceBenchmark() =

    let make_crn_with_fixed_data program = 
      let crn = 
        program
        |> Parser.from_string Crn.parse

      //TODO: Let user specify their data
      let datastring = System.IO.File.ReadAllText "AM_obs_noised.csv"
      let datasingle = datastring |> Table<float>.parse_csv
      let data = Dataset.create "AM_obs_noised" [datasingle]

      { crn with settings = { crn.settings with data = [data] } }

    let fixedProgram = """directive inference  { burnin = 100; samples = 1000; thin = 100 }
    directive simulation { final=1.0; points=1000; plots=[B; Y; X] }
    directive simulator deterministic
    directive deterministic { reltolerance=1e-5 }
    directive parameters [r = 0.2,  { interval=Log; distribution=Uniform(1e-2,1e2); variation=Random }]

    directive sweeps [dummySweep = [NotUsed = [0.0]]]
    directive data [AM_obs_noised]

    X + Y ->{r} X + B |
    Y + X ->{r} Y + B |
    X + B ->{r} X + X |
    Y + B ->{r} Y + Y |

    init X 30 |
    init Y 20 |
    init B 0"""

    let rawCRN = make_crn_with_fixed_data fixedProgram  
    let osloCRN = rawCRN.update_settings {rawCRN.settings with inference = {rawCRN.settings.inference with print_console = false; print_summary = false } }
    let oslo10000 = osloCRN.update_settings {osloCRN.settings with inference = {osloCRN.settings.inference with samples=10000}}
    let sundialsCRN = osloCRN.update_settings { osloCRN.settings with simulator = Sundials }
    let sundials10000 = oslo10000.update_settings { oslo10000.settings with simulator = Sundials }

    [<Benchmark>]
    member this.Oslo1000() =

        osloCRN.infer()

    [<Benchmark>]
    member this.Oslo10000() =

        oslo10000.infer()

    [<Benchmark>]
    member this.Sundials1000() =

        sundialsCRN.infer()

    [<Benchmark>]
    member this.Sundials10000() =

        sundials10000.infer()