namespace CRNEngineDotNetBench

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Running
open BenchmarkDotNet.Diagnosers
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Toolchains.InProcess;
open BenchmarkDotNet.Environments
open BenchmarkDotNet.Toolchains.CsProj
open BenchmarkDotNet.Exporters

open Microsoft.Research
open Microsoft.Research.CRNEngine
open System.Runtime.Versioning
open BenchmarkDotNet.Environments

[<SimpleJob(RuntimeMoniker.NetCoreApp31)>]
[<RPlotExporter>]
type SimulationBenchmark() =

    [<Benchmark>]
    member this.Oslo() =

        let program = "directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]
directive simulator deterministic
directive deterministic { reltolerance=1e-5 }

X + Y ->{r} X + B |
Y + X ->{r} Y + B |
X + B ->{r} X + X |
Y + B ->{r} Y + Y |

init X 30 |
init Y 20"
        let ig = JSAPI.parse_code program
        let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
        model.top.to_oslo().simulate()

    [<Benchmark>]
    member this.Sundials() =

        let program = "directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]
directive simulator sundials
directive deterministic { reltolerance=1e-5 }

X + Y ->{r} X + B |
Y + X ->{r} Y + B |
X + B ->{r} X + X |
Y + B ->{r} Y + Y |

init X 30 |
init Y 20"

        let ig = JSAPI.parse_code program
        let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
        model.top.to_sundials().simulate()

    [<Benchmark>]
    member this.OsloFunctional() =

        let program = "directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]

X + Y ->[r * [X] * [Y]] X + B |
Y + X ->[r * [X] * [Y]] Y + B |
X + B ->[r * [X] * [B]] X + X |
Y + B ->[r * [B] * [Y]] Y + Y |

init X 30 |
init Y 20"

        let ig = JSAPI.parse_code program
        let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
        model.top.to_oslo().simulate()
      
    [<Benchmark>]
    member this.SundialsFunctional() =

        let program = "directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]

X + Y ->[r * [X] * [Y]] X + B |
Y + X ->[r * [X] * [Y]] Y + B |
X + B ->[r * [X] * [B]] X + X |
Y + B ->[r * [B] * [Y]] Y + Y |

init X 30 |
init Y 20"

        let ig = JSAPI.parse_code program
        let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
        model.top.to_sundials().simulate()

    [<Benchmark>]
    member this.OsloWaves() =

        let program = "directive simulation {final=20000; plots=[SpeciesL; SpeciesR]; }
directive simulator deterministic
directive deterministic {reltolerance=1e-7}
 
| constant 1000 UnaryRx
| constant 1000 UnaryLxLL
| constant 100000 UnaryLxLL_1
| 1000 SpeciesR
| 1000 SpeciesL
| constant 30000 BinaryLRxRR
| constant 3000000 BinaryLRxRR_1
| UnaryLxLL + SpeciesL ->{0.00001} sp10 + sp9
| UnaryRx + SpeciesR ->{0.00001} sp8 + sp7
| BinaryLRxRR + SpeciesL <->{0.00001}{0.1} sp12
| sp12 + SpeciesR ->{0.00001} sp6 + sp5
| BinaryLRxRR + SpeciesR <->{0.00001}{0.1} sp11
| sp11 + SpeciesL ->{0.00001} sp6 + sp5
| UnaryLxLL_1 + sp9 ->{0.00001} sp4 + sp3 + 2SpeciesL
| BinaryLRxRR_1 + sp5 ->{0.00001} sp2 + sp1 + 2SpeciesR"

        let ig = JSAPI.parse_code program
        let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
        model.top.to_oslo().simulate()

    [<Benchmark>]
    member this.SundialsWaves() =

        let program = "directive simulation {final=20000; plots=[SpeciesL; SpeciesR]; }
directive simulator sundials
directive deterministic {reltolerance=1e-7}
 
| constant 1000 UnaryRx
| constant 1000 UnaryLxLL
| constant 100000 UnaryLxLL_1
| 1000 SpeciesR
| 1000 SpeciesL
| constant 30000 BinaryLRxRR
| constant 3000000 BinaryLRxRR_1
| UnaryLxLL + SpeciesL ->{0.00001} sp10 + sp9
| UnaryRx + SpeciesR ->{0.00001} sp8 + sp7
| BinaryLRxRR + SpeciesL <->{0.00001}{0.1} sp12
| sp12 + SpeciesR ->{0.00001} sp6 + sp5
| BinaryLRxRR + SpeciesR <->{0.00001}{0.1} sp11
| sp11 + SpeciesL ->{0.00001} sp6 + sp5
| UnaryLxLL_1 + sp9 ->{0.00001} sp4 + sp3 + 2SpeciesL
| BinaryLRxRR_1 + sp5 ->{0.00001} sp2 + sp1 + 2SpeciesR"

        let ig = JSAPI.parse_code program
        let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
        model.top.to_sundials().simulate()