// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.SimulationTests

open Xunit
open FsUnit
open FsUnit.Xunit
open MathNet.Numerics.Distributions
open Microsoft.Research.CRNEngine

let binding_event_example () = 
    "init X 10 @ 1.0 |
    init T 10 |
    X + T ->{1.0} XT"
    |> Parser.from_string Crn.parse

[<Fact(DisplayName="Simulation - Event binding")>]
let event_binding () = 
    let crn = binding_event_example ()
    crn.to_ssa().simulate() |> ignore;
    crn.to_oslo().simulate() |> ignore;
    crn.to_sundials().simulate() |> ignore;
    ()

let initial_past_example () = 
  "directive simulation { initial=-3.0; final=-2.0; plots=[Y] }
  init X 1 |
  X ->{1.0} Y"
  |> Parser.from_string Crn.parse

[<Fact(DisplayName="Simulation - Initials before t=0")>]
let initials_past () = 
    let crn = initial_past_example ()
    crn.initials 
    |> List.iter (fun i -> 
        Assert.Equal(None, i.time)
    )
    let table = crn.to_oslo().simulate() 
    let final_point = List.last table.columns.Head.values
    Assert.Equal(1.0 - exp (-1.0), final_point, 2)

[<Fact(DisplayName="Simulation - Plot rate expressions")>]
let plot_rates () = 
  let crn = 
    "directive simulation { final=1.0; points=100; plots=[[low]]}
directive rates [ low = [X]+[Y] ]
init X 1 | init Y 1 |
X + Y ->{0.1} Z" 
    |> Crn.from_string 
  crn.to_oslo().simulate() |> ignore;
  ()

let gompertz () = 
  "init X 1e-3 |
  ->[log(1.0/[X])*[X]] X"
  |> Crn.from_string

[<Fact(DisplayName="Simulation - Gompertz model (log)")>]
let gompertz_model_log () = 
  let crn = gompertz ()
  let oslo = crn.to_oslo().simulate()
  let sundials = crn.to_sundials().simulate()
  ()

let last_point table = table.columns.Head.values |> List.last

let laglogistic () =
  "directive simulation { final=20.0; points=250 }
  directive parameters [ rc=1e4; c0=0.002; r=1.0; K=2.0; mat=1.0; tlag=-1.0]

  init x c0 |
  init grow 1 @ tlag |

  // Cell growth
  ->[[grow]*r*(1 - [x] / K)*[x]] x"
  |> Crn.from_string

[<Fact(DisplayName="Simulation - Lag-logistic negative time")>]
let lag_logistic_negative_time () =
  let crn = laglogistic ()
  let final = crn.to_sundials().simulate() |> last_point
  Assert.InRange(final, 1.9, 2.1)

let statistics dist = 
  let mean = dist |> Array.mapi (fun i x -> (float i) * x) |> Array.sum
  let Ex2  = dist |> Array.mapi (fun i x -> (float i)**2.0 * x) |> Array.sum
  let variance = Ex2 - mean**2.0
  mean, variance

  
[<Fact(DisplayName="Simulation - Kinetics stochastic")>]
let compare_kinetics_stochastic () = 
  let crn = Crn.from_string "directive simulation { final=0.02; kinetics=Stochastic; plots=[A] }
  init A 200 | A + A -> B"
  let res_ode = crn.to_oslo().simulate() |> last_point
  let res_cme = crn.simulate_cme() |> last_point
  let res_lna = crn.to_lna().simulate() |> last_point
  // Add SSA here too?
  Assert.Equal (1.0, res_cme.mean / res_ode, 1)
  Assert.Equal (1.0, res_lna.mean / res_ode, 1)

[<Fact(DisplayName="Simulation - Kinetics deterministic")>]
let compare_kinetics_deterministic () = 
  let crn = Crn.from_string "directive simulation { final=0.02; kinetics=Deterministic; plots=[A] }
  init A 200 | A + A -> B"
  let res_ode = crn.to_oslo().simulate() |> last_point
  let res_cme = crn.simulate_cme() |> last_point
  let res_lna = crn.to_lna().simulate() |> last_point
  // Add SSA here too?
  Assert.Equal (1.0, res_cme.mean / res_ode, 1)
  Assert.Equal (1.0, res_lna.mean / res_ode, 1)

(* Modified tests that use Population maxima instead of Species maxima *)
let set_maxima (crn:Crn) (populations:Populations<Species,'v>) (maxima:Map<string,int>) = 
  crn.settings.simulation.plots 
  |> List.iter (fun x -> 
      let sp = match x with Expression.Key (Key.Species k) -> k | _ -> failwith "Can't handle arbitrary expressions"
      populations.set_max  populations.species_to_index.[sp] (Map.tryFind sp.name maxima)
  )

[<Fact(DisplayName="Simulation - Multicore preserves behaviour of ODE simulations")>]
let robust_multicore () = 
  let crn = CharacterizationTests.control()
  let env = Parameters.to_env crn.settings.parameters
  let simulate (ode:Ode) = ode.to_sundials().simulate()
  let instances, odes = Ode.list_to_instances [crn.to_ode().substitute env]
  let table_serial = Ode.list_simulate_instances false simulate instances odes
  let table_parallel = Ode.list_simulate_instances true simulate instances odes
  List.iter2 (fun tab_serial tab_parallel -> Assert.Equal(tab_serial.table,tab_parallel.table)) table_serial table_parallel
  
[<Fact(DisplayName="SSA multiple - Callback method")>]
let ssa_multi_callback () = 
  let crn = Crn.from_string """
  directive simulation {final=1.0; points=100}
  directive simulator stochastic
  directive stochastic { trajectories=10 }

  init A 10 |
  init B 8 |
  A + B -> I + A |
  B + A -> I + B |
  A + I -> A + A |
  B + I -> B + B
  """

  let result = crn.to_ssa().simulate_trajectories()
  Assert.Equal (10.0, result.columns |> List.find (fun c -> c.name = "A") |> fun c -> c.values.Head.mean)
  Assert.Equal (8.0, result.columns |> List.find (fun c -> c.name = "B") |> fun c -> c.values.Head.mean)
  Assert.Equal (0.0, result.columns |> List.find (fun c -> c.name = "I") |> fun c -> c.values.Head.mean)
  
[<Fact(DisplayName="SSA multiple - Simple example")>]
let ssa_multi () = 
  let crn = Crn.from_string """
  directive simulation {final=2.0; points=1000; multicore=true}
  directive simulator stochastic
  directive stochastic { trajectories=10 }

  init A 10 |
  init B 8 |
  A + B -> I + A |
  B + A -> I + B |
  A + I -> A + A |
  B + I -> B + B
  """

  let result = (crn.simulate_case ()).Head.table
  Assert.Equal (30, result.columns.Length) // Number of plot species * number of trajectories
  Assert.Equal (1000, result.times.Length) // Number of points
  let initial_values sp = 
      result.columns |> List.filter (fun col -> col.name = sp) |> List.map (fun col -> col.values.[0])
  initial_values "A" |> List.exists (fun x -> x<>10.0) |> Assert.False
  initial_values "B" |> List.exists (fun x -> x<>8.0) |> Assert.False
  initial_values "I" |> List.exists (fun x -> x<>0.0) |> Assert.False

[<Fact(DisplayName="SSA stationary - Birth-death simple")>]
let ssa_stationary_birth_death_simple () = 
  let crn = 
    "
    directive simulation { final=500000.0; plots=[X] }
    directive stochastic { stationary_skiptime=2000.0 }
    <->{1.0}{0.02} X
    " |> Crn.from_string
  let maxima = ["X",100] |> Map.ofList
  let ssa = crn.to_ssa()
  set_maxima crn ssa.simulator.populations maxima
  let _, _, stationary = Ssa.simulate_with_stationary ssa
  printfn "%A" stationary.["X"]
  
  // Assert that the mean and variance are as pre-computed
  let emean = 50.0
  let evariance = 50.0
  let mean, variance = statistics stationary.["X"]
  Assert.InRange(mean, emean-5.0, emean+5.0)
  Assert.InRange(variance, evariance-5.0, evariance+5.0)

let kolmogorov_smirnov confidence_level pdf1 pdf2 = 
  //this assumes the pdfs have the same number of samples
  //let cdf_stationary = Array.scan (+) 0.0 pdf1
  //let cdf_poisson = Array.scan (+) 0.0 pdf2
  let diff = (pdf1, pdf2) ||> Array.map2 (fun s p -> abs(s - p)) |> Array.max
  let n, n' = (float pdf1.Length), (float pdf2.Length)
  let confidence level = 
    let ca = sqrt (-0.5 * (log (level/2.0)))
    ca * (sqrt (( n + n') / (n*n')))
  let thresh = confidence confidence_level
  diff, thresh, diff < thresh // true if they are the same within confidence level
  

[<Fact(DisplayName="SSA stationary - Birth/death distribution check")>]
let ssa_stationary_birth_death_distribution_check () = 
  let test_crn frwd back =
      sprintf 
          "directive simulation { final=100000.0; plots=[X] }
          directive stochastic { stationary_skiptime=1000.0 }
          directive parameters [ fr = %f; bk = %f ]
          |   ->{fr} X 
          | X ->{bk}  " frwd back 
  let forward = 2.0
  let back = 0.1
  let predicted_mean = forward/back 

  let dist = MathNet.Numerics.Distributions.Poisson(predicted_mean)
  let maxX = (int predicted_mean) * 3
  let Xvals = Array.init (maxX + 1) id
  let poissPDF = Xvals |> Array.map (dist.Probability) 

  let parsed = (test_crn forward back) |> Crn.from_string
  let maxima = Map.ofList [("X",maxX)]
  let ssa = parsed.to_ssa() |> Ssa.set_number_of_points 1
  set_maxima parsed ssa.simulator.populations maxima
  let _, _, stationary = Ssa.simulate_with_stationary ssa
  
  // Assert that the distributions match with very high confidence
  let _, _, same = kolmogorov_smirnov 0.999999 stationary.["X"] poissPDF 
  Assert.True(same)


[<Fact(DisplayName="SSA stationary - Check CME agreement")>]
let ssa_stationary_Check_CME_agreement () = 
  let test_crn frwd back =
    sprintf 
        "
        directive simulation { final=100000.0; plots=[X] }
        directive stochastic { stationary_skiptime=1000.0 }
        directive parameters [ fr = %f; bk = %f ]
        init F 100
        | F ->{fr} X 
        | X ->{bk} F 
        " frwd back 
  let forward = 0.15
  let back = 0.1

  let maxX = 100
  let maxima = Map.ofList ["X",maxX] 
  let Xvals = Array.init maxX id

  let crn = test_crn forward back |> Crn.from_string
  let ssa = crn.to_ssa() |> Ssa.set_number_of_points 1
  set_maxima crn ssa.simulator.populations maxima
  let _, _, stationary = Ssa.simulate_with_stationary ssa      

  let short_crn = {crn with settings = {crn.settings with simulation = {crn.settings.simulation with final=1000.0 }}}
  let ctmc = short_crn.to_ctmc ()
  let cme = short_crn.to_cme ctmc
  let env = Parameters.to_env short_crn.settings.parameters
  let res_cme, _  = Cme.simulate env Map.empty false cme

  let probabilities = Probabilities.probability_map None res_cme.probabilities "X"
  let _, _, same = kolmogorov_smirnov 0.999999 stationary.["X"] (List.last probabilities)
  Assert.True(same)

   
[<Fact(Skip="Not consistent, try seeding random numbers", 
       DisplayName="(skipped) Simulation - Ssa stationary, simple")>]
let ssa_stationary_simple () = 
  let pcrn = 
    "
    directive simulation { final=100000.0; plots=[P] }
    directive stochastic { stationary_skiptime=100.0 }
    init D 1
    | D ->{0.5} D + R 
    | R ->{0.3} R + P
    | R ->{0.02}
    | P ->{0.02}
    " |> Crn.from_string

  let maxima = Map.ofList [("P", 1000)]
  let ssa = pcrn.to_ssa() |> Ssa.set_number_of_points 1
  set_maxima pcrn ssa.simulator.populations maxima
  let _, tr, stationary = Ssa.simulate_with_stationary ssa
  printfn "%A" stationary.["P"]
  
  // Assert that the mean and variance are as pre-computed
  let emean = 373.8
  let mean, variance = statistics stationary.["P"]
  Assert.InRange(mean, emean-10.0, emean+10.0)
  Assert.InRange(variance, 3000.0, 3500.0)


[<Fact(DisplayName="SSA stationary - Gene expression same as precomputed")>]
let ssa_stationary_gene_expression_same_as_precomputed () = 
  let crn = 
    "directive simulation { final=10000000.0; plots=[Protein;DNA;mRNA] }
     directive stochastic { stationary_skiptime=10000.0 }
     directive parameters [ k1 = 0.002;
      k2 = 0.207944151468;
      g1 = 0.01;
      g2 = 4e-4;]
      init DNA 1 | init mRNA 0 | init Protein 0 |
      DNA       ->{k1} DNA + mRNA |
      mRNA      ->{g1} |
      mRNA      ->{k2} mRNA + Protein |
      Protein   ->{g2}"
      |> Crn.from_string
  let ssa = crn.to_ssa() |> Ssa.set_number_of_points 1
  let maxima = Map.ofList [("Protein",300); ("DNA", 2); ("mRNA", 10)]
  set_maxima crn ssa.simulator.populations maxima
  let _, tr, stationary = Ssa.simulate_with_stationary ssa

  let precomp_txt = System.IO.File.ReadAllText("ssa_stationary_2stage.dat")
  let precomp = precomp_txt.Split [|'\t'|] |> Array.map float
  
  let diff, score, same = kolmogorov_smirnov 0.999 stationary.["Protein"] precomp
  Assert.True(same)

[<Fact(DisplayName="Simulation - multiple systems sim")>]
let simulate_multiple_systems() =
  let projectDir = System.IO.Directory.GetParent(System.IO.Directory.GetCurrentDirectory()).Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
  let code = (System.IO.Path.Combine(projectDir, "Models/GoalReceivers.crn")) |> System.IO.File.ReadAllText
  let ig = Parser.from_string InferenceSiteGraph.parse code
  let test_sim (model:Model) =
    // Expand and lift.
    let (model,_) = model.expandAndLift()
    // Simulate.
    let res = model.simulate()
    ()
  ig.nodes |> Map.toSeq |> Seq.map (fun (_,v) -> v) |> Seq.iter test_sim
  ()