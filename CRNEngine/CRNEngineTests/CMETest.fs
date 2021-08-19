module Microsoft.Research.CRNEngine.Tests.CMETest

open Xunit
open FsUnit
open Microsoft.Research.CRNEngine

type Integrator = Oslo | Sundials

let degradation () = 
  "directive simulation { final=0.6931471806; plots=[X] }  //final = log(2)
  directive simulator cme
  directive deterministic { stiff=true; reltolerance=1e-6 }
  directive parameters [ 
    k=1.0;
    a=10.0;
  ]
  init X a |
  X ->{k}"
  |> Parser.from_string Crn.parse

let prepare_for_cme (crn:Crn) = 
  let ctmc = crn.to_ctmc()
  let cme = crn.to_cme ctmc
  let env = Parameters.to_env crn.settings.parameters
  let rates = Crn.to_inlined_rates crn.settings
  cme, env, rates

[<Fact(DisplayName="CME - Degradation")>]
let ``Degradation``() =  
  let crn = degradation ()
  let cme, env, rates = prepare_for_cme crn
  let _, result = Cme.simulate env rates false cme 
  let data = result.columns.Head.values
  let times = result.times
  let points = data.Length
  let t1, t2 = times.[points-1], times.[points-2]
  let m1, m2 = data.[points-1].mean, data.[points-2].mean
  let k = (log 2.0 - t1) / (t2 - t1)
  
  let got = m1 + k * (m2 - m1)  
  let expected = 5.0
  Assert.Equal(expected, got, 3)

[<Fact(DisplayName="CME Sundials - Degradation")>]
let degradation_sundials () =  
  let crn = degradation ()
  let cme, env, rates = prepare_for_cme crn
  let _, result = cme.simulate_sundials env rates
  let data = result.columns.Head.values
  let times = result.times
  let points = data.Length
  let t1, t2 = times.[points-1], times.[points-2]
  let m1, m2 = data.[points-1].mean, data.[points-2].mean
  let k = (log 2.0 - t1) / (t2 - t1)
  
  let got = m1 + k * (m2 - m1)  
  let expected = 5.0
  Assert.Equal(expected, got, 3)

// [<Fact>]
let ``Probability map``() = 
  let crn = degradation ()
  let cme, _ = crn.simulate_cme_state()
  let data = Probabilities.probability_map None cme.probabilities crn.initials.Head.species.name
  
  // Test the initial condition got mapped correctly (program above requires a = 10.0)
  let init_exp = Array.init 11 (fun i -> if i=10 then 1.0 else 0.0)
  let init_obs = List.head data
  (init_exp, init_obs) 
  ||> Array.iter2 (fun eij dij -> Assert.Equal (eij, dij)) 

  // Test the final point is close to what we expect
  let final_exp = 
    [| 0.00098860575836274634
     ; 0.0098618386249395076
     ; 0.044269623071772284
     ; 0.11776309973644848
     ; 0.20558024639188943
     ; 0.24609168149558006
     ; 0.20457411480952942
     ; 0.11661325669693773
     ; 0.0436227834077111
     ; 0.0096701234172185385
     ; 0.00096462658961050365 |]
  let final_obs = List.last data
  (final_exp, final_obs) 
  ||> Array.iter2 (fun eij dij -> Assert.Equal (eij, dij, 3)) 

let am X0 Y0 = 
    let N = X0 + Y0 |> float
    Crn.from_string (sprintf "directive simulation { final=%f }

      init X %d |
      init Y %d |

      X + Y ->{1.0} Y + B |
      Y + X ->{1.0} X + B |
      B + X ->{1.0} X + X |
      B + Y ->{1.0} Y + Y" (100.0/N) X0 Y0)

let AMtest integrator (program:Crn) = 
    let cme, result = 
      match integrator with
      | Oslo -> program.simulate_cme_state()
      | Sundials -> 
        let cme, env, rates = prepare_for_cme program
        cme.simulate_sundials env rates
    let end_point = result.columns |> List.map (fun x -> List.last x.values)
    let means = end_point |> List.map (fun p -> p.mean)
    let stdevs = end_point |> List.map (fun p -> p.stdev)
    let final_marginalX = Probabilities.probability_map None cme.probabilities "X" |> List.last |> List.ofArray
    means, stdevs, final_marginalX

//[<Trait("Category", "Slow")>]
//[<Fact(Skip="Flakey test, produces NaN sometimes, investigate")>]
[<Fact(DisplayName="CME - AM - mass action rates")>]
let approximate_majority () =
    let X0 = 20
    let Y0 = 40
    let N = X0 + Y0 |> float
    let program = am X0 Y0

    let means, stdevs, final_marginalX = AMtest Oslo program
    
    let expected_means = [0.0; N; 0.0]   // Approximately, when X0 > Y0 at high copy
    let expected_stds = means |> List.map (fun m -> if m < 0.0 then 0.0 else sqrt (N*m - m**2.0))
    let expected_marginalX = [ [0.97]; List.init (X0+Y0-1) (fun _ -> 0.0); [0.03] ] |> List.concat
    let dp = N*0.1
    means |> List.iter2 (fun exp act -> Assert.InRange (act, exp-dp, exp+dp)) expected_means
    stdevs |> List.iter2 (fun exp act -> Assert.Equal (exp, act, 1)) expected_stds
    final_marginalX |> List.iter2 (fun exp act -> Assert.Equal(exp,act,1)) expected_marginalX

[<Fact(DisplayName="CME Sundials - AM - mass action rates")>]
let am_sundials () =
    let X0 = 20
    let Y0 = 40
    let N = X0 + Y0 |> float
    let program = am X0 Y0

    let means, stdevs, final_marginalX = AMtest Sundials program
    
    let expected_means = [0.0; N; 0.0]   // Approximately, when X0 > Y0 at high copy
    let expected_stds = means |> List.map (fun m -> if m < 0.0 then 0.0 else sqrt (N*m - m**2.0))
    let expected_marginalX = [ [0.97]; List.init (X0+Y0-1) (fun _ -> 0.0); [0.03] ] |> List.concat
    let dp = N*0.1
    means |> List.iter2 (fun exp act -> Assert.InRange (act, exp-dp, exp+dp)) expected_means
    stdevs |> List.iter2 (fun exp act -> Assert.Equal (exp, act, 1)) expected_stds
    final_marginalX |> List.iter2 (fun exp act -> Assert.Equal(exp,act,1)) expected_marginalX
  
//[<Trait("Category", "Slow")>]
[<Fact(DisplayName="CME - AM - functional rates")>]
let approximate_majority_functional_rates () =
    let X0 = 20
    let Y0 = 10
    let N = X0 + Y0 |> float
    let program = am X0 Y0

    let means, stdevs, final_marginalX = AMtest Oslo program
    
    let expected_means = [N;0.0;0.0]   // Approximately, when X0 > Y0 at high copy
    let expected_stds = means |> List.map (fun m -> if m < 0.0 then 0.0 else sqrt (N*m - m**2.0))
    let expected_marginalX = [ [0.03]; List.init (X0+Y0-1) (fun _ -> 0.0); [0.97] ] |> List.concat
    
    let dp = N*0.1
    means |> List.iter2 (fun exp act -> Assert.InRange (act, exp-dp, exp+dp)) expected_means
    stdevs |> List.iter2 (fun exp act -> Assert.Equal (exp, act, 1)) expected_stds
    final_marginalX |> List.iter2 (fun exp act -> Assert.Equal(exp,act,1)) expected_marginalX

//[<Trait("Category", "Slow")>]
[<Fact(DisplayName="CME - AM - functional rates expressions")>]
let approximate_majority_functional_rates_expressions () =
    let X0 = 20
    let Y0 = 10
    let N = X0 + Y0 |> float
    let program = am X0 Y0

    let means, stdevs, final_marginalX = AMtest Oslo program
    
    let expected_means = [N;0.0;0.0]   // Approximately, when X0 > Y0 at high copy
    let dp = N*0.1
    let expected_stds = means |> List.map (fun m -> if m < 0.0 then 0.0 else sqrt (N*m - m**2.0))
    let expected_marginalX = [ [0.03]; List.init (X0+Y0-1) (fun _ -> 0.0); [0.97] ] |> List.concat
    
    means |> List.iter2 (fun exp act -> Assert.InRange (act, exp-dp, exp+dp)) expected_means
    stdevs |> List.iter2 (fun exp act -> Assert.Equal (exp, act, 1)) expected_stds
    final_marginalX |> List.iter2 (fun exp act -> Assert.Equal(exp,act,1)) expected_marginalX

[<Fact(DisplayName = "CME - Sweeps with reusable state-spaces")>]
let cme_sweep () =
    let crn = Crn.from_string "
    directive simulator cme
    directive simulation { final=2.0 }
    directive parameters [X0=0.0; Y0=0.0; r=1.0]
    directive sweeps [ 
      test = [
        (X0,Y0) = [(5,10); (10,5)]; 
        r = [0.1; 1.0];
      ] 
    ]
    init X X0 |
    init Y Y0 |

    X + Y ->{r} Y + B |
    Y + X ->{r} X + B |
    B + X ->{r} X + X |
    B + Y ->{r} Y + Y"
    let case_columns = crn.simulate_case () |> List.map (fun r -> r.table.columns)
    let Xm, Ym = case_columns |> List.map (fun cols -> cols |> List.map (fun c -> c.values |> List.last) |> fun l -> l.[0], l.[1]) |> List.unzip
    Assert.Equal(0.0, Xm.[0] - Ym.[2], 2)
    Assert.Equal(0.0, Xm.[2] - Ym.[0], 2)
    Assert.Equal(0.0, Xm.[1] - Ym.[3], 2)
    Assert.Equal(0.0, Xm.[3] - Ym.[1], 2)


[<Fact(DisplayName = "CME - Check negative probabilities in Join example")>]
let cme_negatives () = 
    let crn = Crn.from_string "directive simulation { 
    final=1500; 
    plots=[Signal];
}
directive stochastic {scale = 1}
directive simulator cme
directive parameters [ k = 0.003; u = 0.1 ]
| 10 Reporter 
| 10 Join
| 6 Input2
| 6 Input1
| Reporter + Output ->{k} sp2 + Signal
| Join + Input1 <->{k}{k} sp4 + sp3
| sp4 + Input2 <->{k}{k} sp1 + Output"
    let cme, _ = crn.simulate_cme_state ()
    //let sums = cme.probabilities.stateprobabilities |> List.map Array.sum
    let marginals = Probabilities.probability_map (Some 1e-8) cme.probabilities "Signal"
    marginals