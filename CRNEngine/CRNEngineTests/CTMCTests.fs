// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.CTMCTests
open Xunit
open FsUnit
open Microsoft.Research.CRNEngine.Api
open Microsoft.Research.CRNEngine

let crn_from_string = Crn.parse |> Parser.from_string

let item state id = if Map.containsKey id state then state.[id] else 0
let transitions graph = Dictionary.toSeq graph
                        |> Seq.collect (fun (x, ys) -> ys |> List.map (fun (y:Transition) -> sprintf "%d %d -> %d %d" (item x 0) (item x 1) (item y.target 0) (item y.target 1) ) )
                        |> Seq.distinct
                        |> Seq.sort
    
[<Fact(DisplayName="CTMC - Explore a simple float calculus")>]
let explore_a_simple_float_calculus () =

    let react is i =
        is
        |> List.map (fun j -> Reaction.create([], [i; j], !-> (Expression.Float i), [(i + j) / 2.0] ))

    let species = [(3.0, 2); (17.0, 1)]

    let calc = { react = react }
    let is, ar, adapter = calc.initialise_ctmc species
    let ctmc_result = calc.to_ctmc (Hashtable.empty ()) 1.0 is ar adapter

    Assert.Equal(3, ctmc_result.ctmc.graph.Keys.Count)

[<Fact(DisplayName="CTMC - AM")>]
let approximate_majority () =
    let X0 = 10
    let Y0 = 20
    let N = X0 + Y0
    let program = crn_from_string (sprintf "init X %d |
init Y %d |

X + Y ->{1.0} Y + B |
Y + X ->{1.0} X + B |
B + X ->{1.0} X + X |
B + Y ->{1.0} Y + Y" X0 Y0)

    let ctmc_result = program.to_ctmc()

    // All state vectors where x+y+b = 275 are possible
    // so we can expect the number of ordered partitions
    let num_transitions = 2*N*(N-1)
    let num_states = ((N+1)*N)/2 + N // (y = 0 i.e split in two) + (y > 0 i.e split in three)
    Assert.Equal(num_transitions, Seq.length (transitions ctmc_result.ctmc.graph))
    Assert.Equal(num_states, ctmc_result.ctmc.graph.Keys.Count)
    ()

[<Fact(DisplayName="CTMC - AM - functional rates ")>]
let approximate_majority_functional_rates () =
    let X0 = 1
    let Y0 = 2
    let N = X0 + Y0
    let program = crn_from_string (sprintf "init X %d |
init Y %d |

X + Y ->[[X]*[Y]] Y + B |
Y + X ->[[X]*[Y]] X + B |
B + X ->{1.0} X + X |
B + Y ->{1.0} Y + Y" X0 Y0)

    let ctmc_result = program.to_ctmc()

    // All state vectors where x+y+b = 275 are possible
    // so we can expect the number of ordered partitions
    let num_states = ((N+1)*N)/2 + N // (y = 0 i.e split in two) + (y > 0 i.e split in three)
    let num_transitions = 2*N*(N-1)
    Assert.Equal(num_transitions, Seq.length (transitions ctmc_result.ctmc.graph))
    Assert.Equal(num_states, ctmc_result.ctmc.graph.Keys.Count)
    ()

[<Fact(DisplayName="CTMC - AM - functional rates expressions")>]
let approximate_majority_functional_rates_expressions () =
    let X0 = 30
    let Y0 = 20
    let N = X0 + Y0
    let program = crn_from_string (sprintf "
    directive rates [
      massAction = [X]*[Y]
    ]
    init X %d |
init Y %d |

X + Y ->[[massAction]] Y + B |
Y + X ->[[massAction]] X + B |
B + X ->{1.0} X + X |
B + Y ->{1.0} Y + Y" X0 Y0)

    let ctmc_result = program.to_ctmc()

    // All state vectors where x+y+b = 275 are possible
    // so we can expect the number of ordered partitions
    let num_states = ((N+1)*N)/2 + N // (y = 0 i.e split in two) + (y > 0 i.e split in three)
    let num_transitions = 2*N*(N-1)
    Assert.Equal(num_transitions, Seq.length (transitions ctmc_result.ctmc.graph))
    Assert.Equal(num_states, ctmc_result.ctmc.graph.Keys.Count)
    ()

[<Fact(DisplayName="CTMC - AM variation")>]
let am_variation () =

    let program = crn_from_string "X + Y ->{0.001} X + B |
Y + X ->{0.002} Y + B |
Z ~ X + B ->{0.0005} X + X |
Y + B <->{0.003}{0.001} Y + Y |
Y <->{0.002}{0.0003} Z |

init X 3 |
init Y 2 |
init B 0"

    let ctmc_result = program.to_ctmc

    //Assert.Equal(?, graph.Keys.Count)
    ()

[<Fact(DisplayName="CTMC - Degradation")>]
let degradation () =

    let program = crn_from_string "directive parameters [ 
      k=1.0;
      a=10.0;
    ]
    init X a |
    X ->{k}"

    let ctmc_result = program.to_ctmc()

    let num_states = ctmc_result.ctmc.graph.Keys.Count
    Assert.Equal(11, num_states)

    let num_transitions =
        ctmc_result.ctmc.graph
        |> Seq.sumBy (fun st -> st.Value.Length)
    Assert.Equal(10, num_transitions)
    ()
