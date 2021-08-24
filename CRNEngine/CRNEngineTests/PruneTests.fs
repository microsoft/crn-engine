// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.PruneTests
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

let am () = 
  let crn = 
    "directive inference  { burnin = 1000; samples = 1000; thin = 100 }
    directive simulation { final=1.0; points=1000; plots=[B; Y; [X]+[Z]] }
    directive simulator sundials
    directive deterministic { reltolerance=1e-5 }
    directive parameters [r = 0.2,  { interval=Log; distribution=Uniform(1e-2,1e2); variation=Random }]
    directive rates [
      rXY = r * [X] * [Y];
      rZB = r * [Z] * [B];   // This should get pruned
    ]
    
    | X + Y ->[[rXY]] X + B
    | Y + X ->{r} Y + B
    | X + B ->{r} X + X
    | Y + B ->{r} Y + Y
    | Z + B ->[[rZB]] dummy   // This should get pruned
    
    | 30 X
    | 20 Y 
    | 0 B"
    |> Parser.from_string Crn.parse
  let data = Dataset.create "AM_obs_noised" ["AM_obs_noised.csv" |> Io.load_file |> Table<float>.parse_csv]
  crn.update_settings { crn.settings with data = [data] }


[<Fact(DisplayName="Inference - Prune")>]
let plots_test () = 
    let crn = am ()
    let ode = crn.to_ode()
    let plottables, tables, inference_parameters, odes, instances, sweeps = Ode.list_to_inference_method ode.settings [ode]
    let pruned = odes.Head
    Assert.Equal (4, pruned.reactions.Length)
    let rate_keys = pruned.settings.rates |> Map.toList |> List.map fst
    Assert.Equal<string list> (["rXY"], rate_keys)
    Assert.Equal<Functional> (Expression.Key (Key.Species (Species.create "B")), pruned.settings.simulation.plots.Head)