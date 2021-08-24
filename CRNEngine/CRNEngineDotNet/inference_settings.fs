// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
open System.Diagnostics
open Microsoft.Research.Filzbach.FilzbachJS

[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Inference_settings = 
  { 
    name: string;                       // Name of inference problem
    burnin: int;                        // Number of burn-in samples (discarded from MCMC chain) used by Filzbach
    samples: int;                       // Number of samples retained by Filzbach for the joint posterior
    thin: int;                          // MCMC chain is "thinned" to remove autocorrelation. Every nth sample is retained
    noise_model: Noise_model;           // The noise model describes the distribution of model-data deviations (see noise_model type)
    noise_parameter: Noise_parameter;   // The noise parameter is associated with the noise model (see noise parameter type)
    prune: bool;                        // Unused reactions and species can be trimmed from the model, for efficiency sake
    seed: uint32;                       // The seed for the random number generator to be used in Filzbach
    seeds: uint32 list;                 // For each chain, the seed for the random number generator to be used in Filzbach
    lowerbound: float option;           // Bound on the maximum-likelihood when joining multiple MCMC chains
    timer: bool;                        // Keep track of simulation times
    partial_evaluation: bool;           // Use partial evaluation of the likelihood
    print_console: bool;                // Print console output
    print_summary: bool;                // Print parameter summary 
  }

  static member defaults = {
    name = "default";
    burnin = 100;
    samples = 100;
    thin = 10;
    seed = 0u;
    seeds = [];
    lowerbound = None;
    noise_parameter = Random;
    noise_model = Constant;
    prune = false;
    timer = false;
    partial_evaluation = false;
    print_console = true;
    print_summary = true;
  }

  static member to_string =
    Lib.emit_record Inference_settings.defaults
      [ "name"       , fun r -> r.name
      ; "burnin"     , fun r -> r.burnin |> sprintf "%d"
      ; "samples"    , fun r -> r.samples |> sprintf "%d"
      ; "thin"       , fun r -> r.thin |> sprintf "%d"
      ; "seed"       , fun r -> r.seed |> sprintf "%d"
      ; "seeds"       , fun r -> r.seeds |> List.map (sprintf "%d") |> String.concat "; " |> sprintf "[ %s ]"
      ; "lowerbound"  , fun r -> match r.lowerbound with Some lb -> lb.ToString() | None -> ""
      ; "noise_model", fun r -> Noise_model.to_string r.noise_model
      ; "noise_parameter", fun r -> Noise_parameter.to_string r.noise_parameter
      ; "prune"      , fun r -> r.prune |> sprintf "%b"
      ; "timer"      , fun r -> r.timer |> sprintf "%b" 
      ; "print_console" , fun r -> r.print_console |> sprintf "%b" 
      ; "print_summary" , fun r -> r.print_summary |> sprintf "%b" 
      ; "partial"    , fun r -> r.partial_evaluation |> sprintf "%b"
      ]
(*
  sprintf
    "{
  burnin = %d;
  samples = %d;
  thin = %d;
  seed = %d;
  //noise_parameter = Random;
  noise_model = %s;
  prune = %b;
}" i.burnin i.samples i.thin i.seed (string_of_noise_model i.noise_model) i.prune 
*)
  #if JavaScript
  [<WebSharper.Inline "$0">]
  #endif  
  static member uint32 x = uint32 x
    
  static member parse_defaults (defs:Inference_settings) =
    //enables web sharper translation
    let ( |>> ) a b c = Parser.( |>>) a b c
    let ( >>% ) a b c = Parser.( >>% ) a b c
    let ( >>. ) a b c = Parser.( >>. ) a b c
    Parser.record defs [
      "name",    Parser.name |>> fun d s -> { s with name = d }
      "burnin",  Parser.pint32 |>> fun d s -> { s with burnin = d }
      "samples", Parser.pint32 |>> fun d s -> { s with samples = d }
      "thin",    Parser.pint32 |>> fun d s -> { s with thin = d }
      "noise_model", Noise_model.parse |>> fun d s -> { s with noise_model = d}        
      "noise_parameter", Noise_parameter.parse |>> fun d s -> { s with noise_parameter = d}
      "prune", Parser.pbool |>> fun d s -> { s with prune = d }
      "seed", Parser.pint32 |>> fun d s -> { s with seed = Inference_settings.uint32 d }
      "seeds", Parser.list_of Parser.pint32 |>> fun d s -> { s with seeds = (List.map Inference_settings.uint32 d) }
      "lowerbound", Parser.pfloat |>> fun d s -> { s with lowerbound = Some d }
      "timer", Parser.pbool |>> fun d s -> { s with timer = d }
      "print_console", Parser.pbool |>> fun d s -> { s with print_console = d }
      "print_summary", Parser.pbool |>> fun d s -> { s with print_summary = d }
      "partial", Parser.pbool |>> fun d s -> { s with partial_evaluation = d }
    ]
  static member parse = Inference_settings.parse_defaults Inference_settings.defaults
  static member from_string (s:string) = Parser.from_string Inference_settings.parse s