// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

open Operators
open System.Diagnostics

[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Stochastic_settings  = 
  {
    scale: float;
    steps: int option;
    trajectories: int;
    stationary_skiptime: float option;
  }
  static member defaults = {
    scale = 1.0;
    steps = None;
    trajectories = 1;
    stationary_skiptime = None;
  }
  static member to_string_defaults (defaults:Stochastic_settings) (s:Stochastic_settings) = 
    let intoption_to_string (name:string) (o:int option) = 
      match o with
      | None -> ""
      | Some(i) -> sprintf "name = %d; " i
    let floatoption_to_string (name:string) (o:float option) = 
       match o with 
       | None -> ""
       | Some(i) -> sprintf "name = %f" i
    s |> Lib.emit_record defaults [
      "scale", fun r -> r.scale.ToString ();
      "steps", fun r -> intoption_to_string "steps" r.steps;
      "trajectories", fun r -> r.trajectories.ToString ();
      "stationary_skiptime", fun r -> floatoption_to_string "stationary_skiptime" r.stationary_skiptime;
    ] 


(*
    let option_to_string (name:string) (o:int option) = 
      match o with
      | None -> ""
      | Some(i) -> sprintf "name = %d; " i
    sprintf "{scale = %s; %s%strajectories = %d}" 
      (s.scale.ToString()) (option_to_string "seed" s.seed) (option_to_string "steps" s.steps) s.trajectories 
*)
  static member to_string (s:Stochastic_settings) = Stochastic_settings.to_string_defaults Stochastic_settings.defaults s
  static member parse_defaults (defaults:Stochastic_settings) =
     Parser.record defaults [ 
       "scale", Parser.pfloat  |>> fun v (s:Stochastic_settings) -> { s with scale = v }
       "steps", Parser.pint32 |>> fun v (s:Stochastic_settings) -> { s with steps = Some v }
       "trajectories", Parser.pint32 |>> fun v (s:Stochastic_settings) -> { s with trajectories = v }
       "stationary_skiptime", Parser.pfloat |>> fun v (s:Stochastic_settings) -> { s with stationary_skiptime = Some v }
     ]
  static member parse = Stochastic_settings.parse_defaults Stochastic_settings.defaults
  static member from_string (s:string) = Parser.from_string Stochastic_settings.parse s