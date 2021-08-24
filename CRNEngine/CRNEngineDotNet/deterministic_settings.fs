// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Deterministic_settings  = 
  { stiff:bool; abstolerance:float; reltolerance:float }
  static member defaults = 
    { stiff = false; abstolerance = 1e-6; reltolerance = 1e-5 }
  static member create (scale:float) (b:bool) (abs:float) (rel:float) = 
    { stiff = b; abstolerance = abs; reltolerance = rel }
  static member to_string_defaults (defaults:Deterministic_settings) (s:Deterministic_settings) = 
    s |> Lib.emit_record defaults [
      "stiff",        fun r -> r.stiff |> sprintf "%b";
      "reltolerance", fun r -> r.reltolerance.ToString (); 
      "abstolerance", fun r -> r.abstolerance.ToString ()
    ] 
  static member to_string (s:Deterministic_settings) = Deterministic_settings.to_string_defaults Deterministic_settings.defaults s
  static member parse_defaults (defaults:Deterministic_settings) =
    let spaces = Parser.spaces
    Parser.record defaults [ 
      "stiff"       , Parser.pbool  .>> spaces |>> fun b (s:Deterministic_settings) -> { s with stiff = b }
      "abstolerance", Parser.pfloat .>> spaces |>> fun v (s:Deterministic_settings) -> { s with abstolerance = v }
      "reltolerance", Parser.pfloat .>> spaces |>> fun v (s:Deterministic_settings) -> { s with reltolerance = v }
    ]
  static member parse = Deterministic_settings.parse_defaults Deterministic_settings.defaults
  static member from_string (s:string) = Parser.from_string Deterministic_settings.parse s