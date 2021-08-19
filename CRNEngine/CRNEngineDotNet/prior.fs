namespace Microsoft.Research.CRNEngine

open Operators
open System.Diagnostics

[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Prior = 
  { interval:Interval; distribution:Distribution; variation:Variation }
  static member empty = { interval = Log; distribution = Uniform{min=0.0;max=0.0}; variation = Random }
  static member is_fixed (p:Prior) = p.variation = Fixed
  static member is_multiple (p:Prior) = p.variation = Multiple
  static member fix (p:Prior) = {p with variation = Fixed}
  static member vary (p:Prior) = {p with variation = Random}
  static member parse =
    Parser.record Prior.empty [ 
      "interval", Interval.parse |>> fun i p -> {p with interval = i}
      "distribution", Distribution.parse |>> fun d p -> {p with distribution = d}
      "variation", Variation.parse |>> fun v p -> {p with variation = v}
    ] 
  static member to_string (prior:Prior) =
    sprintf "{interval=%s; distribution=%s; variation=%s}"
      (Interval.to_string prior.interval)
      (Distribution.to_string prior.distribution)
      (Variation.to_string prior.variation)