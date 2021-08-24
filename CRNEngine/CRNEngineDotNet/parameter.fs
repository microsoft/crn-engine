// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
open System.Diagnostics
//module Filzbach = Microsoft.Research.Filzbach
module fb = Microsoft.Research.Filzbach.Parameters

[<JavaScript>]
[<DebuggerDisplay("{ this.to_string_debug() }")>] // displays CRNs as strings in VS debugger
type Parameter = 
  {name:string; value:float; prior:Prior option} 
  static member create(name:string, value:float, prior:Prior option) = {name=name;value=value;prior=prior}
  static member create((name:string, value:float), prior:Prior option) = Parameter.create(name,value,prior)
  static member variable (p:Parameter) = 
    match p.prior with
    | None -> false
    | Some(p) -> not (Prior.is_fixed p) 
  static member substitute (e:Environment.t) (p:Parameter) =
    match Map.tryFind p.name e with 
    | None -> p
    | Some (v) -> Parameter.create(p.name,v,p.prior)
  // Retain only "variable" parameters, as fixed parameters should be assigned from an environment
  static member to_filzbach (p:Parameter) : fb.Parameter =
    match p.prior with 
    | Some pr -> 
        let map_interval interval is_fixed = 
          if is_fixed 
          then fb.Fixed 
          else
            match interval with 
            | Interval.Real -> fb.Real
            | Interval.Log  -> fb.Log
        let interval = map_interval pr.interval (pr.variation=Variation.Fixed) in // TODO: Implement Multiple variation
        let r : fb.ParameterRange = 
          match pr.distribution with
          | Distribution.Uniform{min=min;max=max} -> { pType = interval; lb = min; ub = max }
          | Distribution.Normal{mean=mean;stdev=std}  -> 
              // Magic number of 20 standard deviations
              let ndev = 20.0
              let lb = mean - ndev*std      
              let ub = mean + ndev*std
              match interval with 
              | fb.Real -> { pType = interval; lb = lb; ub = ub}
              | fb.Log  -> { pType = interval; lb = max lb 0.0; ub = ub}
              | fb.Fixed -> { pType = interval; lb = mean; ub = mean }
              // No bounds
              (*match interval with 
              | fb.Real -> { pType = interval; lb = System.Double.MinValue; ub = System.Double.MaxValue}
              | fb.Log  -> { pType = interval; lb = 0.0; ub = System.Double.MaxValue}
              | fb.Fixed -> { pType = interval; lb = mean; ub = mean }*)
          | Distribution.LogNormal ln -> { pType = interval; lb=ln.quantile 1e-6; ub=ln.quantile (1.0-1e-6) }
          | Distribution.TruncatedNormal tn -> { pType = interval; lb=tn.min; ub=tn.max }
          //| Distribution.Posterior post  -> { pType = interval; lb = 0.0; ub = 1.0 }     // TODO: Fix bounds to something sensible
        let (d : fb.PriorValue option), initValue = 
          match pr.distribution with
          | Distribution.Normal n -> Some (fb.createNormal n.mean n.stdev), n.mean
          | Distribution.LogNormal ln -> Some (fb.createLogNormal ln.mu ln.sigma), ln.mode ()
          | Distribution.TruncatedNormal tn -> Some (fb.createTruncatedNormal tn.mean tn.stdev tn.min tn.max), tn.mean
          | _ -> None, p.value
        { name = p.name; range = r; initValue = Some initValue; prior = d; summary = true }
    | None -> 
        { name = p.name; range = { pType = fb.Fixed; lb = p.value; ub = p.value }; initValue = Some p.value; prior = None; summary = false }
  static member is_multiple (p:Parameter) = 
    match p.prior with
    | None -> false
    | Some(o) -> Prior.is_multiple o
  static member getname (p:Parameter) = p.name
  static member to_string (p:Parameter) =
    let prior_to_string =
      match p.prior with
      | Some prior -> sprintf ", %s" (Prior.to_string prior)
      | None -> "" 
    sprintf "%s = %s%s" p.name (p.value.ToString()) prior_to_string
  member p.to_string_debug () = p.name + " = " + (p.value.ToString()) + ", " + match p.prior with Some pr -> Distribution.to_string pr.distribution | None -> "None"
  static member simple = Parser.name .>>. (Parser.skw "=" >>. Parser.pfloat) |>> fun (n,v) -> Parameter.create(n,v,None)
  static member with_prior = Parser.name .>>. (Parser.skw "=" >>. Parser.pfloat) .>>. (Parser.skw "," >>. Prior.parse) |>> fun ((n,v),p) -> Parameter.create(n,v,Some p)
  static member parse = 
    Parser.name .>>. (Parser.skw "=" >>. Parser.pfloat) >>=
                (fun (n,v) -> 
                  (Parser.skw "," >>. Prior.parse >>= (fun p -> Parser.preturn (Parameter.create(n,v,Some p))))
                   <|> (Parser.preturn (Parameter.create(n,v,None))))        

