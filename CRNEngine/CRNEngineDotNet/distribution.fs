// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Uniform = 
  {min:float; max:float}
  member u.sample (rng:Rng.Random) = 
    let v = rng.NextDouble()
    (1.0-v)*u.min + v*u.max
[<JavaScript>]
type Normal = 
  {mean:float; stdev:float}
  member n.sample (rng:Rng.Random) = n.stdev*(Statistics.sample_standardnormal rng) + n.mean 
[<JavaScript>]
type LogNormal = 
  {mu:float; sigma:float}
  member n.sample (rng:Rng.Random) = n.sigma*(Statistics.sample_standardnormal rng) + n.mu |> exp
  member n.mode () = exp (n.mu - n.sigma*n.sigma)
  member n.quantile x = exp (Microsoft.Research.Filzbach.Lib.erfinv(2.0*x-1.0)*sqrt(2.0)*n.sigma + n.mu)
[<JavaScript>]
type Truncated = 
  {mean:float; stdev:float; min:float; max:float}
  member tn.sample (rng:Rng.Random) = 
    let mutable x = tn.stdev*(Statistics.sample_standardnormal rng) + tn.mean 
    while (x < tn.min || x > tn.max) do
      x <- tn.stdev*(Statistics.sample_standardnormal rng) + tn.mean 
    x
[<JavaScript>]
[<WebSharper.NamedUnionCases>]
type Distribution = 
  | Uniform of Uniform:Uniform
  | Normal of Normal:Normal
  | LogNormal of LogNormal:LogNormal
  | TruncatedNormal of TruncatedNormal:Truncated
  //| Posterior of Posterior:float list list
  member d.sample rng = 
    match d with
    | Uniform u -> u.sample rng
    | Normal n -> n.sample rng
    | LogNormal n -> n.sample rng
    | TruncatedNormal tn -> tn.sample rng
  static member parse =
    Parser.choice [
      Parser.kw "Uniform" >>. (Parser.pfloat .>>. (Parser.skw "," >>. Parser.pfloat) |> Parser.bracket "(" ")") |>> fun (min,max) -> Uniform {min=min;max=max}
      Parser.kw "Normal" >>. (Parser.pfloat .>>. (Parser.skw "," >>. Parser.pfloat) |> Parser.bracket "(" ")")  |>> fun (m,s) -> Normal {mean=m; stdev=s}
      Parser.kw "LogNormal" >>. (Parser.pfloat .>>. (Parser.skw "," >>. Parser.pfloat) |> Parser.bracket "(" ")")  |>> fun (m,s) -> LogNormal {mu=m; sigma=s}
      Parser.kw "TruncatedNormal" >>. 
        (Parser.pfloat .>>. (Parser.skw "," >>. Parser.pfloat) .>> Parser.skw "," .>>. (Parser.pfloat .>>. (Parser.skw "," >>. Parser.pfloat)) 
        |> Parser.bracket "(" ")")    
        |>> fun ((mean,stdev),(min,max)) -> TruncatedNormal{mean=mean;stdev=stdev;min=min;max=max};
      //Parser.kw "Posterior" >>. (Parser.pfloat |> Parser.list_of |> Parser.list_of) |>> Posterior;
    ]
  static member to_string distribution =
    match distribution with
    | Uniform{min=min;max=max} -> sprintf "Uniform(%s,%s)" (min.ToString()) (max.ToString())
    | Normal{mean=mean;stdev=stdev} -> sprintf "Normal(%s,%s)" (mean.ToString()) (stdev.ToString())
    | LogNormal{mu=m;sigma=s} -> sprintf "LogNormal(%s,%s)" (m.ToString()) (s.ToString())
    | TruncatedNormal{mean=mean;stdev=stdev;min=min;max=max} -> sprintf "TruncatedNormal(%s,%s,%s,%s)" (mean.ToString()) (stdev.ToString()) (min.ToString()) (max.ToString())
    //| Posterior p -> ""