namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Kinetics =
  | [<WebSharper.Constant "Contextual">] Contextual
  | [<WebSharper.Constant "Stochastic">] Stochastic
  | [<WebSharper.Constant "Deterministic">] Deterministic
  member k.to_string = 
    match k with 
    | Contextual -> "Contextual"
    | Stochastic -> "Stochastic"
    | Deterministic -> "Deterministic"
  static member parse = 
    Parser.choice [
      Parser.kw "Contextual" |>> fun _ -> Contextual
      Parser.kw "Stochastic" |>> fun _ -> Stochastic
      Parser.kw "Deterministic" |>> fun _ -> Deterministic
    ] 