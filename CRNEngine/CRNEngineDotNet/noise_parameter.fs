namespace Microsoft.Research.CRNEngine

open Operators
open Parser
                
[<JavaScript>]
type Noise_parameter =
  | Fixed of Fixed:float                      // Fixed at the given value
  | [<WebSharper.Constant "Random">] Random              // To be inferred
  | [<WebSharper.Constant "Multiple">] Multiple          // Each "Plottable" is assigned a different noise parameter, all to be inferred

  static member to_string noise_parameter =
    match noise_parameter with
    | Fixed f -> sprintf "Fixed(%f)" f
    | Random -> "Random"
    | Multiple -> "Multiple"

  static member parse = 
    Parser.choice [ 
      Parser.kw "Fixed" >>. (Parser.pfloat |> Parser.bracket "(" ")") |>> Fixed;
      Parser.kw "Random" >>% Random;
      Parser.kw "Multiple" >>% Multiple
    ]