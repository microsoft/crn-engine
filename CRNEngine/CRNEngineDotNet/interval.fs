namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Interval = 
  | [<WebSharper.Constant "Real">] Real
  | [<WebSharper.Constant "Log">] Log
  static member parse =
    Parser.choice [ 
      Parser.kw "Real" |>> fun _ -> Real
      Parser.kw "Log"  |>> fun _ -> Log
    ]
  static member to_string interval =
    match interval with
    | Real -> "Real"
    | Log -> "Log"