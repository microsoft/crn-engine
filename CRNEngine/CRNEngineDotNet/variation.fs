// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

open Operators
[<JavaScript>]
type Variation =
  | [<WebSharper.Constant "Random">] Random
  | [<WebSharper.Constant "Fixed">] Fixed
  | [<WebSharper.Constant "Initial">] Initial2
  | [<WebSharper.Constant "Multiple">] Multiple
  static member parse =
    Parser.choice [
      Parser.kw "Random"   |>> fun _ -> Random
      Parser.kw "Fixed"    |>> fun _ -> Fixed
      Parser.kw "Initial"  |>> fun _ -> Initial2
      Parser.kw "Multiple" |>> fun _ -> Multiple
    ]
  static member to_string variation =
    match variation with
    | Random -> "Random"
    | Fixed -> "Fixed"
    | Initial2 -> "Initial"
    | Multiple -> "Multiple"