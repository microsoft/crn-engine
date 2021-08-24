// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

open Operators
open Parser

/// The noise model describes the distribution of model-data deviations. Assumed Gaussian, with variance related to the following options:
[<JavaScript>]
type Noise_model =
  | [<WebSharper.Constant "Constant">] Constant          // The variance is a constant value, independent of the signal value
  | [<WebSharper.Constant "Proportional">] Proportional  // The variance is proportional to the signal value

  static member to_string noise_model =
    match noise_model with
    | Constant -> "constant"
    | Proportional -> "proportional"

  static member parse = 
    Parser.choice [ 
      Parser.kw "constant" >>% Constant; 
      Parser.kw "proportional" >>% Proportional 
    ] 