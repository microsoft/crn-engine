// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Posterior

type t = float list list

let parse = Parser.pfloat |> Parser.list_of |> Parser.list_of
let to_string (posterior:t) = ""