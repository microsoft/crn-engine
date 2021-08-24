// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Deterministic_settings 
open WebSharper

type t = {
  stiff: bool;
  abstolerance: float;
  reltolerance: float;
}

val defaults : t
val to_string : t -> string
val parse : Parser.t<t>
val from_string : string -> t