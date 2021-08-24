// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Population

type t<'species,'value> = {
  species: 'species;
  value: 'value;
  constant: bool; // Population never changes.
  initial: bool; // Present at start of simulation. Can include species generated from infinite rate reactions.
  input: bool; // Specified by the user.
  max: int option;
}

val create : bool -> 'value -> 'species -> t<'species,'value>
val to_string : ('species -> string) -> ('value -> string) -> t<'species,'value> -> string
