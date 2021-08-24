// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Instance

type t = {
  model: string;
  sweep: string;
  environment: Environment.t; 
}

val empty : t
val create : model:string -> sweep:string -> environment:Environment.t -> t
val same_sweep : t -> t -> bool
