// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Ode_settings 
open WebSharper

type simulation_settings<'s> when 's:equality = Simulation_settings.t<'s>

type t<'s> when 's:equality = {
  simulation: simulation_settings<'s>;
  deterministic: Deterministic_settings.t;
  inference: Inference_settings.t; 
  data: Dataset.t list;
  units: Units.t;
  parameters: Parameter list;
  rates:Map<string, Expression.t<'s>>;
  sweeps: Sweep list;
}

val defaults : t<'s>
val substitute : Environment.t -> t<'s> -> t<'s>
val update_times : float list -> t<'s> -> t<'s>
val get_inference_parameters : t<'s> -> Parameter list
