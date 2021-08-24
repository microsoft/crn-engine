// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Model

type settings = Crn_settings.t<Key.t<Species>>
type t = {
  top: Crn.t;
  systems: Crn.t list;
  inference: InferenceGraph.t list
}

val create : settings -> Crn.t list -> t
val update_settings : settings -> t -> t
val simulate : t -> Result.t<float> list
val inferMultiple : t -> Inference.mcmc_result
val infer : t -> (string * (Crn.t * Inference.mcmc_result) list) list
val to_string : t -> string
val parse : Parser.t<t> 