// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Inference_settings 
open WebSharper

// The noise model describes the distribution of model-data deviations. Assumed Gaussian, with variance related to the following options:
type noise_model =
  | [<Constant "Constant">] Constant          // The variance is a constant value, independent of the signal value
  | [<Constant "Proportional">] Proportional  // The variance is proportional to the signal value

// The noise model is parameterised by a single quantity. For Constant, this is its value, for Proportional, this is the constant of proportionality.
type noise_parameter =
  | Fixed of Fixed:float                      // Fixed at the given value
  | [<Constant "Random">] Random              // To be inferred
  | [<Constant "Multiple">] Multiple          // Each "Plottable" is assigned a different noise parameter, all to be inferred

type t = { 
  name: string;                       // Name of inference problem
  burnin: int;                        // Number of burn-in samples (discarded from MCMC chain) used by Filzbach
  samples: int;                       // Number of samples retained by Filzbach for the joint posterior
  thin: int;                          // MCMC chain is "thinned" to remove autocorrelation. Every nth sample is retained
  noise_model: noise_model;           // The noise model describes the distribution of model-data deviations (see noise_model type)
  noise_parameter: noise_parameter;   // The noise parameter is associated with the noise model (see noise parameter type)
  prune: bool;                        // Unused reactions and species can be trimmed from the model, for efficiency sake (was useful in old Modelling Engine, but should be redundant here)
  seed: uint32;                       // Seed for the random number generator to be used in Filzbach
  parallel_computation: bool;         // Allow parallel likelihood computation
  timer: bool;                        // Keep track of simulation times
  partial_evaluation: bool;           // Use partial evaluation of the likelihood
}

val string_of_noise_model : noise_model -> string
val defaults : t
val to_string : t -> string
val parse : Parser.t<t>