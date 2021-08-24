// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Inference
open Microsoft.Research.Filzbach

type table = Table<float>
type result = Result<float>
type full = {
  likefn: DataStructures.AssociativeArray<float> -> float;
  simfn:  bool -> DataStructures.AssociativeArray<float> -> result list;
  parameters: Parameters.Parameter list;
  options: Filzbach.RunOptions;
  numData: int;
}
type partial = {
  likefn: Parameters.Ids -> DataStructures.AssociativeArray<float> -> float list;
  simfn:  bool -> Parameters.Ids -> DataStructures.AssociativeArray<float> -> result list;
  parameters: Parameters.Parameter list;
  dependencies: Parameters.Ids list;
  options: Filzbach.RunOptions;
  numData: int;
}
type t = Full of full | Partial of partial
type McmcSummary = {
  seed : uint32
  burnin : int
  samples : int
  thin : int
  mle : float
  aic : float
  bic : float
  dic : float
  numData : int
}
with 
  static member defaults : McmcSummary
  member to_string : unit -> string
  static member from_string : (string -> McmcSummary)
  static member parse : Parser.t<McmcSummary>
type Summary = { mcmc: McmcSummary; parameters: Map<string,ParameterSummary> }
with
  member to_string : unit -> string 
  static member parse : Parser.t<Summary>
  static member from_string : (string -> Summary)
  static member combine : float option -> uint32 list -> Map<uint32,Summary> -> Summary
   
type mcmc_result = {
  parameters: ParameterSummary list
  posterior: Map<string,float> list
  burnin: Map<string,float> list
  pfixed: Map<string,float> 
  mle: Map<string,float>
  mlesims: result list
  postsims: result list
  summary: McmcSummary
}
with member to_summary : unit -> Summary
type mcmc_intermediate_result = {
  iteration: int;
  state: Filzbach.RunPhase;
  mlesims: result list;
  summary: string
}

val create : Inference_settings -> (bool -> Environment.t -> result list) -> Functional list list -> table list -> Parameter list -> t
val create_with_dependencies : Inference_settings -> (bool -> Environment.t -> Parameters.Ids -> result list) -> Functional list list -> table list -> Parameter list -> Parameters.Ids list -> t
val env_of_aa : DataStructures.AssociativeArray<float> -> Environment.t -> Environment.t
val run_mcmc : t -> mcmc_result
val run_mcmc_seq : t -> (mcmc_result->unit) -> seq<mcmc_intermediate_result>
val analyse_chain : Filzbach.Sampling -> Map<string,float> list * Map<string,float> * ParameterSummary list * Map<string,float> list
val getSummary : Filzbach.RunOptions -> int -> Filzbach.Sampling -> McmcSummary
type sample_prior_result = {
  loglikelihood: float;
  parameters: Environment.t;
  simulations: result list
}
val prior_prediction : int -> Inference_settings -> (bool -> Environment.t -> result list) -> Functional list list -> table list -> Parameter list -> sample_prior_result list
