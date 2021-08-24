// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Ode

type key = Key<Species>
type expression = Expression.t<key>
type sim_settings = Simulation_settings<Functional>
type populations = Populations<Species,float>
type event = Event<Species,float,float>
type initial = Initial<Species,Value>
type rate = Rate<Value,expression>
type reaction = Reaction<Species,Value,expression>

type t = { 
  name: string;
  settings: Ode_settings<Functional>;
  reactions: reaction list;
  initials: initial list;
}

type cancel = bool ref
type output<'p> = Row<'p> -> unit

val empty : t
val substitute : Environment.t -> t -> t
//val create : string -> Ode_settings<Species> -> float [] [] -> (int * float) list array -> rate array -> Map<Species,int> -> initial array -> (Environment.t -> Reaction<int,float,Expression.lambda<int>> list) -> t
val create : string -> reaction list -> initial list -> Ode_settings<Functional> -> t
val get_instances : t -> Instance list
val get_inference_parameters : t -> Parameter list
val to_matlab : ode:t -> string

val get_matrices : initial list -> Reaction<Species,'a,Functional> list -> Map<Species,int> * float [] [] * (int * float) list []
val get_rates : Map<Species,int> -> Reaction<Species,Value,Functional> list -> convert_to_stochastic:bool -> Rate<Value,Expression.t<Key<int>>> []
val to_oslo : t -> OdeOslo.t
#if JavaScript
#else
val to_sundials : t -> OdeSundials.t
#endif
val to_lna : float -> t -> Lna.t
val to_inference_oslo : t -> Inference.t
val to_inference_sundials : t -> Inference.t
val simulate_oslo : t -> Result<float> list
val simulate_oslo_single : t -> OdeOslo.evaluated * Table<float>
val simulate_oslo_callback : cancel -> output<float> -> t -> OdeOslo.evaluated
val simulate_lna : t -> Result<Point> list
val simulate_lna_single : t -> Lna.t * Table<Point>
val simulate_lna_callback : cancel -> output<Point> -> t -> Lna.t
val simulate_sundials : t -> Result<float> list
val simulate_sundials_single : t -> Table<float>
val simulate_list : (t -> Table<'v>) -> Ode_settings<Functional> -> t list -> Result<'v> list
//val infer_oslo : t -> Inference.mcmc_result
//val infer_seq_oslo : t -> seq<Inference.mcmc_intermediate_result>
val infer_oslo :  Ode_settings<Functional> -> t list -> Inference.mcmc_result
//val infer_sundials : t -> Inference.mcmc_result
//val infer_seq_sundials : t -> seq<Inference.mcmc_intermediate_result>
val infer_sundials :  Ode_settings<Functional> -> t list -> Inference.mcmc_result