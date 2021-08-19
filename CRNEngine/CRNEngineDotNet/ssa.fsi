[<JavaScript>] 
module Microsoft.Research.CRNEngine.Ssa

type sim_settings = Simulation_settings<Functional>
type simulator = Simulation.t<Functional>
type value = Expression.t<string>
type population = Population<Species,float> 
type populations = Populations<Species,float> 
type event = Event<Species,float,float>
type expression = Expression.t<int>
type lambda = Lambda<Inlined<int>> 
type reaction = Reaction<int,float,lambda>
type reaction_info = { 
  reaction: reaction;
  propensity: float;
  dependencies: int list; (* ids of reactions whose reactants are mentioned anywhere in this one (see calculate_reaction_deps) *)
}
type reactions = Hashtable.t<int, reaction_info> 
type t = { 
  simulator:simulator;
  plots: Expression.t<Inlined<int>> array
  settings: Stochastic_settings;
  random: Rng.Random;
  reactions: reactions;
  total_propensity: float; 
}
type table = Table<float>
type cancel = bool ref
type row = Row<float>
type output = row -> unit
type updater = float -> float -> populations -> unit



val empty: Unit -> t
val create: simulator 
              -> Stochastic_settings 
              -> reactions 
              -> float 
              -> Parameter list 
              -> Map<string, Expression.t<Inlined<Species>>>
              -> t
val make_sim_reactions_pops: Environment.t 
                              -> Map<string, Expression.t<Inlined<Species>>>
                              -> float 
                              -> Reaction<Species,value,Functional> list 
                              -> Populations<Species,float>  
                              -> reactions * int * float
val simulate_callback: cancel -> output -> Option<updater> -> t -> t
//AP//val simulate : ssa:t -> t * table
val simulate : ssa:t -> table
val simulate_with_stationary :  t -> t * table * Map<string,double []>
val set_number_of_points :  int -> t -> t

