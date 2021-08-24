// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Crn 
open WebSharper

type prior = Prior.t
type table = Table.t<float>
type key = Key.t<Species>
type expression = Expression.t<key>
type value = Expression.t<string>
type plottable = Expression.t<Key.t<Species>>
type reaction = Reaction<Species,value,expression>
type rate = Rate.t<value,expression>
type initial = Initial<Species, value>
type population = Population.t<Species,float>
type populations = Populations.t<Species,float>
type event = Event.t<Species,float,float>
type ode_settings = Ode_settings.t<Species>

type species_attributes = { 
  name: string;
  structure: string;
  svg: string;
}

type gui = { 
  name: string;
  settings: Crn_settings.gui;
  reactions: Reaction<string,string,string> list;
  initials: Initial<string,string> list;
  species_attributes: Stringmap.t<species_attributes>
}
type t = { 
  name:string;
  settings: Crn_settings.t<Key.t<Species>>;
  reactions: reaction list;
  initials: initial list;
  species_attributes: Stringmap.t<species_attributes>
}
type cancel = (bool ref) 
type output<'p> = Row.t<'p> -> unit
type svg = Svg.t
type sbml = Sbml.t

val empty : t
val create : string -> Crn_settings.t<Key.t<Species>> -> reaction list -> initial list -> Stringmap.t<species_attributes> -> t
val compute_reversibles : (Species -> Species -> bool) -> t -> reaction list
val from_calculus_translated : ('s -> Species) ->          // maps a species to a CRN species
                               ('s -> species_attributes) -> // gets the species attributes for a species
                               ('s -> 's -> bool) ->         // species equality
                               ('s -> 's -> bool) ->         // plot matching
                               string ->                     // resulting CRN's name
                               Crn_settings.t<Key.t<'s>> ->  // settings
                               Calculus.t<'s> ->             // generates CRN reactions and species
                               Initial<'s, value> list ->  // initial initials
                               Ctmc.reaction<'s> list ->     // initial reactions
                               t when 's:comparison
val update_settings : Crn_settings.t<Key.t<Species>> -> t -> t
val update_times : times:float list -> crn:t -> t
val substitute : Environment.t -> t -> t
val get_instances : t -> Instance.t list
val initials_to_svg : t -> svg
val reactions_to_svg : t -> svg
val to_svg : t -> svg
val default_svg_style : string
val to_sbml : t -> sbml
val to_matlab : t -> string
val to_ssa : t -> Ssa.t
val to_ode : t -> Ode.t
val to_gui : t -> gui
val from_gui : gui -> t
val to_ctmc : t -> Ctmc.ctmc_result<Species>
val to_cme : t -> Cme.t
val to_inference : t -> Inference.t
val group_reactions : t -> t
val saturate_initials : t -> t
val simulate_ssa : t -> Result.t<float> list
val simulate_ssa_single : t -> Ssa.t * Table.t<float>
val simulate_ssa_callback : cancel -> output<float> -> t -> Ssa.t
val simulate_pde_1d : t -> float list * Table.t<float[]>
val simulate_pde_1d_callback : cancel -> output<float[]> -> t -> unit
val simulate_pde_2d : t -> float list * Table.t<float[][]>
val simulate_pde_2d_callback : cancel -> output<float[][]> -> t -> unit
val simulate_oslo : t -> Result.t<float> list
val simulate_oslo_single : t -> OdeOslo.evaluated * Table.t<float>
val simulate_oslo_callback : cancel -> output<float> -> t -> OdeOslo.evaluated
val simulate_lna : t -> Result.t<Point.t> list
val simulate_lna_single : t -> Lna.t * Table.t<Point.t>
val simulate_lna_callback : cancel -> output<Point.t> -> t -> Lna.t
val simulate_cme : t -> Result.t<Point.t> list
val simulate_cme_single : t -> Cme.t * Table.t<Point.t>
val simulate_cme_callback : cancel -> output<Point.t> -> t -> Cme.t
val simulate_sundials : t -> Result.t<float> list
val simulate_sundials_single : t -> Table.t<float>
val simulate : t -> Result.t<float> list
val infer_oslo : t -> Inference.mcmc_result
val infer_seq_oslo : t -> seq<Inference.mcmc_intermediate_result>
val infer_list_oslo : Crn_settings.t<Key.t<Species>> -> t list -> Inference.mcmc_result
val infer_sundials : t -> Inference.mcmc_result
val infer_seq_sundials : t -> seq<Inference.mcmc_intermediate_result>
val infer_list_sundials : Crn_settings.t<Key.t<Species>> -> t list -> Inference.mcmc_result
val infer : t -> Inference.mcmc_result
val to_string : t -> string
val map : (Species -> Species) -> t -> t
val all_species : t -> Species list

(* CRN parsers *)
val parse            : Parser.t<t>
val parseWithSpecies : Parser.t<Species> -> Parser.t<t>
val parse_defaults   : Crn_settings.t<Key.t<Species>> -> Parser.t<t>
val from_string      : string -> t
val create_blank_attributes : t -> t

val simulate_2d_test_callback : cancel -> output<float[][]> -> t -> unit