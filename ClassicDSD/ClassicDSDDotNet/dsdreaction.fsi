// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.Dsdreaction 
open Microsoft.Research.DNA 
open Microsoft.Research.CRNEngine

type value = Expression.t<string> 
type species = Microsoft.Research.DNA.Species.t
type options = Options.t
type species_env = Species.t_env
type sreaction = Reaction<species, value, Expression.t<Key<species>>>
type settings = Crn_settings<Expression.t<Key<species>>>
type 'species crn_rate when 'species:equality =
  | Crn_ma of value
  | Crn_func of Expression.t<Key<'species>>

type 'species mass_action_reaction when 'species:equality =
  { ma_reactants:   'species Mset.t
  ; ma_products:    'species Mset.t
  ; ma_catalysts:   'species Mset.t
  ; ma_rate:        'species crn_rate
  ; ma_reverseRate: 'species crn_rate option }

val map_mass_action_reaction : ('a -> 'b) -> 'a mass_action_reaction -> 'b mass_action_reaction

type t
val reactants : t -> species list
val products : t -> species list
val species : t -> species list
val rate : settings -> options -> Sequence.mapping -> t -> species crn_rate
val is_leak : t -> bool
val is_fast_leak : t -> bool
val is_slow_leak : t -> bool
val is_tau : t -> bool
val looks_like_leak : options -> sreaction -> bool
val reversibles : options -> t list -> (t * t option) list
val display_reversibles : settings -> options -> (sreaction * sreaction option) list -> string
val to_dot_reversibles : settings -> options -> (sreaction * sreaction option) list -> string
val translate : settings -> options -> Sequence.mapping -> t -> sreaction
val translate_reversible : settings -> options -> Sequence.mapping -> (t * t option) -> sreaction

val add_new_species_reactions: Domain.sub_map -> settings -> options -> Sequence.mapping -> species -> species list -> species_env -> sreaction list -> sreaction list -> sreaction list * species list * species_env
val new_reactions: Domain.sub_map -> options -> species -> species list -> sreaction list -> t list
val new_species_env: options -> species list -> species_env -> species_env
(******************************************************************************)
