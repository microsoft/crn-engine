module Microsoft.Research.DNA.Syntax
open Microsoft.Research.DNA

//module Settings = Microsoft.Research.CRNEngine.Settings
//module SettingsConstants = Microsoft.Research.CRNEngine.SettingsConstants
//module Plottable = Microsoft.Research.CRNEngine.Plottable
//module ParseResult = Microsoft.Research.CRNEngine.ParseResult
module Expressions = Microsoft.Research.CRNEngine.Expression

type value = Value.t
type value_env = Value.t_env
type dom_assignment = (* add bind and unbind here *)
  { domain : string
  ; dom_colour : string option
  ; subdomains : (string list * bool) option } (* subdomains, is_short *)
type dom_env = dom_assignment list
type proc = Process.t
type proc_env = Process.t_env
type spec = Process.dom_data
type parameter = Pattern.t
type species = Species.t
(* The type of DSD plot specifications, listing items to monitor *)
type plot_pat =
  | All
  | Pats of value list (* value list may contain wildcards denoted with _ *)
type t_plot_base = 
  | String of string
  | Molecule of Species.t 
  | Module of string * plot_pat
type t_plot = t_plot_base Expressions.t

val t_plot_to_expr : t_plot -> proc Expressions.t

type t = 
  | New of ((string * (string * spec) list) * bool * t)
  | Definition of (string * Types.range * parameter list * proc * t)
  | Value of (string * value * t)
  | Process of proc
(*
  | Predicate of string * t_plot_base Expressions.pred * t
  | Query of string * (species ParseResult.settings_update list option * t_plot_base Expressions.pred) * t
*)

(*
  t_plots have to be turned into plottables after parsing
  when the environment (declared domains) is known, so we
  gather them in this structure during parsing.
*)
type event =
  | Ev_species of Process.t * value * value (* species, amount, time *)
  //| Ev_param of SettingsConstants.filzparam * value * value (* parameter, new value, time *)
  | Ev_endtime of value

type parsed_plots =
  { plottables : t_plot list
  ; sweep_fits : (string * string * t_plot list) list
  ; events : event list }
  (* here we could add spatial plots *)
val empty_parsed_plots : parsed_plots

val expand : Options.t -> Types.type_env -> t_plot_base list -> t -> (int * string list * proc * proc_env * value_env * dom_env (* * (string * t_plot_base Expressions.pred) list * (string * (species ParseResult.settings_update list option * t_plot_base Expressions.pred)) list*))

(* Functions which communicate plot information between the creation of species and the Term *)
val t_plot_base_to_species : (Process.t -> Species.t_map list) -> t_plot_base -> species
val plot_map : (Process.t -> Species.t_map list) -> t_plot -> species Expressions.t
val extractModules : t_plot list -> t_plot list * t_plot_base list
val add_to_plots : Species.t_env -> t_plot list -> t_plot list

(* This is the return type of the parser. I've put it here as a hack... *)
//type parser_return_type = (Options.t * parsed_plots * species Settings.settings) -> ((Options.t * parsed_plots * species Settings.settings) * t)

val display : t -> string