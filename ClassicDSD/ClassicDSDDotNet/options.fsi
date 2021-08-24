// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.Options
open Microsoft.Research.DNA

open Microsoft.Research.DNA.Measures

//#use measures.ml
open Parser

type parser<'a> = Parser.t<'a>
type option_piece

type prim = Microsoft.Research.CRNEngine.Expression.t<string>

type semantics =
  | [<WebSharper.Constant("Infinite")>] Infinite
  | [<WebSharper.Constant("Default")>] Default
  | [<WebSharper.Constant("Finite")>] Finite
  | [<WebSharper.Constant("Detailed")>] Detailed
type generate_predicates =
  | [<WebSharper.Constant("All_predicates")>] All_predicates
  | [<WebSharper.Constant("No_predicates")>] No_predicates

///Species rendering modes

type renderer =
  | [<WebSharper.Constant("No_predicates")>] Classic
  | [<WebSharper.Constant("Circles")>] Circles
  | [<WebSharper.Constant("Branches")>] Branches
///Determines how branches are arranged.
type arrange_mode =
  | [<WebSharper.Constant("Wide")>] Wide
  | [<WebSharper.Constant("Babylon")>] Babylon
type renderer_mode =
  | [<WebSharper.Constant("Complement")>] Complement
  | [<WebSharper.Constant("Condensed")>] Condensed
  | [<WebSharper.Constant("Nucleotides")>] Nucleotides
type rendering = {
    renderer: renderer
    mode: renderer_mode
    rotate_labels: bool
    arrange: arrange_mode
}

(* Main type for DSD-specific options. *)
type t = { (* These are from the UI. *)
           rules : semantics;
           leaks : bool;
           pin_leaks : bool;
           polymers : bool;
           unproductive : bool;
           sequence_rates : bool;
           temperature : float<C>;
           stabilityCorrection : float<kcal/mol>;
           coaxialDangle : float<kcal/mol>;
           doubleCoaxialDangle : float<kcal/mol>;
           terminalDangleFactor : float;
           coaxialCorrection : prim;
           declare_domains : bool;
           check_dna : bool;
           colour_toeholds : bool;
           program : string;
           toeholds : string;
           specificities : string;
           plot_names : bool;
           state_images : bool;
           (* These are from the directives. *)
           toehold_bind_rate:float;
           toehold_unbind_rate:float;
           leak_rate_l:prim;
           leak_rate_w:prim;
           pinleak_rate:prim option;
           tau_rate:prim;
           elementary_migration_rate:prim;
           toehold_length:int;
           specificity_length:int;
           local_concentrations:(string * prim) list;
           generate_predicates: generate_predicates;
           rendering: rendering
           is_jit: bool
           rulesProgram: Map<(string * int), Set<RulesDSD.Syntax.Clause<Microsoft.Research.DNA.LogicDSD.SiteT>>> option}

(* Default values of DSD-specific options. *)
val default_matching_degree : float
val default_toehold_bind_rate : float
val default_toehold_unbind_rate : float
val default_leak_rate_l : float
val default_leak_rate_w : float
val default_tau_rate : float
val default_toehold_length : int
val default_specificity_length : int
val default_elementary_migration_rate : float

val default_options : t
val keep_ui_options : t -> t

val getColourToeholds : t -> bool
val setColourToeholds : bool -> t -> t
val getRules : t -> semantics
val setRules : semantics -> t -> t
val getLeaks : t -> bool
val setLeaks : bool -> t -> t
val getPinLeaks : t -> bool
val setPinLeaks : bool -> t -> t
val getUnproductive : t -> bool
val setUnproductive : bool -> t -> t
val getSequenceRates : t -> bool
val setSequenceRates : bool -> t -> t

val getStabilityCorrection : t -> float<kcal/mol>
val setStabilityCorrection : float -> t -> t
val getCoaxialDangle :  t -> float<kcal/mol>
val setCoaxialDangle :  float ->  t  ->  t

val getDoubleCoaxialDangle :  t -> float<kcal/mol>
val setDoubleCoaxialDangle :  float ->  t  ->  t

val getCoaxialCorrection :  t -> prim
val setCoaxialCorrection :  prim -> t -> t

val getTemperature : t -> float<C>
val setTemperature : t -> float -> t
val getTerminalDangleFactor: t -> float
val setTerminalDangleFactor: t -> float -> t

val getDeclareDomains : t -> bool
val setDeclareDomains : bool -> t -> t
val getCheckDNA : t -> bool
val setCheckDNA : bool -> t -> t
val getProgramText : t -> string
val setProgramText : string -> t -> t
val getToeholdsText : t -> string
val setToeholdsText : string -> t -> t
val getSpecificitiesText : t -> string
val setSpecificitiesText : string -> t -> t
val getPolymers : t -> bool
val setPolymers : bool -> t -> t
val getRendering : t -> rendering
val setRendering : rendering -> t -> t
val getPlotNames : t -> bool
val setPlotNames : bool -> t -> t
val getStateImages : t -> bool
val setStateImages : bool -> t -> t

val get_leak_rate_l : t -> prim
val set_leak_rate_l : prim -> t -> t
val get_leak_rate_w : t -> prim
val set_leak_rate_w : prim -> t -> t
val get_tau_rate : t -> prim
val get_pinleak_rate : t -> prim option
val set_pinleak_rate : prim option -> t -> t
val set_tau_rate : prim -> t -> t
val get_elementary_migration_rate : t -> prim
val set_elementary_migration_rate : prim -> t -> t
val get_toehold_length : t -> int
val set_toehold_length : int -> t -> t
val get_specificity_length : t -> int
val set_specificity_length : int -> t -> t
val get_toehold_bind_rate : t -> float
val set_toehold_bind_rate : float -> t -> t
val get_toehold_unbind_rate : t -> float
val set_toehold_unbind_rate : float -> t -> t

val set_local_concentrations : (string * prim) list -> t -> t
val get_local_concentrations : t -> (string * prim) list
val get_local_concentration : t -> string -> prim

val get_generate_predicates : t -> generate_predicates
val set_generate_predicates : generate_predicates -> t -> t

val get_is_jit : t -> bool
val set_is_jit : bool -> t -> t
// CS: a Rules DSD program is stored as a Map instead of a Dictionary 
//     because W# doesn't support non-string keys in Dictionary (as of 11/04/2018).
val get_rules_program : t -> Map<(string * int), Set<RulesDSD.Syntax.Clause<Microsoft.Research.DNA.LogicDSD.SiteT>>> option
val set_rules_program : Map<(string * int), Set<RulesDSD.Syntax.Clause<Microsoft.Research.DNA.LogicDSD.SiteT>>> option -> t -> t

val optionParsers  : parser<option_piece list> list
val parse_option   : parser<option_piece list>
val update_options : t -> option_piece -> t

val display : t -> string