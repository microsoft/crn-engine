module Microsoft.Research.DNA.Dsd
open Microsoft.Research.DNA

open Microsoft.Research.CRNEngine

type term = Crn
val compile : string -> term
val compile_extended : string -> string -> string -> (term*Task option)

type metadata =
  { meta : Sequence.mapping
  ; info : Species.t_env
  ; domain_colours : Hashtable.t<string,string>
  ; subdomains : Hashtable.t<string list, Domain.t> }

type ClassicBundle = 
    { settings   : Crn_settings<Expression.t<Key<Species.t>>>
    ; meta       : metadata
    ; sreactions : Process.sreaction list
    ; initials   : Initial<Species.t, Species.prim> list 
    ; dsdOptions : Options.t}

type RulesBundle =    
    { settings   : Crn_settings<Expression.t<Key< RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>> >>
    ; initials   : Initial<RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>, Microsoft.Research.CRNEngine.Expression.t<string>> list 
    ; reactions  : Reaction<RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>
                           , Expression.t<string>
                           , Expression.t<Key<RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>>>> list
    ; plotsCache : System.Collections.Generic.Dictionary<RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>, string>
    ; dsdOptions : Options.t
    ; rules      : RulesDSD.Syntax.RulesProgram<Microsoft.Research.DNA.LogicDSD.SiteT> }

type bundle =
  | ClassicDSD of ClassicBundle
  | Rules   of RulesBundle

val get_options : bundle -> Options.t
val set_options : bundle -> Options.t -> bundle

val parse    : string -> bundle
val parse_extended : string -> string -> string -> (bundle*Task option)
val convert_unexpanded : bundle -> term
val convert_expand : bundle -> term

val makeDsdCalculus : b:bundle -> Microsoft.Research.CRNEngine.Calculus<Species.t>
val to_rules_calculus : b:bundle -> Microsoft.Research.CRNEngine.Calculus<RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>>
val to_jit          : b:bundle -> Choice<Jit.t<Species.t>, Jit.t<RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>>>
val is_jit          : b:bundle -> bool
val update_bundle_after_jit : bundle -> Microsoft.Research.CRNEngine.Jit.t<Species.t> -> bundle

val default_namer: opts:Options.t -> species_env:Species.t_env -> (Species.t -> string)
val get_species_attributes: (Species.t -> string) -> ClassicBundle -> (Species.t -> Attributes)