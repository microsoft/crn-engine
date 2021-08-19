[<JavaScript>]
module Microsoft.Research.DNA.Options

open Microsoft.Research.DNA
open Microsoft.Research.DNA.Measures

module Lib  = Microsoft.Research.CRNEngine.Lib
module E    = Microsoft.Research.CRNEngine.Expression

open Parser
type parser<'a> = Parser.t<'a>

type prim = Microsoft.Research.CRNEngine.Expression.t<string>

(* We currently have four core reaction-level semantics to pick from. *)
type semantics =
  | [<WebSharper.Constant("Infinite")>] Infinite
  | [<WebSharper.Constant("Default")>] Default
  | [<WebSharper.Constant("Finite")>] Finite
  | [<WebSharper.Constant("Detailed")>] Detailed
type generate_predicates =
  | [<WebSharper.Constant("All_predicates")>] All_predicates
  | [<WebSharper.Constant("No_predicates")>] No_predicates

///Species rendering modes.
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
let default_rendering = {renderer=Branches;mode=Complement;rotate_labels=false;arrange=Babylon}

(* Type for all DSD-specific options. *)
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

(* DSD-specific constants. *)
let default_matching_degree = 1.0 (* Deprecated *)
let default_toehold_bind_rate = 3.0E-4 (* /nM/s *)
let default_toehold_unbind_rate = 0.1126 (* /s *)
let default_leak_rate_l = 1.0E-9 (* /nM/s *)
let default_leak_rate_w = default_leak_rate_l / 100.0 (* /M/s Currently not used, since "slow leaks" are disabled. *)
let default_tau_rate = default_toehold_unbind_rate
let default_toehold_length = 6
let default_specificity_length = 20
let default_elementary_migration_rate =  1.0 / (125E-6) (* /s elementary step time scale of 125 /s/step *) 

let default_terminalDangleFactor = 1.0 // no units. 1.0 = full terminal dangles, 0.0 = no terminal dangle terms. 
let default_coaxialCorrection = 0.0 //<kcal/mol>
let default_stabilityCorrection = 0.0<kcal/mol>
let default_coaxialDangle = 2.0<kcal/mol>
let default_doubleCoaxialDangle = 3.2<kcal/mol>

(* Default DSD-specific options. *)
let default_options = { rules = Default;
                        leaks = false;
                        pin_leaks = false;
                        polymers = false;
                        unproductive = false;
                        sequence_rates = false;
                        temperature = 23.0<C>;
                        stabilityCorrection = default_stabilityCorrection;
                        coaxialDangle = default_coaxialDangle;
                        doubleCoaxialDangle = default_doubleCoaxialDangle;
                        terminalDangleFactor = default_terminalDangleFactor;
                        coaxialCorrection = prim.Float default_coaxialCorrection;
                        declare_domains = false;
                        check_dna = true;
                        colour_toeholds = true;
                        plot_names = true;
                        rendering = default_rendering;
                        state_images = true;
                        program = "";         
                        toeholds = DefaultDatabase.toeholds;
                        specificities = DefaultDatabase.specificities;
                        toehold_bind_rate = default_toehold_bind_rate;
                        toehold_unbind_rate = default_toehold_unbind_rate;
                        leak_rate_l = prim.Float default_leak_rate_l;
                        leak_rate_w = prim.Float default_leak_rate_w;
                        pinleak_rate = None;
                        tau_rate = prim.Float default_tau_rate;
                        elementary_migration_rate = prim.Float default_elementary_migration_rate;
                        toehold_length = default_toehold_length;
                        specificity_length = default_specificity_length;
                        local_concentrations = [];
                        generate_predicates = No_predicates;
                        is_jit = false
                        rulesProgram = None}

type option_piece =  (* A single option; a list of option_piece's can form an Option.t object. *)
      | Rules           of semantics
      | Leaks           
      | PinLeaks        
      | Polymers        
      | Unproductive    
      | SequenceRates   
      | DeclareDomains  
      | CheckDna        
      | ColourToeholds  
      | Program         of string
      | Toeholds        of string
      | Specificities   of string
      | Rendering       of rendering
      | PlotNames       
      | StateImages     
        (* These are from the directives. *)
      | ToeholdBindRate            of float
      | ToeholdUnbindRate          of float
      | LeakRateL                  of prim
      | LeakRateW                  of prim
      | PinLeakRate                of prim option
      | TauRate                    of prim
      | ElementaryMigrationRate    of prim
      | ToeholdLength              of int
      | SpecificityLength          of int
      | LocalConcentrations        of (string * prim) list
      | GeneratePredicates         of generate_predicates 
      (************************************)
      | Temperature                of float<C>
      | StabilityCorrection        of float<kcal/mol>
      | CoaxialDangle              of float<kcal/mol>
      | DoubleCoaxialDangle        of float<kcal/mol>
      | TerminalDangleFactor       of float
      | CoaxialCorrection          of prim
      | IsJit                      of bool
      | IsRules                    of Map<(string * int), Set<RulesDSD.Syntax.Clause<Microsoft.Research.DNA.LogicDSD.SiteT>>> 

(* Keep the UI settings and erase those that are set in the program text (i.e. reset them to defaults). *)
let keep_ui_options (opts:t) = { default_options with check_dna = opts.check_dna;
                                                      colour_toeholds = opts.colour_toeholds;
                                                      rendering = opts.rendering;
                                                      plot_names = opts.plot_names;
                                                      state_images = opts.state_images;
                                                      program = opts.program;
                                                      toeholds = opts.toeholds;
                                                      specificities = opts.specificities;
                                                      local_concentrations = opts.local_concentrations }
(*
let keep_ui_options (opts:t) = { opts with toehold_bind_rate = default_toehold_bind_rate;
                                           toehold_unbind_rate = default_toehold_unbind_rate;
                                           leak_rate_l = default_leak_rate_l;
                                           leak_rate_w = default_leak_rate_w;
                                           tau_rate = default_tau_rate;
                                           elementary_migration_rate = default_elementary_migration_rate;
                                           toehold_length = default_toehold_length;
                                           specificity_length = default_specificity_length }
                                           *)
(******************************************************************************)
(* *** Get/set DSD-specific options. *)

let getColourToeholds (opts:t) = opts.colour_toeholds
let setColourToeholds (b:bool) (opts:t) = { opts with colour_toeholds=b }
let getRules (opts:t) = opts.rules
let setRules (s:semantics) (opts:t) = { opts with rules=s }
let getLeaks (opts:t) = opts.leaks
let setLeaks (b:bool) (opts:t) = { opts with leaks=b }
let getPinLeaks (opts:t) = opts.pin_leaks
let setPinLeaks (b:bool) (opts:t) = { opts with pin_leaks=b }
let getUnproductive (opts:t) = opts.unproductive
let setUnproductive (b:bool) (opts:t) = { opts with unproductive = b }
let getSequenceRates (opts:t) = opts.sequence_rates
let setSequenceRates (b:bool) (opts:t) = { opts with sequence_rates = b }

let getStabilityCorrection (opts:t) = opts.stabilityCorrection
let setStabilityCorrection (input : float) (opts:t) =  {opts with stabilityCorrection = (input * 1.0<kcal/mol>) }
let getCoaxialDangle (opts:t) = opts.coaxialDangle
let setCoaxialDangle (input : float) (opts:t) = {opts with coaxialDangle = (input * 1.0<kcal/mol>) }
let getDoubleCoaxialDangle (opts:t) = opts.doubleCoaxialDangle
let setDoubleCoaxialDangle (input : float) (opts:t) = {opts with doubleCoaxialDangle = (input * 1.0<kcal/mol>) }

(* 5' (phosphate) vs 3' (hydroxl) penalty *)
let getCoaxialCorrection (opts:t) = opts.coaxialCorrection
let setCoaxialCorrection (input:prim) (opts:t) = {opts with coaxialCorrection =(  input  )  }

let getTemperature (opts:t) = opts.temperature
let setTemperature (opts:t) (input:float) = { opts with temperature = input * 1.0<C>}
let getTerminalDangleFactor (opts:t) = opts.terminalDangleFactor
let setTerminalDangleFactor (opts:t) (input:float) = {opts with terminalDangleFactor = input}

let getDeclareDomains(opts:t) = opts.declare_domains
let setDeclareDomains (b:bool) (opts:t) = { opts with declare_domains = b }
let getCheckDNA (opts:t) = opts.check_dna
let setCheckDNA (b:bool) (opts:t) = { opts with check_dna = b }
let getProgramText (opts:t) = opts.program
let setProgramText (s:string) (opts:t) = { opts with program = s }
let getToeholdsText (opts:t) = opts.toeholds
let setToeholdsText (s:string) (opts:t) = { opts with toeholds = s }
let getSpecificitiesText (opts:t) = opts.specificities
let setSpecificitiesText (s:string) (opts:t) = { opts with specificities = s }
let getPolymers (opts:t) = opts.polymers
let setPolymers (b:bool) (opts:t) = { opts with polymers = b }
let getRendering (opts:t) = opts.rendering
let setRendering m opts = {opts with rendering = m}
let getPlotNames (opts:t) = opts.plot_names
let setPlotNames (b:bool) (opts:t) = { opts with plot_names = b }
let getStateImages (opts:t) = opts.state_images
let setStateImages (b:bool) (opts:t) = { opts with state_images = b }

(* Leak rates. *)
let get_leak_rate_l (opts:t) = opts.leak_rate_l
let get_leak_rate_w (opts:t) = opts.leak_rate_w
let set_leak_rate_l (l:prim) (opts:t) = { opts with leak_rate_l = l } (* Check commented out as not using slow leaks, so don't want to restrict fast leak rate *)
  (*let w = opts.leak_rate_w in
  if (w < l) then {opts with leak_rate_l=l}
  else Errors.leak_rates_error l w*)
let set_leak_rate_w (w:prim) (opts:t) = { opts with leak_rate_w = w } (* Check commented out as not using slow leaks, so don't want to restrict fast leak rate *)
  (*let l = opts.leak_rate_l in
  if (w < l) then {opts with leak_rate_w=w}
  else Errors.leak_rates_error l w*)

let get_pinleak_rate (opts:t) = opts.pinleak_rate
let set_pinleak_rate (l:prim option) (opts:t) = { opts with pinleak_rate = l }


(* Tau reaction rate. *)
let get_tau_rate (opts:t) = opts.tau_rate
let set_tau_rate (r:prim) (opts:t) = {opts with tau_rate=r}

(* Elementary migration rate. *)
let get_elementary_migration_rate (opts:t) = opts.elementary_migration_rate
let set_elementary_migration_rate (r:prim) (opts:t) = {opts with elementary_migration_rate=r}

(* Toehold domain length. *)
let get_toehold_length (opts:t) = opts.toehold_length
let set_toehold_length (thlen:int) (opts:t) =
  let splen = opts.specificity_length in
  if (thlen < splen) then {opts with toehold_length=thlen}
  else Errors.domain_lengths_error thlen splen

(* Specificity domain length. *)
let get_specificity_length (opts:t) = opts.specificity_length
let set_specificity_length (splen:int) (opts:t) =
  let thlen = opts.toehold_length in
  if (thlen < splen) then {opts with specificity_length=splen}
  else Errors.domain_lengths_error thlen splen

(* Toehold rates. *)
let get_toehold_bind_rate (opts:t) = opts.toehold_bind_rate
let set_toehold_bind_rate (r:float) (opts:t) = {opts with toehold_bind_rate=r}
let get_toehold_unbind_rate (opts:t) = opts.toehold_unbind_rate
let set_toehold_unbind_rate (r:float) (opts:t) = {opts with toehold_unbind_rate=r}

let set_local_concentrations lcs opts = {opts with local_concentrations = lcs}
let get_local_concentrations opts = opts.local_concentrations
let get_local_concentration opts tag =
  match Lib.try_assoc tag opts.local_concentrations with
  | Some lc -> lc
  | None -> failwith (sprintf "The local concentration of %s is not defined" tag)

let get_generate_predicates (opts:t) = opts.generate_predicates
let set_generate_predicates g opts = {opts with generate_predicates = g}

let get_is_jit (opts : t) : bool = opts.is_jit
let set_is_jit (b : bool) (opts : t) = {opts with is_jit = b}

let get_rules_program (opts : t) : Map<(string * int), Set<RulesDSD.Syntax.Clause<Microsoft.Research.DNA.LogicDSD.SiteT>>> option      = opts.rulesProgram
let set_rules_program (r : Map<(string * int), Set<RulesDSD.Syntax.Clause<Microsoft.Research.DNA.LogicDSD.SiteT>>> option) (opts : t)  = {opts with rulesProgram = r}

(* parses an "option" directive and returns corresponding option_piece's *)
(* TODO: some things look weird, maybe they have to be fixed somehow?
         - the parser takes "toeholds" followed by 2 floats; the Options structure has 3 fields related to toeholds, one string field "toeholds" which does not seem to be in use in the code base, and 2 float fields "toehold_bind_rate" and "toehold_unbind_rate"
         - the parser takes "leak" followed by one value, and put is into "leak_rate_l". There is another field "leak_rate_w" which is not utilzed.
         *)
let pfloatSp  = pfloat .>> spaces
let pint32Sp  = pint32 .>> spaces 
let nameSp    = name .>> spaces
let intName   = nameSp <|> (pint32Sp |>> string)
let expParser = E.parse nameSp .>> spaces

let localConc = Parser.sqBrackets 
                  (Parser.sepBy (Parser.paren ((intName .>> ((kw ",")) .>>. expParser ))) (kw ";"))
                    >>= fun x -> preturn [LocalConcentrations x]
let localConc2 = Parser.sqBrackets 
                  (Parser.sepBy (intName .>> (kw "=") .>>. expParser ) (kw ";"))
                    >>= fun x -> preturn [LocalConcentrations x]
let toeholds          = pfloatSp >>= fun f1 -> 
                         pfloatSp >>= fun f2 -> preturn [ToeholdBindRate f1; ToeholdUnbindRate f2]
let leak            = expParser >>= fun v -> preturn [ LeakRateL               v  ]
let pinLeak         = expParser >>= fun v -> preturn [ PinLeakRate       (Some v) ]
let tauRate         = expParser >>= fun v -> preturn [ TauRate                 v  ]
let elemMigrateRate = expParser >>= fun v -> preturn [ ElementaryMigrationRate v  ]
let lengths         = pint32Sp >>= fun i -> 
                       pint32Sp >>= fun j -> preturn [ToeholdLength i; SpecificityLength j]

let KW_COMPLEMENT    = "complement"
let KW_CONDENSED     = "condensed"
let KW_NUCLEOTIDES   = "nucleotides"
let KW_BRANCHES      = "branches"
let KW_BABYLON       = "babylon"
let KW_WIDE          = "wide"
let KW_CIRCLES       = "circles"
let KW_CLASSIC       = "classic"
let KW_ROTATE_LABELS = "rotate_labels"

let dictToMap (dic : System.Collections.Generic.IDictionary<_,_>) = 
    dic 
    |> Seq.map (|KeyValue|)  
    |> Map.ofSeq

let displayRendering (r:rendering) = 
  let renderer =
    if r.renderer <> default_rendering.renderer
      then 
        sprintf "renderer = %s; " 
          <|  match r.renderer with 
               | renderer.Branches -> KW_BRANCHES
               | renderer.Circles  -> KW_CIRCLES
               | renderer.Classic  -> KW_CLASSIC
      else ""
  let mode =
    if r.mode <> default_rendering.mode
      then
        sprintf "mode = %s; "
          <| match r.mode with
              | renderer_mode.Complement  -> KW_COMPLEMENT
              | renderer_mode.Condensed   -> KW_CONDENSED
              | renderer_mode.Nucleotides -> KW_NUCLEOTIDES
      else ""
  let rotate_labels =
    if r.rotate_labels <> default_rendering.rotate_labels
      then
        sprintf "rotate_labels = %s; " (string r.rotate_labels)
      else ""
  let arrange =
    if r.arrange <> default_rendering.arrange
      then
        sprintf "arrange = %s; "
          <| match r.arrange with
              | arrange_mode.Babylon -> KW_BABYLON
              | arrange_mode.Wide    -> KW_WIDE
      else ""
  if r <> default_rendering
    then sprintf "directive rendering { %s }" (renderer + mode + arrange + rotate_labels)
    else ""

// \ts directive
let cle = Microsoft.Research.DNA.LogicDSD.engine
let optionParsers = [
      (* 
         \gbar \te{compilation}        \nt{Mode}            & Compilation mode\\
         \nt{Mode} ::= \te{detailed}   &\\
                  \gbar \te{infinite}  &\\
                  \gbar \te{finite}    &\\
                  \gbar \te{default}   &\\
                            *)
      kw "compilation" >>. choice [   kw "detailed" >>. preturn [Rules Detailed]
                                      kw "infinite" >>. preturn [Rules Infinite]
                                      kw "finite"   >>. preturn [Rules Finite]
                                      kw "default"  >>. preturn [Rules Default]  ]
      kw "leaks"          >>. preturn [Leaks]
      kw "pinleaks"       >>. preturn [PinLeaks]
      kw "polymers"       >>. preturn [Polymers]
      kw "unproductive"   >>. preturn [Unproductive]
      kw "sequenceRates"  >>. preturn [SequenceRates]
      kw "declare"        >>. preturn [DeclareDomains]
      kw "toeholds"       >>. toeholds  // sets ToeholdBindRate and ToeholdUnbindRate
      kw "leak"           >>. leak      // sets LeakRateL and LeakRateW
      kw "pinleak"        >>. pinLeak
      kw "tau"            >>. tauRate
      kw "migrate"        >>. elemMigrateRate
      kw "lengths"        >>. lengths
      kw "localconcentrations" >>. localConc
      kw "locations"      >>. localConc2
      kw "predicates"     >>. choice [kw "all"  >>. preturn [GeneratePredicates All_predicates];
                                      kw "none" >>. preturn [GeneratePredicates No_predicates] ]
      kw "rendering"     >>. (record default_rendering
        [
          "renderer", (choice [
                         kw KW_BRANCHES |>> fun _ r -> ({r with renderer = Branches})
                         kw KW_CLASSIC  |>> fun _ r -> ({r with renderer = Classic})
                         kw KW_CIRCLES  |>> fun _ r -> ({r with renderer = Circles})
                       ])
          ; "mode", (choice [
                       kw KW_NUCLEOTIDES  |>> fun _ r -> ({r with mode = Nucleotides})
                       kw KW_COMPLEMENT   |>> fun _ r -> ({r with mode = Complement})
                       kw KW_CONDENSED    |>> fun _ r -> ({r with mode = Condensed})
                    ])
          ; "arrange", (choice [
                        kw "babylon" |>> fun _ r -> ({r with arrange = Babylon})
                        kw "wide"    |>> fun _ r -> ({r with arrange = Wide})
                      ])
          ; KW_ROTATE_LABELS, pbool |>>  fun x b -> ({b with rotate_labels = x})
        ]) >>= (Rendering >> List.singleton >> preturn)
      (*
      kw "rendermode"     >>. choice [kw KW_COMPLEMENT  >>. preturn [Rendering {default_rendering with renderer=Classic;classic=Complement}]
                                      kw KW_CONDENSED   >>. preturn [Rendering {default_rendering with renderer=Classic;classic=Condensed}]
                                      kw KW_NUCLEOTIDES >>. preturn [Rendering {default_rendering with renderer=Classic;classic=Nucleotides}]
                                      kw KW_BRANCHES    >>. preturn [Rendering {default_rendering with renderer=Branches}]
                                      kw KW_CIRCLES     >>. preturn [Rendering {default_rendering with renderer=Circles}]]
      *)
      kw "jit"            >>. preturn [IsJit true]
      kw "rules"          >>. braces (RulesDSD.Parser.pprogram cle (fun x -> Microsoft.Research.DNA.LogicDSD.psite cle x false) : t<RulesDSD.Syntax.RulesProgram<Microsoft.Research.DNA.LogicDSD.SiteT>>) 
                                |>> fun (program:RulesDSD.Syntax.RulesProgram<Microsoft.Research.DNA.LogicDSD.SiteT>)-> 
                                  program
                                  |> dictToMap
                                  |> IsRules
                                  |> List.singleton
                                // (System.Collections.Generic.Dictionary. IsRules >> List.singleton)

      kw "jit"            >>. preturn [IsJit true]
      kw "rules"          >>. braces (RulesDSD.Parser.pprogram cle (fun x -> Microsoft.Research.DNA.LogicDSD.psite cle x false))
                                |>> fun program -> 
                                  program
                                  |> dictToMap
                                  |> IsRules
                                  |> List.singleton
                                // (System.Collections.Generic.Dictionary. IsRules >> List.singleton)
    ]

let parse_option = choice optionParsers

let update_options ops o =
  match o with
  | Rules s                   -> { ops with rules           = s }
  | Leaks                     -> { ops with leaks           = true }
  | PinLeaks                  -> { ops with pin_leaks       = true }
  | Polymers                  -> { ops with polymers        = true }
  | Unproductive              -> { ops with unproductive    = true }
  | SequenceRates             -> { ops with sequence_rates  = true }
  | DeclareDomains            -> { ops with declare_domains = true }
  | CheckDna                  -> { ops with check_dna       = true }
  | ColourToeholds            -> { ops with colour_toeholds = true }
  | Program s                 -> { ops with program         = s }
  | Toeholds s                -> { ops with toeholds        = s }
  | Specificities s           -> { ops with specificities   = s }
  | Rendering r               -> { ops with rendering       = r }
  | PlotNames                 -> { ops with plot_names      = true }
  | StateImages               -> { ops with state_images    = true }
  (* These are from the directives. *)
  | ToeholdBindRate f         -> { ops with toehold_bind_rate         = f  }
  | ToeholdUnbindRate f       -> { ops with toehold_unbind_rate       = f  }
  | LeakRateL p               -> { ops with leak_rate_l               = p  }
  | LeakRateW p               -> { ops with leak_rate_w               = p  }
  | PinLeakRate p             -> { ops with pinleak_rate              = p  }
  | TauRate p                 -> { ops with tau_rate                  = p  }
  | ElementaryMigrationRate p -> { ops with elementary_migration_rate = p  }
  | ToeholdLength i           -> { ops with toehold_length            = i  }
  | SpecificityLength i       -> { ops with specificity_length        = i  }
  | LocalConcentrations l     -> { ops with local_concentrations      = l  }
  | GeneratePredicates gp     -> { ops with generate_predicates       = gp }
  (************************************************************************)
  | Temperature             f -> { ops with temperature          = f }
  | StabilityCorrection     f -> { ops with stabilityCorrection  = f }
  | CoaxialDangle           f -> { ops with coaxialDangle        = f }
  | DoubleCoaxialDangle     f -> { ops with doubleCoaxialDangle  = f }
  | TerminalDangleFactor    f -> { ops with terminalDangleFactor = f }
  | CoaxialCorrection       p -> { ops with coaxialCorrection    = p }
  | IsJit                   b -> { ops with is_jit               = b }
  | IsRules                 r -> { ops with rulesProgram         = Some r}


let display
    {rules = rules;
      leaks = leaks;
      pin_leaks = pin_leaks;
      polymers = polymers ;
      unproductive = unproductive ;
      sequence_rates = sequence_rates;
      temperature = _;
      stabilityCorrection = _;
      coaxialDangle = _;
      doubleCoaxialDangle = _;
      terminalDangleFactor = _;
      coaxialCorrection = _;
      declare_domains = declare_domains ;
      check_dna = _ ; //check_dna ; // not used?
      colour_toeholds = _; // colour_toeholds ; // not used?
      program = _;
      toeholds = _;
      specificities = _;
      rendering = rendering;
      plot_names = _;
      state_images = _;
      (* These are from the directives. *)
      toehold_bind_rate = toehold_bind_rate;
      toehold_unbind_rate= toehold_unbind_rate;
      leak_rate_l=leak_rate_l;
      leak_rate_w=_;
      pinleak_rate=pinleak_rate;
      tau_rate=tau_rate;
      elementary_migration_rate=elementary_migration_rate;
      toehold_length=toehold_length;
      specificity_length=specificity_length;
      local_concentrations=local_concentrations;
      generate_predicates= generate_predicates;
      is_jit = isJit;
      } : string =
    
    let plrates = match pinleak_rate with 
                  | None   -> None
                  | Some x -> Some ("directive pinleak " + E.to_string id x)
    let lengths     = (toehold_length, specificity_length)
    let defLengths  = (default_options.toehold_length, default_options.specificity_length)
    let a1  = if rules = default_options.rules
                then None 
                else Some (match rules with 
                          | Infinite -> "directive compilation infinite"
                          | Finite   -> "directive compilation finite"
                          | Detailed -> "directive compilation detailed"
                          | Default  -> "directive compilation defaul")
    let a2  = if  leaks                     = default_options.leaks                     then None else Some "directive leaks"
    let a3  = if  pin_leaks                 = default_options.pin_leaks                 then None else Some "directive pinleaks" 
    let a4  = if  polymers                  = default_options.polymers                  then None else Some "directive polymers"
    let a5  = if  unproductive              = default_options.unproductive              then None else Some "directive unproductive"
    let a6  = if  sequence_rates            = default_options.sequence_rates            then None else Some "directive sequenceRates"
    let a7  = if  declare_domains           = default_options.declare_domains           then None else Some "directive declare" 
    let a8  = if  toehold_bind_rate         = default_options.toehold_bind_rate         
                  || toehold_unbind_rate    = default_options.toehold_unbind_rate       then None else Some ("directive toeholds " + toehold_bind_rate.ToString() + " " + toehold_unbind_rate.ToString() )
    let a9  = if  leak_rate_l               = default_options.leak_rate_l               then None else Some ("directive leak " + E.to_string id leak_rate_l)
    let a10 = if  pinleak_rate              = default_options.pinleak_rate              then None else plrates
    let a11 = if  tau_rate                  = default_options.tau_rate                  then None else Some ("directive tau " + E.to_string id tau_rate)
    let a12 = if  elementary_migration_rate = default_options.elementary_migration_rate then None else Some ("directive migrate " + E.to_string id elementary_migration_rate)
    let a13 = if  lengths                   = defLengths                                then None else Some ("directive lengths " + toehold_length.ToString() + " "+specificity_length.ToString())
    let a14 = if  local_concentrations      = default_options.local_concentrations      
                then None 
                else Some ("directive localconcentrations [" 
                          + String.concat "; " 
                              (List.map (fun (n, x) -> "(" + n + ", " + E.to_string id x + ")") local_concentrations)
                          + "]")
    let a15 = if  generate_predicates       = default_options.generate_predicates       then None else Some ("directive predicates " + match generate_predicates with
                                                                                                                                       | All_predicates -> "all"
                                                                                                                                       | No_predicates  -> "none")
    let a16 = if rendering                  = default_options.rendering                 then None else Some (displayRendering rendering) 
    let a17 = if isJit                                                                  then Some "directive jit" else None
    match List.choose id [a1;a2;a3;a4;a5;a6;a7;a8;a9;a10;a11;a12;a13;a14;a15;a16;a17] with 
    | []  -> ""
    | xs  -> String.concat "\n" xs + "\n"