[<JavaScript>]
module Microsoft.Research.DNA.DSDParser

open Parser
open Syntax
open Microsoft.Research.CRNEngine
open RulesDSD
open Microsoft.Research.DNA.LogicDSD

(*
module Expression  = Microsoft.Research.CRNEngine.Expression
module Options     = Options
*)
type 'site strand = 'site list

(* Syntax *)
type domain =
  { name : string
  ; complemented : bool
  ; toehold : bool }

type normal_site =
  { domain : domain
  ; doc : Value.t }

type tether =
  { tags : string list
  ; pos : Types.pos }

type site = Site of normal_site | Tether of tether

type toehold =
  { name : string
  ; bind_rate : float
  ; unbind_rate : float }

type 'site hpl =
  { loop : 'site strand
  ; middle : 'site strand
  ; upper_right : 'site strand
  ; lower_right : 'site strand }

type 'site m =
  { upper_left : 'site strand
  ; lower_left : 'site strand
  ; middle : 'site strand
  ; upper_right : 'site strand
  ; lower_right : 'site strand }

type 'site hpr =
  { loop : 'site strand
  ; middle : 'site strand
  ; upper_left : 'site strand
  ; lower_left : 'site strand }

type 'site segment =
  | HairpinLeft of 'site hpl
  | Middle of 'site m
  | HairpinRight of 'site hpr

type 'site gate =
  | Singleton of 'site segment
  | JoinLower of 'site gate * 'site gate
  | JoinUpper of 'site gate * 'site gate

type 'site species =
  | StrandUpper of 'site strand
  | StrandLower of 'site strand
  | Gate of 'site gate
  | LogicDsdProcess of RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT> // CS: horrible hack: I'm adding this string field to host Logic DSD species, and still be compatible/reuse with the existing parser/process conversion

type 'site complex = int * 'site species list

type 'site t = toehold list * 'site complex list

//let dummy_range = None
let to_range pos : Value.range = Some (pos, pos)

let to_float pos f = Microsoft.Research.DNA.Value.Float(f, to_range pos)
//let to_int pos i = Microsoft.Research.DNA.Value.Int(i, to_range pos)

let map_site pos =
  function
  | Site s ->
    let d = Value.Variable(s.domain.name, to_range pos)
    if s.domain.toehold then
      Domain.Toe(d, s.doc, s.domain.complemented, None)
    else
      Domain.Normal(d, s.domain.complemented, None)
  | Tether tether ->
    let domain_tags = tether.tags |> List.map (fun t -> t, 1.0)
    Domain.Normal(Value.Variable("tether", to_range pos), false, Some (domain_tags, 0))

let to_domains pos = List.map (map_site pos)

let to_segment pos = function
  | HairpinLeft hpl ->
      Segment.Hairpin
        ( to_domains pos hpl.lower_right
        , to_domains pos hpl.upper_right
        , to_domains pos hpl.middle
        , to_domains pos hpl.loop
        , Segment.Left)
  | Middle m ->
      Microsoft.Research.DNA.Segment.Double
        ( to_domains pos m.lower_left
        , to_domains pos m.upper_left
        , to_domains pos m.middle
        , to_domains pos m.upper_right
        , to_domains pos m.lower_right)
  | HairpinRight hpr ->
      Microsoft.Research.DNA.Segment.Hairpin
        ( to_domains pos hpr.lower_left
        , to_domains pos hpr.upper_left
        , to_domains pos hpr.middle
        , to_domains pos hpr.loop
        , Microsoft.Research.DNA.Segment.Right)

let rec to_gate pos = function
  | Singleton segment -> [[to_segment pos segment]]
  | JoinLower (gl, gr) ->
      let ssl = to_gate pos gl
      let ssr = to_gate pos gr
      match List.rev ssl, ssr with
      | (last::l_rest_rev), (first::r_rest) -> (List.rev l_rest_rev)@((last@first)::r_rest)
      | _,_ -> failwith "malformed gate"
  | JoinUpper (gl, gr) -> to_gate pos gl @ to_gate pos gr

let start_time pos = to_float pos 0.0
let starting_at t f x = f (x, t)
let at_start pos f = starting_at (start_time pos) f

let to_species pos = function
  | StrandUpper sites -> sites |> List.map (map_site pos) |> Microsoft.Research.DNA.Strand.Upper |> Microsoft.Research.DNA.Species.STRAND
  | StrandLower sites -> sites |> List.map (map_site pos) |> Microsoft.Research.DNA.Strand.Lower |> Microsoft.Research.DNA.Species.STRAND
  | Gate gate -> gate |> to_gate pos |> Microsoft.Research.DNA.Species.GATE
  | LogicDsdProcess p -> Microsoft.Research.DNA.Species.LogicDsdProcess p

let to_process pos (s,t) =
  match s with
  | StrandUpper sites -> sites |> List.map (map_site pos) |> Microsoft.Research.DNA.Strand.Upper |> starting_at t Microsoft.Research.DNA.Process.Strand
  | StrandLower sites -> sites |> List.map (map_site pos) |> Microsoft.Research.DNA.Strand.Lower |> starting_at t Microsoft.Research.DNA.Process.Strand
  | Gate gate -> gate |> to_gate pos |> starting_at t Microsoft.Research.DNA.Process.Gate
  | LogicDsdProcess p -> Microsoft.Research.DNA.Process.LogicDsdProcess (p, t)

(* Parsing *)
let mk_domain ((n, t), c) =
  { name = n
  ; complemented = c
  ; toehold = t }

let mk_site pos (d, doco) =
  { domain = d
  ; doc = match doco with Some doc -> doc | None -> to_float pos 1.0 }
  |> Site

let mk_tether pos tags = Tether { tags = tags; pos = pos }

let pname allowDots = many1Satisfy (fun c -> isLetter c || isDigit c || c = '_' || c = '\'' || (if allowDots then c = '.' else false)) <?> "an identifier"
let parse_name = pname false
  
let after p s = p .>>. opt (pstring s) |>> function n, None -> (n, false) | n,_ -> (n,true)

let op_plus pos a b   = Value.Op (a, Value.Plus, b, to_range pos)
let op_minus pos a b  = Value.Op (a, Value.Minus, b, to_range pos)
let op_mul pos a b    = Value.Op (a, Value.Mul, b, to_range pos)
let op_div pos a b    = Value.Op (a, Value.Div, b, to_range pos)
let op_power pos a b  = Value.Op (a, Value.Power, b, to_range pos)
let op_modulo pos a b = Value.Op (a, Value.Modulo, b, to_range pos)

let rec map_value pos = function
  | Expression.Float f                      -> Value.Float (f, to_range pos)
  | Expression.Key s                        -> Value.Variable (s, to_range pos)
  | Expression.Plus  (e::es)                -> es |> List.map (map_value pos) |> List.fold (op_plus pos) (map_value pos e)
  | Expression.Minus  {sub1=a; sub2=b}      -> op_minus pos (map_value pos a) (map_value pos b)
  | Expression.Times  (e::es)               -> es |> List.map (map_value pos) |> List.fold (op_mul pos) (map_value pos e)
  | Expression.Divide {div1=a; div2=b}      -> op_div pos (map_value pos a) (map_value pos b)
  | Expression.Power  {base_=a; exponent=b} -> op_power pos (map_value pos a) (map_value pos b)
  | Expression.Modulo {div=a; modulo=b}     -> op_modulo pos (map_value pos a) (map_value pos b)
  | Expression.Ceiling x                    -> Value.Ceiling (map_value pos x, to_range pos)
  | Expression.Floor x                      -> Value.Floor (map_value pos x, to_range pos)
  | _ -> failwith "unsupported expression"

(*
   CS: The lookahead of a parenthesis '(' in parse_value disambiguates 
       some ambiguities of the DSD grammar in dealing with expressions, 
       initializations and modules.
       
       Consider this program P with a module N:
         P = def N() = <_>
             ( 3 * N ())
       
       The inner process in P is supposed to be parsed as:
         3 * ( N () )
       
       However, the Expression parser is greedy and tries to parse 'N' as a value:
         ( 3 * N ) ()
       
       and then fails, because "()" is not a valid species. 
       The lookahead verifies that a name is not followed by a parenthesis 
       (and therefore it is not a module).
       It would be better to restrict the grammar to either change '*' with 'of'
       in the initialization, or to force composite expressions to be wrapped
       by parentheses (i.e. 2*3* N() must be written as ( (2*3) * N() ) 
*)
let parse_value_orig = parse_name .>> spaces 
                       |> Expression.parse 
                       |~> map_value
let pValue = (pTry (parse_name .>> spaces >>= fun x -> (kw "(" >>. failParser "") <|> preturn x ) )
               |> Expression.parse 
let parse_value = pValue |~> map_value

let map_values pos = List.map (map_value pos)
let tuple p = sepBy p (skw ",") |> paren

let rec parse_values st : Parser.Result<value list> = 
  let x = parse_value <|> (parse_values |~> fun pos vs -> Value.Tuple (vs, to_range pos) )
  x |> tuple  <| st

let (&>) s p = kw s >>. p

let parse_doc st =
  (* According to the grammar, an integer can be both an identifier and a float.
     This introduces an ambiguity when parsing strands. Consider for example:
       <1^ 2>
     This can be parsed either as the strand with two sites <(1^) (2)>,
     or as the strand with a single site with rate 2 <(1^(2))>.
     If the "pfloat" parser is used in the code below, the grammar always gives 
     precedence to the second interpretation. However, the grammar should
     prioritize the first interpretation instead. Therefore we have to implement
     a lookup in the grammar: if we have an integer that is *not* followed by a dot,
     then the integer is actually an identifier, and the doc parser should return the 
     default value. Otherwise it is a float and should be parsed as such. *)
  let pdefault pos = to_float pos 1.0
  choice
    [ pTry (pint32 >>.
       choice [ kw "." >>. failParser ""   // Float found: fail the current parser and 
                                           // let the float be parsed by "pfloat"
              ; fun _ -> OkEmpty (pdefault (snd st), st)]) // Identifier found: return 
    ; pfloat |~> to_float
    ; parse_value |> paren
    ; preturn () |~> fun pos () -> pdefault pos]
    st

let parse_site allowUnderscores name =
  let domainname_doc = name +>>+ opt (kw "^" >>. parse_doc)
  let site = after domainname_doc "*" >>= fun ((n, doco), c) (str, pos) -> 
    if n = "_" && not allowUnderscores
      then Parser.failParser "domain name \"_\" not allowed" (str, pos)
      else let dom = (mk_domain ((n, doco.IsSome), c), doco) |> mk_site pos 
           Parser.preturn dom (str, pos)
  let int_name = (name <|> (pint32 |>> string)) +>> spaces 
  let tether = "tether" &> (sepBy1 int_name (skw ",") |> paren) |~> mk_tether
  (tether .>> spaces) <|> pTry (site .>> spaces )

let sep_by_bars p = sepBy1 p (skw "|")

let mk_hpl loop m (u, l) =
  { loop = loop
  ; middle = m
  ; upper_right = u
  ; lower_right = l }
  |> HairpinLeft |> Singleton

let mk_m (ul, ll) m (ur, lr) =
  { upper_left = ul
  ; lower_left = ll
  ; middle = m
  ; upper_right = ur
  ; lower_right = lr }
  |> Middle |> Singleton

let mk_hpr (u, l) m loop =
  { loop = loop
  ; middle = m
  ; upper_left = u
  ; lower_left = l }
  |> HairpinRight |> Singleton

let alternatives l =
  match List.rev (List.map pTry l) with
  | [] -> choice []
  | h::t ->
    h :: (t |> List.map attempt) |> List.rev |> choice

let strand_gate site upperSep lowerSep =
  let sites = Parser.many1 site

  let upper     = sites |> bracket "<" ">"
  let lower     = sites |> bracket "{" "}"
  let double    = sites |> bracket "[" "]"
  let left_hp   = sites |> bracket "<" "}"
  let right_hp  = sites |> bracket "{" ">"

  let overhangs =
    choice
      [ upper .>>. (lower <|> preturn []) 
      ; lower .>>. (upper <|> preturn []) >>= fun (b, a) -> preturn (a, b) 
      ; preturn ([], []) ]

  let hpl = pipe3 left_hp double overhangs mk_hpl
  let hpr = pipe3 overhangs double right_hp mk_hpr  
  let m = pipe3 overhangs double overhangs mk_m
  let segment = alternatives [hpl; hpr; m]

  let connect_upper = upperSep |>> fun _ l r -> JoinUpper (l, r)
  let connect_lower = lowerSep |>> fun _ l r -> JoinLower (l, r)
  let connect = (attempt connect_upper) <|> connect_lower
  let gate = chainl1 segment connect |>> Gate

  let strand = (upper |>> StrandUpper) <|> (lower |>> StrandLower)

  strand, gate

let do_parse_molecule upperSep lowerSep allowDots allowUnderscore = 
  let strand, gate = pname allowDots |> parse_site allowUnderscore |> fun x -> strand_gate x upperSep lowerSep
  
  // CS: new: parse a Logic DSD process
  let logicDsdComplex = 
    let cache      = RulesDSD.Syntax.makeProvider ()
    let siteParser = Microsoft.Research.DNA.LogicDSD.psite Microsoft.Research.DNA.LogicDSD.engine cache true
    Parser.sqBrackets (RulesDSD.Parser.psystem siteParser)
    |>> LogicDsdProcess
  
  Parser.plookAheadWith (
      Parser.choice [
        Parser.pTry (Parser.kw "[" >>. Parser.kw "<" >>= fun _ -> preturn true)
        preturn false
      ])
      >>= fun isLogicDSD ->
        if isLogicDSD
          then logicDsdComplex
          else (attempt gate) <|> strand

let parse_molecule_dots = do_parse_molecule (kw "::") (kw ":") true
let parse_molecule = do_parse_molecule (kw "::") (kw ":") false
  

let at_time = ("@" &> parse_value) |> Parser.opt

let parse_ground_species allowUnderscore =
  [ parse_name .>>. parse_values .>>. at_time |>> fun ((n, vs), t) -> Process.Instance(n,None,vs,t)
  ; parse_molecule allowUnderscore .>>. at_time |~> to_process
  ] |> choice

let parse_species allowUnderscore =
  [ (parse_ground_species allowUnderscore |> sep_by_bars |> bracket "[[" "]]") +>>+ at_time |>> Process.Origami // NB: currently this allows timed species inside an origami, whereas the original parser does not
  ; parse_ground_species allowUnderscore
  ] |> choice

let parse_string = manySatisfy ((<>) '\"') |> bracket "\"" "\""

let subdomain = pTry (parse_name .>> pstring "*" |>> sprintf "%s*") <|> parse_name 

let binding =
  [ "seq" &> ("=" &> parse_name |~> fun pos name -> "seq", Process.DNA (name, to_range pos))
  ; "colour" &> ("=" &> parse_string |~> fun pos name -> "colour", Process.Colour (name, to_range pos))
  //; parse_name +>>+ ("=" &> parse_name) |~> fun pos (name, var) -> name, Process.Rate (Value.Variable (var, to_range pos)) TODO: Is this case redundant?
  ; parse_name +>> kw "=" >>= fun name -> 
      choice [ pTry (after (list_of1 subdomain) "*") |~> fun pos (doms, short) -> name, Process.Subdomains ((doms, short), to_range pos)
             ; parse_value |>> fun var -> name, Process.Rate var ]
  ] |> choice

let record = sepEndBy1 binding (skw ";") |> bracket "{" "}"


let int_name = parse_name <|> (pint32 |>> string)

let restriction =
  int_name .>> spaces >>= fun n -> 
    choice  [ "@" &> parse_value +>>+ ("," &> parse_value_orig) |>> fun (b, u) -> n, [("bind", Process.Rate(b)); ("unbind", Process.Rate(u))]
            ; kw "=" >>. preturn n .>>. record
            ; preturn (n, [])
    ]
  
let map_rate _ = function
  | Rate.MassAction v -> v (*|> map_value pos*) |> Dsdreaction.Crn_ma
  | Rate.Function f -> f |> Dsdreaction.Crn_func

let to_dsd_reaction pos (r: Reaction<Process.t, Expression.t<Value.name>, Expression.t<Key<Process.t>>>) =
  { Dsdreaction.ma_catalysts   = r.catalysts
  ; Dsdreaction.ma_reactants   = r.reactants
  ; Dsdreaction.ma_products    = r.products
  ; Dsdreaction.ma_rate        = r.rate |> map_rate pos
  ; Dsdreaction.ma_reverseRate = r.reverse |> Option.map (map_rate pos) }

let before s p = opt (kw s) .>>. p |>> function None, n -> (false, n) | _, n -> (true, n)


(***********************)
//type TempInitialRecord = failwith ""
(***********************)

let rec parse_proc st =
  let pKey = Key.parse (parse_species false)
  [ "new" &> restriction +>>+ parse_proc |>> Process.New
  ; pTry (
          opt (kw "rxn") 
          >>. Reaction.parse                  // parse a reaction...
              (parse_species false)           // ... with these species ...
              (Expression.parse parse_name)   // ... with these rate values...
              (Expression.parse pKey)
              (Expression.Float 1.0)          // ... in these rate expressions
          |~> (fun pos -> to_dsd_reaction pos >> Process.Chemical)
          <?> "a reaction"
    )
  ; pTry ("constant" &> parse_species false
          |~> fun pos p -> Process.Repeat(Value.Int(1, to_range pos), true, p, None, None))

  ; pTry (parse_species false .>> choice [kw "with"; kw "="] >>= fun p -> 
            let proc = Process.Repeat(Value.Int(0, None), false, p, None, None)
            Parser.record 
              proc 
              [ "spatial", Spatial_initial.parser |>> fun sp p -> 
                            match p with 
                            | (Process.Repeat(x,b,p',t,_)) -> 
                                Process.Repeat(x,b,p',t, Some sp)
                            | _ -> failwith "Unexpected initial"
                "time", Value.parse |>> fun t' p -> 
                            match p with 
                            | (Process.Repeat(x,b,p',_,sp)) -> 
                                Process.Repeat(x,b,p',Some t',sp)
                            | _ -> failwith "Unexpected initial"
                "value", Value.parse |>> fun v' p -> 
                            match p with 
                            | (Process.Repeat(_,b,p',t,sp)) -> 
                                Process.Repeat(v',b,p',t,sp)
                            | _ -> failwith "Unexpected initial"
                "constant", Parser.pbool .>> spaces |>> fun b' p -> 
                            match p with 
                            | (Process.Repeat(v,_,p',t,sp)) -> 
                                Process.Repeat(v,b',p',t,sp)
                            | _ -> failwith "Unexpected initial"    ]
          |~> fun pos proc -> match proc with 
                              | (Process.Repeat(v,b,p,t,sp)) -> 
                                  let t' = match t with 
                                           | Some time -> Some (Value.set_range (to_range pos) time)
                                           | None      -> None
                                  Process.Repeat(v,b,p,t',sp)
                              | _ -> failwith "Unexpected initial"
            )
            //let initial = 
            //Parser.record ["spatial", id] )
  ; pTry (before "constant" parse_value +>>+ ((kw "*" <|> kw "of" <|> Parser.preturn "") >>. before "constant" (parse_species false)) 
          |~> fun pos ((c1,n), (c2,p)) -> Process.Repeat(n, c1||c2, p, None, None)
          <?> "species initial"
          )
  ; (pTry <| parse_species false) <?> "a species" 
  ; parse_proc |> sep_by_bars |> paren |>> Process.Parallel
  ] |> choice <| st




let rec parse_parameters st = tuple do_parse_param  st
and 
  do_parse_param = 
    ( (parse_parameters >>= (Pattern.Pat >> preturn))
      <|> ( parse_name >>= (Pattern.Name >> preturn))) 
    
  
type syn =
  | Syn_new of ((string * (string * Process.dom_data) list) * bool)
  | Syn_def of (string * Types.range * Pattern.t list * Process.t)
  | Syn_val of (string * Value.t)
  | Syn_proc of Process.t

let rec to_syntax = function
  | Syn_new (d, b) :: syns -> Syntax.New (d, b, syns |> to_syntax)
  | Syn_def (n, r, vs, p) :: syns -> Syntax.Definition (n, r, vs, p, syns |> to_syntax)
  | Syn_val (n, v) :: syns -> Syntax.Value (n, v, syns |> to_syntax)
  | [Syn_proc p] -> Syntax.Process p
  | [] -> Syntax.Process(Process.Parallel [])
  | Syn_proc _ :: _ -> failwith "Multiple processes"

let seq1 f = sepBy1 f spaces

let rec parse_ds =
  spaces >>. (
    [ "def" &> parse_name .>> spaces >>=
      fun name -> ((parse_parameters +>>+ ("=" &> parse_proc) |~> fun pos (ps, p) -> Syn_def (name, to_range pos, ps, p))
                    <|>
                    (("=" &> ((parse_value >>= fun x->preturn [x]) <|>parse_values) 
                                |~> fun pos v -> Syn_val (name, match v with
                                                                | [x] -> x
                                                                | _   -> Value.Tuple (v, to_range pos) ))))
    ; "new" &> restriction |>> fun r -> Syn_new (r, true)
    ; "dom" &> int_name   +>>+ ("=" &> record)      |>> fun r -> Syn_new (r, false)
    ; parse_proc +>> eof |>> Syn_proc
    ] 
    |> choice
    |> seq1
    |>> to_syntax)

let rec parse_sg =
  let provider   = RulesDSD.Syntax.makeProvider ()
  let siteParser = Microsoft.Research.DNA.LogicDSD.psite Microsoft.Research.DNA.LogicDSD.engine provider true
  let pRulesInitial = (between (kw "[") (kw "]") (RulesDSD.Parser.psystem siteParser))
  spaces >>. 
    paren (sepBy1 (Expression.parse (name) .>>. pRulesInitial) (kw "|")) .>> eof

let speciesParser = parse_molecule true |~> to_species

let plotParser = 
  // lift the species parser to plots parser
  let speciesPlotParser = (speciesParser >>= (Syntax.t_plot_base.Molecule >> preturn))
  
  // module parser
  let moduleParser = name .>> spaces >>= fun n ->
            choice  [ kw "_" >>. preturn (Syntax.t_plot_base.Module (n, Syntax.plot_pat.All))
                    ; paren (many Value.parse) >>= fun vs -> 
                        preturn (Syntax.t_plot_base.Module (n, Syntax.plot_pat.Pats vs))
                    ; preturn (Syntax.t_plot_base.String n) ]

  // a plot is an expression over either a species or a module
  (speciesPlotParser <|> moduleParser)

let dsdDirectivesParser = 
  //AP//let parseSettings = Microsoft.Research.CRNEngine.Directive<t_plot_base>.parse plotParser 
  //let parseSettings = Directive<Expression.t<t_plot_base>>.parse (Expression.parse plotParser)
  //AP NB TODO: replace plot parser with species parser, to ensure that single variables are treated 
  //as species in the plot parser but as parameters in the rate parser.
  let pplot = // parse a single species or an expression over species surrounded by sq. brackets
    (Parser.pTry (plotParser |>> fun s -> Expression.Key (Key.Species s))) <|> 
    (Expression.parse (Key.parse plotParser))
  let parseSettings = Directive<Expression.t<Key<t_plot_base>>>.parse 
                        (Expression.parse (Key.parse plotParser)) 
                        pplot
  let crnDirective = parseSettings        |>> Choice1Of2 
  let optDirective = Options.parse_option |>> Choice2Of2 
  InferenceSiteGraph.parseTask .>>. Parser.many (Parser.kw "directive" >>. ( optDirective <|> crnDirective ))
    
//let updateSettings a b = Crn_settings.update_settings a b
let updateCfg (crns:Crn_settings<'s>, opts) x = 
  match x with
   | Choice1Of2 dir -> (crns.from_directive dir, opts)
   | Choice2Of2 opt -> (crns,                    List.fold Options.update_options opts opt)

let dsdCfg (task,ds) = (task, List.fold updateCfg (Crn_settings.defaults, 
                                                   Options.default_options) ds)

let parse_settings = dsdDirectivesParser |>> dsdCfg 
let parser = dsdDirectivesParser |>> dsdCfg >>= fun (task, (dirs, opts)) -> 
  match Options.get_rules_program opts with
  | None       -> preturn (task, dirs, opts)        .>>. parse_ds .>> eof |>> fun ((a,b,c), e)   -> ((a,b,c, None), e)
  | Some rules -> preturn (task, dirs, opts, rules) .>>. parse_ds .>> eof |>> fun ((a,b,c,d), e) -> ((a,b,c, Some d), e)
let parse x = Parser.run (spaces >>. parser) x