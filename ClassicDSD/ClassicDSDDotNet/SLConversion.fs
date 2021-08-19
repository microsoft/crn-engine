[<JavaScript>]
module Microsoft.Research.DNA.SLConversion

open Parser
open Microsoft.Research.DNA.Syntax
open Microsoft.Research.CRNEngine

(*
module DSDParser  = Microsoft.Research.DNA.DSDParser
module E          = Microsoft.Research.CRNEngine.Expression
module Options    = Microsoft.Research.DNA.Options
type SpS<'a> when 'a:equality       = Microsoft.Research.CRNEngine.Spatial_settings<'a>
type SiS<'a> when 'a:equality        = Microsoft.Research.CRNEngine.Simulation_settings<'a>
type Prior      = Microsoft.Research.CRNEngine.Prior
type Units      = Microsoft.Research.CRNEngine.Units
type Time = Microsoft.Research.CRNEngine.Time
type Concentration = Microsoft.Research.CRNEngine.Concentration
type Parameter = Microsoft.Research.CRNEngine.Parameter
type Reaction<'a,'b,'c> when 'a:equality and 'b:equality and 'c:equality = Microsoft.Research.CRNEngine.Reaction<'a,'b,'c>
module InfSet     = Microsoft.Research.CRNEngine.Inference_settings
module Value      = Microsoft.Research.DNA.Value
module Process    = Microsoft.Research.DNA.Process
*)

(***************************************************************************************
 *** Old DSD language conversion routine.
 *** Parses a program written in the SilverLight version of the DSD language and
 *** translates it into Classic DSD syntax
 ***************************************************************************************)


// utilities
let spFloat     = pfloat .>> spaces
let spInt       = pint32 .>> spaces
let spBool      = choice [ kw "true"  >>. preturn true
                         ; kw "false" >>. preturn false] .>> spaces
let spName      = name .>> spaces

let fpMsg       = "Floating point conversion not supported"
let pOldVal sp  = choice  [ kw "float_of_int" >>= fun _ -> failwith fpMsg
                          ; kw "int_of_float" >>= fun _ -> failwith fpMsg
                          ; pTry (DSDParser.parse_name .>> spaces >>= fun x -> 
                                    (kw "(" >>. failParser "") <|> preturn x ) ]
                    |> Expression.parse <| sp
let pOldValue = pOldVal |~> DSDParser.map_value

let rec pOldVals sp = DSDParser.tuple (pOldValue <|> (pOldVals |~> fun pos xs -> Value.Tuple (xs, (DSDParser.to_range pos)))) sp


let pOldValues sp = pOldVal |> DSDParser.tuple |~> DSDParser.map_values <| sp
// old molecules parsing
let flip (a, b) = (b, a)

let pMolecule upperSep lowerSep = 
  let pSite  = DSDParser.parse_site true DSDParser.parse_name 
  let pSites = Parser.many1 pSite

  let upper     = bracket "<" ">" pSites
  let lower     = bracket "{" "}" pSites
  let double    = bracket "[" "]" pSites

  let overhangs =
    choice
      [ upper .>>. (lower <|> preturn []) 
      ; lower .>>. (upper <|> preturn []) >>= fun (b, a) -> preturn (a, b) 
      ; preturn ([], []) ]
  
  let pLeftHairpinOrOverhangs = 
    kw "<" >>. pSites >>= fun a  -> 
         choice [ kw "}" >>. preturn (Choice1Of2 a) // left hairpin
                ; kw ">" >>. preturn a .>>. (lower <|> preturn []) >>= (Choice2Of2 >> preturn)]

  let rightHairpinOrOverhangs (a, b) =
    choice [  kw "{" >>. pSites >>= fun c -> 
                choice [ kw ">" >>. preturn (DSDParser.mk_hpr a b c) // right hairpin
                       ; kw "}" >>. preturn c .>>. (upper <|> preturn []) >>= (flip >> DSDParser.mk_m a b >> preturn) ]
           ; upper .>>. (lower <|> preturn []) >>= (DSDParser.mk_m a b >> preturn)
           ; preturn (DSDParser.mk_m a b ([], []))
           ]

  let pStr (a : DSDParser.site list * DSDParser.site list) = 
    match a with
    | ([], []) -> failParser "a strand"
    | ([], ls) -> preturn (DSDParser.StrandLower ls)
    | (hs, []) -> preturn (DSDParser.StrandUpper hs)
    | _        -> failParser "a strand"
  let pSegment = 
    choice 
      [ pLeftHairpinOrOverhangs >>= fun a ->
          choice [ double >>= fun b -> 
                    match a with
                    | Choice1Of2 a' -> overhangs >>= (DSDParser.mk_hpl a' b >> DSDParser.Gate >> preturn)
                    | Choice2Of2 a' -> rightHairpinOrOverhangs (a', b) >>= (DSDParser.Gate >> preturn)
                 ; (match a with
                    | Choice2Of2 z  -> pStr z
                    | _             -> failParser "a strand") ]
      ; overhangs >>= fun a ->
          choice [ double >>= fun b -> (a, b) |> rightHairpinOrOverhangs >>= (DSDParser.Gate >> preturn) 
                 ; pStr a ] ] 

  let pItem   = pSegment |~> DSDParser.to_species
  let pSeq    = pItem .>>. many ((upperSep <|> lowerSep) .>>. pItem)
  
  let res = pSeq |~> fun pos (x, y) -> Microsoft.Research.DNA.Species.convert_raw_gate (x, y, DSDParser.to_range pos)
  
  res 

let parseOldMolecule = pMolecule (kw "::" >>. preturn true) (kw ":" >>. preturn false)

let parseOldGroundSpecies =
  choice
    [ DSDParser.parse_name +>>+ pOldVals .>>. DSDParser.at_time |>> fun ((n, vs), t) -> Process.Instance(n,None,vs,t)
    ; parseOldMolecule .>>. DSDParser.at_time >>= fun (v, t) -> 
        match v with
        | Microsoft.Research.DNA.Species.STRAND s -> preturn (Process.Strand (s, t))
        | Microsoft.Research.DNA.Species.GATE s   -> preturn (Process.Gate (s, t))
        | _ -> failParser "a gate or a strand"
        ] 

let pOldSpecies =
  choice 
    [ parseOldGroundSpecies |> DSDParser.sep_by_bars |> bracket "[[" "]]" .>>. DSDParser.at_time |>> Process.Origami 
    // NB: currently this allows timed species inside an origami, whereas the original parser does not
    ; parseOldGroundSpecies ] 

let pReact pKey = Reaction.parse // parse a reaction...
                    pOldSpecies                     // ... with these species ...
                    (Expression.parse DSDParser.parse_name)  // ... with these rate values...
                    (Expression.parse pKey)                  // ... in these rate expressions
                    (Expression.Float 1.0)                   // ... in these rate expressions
                |~> (fun pos -> DSDParser.to_dsd_reaction pos >> Process.Chemical)

let (&>) a b  = DSDParser.(&>) a b

let rec oldProc st =
  let pKey = Microsoft.Research.CRNEngine.Key.parse pOldSpecies
  [ "new" &> DSDParser.restriction +>>+ oldProc |>> Process.New
  ; kw "rxn" >>. pReact pKey
  ; pTry ("constant" &> pOldSpecies 
          |~> fun pos p -> Process.Repeat(Value.Int(1, DSDParser.to_range pos),true,p, None, None))
  ; pTry (DSDParser.before "constant" pOldValue +>>+ ((kw "*" <|> kw "of") >>. DSDParser.before "constant" pOldSpecies) 
          |~> fun pos ((c1,n), (c2,p)) -> Process.Repeat(n,c1||c2,p, None, None)
          <?> "species initialization"
          )
  ; pTry (pOldSpecies) //<?> "a species" 
  ; pTry (pReact pKey)
  ; oldProc |> DSDParser.sep_by_bars |> paren |>> Process.Parallel
  ] |> choice <| st

let pValOrTuple = pOldValue <|> (pOldVals |~> fun pos vs -> (Value.Tuple (vs, (DSDParser.to_range pos))))

// parser.mly
type oldPlot  = PloFloat        of float
              | PloName         of string           // FloatVarAExp or PopulationAExp?
              | PloNameAt       of string * float
              | PloMolecule     of species
              | PloMoleculeAt   of species * float
              | PloModule       of string * value list
              | PloModuleAt     of string * value list * float
              | PloSum          of oldPlot list
              | PloSub          of oldPlot * oldPlot
              | PloDiff         of oldPlot * oldPlot
              | PloProd         of oldPlot list
              | PloDiv          of oldPlot * oldPlot

type oldSimMode = JIT 
                | SSA 
                | OSLO                  of bool (* is stiff? *) 
                | SPACIAL_PERIODIC      of int32
                | SUNDIALS              of bool (* is stiff? *) 
                | CME_OSLO_OR_SUNDIALS  of bool (* is stiff? *) (* the parser is ambiguous on "cme" *)
                | LNA_OSLO              of bool (* is stiff? *) 

type SpParam  = Burnin        of int32
              | SpSamples     of int32
              | Thin          of int32
              | SeparateNoise of bool
              | NoiseModel    of int
              | Prune         of bool

type CoreElem = CoreW of float
              | CoreI of float
              | CoreO of float

type PointElem =  PointX of float
                | PointY of float
                | PointW of float
                | PointV of float

type SpSetting  = SpRandom1       of species * float
                | SpRandom2       of float
                | SpCentralCore   of CoreElem list
                | SpPointOfRecord of PointElem list
                // Settings.add_spatialic [Expressions.PopulationAExp $4] [SettingsConstants.CentralCore {width=$3; inner=$5; outer=Some $6}] z
                | BackwardComp    of species * float * float * float option

type SpBC = Periodic | ZeroFlux


type ParSpace = ParReal | ParLog
type ParVar   = ParFixed | ParRandomized | ParInitVal
type ParItem  = ParAssign of string * float * float * float
              | ParItem   of string * float * float * float * ParSpace * ParVar

type SweepVar = SweepVar of string list * value list list

type SweepItem = NamedSweep   of string * SweepVar list
               | UnnamedSweep of SweepVar

type KMode     = KContextual | KStochastic | KDeterministic

type VerMode = Encoding    of int32
             | Abstraction of int32
             | Enumeration of int32
             | PopBound    of int32
             | DummyRxn    of bool
             | EnfInitial  of bool

type oldDirective = Samples             of float option * float option * int32 option // start, end, points
                  | DurationPoints      of float option * float option * int32 option // start, end, points
                  | Scale               of float
                  | Plot                of oldPlot list
                  | Simulation          of oldSimMode
                  | RelTolerance        of float    
                  | Tolerance           of float    // maps to abstolerance in deterministic
                  | Seed                of int32
                  | OldEvent            of Process.t
                  | SpecMax             of species * int32
                  | OldTime             of Time
                  | Concentration       of Concentration
                  | Dt                  of float
                  | XMax                of float
                  | Nx                  of int32
                  | Theta               of float
                  | Spatialic           of SpSetting
                  | Spatialbc           of SpBC
                  | SpatialPlot         of species
                  | Diffusion           of species * float
                  | DefaultDiffusion    of float
                  | Params              of ParItem list
                  | Sweeps              of SweepItem list
                  | Fit                 of string * string * oldPlot list
                  | FitRun              of SpParam list
                  | PlotWindow          of float * float * float * float
                  | Kinectics           of KMode
                  | Verification        of VerMode
                  | StabilityCorrection of float
                  | CoaxialDAngle       of float
                  | DoubleCoaxialDAngle of float
                  | CoaxialCorrection   of Options.prim
                  | Temperature         of float
                  | TerminalDAngle      of float

// Tokens
let SAMPLE              = kw "sample"
let ALL                 = kw "all"
let PLOT                = kw "plot"
let POINTS              = kw "points"
let DURATION            = kw "duration"
let SCALE               = kw "scale"
let SEED                = kw "seed"
let SIMULATION          = kw "simulation"
let DETERMINISTICSTIFF  = kw "deterministicstiff" 
let RELTOLERANCE        = kw "reltolerance"
let TOLERANCE           = kw "tolerance" <|> kw "abstolerance"
let EVENT               = kw "event"
let SPECMAX             = kw "specmax"
let TIME                = kw "time"
let CONCENTRATION       = kw "concentration"
let DT                  = kw "dt"
let XMAX                = kw "xmax" 
let NX                  = kw "nx"
let THETA               = kw "theta"
let COAXIALDANGLE       = kw "coaxialDangle"
let DOUBLECOAXIALDANGLE = kw "doubleCoaxialDangle"
let COAXIALCORRECTION   = kw "coaxialCorrection"
let TEMPERATURE         = kw "temperature"
let TERMINALDANGLE      = kw "terminalDangle"
let SPATIALIC           = kw "spatialic"
let SPATIALBC           = kw "spatialbc"
let SPATIALPLOT         = kw "spatialplot"
//let COMPILATION         = kw "compilation"
let DECLARE             = kw "declare"
let DEFAULTDIFFUSION    = kw "defaultdiffusion"
let DIFFUSION           = kw "diffusion"
let LEAKS               = kw "leaks"
let PINLEAK             = kw "pinleak"
let PINLEAKS            = kw "pinleaks"
let LENGTHS             = kw "lengths"
let MIGRATE             = kw "migrate"
let LOCALCONCENTRATIONS = kw "localconcentrations"
let POLYMERS            = kw "polymers"
let SEQUENCERATES       = kw "sequenceRates"
let STABILITYCORRECTION = kw "stabilityCorrection"
let TAU                 = kw "tau"
let TOEHOLDS            = kw "toeholds"
let UNPRODUCTIVE        = kw "unproductive"
let VERIFICATION        = kw "verification"
let FIT                 = kw "fit"
let FITRUN              = kw "fit_run"
let KINETICS            = kw "kinetics"
let PARAMETERS          = kw "parameters"
let PLOTWINDOW          = kw "plotwindow"
let PREDICATES          = kw "predicates"
let SWEEP               = kw "sweep"

let COMMA               = kw ","
let SEMI                = kw ";"
let AT                  = kw "@"
let UNDERSCORE          = kw "_"
let SUM                 = kw "sum"
let SUB                 = kw "sub"
let DIFF                = kw "diff"
let PROD                = kw "prod"
let DIV                 = kw "div"
let EQUAL               = kw "="
let DLBRACKET = kw "[["
let DRBRACKET = kw "]]"
let BAR       = kw "|"

(* each function below parses a specific "directive" string *)
// samples directive
let dirSamples = 
  SAMPLE >>. Expression.parse spFloat >>= fun x ->
    choice  [ COMMA >>. spFloat >>= fun y ->
                choice  [ ALL  >>. preturn (Some (Expression.eval id x), Some y, Some 0)
                        ; spInt >>= fun z -> preturn (Some (Expression.eval id x), Some y, Some z)
                        ; preturn  (Some (Expression.eval id x), Some y, None) ]        
            ; ALL >>. preturn (None, Some (Expression.eval id x), Some 0)
            ; spInt >>= fun y -> preturn (None, Some (Expression.eval id x), Some y)
            ; preturn (None, Some (Expression.eval id x), None) ] 
      >>= (Samples >> preturn) // wraps the result inside Samples

// duration directive
let dirDuration = 
  DURATION >>. spFloat >>= fun x ->
    choice  [ COMMA >>. spFloat >>= fun y ->
                choice  [ POINTS >>. 
                            choice [ ALL   >>.          preturn (Some x, Some y, Some 0)
                                   ; spInt >>= fun z -> preturn (Some x, Some y, Some z) ]
                        ; preturn (Some x, Some y, None)                                 ]
            ; POINTS >>.   choice  [ ALL   >>.          preturn (None, Some x, Some 0) 
                                   ; spInt >>= fun y -> preturn (None, Some x, Some y)   ]
            ; preturn (None, Some x, None)                                               ]
      >>= (DurationPoints >> preturn )

// seed directive
let dirSeed  = SEED >>. spInt >>= (Seed >> preturn)

// old molecule parser
let pMol = parseOldMolecule
  // TODO: double check that "--(cogs)" | "--(+ cogs)" | "--(cogs+)" is not used anymore

// plots
// TODO check empty plot "()" is parsed correctly with many or sepBy, double check that many1 or sepBy1 is not misused
let rec pPlot sp = 
  let underscoreVal pos = [Value.Tuple ([Value.Variable ("_", pos)], pos)]
  choice [ spFloat >>= (PloFloat >> preturn) 
         ; SUM  >>. paren (many1 pPlot) >>= (PloSum >> preturn)
         ; SUB  >>. paren (pPlot .>> SEMI .>>. pPlot) >>= (PloSub >> preturn)
         ; DIFF >>. paren (pPlot .>> SEMI .>>. pPlot) >>= (PloDiff >> preturn)
         ; PROD >>. paren (many1 pPlot) >>= (PloProd >> preturn)
         ; DIV  >>. paren (pPlot .>> SEMI .>>. pPlot) >>= (PloDiv >> preturn)
         ; pMol >>= fun m ->
            choice  [ AT >>. paren spFloat >>= fun x -> preturn (PloMoleculeAt (m, x))
                    ; preturn (PloMolecule m) ]
         ; spName  >>= fun n ->
            choice  [ AT >>. paren spFloat >>= fun x -> preturn (PloNameAt (n, x))
                    ; UNDERSCORE >>. 
                        choice [ AT >>. paren spFloat |~> fun pos x -> PloModuleAt (n, underscoreVal (DSDParser.to_range pos), x)
                               ; preturn () |~> fun pos _ ->  PloModule (n, underscoreVal (DSDParser.to_range pos))]
                    ; paren (many Value.parse) >>= fun vs -> 
                        choice [ AT >>. paren spFloat >>= fun x -> preturn (PloModuleAt (n,vs,x))
                               ; preturn (PloModule (n,vs))]
                    ; preturn (PloName n) ] // FloatVarAExp
         ] sp


// plots directive
let pPlots sp = sepBy1 pPlot SEMI sp
let dirPlot = PLOT >>. pPlots >>= (Plot >> preturn)


let curry f (a, b) = f a b
//let valZero = Value.Float(0.0,Microsoft.Research.DNA.Types.emprange)

let pOrigami = 
  let pInsides = sepBy1 pMol BAR // >>= fun ms -> preturn (Process.Origami (List.map (Process.from_species valZero) ms, valZero))
  DLBRACKET >>. pInsides .>> DRBRACKET >>= fun ms -> 
    preturn (fun t -> Process.Origami (List.map (Process.from_species None) ms, t))

let pEvent = choice  [ pOrigami 
                     ; pMol >>= fun x -> preturn (fun t -> Process.from_species t x)
                     ; name .>>. paren (sepBy pOldValue COMMA) >>= 
                        fun (n, vs) -> preturn (fun t -> Process.Instance(n, None, vs, t)) ]

let dirEvents = EVENT >>. pEvent .>>. pOldValue .>> AT .>>. pOldValue
                  >>= (fun ((ev, amount), time) -> let procInstance = Process.Repeat(amount, false, ev (Some time), None, None)
                                                   preturn (OldEvent procInstance))

let dirSpecMax           = SPECMAX       >>. DSDParser.speciesParser .>>. spInt >>= (SpecMax >> preturn)
let dirTime              = 
  let s = Time.Seconds 1.0
  let m = Time.Seconds 6.0
  let h = Time.Seconds 3.6
  let d = Time.Seconds 8.64
  let ret = OldTime >> preturn
  TIME          >>. choice  [ kw "seconds"  >>. ret s
                            ; kw "s"        >>. ret s
                            ; kw "minutes"  >>. ret m
                            ; kw "m"        >>. ret m
                            ; kw "hours"    >>. ret h
                            ; kw "h"        >>. ret h
                            ; kw "days"     >>. ret d
                            ; kw "d"        >>. ret d ]

let dirConcentration     = 
  let m  = Concentration.Molar 0
  let mM = Concentration.Molar -3
  let uM = Concentration.Molar -6
  let nM = Concentration.Molar -9
  let pM = Concentration.Molar -12
  let fM = Concentration.Molar -15
  let aM = Concentration.Molar -18
  let zM = Concentration.Molar -21
  let yM = Concentration.Molar -24
  let ret = Concentration >> preturn 
  CONCENTRATION >>. choice  [ kw "molar"      >>. ret m
                            ; kw "M"          >>. ret m
                            ; kw "milimolar"  >>. ret mM
                            ; kw "mM"         >>. ret mM
                            ; kw "millimolar" >>. ret mM
                            ; kw "micromolar" >>. ret uM
                            ; kw "uM"         >>. ret uM
                            ; kw "nanomolar"  >>. ret nM
                            ; kw "nM"         >>. ret nM
                            ; kw "picomolar"  >>. ret pM
                            ; kw "pM"         >>. ret pM
                            ; kw "femtomolar" >>. ret fM  
                            ; kw "fM"         >>. ret fM
                            ; kw "attomolar"  >>. ret aM
                            ; kw "aM"         >>. ret aM 
                            ; kw "zeptomolar" >>. ret zM
                            ; kw "zM"         >>. ret zM
                            ; kw "yoctomolar" >>. ret yM
                            ; kw "yM"         >>. ret yM ]
let dirRelativeTolerance = RELTOLERANCE  >>. spFloat         >>= (RelTolerance >> preturn)
let dirTolerance         = TOLERANCE     >>. spFloat         >>= (Tolerance >> preturn)
let dirScale             = SCALE         >>. spFloat         >>= (Scale >> preturn)

// simulation
let dirSimulation = 
  SIMULATION >>. choice [ kw "jit"                >>. preturn (Simulation JIT)
                        ; kw "stochastic"         >>. preturn (Simulation SSA)
                        ; kw "deterministicstiff" >>. preturn (Simulation (OSLO true))
                        ; kw "deterministic"      >>. preturn (Simulation (OSLO false))
                        ; kw "spatial1d"          >>. preturn (Simulation (SPACIAL_PERIODIC 1))
                        ; kw "spatial2d"          >>. preturn (Simulation (SPACIAL_PERIODIC 2))
                        ; kw "sundials"           >>. preturn (Simulation (SUNDIALS false))
                        ; kw "sundialsstiff"      >>. preturn (Simulation (SUNDIALS true))
                        ; kw "cme"                >>. preturn (Simulation (CME_OSLO_OR_SUNDIALS false))
                        ; kw "cmestiff"           >>. preturn (Simulation (CME_OSLO_OR_SUNDIALS true))
                        ; kw "lna"                >>. preturn (Simulation (LNA_OSLO false))
                        ; kw "lnastiff"           >>. preturn (Simulation (LNA_OSLO true))              ]

let dirDT    = DT    >>. spFloat  >>= (Dt    >> preturn)
let dirXMax  = XMAX  >>. spFloat  >>= (XMax  >> preturn)
let dirNX    = NX    >>. spInt    >>= (Nx    >> preturn)
let dirTheta = THETA >>. spFloat  >>= (Theta >> preturn)

// spatial directives
let coreElem sp = choice [ kw "width" >>. spFloat >>= (CoreW >> preturn)
                         ; kw "inner" >>. spFloat >>= (CoreI >> preturn) 
                         ; kw "outer" >>. spFloat >>= (CoreO >> preturn) 
                         ; kw "species" >>. failwith "species not supported in Spatial settings, Core" ] sp
let coreRecord  = sepBy coreElem SEMI

let pointElem sp = choice  [ kw "x"      >>. spFloat >>= (PointX >> preturn)
                           ; kw "y"      >>. spFloat >>= (PointY >> preturn)
                           ; kw "width"  >>. spFloat >>= (PointW >> preturn)
                           ; kw "value"  >>. spFloat >>= (PointV >> preturn) 
                           ; kw "species" >>. failwith "species not supported in Spatial settings, Point"] sp
let pointRecord = sepBy pointElem SEMI

let speciesParser st = DSDParser.speciesParser st

let dirSpatialic = 
  SPATIALIC >>. 
    choice  [ kw "random" >>. choice
                [ speciesParser .>>. spFloat >>= (SpRandom1 >> Spatialic >> preturn)
                ; spFloat                    >>= (SpRandom2 >> Spatialic >> preturn) ]
            ; kw "centralcore" >>. 
              choice [ spFloat .>>. speciesParser .>>. spFloat .>>. spFloat 
                        >>= (fun (((x,mol), y), z) -> preturn (Spatialic (BackwardComp (mol, x, y, Some z))) )
                     ; coreRecord >>= (SpCentralCore >> Spatialic >> preturn)]
            ; kw "point" >>. pointRecord >>= (SpPointOfRecord >> Spatialic >> preturn)]
  

let dirSpatialbc = SPATIALBC >>. choice [ kw "periodic" >>. preturn (Spatialbc Periodic)
                                        ; kw "zeroflux" >>. preturn (Spatialbc ZeroFlux)]
let dirSpatialPlot = SPATIALPLOT >>. speciesParser >>= (SpatialPlot >> preturn)

let dirDiffusion        = DIFFUSION >>. speciesParser .>>. spFloat >>= (Diffusion >> preturn)
let dirDefaultDiffusion = DEFAULTDIFFUSION >>. spFloat >>= (DefaultDiffusion >> preturn)

// parameters
let paramP = 
  let f n x y z pType = choice 
                          [ kw "fixedvar"   >>. preturn (ParItem (n, x, y, z, pType, ParFixed))
                          ; kw "fixed"      >>. preturn (ParItem (n, x, y, z, pType, ParFixed))
                          ; kw "randomized" >>. preturn (ParItem (n, x, y, z, pType, ParRandomized))
                          ; kw "random"     >>. preturn (ParItem (n, x, y, z, pType, ParRandomized))
                          ; kw "init"       >>. preturn (ParItem (n, x, y, z, pType, ParInitVal))
                          ; kw "initval"    >>. preturn (ParItem (n, x, y, z, pType, ParInitVal)) ]
  spName >>= fun n ->
    choice [ EQUAL >>. spFloat >>= fun f -> preturn (ParAssign (n, f, f, f))
           ; COMMA >>. paren (spFloat .>> COMMA .>>. spFloat) .>> COMMA .>>. spFloat .>> COMMA  
             >>= fun ((x,y), z) -> 
                  choice [ kw "realspace"  >>. COMMA >>. f n x y z ParReal
                         ; kw "real"       >>. COMMA >>. f n x y z ParReal
                         ; kw "log"        >>. COMMA >>. f n x y z ParLog
                         ; kw "logspace"   >>. COMMA >>. f n x y z ParLog  ] ]

let dirParameters = PARAMETERS >>. sqBrackets (sepBy1 paramP SEMI) >>= (Params >> preturn)

// sweeps
let namesList = sepBy1 name COMMA

let valList1  = sepBy1 pOldValue COMMA
let valList   = sepBy2 pOldValue COMMA
let valListP  = paren valList
let valTuples = sepBy1 valListP COMMA

let varSweep  = choice [ paren namesList .>> EQUAL .>>. sqBrackets valTuples >>= (SweepVar >> preturn)
                       ; spName .>> EQUAL .>>. sqBrackets valList1 >>= fun (n, xs) -> 
                          preturn (SweepVar ([n], List.map (fun x -> [x]) xs)) ]
let combSweep = sepBy1 varSweep COMMA

let dirSweep = 
  SWEEP >>.
    choice [ paren namesList .>> EQUAL .>>. sqBrackets valTuples >>= 
              (SweepVar >> UnnamedSweep >> (fun x -> [x]) >> Sweeps >> preturn)
           ; spName .>> EQUAL >>= fun n ->
              choice [ braces combSweep   >>= (fun x -> preturn (Sweeps [NamedSweep (n, x)]))
                     ; sqBrackets valList1 >>= (fun x -> preturn (Sweeps [UnnamedSweep (SweepVar ([n], List.map (fun y -> [y]) x))]))  ] ]

// fit 
let dirFit = 
  FIT >>. 
    braces
      (spName .>> SEMI .>>. spName .>> SEMI >>= fun (n1, n2) ->
        choice [ sqBrackets pPlots >>= fun ps -> preturn (Fit (n1, n2, ps))
               ; pPlot             >>= fun p  -> preturn (Fit (n1, n2, [p])) ]
      )

// fit run
let fitElem = 
  choice [ kw "burnin"        >>. EQUAL >>. spInt   >>= (Burnin         >> preturn)
         ; kw "samples"       >>. EQUAL >>. spInt   >>= (SpSamples      >> preturn)
         ; kw "thin"          >>. EQUAL >>. spInt   >>= (Thin           >> preturn)  // TODO: default = findintdef "thin" 10 t
         ; kw "separatenoise" >>. EQUAL >>. spBool  >>= (SeparateNoise  >> preturn)  // TODO: default = findbooldef  true t 
         ; kw "noisemodel"    >>. EQUAL >>. spInt   >>= (NoiseModel     >> preturn)  // TODO: Constant t
         ; kw "prune"         >>. EQUAL >>. spBool  >>= (Prune          >> preturn)  // TODO: default true t }
         ]
let fitRecord = braces (sepBy1 fitElem SEMI)
let dirFitRun sp = (FITRUN >>. fitRecord >>= (FitRun >> preturn)) sp

// plot window, kinectics
let dirPlotWindow = PLOTWINDOW >>. spFloat .>>. spFloat .>>. spFloat .>>. spFloat >>= 
                      fun (((tmin, tmax), ymin), ymax) -> preturn (PlotWindow (tmin, tmax, ymin, ymax))
let dirKinectics = KINETICS >>. choice [ kw "contextual"    >>. preturn (Kinectics KContextual)
                                       ; kw "stochastic"    >>. preturn (Kinectics KStochastic)
                                       ; kw "deterministic" >>. preturn (Kinectics KDeterministic) ]

// verification
let dirVerification = 
  VERIFICATION >>.
    choice 
      [ kw "encoding" 
        >>. choice [kw "integer"    >>. preturn (Verification (Encoding 0))
                   ; kw "bitvector" >>. preturn (Verification (Encoding 1)) ]
      ; kw "abstraction"
        >>. choice [ kw "default"   >>. preturn (Verification (Abstraction 0))
                   ; kw "stutter"   >>. preturn (Verification (Abstraction 1))
                   ; kw "connected" >>. preturn (Verification (Abstraction 2))
                   ; kw "boolean"   >>. preturn (Verification (Abstraction 3)) ]
      ; kw "enumeration"
        >>. choice [ kw "first" >>. preturn (Verification (Enumeration -1))
                   ; kw "full"  >>. preturn (Verification (Enumeration -2))
                   ; kw "limit" >>. spInt >>= fun x ->
                      if x < 0 then failParser ("Positive enumeration limit required, but found " + x.ToString())
                               else preturn (Verification (Enumeration x))                    ]
      ; kw "population_bound" >>. spInt  >>= (PopBound   >> Verification >> preturn)
      ; kw "null_reaction"    >>. spBool >>= (DummyRxn   >> Verification >> preturn)
      ; kw "enforce_initial"  >>. spBool >>= (EnfInitial >> Verification >> preturn)
    ]

let dirStabilityCorrection = STABILITYCORRECTION >>. spFloat     >>= (StabilityCorrection >> preturn)
let dirCoaxialDAngle       = COAXIALDANGLE       >>. spFloat     >>= (CoaxialDAngle       >> preturn)
let dirDoubleCoaxialDangle = DOUBLECOAXIALDANGLE >>. spFloat     >>= (DoubleCoaxialDAngle >> preturn)
let dirCoaxialCorrection   = COAXIALCORRECTION   >>. Value.parse >>= (Value.to_engineValue >> CoaxialCorrection >> preturn)
let dirTemperature         = TEMPERATURE         >>. spFloat     >>= (Temperature         >> preturn)
let dirTerminalDAngle      = TERMINALDANGLE      >>. spFloat     >>= (TerminalDAngle      >> preturn)


let pOldDirectives =  [ dirCoaxialCorrection 
                      ; dirCoaxialDAngle
                      ; dirConcentration
                      ; dirDT
                      ; dirDefaultDiffusion
                      ; dirDiffusion
                      ; dirDoubleCoaxialDangle
                      ; dirDuration
                      ; dirEvents
                      ; dirFitRun
                      ; dirFit
                      ; dirKinectics
                      ; dirNX
                      ; dirParameters
                      ; dirPlot
                      ; dirPlotWindow
                      ; dirRelativeTolerance
                      ; dirSamples
                      ; dirScale
                      ; dirSeed
                      ; dirSimulation
                      ; dirSpatialbc
                      ; dirSpatialic
                      ; dirSpatialPlot
                      ; dirSpecMax
                      ; dirStabilityCorrection
                      ; dirSweep
                      ; dirTemperature
                      ; dirTerminalDAngle
                      ; dirTheta
                      ; dirTime
                      ; dirTolerance
                      ; dirVerification
                      ; dirXMax ]

let fromOpt m x = match m with
                  | None   -> x
                  | Some y -> y


let rec oldToCrnPlot : oldPlot -> Expression.t<t_plot_base> = fun p ->
  match p with
  | PloFloat       f            -> Expression.Float f
  | PloName        n            -> Expression.Key (t_plot_base.String n)   
  | PloNameAt     (n, _)        -> Expression.Key (t_plot_base.String n)   
  | PloMolecule   m             -> Expression.Key (t_plot_base.Molecule m) 
  | PloMoleculeAt (m, _)        -> Expression.Key (t_plot_base.Molecule m) 
  | PloModule     (n, mods)     -> let pattern = match mods with
                                                  | [Value.Tuple ([Value.Variable ("_", _)], _)] -> plot_pat.All
                                                  | _                                            -> plot_pat.Pats mods
                                   Expression.Key (t_plot_base.Module (n, pattern))
  | PloModuleAt   (n, mods, _)  -> Expression.Key (t_plot_base.Module (n, plot_pat.Pats mods))
  | PloSum        ps            -> Expression.Plus (List.map oldToCrnPlot ps)
  | PloSub        (p1, p2)      -> Expression.Minus {sub1 = oldToCrnPlot p1; sub2 = oldToCrnPlot p2}
  | PloDiff       (p1, p2)      -> Expression.Absolute (oldToCrnPlot (PloSub (p1, p2)))
  | PloProd       ps            -> Expression.Times (List.map oldToCrnPlot ps)
  | PloDiv        (p1, p2)      -> Expression.Divide {div1 = oldToCrnPlot p1; div2 = oldToCrnPlot p2}

let oldToCrnParam param = 
  match param with
    | ParAssign (n, f1, f2, f3) -> 
      let prior : Prior =  
        { interval     = Interval.Real
        ; variation    = Variation.Fixed
        ; distribution = Distribution.Uniform {min = f1; max = f2} }
      Parameter.create(n, f3 , Some prior)
    
    | ParItem (n, f1, f2, f3, parSpace, parVar) -> 
        let pr : Prior = { interval     = match parSpace with
                                            | ParReal -> Interval.Real
                                            | ParLog  -> Interval.Log
                           ; variation    = match parVar with
                                            | ParFixed       -> Variation.Fixed
                                            | ParRandomized  -> Variation.Random
                                            | ParInitVal     -> Variation.Initial2
                           ; distribution = Distribution.Uniform {min = f1; max = f2}}
        Parameter.create(n, f3 , Some pr)
        

let oldToCrnSweepVar (SweepVar (names, values)) = 
  Microsoft.Research.CRNEngine.Assignment.create(names, List.map (fun vs -> List.map Value.to_engineValue vs) values)


let oldToCrnSweep sidGen (sweep:SweepItem) = 
  match sweep with
  | UnnamedSweep sweep     -> let newName = sidGen ()
                              Microsoft.Research.CRNEngine.Sweep.create(newName, [oldToCrnSweepVar sweep])
  | NamedSweep (n, sweeps) -> let asns = List.map oldToCrnSweepVar sweeps
                              Microsoft.Research.CRNEngine.Sweep.create(n, asns)

type event_data = Choice<Process.t, Microsoft.Research.DNA.Species.t, string * value list>
let c1 (x : Microsoft.Research.CRNEngine.Crn_settings<Expression.t<t_plot_base>>)  
                                = Choice1Of3 x
let c2 (y : Options.t)          = Choice2Of3 y
let c3 (p : Process.t)          = Choice3Of3 p

let updateWithOldDirectives (sidGen : unit -> string) oldDir (s: Microsoft.Research.CRNEngine.Crn_settings<Expression.t<t_plot_base>>
                                                             , o: Options.t) = 
 match oldDir with
  | Samples        (startOpt, endOpt, pointsOpt)
  | DurationPoints (startOpt, endOpt, pointsOpt) ->
    c1 {s with simulation = { s.simulation with points  = fromOpt pointsOpt s.simulation.points
                                              ; initial = fromOpt startOpt  s.simulation.initial
                                              ; final   = fromOpt endOpt    s.simulation.final } }
  | Scale               f -> c1 {s with stochastic = { s.stochastic with scale = f }}
  | Plot               ps -> c1 {s with simulation = { s.simulation with plots  = List.map oldToCrnPlot ps @ s.simulation.plots}}
  | Simulation      smode -> 
    match smode with
    | JIT -> c2 (Options.set_is_jit true o)
    | SSA                     
    | OSLO _                  
    | SPACIAL_PERIODIC _      
    | SUNDIALS _              
    | CME_OSLO_OR_SUNDIALS _
    | LNA_OSLO _               ->
      c1 {s with simulator = 
                  match smode with
                  | JIT                     -> Microsoft.Research.CRNEngine.SSA
                  | SSA                     -> Microsoft.Research.CRNEngine.SSA
                  | OSLO _                  -> Microsoft.Research.CRNEngine.Oslo
                  | SPACIAL_PERIODIC _      -> Microsoft.Research.CRNEngine.PDE
                  | SUNDIALS _              -> Microsoft.Research.CRNEngine.Sundials
                  | CME_OSLO_OR_SUNDIALS _  -> Microsoft.Research.CRNEngine.CME
                  | LNA_OSLO _              -> Microsoft.Research.CRNEngine.LNA
               ; deterministic = { 
                  s.deterministic with 
                   stiff = match smode with
                            | OSLO                  isStiff -> isStiff
                            | LNA_OSLO              isStiff -> isStiff
                            | SUNDIALS              isStiff -> isStiff
                            | CME_OSLO_OR_SUNDIALS  isStiff -> isStiff
                            | SPACIAL_PERIODIC      _       -> false
                            | SSA                           -> false
                            | JIT                           -> false
                }
              ; simulation = s.simulation 
         }// TODO: add dimensions to spatial settings
  | RelTolerance        f -> c1 { s with deterministic = { s.deterministic with reltolerance = f }}
  | Tolerance           f -> c1 { s with deterministic = { s.deterministic with abstolerance = f }}
  | Seed                i -> c1 { s with simulation    = { s.simulation with seed = Some i}}
  | OldEvent            p -> c3 p 
  | SpecMax             _ -> failwith "specMax not supported"  
  | OldTime             _ -> c1 s 
  | Concentration       _ -> c1 s 
  | Dt                  f -> c1 { s with spatial = { s.spatial with dt   = f }}
  | XMax                f -> c1 { s with spatial = { s.spatial with xmax = f }}
  | Nx                  i -> c1 { s with spatial = { s.spatial with nx   = i }}
  | Theta               _ -> failwith "theta directive not supported in the spatial simulator"
  | Spatialic           _ -> failwith "spatial initial concentration not supported"
//      c1 
//       { s with 
//          spatial = 
//            { s.spatial with 
//               initial = match sicSetting with
//                         | SpRandom1 (m, f)    -> failwith "random core with molecule not supported"  
//                         | SpRandom2   f       -> SpS.initial.Random f
//                         | SpCentralCore cs    -> failwith "random core with molecule not supported"
//                         | SpPointOfRecord ps  -> failwith "random core with molecule not supported"
//                         | BackwardComp (a, b, c, d) -> failwith "random core with molecule not supported"
//            } }
  | Spatialbc sbcSetting  ->
    c1 { s with 
            spatial = { s.spatial with 
                          boundary = match sbcSetting with
                                     | Periodic -> Boundary.Periodic
                                     | ZeroFlux -> Boundary.ZeroFlux
                                     } }
  | SpatialPlot       mol -> c1 { s with simulation = { s.simulation with plots = (oldToCrnPlot (PloMolecule mol)) :: s.simulation.plots } }
  | Diffusion           _ -> failwith "directive \"diffusion\" not supported"
  | DefaultDiffusion    _ -> failwith "directive \"default diffusion\" not supported"
  | Params             ps -> c1 { s with parameters = List.map oldToCrnParam ps }
  | Sweeps             ss -> c1 { s with sweeps     = List.map (oldToCrnSweep sidGen) ss }
  | Fit _(*(n1, n2, ps)*) -> failwith "directive \"fit\" not supported "
  | FitRun             ps ->  
      let updateSpSet (inf:Inference_settings) spParam = 
        match spParam with
        | Burnin        i -> { inf with burnin  = i}
        | SpSamples     i -> { inf with samples = i}
        | Thin          i -> { inf with thin    = i }
        | SeparateNoise b -> { inf with noise_parameter = if b then Noise_parameter.Multiple else Noise_parameter.Random}
        | NoiseModel    i -> { inf with noise_model = match i with
                                                      | 0 -> Noise_model.Constant
                                                      | 1 -> Noise_model.Proportional
                                                      | _ -> failwith "Unrecognised noise model. Use linear or proportional." }
        | Prune         b -> { inf with prune = b}
      c1 { s with inference = List.fold updateSpSet s.inference ps }
  | PlotWindow          _ -> failwith "plot window not supported"
  | Kinectics       kmode -> 
      c1 { s with simulation = { s.simulation with kinetics = 
                                                      match kmode with
                                                      | KContextual    -> Kinetics.Contextual
                                                      | KStochastic    -> Kinetics.Stochastic
                                                      | KDeterministic -> Kinetics.Deterministic
                                                    } }
  | Verification        _ -> failwith "directive \"verification\" not supported"
  | StabilityCorrection f -> c2 ( Options.setStabilityCorrection  f o )
  | CoaxialDAngle       f -> c2 ( Options.setCoaxialDangle        f o )
  | DoubleCoaxialDAngle f -> c2 ( Options.setDoubleCoaxialDangle  f o )
  | CoaxialCorrection   v -> c2 ( Options.setCoaxialCorrection    v o ) // TODO: in parser.mly, v can also be UNDERSCORE
  | Temperature         f -> c2 ( Options.setTemperature          o f )
  | TerminalDAngle      f -> c2 ( Options.setTerminalDangleFactor o f )

(**************************************************)

let oldDirective = 
  choice (List.map (fun odp -> odp >>= (Choice1Of2 >> preturn)) pOldDirectives
          @ (List.map (fun op -> op >>= (Choice2Of2 >> preturn)) Options.optionParsers))


let oldDsParser evs = 
  choice
    [ "def" &> DSDParser.parse_name .>> spaces >>= fun name -> 
        (("=" &> pValOrTuple ) >>= fun v -> preturn (DSDParser.Syn_val (name, v)))
        <|>
        (DSDParser.parse_parameters +>>+ ("=" &> oldProc)
          |~> fun pos (ps, p) -> DSDParser.Syn_def (name, DSDParser.to_range pos, ps, p))
    ; "new" &> DSDParser.restriction |>> fun r -> DSDParser.Syn_new (r, true)
    ; "dom" &> DSDParser.int_name   +>>+ ("=" &> DSDParser.record)      |>> fun r -> DSDParser.Syn_new (r, false)
    ; kw "predicate" >>= (fun x -> failwith ("\""+ x + "\" keyword not supported"))
    ; kw "query"     >>= (fun x -> failwith ("\""+ x + "\" keyword not supported"))
    ; oldProc >>= fun ps -> match ps, evs with 
                              | _, [] -> preturn (DSDParser.Syn_proc ps)
                              | Process.Parallel pars, evs -> preturn (DSDParser.Syn_proc (Process.Parallel (pars @ evs)))
                              | _, _  -> preturn (DSDParser.Syn_proc (Process.Parallel (ps :: evs))) 
    ] 
  |> DSDParser.seq1 
  |>> DSDParser.to_syntax
 
let parserOld (sidGen: unit -> string) : Parser.t< Microsoft.Research.DNA.Syntax.t 
                                            * Microsoft.Research.CRNEngine.Crn_settings<Expression.t<t_plot_base>> 
                                            * Options.t> =
  spaces >>. many (kw "directive" >>. oldDirective) >>= 
    (fun oldSettings -> 
      let updateDefs (cs, opts, es) ch = 
        match ch with
          | Choice1Of2 oldDir -> 
              match updateWithOldDirectives sidGen oldDir (cs, opts) with
                | Choice1Of3 cs'   -> (cs', opts,  es)
                | Choice2Of3 opts' -> (cs,  opts', es)
                | Choice3Of3 e     -> (cs,  opts,  e::es)
          | Choice2Of2 o -> (cs, List.fold Options.update_options opts o, es)
      let (settings, opts, evs) = List.fold updateDefs (Microsoft.Research.CRNEngine.Crn_settings.defaults, Options.default_options, []) oldSettings
      oldDsParser evs >>= (
        fun dsd -> 
          preturn (dsd, settings, opts)
      )) .>> spaces

let freshSweepIdGenerator () = 
  let sweep_id = ref 0
  let new_sweep_name () =
    let id = !sweep_id + 1 in
    sweep_id := id
    "sweep_" + id.ToString() // the hard-coded "sweep_" is from the old DNA solution
  new_sweep_name

let parseOld _ = 
  let sidGen = freshSweepIdGenerator ()
  Parser.run (parserOld sidGen) 

(* converts a DSD program written in the Silver Light syntax to Classic DSD syntax *)
let convertSL slCode : string =
  // create a fresh sweep_id generator
  let sidGen = freshSweepIdGenerator ()

  // parse code in the old syntax
  let (z, cs, opts) = match Parser.run (parserOld sidGen) slCode with
                          | Parser.Success ((z, cs, opts), _, _) -> (z, cs, opts)
                          | Parser.Failure (msg,           _, _) -> raise (System.Exception msg)

  let plotPrinter (x:Syntax.t_plot_base) :string = 
    match x with
      | Syntax.t_plot_base.String s      -> s
      | Syntax.t_plot_base.Molecule m    -> "[" + Species.display m + "]"
      | Syntax.t_plot_base.Module (n, pats) -> 
          "[" + n + match pats with
                    | Syntax.plot_pat.All     -> " _]"
                    | Syntax.plot_pat.Pats ps -> "(" + String.concat ", " (List.map Value.to_string ps) + ")]" // + " " +")"
  let plotAlonePrinter (x:Syntax.t_plot_base) :string = 
    match x with
      | Syntax.t_plot_base.String s      -> s
      | Syntax.t_plot_base.Molecule m    -> Species.display m
      | Syntax.t_plot_base.Module (n, pats) -> 
          n + match pats with
              | Syntax.plot_pat.All     -> " _"
              | Syntax.plot_pat.Pats ps -> "(" + String.concat ", " (List.map Value.to_string ps) + ")" // + " " +")"
  let dsdSettings = cs.to_string 
                      (Expression.to_string plotPrinter) 
                      (fun e -> match e with 
                                | Expression.Key x -> plotAlonePrinter x
                                | _ ->  e |> Expression.to_string plotPrinter)
  let dsdOptions  = Options.display opts
  let dsdSyntax   = Syntax.display z
  dsdSettings + dsdOptions + dsdSyntax