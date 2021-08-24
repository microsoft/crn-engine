// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.LegacyParser

open Parser

(**********************************************************************)
(**********************************************************************)
(************* Legacy CRN parser **************************************)
(**********************************************************************)
(**********************************************************************)
(* The code to parse directives in this section is duplicated from 
   SLConversion in Classic DSD. The original Lex grammar for the
   Silver Light version of CRN and DSD were also duplicated 
   (ModellingEngineDotNet had its own parser.mly file, which was copied 
   and modified over in SLDNADotNet). The grammar for CRN directives 
   and species should be the same, but since there might 
   be subtle differences in the two copies of the mly files, 
   the conversion tool is also duplicated here.                       *)
(**********************************************************************)

// utilities
let spaces      = Parser.spaces
let choice      = Parser.choice
let kw          = Parser.kw
let preturn     = Parser.preturn
let spFloat     = Parser.pfloat .>> spaces
let spInt       = Parser.pint32 .>> spaces
let spBool      = choice [ kw "true"  >>. preturn true
                         ; kw "false" >>. preturn false] .>> spaces
let spName      = Parser.name .>> spaces
let paren       = Parser.paren
let braces      = Parser.braces
let sqBrackets  = Parser.sqBrackets
let sepBy       = Parser.sepBy
let sepBy1      = Parser.sepBy1

let fpMsg       = "Floating point conversion not supported"

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

type ParSpace = ParReal | ParLog
type ParVar   = ParFixed | ParRandomized | ParInitVal
type ParItem  = ParAssign of string * float * float * float
              | ParItem   of string * float * float * float * ParSpace * ParVar

type SweepVar = SweepVar of string list * Value list list

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
                  | Plot                of Expression.t<Species> list
                  | Simulation          of oldSimMode
                  | RelTolerance        of float    
                  | Tolerance           of float    // maps to abstolerance in deterministic
                  | Seed                of int32
                  // | SpecMax             of Species * int32
                  | OldTime             of Time
                  | Concentration       of Concentration
                  | Dt                  of float
                  | XMax                of float
                  | Nx                  of int32
                  | Theta               of float
                  | Params              of ParItem list
                  | Sweeps              of SweepItem list
                  | FitRun              of SpParam list
                  | Kinectics           of KMode
//                  | Verification        of VerMode
//                  | StabilityCorrection of float
//                  | CoaxialDAngle       of float
//                  | DoubleCoaxialDAngle of float
//                  | Temperature         of float
//                  | TerminalDAngle      of float

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
    choice  [ COMMA >>. Expression.parse spFloat >>= fun y ->
                choice  [ ALL  >>. preturn (Some (Expression.eval id x), Some (Expression.eval id y), Some 0)
                        ; spInt >>= fun z -> preturn (Some (Expression.eval id x), Some (Expression.eval id y), Some z)
                        ; preturn  (Some (Expression.eval id x), Some (Expression.eval id y), None) ]        
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
  // TODO: double check that "--(cogs)" | "--(+ cogs)" | "--(cogs+)" is not used anymore

// plots
// TODO check empty plot "()" is parsed correctly with many or sepBy, double check that many1 or sepBy1 is not misused
let pPlot pSpecies = Expression.parse pSpecies


// plots directive
let pPlots pSpecies sp = Parser.sepBy1 (pPlot pSpecies) SEMI sp
let dirPlot pSpecies = PLOT >>. pPlots pSpecies >>= (Plot >> preturn)


let curry f (a, b) = f a b

//let dirSpecMax           = SPECMAX       >>. DSDParser.speciesParser .>>. spInt >>= (SpecMax >> preturn)
let dirTime = 
  let s = Seconds 1.0
  let m = Seconds 6.0
  let h = Seconds 3.6
  let d = Seconds 8.64
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
  let m  = Molar 0
  let mM = Molar -3
  let uM = Molar -6
  let nM = Molar -9
  let pM = Molar -12
  let fM = Molar -15
  let aM = Molar -18
  let zM = Molar -21
  let yM = Molar -24
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
// TODO: Add spatial directives
let spatialErrorMsg _ = failwith "spatial directives not supported"
let dirSpatialic = SPATIALIC >>= spatialErrorMsg
    
  

let dirSpatialbc   = SPATIALBC   >>= spatialErrorMsg
let dirSpatialPlot = SPATIALPLOT >>= spatialErrorMsg

let dirDiffusion        = DIFFUSION        >>= spatialErrorMsg
let dirDefaultDiffusion = DEFAULTDIFFUSION >>= spatialErrorMsg

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
           ; COMMA >>. Parser.paren (spFloat .>> COMMA .>>. spFloat) .>> COMMA .>>. spFloat .>> COMMA  
             >>= fun ((x,y), z) -> 
                  choice [ kw "realspace"  >>. COMMA >>. f n x y z ParReal
                         ; kw "real"       >>. COMMA >>. f n x y z ParReal
                         ; kw "log"        >>. COMMA >>. f n x y z ParLog
                         ; kw "logspace"   >>. COMMA >>. f n x y z ParLog  ] ]

let dirParameters = PARAMETERS >>. Parser.sqBrackets (sepBy1 paramP SEMI) >>= (Params >> preturn)

// sweeps
let namesList = Parser.sepBy1 spName COMMA

let valList   = Parser.sepBy1 (Expression.parse spName) COMMA
let valListP  = Parser.paren valList
let valTuples = Parser.sepBy1 valListP COMMA

let varSweep  = choice [ Parser.paren namesList .>> EQUAL .>>. Parser.sqBrackets valTuples 
                            >>= (SweepVar >> preturn)
                       ; spName .>> EQUAL .>>. Parser.sqBrackets valList >>= fun (n, xs) -> 
                          preturn (SweepVar ([n], List.map (fun x -> [x]) xs)) ]
let combSweep = Parser.sepBy1 varSweep COMMA

let dirSweep = 
  SWEEP >>.
    choice [ paren namesList .>> EQUAL .>>. sqBrackets valTuples >>= 
              (SweepVar >> UnnamedSweep >> (fun x -> [x]) >> Sweeps >> preturn)
           ; spName .>> EQUAL >>= fun n ->
              choice [ braces combSweep   >>= (fun x -> preturn (Sweeps [NamedSweep (n, x)]))
                     ; sqBrackets valList >>= (fun x -> preturn (Sweeps [UnnamedSweep (SweepVar ([n], [x]))]))  ] ]

// fit 
let dirFit = FIT >>= fun _ -> failwith "fit not supported"

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
let dirPlotWindow = PLOTWINDOW >>= fun _ -> failwith "plot window not supported"
let dirKinectics = KINETICS >>. choice [ kw "contextual"    >>. preturn (Kinectics KContextual)
                                       ; kw "stochastic"    >>. preturn (Kinectics KStochastic)
                                       ; kw "deterministic" >>. preturn (Kinectics KDeterministic) ]

// verification
let dirVerification = VERIFICATION >>= fun _ -> failwith "verification not supported"


let pOldDirectives pSpecies =  [ dirConcentration
                      ; dirDT
                      ; dirDefaultDiffusion
                      ; dirDiffusion
                      ; dirDuration
                      ; dirFit
                      ; dirFitRun
                      ; dirKinectics
                      ; dirNX
                      ; dirParameters
                      ; dirPlot pSpecies
                      ; dirPlotWindow
                      ; dirRelativeTolerance
                      ; dirSamples
                      ; dirScale
                      ; dirSeed
                      ; dirSimulation
                      ; dirSpatialbc
                      ; dirSpatialic
                      ; dirSpatialPlot
                      ; dirSweep
                      ; dirTheta
                      ; dirTime
                      ; dirTolerance
                      ; dirVerification
                      ; dirXMax ]
let oldDirective pSpecies = choice (pOldDirectives pSpecies)

let oldToCrnParam param = 
  match param with
    | ParAssign (n, f1, f2, f3) -> 
        Parameter.create(
          name  = n,
          value = f3,
          prior = Some { interval     = Interval.Real
                         ; variation    = Variation.Fixed
                         ; distribution = Distribution.Uniform{min=f1;max=f2} } 
        )
    | ParItem (n, f1, f2, f3, parSpace, parVar) -> 
        let pr : Prior = { interval     = match parSpace with
                                            | ParReal -> Interval.Real
                                            | ParLog  -> Interval.Log
                           ; variation    = match parVar with
                                            | ParFixed       -> Variation.Fixed
                                            | ParRandomized  -> Variation.Random
                                            | ParInitVal     -> Variation.Initial2
                           ; distribution = Distribution.Uniform{min=f1;max=f2}}
        let par = Parameter.create(name  = n, value = f3, prior = Some pr)
        par
    

let oldToCrnSweepVar (SweepVar (names, values)) : Assignment = Assignment.create(names,values)


let sweep_id = ref 0
let new_sweep_name () =
  let id = !sweep_id + 1 in
  sweep_id := id
  "sweep_" + id.ToString() // the hard-coded "sweep_" is from the old DNA solution

let oldToCrnSweep (sweep:SweepItem) = 
  match sweep with
  | UnnamedSweep sweep     -> let newName = new_sweep_name()
                              Sweep.create(newName,[oldToCrnSweepVar sweep])
  | NamedSweep (n, sweeps) -> let asns = List.map oldToCrnSweepVar sweeps
                              Sweep.create(n,asns)


let fromOpt m x = match m with
                  | None   -> x
                  | Some y -> y

let updateSettings (cs : Crn_settings<Functional> )oldDir  = 
 match oldDir with
  | Samples        (startOpt, endOpt, pointsOpt)
  | DurationPoints (startOpt, endOpt, pointsOpt) ->
    {cs with simulation = { cs.simulation with points  = fromOpt pointsOpt  cs.simulation.points
                                              ; initial = fromOpt startOpt  cs.simulation.initial
                                              ; final   = fromOpt endOpt    cs.simulation.final } }
  | Scale               f -> { cs with stochastic = { cs.stochastic with scale = f }}
  | Plot               ps -> { cs with simulation = { cs.simulation with plots  = List.map (Expression.map Key.Species) ps @ cs.simulation.plots}}
  | Simulation      smode -> 
    {cs with simulator = 
                match smode with
                | JIT                     -> Simulator.SSA
                | SSA                     -> Simulator.SSA
                | OSLO _                  -> Simulator.Oslo
                | SPACIAL_PERIODIC _      -> Simulator.PDE
                | SUNDIALS _              -> Simulator.Sundials
                | CME_OSLO_OR_SUNDIALS _  -> Simulator.CME
                | LNA_OSLO _              -> Simulator.LNA
             ; deterministic = { 
                cs.deterministic with 
                 stiff = match smode with
                          | OSLO                  isStiff -> isStiff
                          | LNA_OSLO              isStiff -> isStiff
                          | SUNDIALS              isStiff -> isStiff
                          | CME_OSLO_OR_SUNDIALS  isStiff -> isStiff
                          | SPACIAL_PERIODIC      _       -> false
                          | SSA                           -> false
                          | JIT                           -> false
             }
             ; simulation = match smode with 
                            | JIT                     -> failwith "JIT simulation not supported for CRNs."
                            | SSA                     -> cs.simulation
                            | OSLO _                  -> cs.simulation 
                            | SPACIAL_PERIODIC _      -> cs.simulation 
                            | SUNDIALS _              -> cs.simulation 
                            | CME_OSLO_OR_SUNDIALS _  -> cs.simulation 
                            | LNA_OSLO _              -> cs.simulation 
       }// TODO: add dimensions to spatial settings
  | RelTolerance        f -> { cs with deterministic = { cs.deterministic with reltolerance = f }}
  | Tolerance           f -> { cs with deterministic = { cs.deterministic with abstolerance = f }}
  | Seed                i -> { cs with simulation    = { cs.simulation with seed = Some i}}
  | OldTime             _ -> cs 
  | Concentration       _ -> cs 
  | Dt                  f -> { cs with spatial = { cs.spatial with dt   = f }}
  | XMax                f -> { cs with spatial = { cs.spatial with xmax = f }}
  | Nx                  i -> { cs with spatial = { cs.spatial with nx   = i }}
  | Theta               _ -> failwith "theta directive not supported in the spatial simulator"
  | Params             ps -> { cs with parameters = List.map oldToCrnParam ps }
  | Sweeps             ss -> { cs with sweeps     = List.map oldToCrnSweep ss }
  | FitRun             ps ->  
      let updateSpSet (inf:Inference_settings) spParam = 
        match spParam with
        | Burnin        i -> { inf with burnin  = i}
        | SpSamples     i -> { inf with samples = i}
        | Thin          i -> { inf with thin    = i }
        | SeparateNoise b -> { inf with noise_parameter = if b 
                                                            then Multiple 
                                                            else Random}
        | NoiseModel    i -> { inf with noise_model = match i with
                                                      | 0 -> Constant
                                                      | 1 -> Proportional
                                                      | _ -> failwith "Unrecognised noise model. Use linear or proportional." }
        | Prune         b -> { inf with prune = b}
      { cs with inference = List.fold updateSpSet cs.inference ps }
  | Kinectics       kmode -> 
      { cs with simulation = 
                  { cs.simulation with kinetics = 
                                        match kmode with
                                        | KContextual    -> Kinetics.Contextual
                                        | KStochastic    -> Kinetics.Stochastic
                                        | KDeterministic -> Kinetics.Deterministic } }

let parseOldDirectives pSpecies : Parser.t<Crn_settings<Functional>> =
  Parser.many (Parser.kw "directive" >>. oldDirective pSpecies) 
  .>> spaces
  >>= (fun oldSettings -> let def : Crn_settings<Functional> = Crn_settings.defaults
                          let settings = List.fold updateSettings def oldSettings
                          preturn settings)
  
(* legacy CRN parser, parses the body of a CRN written in the Silver Light tool's syntax.
   The parser is parametric to the species, to allow species names such as '"<a b c>"'
   for testing purposes in Classic DSD. *)

type key          = Key<Species>
type expression   = Expression.t<key>
type reaction     = Reaction<Species,Value,expression>
type initial      = Initial<Species,Value>

type instruction =
  | Reaction of reaction
  | Initial2  of initial

let convert_instructions (instructions:instruction list) =
  let f (reactions,initials) command =
    match command with
    | Reaction reaction -> reaction::reactions, initials
    | Initial2 initial -> reactions, initial::initials
  let reactions, initials = List.fold f ([],[]) instructions
  List.rev reactions, List.rev initials

let create_from_instructions (settings:Crn_settings<Functional>) (instructions:instruction list) = 
  let reactions,initials = convert_instructions instructions
  Crn.create "" settings reactions initials Stringmap.empty true

let parse_legacy_SL pSpecies = 
  let zeroVal (init : float) = Expression.Float init
  let zero                   = Expression.zero
  let unitVal                = Expression.Float 1.0
  
  // base parsers
  let pValue = Expression.parse (Parser.name .>> Parser.spaces)
  let pExpr  = Expression.parse (Key.parse pSpecies)

  // crn parsers
  let pInitial  = Initial<Species,Value>.parse pSpecies pValue zero |>> Initial2
  let pConstant = Parser.kw "constant" >>. pSpecies .>>. pValue >>= fun (sp, v) -> Parser.preturn (Initial.create(true, v, sp, None, None) |> Initial2)
  let pEvent    = Parser.kw "event" >>. pSpecies .>>. pValue .>> Parser.kw "@" .>>. pValue 
                  >>= fun ((sp, amount), time) -> Parser.preturn (Initial.create(false, amount, sp, Some time, None) |> Initial2)
  let pReaction = Reaction.parse pSpecies pValue pExpr unitVal  |>> Reaction
  let pBar      = Parser.kw "|"
  let pLine     = (pConstant <|> pEvent <|> pInitial) <|> pReaction
  
  // full CRN parser
  parseOldDirectives pSpecies >>= fun settings ->
  Parser.spaces >>. Parser.opt pBar
                >>. Parser.sepBy pLine pBar
                |>> create_from_instructions settings