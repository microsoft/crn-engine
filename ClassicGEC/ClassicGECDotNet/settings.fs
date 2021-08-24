// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.GEC.Settings
open Microsoft.Research.CRNEngine
open Microsoft.Research.GEC.Ast
open Parser
//open FsCheck

type crnModuleDefinition = (string * string list)
type crnInstructions = ((crnModuleDefinition * Instruction list) list) * Instruction list


let pSpecies = GecSpecies.parse_crn_species

type Directive =
  | Crn of crnInstructions
  | Override of crnInstructions
  | RmrnaDeg of float
  | Rules of LogicGEC.Semantics
  static member parse_defaults (settings:Crn_settings<'e>) =
    //let parse_species = pSpecies - Clashes with key words...
    let parse_species = (GecSpecies.parse_kw Microsoft.Research.CRNEngine.Keywords.kwList) |>> fun x ->  x.to_string() |> Species.create 
    let CRN = Parser.kw "crn"
    let OVERRIDE = Parser.kw "override"
    let RMRNADEG = Parser.kw "RMRNADeg"
    let RULES = Parser.kw "rules"
    let parse_crn = CRN 
    Parser.choice[
        CRN >>. Parser.braces (Instruction.parse parse_species settings) |>> fun x -> Crn x
        OVERRIDE >>. Parser.braces (Instruction.parse parse_species settings) |>> fun x -> Override x
        RMRNADEG >>. Parser.pfloat .>> Parser.spaces |>> fun x -> RmrnaDeg x
        RULES >>. Parser.braces (LogicGEC.pGecProgram) |>> Rules
    ]
  static member parse = 
    Directive.parse_defaults Crn_settings.defaults

type Gec_settings = {directives:Directive<Functional> list; crn:crnInstructions; overrideCrn:bool; rmrnadeg:float; rules:LogicGEC.Semantics option}
with 
  static member default_settings = {directives=[]; crn=([],[]); overrideCrn=false;rmrnadeg=0.001;rules=None}
  member default_settings.from_directive (gecDirective:Directive)=
    match gecDirective with 
    | Crn (x) -> {default_settings with crn = x; overrideCrn = false}
    | Override(x) -> {default_settings with crn = x; overrideCrn = true}
    | RmrnaDeg(x) -> {default_settings with rmrnadeg = x}
    | Rules x -> {default_settings with rules = Some x}
  member default_settings.from_directive_list (ds:Directive list) = 
    ds |> List.fold (fun (acc:Gec_settings) s -> (acc.from_directive s)) default_settings


let parse_species:Parser.t<Functional> = Expression.parse (Key.parse pSpecies)
  ///Special case: a single name x is interpreted as a species instead of a parameter
  ///e.g. "plots = [x]"
let parse__species_plot:Parser.t<Functional> = 
  let species_from_string = Parser.from_string pSpecies 
  Expression.parse (Key.parse pSpecies)
  |>> fun exp -> 
        match exp with
        | Expression.Key (Key.Parameter s) -> Expression.Key (Key.Species (species_from_string s))
        | _ -> exp

type directiveType = 
  | CRNDir of Microsoft.Research.CRNEngine.Directive<Functional>
  | GECDir of Directive

let parseCrnDir = Directive<Functional>.parse parse_species parse__species_plot |>> CRNDir

let parseGecDir_defaults (settings:Crn_settings<Functional>) = Directive.parse_defaults settings |>> GECDir
let parseGecDir = parseGecDir_defaults Crn_settings.defaults

let parse_defaults (default_crn_settings:Crn_settings<Functional>) = 
  Parser.many (Parser.kw "directive" >>. ( (parseGecDir_defaults default_crn_settings) <|> parseCrnDir))
  |>> fun(dirlist) -> 
    let crnlist = dirlist |> List.choose(fun elem -> 
                    match elem with 
                    | CRNDir(x) -> Some(x)
                    | _ -> None)
    let geclist = dirlist |> List.choose(fun elem -> 
                    match elem with 
                    | GECDir(x) -> Some(x)
                    | _ -> None)
    //let crnSettings = default_crn_settings.from_default_directive_list crnlist
    let settings = { Gec_settings.default_settings with directives = crnlist}
    settings.from_directive_list geclist
    

let parse = parse_defaults Crn_settings.defaults 


let convertExprToAst (exp:Functional) = 
    
    
    let gecExp = Parser.from_string (GecSpecies.parse) 
    let f s = gecExp (Species.to_string s) |> fun x -> x.to_ast_gecSpecies()
    Expression.map (Key.map f) exp
    (*match exp with 
    | Expression (Key.Species s) ->
      let gecExp = Parser.from_string (GecSpecies.parse) 
      let gecSpecies = gecExp (Species.to_string s) |> fun x -> x.to_ast_gecSpecies()
      Expression.Key(gecSpecies)
    
    | _ -> failwith "Unexpected expression encountered in directive functional"*)
    
    
        
let convertCRNdirToGECdir (directive:Directive<Functional>) =
    match directive with
    | Simulation x -> 
      let plotSpecies = x.plots |> List.map convertExprToAst 
      let plots = Ast.PLOT(plotSpecies)
      let kinetics = 
        match x.kinetics with 
        | Kinetics.Contextual -> Ast.KINETICS(Ast.Contextual_kinetics)
        | Kinetics.Stochastic -> Ast.KINETICS(Ast.Stochastic_kinetics)
        | Kinetics.Deterministic -> Ast.KINETICS(Ast.Deterministic_kinetics)
      [Ast.SAMPLE(x.final,Ast.IntPoints(x.points));kinetics;plots]
    | Units x -> [Ast.TIME(x.time);Ast.CONCENTRATION(x.concentration)]
    | Microsoft.Research.CRNEngine.Directive.Deterministic x -> [Ast.ABSTOLERANCE(x.abstolerance);Ast.RELTOLERANCE(x.reltolerance)]
    | Microsoft.Research.CRNEngine.Directive.Stochastic x -> [Ast.SCALE(x.scale)]
    | _ -> []
    (*| Samples(startVal,endVal,inc) ->
        if(inc.IsSome) then
            if(inc.Value = 0) then
                Ast.SAMPLE(endVal.Value,Ast.AllPoints) 
            else
                Ast.SAMPLE(endVal.Value,Ast.IntPoints(inc.Value))
        else
            Ast.SAMPLE(endVal.Value,Ast.Default)
    | OldTime(sec) -> Ast.TIME(sec)
    | Concentration(conc) -> Ast.CONCENTRATION(conc)
    | Tolerance(value) -> Ast.ABSTOLERANCE(value)
    | RelTolerance(value) -> Ast.RELTOLERANCE(value)
    | Scale(value) -> Ast.SCALE(value)
    | Kinectics(mode) -> 
        match mode with 
        | KContextual -> Ast.KINETICS(Ast.Contextual_kinetics)
        | KStochastic -> Ast.KINETICS(Ast.Stochastic_kinetics)
        | KDeterministic -> Ast.KINETICS(Ast.Deterministic_kinetics)
    | Plot(expList) ->
        let gecExpList = expList |> List.map convertExprToAst 
        Ast.PLOT(gecExpList)
    | _ -> failwith "Unknown Directive encountered"*)


let convert_crn_to_gec_directives (directives :Directive<Functional> list) = 
  let list = directives |> List.map convertCRNdirToGECdir
  list |> List.fold (fun acc x -> acc@x) []