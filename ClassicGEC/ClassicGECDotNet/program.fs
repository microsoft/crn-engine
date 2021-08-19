[<JavaScript>]
module Microsoft.Research.GEC.Program

open Parser
open Microsoft.Research.GEC.Ast
open Microsoft.Research.GEC.Trans
open Microsoft.Research.GEC.LogicGEC
open Microsoft.Research.CRNEngine
open Microsoft.Research.GEC.DirectivesParser
open RulesDSD.Syntax
//open FsCheck

//open Microsoft.Research.CRNEngine.InferenceSiteGraph


type moduleDefinition = string * (string list)
type moduleInvocation = string * (value list)

type deviceDefinition = moduleDefinition * (moduleInvocation list)

type crnModules = ((string * string list) * Microsoft.Research.CRNEngine.Instruction list) list // * Instruction list

type igPriorType  = 
    | Fixed
    | Normal
    | TruncatedNormal

type igPriorOption =
    | HasPrior of igPriorType
    | NoPrior 

type igPrior = string * igPriorOption

type igName = string list

type igedge = igName  * (igPrior list) * igName
type ignode = string * (string list) * (Inference_settings option)



type igraphElement = 
     | Node of ignode
     | Edge of igedge


type systemReference = 
    | System of string
    | NoSystem

type system = {
  name:string; 
  withSystem:systemReference; 
  settings:Settings.Gec_settings; 
  modules:crnModules;
  devices:deviceDefinition list;
  prog:Ast.prog}

type ClassicProgram = {
  settings:Settings.Gec_settings; 
  modules:crnModules;
  templates:prog;
  devices:deviceDefinition list;
  systems:system list;
  prog:Ast.prog;
  graph:igraphElement list
  }

type LogicProgram =  {
  settings  : Settings.Gec_settings; 
  rules     : Semantics;
  program   : LogicGEC.Instruction list;
}

type t = ClassicGec of ClassicProgram 
       | LogicGec of LogicProgram

let isLogicGecProgram (prog:t) = match prog with LogicGec _ -> true | _ -> false

let MODULE = Parser.kw "module"
let PROM = Parser.kw "prom"
let RBS = Parser.kw "rbs"
let PCR = Parser.kw "pcr"
let TER = Parser.kw "ter"
let POS = Parser.kw "pos"
let NEG = Parser.kw "neg"
let CON = Parser.kw "con"
let INITPOP = Parser.kw "initPop"
let NEW = Parser.kw "new"
let DIRECTIVE = Parser.kw "directive"
let MODEL = Parser.kw "model"
let DEVICE = Parser.kw "device"
let TEMPLATE = Parser.kw "template"
let SYSTEM = Parser.kw "system"
let PIPE = Parser.kw "|"
let COMMA = Parser.kw ","
let DOT = Parser.kw "."

let pPipeLookAhead = Parser.pTry(Parser.pstring "||" >>. Parser.failParser "single expected" <|> Parser.pstring "|")
let PARALLEL = Parser.choice [
                                 pPipeLookAhead
                                 Parser.pstring "||"
                             ] .>> Parser.spaces


//let speciesnameparser = Parser.pTry(Parser.kw "_" >>. Parser.failParser "Reserved as wildcard keyword" <|> (Parser.many1Satisfy Parser.isLetter .>>. Parser.manySatisfy (fun c -> Parser.isLetter c || Parser.isDigit c || c = '_'|| c = '['|| c = ']'|| c = '-' || c = '\'') |>> fun (a,b) -> a + b) <?> "an identifier")



let oldDirectiveLineParser = (DIRECTIVE >>. (DirectivesParser.oldDirective GecSpecies.parse_crn_species))
//let directiveParser = oldDirectiveLineParser |>> convertOldDirectiveToGECDirective
let directiveParser = oldDirectiveLineParser 

let parse_id_ns = (Parser.name_kw Microsoft.Research.GEC.Keywords.kwList)
let parse_id = parse_id_ns .>> Parser.spaces

let pSepLookAhead = Parser.pTry(Parser.pstring "::" >>. Parser.failParser "Unexpected Species separator found" <|> Parser.kw ":")
let parse_brickid = parse_id .>> pSepLookAhead

let parse_species_id = GecSpecies.parse_species |>> fun(splist) -> {GecSpecies.t.empty_Species with species = splist}.to_string()





let rec convertExpToAst (exp:Expression.t<string>)=
    match exp with 
    | Expression.Float(float) -> Ast.FloatAExp(float)
    | Expression.Key(key) -> Ast.IdAExp(key)
    | Expression.Plus(plus) -> 
        match plus with first::second::remaining ->
                        let plusfirst = Ast.PlusAExp(convertExpToAst(first),convertExpToAst(second))
                        remaining |> List.fold (fun x y -> Ast.PlusAExp(x,convertExpToAst(y))) plusfirst
                      | _ -> failwith ""
    | Expression.Minus(minus) -> Ast.MinusAExp(convertExpToAst(minus.sub1),convertExpToAst(minus.sub2))
    | Expression.Times(times) -> 
        match times with first::second::remaining ->
                         let multfirst = Ast.MulAExp(convertExpToAst(first),convertExpToAst(second))
                         remaining |> List.fold (fun x y -> Ast.PlusAExp(x,convertExpToAst(y))) multfirst
                       | _ -> failwith ""
    | Expression.Divide(divide) -> Ast.DivAExp(convertExpToAst(divide.div1),convertExpToAst(divide.div2))
    | Expression.Power(power) -> Ast.PowAExp(convertExpToAst(power.base_),convertExpToAst(power.exponent))
    | _ -> failwith ""


let expressionParser = Microsoft.Research.CRNEngine.Expression.parse parse_species_id |>> convertExpToAst


let parse_value = Parser.choice[ 
                    Parser.kw "_" |>> fun(_) -> Ast.WildCardVal //Wildcard check against IDParser
                    expressionParser |>> fun(x) -> 
                      match x with 
                      | Ast.IdAExp(y) -> Ast.IdVal(y)
                      | Ast.FloatAExp(y) -> Ast.FloatVal(y)
                      | _ -> Ast.AlgebraicExp(x)
                  ]


let parse_initpop = INITPOP >>. GecSpecies.parse_species .>>. Parser.pfloat 
                    |>> fun(complexSpecies,fl:float) -> 
                      Ast.InitPop(GecSpecies.species_to_gecAbstractComplex complexSpecies,fl)

let LT = (Parser.pTry((Parser.kw "<-" >>. Parser.failParser "Unexpected - symbol found")  <|> (Parser.kw "<")) ) |>> fun _ -> Ast.Lt
//let LT = Parser.kw "<"
let GT = Parser.kw ">" |>> fun _ -> Ast.Gt
let EQ = Parser.kw "=" |>> fun _ -> Ast.Eq


let COMPAREOP = LT <|> GT <|> EQ


let lookaheadLinebreak = Parser.pTry (Parser.linebreak >>. Parser.failParser "" <|> Parser.satisfy Parser.isWhiteSpace >>. preturn ())
let lookaheadDashSeparator = Parser.pTry(Parser.pstring "->" >>. failParser "" <|> Parser.pstring "-")
 // Parser.satisfy (fun c -> Parser.isWhiteSpace c && c <> '\n')
//let whiteSpacenlb : t<unit> = skipChar isWhiteSpace <?> "a white space"

//#region BrickParser
let propParser = Parser.sepBy parse_value (Parser.kw ",")

type prom = Regprom of string * Option<Ast.value> * Ast.value list list //regType * By * Properties
          | Conprom of Option<Ast.value>

//let value_from_string (s:string) = Parser.from_string valueParser s

let convPromToAstProp (a:prom) = 
    match a with
    |Regprom(regtype,regBy,otherprops) -> 
        if regBy.IsNone then
            (regtype,[])
        else 
            let firstreg = match regBy.Value with 
                            | IdVal(str) -> (Parser.from_string GecSpecies.parse_species str) |> List.map (fun(x) -> Ast.IdVal(x))
                            | _ -> [regBy.Value] 
            let proplist = firstreg::otherprops
            (regtype,proplist)
        
    |Conprom(conprop) ->
        if conprop.IsNone then
            "con",[] 
        else 
            "con",[[conprop.Value]]
        
let regProm (t:string) (props:value list) = 
    match props with 
        [] -> Regprom(t,None,[])
      | regBy::properties ->
          let proplist = properties |> List.map (fun(x) -> [x])
          Regprom(t,Some(regBy),proplist)

let conProm (props:value list) = 
    match props with
        [] -> Conprom(None)
      | conval::error ->
          if error.IsEmpty then
              Conprom(Some(conval))
          else
              //Parser.failParser "Too many consititutive options" //Can I do this? 
              Conprom(Some(conval))

let promSinglePropertyParser = Parser.choice[
                                 (POS <|> NEG) .>>. Parser.paren propParser |>> fun(reg,props) -> regProm reg props
                                 CON >>. Parser.paren propParser |>>  fun(props) -> conProm props                                              
                               ]

let promProperties = Parser.sepBy promSinglePropertyParser (Parser.kw ",") 

let rbsProperty = Parser.kw "rate" >>. Parser.paren propParser |>> fun(proplist) -> proplist |> List.map (fun(x)-> ("rate",[[x]]))

let codesProperty = Parser.kw "codes" >>. Parser.paren propParser |>> fun(proplist) -> 
                                                                        let absComplexlist = proplist |> List.map(fun(x) -> [x])
                                                                        ("codes",absComplexlist)

let brickPartParser = 
  Parser.choice[
    PROM >>. Parser.choice [
               Parser.kw "<" 
               >>. promProperties 
               .>> Parser.kw ">" |>> fun(proplist) -> 
                 let convertedproplist = proplist |> List.map convPromToAstProp 
                 ("prom",convertedproplist)
               Parser.preturn("prom",[])
             ]
    RBS >>. Parser.choice[
              Parser.kw "<" >>. rbsProperty .>> Parser.kw ">" |>> fun(proplist) -> ("rbs",proplist)
              Parser.preturn("rbs",[])
            ]
    PCR >>. Parser.choice[
              Parser.kw "<" >>. codesProperty .>> Parser.kw ">" |>> fun(proplist) -> ("pcr",[proplist])
              Parser.preturn("pcr",[])
            ]
    TER |>> fun(_) -> ("ter",[])
  ]


let parse_brick = 
  let brickid_to_value (str:string) = 
    match str with 
    | "_" -> Ast.WildCardVal
    | _ -> Ast.IdVal(str)
  Parser.choice[
    parse_brickid >>= fun(brickid) -> 
      Parser.choice[
        brickPartParser |>> fun(bricktype,propLst) -> Ast.Brick(brickid_to_value brickid,bricktype,propLst)
        DEVICE |>> fun _ -> Ast.Device(brickid)
      ]                    
    brickPartParser |>> fun(bricktype,propLst) -> Ast.Brick(Ast.WildCardVal,bricktype,propLst)
  ]
//#endregion



type nextParser = 
  | Brick
  | Seq
  | Reaction
  | ModuleInvocation
  | Constraint
  | Nil
  | GecSpecies
  | Compartment


let parse_constraints = expressionParser .>>. COMPAREOP .>>. expressionParser
let parse_ast_constraints = parse_constraints |>> fun ((lhs,op),rhs) -> Ast.Constraint(lhs,op,rhs)

let parse_sequence = Parser.sepBy parse_brick (Parser.kw ";") 
let parse_ast_sequence = 
  parse_sequence |>> fun(seq) -> 
    match seq with 
    | [] -> Ast.Nil
    | [brick] -> brick
    | f::s::rem -> 
      let fs = Ast.Seq(f,s)
      match rem.Length with 
      | 0 -> fs
      | _ -> rem |> List.fold (fun acc x -> Ast.Seq(acc,x)) fs

let parse_template_args = parse_id .>>. Parser.paren (Parser.sepBy parse_value COMMA)

let parse_template_inv_args = parse_id >>. Parser.paren (Parser.sepBy GecSpecies.parse_species COMMA)
let parse_ast_template_inv = parse_template_args
                             |>> fun(modname,args) -> 
                               let complexargs = args |> List.map (fun x -> 
                                 match x with 
                                 | IdVal(id) -> 
                                   let strlist = (Parser.from_string GecSpecies.parse_species id)
                                   strlist |> List.map(fun x -> Ast.IdVal(x))
                                 | _ -> [x]
                                 )
                               Ast.TemplateInv(modname,complexargs)  
                           // check with edge cases. should complexes be separated?

let parse_reaction_species = Parser.sepBy GecSpecies.parse (Parser.kw "+")
let parse_reactants = 
    Parser.sepBy GecSpecies.parse_species (Parser.kw "+")
    |>> fun (reactants) -> 
        match reactants with 
        | [[]] -> []
        | _ -> reactants
let parse_reaction_rate = 
  Parser.opt (Parser.braces parse_value) 

let parse_reaction_rates = parse_reaction_rate .>>. parse_reaction_rate
   
type reactionDirection = | Forward | Reversible
let parse_reaction_direction = 
  Parser.choice [
    Parser.kw "->" |>> fun _ -> Forward
    Parser.kw "<->" |>> fun _ -> Reversible
  ]

let parse_enzyme_option = Parser.opt (Parser.kw "~")
let parse_simulation_option = 
  Parser.opt (Parser.kw "*") 
  |>> fun (x) -> 
  match x with 
  | Some(_)  -> true
  | None -> false

let create_transport_reaction (lhs:GecSpecies.t) (rhs:GecSpecies.t) (dir:reactionDirection) ((r1,r2):value option*value option) (sim:bool) =  
  
  let get_direction (lhs:GecSpecies.t) = match lhs.compartment with | None -> Ast.In | Some(_) -> Ast.Out
  let get_compartment (lhs:GecSpecies.t) (rhs:GecSpecies.t) = 
    match lhs.compartment with 
    | Some(comp) -> comp
    | None -> 
      match rhs.compartment with 
      | Some(comp) -> comp
      | None -> failwith "Malformed Transport Reaction"
  let create_forward_transport_reaction (lhs:GecSpecies.t) (rhs:GecSpecies.t) (rate:value) (sim:bool) = 
    let reactants = GecSpecies.species_to_gecAbstractComplex lhs.species
    let products = GecSpecies.species_to_gecAbstractComplex rhs.species
    let compartment = get_compartment lhs rhs
    let dir = get_direction lhs
    Ast.Trans(reactants,products,compartment,rate,sim,dir)
  match dir with 
  | Forward -> match r2 with None -> () | _ -> failwith "Extra rate found in Forward reaction"
  | Reversible -> ()

  match dir with 
  | Forward -> 
    let rate = match r1 with | Some(r) -> r | None -> Ast.WildCardVal
    create_forward_transport_reaction lhs rhs rate sim
  | Reversible ->
    let frate = match r1 with | Some(r) -> r | None -> Ast.WildCardVal
    let rrate = match r2 with | Some(r) -> r | None -> Ast.WildCardVal
    let forreac = create_forward_transport_reaction lhs rhs frate sim
    let revreac = create_forward_transport_reaction rhs lhs rrate sim
    Ast.Par(forreac,revreac)

let create_normal_reaction (enzymes:string list list) (reactants:string list list) (products:string list list) (dir:reactionDirection) ((r1,r2):value option*value option) (sim:bool) = 
  let reactant_to_ast_val (slist:string list list) = 
    slist |> List.map (fun x -> GecSpecies.species_to_gecAbstractComplex x)
        
  let create_forward_reaction (enzymes:abstractComplex list) (reactants:abstractComplex list) (products:abstractComplex list) (rate_option:value option) (sim:bool) = 
    let rate = 
      match rate_option with 
      | Some (x) -> x
      | None -> Ast.WildCardVal
    Ast.Reac(enzymes,reactants,products,rate,sim)
  
  match dir with 
  | Forward -> 
    match r2 with | None -> () | Some(_) -> failwith "Forward reaction can't have two rates."
    create_forward_reaction (reactant_to_ast_val enzymes) (reactant_to_ast_val reactants) (reactant_to_ast_val products) r1 sim
  | Reversible ->
    match enzymes with | [] -> () | _ -> failwith "Reversible reactions should not have any enzymes." 

    let freac = create_forward_reaction [] (reactant_to_ast_val reactants) (reactant_to_ast_val products) r1 sim
    let rreac = create_forward_reaction [] (reactant_to_ast_val products) (reactant_to_ast_val reactants) r2 sim
    Ast.Par(freac,rreac)

let parse_reaction = 
  let compartment_checker (splist:GecSpecies.t list) = 
    splist |> List.iter (fun sp -> 
      match sp.compartment with 
      | Some (comp) -> failwith "Improper format for Transport Reaction" 
      | None -> ())    
  parse_reaction_species >>= fun(species)  -> 
    match species with 
    | [reactant] -> 
      match reactant.compartment with 
      | Some(comp) -> 
        //Start Transport Reaction
        parse_simulation_option 
        .>>. parse_reaction_direction 
        .>>. (parse_reaction_rates)
        .>>. GecSpecies.parse_species
        |>> fun (((simulationonly,reactiondir),rates),products) -> create_transport_reaction reactant {GecSpecies.t.empty_Species with species = products} reactiondir rates simulationonly
      | None ->
        parse_enzyme_option 
        .>>. parse_reactants
        .>>. parse_simulation_option 
        .>>. parse_reaction_direction 
        .>>. (parse_reaction_rates)
        .>>. parse_reaction_species
        |>> fun (((((enzyme_option,reactants),simulationonly),reactiondir),rates),product_species)-> 
          match enzyme_option with 
          | Some(_) ->
            let enzymes = 
              match species with 
              | [] -> []
              | _ -> species |> List.map (fun x -> x.species)
            compartment_checker product_species
            let products = product_species |> List.map (fun x -> x.species)
            create_normal_reaction enzymes reactants products reactiondir rates simulationonly
          | None -> 
            match product_species with 
            | [product] -> 
              match product.compartment with 
              | Some(comp) ->
                create_transport_reaction reactant product reactiondir rates simulationonly
              | None ->
                let r = 
                  match species with 
                  | [] -> []
                  | _ -> species |> List.map (fun x -> x.species)
                compartment_checker product_species
                let products = product_species |> List.map (fun x -> x.species)
                create_normal_reaction [] r products reactiondir rates simulationonly
            | _ -> 
              let r = 
                match species with 
                | [] -> []
                | _ -> species |> List.map (fun x -> x.species)
              compartment_checker product_species
              let products = product_species |> List.map (fun x -> x.species)
              create_normal_reaction [] r products reactiondir rates simulationonly
            
    | _ ->   
      compartment_checker species //Checks to see if there are any compartments in normal reactions.
      parse_enzyme_option 
        .>>. parse_reactants
        .>>. parse_simulation_option 
        .>>. parse_reaction_direction 
        .>>. (parse_reaction_rates)
        .>>. parse_reaction_species
        |>> fun (((((enzyme_option,reactants),simulationonly),reactiondir),rates),product_species)->
          compartment_checker product_species
          let enzymes = 
            match species with 
            | [] -> []
            | _ -> species |> List.map (fun x -> x.species)
          let products = product_species |> List.map (fun x -> x.species)
          match enzyme_option with 
          | Some(_) -> create_normal_reaction enzymes reactants products reactiondir rates simulationonly
          | None ->
            let r = 
                match species with 
                | [] -> []
                | _ -> species |> List.map (fun x -> x.species)
            create_normal_reaction [] r products reactiondir rates simulationonly
          
          

let reactionOrConstraint = 
  Parser.choice[
    Parser.kw "~" |>> fun _ -> Reaction
    Parser.kw "*" >>. Parser.choice[
                        Parser.kw "-" |>> fun _ -> Reaction
                        Parser.kw "<" |>> fun _ -> Reaction
                        Parser.preturn Constraint
                      ]
    Parser.pstring "-" >>. Parser.choice[
                             Parser.pstring ">" |>> fun _ -> Reaction
                             Parser.preturn Constraint
                           ]
    Parser.pstring "<" >>. Parser.choice[
                             Parser.pstring "-" |>> fun _ -> Reaction
                             Parser.preturn Constraint
                           ]
    Parser.pstring ">" |>> fun _ -> Constraint
  ]

let pInnerParallelContent = 
  Parser.plookAheadWith(
    Parser.choice[
      Parser.pTry(Parser.pfloat |>> fun _ -> true)
      Parser.preturn(false)
  ])
  >>= 
  fun(startsWithFloat) -> 
    if startsWithFloat then 
      //Parser an expression and this should be a constraint, right?
      parse_ast_constraints
    else 
      //This is where the real plookahead fun begins...
      Parser.plookAheadWith(
        Parser.choice[
          Parser.pTry(parse_sequence |>> fun _ -> Seq)
          GecSpecies.parse |>> fun _ -> GecSpecies
        ]
      )
      >>= 
      fun (progType) -> 
        match progType with 
        | Seq -> parse_ast_sequence
        | GecSpecies -> 
          Parser.plookAheadWith(
            Parser.choice[
              Parser.sepBy GecSpecies.parse (Parser.kw "+") 
              >>= fun(pseq) -> 
                match pseq.Length with
                | 1 -> Parser.choice[
                         Parser.kw "(" |>> fun _ -> ModuleInvocation
                         reactionOrConstraint
                       ]
                | _ -> reactionOrConstraint
            ]
          )
          >>= fun (progType2) -> 
            match progType2 with 
            | Constraint ->  parse_ast_constraints
            | ModuleInvocation ->  parse_ast_template_inv
            | Reaction -> parse_reaction
            | _ -> failParser "At this stage, only Constraint, ModuleInvocation, and Reactions were expected"
        | _ -> failParser "At this stage, only Seq or GecSpecies was expected as the Next Parser"  
  

let fold_parallel (proglist: Ast.prog list) = 
  match (List.rev proglist) with 
  | [] -> Ast.Nil
  | [prog] -> prog
  | first::second::remaining ->
    let parfst = Ast.Par(second,first)
    if remaining.IsEmpty then
        parfst
    else
        List.foldBack (fun x y -> Ast.Par(x,y)) (List.rev remaining) parfst

let parse_parallel_inner = Parser.sepBy (Parser.choice [
                                   parse_initpop
                                   pInnerParallelContent
                                 ]) PARALLEL 
                                 |>> fold_parallel
                            
let parse_new = NEW >>. parse_species_id .>> Parser.kw "."

let rec parse_new_inner st = 
  Parser.choice[
    parse_new .>>. parse_new_inner |>> fun(newstr,prog) -> Ast.New(newstr,prog)
    parse_parallel_inner
  ] st


let parse_compartment = Parser.plookAheadWith(Parser.choice[
                                Parser.pTry(parse_id .>> Parser.kw "[" .>> GecSpecies.parse_species .>> Parser.kw "]" |>> fun _ -> Reaction)
                                Parser.pTry(parse_id .>> Parser.kw "[" |>> fun _ -> Compartment)
                              ])
                              >>= fun(progType) -> 
                                match progType with 
                                | Reaction -> parse_reaction
                                | Compartment -> parse_id .>>. Parser.sqBrackets parse_new_inner |>>  fun(compName,prog) -> Ast.Comp(compName,prog)
                                | _ -> failwith "At this point, the program type should only either be a Reaction or a Compartment."

let parse_parallel_outer = Parser.sepBy (Parser.choice [
                                   parse_initpop
                                   parse_compartment
                                   pInnerParallelContent
                                 ]) PARALLEL 
                                 |>> fold_parallel

let rec parse_new_outer st = 
  Parser.choice[
    parse_new .>>. parse_new_outer |>> fun(newstr,prog) -> Ast.New(newstr,prog)
    parse_parallel_outer
  ] st

let rec parse_system_template st =
  Parser.choice[
    TEMPLATE >>. parse_template_args 
      .>>. (Parser.braces parse_new_outer) .>> Parser.kw ";" 
      .>>. parse_system_template |>> 
      fun (((temName,temArgs),progBody),progout) ->
        let args = temArgs |> List.map(fun x -> match x with | IdVal(y) -> y | _ -> failwith "unexpected format for template arguments")
        Ast.TemplateDef(temName,args,progBody,progout)
    parse_new_outer 
  ] st


let parse_template = 
  let rec parse_system_template st =
    Parser.choice[
      TEMPLATE >>. parse_template_args 
        .>>. (Parser.braces parse_new_outer) .>> Parser.kw ";" 
        .>>. parse_system_template |>> 
        fun (((temName,temArgs),progBody),progout) ->
          let args = temArgs |> List.map(fun x -> match x with | IdVal(y) -> y | _ -> failwith "unexpected format for template arguments")
          Ast.TemplateDef(temName,args,progBody,progout)
      Parser.preturn Ast.Nil
    ] st  
  parse_system_template .>>. parse_new_outer



let parse_arguments = Parser.paren (Parser.sepBy parse_id COMMA)
let parse_val_arguments = Parser.paren (Parser.sepBy parse_value COMMA)
let parse_moduleDefinition = parse_id .>> Parser.spaces .>>. parse_arguments
let parse_moduleInvocation = parse_id .>> Parser.spaces .>>. parse_val_arguments

let parse_device = Parser.kw "device" >>. parse_moduleDefinition .>> Parser.kw "=" 
                    .>>. Parser.braces (Parser.sepBy parse_moduleInvocation PIPE)

let parse_instructions pspecies settings = 
  let zero     = Expression.zero
  let unitVal  = Expression.one
  
  let pvalue = Expression.parse parse_id
  let pexpr  = Expression.parse (Key.parse pspecies)
  let pcomma = Parser.kw ","
  
  // crn parsers
  let pinitial  = Microsoft.Research.CRNEngine.Initial<Species,Value>.parse pspecies pvalue zero |>> Instruction.Initial2
  let preaction = Reaction.parse pspecies pvalue pexpr unitVal |>> Instruction.Reaction
  let pinvoke   = Parser.pTry( parse_id .>> Parser.kw "(") 
                    .>>. Parser.sepBy pvalue pcomma 
                    .>>  Parser.kw ")"
                    |>> Module
  let pline     = pinvoke <|> preaction <|> pinitial 
  let pbar      = Parser.kw "|"
  
  // module definitions parser
  let pmodule = 
    Parser.kw "module"                                                              
      >>. Parser.name .>>. Parser.paren (Parser.sepBy Parser.name pcomma)  // module_name(comma-separated args)
      .>> Parser.kw "=" 
      .>>. Parser.braces (Parser.opt pbar >>. Parser.sepBy pline pbar)
  
  // full CRN parser
  Parser.spaces 
    >>.  Parser.many pmodule
    //.>>  Parser.opt pbar
    //.>>. Parser.sepBy pline pbar

let parse_crnModules crnSettings = 
  parse_instructions GecSpecies.parse_gec_to_crn_species crnSettings


let pfixed = Parser.kw "Fixed" |>> fun _ -> Fixed
let pnormal = Parser.kw "Normal" |>> fun _ -> Normal
let ptruncatedNormal = Parser.kw "TruncatedNormal" |>> fun _ -> TruncatedNormal

let parse_priorType =  pfixed <|> pnormal <|> ptruncatedNormal 

let parse_priorOption:Parser.t<igPriorOption> = 
  Parser.plookAheadWith (
     Parser.choice[
       Parser.pTry (Parser.kw "=" >>. parse_priorType >>= fun _ -> Parser.preturn true)
       Parser.preturn false
     ])
     >>= fun hasPriorType -> 
       if hasPriorType  
         then Parser.kw "=" >>. parse_priorType |>> fun (x:igPriorType) -> HasPrior(x)
         else Parser.preturn(NoPrior)
  

let parse_prior:t<igPrior> = Parser.name .>> Parser.spaces .>>. parse_priorOption


let parse_igInferenceSettings = Parser.kw ";" >>. Parser.kw "inference" >>. Parser.kw "=" >>. (Inference_settings.parse)

let parse_igName = Parser.sepBy (Parser.name .>> Parser.spaces) DOT

let parse_edge = Parser.kw "edge" >>. parse_igName .>> Parser.kw "->" 
                 .>>. Parser.list_of parse_prior .>>. parse_igName |>> fun ((x,y),z) -> Edge(x,y,z)

let parse_node = 
  Parser.kw "node" >>. (Parser.name .>> Parser.spaces) 
  .>> Parser.kw "{" .>>. (Parser.kw "systems" >>. Parser.kw "=" 
  >>. Parser.list_of (Parser.name .>> Parser.spaces)) >>= fun (x,y) -> 
    Parser.plookAheadWith(
      Parser.choice[
        Parser.pTry(Parser.kw ";" >>. Parser.kw "inference" >>= fun _ -> Parser.preturn true)
        Parser.preturn false
      ])
      >>= fun hasInference -> 
        if hasInference 
          then 
            parse_igInferenceSettings .>> Parser.kw "}" |>> fun (z) -> Node(x,y,Some(z))
          else 
            Parser.kw "}" |>> fun _ -> Node(x,y,None)
                        
                        

let parse_igraphElement = parse_edge <|> parse_node

let parse_withSystem = 
  Parser.plookAheadWith (
    Parser.choice [
      Parser.pTry(Parser.name .>> Parser.spaces .>> Parser.kw "with" >>= fun _ -> Parser.preturn true)
      Parser.preturn false
    ]) 
    >>= fun hasWith ->
      if hasWith 
        then Parser.name .>> Parser.spaces .>> Parser.kw "with" |>> fun x -> System(x)
        else Parser.preturn(NoSystem)                  

let rename_settings_species (settings:Settings.Gec_settings) =
  let rename_species (sp:Species) = 
    let spName = sp.name
    if spName.Contains("[") && spName.Contains("]") then
      let spNameParser = Parser.name .>>. Parser.sqBrackets Parser.name |>> fun(x,y) -> (x + "_" + y)
      let newName (str:string) = Parser.from_string spNameParser str
      Species.create(newName spName)
    else 
      sp
  let crn = Crn_settings.defaults.from_default_directive_list settings.directives
  let renamed_crn_settings = crn.map (Expression.map (Key.map (fun sp -> rename_species sp)))
  renamed_crn_settings
  

let parse_program (crnsettings:Crn_settings<Functional>)= 
  Settings.parse_defaults crnsettings >>= fun(top_settings) -> 
    let top_crn_settings = crnsettings.from_default_directive_list top_settings.directives
    parse_crnModules top_crn_settings
    .>>. (Parser.many parse_device) 
    .>>. parse_system_template
    |>> fun((crn_modules,device_definitions),gecprog) ->
      (top_settings,crn_modules,device_definitions,gecprog)    


let parse_system (settings:Crn_settings<Functional>) = 
  SYSTEM >>. parse_id .>> (Parser.kw "=") .>>. Parser.braces (parse_withSystem .>>. parse_program settings)
  |>> fun(systemName,(sysRef,(settings,modules,devices,prog))) -> 
    {name=systemName;
    withSystem=sysRef;
    settings=settings; 
    modules = modules; 
    devices = devices;
    prog = prog}

let parseClassic (top_settings:Settings.Gec_settings) = 
  let top_crn_settings = Crn_settings.defaults.from_default_directive_list top_settings.directives
  parse_crnModules top_crn_settings
  .>>. (Parser.many parse_device)
  .>>. parse_template
  .>>. (Parser.many (parse_system top_crn_settings))
  .>>. (Parser.many parse_igraphElement)
  |>> fun((((modules,devices),(templates,topProg)),systems),igraph) ->  
    {settings=top_settings;
    modules = modules;
    devices = devices;
    systems = systems;
    templates = templates;
    prog = topProg;
    graph = igraph}

let getMapInstruction m (i:LogicGEC.Instruction) =
  let getMapComplex sp m = 
    sp |> List.fold (fun acc (_, x) -> getMapTerm acc x) m
  match i with 
  | LogicGEC.Constraint c        -> LogicGEC.getMapLit m c
  | LogicGEC.Device d            -> d |> List.fold (LogicGEC.getMapElement) m
  | LogicGEC.Initial(c,sp)       -> c 
                                    |> LogicGEC.getMapTerm m
                                    |> getMapComplex sp
  | LogicGEC.Reaction(a,b,c,d,e) -> 
    let cs = if a.IsSome then [a.Value; b; c]
                         else [b;c]
    let rs = if e.IsSome then [e.Value; d]
                         else [d]
    let m' = cs |> List.fold (fun acc x -> getMapComplex x acc) m
    rs |> List.fold getMapTerm m'

let parseLogic(settings:Settings.Gec_settings) = 
  let idProvider = RulesDSD.Syntax.makeProvider () // initialise an ID provider for logical variables, so that the same variable in different predicates/devices is given the same ID
  Parser.opt (kw "|")
    >>. Parser.sepBy1 (pInstruction idProvider) (kw "|")
    >>= fun prog -> 
      
      // find the parts declared in the database
      let declaredParts = 
        match settings.rules with 
        | None -> failwith "Missing Logic GEC rules in instructions parsing."
        | Some rules -> LogicGEC.getParts rules 
      let declaredNames = 
        declaredParts 
        |> List.choose (fun p -> match p.name with Term.Const n -> Some n | _ -> None) 
        |> Set.ofList
      
      // part name and types disambiguation step
      let parts = 
        prog 
        |> List.collect (Instruction.Species)
        |> List.fold (fun acc x -> match x with Element.Var _ -> acc | Element.Part p -> p::acc) []

      let partNames, partTypes = 
        parts 
        |> List.fold (fun (acc1, acc2) p -> 
          let names = 
            match p.name with
            | RulesDSD.Syntax.Term.Const x -> 
              if not (acc1 |> Set.contains x) && p.type_ <> Term.Const LogicGEC.noTypePart
                then Set.add x acc1
                else acc1
            | _ -> acc1
          let types = 
            match p.type_ with
            | RulesDSD.Syntax.Term.Const x ->
              if not (acc2 |> Set.contains x) && p.type_ <> Term.Const LogicGEC.noTypePart
                then Set.add x acc2
                else acc2
            | _ -> acc2
          names, types  
        ) (Set.empty, LogicGEC.PREDEFINED_PART_TYPE_NAMES |> Set.ofList)
      let ambiguousNames = Set.intersect partNames partTypes
      if not (ambiguousNames.IsEmpty)
        then ambiguousNames |> Set.toList |> List.head |> failwithf "Ambiguous part declaration: \"%s\" is used as a part name and a part type at the same time."
        else 
          // guess #1: turn undeclared parts back into strings
          let prog1 = 
            let rec f (x:Term<Element>) : Term<Element> = 
              match x with 
              | Term.Pat (Pattern.Inner [y, _]) -> 
                match y with 
                | Element.Part {name = Term.Const n 
                                type_ = _           } -> if declaredNames.Contains n || partNames.Contains n
                                                          then x 
                                                          else Term.Const n
                | _ -> x
              | Term.Pat _ 
              | Term.Proc _
              | Term.Var _    
              | Term.Const  _ 
              | Term.Float  _        -> x
              | Term.TCRN   ts       -> Term.TCRN (ts |> List.map f)
              | Term.Func   (n, ts)  -> Term.Func (n, ts |> List.map f)
              | Term.TList  ts       -> Term.TList (ts |> List.map f)
              | Term.TCons  (t1, t2) -> Term.TCons (f t1, f t2)
              | Term.TMSet  s        -> Term.TMSet (s |> List.map (fun (i, x) -> i, f x))
            prog 
            |> List.map (fun i ->
              match i with 
              | LogicGEC.Device _ -> i
              | LogicGEC.Constraint c -> LogicGEC.Constraint (c |> Literal.Map f)
              | LogicGEC.Initial (n,c) -> LogicGEC.Initial (f n, c |> List.map (fun (x,y) -> x, f y))
              | LogicGEC.Reaction (a,b,c,d,e) -> LogicGEC.Reaction (a,b,c,f d, Option.map f e)
            )

          // guess #2: turn a part name into a part type (e.g. "ter" might be parsed as a part called ter and no type, when it's actually a terminator part type )
          let guessPartTypes e = 
            match e with 
            | Element.Part {name = Term.Const n; type_ = Term.Const t} -> 
              if t = noTypePart 
                then if Set.contains n partTypes 
                        then Element.Part { name = Term.wildcard; type_ = Term.Const n}
                        else Element.Part { name = Term.Const n;  type_ = Term.wildcard}
                else e 
            | _ -> e
          
          let prog2 = prog1 |> List.map (Instruction.Map guessPartTypes)

          // guess #3: add part types to parts that were only written as part names (e.g. r0011 in "<r0011 b0034 c0062 b0015>" )
          let declaredDict = declaredParts 
                              |> List.map (fun p -> p.name, p) 
                              |> fun decs -> // W# doesn't process "dict"
                                let x = new System.Collections.Generic.Dictionary<Term<Element>, Part>()
                                for k,v in decs do
                                  x.Add(k,v)
                                x
          let prog3 = 
            prog2
            |> List.map (Instruction.Map (fun e -> 
              match e with 
              | Element.Var _  -> e 
              | Element.Part p -> if p.type_ = Term.wildcard && declaredDict.ContainsKey p.name 
                                    then Element.Part (declaredDict.Item p.name)
                                    else e
            ))

          // disambiguate variables in prog


          preturn { settings = settings
                  ; rules    = settings.rules.Value // TODO: refactor settings
                  ; program  = prog3 }

let parse : Parser.t<t> = 
  Parser.spaces >>.
  Settings.parse >>= fun(top_settings) ->
    if top_settings.rules.IsNone
      then parseClassic top_settings |>> ClassicGec
      else parseLogic top_settings   |>> LogicGec
/// TO STRING METHODS.

let crnSettings_to_string (crnSettings:Crn_settings<Functional>) = crnSettings.to_string Functional2.to_string Functional2.to_string_plot


let argsval_to_string (args: value list) = Lib.string_of_list (fun x -> Ast.stringOfValue x) "," (args) 
let args_to_string (args: string list) = Lib.string_of_list (fun x -> x) "," (args) 


let modules_to_string initial_time (modules:crnModules) = 
    let moduleList = modules 
    let instructionList_to_string (instructions:Instruction list) = 
        let instructionStringList = instructions |> List.map (fun x -> ("| " + (Instruction.to_string initial_time x))) 
        Lib.string_of_list (fun x -> x) "\n" instructionStringList 
    let module_to_string (((moduleName:string),(moduleArgs:string list)),(instructions:Instruction list)) = 
        let str = "module " + moduleName + "("  + (args_to_string moduleArgs) + ") = {\n" +
                    (instructionList_to_string instructions) + "\n}\n"
        str 
    let str = 
        let moduleListString = moduleList |> List.map (fun x -> (module_to_string x))
        let moduleStr = Lib.string_of_list (fun x -> x) "" moduleListString    
        moduleStr //+ (instructionList_to_string externalInstructions)
    
    match modules with 
    | [] -> ""
    | _ -> str

let crn_contents_to_string (initial_time) (crnExtended:Crn option * crnInstructions) = 
  
  let (crnOpt,crnInstructions) = crnExtended
  let (modules,instructions) = crnInstructions
  let instructions_tp_string instructions = Lib.string_of_list (fun x -> ("|" + Instruction.to_string initial_time x)) "\n" instructions
  let module_to_string (m) = 
    let ((mname,margs),instructions) = m
    mname + "(" + (Lib.string_of_list (fun x -> x) "," margs) + ") =  {\n" + 
    instructions_tp_string instructions +    
    "}\n"
    
  let modules_to_string (modules:crnModules) = Lib.string_of_list (fun m -> module_to_string m) "\n" modules

  let inst_string =  (modules_to_string modules) + (instructions_tp_string instructions)

  match crnOpt with 
  | Some(crn) -> 
    let initials = crn.initials
    let reactions = crn.reactions
    let not_initial i = Expression.simplify i <> Expression.Float initial_time
    let initial_to_string = Initial.to_string Species.to_string (Expression.to_string id)  not_initial
    let reaction_to_string = Reaction.to_string Species.to_string (Expression.to_string id) Functional2.to_string 
    let initStr = Lib.string_of_list (fun x -> ("| " + (initial_to_string x))) "\n" initials
    let reacStr = Lib.string_of_list (fun x -> ("| " + (reaction_to_string x))) "\n" reactions
    inst_string + initStr + reacStr
  | None -> inst_string

let moduleDefinition_to_string (moduleDef:moduleDefinition) = 
    let (moduleName,moduleArgs) = moduleDef
    moduleName + "(" + (args_to_string moduleArgs) + ")"

let moduleInvocation_to_string (moduleInv:moduleInvocation) = 
    let (moduleName,moduleArgs) = moduleInv
    moduleName + "(" + (argsval_to_string moduleArgs) + ")"

let deviceDefinition_to_string (device:deviceDefinition) =     
    let ((deviceDef),deviceBody) = device
    let deviceBodyString = "{" + (Lib.string_of_list (fun x -> x)  " | " (deviceBody |> List.map (fun x -> moduleInvocation_to_string x))) + "}"
    let str = "module " + (moduleDefinition_to_string deviceDef) + " = " + deviceBodyString
    str

let system_to_string (crn) (modules) (deviceDefs) (system:system) (db:Database.t)= 
  let moduleDefinitions = modules |> List.map fst
  let deviceDefinitions = deviceDefs |> List.map fst
  
  let  device_list = 
    let rec get_devices (prog:Ast.prog) = 
      match prog with 
      | Ast.Device(d) -> [d]
      | Ast.Seq(s1,s2) -> (get_devices s1)@(get_devices s2)
      | Ast.Par(p1,p2) -> (get_devices p1)@(get_devices p2)
      | Ast.Comp(c,p) -> get_devices p
      | Ast.New(n,p) -> get_devices p
      | Ast.TemplateDef(_,_,p1,p2) -> (get_devices p2)
      | Ast.Copy(_,p,_,_) -> get_devices p
      | _ -> []
    get_devices system.prog

  let hypothesisSettings_to_string (deviceList:string list) (devices:Database.device list) (deviceDefs:moduleDefinition list) (moduleDefs:moduleDefinition list) = 
    let rec device_unroll (dev:string) (devices:Database.device list) (deviceDefs:moduleDefinition list) (moduleDefs:moduleDefinition list) =
        let deviceDefOpt = deviceDefs |> List.tryFind (fun (x,y) -> x=dev)
        match deviceDefOpt with 
        | Some(a) -> [a]
        | None -> 
            let modOpt = (moduleDefs |> List.tryFind (fun (x,y) -> x=dev))
            match modOpt with 
            | Some(a) -> [a]
            | None ->
                let devOpt = (devices |> List.tryFind (fun (x,y) -> x=dev ))
                match devOpt with 
                | Some (devName,devComps) -> 
                    let deflist = devComps |> List.map (fun x ->  device_unroll x devices deviceDefs moduleDefs)
                    match deflist.Length with 
                    | 0 -> []
                    | _ -> deflist |> List.reduce (fun x y -> x@y)
                | None -> 
                    raise (System.ArgumentException("Device in System Directive must be defined."))             
    let mdeflistlist = deviceList |> List.map (fun x -> device_unroll x devices deviceDefs moduleDefs)
    let mdeflist = 
        match mdeflistlist.Length with 
        | 0 -> []
        | _ -> mdeflistlist |> List.reduce (fun x y -> x@y)
    let l = (mdeflist |> List.map (fun x -> "| " + (moduleDefinition_to_string x)))
    match l with 
    | [] -> ""
    | _ -> Lib.string_of_list (fun x -> x) "\n" l 
    
  let system_to_string (devices:Database.device list) (deviceDefs:moduleDefinition list) (moduleDefs:moduleDefinition list)= 
    let sysRef_to_string (sysRef:systemReference) = 
        match sysRef with 
        | System(x) -> x + " with "
        | NoSystem -> ""    
    let nl_str (str:string) = 
        match (str.Trim()) with 
        | "" -> ""
        | _ -> str + "\n"
    let str = 
        "system " + system.name + " = { " + (sysRef_to_string system.withSystem) + "\n" + 
        let crnSettings = Crn_settings.defaults.from_default_directive_list system.settings.directives
        nl_str (crnSettings_to_string  crnSettings) +                 
        nl_str (modules_to_string crnSettings.simulation.initial system.modules) + 
        nl_str (crn_contents_to_string crnSettings.simulation.initial crn) + //Crn to String (Only initials and reactions) 
        hypothesisSettings_to_string (device_list) devices deviceDefs moduleDefs + "\n}\n"        

    str
  
  system_to_string db.devices deviceDefinitions moduleDefinitions

let igNode_to_string (node:ignode) =   
    let (nodeName,nodeSystems,iSettings) = node
    let str = "node " + nodeName + " { systems = [" + (Lib.string_of_list (fun x -> x) "; " nodeSystems) + "]"
    match iSettings with 
    | Some(x) -> str + "; " + "inference " + "= " + (Inference_settings.to_string(x)) + "}"
    | None -> str + "}"

     

let igEdge_to_string (edge:igedge) = 
    let edgeNameString edgelist =  Lib.string_of_list (fun x -> x) "." edgelist 
    let priorType_to_string (ptype:igPriorType) =
        match ptype with 
        | Fixed -> "Fixed"
        | Normal -> "Truncated"
        | TruncatedNormal -> "TruncatedNormal"
    let prior_to_string ((p,ptype):igPrior) = 
        match ptype with 
        | HasPrior(x) -> p + "=" + (priorType_to_string x)
        | NoPrior -> p
    let (fromNode,priorlist,toNode) = edge
    let str = "edge " + (edgeNameString fromNode) + " -> " + "["+ 
                (Lib.string_of_list (fun x -> x) "; " (priorlist |> List.map (fun x-> prior_to_string x)) ) + 
                "] " + (edgeNameString toNode)
    str

let igElement_to_string (elem:igraphElement) = 
    match elem with 
    | Node(x) -> igNode_to_string x
    | Edge(x) -> igEdge_to_string x
    
      