module Microsoft.Research.GEC.Hypothesis

open Parser
open Microsoft.Research.CRNEngine


type parser<'a> = Parser.t<'a>


type HypothesisDirective = 
    | Devices of (string list)



type HypothesisSettings = 
    {
    devices : string list
    }
    static member defaults = {
        devices = []
    } 
    member setting.from_directive (directive:HypothesisDirective) = 
        match directive with 
        | Devices(x) -> {setting with devices=x}
    member setting.from_directiveList (directives:HypothesisDirective list) = 
        directives |> List.fold (fun (s:HypothesisSettings) (d:HypothesisDirective) -> setting.from_directive  d) setting 



type arguments = (string list)
type crndirective = string
type crnmodule = string * arguments * string
//type device = string * arguments * string

type crnModules = ((string * string list) * Instruction list) list * Instruction list

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
 
type moduleDefinition = string * (arguments)

type deviceDefinition = moduleDefinition * (moduleDefinition list)

type systemReference = 
    | System of string
    | NoSystem

type crnSystem = string * systemReference * Crn_settings<Functional> * HypothesisSettings * crnModules

type hypothesis = Crn_settings<Functional> * (crnModules) * (deviceDefinition list) * (crnSystem list) * (igraphElement list)


let kwList = ["module";"system";"inference";"directive";"device"]

//Parsers Start here:

let parse_species = Parser.(|>>) (Parser.name_kw kwList) Species.create

let pcomma = Parser.kw ","
let pPipe = Parser.kw "|"
let pDot = Parser.kw "."

let argumentsParser = Parser.paren (Parser.sepBy Parser.name pcomma)

let parse_crnDirective = Directive<Functional>.parse Functional2.parse Functional2.parse_plot    


let parse_hypothesisDirective = Parser.kw "device" >>. Parser.list_of Parser.name .>> Parser.opt (Parser.kw ";") |>> Devices

let parse_crnSettings = Crn_settings<Functional>.parse Functional2.parse Functional2.parse_plot


let parse_moduleInvocation = Parser.name .>> Parser.spaces .>>. argumentsParser

let parse_device = Parser.kw "device" >>. parse_moduleInvocation .>> Parser.kw "=" 
                    .>>. Parser.braces (Parser.sepBy parse_moduleInvocation pPipe)



let parse_instructions pspecies settings = 
    let zero     = Expression.zero
    let unitVal  = Expression.one
  
    let pname  = Parser.name_kw kwList .>> Parser.spaces
    let pvalue = Expression.parse pname
    let pexpr  = Expression.parse (Key.parse pspecies)
    let pcomma = Parser.kw ","

    // crn parsers
    let pinitial  = Initial<Species,Value>.parse  pspecies pvalue zero |>> Instruction.Initial2
    let preaction = Reaction.parse pspecies pvalue pexpr unitVal       |>> Instruction.Reaction
    let pinvoke   = Parser.pTry(  pname .>> Parser.kw "(") 
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
      .>>  Parser.opt pbar
      .>>. Parser.sepBy pline pbar

let parse_crnModules crnSettings = parse_instructions parse_species crnSettings

let crnDirectiveChoice = parse_crnDirective |>> Choice1Of2
let hypothesisDirectiveChoice = parse_hypothesisDirective |>> Choice2Of2


let pfixed = Parser.kw "Fixed" |>> fun _ -> Fixed
let pnormal = Parser.kw "Normal" |>> fun _ -> Normal
let ptruncatedNormal = Parser.kw "TruncatedNormal" |>> fun _ -> TruncatedNormal

let parse_priorType =  pfixed <|> pnormal <|> ptruncatedNormal 

let parse_priorOption:Parser.t<igPriorOption> = (Parser.plookAheadWith (
                                                     Parser.choice[
                                                         Parser.pTry (Parser.kw "=" >>. parse_priorType >>= fun _ -> Parser.preturn true)
                                                         Parser.preturn false
                                                     ])
                                                     >>= fun hasPriorType -> 
                                                         if hasPriorType  
                                                             then Parser.kw "=" >>. parse_priorType |>> fun (x:igPriorType) -> HasPrior(x)
                                                             else Parser.preturn(NoPrior)
                                                 )

let parse_prior:t<igPrior> = Parser.name .>> Parser.spaces .>>. parse_priorOption


let parse_igInferenceSettings = Parser.kw ";" >>. Parser.kw "inference" >>. Parser.kw "=" >>. (Inference_settings.parse)

let parse_igName = Parser.sepBy (Parser.name .>> Parser.spaces) pDot

let parse_edge = Parser.kw "edge" >>. parse_igName .>> Parser.kw "->" .>>.
                    Parser.list_of parse_prior .>>. parse_igName |>> fun ((x,y),z) -> Edge(x,y,z)

let parse_node = Parser.kw "node" >>. (Parser.name .>> Parser.spaces) .>> 
                    Parser.kw "{" .>>. (Parser.kw "systems" >>. Parser.kw "=" >>. 
                        Parser.list_of (Parser.name .>> Parser.spaces)) >>= fun (x,y) -> 
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


let parse_withSystem = (Parser.plookAheadWith (
                            Parser.choice [
                                Parser.pTry(Parser.name .>> Parser.spaces .>> Parser.kw "with" >>= fun _ -> Parser.preturn true)
                                Parser.preturn false
                            ]) 
                            >>= fun hasWith ->
                                if hasWith 
                                    then Parser.name .>> Parser.spaces .>> Parser.kw "with" |>> fun x -> System(x)
                                    else Parser.preturn(NoSystem))
                                        
                                        


let parse_hybridDirective = Parser.many (Parser.kw "directive" >>. (crnDirectiveChoice <|> hypothesisDirectiveChoice)) |>> 
                                (fun hybridList -> 
                                        let crnList = hybridList |> 
                                                      List.filter (fun x -> 
                                                            match x with 
                                                            | Choice1Of2 t -> true
                                                            | _ -> false) |> 
                                                      List.map (fun x -> 
                                                            match x with 
                                                            | Choice1Of2 t -> t
                                                            | _ -> failwith "Unexpected choice2 in parse_hybridDirective")
                                        let hypList = hybridList |> 
                                                      List.filter (fun x -> 
                                                            match x with 
                                                            | Choice2Of2 t -> true
                                                            | _ -> false) |> 
                                                      List.map (fun x -> 
                                                            match x with 
                                                            | Choice2Of2 t -> t
                                                            | _ -> failwith "Unexpected choice1 in parse_hybridDirective")
                                        let crn_settings = Crn_settings.defaults.from_directive_list crnList
                                        let hypothesis_settings = HypothesisSettings.defaults.from_directiveList hypList
                                        (crn_settings,hypothesis_settings)
                                )

let (parse_crn_system:t<crnSystem>) = Parser.kw "system" >>. Parser.name .>> Parser.spaces .>> Parser.kw "=" 
                                        .>> Parser.kw "{" .>>. parse_withSystem .>>. parse_hybridDirective >>= fun((systemName,withSystem),(crnSettings,hypothesisSettings)) -> 
                                            parse_crnModules crnSettings .>> (Parser.kw "}")  >>= fun y -> Parser.preturn(systemName,withSystem,crnSettings,hypothesisSettings,y) 




let (parse_hypothesis_content:t<hypothesis>) = 
    parse_crnSettings >>= fun (crnSettings) -> 
        (parse_crnModules crnSettings) 
        .>>. (Parser.many parse_device) 
        .>>. (Parser.many parse_crn_system)
        .>>. (Parser.many parse_igraphElement)
        |>> fun (((modulelist:crnModules,deviceDefinitions: deviceDefinition list),systemlist:crnSystem list),igraph) -> (crnSettings,modulelist,deviceDefinitions,systemlist,igraph)
                   
                   
//To String Methods
let fold_string_list (strList:string list) (folder:string) = 
    match strList.Length with 
    | 0 -> ""
    | 1 -> strList.Head
    | _ -> strList.Tail |> List.fold (fun f s -> (f + folder + s)) strList.Head

let crnDirectives_to_string (crnSettings:Crn_settings<Functional>) = crnSettings.to_string Functional2.to_string Functional2.to_string_plot

let args_to_string (args: arguments) = fold_string_list args ","

let modules_to_string initial_time (modules:crnModules) = 
    let moduleList,externalInstructions = modules 
    let instructionList_to_string (instructions:Instruction list) = 
        let instructionStringList = instructions |> List.map (fun x -> ("| " + (Instruction.to_string initial_time x))) 
        fold_string_list instructionStringList "\n"
    let module_to_string (((moduleName:string),(moduleArgs:string list)),(instructions:Instruction list)) = 
        let str = "module " + moduleName + "("  + (args_to_string moduleArgs) + ") = {\n" +
                    (instructionList_to_string instructions) + "\n}\n"
        str 
    let str = 
        let moduleListString = moduleList |> List.map (fun x -> (module_to_string x))
        let moduleStr = fold_string_list moduleListString ""    
        moduleStr + (instructionList_to_string externalInstructions)
    str

let moduleDefinition_to_string (moduleDef:moduleDefinition) = 
    let (moduleName,moduleArgs) = moduleDef
    moduleName + "(" + (args_to_string moduleArgs) + ")"

let deviceDefinition_to_string (device:deviceDefinition) =     
    let ((deviceDef),deviceBody) = device
    let deviceBodyString = "{" + (fold_string_list (deviceBody |> List.map (fun x -> moduleDefinition_to_string x)) " | ") + "}"
    let str = "module " + (moduleDefinition_to_string deviceDef) + " = " + deviceBodyString
    str


let igNode_to_string (node:ignode) =   
    let (nodeName,nodeSystems,iSettings) = node
    let str = "node " + nodeName + " { systems = [" + (fold_string_list nodeSystems ";") + "]"
    match iSettings with 
    | Some(x) -> str + ";" + "inference " + "=" + (Inference_settings.to_string(x)) + "}"
    | None -> str + "}"

     

let igEdge_to_string (edge:igedge) = 
    let edgeNameString edgelist =  fold_string_list edgelist "."
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
                (fold_string_list (priorlist |> List.map (fun x-> prior_to_string x)) ";") + 
                "]" + (edgeNameString toNode)
    str

let igElement_to_string (elem:igraphElement) = 
    match elem with 
    | Node(x) -> igNode_to_string x
    | Edge(x) -> igEdge_to_string x

let hypothesisSettings_to_string (hypSettings:HypothesisSettings) (devices:Database.device list) (deviceDefs:moduleDefinition list) (moduleDefs:moduleDefinition list) = 
    let deviceList = hypSettings.devices
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
    fold_string_list (mdeflist |> List.map (fun x -> "| " + (moduleDefinition_to_string x))) "\n"
    


let system_to_string (sys:crnSystem) (devices:Database.device list) (deviceDefs:moduleDefinition list) (moduleDefs:moduleDefinition list)= 
    let (sysName,sysRef,crnSettings,hypSettings,crnMods) = sys
    let sysRef_to_string (sysRef:systemReference) = 
        match sysRef with 
        | System(x) -> x + " with "
        | NoSystem -> ""    
    
    let str = "system " + sysName + " = { " + (sysRef_to_string sysRef) + "\n" + 
                (crnDirectives_to_string crnSettings) + "\n" + 
                hypothesisSettings_to_string hypSettings devices deviceDefs moduleDefs + 
                (modules_to_string crnSettings.simulation.initial crnMods) + "\n" + "}\n"        

    str
    
let hypothesis_to_crn_program (h:hypothesis) (devicelib:Database.device list)= 
    let (crnSettings,moduleDefs,deviceDefs,systemDefs,igraph) = h
    let (modules,_) = moduleDefs
    let moduleDefinitions = modules |> List.map fst
    let deviceDefinitions = deviceDefs |> List.map fst
    let str = crnDirectives_to_string crnSettings + "\n" + 
              (modules_to_string crnSettings.simulation.initial moduleDefs) + "\n" + 
              (fold_string_list (deviceDefs |> List.map deviceDefinition_to_string) "\n") + "\n" +
              (fold_string_list (systemDefs |> List.map (fun x -> system_to_string x devicelib deviceDefinitions moduleDefinitions)) "\n") + "\n" +
              (fold_string_list (igraph |> List.map igElement_to_string) "\n")  
              
    str
