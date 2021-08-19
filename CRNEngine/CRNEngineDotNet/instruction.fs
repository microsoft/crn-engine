namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>] 
type Instruction =
  | Reaction          of Reaction<Species,Value,Functional>
  | Initial2           of Initial<Species,Value>
  | Module  of string * (Value list)
  static member to_string initial_time (instruction:Instruction) = 
    match instruction with 
    | Reaction(x) -> Reaction.to_string Species.to_string (Expression.to_string id) Functional2.to_string x
    | Initial2(x) -> 
      let not_initial i = Expression.simplify i <> Expression.Float initial_time 
      Initial.to_string Species.to_string (Expression.to_string id) not_initial x
    | Module(moduleName,argVals) ->
        let args = (argVals |> List.map (fun x -> Expression.to_string id x))
        let join (arglist:string list) = 
            match arglist.Length with
            | 0 -> ""
            | 1 -> arglist.Head
            | _ -> arglist.Tail |> List.fold (fun f s -> f + "," + s) arglist.Head
        moduleName + "(" + (join args)  + ")" 
  
  static member convert_instructions (moduleDefs : ((string * string list) * Instruction list) list, instructions:Instruction list) =
    let rec f (reactions,initials) command =
      match command with
      | Reaction reaction   -> reaction::reactions, initials
      | Initial2 initial     -> reactions, initial::initials
      | Module (invName, invArgs) -> 
          let defArgs, defBody = 
            moduleDefs 
              |> List.tryFind (fun ((defName, defArgs), _) -> defName = invName && invArgs.Length = defArgs.Length)
              |> function
                  | Some ((_, x), y) -> x, y
                  // print error message
                  | None -> 
                      let invokation = sprintf "%s(%s)" invName (String.concat ", " (invArgs |> List.map (Expression.to_string id)))
                      let errorMsg   = 
                              match moduleDefs |> List.tryFind (fun ((defName, _), _) -> defName = invName) with
                              | None   -> sprintf "No module definition found for module invocation \"%s\"" invokation
                              | Some _ -> sprintf "No module definition with %i arguments found for invokation \"%s\"" 
                                                    invArgs.Length 
                                                    invokation
                      failwith errorMsg 
          let argpairs = List.zip defArgs invArgs
          //let singleName e = match e with
          //                    | Expression.Key s -> Some s
          //                    | _                -> None
          let lookupArg str = argpairs |> List.tryFind (fst >> ((=) str)) |> Option.map snd
          let stringSub modName (str:string) : string = 
            match lookupArg str with
            | Some exp -> match exp with
                          | Expression.Key str' -> str'
                          | _         -> failwith <| sprintf "Module invokation failed: cannot substitute expression \"%s\" for \"%s\" in module \"%s\"" (Expression.to_string id exp) str modName
            | None          -> str
          let speciesSub modName (sp:Species) : Species =
            let sp' = stringSub modName sp.name
            {name = sp'}
          let subMSet modName (s : Mset.t<Species>) = Mset.map (speciesSub modName)
          let subVal v = match lookupArg v with
                         | None   -> Expression.Key v
                         | Some v' -> v'
          let subRate modName (r : Rate<Value,Functional>) =
            r.map (Expression.expand subVal)  
                          (Expression.expand (function
                                              | Key.Time        -> Expression.Key Key.Time
                                              | Key.Parameter p -> match lookupArg p with
                                                                    | None   -> Expression.Key (Key.Parameter p)
                                                                    | Some v -> v |> Expression.map Key.Parameter
                                              | Key.Rate      r -> Expression.Key (Key.Rate    (stringSub modName r))
                                              | Key.Species  sp -> Expression.Key (Key.Species (speciesSub modName sp)))) 
          let subReaction modName (r:Reaction<Species,Value,Functional>) = 
            let catalysts   = r.catalysts   |> Mset.map (speciesSub modName)
            let reactants   = r.reactants   |> Mset.map (speciesSub modName)
            let reverse = 
              match r.reverse with
              | None    -> None
              | Some r  -> r |> subRate modName |> Some
            let rate        = r.rate        |> subRate modName
            let products    = r.products    |> Mset.map (speciesSub modName)
            Reaction.create(catalysts,reactants,reverse,rate,products)
        
          let subInitial modName (i:Initial<Species,Value>) : Initial<Species,Value> =
            let species  = speciesSub modName i.species
            let value    = Expression.expand subVal i.value
            let constant = i.constant
            let time     = Option.map (Expression.expand subVal) i.time
            let spatial  = i.spatial 
            Initial.create(constant,value,species,time,spatial)

          let subInstruction modName (rs, is) (x:Instruction) = 
            match x with
              | Reaction r -> subReaction modName r :: rs, is
              | Initial2 i  -> rs, subInitial modName i :: is
              | Module (name, args) -> 
                let updatedArgs = args 
                                  |> List.map (fun x -> Expression.expand subVal x)
                f (rs, is) (Module (name, updatedArgs))
          List.fold (subInstruction invName) (reactions, initials) defBody 
    let reactions, initials = List.fold f ([],[]) instructions
    List.rev reactions, List.rev initials

  static member parse pspecies settings = 
    let zero     = Expression.zero
    let initTime = Expression.Float settings.simulation.initial
    let unitVal  = Expression.one
  
    let pname  = Parser.name_kw Keywords.kwList .>> Parser.spaces
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

(* TODO: merge with parsers in instruction.fs and crn.fs

(* main CRN parser *)
let parse_crn_module pspecies (moduleDefs : moduleDefinition list) (settings:Crn_settings<Key<Species>>) = 
  let zeroVal :Expression.t<string> = Expression.Float settings.simulation.initial
  let unitVal :Expression.t<string> = Expression.Float 1.0
  
  // base parsers

  //let pNameLookahead = (Parser.lookAhead("system") <|> Parser.lookAhead("inference")) >>. Parser.name
  let pNameLookahead = Parser.name_kw Keywords.kwList
  let pvalue = Expression.parse (pNameLookahead .>> Parser.spaces)
  let pexpr  = Expression.parse (Key.parse pspecies)
  let pcomma = Parser.kw ","

  // crn parsers
  let pinitial  = Initial.parse  pspecies pvalue zeroVal        |>> Initial
  let preaction = Reaction.parse pspecies pvalue pexpr unitVal  |>> Reaction
  let pinvoke   = Parser.pTry(  pNameLookahead .>> Parser.kw "(") 
                    .>>. Parser.sepBy pvalue pcomma 
                    .>>  Parser.kw ")"
                    |>> ModuleInvokation
  let pline = pinvoke <|> preaction <|> pinitial 
  let pbar  = Parser.kw "|"
  
  // module definitions parser
  let pmodule = 
    Parser.kw "module"                                                              
      >>. Parser.name .>>. Parser.paren (Parser.sepBy Parser.name pcomma)  // module_name(comma-separated args)
      .>> Parser.kw "=" 
      .>>. Parser.braces (Parser.opt pbar >>. Parser.sepBy pline pbar)
  
  // full CRN parser
  Parser.spaces 
    >>.  Parser.many pmodule >>= 
    (fun ms -> 
        Parser.opt pbar >>.
        Parser.sepBy pline pbar
        |>> fun is -> (create_from_instructions settings (moduleDefs @ ms, is), moduleDefs @ ms))

(* main CRN parser *)
let parse_crn pspecies (settings:Crn_settings<Key<Species>>) = ...


*)