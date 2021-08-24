// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module RulesDSD.Parser

open Parser
open RulesDSD.Syntax
open RulesDSD.Substitution

(* Terms syntax:
TERM ::= 
  (* standard logic programming terms *)
      // standard Prolog structures
         <VARIABLE>
      |  <CONSTANT>
      |  <INT>
      |  <OPERATION> 
      // lists
      |  '[' <TERM>* ']'            // finite  list of terms
      |  '[' <TERM>+ '|' <VAR> ']'  // partial list of terms
      // |  TCONS  // appends a term to a TList
      | <PATTERN>
      | <CONTEXT>
  (* Pattern related terms *)
      |  PATTERN
      |  SYSTEM
      |  HOLE
      |  CONTEXT

// operations, such as "+(1, 2)"
OPERATION ::= <OP_NAME> '(' (<TERM> ',')* <TERM>')'
OP_NAME   ::= '+' | '-' | '*' | '/' | NAME
*)
let pname = (name .>> spaces) <|> kw "_"
let pnick = kw ">" >>. kw "|" >>. kw "<"

let pnil st = (kw "nil" >>. preturn (Pattern<'s>.Nihil)) st

// grammar disambiguation types
type NextParser = PFunction | PPattern | PArithmetic | PVariable | PContext 
            

let rec plocation idProvider = 
  let f x = if x = "_" then (-1, "_") else idProvider x
  choice [ kw "@" >>. pname |>> (f >> Location.Var)
           preturn (Location.Var (-1, "_"))       ]

and ppattern (sp : Parser.t<'s>) idProvider = 
  let ppatternSite : t<'s * Location> = sp .>>. plocation idProvider
  choice [
    // naught pattern
    pnil 
    // <... or <...> patterns
    kw "<" >>. many1 ppatternSite >>= fun sites -> 
      choice [ 
        kw ">" >>. preturn (Pattern.Strand sites)
        preturn (Pattern.FivePrime sites)
      ]
    
    // ... or ...> or ...> | <... patterns
    many1 (sp .>>. plocation idProvider)  >>= fun sites -> 
      choice [ 
        kw ">" >>.  choice [ 
                      // ...> | <...
                      kw "|" >>. kw "<" >>. many (sp  .>>. plocation idProvider)  |>> fun moreSites -> Pattern.Nicking (sites, moreSites)
                      // ...>
                      preturn (Pattern.ThreePrime sites)
                    ]
        preturn (Pattern.Inner sites)
      ]
    ]

and psystem sp  = 
  (Parser.sepBy1 (between (kw "<") (kw ">") (many1 sp)) (kw "|") 
    |>> Process.OfList) 

and doPTerm (sp:Parser.t<'s>) cle (isExp:bool) (isComplex:bool) (idProvider : string -> (int*string)) (domainKeywords: string list (* the list of keywords that distinguish a variable from a domain specific element (e.g. in DSD "!" turns A in A!i from a generic variable to a DSD variable) *)) 
    st : Parser.Result<Term<'s>> = 
  let pexp = choice [ kw "+"  >>. preturn PArithmetic
                      kw "-"  >>. preturn PArithmetic
                      kw "*"  >>. preturn PArithmetic
                      kw "/"  >>. preturn PArithmetic
                      kw "**" >>. preturn PArithmetic
                      kw "%"  >>. preturn PArithmetic ]
  
  let termParser st : Parser.Result<Term<'s>> = doPTerm sp cle isExp isComplex idProvider domainKeywords st
  // let termParserNoExp st : Parser.Result<Term<'s>> = doPTerm sp cle false idProvider domainKeywords st
  let termParserNoExp st : Parser.Result<Term<'s>> = doPTerm sp cle true isComplex idProvider domainKeywords st
  let ptermExpression st = (Microsoft.Research.CRNEngine.Expression.parse termParserNoExp |>> Term<'s>.FromExpression) st
  
  (choice [
    
    // Naught pattern (like the empty reactant/product set 0 in CRNs)
    pnil |>> Term.Pat
    
    // term in parentheses
    paren (termParser)

    // NUMBERS
    plookAheadWith (
      pfloat >>. spaces >>. choice [ pexp
                                     preturn PPattern] )
      >>= fun disambiguation ->
        match disambiguation with 
        | PArithmetic -> ptermExpression
        | PPattern    -> pfloat .>> spaces |>> Term.Float
        | _ -> failwith ""
    
    plookAheadWith (
      pint32 >>. spaces >>. choice [ pexp
                                     preturn PPattern] )
      >>= fun disambiguation ->
        match disambiguation with 
        | PArithmetic -> ptermExpression
        | PPattern    -> pint32 .>> spaces |>> (float >> Term.Float)
        | _ -> failwith ""
    
    // CONSTANT
    kw "\"" >>. (manySatisfy ((<>) '"') .>> kw "\"") 
      |>> System.String.Concat 
      |>> Term.Const

    // TLIST
    sqBrackets 
      <| choice [
          sepBy termParser (kw ";") >>= 
            (fun listElements -> 
              choice [
                  kw "#" >>. pname |>> fun restOfList -> 
                    let f x = if x = "_" then (-1, "_") else idProvider x
                    List.foldBack (fun x y -> TCons(x, y))  listElements (Term.Var (f restOfList))
                  preturn (TList listElements)
              ]
            )
         ]
    
    // PROCESS
    psystem sp |>> Term.Proc // >> (Term.Canonical CLE.empty))

    // CRNs
    Parser.braces 
      (Parser.opt (Parser.kw "|") 
         >>. Parser.sepBy1 (
          // reaction parser with mass action or functional rate, mostly taken from CRNEngine
          let pmset st = let counted =
                           (Parser.pint32 +>>+ termParserNoExp  |>> fun (n,s) -> n, s)
                           <|> (termParserNoExp |>> (fun s -> 1, s))
                         Parser.sepBy counted (Parser.kw "+") |>> Term.TMSet 
                          <| st
          
          
          // parse reactants
          pmset .>>.
            Parser.choice [ 
              Parser.kw "~" >>. pmset |>> Some
              Parser.preturn None
            ]
          >>= fun (mset1, mset2) ->
             //parse arrows and rate
             let ptermExpression st = (Microsoft.Research.CRNEngine.Expression.parse termParser |>> Term<'s>.FromExpression) st
             let ptermSqExpression st = (Microsoft.Research.CRNEngine.Expression.parse (Microsoft.Research.CRNEngine.Key.parse termParser) 
                                                                                        |>> fun x -> Term<'s>.FromFuncExpression x) st
             let defaultRate = Term.Float 1.0
             let parseRate st = 
               choice [
                  Parser.bracket "{" "}" ptermExpression |>> fun x -> Term.Func(Keywords.massActionRate, [x])
                  Parser.bracket "[" "]" ptermSqExpression |>> fun x -> Term.Func (Keywords.functionalRate, [x])
               ] st
             Parser.choice [
               Parser.kw "->" >>. (parseRate |>> fun r -> (r,None)
                                   <|> Parser.preturn (Term.Func (Keywords.massActionRate, [defaultRate]), None))
               Parser.kw "<->" >>. (Parser.pTry (parseRate +>>+ parseRate)
                                     <|> (Parser.braces (ptermExpression +>> Parser.kw "," +>>+ ptermExpression) 
                                          <|> Parser.preturn (defaultRate, defaultRate)
                                          |>> fun (x,y) -> Term.Func (Keywords.massActionRate, [x]), Term.Func (Keywords.massActionRate, [y])))
                                   |>> fun (r,r') -> (r,Some r')
              ] 
              //parse products
              .>>. pmset >>= fun ((fwRate, bwRateOpt), products) ->
                let bwRate = match bwRateOpt with | None -> Term.Func("",[]) | Some x -> x
                //create reaction
                Parser.preturn (
                  match mset2 with
                  | None           -> Term.Func(Keywords.reaction, [Term.TMSet([]); mset1;     fwRate; bwRate; products])
                  | Some reactants -> Term.Func(Keywords.reaction, [mset1;          reactants; fwRate; bwRate; products]))
         ) (kw "|"))
      |>> Term<'s>.TCRN
      
    

    // disambiguation step
    (Parser.plookAheadWith
      (choice [ 
        (pname <|> kw "_") >>= fun str ->
          let isVar = str = "_" || System.Char.IsUpper (str.Chars 0)
          if isVar
            then
              choice [ 
                kw "->"  >>. preturn PVariable
                kw "<->" >>. preturn PVariable

                // <CONTEXT>
                kw "[[" >>. preturn PContext
                kw "[" >>. preturn PContext
        
                // <PATTERN>
                Parser.choice (domainKeywords |> List.map Parser.kw) >>. preturn PPattern

                // <EXPRESSION>
                if isExp  then preturn PVariable else pexp
                
                kw "is"  >>. preturn PVariable
                kw ">="  >>. preturn PVariable
                kw "<="  >>. preturn PVariable
                kw "<"   >>. preturn PVariable
                kw ">"   >>. preturn PVariable
                kw "="   >>. preturn PVariable
                kw "->"  >>. preturn PVariable
                kw "<->" >>. preturn PVariable
                
                // complex
                kw ":"  >>. preturn PVariable
        
                preturn PVariable
              ]
            elif List.contains str ["sum"; "prod"; "log"; "ceiling"; "floor"; "round"] 
              then preturn PArithmetic
            else 
              choice [
                // <OPERATION>
                kw "(" >>. preturn PFunction
              
                // <PATTERN>
                Parser.choice (domainKeywords |> List.map Parser.kw) >>. preturn PPattern

                kw "is" >>. preturn PPattern
                kw ">=" >>. preturn PPattern
                kw "<=" >>. preturn PPattern
                kw "<"  >>. preturn PPattern
                kw ">"  >>. preturn PPattern
                kw "="  >>. preturn PPattern

                kw "@" >>. preturn PPattern
                // CRN
                kw "->"  >>. preturn PPattern
                kw "<->" >>. preturn PPattern
                
                kw ":"  >>. preturn PPattern

                // if there is another name, the first name is a domain and the rest is a pattern 
                pname >>. preturn PPattern
                preturn PPattern
              ]
        ])
      >>= fun disambiguationWithoutComplex ->
        let disambiguation = 
          if not isComplex then disambiguationWithoutComplex
            else match disambiguationWithoutComplex with
                 | PFunction _   -> PFunction
                 // ":" has higher binding priority than e.g. + from an expression
                 | PPattern    _  -> PPattern
                 | PVariable
                 | PArithmetic _  -> PVariable
                 | PContext _     -> disambiguationWithoutComplex
        let f x = if x = "_" then (-1, "_") else idProvider x
        match disambiguation with
        | PFunction   -> pname .>>. paren (sepBy termParser (kw ",")) |>> Term.Func
        | PPattern    -> ppattern sp idProvider |>> Term.Pat
        | PArithmetic -> Microsoft.Research.CRNEngine.Expression.parse (doPTerm sp cle true isComplex idProvider domainKeywords) |>> Term.FromExpression
        | PVariable   -> choice [ pname  |>> (f >> Term.Var)
                                  kw "_" |>> fun x -> Term.Var (-1, x)]
                           // parses process parallel composition such as P1 | P2 
                           >>= fun v -> 
                             if isExp 
                               then preturn v
                               else choice [ kw "|" >>. sepBy1 termParser (kw "|") |>> fun ts -> TList (v :: ts)
                                             preturn v ]
        | PContext    -> (pname <|> kw "_") .>>. 
                            choice [
                              kw "[[" >>. name .>> spaces .>> kw "]]" |>> (f >> Term.Var)
                              many1 (sqBrackets (ppattern sp idProvider)) |>> (fun ps -> ps |> List.map Term.Pat |> TList)
                            ] |>> fun (ctxName, holes) -> Func (Keywords.context, [Term.Var (f ctxName); holes])
                              >>= fun v -> choice [ kw "|" >>. sepBy1 termParser (kw "|") |>> fun ts -> Func ("|", v :: ts)
                                                    preturn v ]

      )
  ] >>= fun t -> 
    // COMPLEXES
    Parser.plookAheadWith (choice [ kw "::" >>. preturn false 
                                    kw ":"  >>. preturn true 
                                    preturn false ])
      >>= fun isComplex ->
      if isComplex
        then 
          let pterm st = doPTerm sp cle isExp true idProvider domainKeywords st
          kw ":" >>. Parser.sepBy pterm (kw ":") >>= fun ts -> 
            let ts' = t::ts |> List.map (fun x-> 1, x)
            preturn (Term.TMSet ts')
        else preturn t
  ) st

and pterm (sp:Parser.t<'s>) cle (idProvider : string -> (int*string)) (domainKeywords: string list (* the list of keywords that distinguish a variable from a domain specific element (e.g. in DSD "!" turns A in A!i from a generic variable to a DSD variable) *)) 
    st : Parser.Result<Term<'s>> = doPTerm sp cle false false idProvider domainKeywords st
  

let ppredicate cle sp idProvider domainKeywords st = 
  let z = (pterm sp cle idProvider domainKeywords >>= fun t1 ->
    choice [
      kw "is" >>. pterm sp cle idProvider domainKeywords |>> fun t2 -> Pred ("is", [t1; t2])
      kw ">=" >>. pterm sp cle idProvider domainKeywords |>> fun t2 -> Pred (">=", [t1; t2])
      kw "<=" >>. pterm sp cle idProvider domainKeywords |>> fun t2 -> Pred ("<=", [t1; t2])
      kw "<"  >>. pterm sp cle idProvider domainKeywords |>> fun t2 -> Pred ("<",  [t1; t2])
      kw ">"  >>. pterm sp cle idProvider domainKeywords |>> fun t2 -> Pred (">",  [t1; t2])
      kw "="  >>. pterm sp cle idProvider domainKeywords |>> fun t2 -> 
        match t1 with
        | Func ("_ctx", [Term.Var (i, ctxName); TList holes]) -> Pred ("is", [t2; Term.Var (i, ctxName); TList holes])
        | Func ("_ctx", [Term.Var (i, ctxName); Term.Var (j, holes)])   -> Pred ("is", [t2; Term.Var (i, ctxName); Term.Var (j, holes)])
        | Func ("|", _)                             -> Pred ("is", [t2; t1])
        | _ -> match t2 with 
               | Func ("_ctx", [Term.Var (i, ctxName); TList holes])         -> Pred ("is", [t1; Term.Var (i, ctxName); TList holes])
               | Func ("_ctx", [Term.Var (i, ctxName); Term.Var (j, holes)]) -> Pred ("is", [t1; Term.Var (i, ctxName); Term.Var (j, holes)])
               | Func ("|", _)                                               -> Pred ("is", [t1; t2])
               | _ -> Pred ("=",  [t1; t2])
        
      // if the parser caught a function but there is no subsequent operation "is", "=", etc.
      // then turn the function into a predicate
      (match t1 with
       | Term.Func (name, args) -> preturn <| Pred (name, args)
       | _ -> let msg = sprintf "Unexpected predicate %s" (Term.ToStringWith cle t1)
              failParser msg)
      ])
  z st
(* Atoms syntax:
ATOM ::= <NAME> '(' <TERM>* ')'
*)
let patom cle x y = ppredicate cle x y

(* Literals syntax: 
LITERAL ::= <ATOM>
          | 'not' <ATOM>
*)
let pliteral cle sp idProvider domainKeywords = 
  choice [
    kw "not" >>. choice [ paren (patom cle sp idProvider domainKeywords); (patom cle sp idProvider domainKeywords)] |>> Neg
    (patom cle sp idProvider domainKeywords) |>> Pos
  ]

(* clause syntax:
CLAUSE ::= <NAME> '(' <TERM>* ')' '.'                       // a stated fact in the prolog database
         | <NAME> '(' <TERM>* ')' ':-'  <LITERAL>* '.'       // a derived fact, assuming its body ( the LITERAL* ) holds
*)
let pclause (cle:CLE<'s, 'a>) sp idProvider = 
  let cache         = idProvider ()
  let speciesParser = sp cache
  pname .>>. paren (sepBy (pterm speciesParser cle cache cle.domainKeywords) (kw ",")) >>= fun p -> 
    let predicate = Pred p
    choice [ kw "."  >>. preturn (Clause.Create predicate)
             kw ":-" >>. Parser.sepBy1 (pliteral cle speciesParser cache cle.domainKeywords) (kw ",") .>> kw "." >>= fun atoms ->
                preturn (Clause.Create(predicate, atoms)) ]
    |>> cle.disambiguateVars

(* program syntax: *)
let pprogram (cle:CLE<'s, 'a>) (sp : (string -> int * string) -> Parser.t<'s>) : Parser.t<RulesProgram<'s>> = 
  Parser.many (pclause cle sp idProvider) 
  |>> toProgram