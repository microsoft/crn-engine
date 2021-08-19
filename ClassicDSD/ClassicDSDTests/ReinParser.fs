module Microsoft.Research.DNA.ReinParser

open Xunit
open Parser

(** TOKENS **)
let DIRECTIVE     = "directive"
let UNIQUENESS    = "uniqueness"
let LIMIT         = "limit"
let REGULATION    = "regulation"
let LENGTH        = "length"
let UPDATES       = "updates"
let UPATH         = "path"
let INTERACTIONS  = "interactions"
let FULL          = "full"
let PATHS         = "paths"
let LEGACY        = "legacy"
let CARDINALITY   = "cardinality"
let DEFAULT       = "default"
let NOTHRESHOLDS  = "noThresholds"
let SYNC          = "sync"
let ASYNC         = "async"
let FIXPOINT      = "fixpoint"
let HYPOTHESIS    = "hypothesis"
let SAT           = "satisfies"
let POS           = "positive"
let NEG           = "negative"
let OPT           = "optional"
let KOT           = "KO"
let FET           = "FE"

let IMPLIES       = "implies"
let TRUE          = "true"
let FALSE         = "false"
let NOT           = "not"
let AND           = "and"
let OR            = "or"

(* symbols*)
let SAT_SYM = "|="
let TO      = ".."
let COMMA   = ","
let PLUS    = "+"
let MINUS   = "-"
let EXCL    = "!"

// punctuation
let ASSIGN  = ":="
let LCBRA   = ""
let RCBRA   = ""
let LSBRA   = "["
let RSBRA   = "]"
let LPAREN  = "("
let RPAREN  = ")"
let DOT     = "."

// Logical
let NOT_SYM     = "!"
let AND_SYM     = "&"
let OR_SYM      = "|"
let IMPLIES_SYM = "=>"
let EQ          = "="
let EOL         = ";"


let keywords = [| 
    DIRECTIVE
    UNIQUENESS
    LIMIT
    REGULATION
    LENGTH
    UPDATES
    UPATH
    INTERACTIONS
    FULL
    PATHS
    LEGACY
    CARDINALITY
    DEFAULT
    NOTHRESHOLDS
    SYNC
    ASYNC
    FIXPOINT
    HYPOTHESIS
    SAT
    POS
    NEG
    OPT
    KOT
    FET
    IMPLIES
    TRUE
    FALSE
    NOT
    AND
    OR
|]


let pint = pint32 .>> spaces



let pidentifier = pTry (many1Satisfy (fun c -> isLetter c || isDigit c || c = '_')
                      >>= fun str -> if Array.contains str keywords
                                      then failParser <| "keyword \"" + str + "\" cannot be used as an identifier"
                                      else preturn str .>> spaces 
                                      ) <?> "an identifier"
type vars = NameVar of string
          | PathVar of string

//variable names
let pname = pchar '$' >>. pidentifier |>> NameVar // alpha+ NAME (lexeme lexbuf) //property
let ppath = pchar '#' >>. pidentifier |>> PathVar // alpha+ PATH  (lexeme lexbuf) //experiment


// predicates
type predicate = AbsPred of vars
               | ConcPred of vars * int * vars
               | AbsPPred of vars * int
               | AbsKPred of vars * vars
let ppredicate = 
  choice [
    pname >>= fun n -> 
      choice 
        [ 
          sqBrackets pint >>= fun i -> preturn (AbsPPred (n, i))
          preturn (AbsPred n) 
        ]
  ]
  

// integer lists
let pints = sepBy1 
              (pint >>= fun i -> choice [ kw TO >>. pint >>= fun j -> preturn [i..j]
                                        ; preturn [i]]) 
              (kw COMMA)
            |>> (List.concat >> List.distinct)

//BTerm: 
type bvar = AbsStateVar of string
          | AbsPStateVar of int * string
          | AbsKStateVar of vars * string
          | StateVar     of vars * int * string
          | KOVar        of vars * string
          | FEVar        of vars * string
          | AbsKOVar     of string
          | AbsFEVar     of string

type bterm = BConst of bool
           | BVar of bvar
           | Fixpoint     of vars * int
           | Pred         of predicate

let pbSpecies = 
    pidentifier >>= fun sp -> 
      choice [
        sqBrackets pint |>> fun i -> BVar (AbsPStateVar (i, sp))
        preturn (BVar (AbsStateVar sp))
      ]

let pbTerm = 
  choice [
    kw TRUE  >>. preturn (BConst true)
    kw FALSE >>. preturn (BConst false)
    pint     >>= fun i -> preturn (if i > 0 then BConst true else BConst false)
    ppredicate >>= (Pred >> preturn)
    ppath >>= fun p ->
      choice [
         sqBrackets pint >>=  fun i -> 
          choice [
            kw DOT >>. pidentifier >>= fun sp -> preturn (BVar(StateVar(p,i,sp)))
            kw SAT >>. pname       >>= fun n  -> preturn (Pred (ConcPred (n, i, p)))
            kw SAT_SYM >>. pname       >>= fun n  -> preturn (Pred (ConcPred (n, i, p)))
          ]
         kw DOT >>. 
          choice [ 
            kw KOT >>. paren pidentifier >>= fun sp -> preturn (BVar (KOVar (p, sp)))
            kw FET >>. paren pidentifier >>= fun sp -> preturn (BVar (FEVar (p, sp)))
            pidentifier                  >>= fun sp -> preturn (BVar (AbsKStateVar (p, sp))) 
          ]
          kw SAT >>. pname >>= fun n -> preturn (Pred (AbsKPred (n, p)))
        ]
    kw KOT >>. paren pidentifier >>= (AbsKOVar >> BVar >> preturn)
    kw FET >>. paren pidentifier >>= (AbsFEVar >> BVar >> preturn)
    kw FIXPOINT >>. paren (ppath .>>. sqBrackets pint) >>= (Fixpoint >> preturn)
  ]

//BExpr:
type bexpr = BTerm of bterm
           | Beq   of bexpr * bexpr
           | BImp  of bexpr * bexpr
           | BAnd  of bexpr * bexpr
           | BOr   of bexpr * bexpr
           | BNot  of bexpr

let rec pbAtom isNested st = 
  if isNested
    then 
      choice [ 
        pbTerm    |>> BTerm
        pbSpecies |>> BTerm
        paren (pbExpr true)
      ]  st
    else
      choice [ 
        pbTerm    |>> BTerm
        paren (pbExpr true)
      ]  st

and pbExpr isNested st = 
 choice [
    pbAtom isNested >>= fun e1 ->
      choice [
        kw EQ           >>. pbExpr true >>= fun e2 -> preturn (Beq  (e1, e2))
        kw IMPLIES      >>. pbExpr true >>= fun e2 -> preturn (BImp (e1, e2))
        kw IMPLIES_SYM  >>. pbExpr true >>= fun e2 -> preturn (BImp (e1, e2))
        kw AND          >>. pbExpr true >>= fun e2 -> preturn (BAnd (e1, e2))
        kw AND_SYM      >>. pbExpr true >>= fun e2 -> preturn (BAnd (e1, e2))
        kw OR           >>. pbExpr true >>= fun e2 -> preturn (BOr (e1, e2))
        kw OR_SYM       >>. pbExpr true >>= fun e2 -> preturn (BOr (e1, e2))
        preturn e1
      ]
    kw NOT_SYM  >>. pbExpr true |>> BNot
    kw NOT      >>. pbExpr true |>> BNot
  ]
  st

//Interaction:
type interaction = Interaction of string * string * bool * bool
let pinteraction sp1 = pidentifier >>= fun sp2 ->
  choice [
    kw POS >>.
      choice [
        kw OPT >>. preturn (Interaction (sp1, sp2, true, false))
        preturn (Interaction (sp1, sp2, true, true))
      ]
    kw NEG >>. 
      choice [
        kw OPT >>. preturn (Interaction (sp1, sp2, false, false))
        preturn (Interaction (sp1, sp2, false, true))
      ]
  ]

//Species:

type species = Species of string * int list option * bool * bool * bool 

let pintsOpt = choice [ paren pints |>> Some
                        preturn None ]

let pspecies sp = 
  choice [
    sqBrackets (
      choice [
        // | PLUS            RSBRA LPAREN Ints RPAREN {Species($1,Some($6),false,true,false)}
        // | PLUS            RSBRA                    {Species($1,None    ,false,true,false)}
        // | PLUS MINUS      RSBRA LPAREN Ints RPAREN {Species($1,Some($7),true,true,false)}
        // | PLUS MINUS      RSBRA                    {Species($1,None,true,true,false)}
        // | PLUS MINUS EXCL RSBRA LPAREN Ints RPAREN {Species($1,Some($8),true,true,true)}
        // | PLUS MINUS EXCL RSBRA                    {Species($1,None,true,true,true)}
        // | PLUS EXCL       RSBRA LPAREN Ints RPAREN {Species($1,Some($7),false,true,true)}
        // | PLUS EXCL       RSBRA                    {Species($1,None,false,true,true)}
        // | PLUS EXCL MINUS RSBRA LPAREN Ints RPAREN {Species($1,Some($8),true,true,true)}
        // | PLUS EXCL MINUS RSBRA                    {Species($1,None,true,true,true)}
        kw PLUS >>. choice [
          kw MINUS >>. choice [
            kw EXCL >>. preturn (true, true, true)
            preturn (true, true, false)
          ]

          kw EXCL >>. choice [
            kw MINUS >>. preturn (true,true,true)
            preturn (false,true,true)
          ]
          
          preturn (false,true,false)
        ]

        //| SPECIES LSBRA MINUS           RSBRA LPAREN Ints RPAREN {Species($1,Some($6),true,false,false)}
        //| SPECIES LSBRA MINUS           RSBRA                    {Species($1,None,true,false,false)}
        //| SPECIES LSBRA MINUS PLUS      RSBRA LPAREN Ints RPAREN {Species($1,Some($7),true,true,false)}
        //| SPECIES LSBRA MINUS PLUS      RSBRA                    {Species($1,None,    true,true,false)}
        //| SPECIES LSBRA MINUS PLUS EXCL RSBRA LPAREN Ints RPAREN {Species($1,Some($8), true,true,true)}
        //| SPECIES LSBRA MINUS PLUS EXCL RSBRA                    {Species($1,None,     true,true,true)}
        //| SPECIES LSBRA MINUS EXCL      RSBRA LPAREN Ints RPAREN {Species($1,Some($7),true,false,true)}
        //| SPECIES LSBRA MINUS EXCL      RSBRA                    {Species($1,None,    true,false,true)}
        //| SPECIES LSBRA MINUS EXCL PLUS RSBRA LPAREN Ints RPAREN {Species($1,Some($8),true,true,true)}
        //| SPECIES LSBRA MINUS EXCL PLUS RSBRA                    {Species($1,None,    true,true,true)}
        kw MINUS >>. choice [
          kw PLUS >>. choice [
            kw EXCL >>. preturn (true, true, true)
            preturn (true, true, false)
          ]

          kw EXCL >>. choice [
            kw PLUS  >>. preturn (true,true,true)
            preturn (true,false,true)
          ]
          
          preturn (true,false,false)
        ]

        // | SPECIES LSBRA EXCL            RSBRA LPAREN Ints RPAREN {Species($1,Some($6),false,false,true)}
        // | SPECIES LSBRA EXCL            RSBRA                    {Species($1,None    ,false,false,true)}
        // | SPECIES LSBRA EXCL PLUS       RSBRA LPAREN Ints RPAREN {Species($1,Some($7),false,true,true)}
        // | SPECIES LSBRA EXCL PLUS       RSBRA                    {Species($1,None    ,false,true,true)}
        // | SPECIES LSBRA EXCL PLUS MINUS RSBRA LPAREN Ints RPAREN {Species($1,Some($8),true,true,true)}
        // | SPECIES LSBRA EXCL PLUS MINUS RSBRA                    {Species($1,None    ,true,true,true)}
        // | SPECIES LSBRA EXCL MINUS      RSBRA LPAREN Ints RPAREN {Species($1,Some($7),true,false,true)}
        // | SPECIES LSBRA EXCL MINUS      RSBRA                    {Species($1,None    ,true,false,true)}
        // | SPECIES LSBRA EXCL MINUS PLUS RSBRA LPAREN Ints RPAREN {Species($1,Some($8),true,true,true)}
        // | SPECIES LSBRA EXCL MINUS PLUS RSBRA                    {Species($1,None    ,true,true,true)}
        kw EXCL >>. choice [
          kw PLUS >>. choice [
            kw MINUS >>. preturn (true, true, true)
            preturn (false, true, true)
          ]

          kw MINUS >>. choice [
            kw PLUS >>. preturn (true,true,true)
            preturn (true,false,true)
          ]
          
          preturn (false,false,true)
        ]
      ]
    ) .>>. pintsOpt >>= fun ((b1,b2,b3), is) -> preturn (Species (sp,is,b1,b2,b3))
    // | SPECIES LPAREN Ints RPAREN {Species($1,Some($3),false,false,false)}
    // | SPECIES                    {Species($1,None,false,false,false)}
    pintsOpt >>= fun is -> preturn (Species (sp,is,false,false,false))
  ]

//Directive:
type uniqueness = UInteractions
                | UModel
                | UAll
                | UPath of vars * int
type regulation = RDefault
                | RCardinality
                | RLegacy
                | RNoThresholds
type updates    = USync
                | UAsync
type directive = DU       of uniqueness 
               | DReg     of regulation
               | DUp      of updates
               | DLimit   of int
               | DLength  of int 

let pdirective = 
  choice [
    kw UNIQUENESS >>. 
      choice [
        kw INTERACTIONS              |>> ((fun _ -> UInteractions) >> DU)
        kw FULL                      |>> ((fun _ -> UModel)        >> DU)
        kw PATHS                     |>> ((fun _ -> UAll)          >> DU)
        kw UPATH >>. ppath .>>. pint |>> (UPath >> DU)
      ]
    kw REGULATION >>. 
      choice [
        kw DEFAULT      |>> ((fun _ -> RDefault)      >> DReg)
        kw CARDINALITY  |>> ((fun _ -> RCardinality)  >> DReg)
        kw LEGACY       |>> ((fun _ -> RLegacy)       >> DReg)
        kw NOTHRESHOLDS |>> ((fun _ -> RNoThresholds) >> DReg)
      ]
    kw UPDATES >>.
      choice [
        kw SYNC  |>> ((fun _ -> USync) >> DUp)
        kw ASYNC |>> ((fun _ -> UAsync) >> DUp)
      ]
    kw LIMIT  >>. pint |>> DLimit
    kw LENGTH >>. pint |>> DLength
  ]

//Line:
// |Species                                     { $1 }
// |Interaction                                 { $1 }
// |HYPOTHESIS LCBRA BExpr RCBRA                { Hypothesis($3, None) }
// |HYPOTHESIS LCBRA BExpr RCBRA DESCRIPTION    { Hypothesis($3, Some($5)) }
// |NAME ASSIGN LCBRA BExpr RCBRA               { Assign($1,$4) }
// |BExpr                                       { Assert($1,None)     }
// |BExpr DESCRIPTION                           { Assert($1,Some($2)) }
// |DIRECTIVE Directive                         { Directive($2)       }
type line = LSpecies     of species
          | LInteraction of interaction
          | LHypothesis  of bexpr * string option
          | LAssign      of vars * bexpr
          | LAssert      of bexpr * string option
          | LDirective   of directive
let pdescription = 
  choice [
    kw "\"" >>. manySatisfy (fun c -> c <> '\"') >>. kw "\"" |>> Some
    preturn None
  ]

let pline = 
  let nested = true
  choice [
    kw HYPOTHESIS >>. braces (pbExpr nested) .>>. pdescription |>> LHypothesis
    pidentifier >>= fun sp1 -> choice [
                                 pinteraction sp1 |>> LInteraction
                                 pspecies     sp1 |>> LSpecies
                               ]
    pname  .>> kw ASSIGN .>>. braces (pbExpr nested) |>> LAssign
    pbExpr (not nested) .>>. pdescription            |>> LAssert
    pdirective                                       |>> LDirective
  ]

let parse = many1 (pline .>> kw EOL)

//[<Fact>]
let ``REIN parsing test 1`` () = 
  let prog = "A(0..15); B(0..15); C(0..15); D(0..15);

A	B 	positive	;
B	A 	positive 	optional;
B	C	positive	optional;
C	B	negative 	;
C	A	positive	optional;
A	C	negative	optional;	
B	D	positive	optional;
D	A	negative;
D	C	positive	optional;
"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 2`` () = 
  let prog = "
#ExperimentOne[0] |= $InitialValues;
#ExperimentOne[1] |= $SecondValues;
#ExperimentOne[18]|= $FinalValues;
#ExperimentOne[19]|= $FinalValues;

$InitialValues :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1
};

$SecondValues :=
{
 A = 1 and
 B = 1
};

$FinalValues :=
{
 A = 1 and 
 B = 1 and 
 C = 0 and 
 D = 0
};
"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 3`` () = 
  let prog = "
#ExperimentOne[0] |= $InitialValues;
#ExperimentOne[1] |= $SecondValues;
#ExperimentOne[18]|= $FinalValues;
#ExperimentOne[19]|= $FinalValues;

$InitialValues :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1
};

$SecondValues :=
{
 A = 1 and
 B = 1
};

$FinalValues :=
{
 A = 1 and 
 B = 1 and 
 C = 0 and 
 D = 0
};"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 4`` () = 
  let prog = "A[+-](0..15); 
  B[-](0..15); C[+](0..15); D(0..15);

A	B 	positive	;
B	A 	positive 	optional;
B	C	positive	optional;
C	B	negative 	;
C	A	positive	optional;
A	C	negative	optional;	
B	D	positive	optional;
D	A	negative;
D	C	positive	optional;
"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 5`` () = 
  let prog = "A(0,13); B(4,5); C(10,15); D(7,12);

A	B 	positive;	
B	A 	positive 	optional;
B	C	positive	optional;
C	B	negative; 	
C	A	positive	optional;
A	C	negative	optional;	
B	D	positive	optional;
D	A	negative;
D	C	positive	optional;
"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 6`` () = 
  let prog = "
A(18); B(0..15); C(0..15); D(0..15); E(0..15); F(0..15); G(0..15); H(0..15);

A	H	positive	optional;
B	A	positive;
C	A	positive;
D	A	positive;
E	A	negative;
F	A	negative;
G	A	negative;
B	B	positive;
C	C	positive;
D	D	positive;
E	E	positive;
F	F	positive;
G	G	positive;
"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 7`` () = 
  let prog = "
// Different initial conditions that correspond to different cases to examine

#ExperimentOne[0] |= $InitialEqualActivatorsAndRepressorsAOff;
#ExperimentOne[1] |= $FinalEqualActivatorsAndRepressorsAOff;

$InitialEqualActivatorsAndRepressorsAOff :=
{
 A = 0 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};

$FinalEqualActivatorsAndRepressorsAOff :=
{
 A = 0 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};


#ExperimentTwo[0] |= $InitialEqualActivatorsAndRepressorsAOn;
#ExperimentTwo[1] |= $FinalEqualActivatorsAndRepressorsAOn;

$InitialEqualActivatorsAndRepressorsAOn :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};

$FinalEqualActivatorsAndRepressorsAOn :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};

#ExperimentThree[0] |= $InitialMoreActivatorsThanRepressorsAOff;
#ExperimentThree[1] |= $FinalMoreActivatorsThanRepressorsASwitchesOn;
#ExperimentThree[2] |= $FinalMoreActivatorsThanRepressorsASwitchesOn;

$InitialMoreActivatorsThanRepressorsAOff :=
{
 A = 0 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 0 and 
 G = 1
};

$FinalMoreActivatorsThanRepressorsASwitchesOn :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 0 and 
 G = 1
};

#ExperimentFour[0] |= $InitialMoreActivatorsThanRepressorsAOn;
#ExperimentFour[1] |= $FinalMoreActivatorsThanRepressorsAStaysOn;


$InitialMoreActivatorsThanRepressorsAOn :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 0 and 
 G = 1
};

$FinalMoreActivatorsThanRepressorsAStaysOn :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 0 and 
 G = 1
};


#ExperimentFive[0] |= $InitialMoreRepressorsThanActivatorsAOn;
#ExperimentFive[1] |= $FinalMoreRepressorsThanActivatorsASwitchesOff;
#ExperimentFive[2] |= $FinalMoreRepressorsThanActivatorsASwitchesOff;

$InitialMoreRepressorsThanActivatorsAOn :=
{
 A = 1 and
 B = 0 and
 C = 0 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};

$FinalMoreRepressorsThanActivatorsASwitchesOff :=
{
 A = 0 and
 B = 0 and
 C = 0 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};
"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 8`` () = 
  let prog = "
// Under conditions of equal activators and repressors then A should stay as it is so this specification should be UNSAT

#ExperimentOne[0] |= $InitialEqualActivatorsAndRepressorsAOff;
#ExperimentOne[1] |= $FinalEqualActivatorsAndRepressorsAOn;

$InitialEqualActivatorsAndRepressorsAOff :=
{
 A = 0 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};

$FinalEqualActivatorsAndRepressorsAOn :=
{
 A = 1 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};


"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 9`` () = 
  let prog = "// This should be UNSAT

#ExperimentThree[0] |= $InitialMoreActivatorsThanRepressorsAOff;
#ExperimentThree[1] |= $FinalMoreActivatorsThanRepressorsAStaysOff;
#ExperimentThree[2] |= $FinalMoreActivatorsThanRepressorsAStaysOff;

$InitialMoreActivatorsThanRepressorsAOff :=
{
 A = 0 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 0 and 
 G = 1
};

$FinalMoreActivatorsThanRepressorsAStaysOff :=
{
 A = 0 and
 B = 1 and
 C = 1 and 
 D = 1 and
 E = 1 and
 F = 0 and 
 G = 1
};

"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result

//[<Fact>]
let ``REIN parsing test 10`` () = 
  let prog = "// This should be UNSAT

#ExperimentFive[0] |= $InitialMoreRepressorsThanActivatorsAOn;
#ExperimentFive[1] |= $FinalMoreRepressorsThanActivatorsAStaysOn;
#ExperimentFive[2] |= $FinalMoreRepressorsThanActivatorsAStaysOn;

$InitialMoreRepressorsThanActivatorsAOn :=
{
 A = 1 and
 B = 0 and
 C = 0 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};

$FinalMoreRepressorsThanActivatorsAStaysOn :=
{
 A = 1 and
 B = 0 and
 C = 0 and 
 D = 1 and
 E = 1 and
 F = 1 and 
 G = 1
};
"
  let result = Parser.from_string parse prog
  Assert.NotEmpty result