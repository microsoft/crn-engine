// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.ParsingTests

open Xunit
open Microsoft.Research.CRNEngine

open RulesDSD.Syntax
open Microsoft.Research.GEC
open Microsoft.Research.GEC.LogicGEC

[<Fact(DisplayName="Logic GEC - parsing - single part")>]
let testParseSinglePart() = 
  let pName = "r0011"
  let pType = "prom"
  let idProvider = newIdProvider () (ref Map.empty)

  let a : Part  = pName |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (Const pName, Term.wildcard)
  Assert.Equal(e, a)

  let a : Part  = pName + "::" + pType |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (Const pName, Const "prom") 
  Assert.Equal(e, a)

  let a : Part  = pName + "::" + pType |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (Const pName, Const pType)
  Assert.Equal(e, a)

  let a : Part  = pName + "::" + pType |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (Const pName, Const pType)
  Assert.Equal(e, a)

  // Variables
  let x        = "X"
  let y        = "Y"
  let xTerm    = idProvider x |> Term.Var
  let yTerm    = idProvider y |> Term.Var
  let a : Part = x 
                 |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (xTerm, Term.wildcard) 
  Assert.Equal(e, a)

  let a : Part = x + "::" + pType 
                 |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (xTerm, Const pType) 
  Assert.Equal(e, a)

  let a : Part = x + "::" + y 
                 |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (xTerm, yTerm) 
  Assert.Equal(e, a)

  let a : Part = pName + "::" + y 
                 |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  let e : Part = Part.Create (Const pName, yTerm) 
  Assert.Equal(e, a)


[<Fact(DisplayName="Logic GEC - parsing - Classic GEC parts")>]
let testParseClassicGecParts() = 
  let idProvider = newIdProvider () (ref Map.empty)
  
  // r0011
  let e = ModelGenerationTests.r0011
  let a = """r0011::prom""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)

  // r0040
  let e = ModelGenerationTests.r0040
  let a = """r0040::prom""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)

  // r0051
  let e = ModelGenerationTests.r0051
  let a = """r0051::prom""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)

  // b0034
  let e = ModelGenerationTests.b0034
  let a = """b0034::rbs""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)

  // c0012
  let e = ModelGenerationTests.c0012
  let a = """c0012::cds""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)

  // c0040
  let e = ModelGenerationTests.c0040
  let a = """c0040::cds""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)
  
  // c0051
  let e = ModelGenerationTests.c0051
  let a = """c0051::cds""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)
  
  // b0015
  let e = ModelGenerationTests.b0015
  let a = """b0015::ter""" |> Parser.from_string (Part.parse idProvider cle.domainKeywords)
  Assert.Equal(e, a)

let db = """
part( r0011::prom).
  neg( r0011, lacI, 1.0, 0.5, 0.00005).
  con( r0011, 0.1).

part( r0040::prom).
  neg( r0040, tetR, 1.0, 0.5, 0.00005).
  con( r0040, 0.09).

part( r0051::prom).
  neg( r0051, cI, 1.0, 0.5, 0.00005).
  con( r0051, 0.12 ).

part( b0034::rbs ).
  rbs(b0034, 0.1).

part( c0012::cds ).
  codes( c0012, lacI, 0.01 ).
part( c0040::cds ).
  codes( c0040, tetR, 0.01 ).
part( c0051::cds ).
  codes(c0051, cI, 0.01).

part( b0015::ter).
""" 
[<Fact(DisplayName="Logic GEC - parsing - Classic GEC DB")>]
let testParseClassicGecDB() = 
  let e = ModelGenerationTests.dummyDB |> RulesDSD.Syntax.toProgram
  let a = Parser.from_string (pGecProgram) db
  let eSeq = e |> Dictionary.toSeq
  let aSeq = a |> Dictionary.toSeq
  // check signatures
  let eSig = eSeq |> Seq.map fst |> Seq.toList
  let aSig = aSeq |> Seq.map fst |> Seq.toList
  List.zip eSig aSig 
  |> List.map (fun (e,a) -> Assert.Equal(e,a))
  |> ignore
  
  // check predicates
  eSig 
  |> List.map(fun s -> 
    let eClauses = e.[s] |> Set.toList |> List.sort
    let aClauses = a.[s] |> Set.toList |> List.sort
    Assert.Equal(eClauses.Length, aClauses.Length)

    List.zip eClauses aClauses
    |> List.map (fun (e,a) ->Assert.Equal(e,a)))


let simpleRegulationSemantics = """
// negative regulation
reactions(S, CRN) :-
  S = C[ Prom::prom  Rbs::rbs Pcr::cds Ter::ter ], 
  neg(Prom, TF, Kb, Ku, Kt),
  G = gene(<Prom Rbs Pcr Ter>), 
  M = mRNA(<Rbs Pcr>),
  CRN = { TF + G <->{Kb}{Ku} TF:G
        | G:TF ->{Kt} G:TF + M    }.

// constitutive expression
reactions(S,CRN) :-
  S = C[ Prom::prom Rbs::rbs Pcr::cds Ter::ter ], 
  con(Prom, Kt),
  G = gene(<Prom Rbs Pcr Ter>), 
  M = mRNA(<Rbs Pcr>),
  CRN = { G ->{Kt} G + M }.

// translation
reactions(M, CRN) :-  
  M   = mRNA(S),
  S   = C[Rbs::rbs Pcr::cds ],
  rbs(Rbs, Kl),
  codes(Pcr, P, Kd),
  CRN = { M ->{Kl} M + P
        | P ->{Kd}       }.

// degradation
reactions(M, CRN) :-
  M = mRNA(S),
  CRN = {M ->{0.001}}.
"""


[<Fact(DisplayName="Logic GEC - parsing - simple gene regulation semantics", Skip="Currently non-deterministic, and sometimes fails")>]
let testParseSimpleRegulationSemantics() = 
  let a = Parser.from_string (pGecProgram) simpleRegulationSemantics
  let e : RulesProgram<Element> = ModelGenerationTests.getMassActionRegulation () |> RulesDSD.Syntax.toProgram
  
  let eSeq = e |> Dictionary.toSeq
  let aSeq = a |> Dictionary.toSeq
  // check signatures
  let eSig = eSeq |> Seq.map fst |> Seq.toList
  let aSig = aSeq |> Seq.map fst |> Seq.toList  
  
  List.zip eSig aSig |> List.iter Assert.Equal
  
  eSig 
  |> List.map(fun s -> 
    let eClauses = e.[s] |> Set.toList |> List.sort
    let aClauses = a.[s] |> Set.toList |> List.sort
    Assert.Equal(eClauses.Length, aClauses.Length)

    List.zip eClauses aClauses |> List.iter Assert.Equal
  )


[<Fact(DisplayName="Logic GEC - parsing - receiver device")>]
let testParseDeviceEnumerationReceiver() = 
  let code = """
  | <prom rbs X1::pcr ter> 
  | <X2::prom rbs X3::pcr ter>
  | codes(X1,Receiver) 
  | pos(Receiver:Signal,X2) 
  | codes(X3,gfp)
  | bind(Receiver,Signal)  
  """

  let semantics = new System.Collections.Generic.Dictionary<_,_>()
  let settings  = { Microsoft.Research.GEC.Settings.Gec_settings.default_settings with rules = Some semantics }
  let parser    = Microsoft.Research.GEC.Program.parseLogic settings
  let actual    = Parser.from_string parser code
  
  let idProvider = makeProvider()

  let toPattern (p:Part) = Term.Pat (Pattern.Inner [Element.Part p, Location.wildcard])

  let expected : Microsoft.Research.GEC.Program.LogicProgram = 
    { settings = settings
      rules    = semantics
      program  = [ LogicGEC.Device [ Element.Part (Part.CreateByType "prom")
                                     Element.Part (Part.CreateByType "rbs")
                                     Element.Part (Part.Create(Term.Var(idProvider "X1"), Term.Const "pcr"))
                                     Element.Part (Part.CreateByType "ter") ]
                   LogicGEC.Device [ Element.Part (Part.Create(Term.Var(idProvider "X2"), Term.Const "prom"))
                                     Element.Part (Part.CreateByType "rbs")
                                     Element.Part (Part.Create(Term.Var(idProvider "X3"), Term.Const "pcr"))
                                     Element.Part (Part.CreateByType "ter") ]
                   LogicGEC.Constraint(Literal.Pos (Pred("codes", [ Term.Var(idProvider "X1"); Term.Var(idProvider "Receiver")])))
                   LogicGEC.Constraint(Literal.Pos (Pred("pos",   [ Term.TMSet [1, Term.Var(idProvider "Receiver"); 1, Term.Var(idProvider "Signal")] |> Term.Canonical cle; Term.Var(idProvider "X2") ])))
                   LogicGEC.Constraint(Literal.Pos (Pred("codes", [ Term.Var(idProvider "X3"); Term.Const "gfp" ])))
                   LogicGEC.Constraint(Literal.Pos (Pred("bind",  [ Term.Var(idProvider "Receiver"); Term.Var(idProvider "Signal")])))

      ]
    }
  
  Assert.Equal(expected, actual)



[<Fact(DisplayName="Logic GEC - parsing - receiver device with initials")>]
let testParseDeviceEnumerationReceiverWithInitials() = 
  let code = """
  | 10 <prom rbs X1::pcr ter> 
  | 10 <X2::prom rbs X3::pcr ter>
  | codes(X1,Receiver) 
  | pos(Receiver:Signal,X2) 
  | codes(X3,gfp)
  | bind(Receiver,Signal)  
  """

  let semantics = new System.Collections.Generic.Dictionary<_,_>()
  let settings  = { Microsoft.Research.GEC.Settings.Gec_settings.default_settings with rules = Some semantics }
  let parser    = Microsoft.Research.GEC.Program.parseLogic settings
  let actual    = Parser.from_string parser code
  
  let idProvider = makeProvider()

  let toPattern (p:Part) = Term.Pat (Pattern.Inner [Element.Part p, Location.wildcard])

  let expected : Microsoft.Research.GEC.Program.LogicProgram = 
    { settings = settings
      rules    = semantics
      program  = [ LogicGEC.Initial 
                    ( Term.Float 10.0,
                      [ Element.Part (Part.CreateByType "prom")
                        Element.Part (Part.CreateByType "rbs")
                        Element.Part (Part.Create(Term.Var(idProvider "X1"), Term.Const "pcr"))
                        Element.Part (Part.CreateByType "ter") ]
                       |> List.singleton
                       |> Process.OfList
                       |> Term.Proc 
                       |> toComplex)
                   LogicGEC.Initial 
                    ( Term.Float 10.0,
                      [ Element.Part (Part.Create(Term.Var(idProvider "X2"), Term.Const "prom"))
                        Element.Part (Part.CreateByType "rbs")
                        Element.Part (Part.Create(Term.Var(idProvider "X3"), Term.Const "pcr"))
                        Element.Part (Part.CreateByType "ter") ]
                      |> List.singleton
                      |> Process.OfList
                      |> Term.Proc 
                      |> toComplex)
                   LogicGEC.Constraint(Literal.Pos (Pred("codes", [ Term.Var(idProvider "X1"); Term.Var(idProvider "Receiver")])))
                   LogicGEC.Constraint(Literal.Pos (Pred("pos",   [ Term.TMSet [1, Term.Var(idProvider "Receiver"); 1, Term.Var(idProvider "Signal")] |> Term.Canonical cle; Term.Var(idProvider "X2") ])))
                   LogicGEC.Constraint(Literal.Pos (Pred("codes", [ Term.Var(idProvider "X3"); Term.Const "gfp" ])))
                   LogicGEC.Constraint(Literal.Pos (Pred("bind",  [ Term.Var(idProvider "Receiver"); Term.Var(idProvider "Signal")])))

      ]
    }
  
  Assert.Equal(expected, actual)



