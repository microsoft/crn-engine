// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module modelGenerationTests


open Xunit
open FsUnit
open Microsoft.Research.CRNEngine


open RulesDSD.Syntax
open Microsoft.Research.GEC.LogicGEC






let lacI = Term.Const "lacI"
let tetR = Term.Const "tetR"
let cI   = Term.Const "cI"
let r0011 : Part = 
  { name  = Const "r0011"
    type_ = Const "prom" }
let r0011a = Pred ("neg", [r0011.name; lacI; Float 1.0; Float 0.5; Float 0.00005]) 
let r0011b = Pred ("con", [r0011.name; Float 0.1])
 
let r0040 : Part = 
  { name  = Term.Const "r0040"
    type_ = Term.Const "prom" }
let r0040a = Pred ("neg", [r0040.name; tetR; Float 1.0; Float 0.5; Float 0.00005])
let r0040b = Pred ("con", [r0040.name; Float 0.09])

let r0051 : Part = 
  { name  = Const "r0051"
    type_ = Const "prom" }
let r0051a = Pred ("neg", [r0051.name; cI;  Float 1.0; Float 0.5; Float 0.00005])
let r0051b = Pred ("con", [r0051.name; Float 0.12])

let b0034 : Part = 
  { name  = Const "b0034"
    type_ = Const "rbs" }
let b0034a = Pred ("rbs", [b0034.name; Float 0.1])
  
let c0012 : Part = 
  { name  = Const "c0012"
    type_ = Const "cds" }
let c0012a = Pred ("codes", [c0012.name; lacI; Float 0.01])

let c0040 : Part = 
  { name  = Const "c0040"
    type_ = Const "cds" }
let c0040a = Pred ("codes", [c0040.name; tetR; Float 0.01])
  
let c0051 : Part = 
  { name  = Const "c0051"
    type_ = Const "cds" }
let c0051a = Pred ("codes", [c0051.name; cI; Float 0.01])

let b0015 : Part = 
  { name  = Const "b0015"
    type_ = Const "ter" }

let toTerm (e:Element) = Term.Pat (Pattern.Inner [e, Location.wildcard])
let dummyDB = [ Pred("part", [toTerm (Part r0011)]) |> Clause.Create  
                Pred("part", [toTerm (Part b0034)]) |> Clause.Create  
                Pred("part", [toTerm (Part c0040)]) |> Clause.Create  
                Pred("part", [toTerm (Part b0015)]) |> Clause.Create  
                Pred("part", [toTerm (Part r0040)]) |> Clause.Create  
                Pred("part", [toTerm (Part c0051)]) |> Clause.Create  
                Pred("part", [toTerm (Part r0051)]) |> Clause.Create  
                Pred("part", [toTerm (Part c0012)]) |> Clause.Create  
                r0011a |> Clause.Create  
                r0011b |> Clause.Create  
                r0040a |> Clause.Create  
                r0040b |> Clause.Create  
                r0051a |> Clause.Create  
                r0051b |> Clause.Create  
                b0034a |> Clause.Create  
                c0012a |> Clause.Create  
                c0040a |> Clause.Create  
                c0051a |> Clause.Create  ]

let getMassActionRegulation () = 
  let varCount = ref 0
  let tvar s   = let i = !varCount
                 varCount := i + 1
                 Term.Var(i, s)
  
  let s        = tvar "S"
  let crn      = tvar "CRN"
  let prom     = tvar "Prom"
  let rbs      = tvar "Rbs"
  let pcr      = tvar "Pcr"
  let ter      = tvar "Ter"
  let c        = tvar "C"
  let tf       = tvar "TF"
  let kb       = tvar "Kb"
  let ku       = tvar "Ku"
  let kt       = tvar "Kt"
  let g        = tvar "G"
  let m        = tvar "M"
  let kl       = tvar "Kl"
  let p        = tvar "P"
  let kd       = tvar "Kd"

  let promT = Term.Const "prom"
  let rbsT  = Term.Const "rbs"
  let cdsT  = Term.Const "cds"
  let terT  = Term.Const "ter"

  let negHead    = Predicate.Pred ("reactions", [s; crn])
  let promQuery  = Part.Create(prom, promT) |> Element.Part 
  let rbsQuery   = Part.Create(rbs,  rbsT)  |> Element.Part
  let pcrQuery   = Part.Create(pcr,  cdsT)  |> Element.Part
  let terQuery   = Part.Create(ter,  terT)  |> Element.Part 
  let pattern    = Term.Pat (Pattern.Inner [ promQuery, Location.wildcard
                                           ; rbsQuery,  Location.wildcard
                                           ; pcrQuery,  Location.wildcard
                                           ; terQuery,  Location.wildcard ])
  let holes     = TList [pattern]
  let promE     = Element.Part { name = prom; type_ = promT }
  let rbsE      = Element.Part { name = rbs;  type_ = rbsT }
  let pcrE      = Element.Part { name = pcr;  type_ = cdsT }
  let terE      = Element.Part { name = ter;  type_ = terT }

  let gene      = Term.Func("gene", [Process.OfList [[promE; rbsE; pcrE; terE]] |> Term.Proc])
  let rna       = Term.Func("mRNA", [Process.OfList [[rbsE; pcrE]] |> Term.Proc])
  
  // TF + G <->{Kb}{Ku} TF:G
  let r1        = Term.Func("_rxn", [ Term.TMSet []
                                    ; Term.TMSet [1, tf; 1, g]
                                    ; Term.Func("_massActionRate", [Term.Func("_key", [kb])] )
                                    ; Term.Func("_massActionRate", [Term.Func("_key", [ku])] )
                                    ; Term.TMSet [1, Term.TMSet [1, tf; 1, g]]                  ])
  // G:TF ->{Kt} G:TF + M
  let r2        = Term.Func("_rxn", [ Term.TMSet []
                                    ; Term.TMSet [1, Term.TMSet [1, g; 1, tf]]
                                    ; Term.Func("_massActionRate", [Term.Func("_key", [kt])] )
                                    ; Term.Func("", [] )
                                    ; Term.TMSet [1, Term.TMSet [1, g; 1, tf]; 1, m]            ])
  let crnT      = Term.TCRN [r1; r2]
  let negBody   = [ Predicate.Pred ("is",     [s; c; holes])          |> Pos 
                  ; Predicate.Pred ("neg",    [prom; tf; kb; ku; kt]) |> Pos
                  ; Predicate.Pred ("=",      [g; gene])              |> Pos 
                  ; Predicate.Pred ("=",      [m; rna])               |> Pos 
                  ; Predicate.Pred ("=",      [crn; crnT])            |> Pos ]
  let negRule   = Clause.Create(negHead, negBody)

  (*
  constitutive(S,CRN) :-
    S = C[ Prom::prom Rbs::rbs Pcr::pcr Ter::ter ], 
    con(Prom, Kt),
    G = gene(<Prom Rbs Pcr Ter>), 
    M = mrna(<Rbs Pcr>),
    CRN = { | G ->{Kt} G + M }.
  *)
  
  let r2'        = Term.Func("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, g]
                                     ; Term.Func("_massActionRate", [Term.Func("_key", [kt])] )
                                     ; Term.Func("", [] )
                                     ; Term.TMSet [1, g; 1, m]            ])
  let conCrn  = Term.TCRN [r2']
  let promQuery  = Part.Create(prom, Term.Const "prom") |> Element.Part 
  let rbsQuery   = Part.Create(rbs,  Term.Const "rbs")  |> Element.Part 
  let pcrQuery   = Part.Create(pcr,  Term.Const "cds")  |> Element.Part 
  let terQuery   = Part.Create(ter,  Term.Const "ter")  |> Element.Part 
  let pattern    = Term.Pat (Pattern.Inner [ promQuery, Location.wildcard
                                          ; rbsQuery,  Location.wildcard
                                          ; pcrQuery,  Location.wildcard
                                          ; terQuery,  Location.wildcard ])
  let holes     = TList [pattern]
  let conBody = [ Predicate.Pred ("is", [s; c; holes]) |> Pos 
                ; Predicate.Pred ("con", [prom; kt])   |> Pos
                ; Predicate.Pred ("=", [g; gene])      |> Pos 
                ; Predicate.Pred ("=", [m; rna])       |> Pos 
                ; Predicate.Pred ("=", [crn; conCrn])  |> Pos ]
  let conHead = negHead
  let conRule = Clause.Create(conHead, conBody)
  
  (* translation rule:
      reactions(M, CRN) :-  
        M   = mrna(S),
        S   = C[Rbs:: Pcr::pcr ],
        rbs(Rbs, Kl),
        codes(Pcr, P, Kd),
        CRN = { | M ->{Kl} M + P
                | P ->{Kd}       }.
      *)
  let rbsQuery   = Part.Create(rbs,  Term.Const "rbs") |> Element.Part 
  let pcrQuery   = Part.Create(pcr,  Term.Const "cds") |> Element.Part 
  let pattern    = Term.Pat (Pattern.Inner [ rbsQuery,  Location.wildcard
                                           ; pcrQuery,  Location.wildcard ])
  let holes      = TList [pattern]
  let tr1       = Term.Func("_rxn", [ Term.TMSet []
                                    ; Term.TMSet [1, m]
                                    ; Term.Func("_massActionRate", [Term.Func("_key", [kl])] )
                                    ; Term.Func("", [] )
                                    ; Term.TMSet [1, m; 1, p]                                    ])
  let tr2       = Term.Func("_rxn", [ Term.TMSet []
                                    ; Term.TMSet [1, p]
                                    ; Term.Func("_massActionRate", [Term.Func("_key", [kd])] )
                                    ; Term.Func("", [] )
                                    ; Term.TMSet []                                              ])
  let translCrn  = Term.TCRN [tr1; tr2]
  
  let translHead = Predicate.Pred ("reactions", [m; crn])
  let translBody = [ Predicate.Pred ("=", [m; Term.Func("mRNA", [s])])          |> Pos 
                   ; Predicate.Pred ("is", [s; c; holes])           |> Pos 
                   ; Predicate.Pred ("rbs",    [rbs; kl])                       |> Pos
                   ; Predicate.Pred ("codes",  [pcr; p; kd])                    |> Pos
                   ; Predicate.Pred ("=", [crn; translCrn])                     |> Pos ]
                                           
  let translRule = Clause.Create(translHead, translBody)

  (* all mRNA degrades with rate 0.001:
      reactions(mrna(M), {mrna(M) ->{0.001}} ) *)
  let degCrn  = Term.TCRN [ Term.Func("_rxn", [ Term.TMSet []
                                              ; Term.TMSet [1, m]
                                              ; Term.Func("_massActionRate", [Term.Float 0.001] )
                                              ; Term.Func("", [] )
                                              ; Term.TMSet []       ]) ]
  let degHead = Predicate.Pred ("reactions", [m; crn])
  let degBody = [ Predicate.Pred ("=", [m; Term.Func("mRNA", [s])])          |> Pos 
                ; Predicate.Pred ("=", [crn; degCrn])                     |> Pos ]
  let degRule = Clause.Create(degHead, degBody)
                
  [ negRule; degRule; conRule; translRule ]

[<Fact(DisplayName="Logic GEC - model generation - repressilator")>]
let testModelGenRepressilator() = 
  // build a model for the repressilator device:
  //   r0011; b0034; c0040; b0015; r0040; b0034; c0051; b0015; r0051; b0034; c0012; b0015
  // which is one of the solutions found for the repressilator during device enumeration

  // simple database with eight parts, plus negative regulation, constitute, mRNA transcription and mRNA degradation rules
  let db2 = dummyDB @ getMassActionRegulation () |> toProgram
  let repressilator = [r0011; b0034; c0040; b0015; r0040; b0034; c0051; b0015; r0051; b0034; c0012; b0015] 
                      |> List.map Part 
  let prog = [ Instruction.Device repressilator ]
  let generatedCrn = generateCRN cle db2 prog 
  Assert.Equal(18, generatedCrn.reactions.Length)

[<Fact(DisplayName="Logic GEC - model generation - repressilator with initials")>]
let testModelGenRepressilatorInits() = 
  let db2 = dummyDB @ getMassActionRegulation () |> toProgram
  let repressilator : Complex = 
    [r0011; b0034; c0040; b0015; r0040; b0034; c0051; b0015; r0051; b0034; c0012; b0015] 
    |> List.map Part 
    |> fun x -> [0, x]
    |> Map.ofList
    |> Process.Proc
    |> Term.Proc
    |> fun x -> [1, x]
  let prog = [ Instruction.Initial (Term.Float 10.0, repressilator) ]
  let generatedCrn = generateCRN cle db2 prog 
  let debug = generatedCrn.to_string ()
  Assert.Equal(18, generatedCrn.reactions.Length)