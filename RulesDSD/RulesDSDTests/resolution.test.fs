// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module RulesDSD.ResolutionTests

open FsUnit
open Xunit
open FsUnit.Xunit
open System.Text.RegularExpressions

open RulesDSD.Syntax
open RulesDSD.Substitution
open RulesDSD.Unification
open RulesDSD.Resolution
open Microsoft.Research.DNA.LogicDSD
open Parser

let cle = Microsoft.Research.DNA.LogicDSD.engine

let removeVarNumbers x = Regex.Replace(x, "\([0-9]*\)", "")
let print    = List.map (Clause.ToStringWith cle) >> String.concat "\n" >> removeVarNumbers
let printClause = Clause.ToStringWith cle >> removeVarNumbers

// wildcards
let wc  = Term.Var (-1, "_")
let dwc = DomainT.Var ((-1, "_"), AnyTag)
let lwc = Location.Var (-1, "_")
let swc = SiteT.Var (-1, "_")
let bwc = Bond.Var (-1, "_")

let namer () =
  let m = ref Map.empty
  let counter = ref 0
  fun key -> if (!m).ContainsKey key
              then (!m).[key]
              else 
                let n = !counter
                counter := n + 1
                let v = (n, key)
                m := (!m).Add(key, v)
                v


let junctionProgram =
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let X   = DomainT.Var (nmr2 "X")
  let i   = Bond.Var    (nmr  "i")
  let X'  = DomainT.Var (nmr2 "X'")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  let Xi  = Site (Bound (X, i)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")

  let Gl = Site (Bound (DomainT.Var (nmr2 "G"), Bond.Var (nmr "l"))), lwc
  [
    //junction(E!j,F!j,_).
    Clause.Create( Pred("junction", [Pattern.Inner [Site (Bound (e, j)), lwc] |> Pat
                                     Pattern.Inner [Site (Bound (f, j)), lwc] |> Pat
                                     wc]))
     //junction(E!j,F!k,Q):- 
     //  Q = C [G!l E'!j] [E!j]
     //  junction(G!l,F!k,Q).
    Clause.Create( Pred("junction", [ Pattern.Inner [Ej] |> Pat
                                      Pattern.Inner [Fk] |> Pat
                                      Q]),
        [ Pos <| Pred ("is", [Q; C; TList [
                                                  Pattern.Inner [ Fk ] |> Pat
                                                  Pattern.Inner [ Gl; E'j ] |> Pat
                                                  Pattern.Inner [ Ej ] |> Pat
                                                 ]])
          Pos <| Pred ("junction", [ Pattern.Inner [Gl] |> Pat
                                     Pattern.Inner [Fk] |> Pat
                                     Q])
        ])
    
    Clause.Create( Pred("junction", [ Pattern.Inner 
                                         [SiteT.Site (Site.Bound (dwc, i)), lwc]
                                         |> Pat
                                      P ]),
        let p1 = Pattern.Inner [ Xi; Ej ]
        let p2 = Pattern.Inner [ Fk; X'i ]
        [ Pos ( Pred ("is",     [P; C; TList [Pat p1; Pat p2] ])) 
          Pos ( Pred ("junction", [Pat <| Pattern.Inner [ Ej ]
                                   Pat <| Pattern.Inner [ Fk ]
                                   P]))
        ]
        ) 
        ]


let (strandDisplacementProgram, strandDisplacementSystem, strandDisplacementExpectedSystem) =
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let X   = DomainT.Var (nmr2 "X")
  let i   = Bond.Var    (nmr  "i")
  let X'  = DomainT.Var (nmr2 "X'")
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")


  let a = Domain.Create("a", false, false)    |> DomainT.Dom
  let b = Domain.Create("b", false, false)    |> DomainT.Dom
  let c = Domain.Create("c", false, false)    |> DomainT.Dom
  let aStar = Domain.Create("a", true, false) |> DomainT.Dom
  let bStar = Domain.Create("b", true, false) |> DomainT.Dom
  let cStar = Domain.Create("c", true, false) |> DomainT.Dom
  
  let bond1 = Bond.Bond 1
  let bond2 = Bond.Bond 2
  let bond3 = Bond.Bond 3

  let invader  = [  Site (Bound (a, bond1)) 
                    Site (Unbound b)
                    Site (Unbound c) ]

  let defender = [  Site (Unbound a) 
                    Site (Bound (b, bond2))
                    Site (Bound (c, bond3))  ]

  let backbone = [  Site (Bound (cStar, bond3)) 
                    Site (Bound (bStar, bond2))
                    Site (Bound (aStar, bond1)) ]

  let strands = [ invader
                  defender
                  backbone ] 
                |> Process.OfList
  
  // encoding of C [E!j D] [D!i] [D'!i E'!j]
  let Ej  = Site (Bound (e, j)), lwc
  let D   = Site (Unbound d), lwc
  let Di  = Site (Bound (d, i)), lwc
  let D'i = Site (Bound (d', i)), lwc
  let E'j = Site (Bound (e', j)), lwc
  
  let p1  = Pattern.Inner [Ej; D]
  let p2  = Pattern.Inner [Di]
  let p3  = Pattern.Inner [D'i; E'j]
  
  // encoding of C [E!j D!i] [D] [D'!i E'!j]
  let p1' = Pattern.Inner [Ej; Di]
  let p2' = Pattern.Inner [D]
  let p3' = Pattern.Inner [D'i; E'j]

  let context1   = Pred ("is", [P; C; TList [Pat p1; Pat p2; Pat p3]])
  let context2 Q = Pred ("is", [Q; C; TList [Pat p1'; Pat p2'; Pat p3']])

  let program =
    junctionProgram @
    [Clause.Create( Pred("strandDisplacementRule", [P; Q]), 
        [ 
          Pos context1
          Pos (context2 Q)
          Pos <| Pred ("junction", [Pat p2; Q])
          ])
    ] |> toProgram
  
  // expected: "<a!1 b c> | <a b!2 c!3> | <c*!3 b*!2 a*!1>"
  let invader'  = [  Site (Bound (a, bond1)) 
                     Site (Bound (b, bond2))
                     Site (Unbound c) ]

  let defender' = [  Site (Unbound a) 
                     Site (Unbound b)
                     Site (Bound (c, bond3))  ]

  let expectedSystem = [invader'; defender'; backbone]
                       |> Process.OfList
                       |> Term.Proc
  (program, Term.Proc strands, expectedSystem)

[<Fact(DisplayName="Logic DSD - SG semantics - strand displacement rule")>]
(* 
   a   b  c              a  b   c
   __X_____              _____X__
   ________     ---->    ________
   a* b* c*              a* b* c*       
   
   C [E!j D] [D!i] [D'!i E'!j] ->{fast} C [E!j D!i] [D] [D'!i E'!j]

   *)
let strandDisplacement() =  
  let nmr = namer ()
  let query = Pos (Pred("strandDisplacementRule", [strandDisplacementSystem; Term.Var (nmr "Q")]))
  
  let theta = match resolve query strandDisplacementProgram cle with
              | Some s -> s
              | None   -> failwith "Strand displacement rule failed."

  Assert.Equal(strandDisplacementExpectedSystem, theta.Apply (Term.Var (nmr "Q"), cle))



[<Fact(DisplayName="Logic DSD - Resolution - anchored predicate (pass)")>]
(* The following strand displacement must be executed:
   a   b  c              a  b   c
   __X_____              _____X__
   ________     ---->    ________
   a* b* c*              a* b* c*       

   or: [a]<b c>:<a>[b c] --> [a b]<c>:<a b>[c]
   
   The  starting system is: <a!1 b c> | <a b!2 c!3> | <c*!3 b*!2 a*!1>   *)
let strandDisplacementAnchored() =
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let g   = DomainT.Var (nmr2 "G")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let i   = Bond.Var    (nmr  "i")
  let l   = Bond.Var    (nmr  "l")
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  
  let a     = Domain.Create("a", false, false) |> DomainT.Dom
  let b     = Domain.Create("b", false, false) |> DomainT.Dom
  let c     = Domain.Create("c", false, false) |> DomainT.Dom
  let aStar = Domain.Create("a", true , false) |> DomainT.Dom
  let bStar = Domain.Create("b", true , false) |> DomainT.Dom
  let cStar = Domain.Create("c", true , false) |> DomainT.Dom
  
  let bond1 = Bond.Bond 1
  let bond2 = Bond.Bond 2
  let bond3 = Bond.Bond 3

(* Strand displacement rule:
   C [E!j D] [D!i] [D'!i E'!j] ->{fast} C [E!j D!i] [D] [D'!i E'!j]
   *)

  let invader  = [  Site (Bound (a, bond1)) 
                    Site (Unbound b)
                    Site (Unbound c) ]

  let defender = [  Site (Unbound a) 
                    Site (Bound (b, bond2))
                    Site (Bound (c, bond3))  ]

  let backbone = [  Site (Bound (cStar, bond3)) 
                    Site (Bound (bStar, bond2))
                    Site (Bound (aStar, bond1)) ]

  let strands = [ invader
                  defender
                  backbone ] 
                |> Process.OfList
  
  // encoding of C [E!j D] [D!i] [D'!i F!k] 
  let Ej  = Site (Bound (e, j)), lwc
  let D   = Site (Unbound d), lwc
  let Di  = Site (Bound (d, i)), lwc
  let D'i = Site (Bound (d', i)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  
  let p1  = Pattern.Inner [Ej; D]
  let p2  = Pattern.Inner [Di]
  let p3  = Pattern.Inner [D'i; Fk]
  
  // encoding of C [E!j D!i] [D] [D'!i F!k]
  let p1' = Pattern.Inner [Ej; Di]
  let p2' = Pattern.Inner [D]
  let p3' = Pattern.Inner [D'i; Fk]

  let P = Term.Proc strands
  let context1   = Pred ("is", [P; C; TList [Pat p1; Pat p2; Pat p3]])
  let context2 Q = Pred ("is", [Q; C; TList [Pat p1'; Pat p2'; Pat p3']])

  let Gl = Site (Bound (g, l)), lwc

  let program =
    //C [E!j D] [D!i] [D'!i F!k] ->{1.0} C [E!j D!i] [D] [D'!i F!k]
    [Clause.Create( Pred("strandDisplacementRule", [Q]), 
        [ 
          Pos context1
          //if Q = C [E!j D] [D!i] [D'!i F!k]
          Pos (context2 Q)
          //   junction(E!j,F!k,Q)
          Pos (Pred ("junction", [ Pattern.Inner [Ej] |> Pat
                                   Pattern.Inner [Fk] |> Pat
                                   Q]))
          ])
     //junction(E!j,F!j,Q).
     Clause.Create( Pred("junction", [Pattern.Inner [Ej] |> Pat
                                      Pattern.Inner [Site (Bound (f, j)), lwc] |> Pat
                                      wc]))
     //junction(E!j,F!k,Q):- 
     //  Q = C [G!l E'!j] [E!j]
     //  junction(G!l,F!k,Q).
     Clause.Create( Pred("junction", [ Pattern.Inner [Site (Bound (e, j)), lwc] |> Pat
                                       Pattern.Inner [Site (Bound (f, k)), lwc] |> Pat
                                       Q]),
        [ Pos (Pred ("is", [Q; wc; TList [
                                                Pattern.Inner [ Gl; E'j ] |> Pat
                                                Pattern.Inner [ Ej ] |> Pat
                                               ]]))
          Pos (Pred ("junction", [ Pattern.Inner [Gl] |> Pat
                                   Pattern.Inner [Fk] |> Pat
                                   Q]))
        ]
       )
    ] |> toProgram

  let query = Pos (Pred("strandDisplacementRule", [Q]))
  
  let theta = match resolve query program cle with
              | Some s -> s
              | None   -> failwith "Strand displacement rule failed."

  // expected: "<a!1 b c> | <a b!2 c!3> | <c*!3 b*!2 a*!1>"
  let invader'  = [  Site (Bound (a, bond1)) 
                     Site (Bound (b, bond2))
                     Site (Unbound c) ]

  let defender' = [  Site (Unbound a) 
                     Site (Unbound b)
                     Site (Bound (c, bond3))  ]

  let expectedSystem = [invader'; defender'; backbone]
                       |> Process.OfList
                       |> Proc
  let actualSystem   = theta.Apply(Q, cle)
  Assert.Equal(expectedSystem, actualSystem)


let anchoredPredicateFailProgram = 
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let g   = DomainT.Var (nmr2 "G")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let X   = DomainT.Var (nmr2 "X")
  let i   = Bond.Var    (nmr  "i")
  let l   = Bond.Var    (nmr  "l")
  let X'  = DomainT.Var (nmr2 "X'")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  let Xi  = Site (Bound (X, i)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")

  let a     = Domain.Create("a", false, false) |> DomainT.Dom
  let b     = Domain.Create("b", false, false) |> DomainT.Dom
  let c     = Domain.Create("c", false, false) |> DomainT.Dom
  let aStar = Domain.Create("a", true,  false) |> DomainT.Dom
  let bStar = Domain.Create("b", true,  false) |> DomainT.Dom
  let cStar = Domain.Create("c", true,  false) |> DomainT.Dom
  
  let bond1 = Bond.Bond 1
  let bond2 = Bond.Bond 2
  let bond3 = Bond.Bond 3
  let bond4 = Bond.Bond 4
  
  let invaderStar = [  Site (Bound (aStar, bond1)) ]
  let invader     = [  Site (Bound (a,     bond1)) 
                       Site (Unbound b) ]

  let defender1 = [  Site (Bound (b, bond2)) ]
  let defender2 = [  Site (Bound (a, bond3)) ]
  let defender3 = [  Site (Bound (c, bond4)) ]

  let backbone = [  Site (Bound (cStar, bond4))
                    Site (Bound (bStar, bond2))
                    Site (Bound (aStar, bond3))]

  let strands = [ invaderStar
                  invader
                  defender1
                  defender2
                  defender3
                  backbone ] 
                |> List.mapi (fun i x -> i, x)
                |> Map.ofList
                |> Process.Proc
  let p = Term.Proc strands
  
  // encoding of C [E!j D] [D!i] [D'!i F!k] 
  let Ej  = Site (Bound (e, j)), lwc
  let D   = Site (Unbound d), lwc
  let Di  = Site (Bound (d, i)), lwc
  let D'i = Site (Bound (d', i)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  
  let p1  = Pattern.Inner [Ej; D]
  let p2  = Pattern.Inner [Di]
  let p3  = Pattern.Inner [D'i; Fk]
  
  // encoding of C [E!j D!i] [D] [D'!i F!k]
  let p1' = Pattern.Inner [Ej; Di]
  let p2' = Pattern.Inner [D]
  let p3' = Pattern.Inner [D'i; Fk]

  let context1   = Pred ("is", [P; C; TList [Pat p1; Pat p2; Pat p3]])
  let context2 Q = Pred ("is", [Q; C; TList [Pat p1'; Pat p2'; Pat p3']])

  let Gl = Site (Bound (g, l)), lwc

  //C [E!j D] [D!i] [D'!i F!k] ->{1.0} C [E!j D!i] [D] [D'!i F!k]
  [ Clause.Create( Pred("strandDisplacementRule", [P; Q]), 
        [ 
          Pos context1
          //if Q = C [E!j D] [D!i] [D'!i F!k]
          Pos (context2 Q)
          //   junction(E!j,F!k,Q)
          Pos (Pred ("junction", [ Pattern.Inner [Ej] |> Pat
                                   Pattern.Inner [Fk] |> Pat
                                   Q ] ))
          ])
    //junction(E!j,F!j,Q).
    Clause.Create( Pred("junction", [ Pattern.Inner [Site (Bound (e, j)), lwc] |> Pat
                                      Pattern.Inner [Site (Bound (f, j)), lwc] |> Pat
                                      wc ] ))
    //junction(E!j,F!k,Q):- 
    //  Q = C [G!l E'!j] [E!j]
    //  junction(G!l,F!k,Q).
    Clause.Create( Pred("junction", [ Pattern.Inner [Ej] |> Pat
                                      Pattern.Inner [Fk] |> Pat
                                      Q]),
        [ Pos (Pred ("is", [Q; C; TList [ Pattern.Inner [ Gl; E'j ] |> Pat
                                          Pattern.Inner [ Ej ]      |> Pat ]]))
          Pos (Pred ("junction", [ Pattern.Inner [Gl] |> Pat
                                   Pattern.Inner [Fk] |> Pat
                                   Q ] ))        ]
      ) 
    ] |> toProgram


[<Fact(DisplayName="Logic DSD - Resolution - anchored predicate (fail)")>]
(* The following strand displacement is invalid:
    a* a b    b   c             b  a* a b  c
      \\___  ___ ___           ___   \\___ ___
             _______   --/-->         _______
           //b*  c*                 // b*  c* 
         a a*                      a a*
   because the strands involved are not anchored. 
   This transition must not be executed. 
   
   The starting system is: <a*!1> | <a!1 b> | <b!2> | <a!3> | <c!4> | <c*!4 b*!2 a*!3>
   
   *)
let strandDisplacementAnchoredFail() =
  let a     = Domain.Create("a", false, false) |> DomainT.Dom
  let b     = Domain.Create("b", false, false) |> DomainT.Dom
  let c     = Domain.Create("c", false, false) |> DomainT.Dom
  let aStar = Domain.Create("a", true,  false) |> DomainT.Dom
  let bStar = Domain.Create("b", true,  false) |> DomainT.Dom
  let cStar = Domain.Create("c", true,  false) |> DomainT.Dom
  
  let bond1 = Bond.Bond 1
  let bond2 = Bond.Bond 2
  let bond3 = Bond.Bond 3
  let bond4 = Bond.Bond 4
  
  let invaderStar = [  Site (Bound (aStar, bond1)) ]
  let invader     = [  Site (Bound (a,     bond1)) 
                       Site (Unbound b) ]

  let defender1 = [  Site (Bound (b, bond2)) ]
  let defender2 = [  Site (Bound (a, bond3)) ]
  let defender3 = [  Site (Bound (c, bond4)) ]

  let backbone = [  Site (Bound (cStar, bond4))
                    Site (Bound (bStar, bond2))
                    Site (Bound (aStar, bond3))]

  let strands = [ invaderStar
                  invader
                  defender1
                  defender2
                  defender3
                  backbone ] 
                |> List.mapi (fun i x -> i, x)
                |> Map.ofList
                |> Process.Proc
  let P = Term.Proc strands
  let query = Pos (Pred("strandDisplacementRule", [P; Term.Var (0, "Q")]))
  let result = resolve query anchoredPredicateFailProgram cle
  Assert.Equal(None, result)

let hiddenProg =
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let g   = DomainT.Var (nmr2 "G")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let i   = Bond.Var    (nmr  "i")
  let l   = Bond.Var    (nmr  "l")
  let X'  = DomainT.Var (nmr2 "X'")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")

  let Di  = Site (Bound (d, i)), lwc
  let End = Location.Var (nmr "End")
  let EndT = Term.Var (nmr "End")
  let D'i = Site (Bound (d', i)), End
  
  let Start = Location.Var (nmr "Start")
  let StartT = Term.Var (nmr "Start")
  let A = SiteT.Var (nmr "A"), Start
  let B = swc, lwc

  let p1  = Pattern.Inner [Di]
  let p2  = Pattern.Inner [A; D'i]
  
  let Path  = Term.Var (nmr "Path")
  
  let uscore = wc

  let X       = Term.Var (nmr "X")
  let Y       = Term.Var (nmr "Y")
  let Visited = Term.Var (nmr "Visited")
  [ Clause.Create( Pred("hidden", [Pat p1; P]), [ 
      //C [D!i] [ ( A @ Start )  ( D'!i @ End ) _ ]
      Pos <| Pred ("is",    [P; C; TList [Pat p1; Pat p2]])
      // Pos <| Pred ("pos",    [Float 1.0; Float 0.0; C; Start]) 
      // Pos <| Pred ("pos",    [Float 1.0; Float 1.0; C; End]) 
       
      // find a loop that does not contain D!i
      Pos <| Pred ("path",   [  Pat <| Pattern.Inner [A]
                             ;  Pat <| Pattern.Inner [SiteT.Var(-1, "_"), Start]
                             ;  Pat <| Pattern.Inner [SiteT.Var(-1, "_"), End]; P; Const "left"; TList []; Path]) 
      Neg <| Pred ("member", [ Pat <| Pattern.Inner [Di]; Path ])
    ])
      
    (* path(A, B, P, Direction, Path) predicate: find a path "Path" from position "A" to position "B" in "P", following "Direction".
      "Direction" can be: 
        "left" to search towards the 5' end
        "right" to search towards the 3' end
        "any" to non-deterministically go either left or right.*)
    // base case: start = end
    Clause.Create( Pred("path", [ uscore
                                  EndT
                                  EndT
                                  uscore
                                  uscore
                                  Visited
                                  Path ]),
      [ Pos <| Pred ("reverse", [Visited; Path]) ]                            
      )
    // inductive cases
    // go to the left
      
    Clause.Create( Pred("path", [ X
                                  Pat <| Pattern.Inner [SiteT.Var (-1, "_"), Start]
                                  EndT
                                  P
                                  Const "left"
                                  Visited
                                  Path ]
                                  ),
        let Start'  = Location.Var (nmr "Start'")
        let StartT' = Term.Var (nmr "Start'")
        let Ys = SiteT.Var (nmr "Y")
        [ 
        Neg <| Pred ("=", [Pat <| Pattern.Inner [SiteT.Var (-1, "_"), Start]; EndT])
        Pos <| Pred ("is", [P; C; TList [Term.Pat (Pattern.Inner [ SiteT.Var (-1, "Y"), Start'
                                                                 ; SiteT.Var (-1, "_"), Start ])]]) 
        // Pos <| Pred ("prevDomain", [StartT; TList [Y; StartT']; P]) 
        Neg <| Pred ("member",     [TList [Y; Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Start'])] ; Visited ])
        Pos <| Pred ("path",       [Y; Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Start']); EndT; P; Const "left"
                                   ; TCons (TList [X; Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Start])], Visited); Path]) 
        ]
      )
    // go to the right
    Clause.Create( Pred("path", [ X
                                  Pat <| Pattern.Inner [SiteT.Var (-1, "_"), Start]
                                  EndT
                                  P
                                  Const "right"
                                  Visited
                                  Path]),
        let Start'  = Location.Var (nmr "Start'")
        let StartT' = Term.Var (nmr "Start'")
        let Ys = SiteT.Var (nmr "Y")
        [
        Neg <| Pred ("=", [Pat <| Pattern.Inner [SiteT.Var (-1, "_"), Start]; EndT])
        Pos <| Pred ("is", [P; C; TList [Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Start
                                                                 ; SiteT.Var (-1, "Y"), Start' ])]]) 
        // Pos <| Pred ("nextDomain", [StartT; TList [Y; StartT']; P ]) 
        Neg <| Pred ("member",     [TList [Y; Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Start'])] ; Visited ])
        Pos <| Pred ("path",       [Y; Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Start']); EndT; P; Const "right"; TCons (TList [X; Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Start])], Visited); Path]) 
        ]
      )
    // go either way
    Clause.Create( Pred("path", [ X
                                  StartT
                                  EndT
                                  P
                                  Const "any"
                                  Visited
                                  Path]),
        [ 
        Neg <| Pred ("=", [StartT; EndT])
        Pos <| Pred ("path", [X; StartT; EndT; P; Const "left"; Visited; Path]) 
        ])
    Clause.Create( Pred("path", [ X
                                  StartT
                                  EndT
                                  P
                                  Const "any"
                                  Visited
                                  Path]),
        [ 
        Neg <| Pred ("=", [StartT; EndT])
        Pos <| Pred ("path", [X; StartT; EndT; P; Const "right"; Visited; Path]) 
        ]
      )
    (// jump across a bond and then go either way
    //let i  = Bond.Var "i"
    //let D  = DomainT.Var "D"
    //let D' = DomainT.Var "D'"
    let Start'  = Location.Var (nmr "Start'")
    let StartT' = Term.Var (nmr "Start'")
    let p1 = Pat <| Pattern.Inner [SiteT.Site (Site.Bound (d, i)), lwc]
    let p2 = Pat <| Pattern.Inner [SiteT.Site (Site.Bound (d', i)), Start']
    Clause.Create( Pred("path", [ p1
                                  StartT
                                  Pat <| Pattern.Inner [SiteT.Var(-1, "_"), End]
                                  P
                                  uscore
                                  Visited
                                  Path]),
      [
        Neg <| Pred ("=",      [ StartT; Pat <| Pattern.Inner [SiteT.Var(-1, "_"), End] ])
        Pos <| Pred ("is",     [ P; C; TList [p1; p2]])
        // Pos <| Pred ("pos",    [ Float 1.0; Float 0.0; C; StartT']) 
        Neg <| Pred ("member", [ TList [Pat <| Pattern.Inner [SiteT.Site (Site.Bound (d', i)), Location.Var (-1, "_")]
                                       ; Pat <| Pattern.Inner [SiteT.Var (-1, "_"), Start'] ]; Visited ])
        Pos <| Pred ("path",   [ Pat <| Pattern.Inner [SiteT.Site (Site.Bound (d', i)), Location.Var (-1, "_")]
                               ; Pat <| Pattern.Inner [SiteT.Var(-1, "_"), Start']
                               ; Pat <| Pattern.Inner [SiteT.Var(-1, "_"), End]
                               ; P; Const "any"; TCons (TList [p1; StartT], Visited); Path]) 
      ]))
    ]


[<Fact(DisplayName="Logic DSD - Resolution - hidden predicate (pass)")>]
(* the bond over domain b in this configuration is hidden:
    [ab]{c>
   because it's occluded by the bond over domain a and the hairpin over c.   
   *)
let sgHiddenPass() =
  let mkDom  x = Domain.Create(x, false, false) |> DomainT.Dom
  let mkDomC x = Domain.Create(x, true,  false) |> DomainT.Dom
  let a = mkDom "a"
  let b = mkDom "b"
  let c = mkDom "c"
  let d = mkDom "d"
  let e = mkDom "e"
  let f = mkDom "f"
  let g = mkDom "g"

  let aStar = mkDomC "a"
  let bStar = mkDomC "b"
  let cStar = mkDomC "c"
  let dStar = mkDomC "d"
  let fStar = mkDomC "f"
  
  let bond1 = Bond.Bond 1
  let bond2 = Bond.Bond 2
  let bond3 = Bond.Bond 3
  let bond4 = Bond.Bond 4
  
  let mkBound d i = Site (Bound (d, i))
  let mkUnbound d = Site (Unbound d)

  let baseStrand = [  
    mkBound  dStar bond1 
   ] 

  let strand1  = [
    mkBound a bond2
    mkBound d bond1
    mkBound c bond4
    ]

  let strand2  = [
    mkBound f bond3
    mkUnbound e
    mkBound aStar bond2
    ]
  
  let strand3  = [
    mkBound fStar bond3
    mkUnbound g
    mkBound cStar bond4
    ]

  //let hairpin = [  
  //  Site (Bound (a, bond1)) 
  //  Site (Bound (b, bond2)) 
  //  Site (Unbound c) 
  //  Site (Bound (bStar, bond2)) 
  //  Site (Bound (aStar, bond1)) 
  //]

  let strands = [ baseStrand
                  // strands loop
                  strand1 
                  strand2
                  strand3  
                  ] 
                |> List.mapi (fun i x -> i, x)
                |> Map.ofList
                |> Process.Proc
  
  // encoding of C [D!i] [D'!i]
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let g   = DomainT.Var (nmr2 "G")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let X   = DomainT.Var (nmr2 "X")
  let i   = Bond.Var    (nmr  "i")
  let l   = Bond.Var    (nmr  "l")
  let X'  = DomainT.Var (nmr2 "X'")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  let Xi  = Site (Bound (X, i)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")

  let Di  = Site (Bound (d, i)), lwc
  let D'i = Site (Bound (d', i)), lwc
  let A = SiteT.Var (nmr "A"), lwc
  let B = swc, lwc

  let p1  = Pattern.Inner [Di]
  let p2  = Pattern.Inner [A; D'i; B]
  
  let Start = Term.Var (nmr "Start")
  let End   = Term.Var (nmr "End")
  let Path  = Term.Var (nmr "Path")
  let sys = Term.Proc strands
  
  
  let Gl = Site (Bound (g, l))
  let uscore = wc

  let X       = Term.Var (nmr "X")
  let Y       = Term.Var (nmr "Y")
  let Visited = Term.Var (nmr "Visited")
  
  let program = hiddenProg |> toProgram
  let query = Pos (Pred("hidden", [Pat <| Pattern.Inner [SiteT.Site (Site.Bound (dwc, bond1)), lwc]
                                   sys]))
  let result = resolve query program cle
  Assert.True(result.IsSome)


(* Hierarchy of transitions*)
let hierarchyProgram, sys, expectedSys = 
(* The following strand displacement is invalid:
    a* a b    b   c             b  a* a b  c
      \\___  ___ ___           ___   \\___ ___
             _______   --/-->         _______
           //b*  c*                 // b*  c* 
         a a*                      a a*
   because the strands involved are not anchored. 
   This transition must not be executed. 
   
   The starting system is: <a*!1> | <a!1 b> | <b!2> | <a!3> | <c!4> | <c*!4 b*!2 a*!3>
   
   *)
  let a     = Domain.Create("a", false, false) |> DomainT.Dom
  let b     = Domain.Create("b", false, false) |> DomainT.Dom
  let c     = Domain.Create("c", false, false) |> DomainT.Dom
  let aStar = Domain.Create("a", true, false)  |> DomainT.Dom
  let bStar = Domain.Create("b", true, false)  |> DomainT.Dom
  let cStar = Domain.Create("c", true, false)  |> DomainT.Dom
  
  let bond1 = Bond.Bond 1
  let bond2 = Bond.Bond 2
  let bond3 = Bond.Bond 3
  
  let invader  = [  Site (Bound (a, bond1)) 
                    Site (Unbound b)
                    Site (Unbound c) ]

  let defender = [  Site (Unbound a) 
                    Site (Bound (b, bond2))
                    Site (Bound (c, bond3))  ]

  let backbone = [  Site (Bound (cStar, bond3)) 
                    Site (Bound (bStar, bond2))
                    Site (Bound (aStar, bond1)) ]

  let strands = [ invader
                  defender
                  backbone ] 
                  |> Process.OfList
  
  // encoding of C [E!j D] [D!i] [D'!i F!k] 
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let g   = DomainT.Var (nmr2 "G")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let X   = DomainT.Var (nmr2 "X")
  let i   = Bond.Var    (nmr  "i")
  let l   = Bond.Var    (nmr  "l")
  let X'  = DomainT.Var (nmr2 "X'")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  let Xi  = Site (Bound (X, i)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")

  let Ej  = Site (Bound (e, j)), lwc
  let D   = Site (Unbound d), lwc
  let Di  = Site (Bound (d, i)), lwc
  let D'i = Site (Bound (d', i)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  
  let p1  = Pattern.Inner [Ej; D]
  let p2  = Pattern.Inner [Di]
  let p3  = Pattern.Inner [D'i; Fk]
  
  // encoding of C [E!j D!i] [D] [D'!i F!k]
  let p1' = Pattern.Inner [Ej; Di]
  let p2' = Pattern.Inner [D]
  let p3' = Pattern.Inner [D'i; Fk]

  let sys = Term.Proc strands
  let R = Term.Var (nmr "R")
  let V = Term.Var (nmr "V")
  let context1 P = Pred ("is", [P; C; TList [Pat p1; Pat p2; Pat p3]])
  let context2 Q = Pred ("is", [Q; C; TList [Pat p1'; Pat p2'; Pat p3']])

  let Gl = Site (Bound (g, l)), lwc

  let program =
    [ 
      // Displacement rule: C [E!j D] [D!i] [D'!i F!k] ->{1.0} C [E!j D!i] [D] [D'!i F!k]
      Clause.Create( Pred("rule", [Term.Const "fast"; P; Q]), 
         [ Pos (context1 P)
           //if Q = C [E!j D] [D!i] [D'!i F!k]
           Pos (context2 Q)
           //   junction(E!j,F!k,Q)
           Pos (Pred ("junction", [ Pattern.Inner [Ej] |> Pat
                                    Pattern.Inner [Fk] |> Pat
                                    Q ] 
                )) 
         ])

      //junction(E!j,F!j,Q).
      Clause.Create( Pred("junction", [ Pattern.Inner [Site (Bound (e, j)), lwc] |> Pat
                                        Pattern.Inner [Site (Bound (f, j)), lwc] |> Pat
                                        wc ] ))
      //junction(E!j,F!k,Q):- 
     //  Q = C [G!l E'!j] [E!j] [F!k]
     //  junction(G!l,F!k,Q).
      Clause.Create( Pred("junction", [ Pattern.Inner [Ej] |> Pat
                                        Pattern.Inner [Fk] |> Pat
                                        Q]),
         [ Pos (Pred ("is", [Q; C; TList 
                                    [ 
                                      Pattern.Inner [ Fk ]      |> Pat
                                      Pattern.Inner [ Ej ]      |> Pat 
                                      Pattern.Inner [ Gl; E'j ] |> Pat
                                      ]
                             ]))
           Pos (Pred ("junction", [ Pattern.Inner [Gl] |> Pat
                                    Pattern.Inner [Fk] |> Pat
                                    Q ] ))        ]
        ) 

      Clause.Create( Pred("rule", [Term.Const "loop"; P; Q; Q; V])) 
      Clause.Create( Pred("rule", [Term.Const "loop"; P; Q; R; V]),
         [ 
           Pos (Pred ("rule", [Term.Const "hierarchy"; Q; R; V]))
           // Neg (Pred ("member", [R; V]))
           //Pos (Pred ("rule", [Term.Const "hierarchy" ; R; Q; TCons (R, V) ]))
           // Pos (Pred ("=", [V'; TCons (R, V)]))
         ])
      Clause.Create( Pred("rule", [Term.Const "hierarchy"; P; R; V]), 
         [ 
           Pos (Pred ("rule", [Term.Const "fast"; P; Q ]))
           Neg (Pred ("member", [Q; V]))
           Pos (Pred ("rule", [Term.Const "loop" ; P; Q; R; TCons (Q, V) ]))
           // Pos (Pred ("=", [V'; TCons (R, V)]))
         ])
      
      Clause.Create( Pred("rule", [Term.Const "rxn"; P; Q]), 
         [ 
           Pos (Pred ("rule", [Term.Const "hierarchy"; P; Q; TList [P]]))
           Neg (Pred ("rule", [Term.Const "fast"; Q; wc]))
         ])
      
      ] |> toProgram
  let invader'  = [  Site (Bound (a, bond1)) 
                     Site (Bound (b, bond2))
                     Site (Bound (c, bond3)) ]

  let defender' = [  Site (Unbound a) 
                     Site (Unbound b)
                     Site (Unbound c)  ]

  let expectedSystem = [invader'; defender'; backbone] 
                        |> Process.OfList 
                        |> Proc
  (program, sys, expectedSystem)

[<Fact(DisplayName="Logic DSD - Resolution - hierarchy (pass)")>]
let hierarchyDisplacement() =
  let query = Pos (Pred("rule", [Term.Const "rxn"; sys; Term.Var (0, "Q")]))
  let thetas = match resolveAll query hierarchyProgram cle with
               | None -> failwith "No solution found"
               | Some thetas -> thetas

  // expected: "<a!1 b!2 c!3> | <a b c> | <c*!3 b*!2 a*!1>"
  let actualSystem = thetas |> List.map (fun theta -> theta.Apply (Term.Var (0, "Q"), cle))
  Assert.Contains(expectedSys, actualSystem)



(****************************)
(* Strand Graphs semantics  *)
(****************************)

let (bindingProgram, bindingSys) = 
  let mkDom  x = Domain.Create(x, false, false) |> DomainT.Dom
  let a = mkDom "a"
  let b = mkDom "b"
  let c = mkDom "c"
  
  let mkDomC x = Domain.Create(x, true,  false) |> DomainT.Dom
  let aStar = mkDomC "a"
  let bStar = mkDomC "b"
  let cStar = mkDomC "c"

  let mkUnbound d = Site (Unbound d)

  let strand1  = [
    mkUnbound a
    mkUnbound b
    mkUnbound c
    ]

  let strand2  = [
    mkUnbound aStar
    mkUnbound bStar
    mkUnbound cStar
    ]
  
  let strands = [ strand1 
                  strand2
                  ] 
                |> List.mapi (fun i x -> i, x)
                |> Map.ofList
                |> Process.Proc
  
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let i   = Bond.Var    (nmr  "i")
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")

  let D  = Site (Unbound d), lwc
  let D' = Site (Unbound d'), lwc
  let Di  = Site (Bound (d, i)), lwc
  let D'i = Site (Bound (d', i)), lwc
  let p1  = Pattern.Inner [D]
  let p2  = Pattern.Inner [D']
  let p1'  = Pattern.Inner [Di] 
  let p2'  = Pattern.Inner [D'i]
  
  let program = 
    hiddenProg 
    @ [
      Clause.Create(Pred("bindingRule", [P; Q]), 
        [
          Pos <| Pred ("is",      [P; C; TList [Pat p1; Pat p2]])
          Pos <| Pred ("freshBond", [Pat p1'; P])
          Pos <| Pred ("compl",     [Pat <| Pattern.Inner [D]; Pat <| Pattern.Inner [D']])
          Pos <| Pred ("is",      [Q; C; TList [Pat p1'; Pat p2']])
          Neg <| Pred ("hidden",    [Pat <| Pattern.Inner [Di]; Q])
        ])
      ]
    |> toProgram
  (program, Proc strands)

[<Fact(DisplayName="Logic DSD - SG semantics - binding rule")>]
let bindingRule() =
  let query = Pos (Pred("bindingRule", [bindingSys; Term.Var (0, "Q")]))
  match resolveAll query bindingProgram cle with
  | None -> failwith "No solution found"
  | Some sols -> sols |> List.map (fun sol -> sol.Apply (Term.Var (0, "Q"), cle))
                      |> Set.ofList
                      |> fun x -> Assert.Equal(x.Count, 3)


let unbindingProgram, unbindingSystem, unbindingSystem' =
  let mkDom  x = Domain.Create(x, false, false) |> DomainT.Dom
  let a     = mkDom "a"
  let b     = mkDom "b"
  let c     = mkDom "c"

  let mkDomC x = Domain.Create(x, true,  false) |> DomainT.Dom
  let aStar = mkDomC "a"
  let bStar = mkDomC "b"
  let cStar = mkDomC "c"
  
  let bond1 = Bond.Bond 1
  
  let strand1  = [  Site (Unbound cStar)
                    Site (Bound   (bStar, bond1)) 
                    Site (Unbound aStar) ]

  let strand2 = [  Site (Unbound a) 
                   Site (Bound (b, bond1))
                   Site (Unbound c)  ]

  let strands = [ strand1
                  strand2 ] 
                |> Process.OfList
  
  // encoding of C [X!i] [X'!i]
  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let g   = DomainT.Var (nmr2 "G")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let X   = DomainT.Var (nmr2 "X")
  let i   = Bond.Var    (nmr  "i")
  let l   = Bond.Var    (nmr  "l")
  let X'  = DomainT.Var (nmr2 "X'")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  let Xi  = Site (Bound (X, i)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")

  let Xi  = Site (Bound (X,  i)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let p1  = Pattern.Inner [Xi]
  let p2  = Pattern.Inner [X'i]
  
  // encoding of C [X] [X']
  let X   = Site (Unbound X), lwc
  let X'  = Site (Unbound X'), lwc
  let p1' = Pattern.Inner [X]
  let p2' = Pattern.Inner [X']

  // various variable definitions
  let Ej  = Site (Bound (e, j)), lwc
  let D   = Site (Unbound d)
  let Di  = Site (Bound (d,  i))
  let D'i = Site (Bound (d', i))
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f,  k)), lwc
  

  let sys = Term.Proc strands
  let R = Term.Var (nmr "R")
  let V = Term.Var (nmr "Visited")
  let context1 P = Pred ("is", [P; C; TList [Pat p1; Pat p2]])
  let context2 Q = Pred ("is", [Q; C; TList [Pat p1'; Pat p2']])

  let Gl = Site (Bound (g, l)), lwc

  let program =
    [ 
      // Displacement rule: C [X!i] [X'!i] ->{1.0} C [X] [X'] if not junction(i, P)
      Clause.Create( Pred("rule", [Term.Const "unbinding"; P; Q]), 
         [ Pos (context1 P) // P = C [X!i] [X'!i]
           Pos (context2 Q) // Q = C [X] [X']
           //   not junction(X!i, P)
           Neg (Pred ("junction", [ Pattern.Inner [Xi] |> Pat; P ] )) 
         ])
      
      Clause.Create( Pred("junction", [ Pattern.Inner 
                                         [SiteT.Site (Site.Bound (dwc, i)), lwc]
                                         |> Pat
                                        P ]),
        let p1 = Pattern.Inner [ Xi; SiteT.Site (Bound (e, j)), lwc]
        let p2 = Pattern.Inner [ SiteT.Site (Bound (f, k)), lwc; X'i]
        [ Pos ( Pred ("is", [P; C; TList [Pat p1; Pat p2] ])) 
          Pos ( Pred ("junction", [Pat p1; Pat p2; P]))
        ]
        )

      //junction(E!j,F!j,Q).
      Clause.Create( Pred("junction", [ Pattern.Inner [Site (Bound (e, j)), lwc] |> Pat
                                        Pattern.Inner [Site (Bound (f, j)), lwc] |> Pat
                                        wc ] ))
     //junction(E!j,F!k,Q):- 
     //  Q = C [G!l E'!j] [E!j] [F!k]
     //  junction(G!l,F!k,Q).
      Clause.Create( Pred("junction", [ Pattern.Inner [Ej] |> Pat
                                        Pattern.Inner [Fk] |> Pat
                                        Q]),
         [ Pos (Pred ("is", [Q; C; TList [ 
                                                  Pattern.Inner [ Fk ]      |> Pat
                                                  Pattern.Inner [ Ej ]      |> Pat 
                                                  Pattern.Inner [ Gl; E'j ] |> Pat
                                                ]
                             ]))
           Pos (Pred ("junction", [ Pattern.Inner [Gl] |> Pat
                                    Pattern.Inner [Fk] |> Pat
                                    Q ] ))        ]
        ) 
      ] |> toProgram

  // expected: "<a!1 b!2 c!3> | <a b c> | <c*!3 b*!2 a*!1>"
  let strand1' = [  Site (Unbound cStar) 
                    Site (Unbound bStar) 
                    Site (Unbound aStar)  ]

  let strand2' = [  Site (Unbound a) 
                    Site (Unbound b)
                    Site (Unbound c)  ]

  let expectedSystem = [strand1'; strand2']
                       |> Process.OfList
                       |> Proc
  (program, Proc strands, expectedSystem )

[<Fact(DisplayName="Logic DSD - SG semantics - unbinding rule")>]
(* The following unbinding must occur:
    <a>{a*}[b^]{c*}<c> ----> <a b c> | <c* b* a*>
   
   The starting system is: <c* b^*!1 a*> | <a b^!1 c>
   
   *)
let unbinding() =
  let query = Pos (Pred("rule", [Term.Const "unbinding"; unbindingSystem; Term.Var (0, "Q")]))
  let thetas = match resolveAll query unbindingProgram cle with
               | None -> failwith "No solution found"
               | Some thetas -> thetas

  let z = unbindingSystem'
  let actualSystem = thetas |> List.map (fun theta -> theta.Apply (Term.Var (0, "Q"), cle))
  Assert.Contains(unbindingSystem', actualSystem)


let branchMigrationProgram, branchMigrationSystem, branchMigrationExpectedSystem =
  let mkDoms x t = Domain.Create(x, false, t) |> DomainT.Dom
                   , Domain.Create(x, true, t) |> DomainT.Dom
  let L,  L'  = mkDoms "L" false
  let T2, T2' = mkDoms "T2" true
  let T1, T1' = mkDoms "T1" true
  let X,  X'  = mkDoms "X" false
  let A,  A'  = mkDoms "A" false
  let R,  R'  = mkDoms "R" false

  let mkBond i = Bond.Bond i
  let i1 = mkBond 1
  let i2 = mkBond 2
  let i3 = mkBond 3
  let j1 = mkBond 4
  let j2 = mkBond 5
  
  let mkU d   = Site (Unbound d)
  let mkB d i = Site (Bound (d, i))

  let s1 = [
    mkU L
    mkB T2  i2 
    mkB X'  i1
    mkB T1  i3
    ]

  let s2 = [
    mkB A   j2 
    mkB X   i1
    mkB T2' i2
    ]  
  
  let s3 = [
    mkB T1' i3 
    mkB X   j1
    mkU R
    ]  
  
  let s4 = [
    mkB X'  j1 
    mkB A'  j2
    ] 
  
  let s5 = [
    mkU A
    ]
  // these strands are from Fig. 1 from the Strands Graphs paper
  let strands = [ 
                  s1
                  s2 
                  s3 
                  s4 
                  s5
                ]
                |> Process.OfList

  let nmr = namer ()
  let nmr2 x = (nmr x, AnyTag) 
  let d   = DomainT.Var (nmr2 "D")
  let d'  = DomainT.Var (nmr2 "D'")
  let g   = DomainT.Var (nmr2 "G")
  let e   = DomainT.Var (nmr2 "E")
  let j   = Bond.Var    (nmr  "j")
  let e'  = DomainT.Var (nmr2 "E'")
  let f   = DomainT.Var (nmr2 "F")
  let k   = Bond.Var    (nmr  "k")
  let X   = DomainT.Var (nmr2 "X")
  let i   = Bond.Var    (nmr  "i")
  let l   = Bond.Var    (nmr  "l")
  //let X'  = DomainT.Var (nmr "X'")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let Fk  = Site (Bound (f, k)), lwc
  let Xi  = Site (Bound (X, i)), lwc
  let X'i = Site (Bound (X', i)), lwc
  let Q = Term.Var (nmr "Q")
  let C = Term.Var (nmr "C")
  let P = Term.Var (nmr "P")
  let Ej  = Site (Bound (e, j)), lwc
  let E'j = Site (Bound (e', j)), lwc
  let vnm v = Term.Var (nmr v)
  let domainsList = vnm "DomainsList"
  let domain      = vnm "Domain"

  let toKey t = Term.Func("_key", [t])

  let program = 
    junctionProgram 
    @ hiddenProg 
    @ [
      // create patterns of the form [D!i1][D'!i1][D!i2][D'!i2] ... [D!iN][D'!iN]  
      Clause.Create(Pred("makeNHoles", [Pat <| Pattern.Inner [Site (Unbound (d)), lwc]; 
                                        Pat <| Pattern.Inner [Site (Unbound (d')), lwc]; 
                                        TList [ Pat <| Pattern.Inner [Site (Bound (d,  i)), lwc]
                                                Pat <| Pattern.Inner [Site (Bound (d', i)), lwc] ]
                                        TList [ Pat <| Pattern.Inner [Site (Bound (d,  i)), lwc] ] 
                                        Float 1.0]))
      Clause.Create(Pred("makeNHoles", [Pat <| Pattern.Inner [Site (Unbound d), lwc] 
                                        Pat <| Pattern.Inner [Site (Unbound d'), lwc] 
                                        TCons (Pat <| Pattern.Inner [Site (Bound (d,  i)), lwc]
                                          , TCons (Pat <| Pattern.Inner [Site (Bound (d', i)), lwc]
                                            , vnm "RestOfPatterns")) 
                                        TCons (Pat <| Pattern.Inner [Site (Bound (d, i)), lwc]
                                              , vnm "FreshBonds")
                                        vnm "N"]), 
        [ 
          Pos <| Pred (">", [vnm "N"; Float 1.0])
          Pos <| Pred ("is",        [vnm "M"; Term.Func("_Minus", [vnm "N"|> toKey; Float 1.0])])
          Pos <| Pred("makeNHoles", [Pat <| Pattern.Inner [Site (Unbound d), lwc]; 
                                     Pat <| Pattern.Inner [Site (Unbound d'), lwc];
                                     vnm "RestOfPatterns"
                                     vnm "FreshBonds"
                                     vnm "M"])
        ])
      // turn [D!i1][D'!i1][D!i2][D'!i2] ... [D!iN][D'!iN] into [D!i1][D'!i2][D!i2][D'!i3] ... [D!iN][D'!i1]
      Clause.Create(Pred("migrationPattern", [
                                              TList [
                                                  Pat <| Pattern.Inner [Site (Bound (d,i)), lwc]
                                                  Pat <| Pattern.Inner [Site (Bound (d',j)), lwc]
                                                ]
                                              TList [
                                                Pat <| Pattern.Inner [Site (Bound (d,i)), lwc]
                                                Pat <| Pattern.Inner [Site (Bound (d',j)), lwc]
                                              ]
        ]))
      Clause.Create(Pred("migrationPattern", [ 
                                              // input pattern
                                              TCons (    Pat <| Pattern.Inner [Site (Bound (d,i)), lwc]  // 1
                                              , TCons (  Pat <| Pattern.Inner [Site (Bound (d',j)), lwc] // 2
                                              , TCons (  Pat <| Pattern.Inner [Site (Bound (d,k)), lwc]  // 3
                                              , TCons (  Pat <| Pattern.Inner [Site (Bound (d',k)), lwc] // 4
                                              , vnm "RestOfStartingPattern"))))
                                              // migration pattern
                                              TCons (   Pat <| Pattern.Inner [Site (Bound (d,  i)), lwc]   // 1
                                              , TCons ( Pat <| Pattern.Inner [Site (Bound (d',  k)), lwc]  // 4
                                              ,         vnm "RestOfMigrationPattern"))
                                              ]),
        [
          Pos <| Pred ("migrationPattern", 
                        [
                         // input pattern: continue starting from j and k
                         TCons (    Pat <| Pattern.Inner [Site (Bound (d,  k)), lwc]  // 3
                         , TCons (  Pat <| Pattern.Inner [Site (Bound (d', j)), lwc] // 2
                         , vnm "RestOfStartingPattern"))
                         // output pattern
                         vnm "RestOfMigrationPattern"
                        ])
          ])
      Clause.Create( Pred("findLoop", [ Pattern.Inner [Ej] |> Pat
                                        Q
                                        vnm "Loop"]), 
        [
          Pos <| Pred ("is", [Q; vnm "C"; TList [ 
                                                    Pattern.Inner [ fst Ej, Location.Var (nmr "Start")  ] |> Pat
                                                    Pattern.Inner [ fst E'j, Location.Var (nmr "End") ] |> Pat 
                                                  ] ])
          // Pos <| Pred ("pos",    [Int 0; Int 1; Var "C"; Var "Start"]) 
          // Pos <| Pred ("pos",    [Float 0.0; Float 0.0; vnm "C"; vnm "Start"]) 
          // Pos <| Pred ("pos",    [Float 1.0; Float 0.0; vnm "C"; vnm "End"]) 
          Pos <| Pred ("path",   [ Pat <| Pattern.Inner [ Ej ]
                                   Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Location.Var (nmr "Start")])
                                   Term.Pat (Pattern.Inner [ SiteT.Var (-1, "_"), Location.Var (nmr "End")])
                                   Q
                                   Const "any" // // search towards 5'
                                   TList []  // visited nodes
                                   TCons (wc, vnm "Loop")]) // remove Ej from the path
          Neg <| Pred ("=", [vnm "Loop"; TList []] )
        ])
      Clause.Create( Pred("branchesOnly", [ TList[] ]))
      Clause.Create( Pred("branchesOnly", [ TCons( TList [Pattern.Inner [ Ej  ] |> Pat; wc]
                                            , TCons ( TList [ Pattern.Inner [ E'j ] |> Pat; wc]
                                            , vnm "Rest"))]),
        [
          Pos <| Pred("branchesOnly", [ vnm "Rest" ])
        ])
      Clause.Create( Pred("junctionPath", [ Pattern.Inner [Ej] |> Pat
                                            Q ]),
        [
          // find a loop from E!j to E'!j
          Pos <| Pred ("findLoop", [Pattern.Inner [ Ej  ] |> Pat; Q; vnm "Loop"])
          Pos <| Pred ("branchesOnly", [vnm "Loop"])
          ])
      
      Clause.Create(Pred ("checkAllAnchored", [TList []; Q]))
      Clause.Create(Pred ("checkAllAnchored", [TCons ( Pat <| Pattern.Inner [Site (Bound (d, i)), lwc]
                                                     , vnm "Rest"); Q]),
       [
        Pos <| Pred ("junctionPath", [Pat <| Pattern.Inner [Site (Bound (d, i)), lwc]; Q])
        Pos <| Pred ("checkAllAnchored", [vnm "Rest"; Q])
        ])
      // perform N-way migration on D
      Clause.Create(Pred("branchMigration", [
                                             vnm "D"
                                             P
                                             Q
                                             vnm "N"
                                            ]), 
        [ 
          // compl(D, D'),
          Pos <| Pred ("unbound", [vnm "D"])
          Pos <| Pred ("compl", [vnm "D"
                                 vnm "D'"])
          Pos <| Pred ("makeNHoles", [
                                      vnm "D"
                                      vnm "D'"
                                      vnm "Junctions"
                                      vnm "FreshBonds"
                                      vnm "N"
                                     ])
          // ctx: check that the junction patterns occurs in P
          Pos <| Pred ("is",   [P; C; vnm "Junctions"])
          // make migrated junctions pattern
          Pos <| Pred ("migrationPattern", [vnm "Junctions"; vnm "MigratedJunctions"])
          // ctx: migrate junctions in P to Q
          Pos <| Pred ("is",   [Q; C; vnm "MigratedJunctions"])
          Pos <| Pred ("checkAllAnchored", [vnm "FreshBonds"; Q])
        ])
      
      Clause.Create(Pred("siteMatchesDomain",    [Pat <| Pattern.Inner [SiteT.Site (Site.Bound (d, bwc)), lwc]  
                                                  Pat <| Pattern.Inner [SiteT.Site (Site.Unbound (d)), lwc]]))
      Clause.Create(Pred("countSitesInStrand", [wc; TList []; Float 0.0]))
      Clause.Create(Pred("countSitesInStrand", [ vnm "D"
                                                 TCons ( vnm "Site", vnm "Rest" )
                                                 vnm "N" ]),
        [
          Pos <| Pred ("siteMatchesDomain", [vnm "Site"; vnm "D"])
          Pos <| Pred ("countSitesInStrand", [ vnm "D"
                                               vnm "Rest"
                                               vnm "M"])
          Pos <| Pred ("is", [vnm "N"; Func("_Plus", [vnm "M" |> toKey; Float 1.0])])
        ])
      Clause.Create(Pred("countSitesInStrand", [vnm "D"
                                                TCons (vnm "Site"
                                                      , vnm "Rest")
                                                vnm "N"]),
        [
          Neg <| Pred ("siteMatchesDomain", [vnm "Site"; vnm "D"])
          Pos <| Pred ("countSitesInStrand", [ vnm "D"
                                               vnm "Rest"
                                               vnm "N"])
        ])
 
      Clause.Create(Pred("countSites", [wc; TList []; Float 0.0]))
      Clause.Create(Pred("countSites", [vnm "D"; TCons (vnm "Strand", vnm "Rest"); vnm "N"]),
        [
          Pos <| Pred ("countSitesInStrand", [ vnm "D"
                                               ; vnm "Strand"
                                               ; vnm "X"])
          Pos <| Pred ("countSites", [ vnm "D"
                                       ; vnm "Rest"
                                       ; vnm "Y"])
          Pos <| Pred ("is", [vnm "N"; Func("_Plus", [vnm "X" |> toKey; vnm "Y" |> toKey])])
        ]) 
      Clause.Create(Pred("rangeTo", [Float 2.0; TList [Float 2.0]]))
      Clause.Create(Pred("rangeTo", [vnm "N"; TCons (vnm "N", vnm "Rest")])
        , [
            Pos <| Pred (">", [vnm "N"; Float 2.0])
            Pos <| Pred ("is", [vnm "M"; Func ("_Minus", [vnm "N" |> toKey; Float 1.0])])
            Pos <| Pred ("rangeTo", [vnm "M"; vnm "Rest"]) 
          ])
      Clause.Create(Pred("getDomain", [vnm "D"; vnm "D"]), 
        [
          Pos <| Pred ("unbound", [vnm "D"])
          ])
      Clause.Create(Pred("getDomain", [Pat <| Pattern.Inner [SiteT.Site (Site.Bound (d, bwc)), lwc]
                                       Pat <| Pattern.Inner [SiteT.Site (Site.Unbound (d)), lwc]
          ]))
      Clause.Create(Pred("branchMigrationRule", [P; Q]), 
        [
          Pos <| Pred ("toDomains", [P; domainsList])
          Pos <| Pred ("member", [vnm "Domain"; domainsList])
          // optimization: search for junctions only on D domains, not on D* domains too
          Neg <| Pred ("complementary", [vnm "Domain"]) 
          // check how many junctions over D are available in P
          Pos <| Pred ("toList", [P; vnm "Strands"])
          Pos <| Pred ("countSites", [ vnm "Domain"
                                       ; vnm "Strands"
                                       ; vnm "JunctionsCount"])
          Pos <| Pred (">=", [ vnm "JunctionsCount"; Float 2.0])
          Pos <| Pred ("rangeTo", [ vnm "JunctionsCount"
                                  ; vnm "Range"])
          Pos <| Pred ("member", [ vnm "N"
                                 ; vnm "Range"])
          Pos <| Pred ("branchMigration", [ vnm "Domain"
                                            P
                                            Q
                                            vnm "N"])
          ])
      ] |> toProgram
  let s1' = [
      mkU L
      mkB T2  i2 
      mkB X'  j1
      mkB T1  i3
    ]

  let s2' = [
    mkB A   j2 
    mkB X   i1
    mkB T2' i2
    ]  
  
  let s3' = [
    mkB T1' i3 
    mkB X   j1
    mkU R
    ]  
  
  let s4' = [
    mkB X'  i1 
    mkB A'  j2
    ] 
  
  let s5' = [
    mkU A
    ]
  // these strands are from Fig. 1 from the Strands Graphs paper
  let expectedSystem = [ s1'; s2'; s3'; s4'; s5']
                        |> Process.OfList
                        |> Proc
  (program, strands, expectedSystem)

[<Fact(DisplayName="Logic DSD - SG semantics - branch migration rule")>]
let branchMigrationRule() =
  let query = Pos (Pred("branchMigrationRule", [Proc branchMigrationSystem; Term.Var (0, "Q")]))
  match resolveAll query branchMigrationProgram cle with
  | None -> failwith "No solution found"
  | Some sols -> 
    let actualSystems = sols |> List.map (fun sol -> sol.Apply (Term.Var (0, "Q"), cle))
                        |> Set.ofList
    Assert.Equal(actualSystems.Count, 1)
    let actualSystem = actualSystems
                        |> Set.toList 
                        |> List.head
    Assert.Equal(branchMigrationExpectedSystem |> Term.ToString |> removeVarNumbers
                , actualSystem |> Term.ToString |> removeVarNumbers)
                
[<Fact(DisplayName="Logic Engine - Resolution - Expressions")>]
let testArithmetics () = 
  let provider  = makeProvider ()
  let ts        = psite cle provider false
  let program   = from_string (pprogram engine (fun x -> psite cle x false)) """question(Answer) :- Answer is 4 + 1 - 2 * 2."""
  let query     = "question(Answer)" |> from_string (ppredicate engine ts provider engine.domainKeywords) |> Pos
  match resolveAll query program engine with
  | None -> failwith "No solution found"
  | Some sols -> 
    let actualSystems = sols |> List.map (fun sol -> sol.Apply (Term.Var (0, "Answer"), cle))
                             |> Set.ofList
    Assert.Equal(actualSystems.Count, 1)
    Assert.Equal(actualSystems |> Set.toList |> List.head, Term.Float 1.0)
