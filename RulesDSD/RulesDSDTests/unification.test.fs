module RulesDSD.UnificationTests

open FsUnit
open Xunit
open FsUnit.Xunit

open RulesDSD.Syntax
open RulesDSD.Unification
open Microsoft.Research.DNA.LogicDSD

let X = Term.Var (0, "X")
let Y = Term.Var (1, "Y")
let Z = Term.Var (2, "Z")

let cle = Microsoft.Research.DNA.LogicDSD.engine

[<Fact(DisplayName="Logic Engine - Unification - var/const")>]
let varConst() =
  let eqs = [TEq (X, Const "c")]
  let sol = match unify cle eqs with
            | [theta]  -> theta
            | _       -> failwith "Unification failed"
  Assert.Equal(Const "c", sol.Apply(X, cle))



[<Fact(DisplayName="Logic Engine - Unification - var/func")>]
let varFunc () =
  let eqs = [TEq (X, Func("f", [Const "c"; Y]) )]
  let sol = match unify cle eqs with
            | [theta]  -> theta
            | _        -> failwith "Unification failed"
  Assert.Equal(Func("f", [Const "c"; Y]), sol.Apply(X, cle))



[<Fact(DisplayName="Logic Engine - Unification - func/func")>]
let funcFunc () =
  let eqs = [TEq (Func ("f", [X; Float 1.0]), Func("f", [Const "c"; Y]) )]
  let sol = match unify cle eqs with
            | [theta]  -> theta
            | _        -> failwith "Unification failed"
  Assert.Equal(Const "c", sol.Apply(X, cle))
  Assert.Equal(Float 1.0, sol.Apply(Y, cle))



[<Fact(DisplayName="Logic Engine - Unification - mixed")>]
let mixed() =
  let eqs = [ TEq (Func ("f", [X; Z]), Func("f", [Y; Const "2"]) ); 
              TEq (X, Const "c")]
  let sol = match unify cle eqs with
            | [theta]  -> theta
            | _        -> failwith "Unification failed"
  Assert.Equal(Const "c", sol.Apply (X, cle))
  Assert.Equal(Const "c", sol.Apply (Y, cle))
  Assert.Equal(Const "2", sol.Apply (Z, cle))



[<Fact(DisplayName="Logic Engine - Unification - fibonacci")>]
let fibonacci() =
  let eqs = [TEq (Func("fib", [X; Float 1.0]), Func("fib", [Float 0.0; Float 1.0]) )]
  let sol = match unify cle eqs with
            | [theta]  -> theta
            | _        -> failwith "Unification failed"
  Assert.Equal(Float 0.0, sol.Apply(X,cle))



[<Fact(DisplayName="Logic Engine - Unification - nicking pattern matching")>]
let nick() =
  let domA  = Dom {name = "a"; isComplementary = false; isToehold = false; tag = NoTag}
  let domB  = Dom {name = "b"; isComplementary = false; isToehold = false; tag = NoTag}
  let a = Site (Unbound domA), Location.Var (-1, "_")
  let b = Site (Unbound domB), Location.Var (-1, "_")
  let p1  = Pat (Nicking ([SiteT.Var (0, "X"), Location.Var (-1, "_")],[b]))
  let p2  = Pat (Nicking ([a],[SiteT.Var (1, "Y"), Location.Var (-1, "_")]))
  let eqs = [TEq (p1, p2)]
  let sol = match unify cle eqs with
            | [theta]  -> theta
            | sub      -> failwith "Unification failed"
  Assert.Equal(a, sol.Apply(((SiteT.Var (0, "X")), Location.Var (-1, "_")), cle))
  Assert.Equal(b, sol.Apply(((SiteT.Var (1, "Y")), Location.Var (-1, "_")), cle))




[<Fact(DisplayName="Logic Engine - Unification - Multi-sets")>]
let mset1() =
  let t1 : Term<unit> = Term.TMSet [1, Term.Const "A"; 2, Term.Const "B"]
  let t2 : Term<unit> = Term.TMSet [2, Term.Const "B"; 1, Term.Const "A"]
  match unify CLE.empty [TEq (t1, t2)] with
  | [theta]  -> if theta <> Substitution.Substitution.id then failwith "Unification failed (1.1)"
  | sub      -> failwith "Unification failed (1)"

  let t1 : Term<unit> = Term.TMSet [1, Term.Const "A"; 2, Term.Const "B"]
  let t2 : Term<unit> = Term.TMSet [2, Term.Const "B"; 1, Term.Var (1, "X")]
  match unify CLE.empty [TEq (t1, t2)] with
  | [theta]  -> Assert.Equal(Term.Const "A", theta.Apply(Term.Const "A", CLE<unit,unit>.empty))
  | sub      -> failwith "Unification failed (2)"

  let t1 = Term.TMSet [1, Term.Const "A"; 2, Term.Const "B"]
  let t2 = Term.TMSet [1, Term.Var (1, "X");  1, Term.Const "A"; 1, Term.Var(1, "Y")]
  match unify CLE.empty [TEq (t1, t2)] with
  | [theta]  -> Assert.Equal(Term.Const "B", theta.Apply(Term.Var (1, "X"), CLE<unit,unit>.empty))
                Assert.Equal(Term.Const "B", theta.Apply(Term.Var (1, "Y"), CLE<unit,unit>.empty))
  | sub      -> failwith "Unification failed (3)"

  ()

[<Fact(DisplayName="Logic Engine - Unification - CRNs")>]
let crn1() =
  // test 1
  let crn1 = Term.TCRN [
                // A + B -> 2C
                Term.Func("_rxn", [Term.TMSet [1, Term.Const "A"; 1, Term.Const "B"]; Term.Const "k"; Term.TMSet [2, Term.Const "C"] ]);
            ]
  let crn2 = Term.TCRN [
                // B + A -> 2C
                Term.Func("_rxn", [Term.TMSet [1, Term.Const "B"; 1, Term.Const "A"]; Term.Const "k"; Term.TMSet [2, Term.Const "C"] ]);
             ]
  match unify CLE.empty [TEq (crn1, crn2)] with
  | [theta]  -> if theta <> Substitution.Substitution.id then failwith "Unification failed (1.1)"
  | sub      -> failwith "Unification failed (1)"

  // test 2
  let crn1 = Term.TCRN [
    // A + B -> 2C
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "A"; 1, Term.Const "B"]; Term.Const "k"; Term.TMSet [2, Term.Const "C"] ]);
            ]
  let crn2 = Term.TCRN [
    // B + A -> X
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "B"; 1, Term.Const "A"]; Term.Const "k"; Term.Var(-1, "X") ]);
             ]
  match unify CLE.empty [TEq (crn1, crn2)] with
  | [theta]  -> Assert.Equal(Term.TMSet [2, Term.Const "C"], theta.Apply(Term.Var(-1, "X"), CLE<unit,unit>.empty))
  | sub      -> failwith "Unification failed (1)"

  // test 3: multiple ground reactions
  let crn1 = Term.TCRN [
    // A + B -> 2C
    // C -> 2A + B
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "A"; 1, Term.Const "B"]; Term.Const "k1"; Term.TMSet [2, Term.Const "C"] ])
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "C"]; Term.Const "k2"; Term.TMSet [2, Term.Const "A"; 1, Term.Const "B"] ]) ]
  let crn2 = Term.TCRN [
    // C -> B + 2A
    // B + A -> 2C
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "C"]; Term.Const "k2"; Term.TMSet [1, Term.Const "B"; 2, Term.Const "A"] ]) 
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "B"; 1, Term.Const "A"]; Term.Const "k1"; Term.TMSet [2, Term.Const "C" ] ]) ]
  
  match unify CLE.empty [TEq (crn1, crn2)] with
  | [theta]  -> if theta <> Substitution.Substitution.id then failwith "Unification failed (3.1)"
  | sub      -> failwith "Unification failed (3)"
  
  // test 4: multiple reactions with variables, 1 possible solution
  let crn1 = Term.TCRN [
    // A + B -> 2C
    // C -> 2A + B
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "A"; 1, Term.Const "B"]; Term.Const "k1"; Term.TMSet [2, Term.Const "C"] ])
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "C"]; Term.Const "k2"; Term.TMSet [2, Term.Const "A"; 1, Term.Const "B"] ]) ]
  let crn2 = Term.TCRN [
    // C -> B + 2A
    // B + A -> 2C
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "C"]; Term.Const "k2"; Term.TMSet [1, Term.Const "B"; 2, Term.Var (2, "Y")] ]) 
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "B"; 1, Term.Var (1, "X")]; Term.Const "k1"; Term.TMSet [2, Term.Const "C" ] ]) ]
  
  match unify CLE.empty [TEq (crn1, crn2)] with
  | [theta]  -> Assert.Equal(Term.Const "A", theta.Apply(Term.Var(1, "X"), CLE<unit,unit>.empty)) 
                Assert.Equal(Term.Const "A", theta.Apply(Term.Var(2, "Y"), CLE<unit,unit>.empty)) 
  | sub      -> failwith "Unification failed (4)"

  // test 5: multiple reactions with variables, many possible solutions
  let crn1 = Term.TCRN [
    // A + B -> 2C
    // C -> 2A + B
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "A"; 1, Term.Const "B"]; Term.Const "k1"; Term.TMSet [2, Term.Const "C"] ])
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "C"]; Term.Const "k2"; Term.TMSet [2, Term.Const "A"; 1, Term.Const "B"] ]) ]
  let crn2 = Term.TCRN [
    // C -> B + 2A
    // B + A -> 2C
    Term.Func("_rxn", [Term.TMSet [1, Term.Const "C"]; Term.Const "k2"; Term.TMSet [1, Term.Const "B"; 2, Term.Var (3, "Z")] ]) 
    Term.Func("_rxn", [Term.TMSet [1, Term.Var (1, "X"); 1, Term.Var (2, "Y")]; Term.Const "k1"; Term.TMSet [2, Term.Const "C" ] ]) ]
  
  match unify CLE.empty [TEq (crn1, crn2)] with
  | [theta1; theta2]  -> let sol1 = [theta1.Apply(Term.Var(1, "X"), CLE<unit,unit>.empty); theta1.Apply(Term.Var(2, "Y"), CLE<unit,unit>.empty)]
                         let sol2 = [theta2.Apply(Term.Var(2, "Y"), CLE<unit,unit>.empty); theta2.Apply(Term.Var(1, "X"), CLE<unit,unit>.empty)]
                         let x = sol1 = sol2
                         Assert.True(x) 
                         
  | sub      -> failwith "Unification failed (5)"
  ()