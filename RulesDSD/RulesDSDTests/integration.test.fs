// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module RulesDSD.IntegrationTests

open FsUnit
open Xunit
open FsUnit.Xunit

open RulesDSD.Syntax
open RulesDSD.Resolution


let cle   = Microsoft.Research.DNA.LogicDSD.engine
let psite = Microsoft.Research.DNA.LogicDSD.psite cle

[<Fact(DisplayName="Logic DSD - Resolution - Context finding")>]
let testContextFinding () =
  let provider  = RulesDSD.Syntax.makeProvider ()
  let ps = psite provider true
  let strands = Parser.from_string (RulesDSD.Parser.psystem ps)
                  "<a b!0> | <d e^!1> | <f c^!2 d!3> | <e^*!1 d*!3 c^*!2 b*!0>"
                  
  let program = Parser.from_string (RulesDSD.Parser.pprogram cle (fun x -> psite x false))
                  "ok(P) :- 
                    P = C [E!j D] [D!i] [D'!i F!k].
                    
                   ok(P) :-
                    P = C [D E!j] [D!i] [F!k D'!i]."
  let goal    = Pos (Pred("ok", [Term.Proc strands]))
  match resolveAll goal program cle with 
  | None      -> failwith "Test failed."
  | Some sols -> let xs = sols |> List.map (fun theta -> theta.Apply (Term.Var (provider "C"), cle))
                 Assert.Equal(1, xs.Length)


[<Fact(DisplayName="Logic DSD - Resolution - Context substitution")>]
let testContextSubs () =
  let provider  = RulesDSD.Syntax.makeProvider ()
  let ps = psite provider true
  let strands = Parser.from_string (RulesDSD.Parser.psystem ps)
                  "<a b!0> | <d e^!1> | <f c^!2 d!3> | <e^*!1 d*!3 c^*!2 b*!0>"
                  
  let program = Parser.from_string (RulesDSD.Parser.pprogram cle (fun x -> psite x false))
                  "ok(P, Q) :- 
                    P = C [E!j D] [D!i] [D'!i F!k],
                    Q = C [E!j D!i] [D] [D'!i F!k].
                    
                   ok(P, Q) :-
                    P = C [D E!j] [D!i] [F!k D'!i],
                    Q = C [D!i E!j] [D] [F!k D'!i]."
  let goal    = Pos (Pred("ok", [Term.Proc strands; Term.Var (provider "Q")]))
  match resolveAll goal program cle with 
  | None      -> failwith "Test failed."
  | Some sols -> let xs = sols |> List.map (fun theta -> theta.Apply (Term.Var (provider "Q"), cle))
                 Assert.Equal(1, xs.Length)


[<Fact(DisplayName="Logic DSD - Resolution - Context displacement")>]
let testContextDispl () =
  let provider  = RulesDSD.Syntax.makeProvider ()
  let ps = psite provider true
  let strands  = Parser.from_string (RulesDSD.Parser.psystem ps)
                  "<a b!0> | <d e^!1> | <f c^!2 d!3> | <e^*!1 d*!3 c^*!2 b*!0>"
                  
  let program = Parser.from_string (RulesDSD.Parser.pprogram cle (fun x -> psite x false))
                  "reaction([P], Rate, Q) :-
                      P = C [E!j D] [D!i] [D'!i F!k],
                      Q = C [E!j D!i] [D] [D'!i F!k],
                      Rate is 20,
                      junction(E!j, F!k, Q).
                      // junction(E!j, Q).

                   reaction([P], Rate, Q) :-
                      P = C [D E!j] [D!i] [F!k D'!i],
                      Q = C [D!i E!j] [D] [F!k D'!i],
                      Rate is 20,
                      junction(E!j, F!k, Q).
                   
                   junction(E!j,F!j,_).
                   junction(E!j,F!k,Q):- 
                      Q = C [F!k] [G!l E'!j] [E!j],
                      junction(G!l,F!k,Q)."

  let goal    = Pos (Pred("reaction", [TList [Term.Proc strands]; Term.Var (provider "Rate"); Term.Var (provider "Q")]))
  match resolveAll goal program cle with 
  | None      -> failwith "Test failed."
  | Some sols -> let xs = sols |> List.map (fun theta -> theta.Apply (Term.Var (provider "Q"), cle))
                 Assert.Equal(1, xs.Length)


[<Fact(DisplayName="Logic DSD - Syntax - Canonical form")>]
let testCanonical () =
  let provider  = RulesDSD.Syntax.makeProvider ()
  let ps = psite provider true
  let canonize x = x |> Parser.from_string (RulesDSD.Parser.psystem ps) |> Process.Canonical cle
  let a = canonize "<a!0 b!1> | <a!2 b!3> | <b!3 a!2 b!1 a!0>" |> Process.ToStringWith cle
  let b = canonize "<a!2 b!3> | <a!0 b!1> | <b!3 a!2 b!1 a!0>" |> Process.ToStringWith cle
  let c = canonize "<a!0 b!1> | <a!2 b!3> | <b!1 a!0 b!3 a!2>" |> Process.ToStringWith cle
  let d = canonize "<a!2 b!3> | <a!0 b!1> | <b!1 a!0 b!3 a!2>" |> Process.ToStringWith cle
  Assert.True( (a = b) )
  Assert.True( (b = c) )
  Assert.True( (c = d) )
  
  let e = canonize "<b tx^!0> | <to^*!1 x*!2 tx^*!0 b*!3 tb^*!4> | <x!2 to^!1> | <tb^!4 b!3>" |> Process.ToStringWith cle
  let f = canonize "<to^*!0 x*!1 tx^*!2 b*!3 tb^*!4> | <x!1 to^!0> | <b tx^!2> | <tb^!4 b!3>" |> Process.ToStringWith cle
  Assert.True( (e = f) )

  let g = canonize "<a^!0 c^*!1 a^*!2 c^!3 a^*!4 c^*!5 a^*!6 a^*!7 a^*!8 c^!9 c^*!10 a^!11 c^*!12 a^*!13 a^!14 a^!15> | <a^!16 a^*!17 c^*!18 a^!19 c^*!20 a^*!21 c^!22 a^!23 a^*!24 c^*!25 c^!26> | <a^*{\"O\"}!15 a^*!14 a^!13 c^!12 a^*!11 c^!10 c^*!9 a^!8 a^!7 a^!6 c^!5 a^!4 c^*!3 a^!2 c^!1 a^*!0 c^*!26 c^!25 a^!24 a^*!23 c^*!22 a^!21 c^!20 a^*!19 c^!18 a^!17 a^*!16>" |> Process.ToStringWith cle
  let h = canonize "<a^!0 a^*!1 c^*!2 a^!3 c^*!4 a^*!5 c^!6 a^!7 a^*!8 c^*!9 c^!10> | <a^!11 c^*!12 a^*!13 c^!14 a^*!15 c^*!16 a^*!17 a^*!18 a^*!19 c^!20 c^*!21 a^!22 c^*!23 a^*!24 a^!25 a^!26> | <a^*{\"O\"}!26 a^*!25 a^!24 c^!23 a^*!22 c^!21 c^*!20 a^!19 a^!18 a^!17 c^!16 a^!15 c^*!14 a^!13 c^!12 a^*!11 c^*!10 c^!9 a^!8 a^*!7 c^*!6 a^!5 c^!4 a^*!3 c^!2 a^!1 a^*!0>" |> Process.ToStringWith cle
  Assert.True( (g = h) )



[<Fact(DisplayName="Logic DSD - Resolution - Phosphate nucleotide")>]
let testPhopsho () =  
  let provider1  = RulesDSD.Syntax.makeProvider ()
  let ps1 = psite provider1 true
  let strands1 = Parser.from_string (RulesDSD.Parser.psystem ps1)
                  """<a!0 b{"phosphate"}> | <a*!0>"""
                  
  let program = Parser.from_string (RulesDSD.Parser.pprogram cle (fun x -> psite x false))
                  """polymerase(P,Q,A!i,B!j) :- 
                    P = C [<A!i>]     [<A'!i B'>], not (B' = _ {"phosphate"}), compl(B, B'),
                    Q = C [<A!i B!j>] [<B'!j A'!i>], freshBond(B!j, P)."""

  let goal   = Pos (Pred("polymerase", [Term.Proc strands1; Term.Var (-1, "_"); Term.Var (-1, "_"); Term.Var (-1, "_")]))
  let result = resolveAll goal program cle
  Assert.True( Option.isNone result)

  let provider2  = RulesDSD.Syntax.makeProvider ()
  let ps2 = psite provider2 true
  let strands2 = Parser.from_string (RulesDSD.Parser.psystem ps2)
                  """<a!0 b{"phosphat"}> | <a*!0>"""
  let goal   = Pos (Pred("polymerase", [Term.Proc strands2; Term.Var (-1, "_"); Term.Var (-1, "_"); Term.Var (-1, "_")]))
  let result = resolveAll goal program cle
  Assert.True( Option.isSome result)
  
  let provider3  = RulesDSD.Syntax.makeProvider ()
  let ps3 = psite provider3 true
  let strands3 = Parser.from_string (RulesDSD.Parser.psystem ps3)
                  """<a!0 b> | <a*!0>"""
  let goal   = Pos (Pred("polymerase", [Term.Proc strands3; Term.Var (-1, "_"); Term.Var (-1, "_"); Term.Var (-1, "_")]))
  let result = resolveAll goal program cle
  Assert.True( Option.isSome result)


[<Fact(DisplayName="Logic DSD - Resolution - Pseudoknot formation")>]
let testPseudoknot () =  
  let provider1  = RulesDSD.Syntax.makeProvider ()
  let ps1 = psite provider1 true
  let program = Parser.from_string (RulesDSD.Parser.pprogram cle (fun x -> psite x false)) """
bind(P,Q,D!i) :- 
  P = C [D] [D'], compl(D, D'), 
  Q = C [D!i] [D'!i], freshBond(D!i, P).
  
displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!j D'!i],
  Q = C [D] [D!i E!j] [E'!j D'!i].

unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

reaction([P], "bind",Q) :- bind(P,Q,_).
reaction([P],"displace",Q) :- displace(P,Q,_,_).
reaction([P],"displace",Q) :- displaceL(P,Q,_,_).
reaction([P],"unbind",Q) :- unbind(P,Q,_)."""

  let strands = Parser.from_string (RulesDSD.Parser.psystem ps1)
                  """<a b a* b*>"""
  let goal   = Pos (Pred("reaction", [Term.TList [Term.Proc strands]; Term.Var (-1, "_"); Term.Var (-1, "Q")]))
  let result = resolveAll goal program cle
  Assert.True( Option.isSome result)