module RulesDSD.ParserTests

open FsUnit
open Xunit
open FsUnit.Xunit

open RulesDSD.Syntax
open RulesDSD.Unification
open RulesDSD.Substitution
open RulesDSD.Resolution
open RulesDSD.Parser
open Parser
open ResolutionTests
open System.Text.RegularExpressions
open Microsoft.Research.DNA.LogicDSD

let cle = Microsoft.Research.DNA.LogicDSD.engine

let flattenDict (x:System.Collections.Generic.IDictionary<(string * int),Set<Clause<SiteT>>>) = 
    x.Values 
    |> Seq.concat
    |> Seq.toList

let removeVarNumbers x = Regex.Replace(x, "\([0-9]*\)", "")
let print    = List.map (Clause.ToStringWith cle >> removeVarNumbers) >> Seq.sortWith (fun x y -> System.String.Compare(x, y)) >> String.concat "\n"
let printClause = Clause.ToStringWith cle >> removeVarNumbers

[<Fact(DisplayName="Logic Engine - Parsing - numbers")>]
let parsingNumbers() =
  let text = """1"""
  let cache      = idProvider ()
  let ps         = fun _ -> failwith ""
  let parsed     = Parser.from_string (pterm ps cle cache engine.domainKeywords) text 
  
  Assert.Equal<Term<SiteT>>(Term.Float 1.0, parsed)

[<Fact(DisplayName="Logic DSD - Parsing - anchored predicate")>]
let parsingAnchoredPredicateTest() =
  let text = """
strandDisplacementRule(P, Q) :-
  P = C [E!j D] [D!i] [D'!i F!k],
  Q = C [E!j D!i] [D] [D'!i F!k],
  junction(E!j, F!k, Q).
  
junction(E!j,F!j,_).

junction(E!j,F!k,Q) :- 
    Q = C [G!l E'!j] [E!j],
    junction(G!l,F!k,Q).
  """

  let parsed   = Parser.from_string (pprogram cle (fun x -> psite cle x false)) text |> flattenDict
  let expected = anchoredPredicateFailProgram     |> flattenDict
  // TODO: parse  <a*!1> | <a!1 b> | <b!2> | <a!3> | <c!4> | <c*!4 b*!2 a*!3>
  Assert.Equal<string>(print expected, print parsed)

let hiddenProgramText = """
hidden(D!i, P) :-
  P = C [D!i] [A @ Start  D'!i @ End],
  path(A@Start, _@Start, End, P, "left", [], Path),
  not (member(D!i, Path)).

path(_, End, End, _, _, Visited, Path) :-
  reverse(Visited, Path).

path(X, Start, End, P, "left", Visited, Path) :-
  not (Start = End),
  P = C[ Y@Start' _@Start],
  // prevDomain(Start, [Y; Start'], P),
  not member([Y; Start'], Visited),
  path(Y, Start', End, P, "left", [[X; Start] # Visited], Path).

path(X, Start, End, P, "right", Visited, Path) :-
  not (Start = End),
  // nextDomain(Start, [Y; Start'], P),
  P = C[ _@Start Y@Start' ],
  not member([Y; Start'], Visited),
  path(Y, Start', End, P, "right", [[X; Start] # Visited], Path).
  
path(X, Start, End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X, Start, End, P, "left", Visited, Path).

path(X, Start, End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X, Start, End, P, "right", Visited, Path).

path(D!i, Start, _@End, P, _, Visited, Path) :-
  not (Start = _@End),
  P = C [D!i] [D'!i@Start'],
  // pos(1, 0, C, Start'),
  not member([D'!i; Start'], Visited),
  path(D'!i, _@Start', _@End, P, "any", [[D!i; Start] # Visited], Path).
"""

[<Fact(DisplayName="Logic DSD - Parsing - hidden predicate")>]
let parsingHiddenPredicateTest() =
  let parsed   = Parser.from_string (pprogram cle (fun x -> psite cle x false)) hiddenProgramText 
                  |> fun x -> x.Values 
                  |> Seq.concat
                  |> Seq.toList
  let expected = hiddenProg 
  Assert.Equal<string>(print expected, print parsed)


[<Fact(DisplayName="Logic DSD - Parsing - hierarchy")>]
let parsingHierarchyTest() =
  let text = """rule("fast", P, Q) :-
  P = C [E!j D] [D!i] [D'!i F!k],
  Q = C [E!j D!i] [D] [D'!i F!k],
  junction(E!j, F!k, Q).

junction(E!j,F!j,_).
  
junction(E!j,F!k,Q):- 
  // Q = C [G!l E'!j] [E!j] [F!k],
  Q = C [F!k] [E!j] [G!l E'!j],
  junction(G!l, F!k, Q).
  
rule("loop", P, Q, Q, V).  // V for visited
rule("loop", P, Q, R, V) :-
  rule("hierarchy", Q, R, V).  

rule("hierarchy", P, R, V) :-
  rule("fast", P, Q),
  not member(Q, V),
  rule("loop", P, Q, R, [Q # V]).
  
rule("rxn", P ,Q) :-
  rule("hierarchy", P, Q, [P]),
  not rule("fast", Q, _).
"""
  let parsed   = Parser.from_string (pprogram cle (fun x -> psite cle x false)) text |> flattenDict
  let expected = hierarchyProgram |> flattenDict
  Assert.Equal<string>(print expected, print parsed)


(****************************)
(* Strand Graphs semantics  *)
(****************************)
[<Fact(DisplayName="Logic DSD - Parsing - SG binding rule")>]
let parsingBindingRuleTest() =
  let text = hiddenProgramText + """
  bindingRule(P, Q) :-
    P = C [D][D'],
    freshBond(D!i, P),
    compl(D, D'),
    Q = C [D!i] [D'!i],
    not hidden(D!i, Q).
  """
  let parsed   = Parser.from_string (pprogram cle (fun x -> psite cle x false)) text |> flattenDict
  let expected = bindingProgram |> flattenDict
  let s1 = print expected
  let s2 = print parsed
  Assert.Equal<string>(print expected, print parsed)


[<Fact(DisplayName="Logic DSD - Parsing - SG unbinding rule")>]
let parsingUnbindingRuleTest() =
  let text = """
rule("unbinding", P, Q) :-
  P = C [X!i] [X'!i],
  // toehold(X),
  Q = C [X] [X'],
  not junction(X!i, P).
  
junction(_!i, P) :-
  P = C [X!i E!j] [F!k X'!i],
  junction(X!i E!j, F!k X'!i, P).
  
junction(E!j,F!j,_).

junction(E!j,F!k,Q):- 
  Q = C [F!k] [E!j] [G!l E'!j],
  junction(G!l,F!k,Q).
  """
  let parsed   = Parser.from_string (pprogram cle (fun x -> psite cle x false)) text |> flattenDict
  let expected = unbindingProgram                 |> flattenDict
  Assert.Equal<string>(print expected, print parsed)



[<Fact(DisplayName="Logic DSD - Parsing - SG strand displacement rule")>]
let parsingStrandDisplacementRuleTest() =
  let text = """
junction(_!i, P) :-
  P = C [X!i E!j] [F!k X'!i],
  junction(E!j, F!k, P).
  
junction(E!j,F!j,_).

junction(E!j,F!k,Q):- 
  Q = C [F!k] [G!l E'!j] [E!j],
  junction(G!l,F!k,Q).

strandDisplacementRule(P, Q) :- 
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j],
  junction(D!i, Q).
  """
  
  let parsed   = Parser.from_string (pprogram cle (fun x -> psite cle x false)) text
                  |> fun x -> x.Values 
                  |> Seq.concat
                  |> Seq.toList
  let expected = strandDisplacementProgram
                  |> fun x -> x.Values 
                  |> Seq.concat
                  |> Seq.toList
  Assert.Equal<string>(print expected, print parsed)


(* This case implements the branch migration rule for N junctions in the most general case possible. Because of this its implementation is rather convoluted, and uses experimental features like the C[[X]] construct, where X is a list of N patterns created dynamically, which encodes a context C[\pi_1] ... [\pi_N]. *)
[<Fact(DisplayName="Logic DSD - Parsing - SG branch migration rule")>]
let parsingBranchMigrationRule() =
  let text = """// create a pattern of the form [D!i1][D'!i1][D!i2][D'!i2] ... [D!iN][D'!iN]  
makeNHoles(D, D', [D!i; D'!i], [D!i], 1).
makeNHoles(D, D', [D!i; D'!i # RestOfPatterns], [D!i # FreshBonds], N):-
N > 1,
M is N - 1,
makeNHoles(D, D', RestOfPatterns, FreshBonds, M).

// turn [D!i1][D'!i1][D!i2][D'!i2] ... [D!iN][D'!iN] into [D!i1][D'!i2][D!i2][D'!i3] ... [D!iN][D'!i1]
migrationPattern([D!i; D'!j], [D!i; D'!j]).
migrationPattern([ D!i  // 1
               ; D'!j // 2
               ; D!k  // 3
               ; D'!k // 4
               # RestOfStartingPattern ], 
       [ D!i  // 1
       ; D'!k // 4
       # RestOfMigrationPattern ]) :-
migrationPattern([ D!k   // 3
                 ; D'!j  // 2
                 # RestOfStartingPattern],
                 RestOfMigrationPattern).
         
findLoop(E!j, Q, Loop) :-
// encoding of Q = C [(E@Start)!j] [(E'@End)!j]
Q = C [E!j @ Start] [E'!j @ End],
path(E!j, Start, End, Q, "any", [], [_ # Loop] (* discards E!j from the loop*)),  
not (Loop = []).

branchesOnly([]).
branchesOnly([[E!j; _]; [E'!j; _] # Rest]) :-
branchesOnly(Rest).

junctionPath(E!j, Q) :-
findLoop(E!j, Q, Loop),
branchesOnly(Loop).

checkAllAnchored([], Q).
checkAllAnchored([D!i#Rest], Q) :- 
junctionPath(D!i, Q),
checkAllAnchored(Rest, Q).

// perform N-way migration on D
branchMigration(D, P, Q, N) :-
unbound(D),
compl(D, D'),
makeNHoles(D, D', Junctions, FreshBonds, N),
P = C[[Junctions]],
migrationPattern(Junctions, MigratedJunctions),
Q = C[[MigratedJunctions]],
checkAllAnchored(FreshBonds, Q).

siteMatchesDomain(D!_, D).

countSitesInStrand(_, [], 0).
countSitesInStrand(D, [Site # Rest], N) :-
siteMatchesDomain(Site, D),
countSitesInStrand(D, Rest, M),
N is M + 1.
countSitesInStrand(D, [Site # Rest], N) :-
not siteMatchesDomain(Site, D),
countSitesInStrand(D, Rest, N).

countSites(_, [], 0).
countSites(D, [Strand # Rest], N) :-
countSitesInStrand(D, Strand, X),
countSites(D, Rest, Y),
N is X + Y.

rangeTo(2,[2]).
rangeTo(N, [N # Rest]) :-
N > 2,
M is N - 1,
rangeTo(M, Rest).

getDomain(D, D) :- unbound(D). 
getDomain(D!_, D).

branchMigrationRule(P, Q) :-
// get all the domains in P 
toDomains(P, DomainsList),

// pick a domain in P
member(Domain, DomainsList),
// optimization: search for junctions only on D domains, not on D* domains too
not complementary(Domain),
// get all strands in P
toList(P, Strands),
countSites(Domain, Strands, JunctionsCount),
JunctionsCount >= 2,
// create a list [2.. max number of junctions over D]
rangeTo(JunctionsCount, Range),
// pick a number N in that range
member(N, Range),
// perform branch migration over N branches on D
branchMigration(Domain, P, Q, N)."""
  let parsed     = Parser.from_string (pprogram cle (fun x -> psite cle x false)) text 
                  |> fun x -> x.Values 
                  |> Seq.concat
                  |> Seq.toList
  
  let otherProgs = junctionProgram @ hiddenProg |> Set.ofList
  let branchMP'  = Set.difference (branchMigrationProgram |> flattenDict |> Set.ofList) otherProgs
  let expected   = branchMP'  |> Set.toList
  let y = print expected
  let x = print parsed
  Assert.Equal<string>(print expected, print parsed)


(* Instance of the N-way branch migration rule *)
[<Fact(DisplayName="Logic DSD - Parsing - SG 4-way branch migration rule")>]
let parsing4wayBranchMigrationRule() =
  let text = """
(* Encoding of the junction predicate using the path predicate *)
findLoop(E!j, Q, Loop) :-
  // encoding of Q = C [(E@Start)!j] [(E'@End)!j]
  Q = C [E!j] [E'!j],
  pos(0, 0, C, Start),
  pos(1, 0, C, End),
  path(E!j, Start, End, Q, "any", [], [_ # Loop] (* discards E!j from the loop*)),  
  not (Loop = []).

branchesOnly([]).
branchesOnly([[E!j; _]; [E'!j; _] # Rest]) :-
  branchesOnly(Rest).
  
junction(E!j, Q) :-
  findLoop(E!j, Q, Loop),
  branchesOnly(Loop).
  
// perform N-way migration on D
branchMigration(P, Q) :-
  P = C [D!i1] [D'!i1] [D!i2] [D'!i2],
  Q = C [D!i1] [D'!i2] [D!i2] [D'!i1],
  junction(D!i1, Q),
  junction(D!i2, Q).
"""
  Parser.from_string (pprogram cle (fun x -> psite cle x false)) text 
    |> ignore


[<Fact(DisplayName="Logic DSD - Resolution - locations")>]
let testLocations() =
  let text = """
testLocations(P, X, Y, Z) :- 
  P = C1 [a!I@X],
  P = C2 [_@X _@Y],
  P = C3 [e@Z].
  """
  
  let provider = makeProvider ()
  let ps = psite cle provider true
  let strands = Parser.from_string (psystem ps)"<d a!1 b!2> | <c!3 e> |  <c*!3 b*!2 a*!1>"
  
  let program = Parser.from_string (pprogram cle (fun x -> psite cle x false)) text
  let query   = Pos (Pred("testLocations", [Proc strands; Term.Var (0, "X"); Term.Var (1, "Y"); Term.Var (2, "Z")]))
  
  let theta = match resolve query program cle with
              | Some s -> s
              | None   -> failwith "Strand displacement rule failed."

  let x = theta.Apply (Term.Var (0, "X"), cle)
  let y = theta.Apply (Term.Var (1, "Y"), cle)
  let z = theta.Apply (Term.Var (2, "Z"), cle)
  Assert.Equal(Term.Pat <| Pattern.Inner[SiteT.Var (-1, "_"), Location.Loc (0,1)], x)
  Assert.Equal(Term.Pat <| Pattern.Inner[SiteT.Var (-1, "_"), Location.Loc (0,2)], y)
  Assert.Equal(Term.Pat <| Pattern.Inner[SiteT.Var (-1, "_"), Location.Loc (1,1)], z)



[<Fact(DisplayName="Logic DSD - Parsing - Indeterminate list")>]
let testIndetList () = 
  let provider1 = makeProvider ()
  let ts1       = psite cle provider1 false
  let actualT   = from_string (pterm ts1 cle provider1 engine.domainKeywords) "[Q # V]"
  let provider2 = makeProvider ()
  let ts2       = psite cle provider2 false
  let q         = from_string (pterm ts2 cle provider2 engine.domainKeywords) "Q"
  let v         = from_string (pterm ts2 cle provider2 engine.domainKeywords) "V"
  let expectedT = TCons(q, v)
  Assert.Equal(expectedT, actualT)

  let provider3 = makeProvider ()
  let ts3       = psite cle provider3 false
  let actualP   = from_string (ppredicate cle ts3 provider3 engine.domainKeywords) "rule(\"loop\", P, Q, R, [Q # V])"
  let provider4 = makeProvider ()
  let ts4       = psite cle provider4 false
  let pt        = from_string (pterm ts4 cle provider4 engine.domainKeywords)
  let p         = pt "P"
  let q         = pt "Q"
  let r         = pt "R"
  let v         = pt "V"
  let expectedP = Pred("rule", [Const "loop"; p; q; r; TCons(q, v)])
  Assert.Equal(expectedP |> Predicate.ToStringWith cle |> removeVarNumbers, 
               actualP   |> Predicate.ToStringWith cle |> removeVarNumbers)


[<Fact(DisplayName="Logic DSD - Parsing - Domain")>]
let testDom () = 
  let provider  = makeProvider ()
  let ts        = psite cle provider false
  let actualT   = from_string (pterm ts cle provider engine.domainKeywords) """a!i"""
  let expectedT = Term.Pat 
                    (Pattern.Inner 
                      [SiteT.Site 
                        (Site.Bound 
                          (DomainT.Dom(Domain.Create("a", false, false))
                          , Bond.Var (provider "i"))), Location.Var (-1,"_")])
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)

[<Fact(DisplayName="Logic DSD - Parsing - Domain tags")>]
let testTags () = 
  let provider  = makeProvider ()
  let ts        = psite cle provider false
  let actualT   = from_string (pterm ts cle provider engine.domainKeywords) """A {tethered("x","y","z")}!i"""
  let expectedT = Term.Pat 
                    (Pattern.Inner 
                      [SiteT.Site 
                        (Site.Bound 
                          (DomainT.Var(provider "A", Tag <| Term.Func("tethered", [Const "x"; Const"y"; Const"z"]))
                          , Bond.Var (provider "i"))), Location.Var (-1,"_")])
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)

   
[<Fact(DisplayName="Logic Engine - Parsing - Expressions")>]
let testArithmetics () = 
  let provider  = makeProvider ()
  let ts        = psite cle provider false
  let actualT   = from_string (pterm ts cle provider engine.domainKeywords) """N + 1 - 2 * X"""
  let n  : Var = (0, "N") 
  let x  : Var = (1, "X")
  let t1 = Term.Func("_Plus",  [Term.Func("_key", [Term.Var n]); Term.Float 1.0 ]) 
  let t2 = Term.Func("_Times", [Term.Float 2.0; Term.Func("_key", [ Term.Var x ]) ])
  let expectedT = Term.Func("_Minus", [t1; t2])
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)
  
  let sub  = Substitution<SiteT, unit>.Create(n, Term.Float 4.0).Add(TVar x, Term.Float 2.0, CLE.empty)
  let t'   = Substitution.Apply(sub, actualT, CLE.empty) 
  let exp' = Term.ToExpression t' 
  let res  = exp' |> Microsoft.Research.CRNEngine.Expression.eval (fun t -> match t with Term.Float n -> n | _ -> failwith "Substitution application failed")
  Assert.Equal(1.0, res)

  
[<Fact(DisplayName="Logic Engine - Parsing - Predicates")>]
let testPreds () = 
  let provider  = makeProvider ()
  let ts        = psite cle provider false
  let n  : Var = (0, "N") 
  let x  : Var = (1, "X")
  
  let actualT   = from_string (ppredicate cle ts provider engine.domainKeywords) """N is 1"""
  let t1 = Term.Float 1.0
  let expectedT = Predicate.Pred("is", [Term.Var n; t1 ])
  Assert.Equal(expectedT |> Predicate<_>.ToString |> removeVarNumbers, 
               actualT   |> Predicate<_>.ToString |> removeVarNumbers)

  let actualT   = from_string (ppredicate cle ts provider engine.domainKeywords) """N is X + 1"""
  let t1 = Term.Func("_Plus",  [Term.Func("_key", [Term.Var x]); Term.Float 1.0 ]) 
  let expectedT = Predicate.Pred("is", [Term.Var n; t1 ])
  Assert.Equal(expectedT |> Predicate<_>.ToString |> removeVarNumbers, 
               actualT   |> Predicate<_>.ToString |> removeVarNumbers)


[<Fact(DisplayName="Logic Engine - Parsing - Clauses")>]
let testClauses () = 
  let provider  = makeProvider ()
  let ts     x  = psite cle x false
  let p  : Var = (0, "P")
  let q  : Var = (1, "Q")
  let r  : Var = (2, "Rate")
  
  let actualT   = from_string (pclause engine ts makeProvider) """reaction([P], Rate, Q) :- Rate is 1."""
  let expectedT = 
    Clause.Create(
      Predicate.Pred("reaction", [Term.TList [Term.Var p]; Term.Var r; Term.Var q ]),
      [ Pos <| Predicate.Pred("is", [Term.Var r; Float 1.0 ]) ] )
  Assert.Equal(expectedT |> Clause<_>.ToStringWith engine |> removeVarNumbers, 
               actualT   |> Clause<_>.ToStringWith engine |> removeVarNumbers)







[<Fact(DisplayName="Logic Engine - Parsing - CRNs")>]
let testCrn () = 
  let provider  = makeProvider ()
  let ts        = Parser.plookAhead (pname >>= fun x -> if x.Length > 0 && (x = "_" || System.Char.IsUpper (x.Chars 0)) then Parser.failParser "" else preturn ()) >>. pname
  
  // Single reaction CRN
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ X -> Y }"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet([1, Term.Var (0, "X")])
                                     ; Term.Func("_massActionRate", [Term.Float 1.0])
                                     ; Term.Func("",[])
                                     ; Term.TMSet([1, Term.Var (1, "Y")])
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)


  // Mass action rate
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ X ->{k} Y }"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, Term.Var (0, "X")]
                                     ; Term.Func("_massActionRate", [Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [1, Term.Var (1, "Y")]
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)

  // Functional rate 
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ X ->[[k]] Y }"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, Term.Var (0, "X")]
                                     ; Term.Func("_functionalRate", [Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [1, Term.Var (1, "Y")]
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)

  // Ground reaction
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ a ->{k} b }"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["a", Location.Var (-1, "_")]) ]
                                     ; Term.Func("_massActionRate", [Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["b", Location.Var (-1, "_")]) ]
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)

  // multisets
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ 2a + 3b ->{k} c + 4d}"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [ 2, Term.Pat (Pattern.Inner ["a", Location.Var (-1, "_")])
                                                  ; 3, Term.Pat (Pattern.Inner ["b", Location.Var (-1, "_")]) ]
                                     ; Term.Func("_massActionRate", [Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["c", Location.Var (-1, "_")]) 
                                                   4, Term.Pat (Pattern.Inner ["d", Location.Var (-1, "_")]) ]
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)


  // catalytic reaction
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ a ~ b ->{k} c }"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet [1, Term.Pat (Pattern.Inner ["a", Location.Var (-1, "_")]) ]
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["b", Location.Var (-1, "_")]) ]
                                     ; Term.Func("_massActionRate", [Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["c", Location.Var (-1, "_")]) ]
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)

  // reversible reaction
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ a <->{k}[[l]] b }"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["a", Location.Var (-1, "_")]) ]
                                     ; Term.Func("_massActionRate", [Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ]) ])
                                     ; Term.Func("_functionalRate", [Term.Func("_key", [Term.Pat (Pattern.Inner ["l", Location.Var (-1, "_")]) ]) ])
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["b", Location.Var (-1, "_")]) ]
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)
  
  // rate expression
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ a ->{k + 2 * X} b }"""
  let expectedT = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["a", Location.Var (-1, "_")]) ]
                                     ; Term.Func("_massActionRate", [Term.Func("_Plus", [ Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ])
                                                                                        ; Term.Func("_Times", [Term.Float 2.0 ; Term.Func("_key", [Term.Var (0, "X")])])    ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["b", Location.Var (-1, "_")]) ]
                                    ]) 
                    |> List.singleton 
                    |> Term.TCRN
  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)

  // multiple reactions
  let actualT   = from_string (pterm ts CLE.empty provider engine.domainKeywords) """{ | a ->{k + 2 * X} b 
                                                       | c ->[log([z])] 2d + 3e}"""
  let reaction1 = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["a", Location.Var (-1, "_")]) ]
                                     ; Term.Func("_massActionRate", [Term.Func("_Plus", [ Term.Func("_key", [Term.Pat (Pattern.Inner ["k", Location.Var (-1, "_")]) ])
                                                                                        ; Term.Func("_Times", [Term.Float 2.0 ; Term.Func("_key", [Term.Var (0, "X")])])    ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["b", Location.Var (-1, "_")]) ] ]) 
  let reaction2 = Term.Func ("_rxn", [ Term.TMSet []
                                     ; Term.TMSet [1, Term.Pat (Pattern.Inner ["c", Location.Var (-1, "_")]) ]
                                     ; Term.Func("_functionalRate", [Term.Func("_Log", [ Term.Func("_key", [Term.Pat (Pattern.Inner ["z", Location.Var (-1, "_")]) ]) ]) ])
                                     ; Term.Func("",[])
                                     ; Term.TMSet [2, Term.Pat (Pattern.Inner ["d", Location.Var (-1, "_")])  
                                                   3, Term.Pat (Pattern.Inner ["e", Location.Var (-1, "_")]) ] ]) 
  let expectedT = Term.TCRN [ reaction1; reaction2 ]

  Assert.Equal(expectedT |> Term.ToString |> removeVarNumbers, 
               actualT   |> Term.ToString |> removeVarNumbers)