module deviceEnumerationTests

open Xunit
open FsUnit
open Microsoft.Research.CRNEngine


open RulesDSD.Syntax
open Microsoft.Research.GEC
open Microsoft.Research.GEC.LogicGEC

let db = """////////////////////////////
////////////////////////////
// Knowledge Graph encoding
////////////////////////////
////////////////////////////

////////////////
// devices list
////////////////
device(d1_P76_CFP,          "GCTCTTCAGAAAC").
device(d2_P81_YFP,          "GCTCTTCAACGAC").
device(d3_r0011_b0034_luxR, "GCTCTTCACATAA").
device(d4_r0011_S175_lasR,  "GCTCTTCATTTAATTGTGA").
device(r0040_b0033_cinR,    "GCTCTTCATTTAATTGTGG").
device(d5_PSB3K3_Dest,      "AATTTTGTGTCGCCCTTGA").

// linkers
device(linker1, "GCTCTTCAG").
device(linker2, "GCTCTTCAA").
device(linker3, "GCTCTTCAC").
device(linker4, "GCTCTTCAT").

//////////////////////////////
// drop-down menus selection
//////////////////////////////
menu("component1", [ d1_P76_CFP
                   ; linker1  ]).
                 
menu("component2", [ d2_P81_YFP
                   ; linker2  ]).
                 
menu("component3", [ d3_r0011_b0034_luxR
                   ; linker3
                   ; r0040_b0033_cinR  ]).
                 
menu("component4", [ d4_r0011_S175_lasR
                   ; linker4          ]).
                 
menu("backbone",   [ d5_PSB3K3_Dest ]).

///////////////
// annotations (assuming they are provided by querying the Knowledge Graph)
///////////////
annotateSeq("GCTCTTCAGAAAC",         <sapl_gaa    P76          scar34scar eCFP B0015   sapl_acg_RC>). // 1_P76_CFP
annotateSeq("GCTCTTCAACGAC",         <sapl_acg    P81          scar34scar eYFP L3S2P2s sapl_cat_RC>). // 2_P81_YFP
annotateSeq("GCTCTTCACATAA",         <sapl_cat    Plac_R0011   scar33scar LuxR B0015   sapl_ttt_RC>). // 3_r0011_b0034_luxR
annotateSeq("GCTCTTCATTTAATTGTGA",   <sapl_ttt    Plac_R0011   scarS175   LasR B0015   sapl_ggt_RC>). // 4_r0011_S175_lasR
annotateSeq("GCTCTTCATTTAATTGTGG",   <sapl_cat    Ptet_R0040   B0033      CinR B0015   sapl_ttt_RC>). // r0040_b0033_cinR
annotateSeq("AATTTTGTGTCGCCCTTGA",   <sapl_gaa_RC levansucrase sapl_ggt>).                            // PSB3K3_Dest

annotateSeq("GCTCTTCAG", <sapl_gaa Linker1 sapl_acg_RC>).  // Linker 1 
annotateSeq("GCTCTTCAA", <sapl_acg Linker2 sapl_cat_RC>).  // Linker 2
annotateSeq("GCTCTTCAC", <sapl_cat Linker3 sapl_ttt_RC>).  // Linker 3 
annotateSeq("GCTCTTCAT", <sapl_ttt Linker4 sapl_ggt_RC>).  // Linker 4 

/////////
// parts
////////
// Restriction site
part(sapl_gaa, "RestrictionSite"). // add sequence to part
part(sapl_acg, "RestrictionSite").
part(sapl_cat, "RestrictionSite").
part(sapl_ttt, "RestrictionSite").
part(sapl_ggt, "RestrictionSite").

part(sapl_acg_RC, "RestrictionSite").
part(sapl_cat_RC, "RestrictionSite").
part(sapl_ttt_RC, "RestrictionSite").
part(sapl_ggt_RC, "RestrictionSite").
part(sapl_gaa_RC, "RestrictionSite").

// Promoter
part(P76,        "Promoter"). // HSL-regulated
part(P81,        "Promoter"). // HSL-regulated
part(Plac_R0011, "Promoter").
part(Ptet_R0040, "Promoter"). 

// HSL-regulated 
property(P76, "HSL-regulated").
property(P81, "HSL-regulated").

// intended
// luxr binds p76
// lasr binds p81
// x-talk
// luxr binds p81
// lasr binds p76
// CinR binds p81
// CinR binds p76

// Linker
part(Linker1, "Linker").
part(Linker2, "Linker").
part(Linker3, "Linker").
part(Linker4, "Linker").

// CDS
part(eCFP,         "CDS").
part(eYFP,         "CDS").
part(LuxR,         "CDS").    // receiver
part(LasR,         "CDS").    // receiver
part(CinR,         "CDS").
part(levansucrase, "CDS").    // missing
// missing: c6 c12
initials("c6").
initials("c12").

// LuxR
interaction("production", "LuxR", "Lux").
reagent("Lux", "chemical").
property("Lux", "HSL-regulator").

interaction("bind", "Lux", "P76").
interaction("bind", "Lux", "P81").   // crosstalk

// LasR
interaction("production", "LasR", "Las").
reagent("Las", "chemical").
property("Las", "HSL-regulator").
interaction("bind", "Las", "P81").
interaction("bind", "Las", "P76").   // crosstalk

// CinR
interaction("production", "CinR", "Cin").
reagent("Cin", "chemical").
property("Cin", "HSL-regulator").
interaction("bind", "Cin", "P81").   // crosstalk
interaction("bind", "Cin", "P76").   // crosstalk

// YFP
interaction("production", "eYFP", "YFP").
reagent("YFP", "chemical").
property("YFP", "fluorophore").

// CFP
interaction("production", "eCFP", "CFP").
reagent("CFP", "chemical").
property("CFP", "fluorophore").

// RBS
part(scar34scar, "RBS").
part(scar33scar, "RBS").
part(scarS175,   "RBS"). 
part(B0033,      "RBS").

// Terminator
part(B0015,   "Terminator").  
part(L3S2P2s, "Terminator").

/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////

///////////////////////////
///////////////////////////
// Combinatorial assembly
///////////////////////////
///////////////////////////

// Utilities
parts(Device, Parts) :-
    device(Device, Sequence),
    annotateSeq(Sequence, Parts).

produces(Device, Molecule) :- 
    parts(Device, Parts), Parts = C[ P ]
    type(P, cds(codes(Molecule))),
    //interaction("production", P, Molecule),
    reagent(Molecule, "chemical").

producesKind(Device, M, TypeOfMolecule) :-
    produces(Device, M),
    property(M, TypeOfMolecule).

isLinker(Device) :-
  parts(Device, Parts),
  member(P, Parts),
  part(P, "Linker").

binds(Device, Molecule) :- 
    parts(Device, Ps),
    member(Part, Ps),
    interaction("bind", Molecule, Part).

////////////////////////////////////////////
// Condition 1: not all devices are linkers 
////////////////////////////////////////////
consecutiveLinkers([C1; C2; C3; C4]) :- isLinker(C1), isLinker(C2), isLinker(C3), isLinker(C4).

(* recursive version:
consecutiveLinkers([]).
consecutiveLinkers([D # Rest ]) :- isLinker(D), consecutiveLinkers(Rest). *)

////////////////////////////////////////////
// Condition 2: must have a fluorophore
////////////////////////////////////////////
hasFluorophore(Devices) :-
    member(D, Devices),
    producesKind(D, M, "fluorophore").

///////////////////////////////////////////////////
// Condition 3: don't produce the same fluorophore
// (Negate the condition below in the code)
///////////////////////////////////////////////////
produceSameFluorophore(Devices) :-
    member(D1, Devices),
    member(D2, Devices),
    not (D1 = D2),
    produces(D1, M),
    property(M, "fluorophore"),
    produces(D2, M).
    
///////////////////////////////////////////////////
// Condition 4: all non-constitutive promoters must be regulated by some device
// (Negate the condition below in the code)
///////////////////////////////////////////////////
hasHSLRegulator(Devices, M, P) :- 
    member(D, Devices),
    producesKind(D, M', "HSL-regulator"),
    interaction("bind", M', P),
    M = M'.

isHSLRegulated(Devices, M, P') :-
    member(D, Devices),
    parts(D, Parts),
    member(P', Parts),
    property(P', "HSL-regulated"),
    interaction("bind", M, P'),
    P = P'.

uninducedHSL(Devices) :-
   hasHSLRegulator(Devices, M, _),
   not isHSLRegulated(Devices, M, _).

//uninducedHSL(Devices) :- not isHSLRegulated(Devices, _, _).
uninducedHSL(Devices) :-
    isHSLRegulated(Devices, _, P),
    not hasHSLRegulator(Devices, _, P).

///////////////////////////////////////////////////
// Condition 5: no protein is regulated twice
// (Negate the condition below in the code)
/////////////////////////////////////////////////// 
produceSameProtein(Devices, M) :-
    member(D1, Devices),
    member(D2, Devices),
    not (D1 = D2),
    producesKind(D1, M', "HSL-regulator"),
    produces(D2, M'),
    M = M'.

///////////////////////////////////////////////////
// Condition 6: no two proteins regulated the same promoter
// (Negate the condition below in the code)
/////////////////////////////////////////////////// 
regulateSamePromoter(Devices) :-
    member(D1, Devices),
    producesKind(D1, M1, "HSL-regulator"),
    member(D2, Devices),
    producesKind(D2, M2, "HSL-regulator"),
    not (M1 = M2),
    member(D, Devices),
    binds(D, M1),
    binds(D, M2).

///////////////////////////////////////////////////
// Condition 7: no promoter produces different proteins 
// (Negate the condition below in the code)
/////////////////////////////////////////////////// 
samePromoterDifferentProtein(Devices) :-
    member(D1, Devices), parts(D1, Ps1),
    member(D2, Devices), parts(D2, Ps2),
    member(P, Ps1), part(P, "Promoter"),
    member(P, Ps2),
    member(C1, Ps1), part(C1, "Promoter"),
    member(C2, Ps2), part(C2, "Promoter"),
    produces(C1, M1),
    produces(C2, M2),
    not(M1 = M2).

/////////////////////////////////////////
// Main combinatorial assembly algorithm
/////////////////////////////////////////
find_device(C, X) :-
    
    // populate menus
    menu("component1", Cs1), member(C1, Cs1),
    menu("component2", Cs2), member(C2, Cs2),
    menu("component3", Cs3), member(C3, Cs3),
    menu("component4", Cs4), member(C4, Cs4),
    menu("backbone",   Bkb), member(B,  Bkb),
        
    Devices = [C1; C2; C3; C4],       (* 24 combinations *)
    // 1) not all devices are linkers 
    not consecutiveLinkers(Devices),  (* filtered: linker1 linker2 linker3 linker4 d5_PSB3K3_Dest *)

    // 2) a fluorophore must be produced
    hasFluorophore(Devices),          (* filtered: linker1 linker2 d3_r0011_b0034_luxR d4_r0011_S175_lasR d5_PSB3K3_Dest
                                                   linker1 linker2 d3_r0011_b0034_luxR linker4 d5_PSB3K3_Dest
                                                   linker1 linker2 linker3 d4_r0011_S175_lasR d5_PSB3K3_Dest
                                                   linker1 linker2 r0040_b0033_cinR d4_r0011_S175_lasR d5_PSB3K3_Dest
                                                   linker1 linker2 r0040_b0033_cinR linker4 d5_PSB3K3_Dest *)
    
    // 3) all fluorophores have a different colour
    not produceSameFluorophore(Devices), (* no device is filtered *)

    // 4) AHL promoter implies AHL receiver (possibly viceversa)
    not uninducedHSL(Devices),        (* filtered: linker1    d2_P81_YFP linker3 linker4 d5_PSB3K3_Dest
                                                   d1_P76_CFP linker2    linker3 linker4 d5_PSB3K3_Dest
                                                   d1_P76_CFP d2_P81_YFP linker3 linker4 d5_PSB3K3_Dest *)
    
    // 5) no protein is regulated twice
    // no two parts produce the same molecule
    not produceSameProtein(Devices),  (* no device is filtered *)
    
    // 6) no promoter is regulated twice
    // not regulateSamePromoter(Devices), (* disabled *)

    // 7) no promoter produces two different proteints
    not samePromoterDifferentProtein(Devices),
    
    X = C[<C1  C2 C3 C4 B>]. 
    (*final list, 9 combinations: 
        linker1    d2_P81_YFP r0040_b0033_cinR    d4_r0011_S175_lasR d5_PSB3K3_Dest
        linker1    d2_P81_YFP d3_r0011_b0034_luxR d4_r0011_S175_lasR d5_PSB3K3_Dest
        d1_P76_CFP linker2    r0040_b0033_cinR    d4_r0011_S175_lasR d5_PSB3K3_Dest
        d1_P76_CFP linker2    d3_r0011_b0034_luxR d4_r0011_S175_lasR d5_PSB3K3_Dest
        d1_P76_CFP d2_P81_YFP r0040_b0033_cinR    d4_r0011_S175_lasR d5_PSB3K3_Dest
        d1_P76_CFP d2_P81_YFP d3_r0011_b0034_luxR d4_r0011_S175_lasR d5_PSB3K3_Dest
        linker1    d2_P81_YFP r0040_b0033_cinR    linker4            d5_PSB3K3_Dest
        linker1    d2_P81_YFP linker3             d4_r0011_S175_lasR d5_PSB3K3_Dest
        linker1    d2_P81_YFP d3_r0011_b0034_luxR linker4            d5_PSB3K3_Dest
        d1_P76_CFP linker2    r0040_b0033_cinR    linker4            d5_PSB3K3_Dest
        d1_P76_CFP linker2    linker3             d4_r0011_S175_lasR d5_PSB3K3_Dest
        d1_P76_CFP linker2    d3_r0011_b0034_luxR linker4            d5_PSB3K3_Dest
        d1_P76_CFP d2_P81_YFP r0040_b0033_cinR    linker4            d5_PSB3K3_Dest
        d1_P76_CFP d2_P81_YFP linker3             d4_r0011_S175_lasR d5_PSB3K3_Dest
        d1_P76_CFP d2_P81_YFP d3_r0011_b0034_luxR linker4            d5_PSB3K3_Dest *)

reaction([P], "found", D) :- P = C[<solution>], find_device(C, D)."""

///////////////////////////////////
///////////////////////////////////
///////////////////////////////////
///////////////////////////////////
///////////////////////////////////
///////////////////////////////////

let part1 : LogicGEC.Part = 
  { name  = Term.Const "A"
    type_ = Term.Const "promoter" } 

let part2 : LogicGEC.Part = 
  { name  = Term.Const "B"
    type_ = Term.Const "RBS" }

let toTerm (e:Element) = Term.Pat (Pattern.Inner [e, Location.wildcard])
let toDB = List.map Clause.Create >> toProgram

/// simple database with just two parts
let db1 = toDB [ Pred("part", [toTerm (Part part1)]) 
                 Pred("part", [toTerm (Part part2)]) ]

let part1': LogicGEC.Part = 
  { name  = Term.Const "A2"
    type_ = Term.Const "promoter" }

let part1'': LogicGEC.Part = 
  { name  = Term.Const "dummy"
    type_ = Term.Const "dummy" }

let part3 : LogicGEC.Part = 
  { name  = Term.Const "C"
    type_ = Term.Const "CDS" }

let part4 : LogicGEC.Part = 
  { name  = Term.Const "D"
    type_ = Term.Const "terminator" }

/// simple database with just five parts: a promoter, rbs, cds, terminator, and a "dummy" part that has the same properties as the promoter
let db2 = toDB [ Pred("part", [toTerm (Part part1')]) 
                 Pred("part", [toTerm (Part part1'')]) 
                 Pred("part", [toTerm (Part part2)]) 
                 Pred("part", [toTerm (Part part3)]) 
                 Pred("part", [toTerm (Part part4)])
                 Pred("neg",   [part1'.name;  Term.Const "c6" ]) 
                 Pred("neg",   [part1''.name; Term.Const "c6" ])
                 Pred("codes", [part1''.name; Term.Const "c7" ])
                 Pred("codes", [part3.name;   Term.Const "c6" ]) ]


[<Fact(DisplayName="Logic GEC - device enumeration - Custom constraints")>]
let testDevicesCustom() = 
  let crn = "" |> Crn.from_string
  let result = crn.simulate_case ()
  // TODO
  ()

[<Fact(DisplayName="Logic GEC - device enumeration - match part")>]
let testDeviceEnumOnePart () = 
  // query: find the device < X >, where X is any one part in the database
  let x       = Term.Var (0, "X")
  let query   = [ Device [Part {name = x; type_ = Term.wildcard }] ]

  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db1 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> Assert.Equal(sols.Length, 2)
                 let sol1   = sols.Head
                 let found1 = sol1.Apply(x, LogicGEC.cle) 
                 Assert.Equal (Term.Const "A", found1)
                 let sol2  = sols.Tail.Head
                 let found2 = sol2.Apply(x, LogicGEC.cle) 
                 Assert.Equal (Term.Const "B", found2)


[<Fact(DisplayName="Logic GEC - device enumeration - 2 parts device")>]
let testDeviceEnumTwoParts () = 
  // query: find the device <X Y>, where X and Y are any two distinct parts in the database
  let x       = Element.Var (0, "X")
  let y       = Element.Var (1, "Y")
  let query   = [ Device [ x; y]
                ; Constraint (Neg (Pred ("=", [toTerm x; toTerm y])))]

  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db1 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> Assert.Equal(2, sols.Length)
                 let p1 = Part part1
                 let p2 = Part part2

                 let sol1   = sols.Head
                 let x1 = sol1.Apply(x, LogicGEC.cle) 
                 let y1 = sol1.Apply(y, LogicGEC.cle) 
                 let d1 = [x1; y1]
                 
                 let sol2  = sols.Tail.Head
                 let x2 = sol2.Apply(x, LogicGEC.cle) 
                 let y2 = sol2.Apply(y, LogicGEC.cle) 
                 let d2 = [x2; y2]
                 
                 let exp1 = [p1; p2]
                 let exp2 = [p2; p1]
                 Assert.True(  (d1 = exp1 && d2 = exp2)
                            || (d1 = exp2 && d2 = exp1) )

[<Fact(DisplayName="Logic GEC - device enumeration - match part type")>]
let testDeviceEnumOnePartType () = 
  // query: find the device < X : RBS >, where X is an RBS
  let x     = Term.Var (0, "X")
  let y     = Term.Const "RBS"
  let query = [ LogicGEC.Instruction.Device [Part { name = x; type_ = y }] ]
  
  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db1 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> Assert.Equal(sols.Length, 1)
                 let sol1   = sols.Head
                 let found1 = sol1.Apply(x, LogicGEC.cle) 
                 Assert.Equal (Term.Const "B", found1)

[<Fact(DisplayName="Logic GEC - device enumeration - 2 parts device with types")>]
let testDeviceEnumTwoPartTypes () = 
  // query: find the device <X:promoter Y:RBS>
  let v1    = Term.Var (0, "X")
  let v2    = Term.Var (1, "Y")
  let x1    = Part {name = v1; type_ = Term.Const "promoter" }
  let x2    = Part {name = v2; type_ = Term.Const "RBS"      }
  let query = [ LogicGEC.Instruction.Device [x1; x2] ]

  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db1 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> 
      Assert.Equal(1, sols.Length)
      let sol1 = sols.Head
      let x1   = sol1.Apply(x1, LogicGEC.cle) |> toTerm
      let y1   = sol1.Apply(x2, LogicGEC.cle) |> toTerm
      let d    = [x1; y1]
      
      let p1  = toTerm (Part part1)
      let p2  = toTerm (Part part2)
      let exp = [p1; p2]
      
      let test = exp = d
      Assert.True(test)

[<Fact(DisplayName="Logic GEC - device enumeration - part with type and property")>]
let testDeviceEnumOnePartTypeProperty () = 
  // query: find the device <X:dummy<neg(P)>>
  let vp   = Term.Var (0, "X")
  let v2   = Term.Var (1, "P")
  let prom = Part { Part.CreateByType("dummy")   with name = vp } // ;  props = Term.TList [Term.Func("neg", [v2])] }

  let query = [ LogicGEC.Instruction.Device [prom] ]

  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db2 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> 
      Assert.Equal(1, sols.Length)
      let sol1 = sols.Head
      let fp   = sol1.Apply(vp, LogicGEC.cle)
      
      Assert.Equal(Term.Const "dummy", fp)

[<Fact(DisplayName="Logic GEC - device enumeration - part with property")>]
let testDeviceEnumOnePartProperty () = 
  // query: find the device <X:_<neg(P)>>, where _<neg(P)> means any type containing property neg(P)
  let x    = Term.Var (0, "X")
  let p    = Term.Var (1, "P")
  let part = Part.Create(x, Term.wildcard) |> Part

  let query = [ LogicGEC.Instruction.Device     [part]
              ; LogicGEC.Instruction.Constraint (Pos (Predicate.Pred ("neg", [ x; p]))) ]

  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db2 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> 
      Assert.Equal(2, sols.Length)
      let sol1 = sols.Head
      let s1   = sol1.Apply(x, LogicGEC.cle)

      let sol2 = sols.Tail.Head
      let s2   = sol2.Apply(x, LogicGEC.cle)
      
      let actual   = Set.ofList [s1;s2]
      let expected = Set.ofList [part1'.name; part1''.name] 
      let b        = actual = expected

      Assert.True(b)
      

[<Fact(DisplayName="Logic GEC - device enumeration - self-repressilator")>]
let testDeviceEnumSelfRepressilator () = 
  // query: find the device <X:promoter<neg(P)> _:RBS _:CDS<neg(P)> _:terminator>
  let vp   = Term.Var (0, "X")
  let vr   = Term.Var (0, "Y")
  let vc   = Term.Var (0, "W")
  let vt   = Term.Var (0, "Z")
  let v2   = Term.Var (1, "P")
  let prom = Part { Part.CreateByType("promoter")   with name = vp } //;  props = Term.TList [Term.Func("neg", [v2] )] }
  let rbs  = Part { Part.CreateByType("RBS")        with name = vr } 
  let cds  = Part { Part.CreateByType("CDS")        with name = vc } //;  props = Term.TList [Term.Func("codes", [v2]) ] }
  let ter  = Part { Part.CreateByType("terminator") with name = vt }

  let query = [ LogicGEC.Instruction.Device [prom; rbs; cds; ter] ]

  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db2 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> 
      Assert.Equal(1, sols.Length)
      let sol1 = sols.Head
      let fp   = sol1.Apply(vp, LogicGEC.cle)
      let fr   = sol1.Apply(vr, LogicGEC.cle)
      let fc   = sol1.Apply(vc, LogicGEC.cle)
      let ft   = sol1.Apply(vt, LogicGEC.cle)
      
      Assert.Equal(Term.Const "A2", fp)
      Assert.Equal(Term.Const "B", fr)
      Assert.Equal(Term.Const "C", fc)
      Assert.Equal(Term.Const "D", ft)


[<Fact(DisplayName="Logic GEC - device enumeration - self-repressilators")>]
let testDeviceEnumSelfRepressilators () = 
  // query: find the device <_:promoter<neg(P)> _:RBS _:CDS<codes(P)> _:terminator>
  let x    = Term.Var (0, "X")
  let y    = Term.Var (0, "Y")
  let w    = Term.Var (0, "W")
  let z    = Term.Var (0, "Z")
  let p    = Term.Var (1, "P")
  let prom = Part ( Part.Create(x, Term.wildcard) ) // ByProperties([Term.Func("neg", [p] )]) with name = x }
  let rbs  = Part { Part.CreateByType("RBS")        with name = y } 
  let cds  = Part { Part.CreateByType("CDS")        with name = w } //;  props = Term.TList [Term.Func("codes", [p]) ] }
  let ter  = Part { Part.CreateByType("terminator") with name = z }

  let query = [ LogicGEC.Instruction.Device [prom; rbs; cds; ter]
              ; LogicGEC.Instruction.Constraint (Pos (Predicate.Pred ("neg",   [ x; p ]))) 
              ; LogicGEC.Instruction.Constraint (Pos (Predicate.Pred ("codes", [ w; p ]))) ]

  match LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle db2 query with 
  | None 
  | Some []   -> failwith "No solution found"
  | Some sols -> 
      Assert.Equal(2, sols.Length)
      let sol1 = sols.Head
      let fp1  = sol1.Apply(x,  LogicGEC.cle)
      let fr1  = sol1.Apply(y,  LogicGEC.cle)
      let fc1  = sol1.Apply(w, LogicGEC.cle)
      let ft1  = sol1.Apply(z, LogicGEC.cle)
      
      let sol2 = sols.Tail.Head
      let fp2  = sol2.Apply(x,  LogicGEC.cle)
      let fr2  = sol2.Apply(y,  LogicGEC.cle)
      let fc2  = sol2.Apply(w, LogicGEC.cle)
      let ft2  = sol2.Apply(z, LogicGEC.cle)

      Assert.Equal(Term.Const "B", fr1)
      Assert.Equal(Term.Const "C", fc1)
      Assert.Equal(Term.Const "D", ft1)

      Assert.Equal(Term.Const "B", fr2)
      Assert.Equal(Term.Const "C", fc2)
      Assert.Equal(Term.Const "D", ft2)

      let proms    = [fp1; fp2] |> Set.ofList
      let expected = [part1'.name; part1''.name] |> Set.ofList
      let b        = proms = expected 
      Assert.True(b)




[<Fact(DisplayName="Logic GEC - device enumeration - receiver")>]
let testParseRepressilator() = 
  let code1 = 
    """directive simulation {final = 100000.0; points = 1000}
directive rules {
  """
  
  let parts = """
  // parts
  part(r0062::prom). pos(r0062, luxR:c6,  0.01, 0.01, 0.01).  // luxR:c6 is a complex
  part(r0090::prom). pos(r0090, lasR:c12, 0.01, 0.01, 0.01).
  part(r0011::prom). neg(r0011, lacI,     0.01, 0.01, 0.01).
  part(r0040::prom). neg(r0040, tetR,     0.01, 0.01, 0.01).
  part(r0051::prom). neg(r0051, cI,       0.01, 0.01, 0.01).
  
  part(b0033::rbs).
  part(b0034::rbs).
  
  part(e0040::pcr). codes(e0040, gfp,  0.01).
  part(c0062::pcr). codes(c0062, luxR, 0.01).
  part(c0079::pcr). codes(c0079, lasR, 0.01).
  part(c0012::pcr). codes(c0012, lacI, 0.01).
  part(c0040::pcr). codes(c0040, tetR, 0.01).
  
  part(b0015::ter).
  """
  
  let code2 = """
  // simplified regulation lookups
  pos(Part, TF) :- pos(Part, TF, _, _, _).
  neg(Part, TF) :- neg(Part, TF, _, _, _).
  codes(Part, Pr) :- codes(Part, Pr, _).
  
  // interactions
  bind(lacI,iptg).
  bind(tetR,aTc).
  bind(luxR,c6).
  bind(lasR,c12).
  synthesise(luxI,c6).
  synthesise(lasI,c12).
  transport(cell,c6).
  transport(cell,c12).

  ////////////////////////
  // Semantics
  ////////////////////////
  // positive regulation
  reactions(S, CRN) :-
    S = C[ Prom::prom  Rbs::rbs Pcr::cds Ter::ter ], 
    pos(Prom, TF, Kb, Ku, Kt),
    G = gene(<Prom Rbs Pcr Ter>), 
    M = mRNA(<Rbs Pcr>),
    CRN = { TF + G <->{Kb}{Ku} TF:G
          | G:TF ->{Kt} G:TF + M    }.
  
  
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
  }
  
  ////////////////////////
  // Genetic instructions
  ////////////////////////
  | <prom rbs X1::pcr ter> 
  | <X2::prom rbs X3::pcr ter>
  | codes(X1, R) 
  | pos(X2, R:S) 
  | codes(X3, gfp)
  | bind(R, S)  
"""
  let code = code1 + parts + code2

  let parser    = Microsoft.Research.GEC.Program.parse 
  match Parser.from_string parser code with 
  | Program.t.LogicGec bundle ->
    // assign fresh variables to wildcard parts (e.g. prom is a part with type prom and name "_")
    let maxVar = 
      bundle.program
      |> List.map fvi 
      |> Set.unionMany
      |> Set.map (fun x -> 
        match x with 
        | Variable.SVar ((n,_), _)
        | Variable.IVar ((n,_), _)
        | Variable.LVar (n,_)
        | Variable.TVar (n,_) -> n)
      |> Set.maxElement
      |> ref
    
    let fullProgram = 
      bundle.program 
      |> List.map (Instruction.Map (fun e ->
        match e with 
        | Element.Var _ -> e
        | Element.Part p -> if p.name = Term.wildcard 
                              then 
                                let x = !maxVar
                                maxVar := !maxVar + 1
                                let freshVar = Term.Var(x, sprintf "X_%i" x)
                                Element.Part { p with name = freshVar}
                              else e))

    let solutions = LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle bundle.rules fullProgram
    Assert.True(solutions.IsSome)
    
    let devs = 
      fullProgram 
      |> List.filter (fun i -> match i with Instruction.Device _ -> true | _ -> false)
    
    let actualDevices = 
      solutions 
      |> Option.get
      |> List.map (fun (RulesDSD.Substitution.Substitution.Sub s) -> 
        devs
        |> List.map (Instruction.Map (cle.applySub s))
        |> List.sort)

      |> List.distinct
    
    (*
    let foundDevices = 
      solutions |> List.map (fun sigma -> 
        devs 
        |> List.map (fun d -> d) )
    *)
      
    let expectedText = """}
     | <r0011 b0034 c0062 b0015>
     | <r0062 b0034 e0040 b0015>"""

    let expectedCode = code1 + parts + expectedText
    let expectedDevice = 
      match Parser.from_string parser expectedCode with 
      | Program.t.LogicGec bundle ->
        bundle.program
        |> List.sort
      | _ -> failwith ""

    Assert.Contains(expectedDevice, actualDevices )

  | Program.t.ClassicGec _ -> failwith "Unexpected Classic GEC program."



[<Fact(DisplayName="Logic GEC - device enumeration - receiver with initials")>]
let testParseReceiverWithInitials() = 
  let code1 = 
    """directive simulation {final = 100000.0; points = 1000}
directive rules {
  """
  
  let parts = """
  // parts
  part(r0062::prom). pos(r0062, luxR:c6,  0.01, 0.01, 0.01).  // luxR:c6 is a complex
  part(r0090::prom). pos(r0090, lasR:c12, 0.01, 0.01, 0.01).
  part(r0011::prom). neg(r0011, lacI,     0.01, 0.01, 0.01).
  part(r0040::prom). neg(r0040, tetR,     0.01, 0.01, 0.01).
  part(r0051::prom). neg(r0051, cI,       0.01, 0.01, 0.01).
  
  part(b0033::rbs).
  part(b0034::rbs).
  
  part(e0040::pcr). codes(e0040, gfp,  0.01).
  part(c0062::pcr). codes(c0062, luxR, 0.01).
  part(c0079::pcr). codes(c0079, lasR, 0.01).
  part(c0012::pcr). codes(c0012, lacI, 0.01).
  part(c0040::pcr). codes(c0040, tetR, 0.01).
  
  part(b0015::ter).
  """
  
  let code2 = """
  // simplified regulation lookups
  pos(Part, TF) :- pos(Part, TF, _, _, _).
  neg(Part, TF) :- neg(Part, TF, _, _, _).
  codes(Part, Pr) :- codes(Part, Pr, _).
  
  // interactions
  bind(lacI,iptg).
  bind(tetR,aTc).
  bind(luxR,c6).
  bind(lasR,c12).
  synthesise(luxI,c6).
  synthesise(lasI,c12).
  transport(cell,c6).
  transport(cell,c12).

  ////////////////////////
  // Semantics
  ////////////////////////
  // positive regulation
  reactions(S, CRN) :-
    S = C[ Prom::prom  Rbs::rbs Pcr::cds Ter::ter ], 
    pos(Prom, TF, Kb, Ku, Kt),
    G = gene(<Prom Rbs Pcr Ter>), 
    M = mRNA(<Rbs Pcr>),
    CRN = { TF + G <->{Kb}{Ku} TF:G
          | G:TF ->{Kt} G:TF + M    }.
  
  
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
  }
  
  ////////////////////////
  // Genetic instructions
  ////////////////////////
  | 10 <prom rbs X1::pcr ter> 
  | 10 <X2::prom rbs X3::pcr ter>
  | codes(X1, R) 
  | pos(X2, R:S) 
  | codes(X3, gfp)
  | bind(R, S)  
"""
  let code = code1 + parts + code2

  let parser    = Microsoft.Research.GEC.Program.parse 
  match Parser.from_string parser code with 
  | Program.t.LogicGec bundle ->
    // assign fresh variables to wildcard parts (e.g. prom is a part with type prom and name "_")
    let maxVar = 
      bundle.program
      |> List.map fvi 
      |> Set.unionMany
      |> Set.map (fun x -> 
        match x with 
        | Variable.SVar ((n,_), _)
        | Variable.IVar ((n,_), _)
        | Variable.LVar (n,_)
        | Variable.TVar (n,_) -> n)
      |> Set.maxElement
      |> ref
    
    let fullProgram = 
      bundle.program 
      |> List.map (Instruction.Map (fun e ->
        match e with 
        | Element.Var _ -> e
        | Element.Part p -> if p.name = Term.wildcard 
                              then 
                                let x = !maxVar
                                maxVar := !maxVar + 1
                                let freshVar = Term.Var(x, sprintf "X_%i" x)
                                Element.Part { p with name = freshVar}
                              else e))

    let solutions = LogicGEC.enumerateDeviceSubstitutions LogicGEC.cle bundle.rules fullProgram
    Assert.True(solutions.IsSome)
    
    let devs = 
      fullProgram 
      |> List.filter (fun i -> match i with Instruction.Initial _ -> true | _ -> false)
    
    let actualDevices = 
      solutions 
      |> Option.get
      |> List.map (fun (RulesDSD.Substitution.Substitution.Sub s) -> 
        devs
        |> List.map (Instruction.Map (cle.applySub s))
        |> List.sort)

      |> List.distinct
    
    (*
    let foundDevices = 
      solutions |> List.map (fun sigma -> 
        devs 
        |> List.map (fun d -> d) )
    *)
      
    let expectedText = """}
     | 10 <r0011 b0034 c0062 b0015>
     | 10 <r0062 b0034 e0040 b0015>"""

    let expectedCode = code1 + parts + expectedText
    let expectedDevice = 
      match Parser.from_string parser expectedCode with 
      | Program.t.LogicGec bundle ->
        bundle.program
        |> List.sort
      | _ -> failwith ""

    Assert.Contains(expectedDevice, actualDevices )

  | Program.t.ClassicGec _ -> failwith "Unexpected Classic GEC program."