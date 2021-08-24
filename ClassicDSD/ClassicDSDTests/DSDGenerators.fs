// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.DSDGenerators

open FsCheck

open Microsoft.Research.DNA

module BMEValue = Microsoft.Research.CRNEngine.Expression


//-------------
// Generators for FsCheck

// Basics
let gen_bool = Arb.generate<bool>
let gen_int = Arb.generate<int>
let gen_float = Arb.generate<NormalFloat> |> Gen.map float
let gen_var = Gen.oneof [ gen { return "s" }; gen { return "t" }; gen { return "u" } ]
let gen_complementarity =
    Gen.choose (0, 100000) |> Gen.map (fun i -> (float i) / 100000.0)

let dsd_prim s =
    let rec sized s =
        match s with
        | 0 -> Gen.oneof [ Gen.map BMEValue.Float gen_float
                           Gen.map BMEValue.Key gen_var ]
        | n when n > 0 ->
            let exp = sized (n/2)
            Gen.oneof [ Gen.map2 (fun a b -> BMEValue.Plus [a; b]) exp exp
                        Gen.map2 (fun a b -> BMEValue.Minus {sub1=a; sub2=b}) exp exp
                        Gen.map2 (fun a b -> BMEValue.Times [a; b]) exp exp
                        Gen.map2 (fun a b -> BMEValue.Divide {div1=a; div2=b}) exp exp
                        Gen.map BMEValue.Absolute exp
                        Gen.map BMEValue.Key gen_var
                        Gen.map BMEValue.Float gen_float
                        Gen.map2 (fun a b -> BMEValue.Power {base_=a; exponent=b}) exp exp ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized sized

// Species
let dsd_domain s =
    let p = dsd_prim (s/2)
    let n = Gen.oneof [ gen { return "x" }; gen { return "y" }; gen { return "z" } ]
    let d = Gen.map3 (fun n v1 v2 -> Value.Domain (n, 0, v1, v2, None)) n p p
    Gen.oneof [ Gen.map (fun v -> Domain.Toe (v, Value.Float (1.0, None), false, None)) d
                Gen.map2 (fun v r -> Domain.Toe (v, Value.Float (r, None), true, None)) d gen_complementarity
                Gen.map2 (fun v b -> Domain.Normal (v, false, None)) d gen_bool ] (* false rather than b to avoid exposing complementary non-toehold domains *)

let no_tags: Domain.tag list = []
let dsd_tether = Gen.choose (0, 30) |> Gen.map (fun i -> (no_tags, i))

let dsd_domain_tethered s =
    Gen.map (fun t -> Domain.Normal (Value.Variable ("tether",Types.emprange), false, Some t)) dsd_tether

let dsd_domain_list s =
    let s = s|>float|>sqrt|>int
    Gen.nonEmptyListOf (dsd_domain s) |> Gen.resize s

let dsd_strand s =
    let ds = dsd_domain_list s
    Gen.oneof [ Gen.map Strand.Upper ds 
                Gen.map Strand.Lower ds ]

let dsd_strand_tethered_left s =
    let ds = Gen.map2 (fun x xs -> x::xs, x) (dsd_domain_tethered s) (dsd_domain_list s)
    Gen.oneof [ Gen.map (fun (l, t) -> Strand.Upper l, t) ds 
                Gen.map (fun (l, t) -> Strand.Lower l, t) ds ]

let dsd_segment_middle s =
    let ds = dsd_domain_list (s/5)
    Gen.map5 (fun lb lt s rt rb -> Segment.Double (lb, lt, s, rt, rb)) ds ds ds ds ds

let dsd_hairpin_left s =
    let ds = dsd_domain_list (s/4)
    Gen.map4 (fun ob ot s hp -> Segment.Hairpin (ob, ot, s, hp, Segment.Left)) ds ds ds ds

let dsd_hairpin_right s =
    let ds = dsd_domain_list (s/4)
    Gen.map4 (fun ob ot s hp -> Segment.Hairpin (ob, ot, s, hp, Segment.Right)) ds ds ds ds

let dsd_segment s =
    Gen.oneof [ dsd_segment_middle s
                dsd_hairpin_left s
                dsd_hairpin_right s ]

let dsd_segment_left s =
    Gen.oneof [ dsd_segment_middle s
                dsd_hairpin_left s ]

let dsd_segment_right s =
    Gen.oneof [ dsd_segment_middle s
                dsd_hairpin_right s ]

let dsd_segments_middle s =
    let s = s|>float|>sqrt|>int
    Gen.nonEmptyListOf (dsd_segment_middle s) |> Gen.resize s

let dsd_segments_left s =
    let x = Gen.eval s (Random.newSeed ()) (dsd_segment_left s)
    let s = s|>float|>sqrt|>int
    let xs = Gen.eval s (Random.newSeed ()) (dsd_segment_middle s |> Gen.listOf)
    gen { return x::xs }

let dsd_segments_right s =
    let x = Gen.eval s (Random.newSeed ()) (dsd_segment_right s)
    let s = s|>float|>sqrt|>int
    let xs = Gen.eval s (Random.newSeed ()) (dsd_segment_middle s |> Gen.listOf)
    gen { return xs@[x] }

let dsd_gate_left s =
    let x = Gen.eval s (Random.newSeed ()) (dsd_segments_left s)
    let s = s|>float|>sqrt|>int
    let xs = Gen.eval s (Random.newSeed ()) (dsd_segments_middle s |> Gen.listOf)
    gen { return x::xs }

let dsd_gate_right s =
    let x = Gen.eval s (Random.newSeed ()) (dsd_segments_right s)
    let s = s|>float|>sqrt|>int
    let xs = Gen.eval s (Random.newSeed ()) (dsd_segments_middle s |> Gen.listOf)
    gen { return xs@[x] }

let dsd_gate s =
    Gen.oneof [ dsd_gate_left s
                dsd_gate_right s ]

let dsd_content_strand s =
    Gen.map (fun (s, _) -> Origami.C_strand s) (dsd_strand_tethered_left s)

let dsd_content_gate s =
    Gen.map (fun g -> Origami.C_gate g) (dsd_gate s)

let dsd_content s =
    Gen.oneof [ dsd_content_strand s
              (*  dsd_content_gate s *)]

let dsd_origami s = dsd_content s |> Gen.listOf

let dsd_species =
    let rec sized s =
        match s with
        | 0 -> Gen.map Species.STRAND (dsd_strand s)
        | n when n > 0 -> Gen.oneof [ Gen.map Species.STRAND (dsd_strand s)
                                      Gen.map Species.GATE (dsd_gate s)
                                      Gen.map Species.ORIGAMI (dsd_origami s) ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized sized

// Reactions


// Main
type Generators =
    static member Species() = Arb.fromGen dsd_species
