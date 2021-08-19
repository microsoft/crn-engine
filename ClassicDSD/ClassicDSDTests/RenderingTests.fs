module Microsoft.Research.DNA.RenderingTests

open FsUnit
open Xunit

open Microsoft.Research.DNA
open Microsoft.Research.DNA.Dsd
open Microsoft.Research.DNA.BranchesRenderer

[<Trait("Category", "Rendering")>]
[<Fact(DisplayName="Rendering - classic strand to dstrand")>]
let classic_strand () =
    let code = "<a^ b>"
    let bundle = Dsd.parse code
    match bundle with
    | ClassicDSD bundle ->
        let species = match bundle.initials with [initial] -> initial.species | _ -> failwith "bad parse result"
        let strand = match species with | Species.STRAND strand -> strand | _ -> failwith "bad parse result"
        let dstrand = BranchesRenderer.from_dsd_strand Sequence.empty strand
        Assert.Equal(DStrandKind.TopStrand, dstrand.kind)
        match dstrand.domains with
        | [dom1;dom2] ->
            Assert.Equal("a", dom1.name)
            Assert.Equal(true, dom1.toehold)
            Assert.Equal(false, dom1.complemented)
            Assert.Equal(None, dom1.tether)
            Assert.Equal(None, dom1.pseudoknot)
            Assert.Equal("b", dom2.name)
            Assert.Equal(false, dom2.toehold)
            Assert.Equal(false, dom2.complemented)
            Assert.Equal(None, dom2.tether)
            Assert.Equal(None, dom2.pseudoknot)
            ()
        | _ -> sprintf "Unexpected number of domains in output: %d" dstrand.domains.Length |> failwith
        ()
    | _ -> failwith "bad parse result"
    ()
    
[<Trait("Category", "Rendering")>]
[<Fact(DisplayName="Rendering - rules strand to dstrand")>]
let rules_strand () =
    let code = "directive rules {} <a^ b>"
    let bundle = Dsd.parse code
    match bundle with
    | Rules bundle ->
        let species = match bundle.initials with [initial] -> initial.species | _ -> failwith "bad parse result"
        let dspecies = BranchesRenderer.from_logic_dsd species
        let dstrand = match dspecies with | Strand dstrand -> dstrand | _ -> failwith "bad parse result"
        Assert.Equal(DStrandKind.TopStrand, dstrand.kind)
        match dstrand.domains with
        | [dom1;dom2] ->
            Assert.Equal("a", dom1.name)
            Assert.Equal(true, dom1.toehold)
            Assert.Equal(false, dom1.complemented)
            Assert.Equal(None, dom1.tether)
            Assert.Equal(None, dom1.pseudoknot)
            Assert.Equal("b", dom2.name)
            Assert.Equal(false, dom2.toehold)
            Assert.Equal(false, dom2.complemented)
            Assert.Equal(None, dom2.tether)
            Assert.Equal(None, dom2.pseudoknot)
            ()
        | _ -> sprintf "Unexpected number of domains in output: %d" dstrand.domains.Length |> failwith
        ()
    | _ -> failwith "bad parse result"
    ()

[<Trait("Category", "Rendering")>]
[<Fact(DisplayName="Rendering - pseudoknot hairpin")>]
let pseudoknot_hairpin () =
    let code = "directive rules{} [<x4!2 x5 x2!1 x4*!2 x3^* x2*!1 x1^*>]"
    let bundle = Dsd.parse code
    match bundle with
    | Rules bundle ->
        let species = match bundle.initials with [initial] -> initial.species | _ -> failwith "bad parse result"
        let dspecies = BranchesRenderer.from_logic_dsd species
        let dbranch = match dspecies with | Branch dbranch -> dbranch | _ -> failwith "bad parse result"
        if dbranch.children.Length > 0 then failwith "Unexpected children in dbranch."
        let dgate = dbranch.gate
        if dgate.left_connection.IsSome then failwith "Unexpected left connection in dgate."
        let dsegment = match dgate.segments with [dsegment] -> dsegment | _ -> sprintf "Unexpected %d segments in dgate." dgate.segments.Length |> failwith
        Assert.Equal(None, dsegment.connection)
        Assert.Equal(None, dsegment.bottom_left)
        Assert.Equal(None, dsegment.top_left)
        Assert.Equal(None, dsegment.hairpin_right)
        Assert.Equal(DStrandKind.DoubleStrand, dsegment.double_strand.kind)
        let ds_dom = match dsegment.double_strand.domains with [ds_domain] -> ds_domain | _ -> sprintf "Unexpected %d domains in double strand." dsegment.double_strand.domains.Length |> failwith
        Assert.Equal("x2", ds_dom.name)
        Assert.Equal(false, ds_dom.toehold)
        Assert.Equal(true, ds_dom.complemented)
        Assert.Equal(None, ds_dom.pseudoknot)
        let lh = match dsegment.hairpin_left with Some lh -> lh | _ -> failwith "Unexpected missing left hairpin."
        Assert.Equal(DStrandKind.LeftHairpinStrand, lh.kind)
        let lh_dom1,lh_dom2 = match lh.domains with [lh_dom1;lh_dom2] -> (lh_dom1,lh_dom2) | _ -> sprintf "Unexpected %d domains in left hairpin." lh.domains.Length |> failwith
        Assert.Equal("x3", lh_dom1.name)
        Assert.Equal(true, lh_dom1.toehold)
        Assert.Equal(true, lh_dom1.complemented)
        Assert.Equal(None, lh_dom1.pseudoknot)
        Assert.Equal("x4", lh_dom2.name)
        Assert.Equal(false, lh_dom2.toehold)
        Assert.Equal(true, lh_dom2.complemented)
        let pk = match lh_dom2.pseudoknot with Some pk -> pk | _ -> failwith "Unexpected missing pseudoknot."
        let tr = match dsegment.top_right with Some tr -> tr | _ -> failwith "Unexpected missing top right overhang."
        Assert.Equal(DStrandKind.TopStrand, tr.kind)
        let tr_dom = match tr.domains with [tr_dom] -> tr_dom | _ -> sprintf "Unexpected %d domains in top right overhang." tr.domains.Length |> failwith
        Assert.Equal("x1", tr_dom.name)
        Assert.Equal(true, tr_dom.toehold)
        Assert.Equal(true, tr_dom.complemented)
        Assert.Equal(None, tr_dom.pseudoknot)
        let br = match dsegment.bottom_right with Some br -> br | _ -> failwith "Unexpected missing bottom right overhang."
        Assert.Equal(DStrandKind.BottomStrand, br.kind)
        let (br_dom1,br_dom2) = match br.domains with [br_dom1;br_dom2] -> (br_dom1,br_dom2) | _ -> sprintf "Unexpected %d domains in bottom right overhang." br.domains.Length |> failwith
        Assert.Equal("x5", br_dom1.name)
        Assert.Equal(false, br_dom1.toehold)
        Assert.Equal(false, br_dom1.complemented)
        Assert.Equal(None, br_dom1.pseudoknot)
        Assert.Equal("x4", br_dom2.name)
        Assert.Equal(false, br_dom2.toehold)
        Assert.Equal(false, br_dom2.complemented)
        Assert.Equal(Some pk, br_dom2.pseudoknot)
        ()
    | _ -> failwith "bad parse result"
    ()