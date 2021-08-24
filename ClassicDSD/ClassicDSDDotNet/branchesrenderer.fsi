// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.BranchesRenderer
open Microsoft.Research.CRNEngine
open Microsoft.Research.DNA.Options


type internal DDomain = {
    name: string;
    nucleotides: Sequence.t;
    toehold: bool;
    complemented: bool;
    tether: string option;
    pseudoknot: int option;
} with
  member GetComplemented : unit -> DDomain
  override ToString : unit -> string
end

type internal DStrandKind = TopStrand | BottomStrand | DoubleStrand | LeftHairpinStrand | RightHairpinStrand

type internal DStrand = {
    kind: DStrandKind;
    domains: DDomain list;
} with
  override ToString : unit -> string


type internal DSegmentConnection = TopLink | BottomLink | DoubleLink

type internal DSegment = {
    top_left: DStrand option;
    top_right: DStrand option;
    bottom_left: DStrand option;
    bottom_right: DStrand option;
    double_strand: DStrand;
    hairpin_left: DStrand option;
    hairpin_right: DStrand option;
    connection: DSegmentConnection option;
} with
  override ToString : unit -> string
end

type internal DGate = {
  left_connection: DSegmentConnection option;
  segments: DSegment list;
} with
  override ToString : unit -> string
end

type internal DBranch = {
    gate: DGate;
    children: DBranch list;
}

type internal DOrigamiElement = Strand of DStrand | Branch of DBranch

type internal DOrigami = DOrigamiElement list

val internal from_dsd_strand : Sequence.mapping -> Strand.t -> DStrand
val internal from_dsd_gate : Sequence.mapping -> Gate.t -> DGate
val internal from_dsd_origami : Sequence.mapping -> Origami.t -> DOrigami
val internal from_logic_dsd : RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT> -> DOrigamiElement

val dom_map : Hashtable.t<string,string> -> Species.t list -> Hashtable.t<string,string>
val species_style : Hashtable.t<string,string> -> Species.t list -> string
val species_to_svg_branches : arrange_mode -> renderer_mode -> Sequence.mapping -> bool -> string -> Species.t -> string