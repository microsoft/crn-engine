module Microsoft.Research.DNA.Visualisation
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

module Species = Microsoft.Research.DNA.Species

type domain = Domain.t
type strand = Strand.t
type gate = Gate.t
type origami = Origami.t

type t
val display : t -> string

val drawInOrigami : t -> bool
val drawBottomLeftArrow : t -> bool
val drawTopRightArrow : t -> bool
val gapTopLeft : t -> bool
val gapTopRight : t -> bool
val gapBottomLeft : t -> bool
val gapBottomRight : t -> bool
val drawTopStrand : t -> bool
val drawBottomStrand : t -> bool
val drawHairpin : t -> bool
val drawHairpinLeft : t -> bool
val drawHairpinRight : t -> bool

val anchoredSpecies : t -> t list list
val mainDomains : t -> domain list
val overhangBottomLeft : t -> domain list
val overhangTopLeft : t -> domain list
val overhangTopRight : t -> domain list
val overhangBottomRight : t -> domain list
val leftDomain : t -> domain
val rightDomain : t -> domain
val hairpinDomains : t -> domain list

val from_strand : strand -> t list
val from_gate : gate -> t list
val from_origami : origami -> t list