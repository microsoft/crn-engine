module Microsoft.Research.DNA.Svg
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open Microsoft.Research.DNA.Options

module Species = Microsoft.Research.DNA.Species
module CrnSvg = Microsoft.Research.CRNEngine.Svg

val species_style : Hashtable.t<string,string> -> Species.t list -> string
val species_to_svg : Species.t -> CrnSvg.box
val species_to_svg_mode : renderer_mode -> Sequence.mapping -> Species.t -> CrnSvg.box