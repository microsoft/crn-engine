module Microsoft.Research.CRNEngine.Units
open WebSharper

[<NamedUnionCases>]
type time = Seconds of Seconds:float
[<NamedUnionCases>]
type space = Metres of Metres:int
[<NamedUnionCases>]
type concentration = Molar of Molar:int

type t = {
  concentration: concentration;
  time: time;
  space: space;
}

val defaults : t
//val time_from_string : string -> time
val time_to_string : time -> string
val space_from_string : string -> space
val space_to_string : space -> string
val concentration_from_string : string -> concentration
val concentration_to_string : concentration -> string
