[<JavaScript>] 
module Microsoft.Research.CRNEngine.Key

type t<'s> = 
  | Parameter of string
  | Rate      of string
  | Species   of 's
  | Time

type inlined_t<'s> = 
  | ISpecies of 's
  | ITime

val to_string         : ('s -> string) -> t<'s> -> string
val to_matlab         : ('s -> string) -> string -> t<'s> -> string
val to_string_plot    : ('s -> string) -> t<'s> -> string
val to_string_inlined : ('s -> string) -> inlined_t<'s> -> string
val map               : ('s1 -> 's2) -> t<'s1> -> t<'s2>
val mapInlined        : ('s1 -> 's2) -> inlined_t<'s1> -> inlined_t<'s2>
val collect           : ('s1 -> 's2 list) -> t<'s1> -> t<'s2> list
val parse             : Parser.t<'s> -> Parser.t<t<'s>>
val from_string       : Parser.t<'s> -> string -> t<'s>