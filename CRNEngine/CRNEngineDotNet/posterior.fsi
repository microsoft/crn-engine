module Microsoft.Research.CRNEngine.Posterior

type t = float list list

val parse : Parser.t<t>
val to_string : t -> string