[<JavaScript>]
module Microsoft.Research.CRNEngine.Dataset

type t = {
  file : string;
  data : Table.t<float> list 
}

val empty : string -> t
val create : string -> Table.t<float> list -> t