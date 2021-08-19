module Microsoft.Research.CRNEngine.Stringbuilder

type t = System.Text.StringBuilder
val empty : unit -> t
val init : string -> t
val append: t -> string -> unit
val value : t -> string
