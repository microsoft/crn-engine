(* Produce long strings relatively efficiently without too much allocation. *)

[<JavaScript>]
module Microsoft.Research.CRNEngine.Stringbuilder
type t = System.Text.StringBuilder
let empty () : t = new System.Text.StringBuilder()
let init (str:string) : t = new System.Text.StringBuilder(str)
let append (sb:t) (str:string) = ignore(sb.Append(str))
let value (sb:t) = sb.ToString()