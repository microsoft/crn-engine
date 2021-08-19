module Microsoft.Research.GEC.Solver

open Microsoft.Research.GEC

//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

(* Compute a set of context-sensitive substitutions for a brick, relative to a database. *)
val matchParts : Database.t -> string -> string -> (string * (string list list)) list -> Cssubst.t list * string list

(* Compute a set of context-sensitive substitutions for a normal reaction, relative to a database. *)
val matchNormalReactions : Database.t -> string list list -> string list list -> string list list -> string -> Cssubst.t list * string list

(* Compute a set of context-sensitive substitutions for a transport reaction, relative to a database. *)
val matchTransportReactions : Database.t -> reactant:string list -> product:string list -> string -> string ->
                              Ast.direction -> Cssubst.t list * string list
