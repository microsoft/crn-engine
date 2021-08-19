module Microsoft.Research.DNA.Pattern
open Microsoft.Research.DNA

type value = Value.t

type t = 
  | Name of string
  | Pat of t list

val toStringList : t list -> string list

val toTypes : t list -> Types.t ref list
val toTypeEnv : Types.t ref list * t list -> Types.type_env

val check_parameter_types : Types.type_env -> string -> Types.range -> Types.t ref list -> Types.t ref list -> unit

val expandTuples : value list -> t list -> value list