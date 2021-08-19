[<JavaScript>]
module Microsoft.Research.CRNEngine.Sweep

type value = Expression.t<string>
type environment = Environment.t
type instance = Instance.t
type t = { 
  name: string;
  assignments: Assignment.t list;
}
type parser = Parser.t<t>

val create : string -> Assignment.t list -> t
val to_variables : s:t -> string list
val list_to_variables : sweeps:t list -> string list
val eval : environment -> t -> environment list
val to_instances : model:string -> e:environment -> s:t -> instance list
val list_to_instances : model:string -> e:environment -> sweeps:t list -> instance list
val variable_dependencies : sweeps:t list -> string list list
val list_add_multiples : int ref -> string list -> t list -> string list list list * t list
val to_string : t -> string
val list_to_string : t list -> string
val parse: parser
