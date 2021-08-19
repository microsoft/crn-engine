module Microsoft.Research.CRNEngine.Parameter

module filzbach = Microsoft.Research.Filzbach.Parameters
type t = {
  name:string;
  value:float;
  prior:Prior.t option;
}

val remove_list : names:string list -> ps:t list -> t list
val fix_list : ps:t list -> t list
val substitute : e:Environment.t -> p:t -> t
val substitute_list : e:Environment.t -> ps:t list -> t list
val create : (string*float*Prior.t option) -> t
val to_env : t list -> Environment.t
val filter_variable : t list -> t list
//val map_interval : Prior.interval -> bool -> Microsoft.Research.Filzbach.ParameterType
val to_filzbach : t -> filzbach.Parameter
val to_string : t -> string
val list_to_string : t list -> string
val list_expand_multiples : Sweep list list -> t list -> Sweep list list * t list
val parse : Parser.t<t>