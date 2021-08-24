// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]

(* Old version 
module Microsoft.Research.CRNEngine.Assignment

type value = Expression.t<string>
type environment = Environment.t
type t = 
  {
    variables: string list; 
    values: value list list;
  }
    with 
      static member create2 : string list*value list list -> t
      static member create2 : string list*float list list -> t  
      static member ( => ) : string list*value list list -> t 
      static member ( => ) : string list*float list list -> t 
      static member ( => ) : string*float list -> t
    end

val create : string list -> value list list -> t
val create_single: string -> float list -> t
val create_floats: string list -> float list list -> t
val get_variables : a:t -> string list
val merge : t list -> t
val merge_eval : environment -> t list -> environment list
val eval : environment -> t -> environment list
val add_multiples : int ref -> string list -> t -> string list list * t
val to_string_bindings: t -> string list
val to_string: t -> string
val parse : Parser.t<t>
*)