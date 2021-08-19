module Microsoft.Research.DNA.Value
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type range = Types.range
type name = string

type op =
  | Plus
  | Minus
  | Mul
  | Div
  | Power
  | Max
  | Modulo
(*  | Equal
  | Different
  | Lt
  | Gt
  | Ltequal
  | Gtequal *)

type fn = 
  | Float2Int
  | Int2Float
  | Sqrt
  | Rate
   
(* Primitive values are either modelling engine values *)
type prim = Expression.t<string>

type parser<'a> = Parser.t<'a>

(* The type of DSD values/expressions. *)
type t =
  | String of string * range
  | Int of int * range
  | Domain of string * int * prim * prim * range
  | DomainS of string * int * string * prim * prim * range (* Domain with specified DNA sequence *)
  | Bool of bool * range
  | Char of char * range
  | Float of float * range
  (*
  | Entalpy of float<kcal/mol> * range
  | Entropy of float<cal/(mol*K)> * range
  *)
  | Variable of name * range
  | Op of t * op * t * range
  | Neg of t * range
  | Show of t * range
  | Function of fn * t * range
  | Tuple of t list * range
  | Ceiling of t * range
  | Floor   of t * range

type t_bind =
  | Val of string * t
  | Sys of string * t
type t_env = t_bind list

val get_range : t -> range
val set_range : range -> t -> t
val erasePosns : t -> t
val replacePosns : range Stringmap.t -> t -> t
val to_string : t -> string
val to_strings : t list -> string

val bind_rate : t -> t -> prim
val unbind_rate : t -> prim
val matching : t * t -> bool
val varInEnv : t_env -> string -> bool
val inferType : Types.type_env -> t -> Types.t ref
val free_names : t -> (name * range) list
val eval : t_env -> t -> t

val universal_counters : t -> t
val to_engineValue : t -> prim

val parse : parser<t>