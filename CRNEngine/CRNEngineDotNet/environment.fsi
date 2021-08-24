// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>] 
module Microsoft.Research.CRNEngine.Environment

type t = Map<string,float>

val create : (string * float) list -> t
val find : t -> string -> float
val empty : t
val extend : t -> t -> t
val remove_list : names:string list -> e:t -> t
val to_string : t -> string
