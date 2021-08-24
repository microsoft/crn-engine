// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Species

type t = { name: string }

val create : string -> t
val to_string : t -> string
val from_string : (string -> t)
val parse : Parser.t<t>