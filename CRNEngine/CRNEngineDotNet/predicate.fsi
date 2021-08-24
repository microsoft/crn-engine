// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module predicate
(*
type 'key pred when 'key:equality =
    | Ptrue
    | Pfalse
    | Peq of 'key t * 'key t
    | Pgt of 'key t * 'key t
    | Pge of 'key t * 'key t
    | Plt of 'key t * 'key t
    | Ple of 'key t * 'key t
    | Pequiv of 'key pred * 'key pred
    | Pimp of 'key pred * 'key pred
    | Pand of 'key pred * 'key pred
    | Por of 'key pred * 'key pred
    | Pnot of 'key pred
    | Plabel of string
    | PtimedLabel of string * Value.t
    | PrangedLabel of string * Value.t * Value.t

val mapPred : ('a -> 'b) -> 'a pred -> 'b pred

val inline_env_pred : environment -> 'a pred -> 'a pred

val mentionsPred : 'a pred -> 'a list

*)