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


let rec mapPred f = function
  | Ptrue -> Ptrue
  | Pfalse -> Pfalse
  | Peq (x, y) -> Peq (map f x, map f y)
  | Pgt (x, y) -> Pgt (map f x, map f y)
  | Pge (x, y) -> Pge (map f x, map f y)
  | Plt (x, y) -> Plt (map f x, map f y)
  | Ple (x, y) -> Ple (map f x, map f y)
  | Pequiv (p, q) -> Pequiv (mapPred f p, mapPred f q)
  | Pimp   (p, q) -> Pimp (mapPred f p, mapPred f q)
  | Pand   (p, q) -> Pand (mapPred f p, mapPred f q)
  | Por    (p, q) -> Por (mapPred f p, mapPred f q)
  | Pnot p -> Pnot (mapPred f p)
  | Plabel n -> Plabel n
  | PtimedLabel (n, i) -> PtimedLabel (n, i)
  | PrangedLabel (n, i, j) -> PrangedLabel (n, i, j)


let rec inline_env_pred e = function
  | Ptrue -> Ptrue
  | Pfalse -> Pfalse
  | Peq (x, y) -> Peq (inline_env e x, inline_env e y)
  | Pgt (x, y) -> Pgt (inline_env e x, inline_env e y)
  | Pge (x, y) -> Pge (inline_env e x, inline_env e y)
  | Plt (x, y) -> Plt (inline_env e x, inline_env e y)
  | Ple (x, y) -> Ple (inline_env e x, inline_env e y)
  | Pequiv (p, q) -> Pequiv (inline_env_pred e p, inline_env_pred e q)
  | Pimp   (p, q) -> Pimp (inline_env_pred e p, inline_env_pred e q)
  | Pand   (p, q) -> Pand (inline_env_pred e p, inline_env_pred e q)
  | Por    (p, q) -> Por (inline_env_pred e p, inline_env_pred e q)
  | Pnot p -> Pnot (inline_env_pred e p)
  | Plabel n -> Plabel n
  | PtimedLabel (n, i) -> PtimedLabel (n, (Value.Float (Value.eval e i)))
  | PrangedLabel (n, i, j) -> PrangedLabel (n, (Value.Float (Value.eval e i)), (Value.Float (Value.eval e j)))


let rec mentionsPred = function
  | Ptrue -> []
  | Pfalse -> []
  | Peq (x, y)
  | Pgt (x, y)
  | Pge (x, y)
  | Plt (x, y)
  | Ple (x, y) -> mentions x @ mentions y
  | Pequiv (p, q)
  | Pimp   (p, q)
  | Pand   (p, q)
  | Por    (p, q) -> mentionsPred p @ mentionsPred q
  | Pnot p -> mentionsPred p
  | Plabel _
  | PtimedLabel _
  | PrangedLabel _ -> []

*)