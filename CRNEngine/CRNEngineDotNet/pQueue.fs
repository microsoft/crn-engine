// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.PQueue

(* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %%%%%%%%%%%%%%%%%%%%%%%%%%%% OPTIMISATION IDEAS FOR (INDEXED) PRIORITY QUEUES:
   1. Use a better data structure than lists...
   2. Don't re-sort every time when modifying! Or else use bubble-sort for insertion? *)

(* A reaction priority queue is an ordered list of (time, reaction_id) pairs. *)
type t = (float * int) list

(* The empty priority queue. *)
let empty : t = []

(* Produce string representation of a priority queue. *)
let to_string (pq:t) = "[" + (Lib.string_of_list (fun (r,i) -> "(" + (string r) + ", " + (string i) + ")") "" pq) + "]"

(* Sort a priority queue into ascending order of putative times. *)
let sort (pq:t) : t = Lib.sort (fun (t1,_) (t2,_) -> compare t1 t2) pq

let from_list l = sort l

(* Get the index and time of the next reaction from a priority queue. *)
let get_next (pq:t) : (float * int) =
  match pq with
  | [] -> failwith "get_next: priority queue is empty"
  | (t,i)::_ -> (t,i)

(* Get the putative time associated with a given index. *)
let get_index (pq:t) (i:int) : float =
  let rec loop (pq:t) =
    match pq with
    | [] -> failwith "get_index: index not found in priority queue"
    | ((tj,j)::pq) -> if i=j then tj else loop pq
  loop pq

(* Modify the putative time of a particular reaction in the queue. *)
let modify (pq:t) (i:int) (t:float) : t =
  let rec loop pq =
    match pq with
    | [] -> failwith "modify: index not found in priority queue"
    | ((tj,j)::pq) -> if i=j then (t,j)::pq else (tj,j)::(loop pq)
  sort (loop pq)

let add (ti,i) (pq:t) : t =
  let rec loop pq =
    match pq with
    | [] -> [ti,i]
    | ((tj,j)::pq) -> if ti<=tj then (ti,i)::(tj,j)::pq else (tj,j)::(loop pq)
  loop pq

let remove i (pq:t) : t =
  let rec loop (pq:t) =
    match pq with
    | [] -> []
    | ((tj,j)::pq) -> if i=j then pq else (tj,j)::(loop pq)
  loop pq

let replace (ti,i) pq = add (ti,i) (remove i pq)