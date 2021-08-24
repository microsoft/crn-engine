// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Strand
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type value = Value.t
type value_env = Value.t_env
type domain = Domain.t

(* A strand is simply a list of domains. *)
type t = Upper of domain list
       | Lower of domain list

(* Mirror a strand between top to bottom. *)
let mirror = function
  | Upper ns -> Lower ns
  | Lower ns -> Upper ns

(* Reverse a strand between left and right. *)
let reverse = function
  | Upper ns -> Upper(List.rev ns)
  | Lower ns -> Lower(List.rev ns)

(* "Physically rotate" a strand. *)
let rotate (s:t) = reverse(mirror s)

(* Are two strands equal, up to physical rotation? *)
let equal (s1:t) (s2:t) = (s1=s2) || (s1 = (rotate s2))

(*Always compare strands in the upper configuration *)
let compare (s1:t) (s2:t) = 
   match s1,s2 with
   | Upper ds1, Upper ds2 -> Domain.compare_domains ds1 ds2
   | Upper ds1, Lower ds2 -> Domain.compare_domains ds1 (List.rev ds2)
   | Lower ds1, Upper ds2 -> Domain.compare_domains (List.rev ds1) ds2
   | Lower ds1, Lower ds2 -> Domain.compare_domains (List.rev ds1) (List.rev ds2)

(* Produce string representations of a strand. *)
let display (s:t) =
  match s with
  | Upper ns -> "<" + Domain.display_sequence ns + ">"
  | Lower ns -> "{" + Domain.display_sequence ns + "}"
let rec displays (sl:t list) = Lib.string_of_list display " + " sl

(* Produce DOT representations of a strand. *)
let to_dot (s:t) =
  match s with
  | Upper ns -> "\"<" + Domain.display_sequence ns + ">\""
  | Lower ns -> "\"{" + Domain.display_sequence ns + "}\""
let rec to_dots (sl:t list) =
  match sl with
  | [] -> ""
  | hd::tl -> (to_dot hd) + " " + (to_dots tl)

(* Evaluate contained values, compute free names, using the constituent domains. *)
let eval (env: value_env) (s:t) =
  match s with
  | Upper ns -> Upper(Domain.eval_sequence env ns)
  | Lower ns -> Lower(Domain.eval_sequence env ns)
let free_names (s:t) =
  match s with
  | Upper ns
  | Lower ns -> List.collect Domain.free_names ns

(* Check types and deal with position information. *)
let inferType in_origami (env:Types.type_env) (s:t) =
  match s with
  | Upper ns
  | Lower ns -> List.iter (Domain.inferType in_origami env) ns
let getPosn = function | Upper ns | Lower ns -> Domain.getPosn (List.head ns)
let erasePosns (s:t) = 
  match s with
  | Upper ns -> Upper(List.map Domain.erasePosns ns)
  | Lower ns -> Lower(List.map Domain.erasePosns ns)
let replacePosns (mrs:Types.range Stringmap.t) (s:t) =
  match s with
  | Upper ns -> Upper(List.map (Domain.replacePosns mrs) ns)
  | Lower ns -> Lower(List.map (Domain.replacePosns mrs) ns)

let standard_form s =
  match s with
  | Upper _ -> s
  | Lower _ -> (*rotate*) s (* rotation is performed when adding reactions *)

(* Does strand s' match the "pattern" s? *)
let matches s s' = 
  let inner_match s s' =
    match s,s' with
    | Upper(ns),Upper(ns') | Lower(ns),Lower(ns') -> Domain.matches_list ns ns'
    | _,_ -> false
  (inner_match s s') || (inner_match s (rotate s'))

let matches_env env s s' = 
  let inner_match_env s s' =
    match s,s' with
    | Upper(ns),Upper(ns') | Lower(ns),Lower(ns') -> Domain.matches_list_env env ns ns'
    | _,_ -> None
  Lib.option_or [inner_match_env s s'; inner_match_env s (rotate s')]

(* Get the list of domains out of a strand. *)
let domains = function Upper ns | Lower ns -> ns

let has_wildcard s = List.exists Domain.is_wildcard (domains s)

(* Get the list of tethered domains out of a strand *)
let tethered_domains = function Upper(ns) | Lower(ns) -> List.filter Domain.is_tethered ns

let get_tags s = Domain.all_tags_list (domains s)
let annotate_tag_sets s =
  let ts = get_tags s in
  match s with
  | Upper ds -> Upper (Domain.set_tags_list ts ds)
  | Lower ds -> Lower (Domain.set_tags_list ts ds)

let unannotate_tag_sets = function
  | Upper ds -> Upper (Domain.set_tags_list None ds)
  | Lower ds -> Lower (Domain.set_tags_list None ds)

(* Is a strand an upper strand? *)
let is_upper = function Upper _ -> true | Lower _ -> false

(* Make an upper strand, suppressing any degree of complementarity information *)
let mk_upper ds =
  let sanitize = function
    | Domain.Toe (d, _, c, t) -> Domain.Toe (d, Value.Float (1.0, None), c, t)
    | d -> d in
  ds |> List.map sanitize |> Upper

(* Get the list of all NON-TOEHOLD domains exposed in the gate. *)
let exposed_nontoeholds (s:t) = List.filter (fun d -> not(Domain.is_toehold d)) (domains s)

(* Find a pair of neighbouring exposed toeholds, if possible. *)
let neighbouring_toeholds (s:t) = Domain.neighbouring_toeholds (domains s)

(* Append two strands together. *)
let append (s:t) (s':t) : t =
  match s,s' with
  | (Lower s),(Lower s') -> Lower(s@s')
  | (Upper s),(Upper s') -> Upper(s@s')
  | _,_ -> failwith "Strand.append: cannot join a lower and an upper strand"

(* Is a strand "reactive", i.e. contains a toehold? *)
let is_reactive (s:t) =
  let domains = match s with Upper ds | Lower ds -> ds in
  Domain.contains_toehold domains

(******************************************************************************)
let universal_counters (s:t) =
  match s with
  | Upper ns -> Upper(List.map Domain.universal_counters ns)
  | Lower ns -> Lower(List.map Domain.universal_counters ns)
