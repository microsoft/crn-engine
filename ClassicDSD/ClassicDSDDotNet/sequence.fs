// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Sequence
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

type domain = Domain.t

(* DNA bases. *)
type dnabase = Adenine | Guanine | Cytosine | Thymine
let complement_dnabase = function
  | Adenine -> Thymine
  | Guanine -> Cytosine
  | Cytosine -> Guanine
  | Thymine -> Adenine
let string_of_dnabase = function Adenine -> "A" | Guanine -> "G" | Cytosine -> "C" | Thymine -> "T"

(* DNA sequences. *)
type t = dnabase list
let complement s = List.map complement_dnabase s
let string_of_sequence s = Lib.string_of_list string_of_dnabase "" s
let string_of_seqopt = function Some s -> string_of_sequence s | None -> "*** INSUFFICIENT DNA SEQUENCES ***"

(* Turn a string into a DNA sequence. Throw an expection if ill-formed *)
let parse_sequence (s:string) : t =
  let translate_char = function | 'A' -> Adenine | 'G' -> Guanine | 'C' -> Cytosine | 'T' -> Thymine | c -> Errors.dna_char_error c
  s.ToCharArray() |> Array.map translate_char |> List.ofArray

let split_sequences (s:string) : string list =
  s.Split('\r','\n') |> Array.toList |> List.map (fun s -> s.Trim()) |> List.where (fun s -> s.Length > 0)

(* Turn a string into a list of DNA sequences. Throw an exception if ill-formed.
FP: rewrote this to be faster. *)
let parse_sequences (s:string) : t list =
  let seqs = split_sequences s
  seqs |> List.map parse_sequence

  (*let chars = Lib.charlist_of_string s in
  let translate_character = function
    | 'A' -> Some Adenine 
    | 'G' -> Some Guanine 
    | 'C' -> Some Cytosine 
    | 'T' -> Some Thymine 
    | '\r' -> None
    | '\n' -> None
    | c -> Errors.dna_char_error c
  in
  let curr,seqs =
    Lib.fold_left (
      fun (curr,seqs) c -> 
          match translate_character c with
          | None -> if curr = [] then ([],seqs) else ([],seqs@[curr])
          | Some b -> (curr@[b], seqs)) ([],[]) chars
    in
  seqs@[curr]*)

(* Mappings from domains to sequences. *)
type mapping =
  { toeholds: string list
  ; specificities: string list
  ; assigned: (domain * t option) list }
let empty : mapping = { toeholds = []; specificities = []; assigned = [] }
let initialise_mapping (opts:Options.t) : mapping =
  let ts = opts |> Options.getToeholdsText |> split_sequences in
  let ss = opts |> Options.getSpecificitiesText |> split_sequences in
  (* If checks are disabled then we don't check the lengths / check for duplicates. *)
  if (Options.getCheckDNA opts) then
   (List.iter (fun (s:string) -> if (s.Length>=10) then (Errors.dna_toe_error s)) ts;
    List.iter (fun (s:string) -> if (s.Length<10) then (Errors.dna_spec_error s)) ss;
    match Lib.find_duplicate (=) (ts@ss) with
      | None -> { toeholds = ts; specificities = ss; assigned = [] }
      | Some s ->  Errors.dna_dup_error s)
  else
    { toeholds = ts; specificities = ss; assigned = [] }

(* Get a domain's value from the mapping. *)
let get_domain (m:mapping) (d:domain) =
  let rec get d = function
    | [] -> None
    | (d',s':t option)::m' -> if Domain.equal d d' then Some s' else get d m'
  in
  match get (Domain.unstar d) m.assigned with
    | None -> None
    | Some None -> Some None
    | Some (Some s) -> Some(Some(if Domain.is_complemented d then List.rev(complement s) else s))

(* Assign the next toehold/specificity domain. *)
let assign_next_toehold (d:domain) (m:mapping) : mapping =
  match m.toeholds with
  | [] -> { m with assigned = (d,None)::m.assigned }
  | (t::ts') -> { m with toeholds = ts'; assigned = (d,t |> parse_sequence |> Some)::m.assigned }
let assign_next_specificity (d:domain) (m:mapping) : mapping =
  match m.specificities with
  | [] -> { m with assigned = (d,None)::m.assigned }
  | (s::ss') -> { m with specificities = ss'; assigned = (d,s |> parse_sequence |> Some)::m.assigned }

(* Add a new domain to the mapping (uncomplemented).
   Return the existing mapping if the uncomplemented domain is already present. *)
let add_domain (m:mapping) (d:domain) =
  let d = Domain.unstar d in
  match (get_domain m d) with
  | Some _ -> m
  | None -> match d with
            | Domain.Toe(Value.DomainS(_,_,s,_,_,_),_,_,_) -> 
              { m with assigned = (d,Some (List.head (parse_sequences s)))::m.assigned }
            | Domain.Toe _ -> assign_next_toehold d m
            | Domain.Normal(Value.DomainS(_,_,s,_,_,_),_,_) ->
              { m with assigned = (d,Some (List.head (parse_sequences s)))::m.assigned }
            | Domain.Normal _ -> assign_next_specificity d m

(* Add multiple new domains to the mapping. *)
let add_domains (m:mapping) (ds:domain list) =
  Lib.fold_left add_domain m ds

(* Produce a string representation of a mapping. *)
let display_mapping (m:mapping) = 
  let sorted = Lib.sort (fun x y -> compare (fst x) (fst y)) m.assigned in
  (Lib.string_of_list (fun (d,s) -> (Domain.display_bare_domain d) + " --> (5\') " + (string_of_seqopt s)) " (3\')\n" sorted) +
  (match sorted with [] -> "" | _ -> " (3')") 

(* **************************************************************************************************** *)

(* Old code for generating random bases and sequences. *)
(*
let random_base () =
  match Randomise.int 4 with
  | 0 -> Adenine
  | 1 -> Guanine
  | 2 -> Cytosine
  | 3 -> Thymine
  | _ -> failwith "Sequence.random_base: random number greater than 3"

let random_sequence (len:int) = Lib.make_list random_base len
let random_toehold () = let len = (Randomise.int 6)+4 in random_sequence len
let random_specificity () = let len = (Randomise.int 11)+20 in random_sequence len
*)
