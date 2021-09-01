// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Strands

type domain =
  { name : string
  ; complemented : bool
  ; toehold : bool }

let domain_to_string d = sprintf "%s%s%s" d.name (if d.toehold then "^" else "") (if d.complemented then "*" else "")

let complementary d1 d2 = d1.name = d2.name && d1.toehold = d2.toehold && d1.complemented <> d2.complemented

type strand = domain array

let strand_to_string = Array.map domain_to_string >> String.concat " " >> sprintf "<%s>"

type port =
  { strand : int
  ; site : int }

//Probably an edge but files are ordered such "edge" is defined later, can we share definition?
type port_pair =
  { port1 : port
  ; port2 : port }

type t =
  { strand_types : strand array
  ; admissible_edges : Map<port_pair, bool> } // true is toehold, false is non-toehold

let type_to_string t i = strand_to_string t.strand_types.[i]
