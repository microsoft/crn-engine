// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Rudimentary

type node = int
type port = int
type edge = (node*port)*(node*port)

let rec list_to_string f s = function
  | [] -> ""
  | [e] ->  f e
  | e::es -> sprintf "%s%s%s" (f e) s (list_to_string f s es)

let edge_to_string ((n1, p1), (n2, p2)) = sprintf "%d:%d -> %d:%d" n1 p1 n2 p2

// weights:
// -2 unbound long complementary
// -1 unbound toehold
//  0 not complementary - same as edge not included
//  1 bound toehold
//  2 bound long domain

 // To avoid storing each edge twice,
 // an edge is stored with lower node index first
 // if the node indices are the same, then lower port index first
type strand_graph = 
  { nodes : Map<node, int> // id -> arity
  ; edges : Map<edge, int> } // id, port , id, port -> weight

let set_edges sg es = { sg with edges = es }

let get_arity sg n = Map.find n sg.nodes

let is_legal_port sg (n, p) =
  p >= 0 && p < get_arity sg n

// This should probably be either pre-computed or stored in the data structure
let free_port sg np =
  Map.exists (fun (np1, np2) w -> w > 0 && (np1 = np || np2 = np)) sg.edges |> not

let normalize (((n1, p1), (n2, p2)) as e) =
  if n1 < n2 then e
  else if n2 < n1 then ((n2, p2), (n1, p1))
  else ((n1, min p1 p2), (n2, max p1 p2))    

let get_weight sg e = Map.tryFind (normalize e) sg.edges |> (function None -> 0 | Some x -> x)

let is_binding_edge sg (np1, np2) =
  is_legal_port sg np1 && is_legal_port sg np2 &&
  get_weight sg (np1, np2) > 0

let adjacent_right sg ((n1, p1), (n2, p2)) =
  ((n1, p1+1), (n2, p2-1)) |> normalize |> is_binding_edge sg // normalize is needed if n1=n2 and p2=p1+1

let adjacent_left sg ((n1, p1), (n2, p2)) =
  ((n1, p1-1), (n2, p2+1)) |> is_binding_edge sg

// Three types of action edges:
//  - isolated toehold unbinding
//  - complementary domain binding
//  - displacing path
type action =
  | Isolated_toehold of edge
  | Complementary of edge
  | Displacing of edge list

let action_to_string = function
  | Isolated_toehold e -> sprintf "Toehold unbinding: %s" (edge_to_string e)
  | Complementary e -> sprintf "Complementary: %s" (edge_to_string e)
  | Displacing es -> sprintf "Displacing: [%s]" (list_to_string edge_to_string ", " es)

let action_edges = function
  | Isolated_toehold e -> [e]
  | Complementary e -> [e]
  | Displacing es -> es

let has_adjacent sg e = adjacent_left sg e || adjacent_right sg e
let isolated sg e = has_adjacent sg e |> not

let collect_edges f = Map.map (fun k _ -> f k) >> Map.toSeq >> Seq.map snd

let isolated_toehold_bindings sg = sg.edges |>  Map.filter (fun e w -> w = 1 && isolated sg e)
let isolated_toehold_unbinding_actions = isolated_toehold_bindings >> collect_edges Isolated_toehold

let complementary_unbounds sg = sg.edges |> Map.filter (fun (np1, np2) w -> w < 0 && free_port sg np1 && free_port sg np2)
let complemtary_binding_actions = complementary_unbounds >> collect_edges Complementary

let displacing_paths sg =
  let can_be_added = sg.edges |> Map.filter (fun e w -> w < 0 && has_adjacent sg e) |> Map.toSeq |> Seq.map fst
  let can_be_removed = sg.edges |> Map.filter (fun _ w -> w > 0) |> Map.toSeq |> Seq.map fst

  // build map from ports to paths from this port
  // invariant is that active paths have edges from can_be_removed at both ends
  // start from can_be_removed
  let add_path np p m =
    match Map.tryFind np m with
    | None -> Map.add np [p] m
    | Some paths -> Map.add np (p::paths) m

  let add_rem m ((np1, np2) as e) =
    m |> add_path np1 ([e], np2)
      |> add_path np2 ([e], np1)

  let m = can_be_removed |> Seq.fold add_rem Map.empty

  let add_edge (paths, m) ((np1, np2) as e) =
    match Map.tryFind np1 m, Map.tryFind np2 m with
    | None, None -> (paths, m)
    | None, Some ps2 -> // paths can be terminated at np1
      let ps = ps2 |> List.map (fun (es, ep) -> (np1, e::es, ep))
      (ps@paths, m)
    | Some ps1, None -> // paths can be terminated at np2
      let ps = ps1 |> List.map (fun (es, ep) -> (np2, e::es, ep))
      (ps@paths, m)
    | Some ps1, Some ps2 -> // paths should be merged, check for loops
      let pairs_to_merge = ps1 |> List.fold (fun ps_o p1 -> ps2 |> List.fold (fun ps_i p2 -> (p1, p2)::ps_i) ps_o) []
      let merged_paths = pairs_to_merge |> List.collect (fun ((es1,ep1),(es2,ep2)) -> [(ep1, (List.rev es1)@[e]@es2, ep2); (ep2, (List.rev es2)@[e]@es1, ep1)])
      let loops, active_ps = merged_paths |> List.partition (fun (sp,_,ep) -> e = normalize (sp, ep))
      let new_m = active_ps |> List.fold (fun m (sp,es,ep) -> add_path sp (es, ep) m) m
      (loops@paths, new_m)

  can_be_added |> Seq.fold add_edge ([], m) |> fst |> List.map (fun (_, es, _) -> Set.ofList es) |> List.toSeq |> Seq.distinct

let displacing_actions = displacing_paths >> Seq.map (Set.toList >> Displacing)

let actions sg =
  Seq.concat [ isolated_toehold_unbinding_actions sg
             ; complemtary_binding_actions sg
             ; displacing_actions sg ]

let execute_action_edges sg es =
  let invert sg e =
    let w = Map.find e sg
    Map.add e -w sg
  es |> List.fold invert sg

let execute_action sg = action_edges >> execute_action_edges sg.edges >> set_edges sg

type state_node = (action * strand_graph) seq

// Should only be called on systems with finite state space
let compute_state_space sg =
  let rec work ss = function
    | [] -> ss
    | w::ws -> 
      let links = w |> actions |> Seq.map (fun a -> a, execute_action w a)
      let new_ss = Map.add w links ss
      let new_ws = links |> Seq.choose (fun (_,sg) -> if Map.containsKey sg new_ss then None else Some sg)
      work new_ss (Seq.toList new_ws @ ws)
  work Map.empty [sg]

// meant to replace the one above, but duplicates some states atm
let state_space sg =
  let rec work ss l =
    seq {
      match l with
      | [] -> ()
      | w::ws -> 
        let links = w |> actions |> Seq.map (fun a -> a, execute_action w a)
        let new_ss = Map.add w links ss
        let new_ws = links |> Seq.choose (fun (_,sg) -> if Map.containsKey sg new_ss then None else Some sg)
        yield w, links
        yield! work new_ss (Seq.toList new_ws @ ws)
    }
  work Map.empty [sg]

let print_state_space ss_seq =
  let state_to_string i (sg, a) =
    sprintf
      "%d: %d edges\n  %s\n   %d actions\n  %s"
      i
      (sg.edges |> Map.toSeq |> Seq.length)
      (sg.edges |> Map.toList |> list_to_string (fun (e,w) -> sprintf "%s @ %d" (edge_to_string e) w) "\n  ")
      (Seq.length a)
      (a |> Seq.toList |> list_to_string (fun (a, sg) -> sprintf "to %d via %s" (Seq.findIndex (fst >> (=) sg) ss_seq) (action_to_string a)) "\n  ")
  ss_seq |> Seq.iteri (fun i s -> printfn "%s" (state_to_string i s))

let catalytic =
  { nodes = Map.ofList [ (0, 2); (1, 3); (2, 3); (3, 2); (4, 4) ]
  ; edges = Map.ofList [ ((0, 1), (4, 3)), 2
                       ; ((1, 0), (4, 3)), -2
                       ; ((1, 1), (4, 2)), -1
                       ; ((1, 2), (4, 1)), -2
                       ; ((2, 1), (4, 2)), 1
                       ; ((2, 2), (4, 1)), 2
                       ; ((3, 0), (4, 1)), -2
                       ; ((3, 1), (4, 0)), -1 ] }

let eight_way = // transcription of the latex code
  { nodes = Map.ofList [ (0, 3); (1, 3); (2, 3); (3, 3); (4, 3); (5, 3); (6, 3); (7, 3) ]
  ; edges = Map.ofList [ ((0, 0), (7, 2)), 2
                       ; ((0, 1), (7, 1)), 2
                       ; ((0, 2), (7, 0)), -2
                       ; ((0, 0), (1, 2)), -2
                       ; ((0, 1), (1, 1)), -2
                       ; ((0, 2), (1, 0)), 2
                       ; ((1, 0), (2, 2)), -2
                       ; ((1, 1), (2, 1)), 2
                       ; ((1, 2), (2, 0)), 2
                       ; ((2, 0), (3, 2)), -2
                       ; ((2, 1), (3, 1)), -2
                       ; ((2, 2), (3, 0)), 2
                       ; ((3, 0), (4, 2)), -2
                       ; ((3, 1), (4, 1)), 2
                       ; ((3, 2), (4, 0)), 2
                       ; ((4, 0), (5, 2)), -2
                       ; ((4, 1), (5, 1)), -2
                       ; ((4, 2), (5, 0)), 2
                       ; ((5, 0), (6, 2)), -2
                       ; ((5, 1), (6, 1)), 2
                       ; ((5, 2), (6, 0)), 2
                       ; ((6, 0), (7, 2)), -2
                       ; ((6, 1), (7, 1)), -2
                       ; ((6, 2), (7, 0)), 2 ] }
