// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Sitegraphs

let parse (s:string) =
  //Syntax.parse s
  ExtendedSyntax.parse_enzymes s

let tryParse s = Parser.run ExtendedSyntax.parser_enzymes s

let strip_strand : Syntax.strand -> Strands.strand = List.map (fun syn -> syn.domain) >> List.toArray

let default_bind_rate = 0.0003
let default_unbind_rate = 0.1126
let default_toehold_rates = default_bind_rate, default_unbind_rate

let default_domain_bind_rate = 0.003
let default_domain_unbind_rate = 0.0
let default_domain_rates = default_domain_bind_rate, default_domain_unbind_rate

let indexed_pairs (a: 'a []) =
  if a.Length < 2 then [||]
  else Array.init (a.Length - 1) (fun i -> i, (a.[i], a.[i+1]))

let all_splits (a: 'a []) (splits: int []) =
  let last = a.Length - 1
  let get_start i = if i = 0 then 0 else splits.[i-1] + 1
  let get_end i = if i = splits.Length then last else splits.[i]
  let pairs = 
    seq {
      for i in 0..splits.Length do
        for j in i..splits.Length do
          yield get_start i, get_end j
        done
      done
    }
  pairs |> Seq.map (fun (s, e) -> Array.sub a s (e-s+1))

let add_default m (t, rates) =
  match Map.tryFind t m with
  | None -> Map.add t rates m
  | Some _ -> m 

let all_maps_to i = Seq.map (fun x -> (x, i))

let semantics (enzymes, (toeholds, complexes)) =
  let toehold_map = toeholds |> List.fold (fun m (t:Syntax.toehold) -> Map.add t.name (t.bind_rate, t.unbind_rate) m) Map.empty
  let syn_strands = complexes |> List.map snd |> List.concat
  let apply_enzyme (before_nick, after_nick) (s: Strands.strand) =
    let to_nick = Array.append before_nick after_nick
    let shift = before_nick.Length-1

    let nick_sites = s |> Seq.windowed to_nick.Length
                       |> Seq.mapi (fun i w -> i+shift, w = to_nick)
                       |> Seq.filter snd |> Seq.map fst
                       |> Seq.toArray
    all_splits s nick_sites
  let apply_enzymes strands =
    enzymes |> Seq.fold (fun ss e -> ss |> Seq.collect (apply_enzyme e)) strands
  let strand_types = syn_strands |> List.map strip_strand |> List.toSeq |> apply_enzymes |> Seq.distinct |> Seq.toArray
  let syn_toeholds = strand_types |> Array.collect (Array.map (fun d -> if d.toehold then d.name, default_toehold_rates else d.name, default_domain_rates))
  let toehold_map = syn_toeholds |> Array.fold add_default toehold_map
  let strand_types_list =
    seq { // strand_types |> Array.iteri (fun i st -> st |> Array.iteri (fun j d -> yield ((i,j),d)))
      for i in 0..strand_types.Length - 1 do
        let st = strand_types.[i]
        for j in 0..st.Length - 1 do
          yield ({Strands.port.strand=i;Strands.port.site=j},st.[j])
    } |> Seq.toList
  let rec add_domains m = function
    | [] -> m
    | (p:Strands.port,d)::ds ->
      let cs = ds |> List.filter (snd >> Strands.complementary d)
      let new_m = cs |> List.fold (fun m_acc (tp,_) -> Map.add {Strands.port_pair.port1=p; Strands.port_pair.port2=tp} d.toehold m_acc) m
      add_domains new_m ds
  let admissible_edges = strand_types_list |> add_domains Map.empty
  let strands =
    { Strands.strand_types = strand_types
    ; Strands.admissible_edges = admissible_edges }
  let complex_to_species c =
    let species_strands = c |> List.toArray |> Array.map (fun syn -> Array.findIndex (fun st -> st = strip_strand syn) strand_types)
    let syns_indexed = c |> List.map (List.map (fun syn -> syn.binding) >> List.toArray) |> List.toArray
    let add_binding es ps =
      match ps |> Seq.toList with
      | [p1:Strands.port, _:string; p2, _] -> Set.add (p1, p2) es
      | _ -> failwith "Invalid binding"
    let species_edges =
      seq { // strand_types |> Array.iteri (fun i st -> st |> Array.iteri (fun j d -> yield ((i,j),d)))
        for i in 0..syns_indexed.Length - 1 do
          let st = syns_indexed.[i]
          for j in 0..st.Length - 1 do
            match st.[j] with
            | None -> ()
            | Some b -> yield ({Strands.port.strand=i;Strands.port.site=j}, b)
      } |> Seq.groupBy snd |> Seq.fold (fun es (_,ps) -> add_binding es ps) Set.empty
    { Species.strands = species_strands
    ; Species.edges = species_edges }
  let ss = complexes
            |> List.map (fun (i, c) -> c |> complex_to_species |> Species.split_ccs strands |> Seq.map fst |> all_maps_to i)
            |> Seq.concat |> Seq.groupBy fst
            |> Seq.map (fun (s, l) -> Seq.sumBy snd l, s)
  enzymes, toehold_map, strands, ss

let compile = parse >> semantics
let tryCompile s =
  match Parser.run ExtendedSyntax.parser_enzymes s with
  | Parser.Success (result, r, p) -> Parser.Success (semantics result, r, p)
  | Parser.Failure (error, m, r) -> Parser.Failure (error, m, r)

let lift_tiles (strands: Strands.t) =
  let bound_strand_types = strands.strand_types |> Array.map (Array.exists (fun d -> d.name = "tether"))
  let is_bound (Species.Rep sg) = sg.sg.strands |> Array.exists (fun i -> bound_strand_types.[i])
  Tiles.lift is_bound

let calculus_detailed s =
  let enzymes, toehold_map, strands, ss = compile s
  Reactions.calculus enzymes toehold_map strands (ss |> Seq.map snd |> Seq.toList)
  //|> lift_tiles strands

let calculus_default s =
  let enzymes, toehold_map, strands, ss = compile s
  let fast_mono sg =
    [ Reactions.displacing_actions strands sg
    ; Reactions.complementary_binding_actions strands sg ]
    |> Seq.concat
    |> Set.ofSeq
    |> Set.map (Reactions.to_calc_react toehold_map strands)
  let slow_mono sg =
    [ Reactions.isolated_toehold_unbinding_actions strands sg
    ; enzymes |> Seq.collect (fun e -> Reactions.nicking_actions strands e sg) ]
    |> Seq.concat
    |> Seq.toList
    |> List.map (Reactions.to_calc_react toehold_map strands)
  Reactions.calculus enzymes toehold_map strands (ss |> Seq.map snd |> Seq.toList)
  |> TimeSeparation.lift fast_mono slow_mono
  //|> lift_tiles strands

let reaction_graph limit enzymes toehold_map strands ss =
  let positivite_propensity = Reactions.get_rate toehold_map strands >> (<) 0.0
  let rec work rs all_ss l =
    match l with
    | [] -> all_ss, rs
    | w::ws ->
      if limit > 0 && (Set.count all_ss > limit) then (w::ws |> Set.ofList |> Set.union all_ss), rs else
      printfn "Species: %d" (Set.count all_ss)
      let new_rs = Reactions.actions enzymes strands all_ss w - rs |> Set.filter positivite_propensity
      let is_new s = Set.contains s all_ss |> not
      let new_ss = new_rs |> Seq.collect Reactions.products |> Seq.map fst |> Seq.distinct |> Seq.filter is_new |> Seq.toList
      work (Set.union new_rs rs) (Set.add w all_ss) (ws @ new_ss)
  work Set.empty Set.empty ss

let reaction_graph_to_dot limit enzymes toehold_map strands ss =
  let all_ss, rs = reaction_graph limit enzymes toehold_map strands ss
  let init_ss = ss |> Set.ofList
  let sname = sprintf "cluster%d"
  let r_att = "[penwidth=3.0]"
  let style = Some "style=\"rounded,filled\"\nfillcolor=beige\ncolor=cornsilk3\npenwidth=2.0"
  let init_style = Some "style=\"rounded,filled\"\nfillcolor=dodgerblue4\ncolor=dodgerblue4\npenwidth=2.0\nedge [color=beige]"
  let sid s = Seq.findIndex ((=) s) all_ss |> sname
  let rl = function
  | Reactions.Isolated_toehold _ -> "TU"
  | Reactions.Complement_int _ -> "IB"
  | Reactions.Complement_ext _ -> "EB"
  | Reactions.Displacing _ -> "D"
  | Reactions.Nick _ -> "N"
  let rr = Reactions.get_rate toehold_map strands
  let r_to_n rid r = sprintf "r%d [label=\"%s: %.3f\"];" rid (rl r) (rr r)
  let r_to_e rid = function
  | Reactions.Isolated_toehold (r,_,ps) -> [sprintf (*"%s -> r%d%s"*)"%s -- r%d%s" (sid r) rid r_att; sprintf (*"r%d -> {%s}"*)"r%d -- {%s}" rid (ps |> Seq.map (fst >> sid) |> String.concat " ")]
  | Reactions.Complement_int (r,_,p) ->  [sprintf (*"%s -> r%d%s"*)"%s -- r%d%s" (sid r) rid r_att; sprintf (*"r%d -> %s"*)"r%d -- %s" rid (p |> fst |> sid)]
  | Reactions.Complement_ext (r1,r2,_,p) ->  [sprintf (*"{%s %s} -> r%d%s"*)"{%s %s} -- r%d%s" (sid r1) (sid r2) rid r_att; sprintf (*"r%d -> %s"*)"r%d -- %s" rid (p |> fst |> sid)]
  | Reactions.Displacing (r,_,ps) ->  [sprintf (*"%s -> r%d%s"*)"%s -- r%d%s" (sid r) rid r_att; sprintf (*"r%d -> {%s}"*)"r%d -- {%s}" rid (ps |> Seq.map (fst >> sid) |> String.concat " ")]
  | Reactions.Nick (r,_,p) ->  [sprintf (*"%s -> r%d%s"*)"%s -- r%d%s" (sid r) rid r_att; sprintf (*"r%d -> %s"*)"r%d -- %s" rid (p |> fst |> sid)]
  seq {
    yield "graph reaction_graph {"
    yield "layout=fdp"
    yield! all_ss |> Seq.mapi (fun i s -> Species.to_dot_node strands s (sprintf "subgraph %s" (sname i)) (if Set.contains s init_ss then init_style else style) (sprintf "sp_%d_str%d" i) )  
    yield "node [style=filled,fillcolor=azure,color=dodgerblue4,penwidth=3.0];"
    yield "edge [color=dodgerblue4];"
    yield! rs |> Seq.mapi r_to_n
    yield! rs |> Seq.mapi r_to_e |> Seq.concat
    yield "}"
  } |> String.concat "\n"

#if JavaScript
#else
let reaction_graph_to_svgdot out_dir limit enzymes toehold_map strands ss =
  let all_ss, rs = reaction_graph limit enzymes toehold_map strands ss
  let init_ss = ss |> Set.ofList
  let sname = sprintf "species%d"
  let r_att = "[penwidth=2.0,dir=none]"
  let style = ""
  let init_style = ",penwidth=\"3.0\""
  let sid s = Seq.findIndex ((=) s) all_ss |> sname
  let rr = Reactions.get_rate toehold_map strands
  let r_to_n rid r = sprintf "r%d [label=\"%.3f\"];" rid (rr r)
  let r_to_e rid = function
  | Reactions.Isolated_toehold (r,_,ps) -> [sprintf "%s -> r%d%s" (sid r) rid r_att; sprintf "r%d -> {%s}[arrowhead=onormal]" rid (ps |> Seq.map (fst >> sid) |> String.concat " ")]
  | Reactions.Complement_int (r,_,p) ->  [sprintf "%s -> r%d%s" (sid r) rid r_att; sprintf "r%d -> %s[arrowhead=onormal]" rid (p |> fst |> sid)]
  | Reactions.Complement_ext (r1,r2,_,p) ->  [sprintf "{%s %s} -> r%d%s" (sid r1) (sid r2) rid r_att; sprintf "r%d -> %s[arrowhead=onormal]" rid (p |> fst |> sid)]
  | Reactions.Displacing (r,_,ps) ->  [sprintf "%s -> r%d%s" (sid r) rid r_att; sprintf "r%d -> {%s}[arrowhead=onormal]" rid (ps |> Seq.map (fst >> sid) |> String.concat " ")]
  | Reactions.Nick (r,_,p) ->  [sprintf "%s -> r%d%s" (sid r) rid r_att; sprintf "r%d -> %s[arrowhead=onormal]" rid (p |> fst |> sid)]
  all_ss |> Seq.iteri (fun i s -> Species.export_svg (System.IO.Path.Combine(out_dir, (sprintf "%s.svg" (sname i)))) strands s)
  ( all_ss
  , rs
  , seq {
      yield "digraph reaction_graph {"
      yield! all_ss |> Seq.mapi (fun i s -> sprintf "%s [image=\"%s.png\",label=\"\",shape=\"box\",style=\"rounded\"%s];" (sname i) (sname i) (if Set.contains s init_ss then init_style else style))
      yield "node [shape=box];"
      yield! rs |> Seq.mapi r_to_n
      yield! rs |> Seq.mapi r_to_e |> Seq.concat
      yield "}"
    } |> String.concat "\n" )
#endif

let state_space_to_dot strands (states, transitions) species =
  let sname = sprintf "cluster%d_%d"
  let st_name = sprintf "cluster%d"
  let r_att = "[penwidth=3.0]"
  let style = "style=\"rounded,filled\"\nfillcolor=beige\ncolor=cornsilk3\npenwidth=2.0"
  let state_seq = states |> Set.toSeq
  let st_id s = Seq.findIndex ((=) s) state_seq |> st_name
  let start_state = States.state_of_species species
  let state_to_dot i st =
    seq {
      yield sprintf "subgraph cluster%d {" i;
      if st = start_state then yield "style=filled\nfillcolor=dodgerblue4"
      yield "color=dodgerblue4"
      yield "penwidth=3.0"
      yield! st |> Map.toSeq |> Seq.mapi (fun j (s,n) -> Species.to_dot_node strands s (sprintf "subgraph %s" (sname i j)) (sprintf "label=\"%d\"\n%s" n style |> Some) (sprintf "st_%d_sp_%d_str%d" i j) )  
      yield "}"
    } |> String.concat "\n"
  seq {
    yield "graph state_space {";
    yield "layout=fdp"
    yield! state_seq |> Seq.mapi state_to_dot
    yield "node [style=filled,fillcolor=azure,color=dodgerblue4,penwidth=3.0];"
    yield "edge [color=dodgerblue4];"
    yield! transitions |> Seq.mapi (fun i (_, r, _) -> sprintf "r%d [label=\"%.3f\"];" i r)
    yield! transitions |> Seq.mapi (fun i (s1, _, s2) -> seq [sprintf "%s -- r%d%s" (st_id s1) i r_att; sprintf "r%d -- %s" i (st_id s2)]) |> Seq.concat
    yield "}"
  } |> String.concat "\n"

#if JavaScript
#else
let state_space_to_svgdot strands (states, transitions) species =
  let all_species = states |> Set.toSeq |> Seq.collect (fun st -> st |> Map.toSeq |> Seq.map fst) |> Seq.distinct
  let sname = sprintf "ss_species%d"
  all_species |> Seq.iteri (fun i s -> Species.export_svg (sprintf "%s.svg" (sname i)) strands s)
  let sp_name_map = all_species |> Seq.mapi (fun i s -> s, i) |> Map.ofSeq
  let sp_name s = Map.find s sp_name_map |> sname
  let st_name = sprintf "state%d"
  let state_seq = states |> Set.toSeq
  let st_id s = Seq.findIndex ((=) s) state_seq |> st_name
  let start_state = States.state_of_species species
  let state_to_dot i st =
    seq {
      yield sprintf "%s [label=<" (st_name i);
      yield "<table border=\"0\" cellborder=\"0\" cellspacing=\"0\" cellpadding =\"0\">"
      yield! st |> Map.toSeq |> Seq.map (fun (s,n) -> sprintf "<tr>\n  <td>%d</td>\n  <td><img src=\"%s.png\"/></td>\n</tr>" n (sp_name s))
      yield sprintf "</table>>%s];" (if st = start_state then ",penwidth=\"3.0\"" else "")
    } |> String.concat "\n"
  seq {
    yield "digraph state_space {";
    yield "node [shape=box];"
    yield! state_seq |> Seq.mapi state_to_dot
    yield! transitions |> Seq.mapi (fun i (s1, r, s2) -> sprintf "%s -> %s [label=\" %.3f \"];" (st_id s1) (st_id s2) r)
    yield "}"
  } |> String.concat "\n"
#endif

#if JavaScript
#else
let state_space_to_textdot strands (states, transitions) species =
  let all_species = states |> Set.toSeq |> Seq.collect (fun st -> st |> Map.toSeq |> Seq.map fst) |> Seq.distinct
  let sname i s = sprintf "ss_species%d\t%s" i (Species.to_html strands s)
  //all_species |> Seq.iteri (fun i s -> Species.export_svg (sprintf "%s.svg" (sname i)) strands s)
  let sp_name_map = all_species |> Seq.mapi (fun i s -> s, sname i s) |> Map.ofSeq
  let sp_name s = Map.find s sp_name_map
  let st_name = sprintf "state%d"
  let state_seq = states |> Set.toSeq
  let st_id s = Seq.findIndex ((=) s) state_seq |> st_name
  let start_state = States.state_of_species species
  let state_to_dot i st =
    seq {
      yield sprintf "%s [label=<" (st_name i);
      yield "<table border=\"0\" cellborder=\"0\" cellspacing=\"0\" cellpadding =\"0\">"
      yield! st |> Map.toSeq |> Seq.map (fun (s,n) -> sprintf "<tr>\n  <td>%d</td>\n  <td>%s</td>\n</tr>" n (sp_name s))
      yield sprintf "</table>>%s];" (if st = start_state then ",penwidth=\"3.0\"" else "")
    } |> String.concat "\n"
  seq {
    yield "digraph state_space {";
    yield "node [shape=box];"
    yield! state_seq |> Seq.mapi state_to_dot
    yield! transitions |> Seq.mapi (fun i (s1, r, s2) -> sprintf "%s -> %s [label=\" %.3f \"];" (st_id s1) (st_id s2) r)
    yield "}"
  } |> String.concat "\n"
#endif

#if JavaScript
#else
let state_space_to_text strands (states, transitions) species =
  let all_species = states |> Set.toSeq |> Seq.collect (fun st -> st |> Map.toSeq |> Seq.map fst) |> Seq.distinct
  let sname i s = sprintf "species%-2d: %s" i (Species.to_string_i 19 strands s)
  //all_species |> Seq.iteri (fun i s -> Species.export_svg (sprintf "%s.svg" (sname i)) strands s)
  let sp_name_map = all_species |> Seq.mapi (fun i s -> s, sname i s) |> Map.ofSeq
  let sp_name s = Map.find s sp_name_map
  let st_name = sprintf "state%-2d"
  let state_seq = states |> Set.toSeq
  let st_id s = Seq.findIndex ((=) s) state_seq |> st_name
  let start_state = States.state_of_species species
  let state_to_text i st =
    seq {
      yield sprintf "%s%s:\n" (st_name i) (if st = start_state then " (Initial state)" else "")
      yield! st |> Map.toSeq |> Seq.map (fun (s,n) -> sprintf "  %2d of %s" n (sp_name s))
    } |> String.concat "\n"
  seq {
    yield "State space";
    yield sprintf "  species: %d" (Seq.length all_species)
    yield sprintf "  states: %d" (Seq.length state_seq)
    yield sprintf "  transitions: %d" (Seq.length transitions)
    yield "\nStates";
    yield! state_seq |> Seq.mapi state_to_text
    yield "\nTransitions";
    yield! transitions |> Seq.mapi (fun i (s1, r, s2) -> sprintf "%s -> %s (%.3f)" (st_id s1) (st_id s2) r)
  } |> String.concat "\n"
#endif

let to_dot strands ss =
  seq {
    yield "graph g {";
    yield! ss |> Seq.mapi (fun i s -> Species.to_dot_node strands s (sprintf "subgraph cluster_species%d" i) None (sprintf "sp_%d_str%d" i) )
    yield "}"
  } |> String.concat "\n"
