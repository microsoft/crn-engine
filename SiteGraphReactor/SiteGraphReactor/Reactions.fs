// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Reactions

type edge = Species.edge
type nick = Strands.domain * Strands.domain

type rep_perm = Species.rep * Map<int,int>

type t =
  | Isolated_toehold of Species.rep * edge * rep_perm list
  | Complement_int of Species.rep * edge * rep_perm
  | Complement_ext of Species.rep * Species.rep * edge * rep_perm
  | Displacing of Species.rep * edge list * rep_perm list
  | Nick of Species.rep * Strands.port * rep_perm

let reactants = function
  | Isolated_toehold (p,_,_) -> [p]
  | Complement_int (p,_,_) -> [p]
  | Complement_ext (p1,p2,_,_) -> [p1;p2]
  | Displacing (p,_,_) -> [p]
  | Nick (p,_,_) -> [p]

let products = function
  | Isolated_toehold (_,_,ps) -> ps
  | Complement_int (_,_,p) -> [p]
  | Complement_ext (_,_,_,p) -> [p]
  | Displacing (_,_,ps) -> ps
  | Nick (_,_,p) -> [p]

let is_binding_edge strands sg (np1:Strands.port, np2:Strands.port) =
  Species.is_legal_edge strands sg (np1, np2) &&
  sg.edges |> Set.contains (np1, np2)

let left_neighbour (p1:Strands.port, p2:Strands.port) =
    ({Strands.port.strand=p1.strand; Strands.port.site=p1.site-1},
     {Strands.port.strand=p2.strand; Strands.port.site=p2.site+1})
let right_neighbour (port1:Strands.port, port2:Strands.port) =
    ({Strands.port.strand=port1.strand; Strands.port.site=port1.site+1},
     {Strands.port.strand=port2.strand; Strands.port.site=port2.site-1})

let adjacent_left strands sg e =
  e |> left_neighbour |> is_binding_edge strands sg

let adjacent_right strands sg e =
  e |> right_neighbour |> Species.normalize_edge |> is_binding_edge strands sg // normalize is needed if n1=n2 and p2=p1+1

let has_adjacent strands sg e = adjacent_left strands sg e || adjacent_right strands sg e
let isolated strands sg e = has_adjacent strands sg e |> not

let is_toehold_edge (strands:Strands.t) (sg:Species.strand_graph) (p1:Strands.port, p2:Strands.port) =
  strands.admissible_edges |> Map.find (Species.normalize_edge ({Strands.port.strand=sg.strands.[p1.strand]; Strands.port.site=p1.site}, {Strands.port.strand=sg.strands.[p2.strand]; Strands.port.site=p2.site}) |> fun edge -> {Strands.port_pair.port1=fst edge;Strands.port_pair.port2=snd edge})

let isolated_toehold strands sg e = is_toehold_edge strands sg e && isolated strands sg e 

let aproximal_toehold strands (s:Species.t) e = is_toehold_edge strands s.sg e && (Set.contains e s.proximal |> not)

let in_hairpin (sg:Species.strand_graph) (n,p) =
  sg.edges |> Set.filter (fun (p1, p2) -> p1.strand = n && p1.site < p && p2.strand = n && p < p2.site) |> Set.isEmpty |> not

let in_bulge (sg:Species.strand_graph) (n,p) =
  sg.edges |> Set.toSeq |> Seq.choose (fun ((p1:Strands.port), (p2:Strands.port)) -> if p1.strand = n && p1.site < p then Some p2
                                                                                     elif p2.strand = n && p2.site < p then Some p1
                                                                                     else None)
           |> Seq.filter (fun port -> sg.edges |> Set.exists (fun (port1a,port2a) -> (port.strand = port1a.strand && n = port2a.strand && p < port2a.site && port2a.site < port.site) || (port.strand = port2a.strand && n = port2a.strand && p < port1a.site && port2a.site < port.site)))
           |> Seq.isEmpty |> not

let hidden sg np = in_hairpin sg np || in_bulge sg np

// Toeholds unbinding
let isolated_toehold_bindings strands sg = Set.filter (isolated_toehold strands sg) sg.edges
let aproximal_toehold_bindings strands s = Set.filter (aproximal_toehold strands s) s.sg.edges
let isolated_toehold_unbinding_actions strands (Species.Rep s as r) =
  let build (e:edge) = (r, e, Species.break_bond strands s.sg e) |> Isolated_toehold
  aproximal_toehold_bindings strands s |> Set.map build |> Set.toSeq

let complementary_unbounds strands (s:Species.t) =
  let edge_free (p1, p2) = not (Set.contains p1 s.bound_ports || Set.contains p2 s.bound_ports)
  s.admissible_bindings |> Set.filter edge_free

let complementary_binding_actions strands (Species.Rep s as r) =
  let act e =
    { s.sg with Species.edges = Set.add e s.sg.edges } |> Species.rep strands
  let build e = (r, e, act e) |> Complement_int
  complementary_unbounds strands s |> Set.map build |> Set.toSeq

// Bindings across species
let admissible_cross_bindings (strands:Strands.t) (sg1:Species.strand_graph) (sg2:Species.strand_graph) =
  let possible_bindings a b =
    let ta = sg1.strands.[a]
    let tb = sg2.strands.[b]
    let choose_edge (pair : Strands.port_pair) =
      let t1, p1_, t2, p2_ = if tb < ta then pair.port2.strand, pair.port2.site, pair.port1.strand, pair.port1.site else pair.port1.strand, pair.port1.site, pair.port2.strand, pair.port2.site
      if ta=t1 && tb=t2 then Some {Strands.port_pair.port1={Strands.port.strand=a;Strands.port.site=p1_}; Strands.port_pair.port2={Strands.port.strand=b;Strands.port.site=p2_}} else None 
    strands.admissible_edges |> Map.toSeq |> Seq.choose (fst >> choose_edge) |> Set.ofSeq
  seq {
    for i in 0..sg1.strands.Length-1 do
      for j in 0..sg2.strands.Length-1 do
        yield possible_bindings i j
  } |> Set.unionMany

let complementary_cross_unbounds strands (s1:Species.t) (s2:Species.t) =
  let edge_free (pair:Strands.port_pair) = not (Set.contains pair.port1 s1.bound_ports || Set.contains pair.port2 s2.bound_ports)
  admissible_cross_bindings strands s1.sg s2.sg |> Set.filter edge_free |> Set.map (fun pair -> pair.port1, pair.port2)

let complementary_cross_binding_actions strands (Species.Rep s1 as r1) (Species.Rep s2 as r2) =
  let edge_to_pair (edge:edge) = {Strands.port_pair.port1=fst edge; Strands.port_pair.port2=snd edge}
  let build (edge:edge) = (r1, r2, edge, Species.form_bond strands s1.sg s2.sg edge) |> Complement_ext
  complementary_cross_unbounds strands s1 s2 |> Set.map build |> Set.toSeq

type path = Strands.port * edge list * Strands.port

let do_all f es m = Seq.fold f m es

// Displacing actions
let displacing_paths strands (s:Species.t) =
  (*let can_be_added = s.proximal - s.sg.edges*)
  let one_free (p1, p2) = (Set.contains p1 s.bound_ports && Set.contains p2 s.bound_ports) |> not
  let can_be_added = s.proximal - s.sg.edges |> Set.filter (fun e -> one_free e || has_adjacent strands s.sg e)
  let can_be_removed = s.sg.edges

  // Extension: loops can be closed by unanchored edges
  let can_close_loops = s.admissible_bindings - can_be_added

  // build map from ports to paths from this port
  // invariant is that active paths have edges from can_be_removed at both ends
  // start from can_be_removed
  let add_path (np:Strands.port) (p:edge list * Strands.port) m =
    match Map.tryFind np m with
    | None -> Map.add np [p] m
    | Some paths -> Map.add np (p::paths) m

  let add_rem m ((np1, np2) as e) =
    m |> add_path np1 ([e], np2)
      |> add_path np2 ([e], np1)

  let m = can_be_removed |> Seq.fold add_rem Map.empty

  // to be applied to completed paths in order to achieve vectorised semantics
  // TODO: flip edges to direction of path rather than normalised direction

  // returns None if there is no neighbouring displacing path
  let admits_adding = s.admissible_bindings - s.sg.edges
  let fatten_once nb (sp, path:Species.edge list) =
    let nsp = (sp,sp) |> nb |> fst
    let rec loop to_add acc p = function
    | [] ->
        if not <| Set.contains p s.bound_ports then
          Some (nsp, acc)
        else None // could try to extend the path here
    | (pa,pb)::es ->
        let e, np =
          let flip = (p = pb) <> to_add
          if flip then (pb,pa), pa
          else (pa,pb), pb
        let n = nb e |> Species.normalize_edge
        if to_add && Set.contains n admits_adding then
          loop false (n::acc) np es
        elif (not to_add) && Set.contains n can_be_removed then
          loop true (n::acc) np es
        elif (not to_add) && acc <> [] && (not <| Set.contains p s.bound_ports) then
          Some (nsp, acc)
        else None
    loop false [] sp path
  
  let fatten_up fo path =
    let rec loop acc (sp: Strands.port, p: Species.edge list) =
      match fo (sp, p) with
      | None -> acc
      | Some (np,es) -> loop (es::acc) (np,es)
    loop [] path

  let fatten sp path =
    let left_fat = fatten_up (fatten_once left_neighbour) (sp, path)
    let right_fat = fatten_up (fatten_once right_neighbour) (sp, path)
    path::left_fat@right_fat |> List.concat // all edges are inverted when the reaction is enacted

  let fatten_loop_once nb (sp, path:Species.edge list) =
    let nsp = (sp,sp) |> nb |> fst
    let rec loop to_add acc p = function
    | [] -> Some (nsp, acc)
    | (pa,pb)::es ->
        let e, np =
          if p = pa then (pa,pb), pb
          else (pb,pa), pa
        let n = nb e
        if to_add && Set.contains n admits_adding then
          loop false (n::acc) np es
        elif (not to_add) && Set.contains n can_be_removed then
          loop true (n::acc) np es
        else None
    loop false [] sp path
  
  let fatten_loop sp path =
    let left_fat = fatten_up (fatten_loop_once left_neighbour) (sp, path)
    let right_fat = fatten_up (fatten_loop_once right_neighbour) (sp, path)
    path::left_fat@right_fat |> List.concat // all edges are inverted when the reaction is enacted

  let add_edge (paths, m) ((np1, np2) as e) =
    match Map.tryFind np1 m, Map.tryFind np2 m with
    | None, None -> (paths, m)
    | None, Some ps2 -> // paths can be terminated at np1
      let ps = ps2 |> List.map (fun (es, ep) -> e::es |> List.rev |> fatten ep)
      (ps@paths, m)
    | Some ps1, None -> // paths can be terminated at np2
      let ps = ps1 |> List.map (fun (es, ep) -> e::es |> List.rev |> fatten ep)
      (ps@paths, m)
    | Some ps1, Some ps2 -> // paths should be merged, check for loops
      let split_loops sp = List.partition (fun (_:edge list,ep) -> e = Species.normalize_edge (sp, ep))
      let ps1_loops, ps1_paths = ps1 |> split_loops np1
      let ps2_loops, ps2_paths = ps2 |> split_loops np2
      let non_crossing (es1, ep1:Strands.port) (es2, ep2:Strands.port) =
        let ports1 = es1 |> List.map (fun (a:Strands.port,b) -> Set.ofList [a; b]) |> Set.unionMany
        let ports2 = es2 |> List.map (fun (a,b) -> Set.ofList [a; b]) |> Set.unionMany
        Set.intersect ports1 ports2 |> Set.isEmpty
      let pairs_to_merge =
        ps1_paths
        |> List.fold
            (fun ps_o p1 ->
               ps2_paths
               |> List.fold
                    (fun ps_i p2 ->
                       if non_crossing p1 p2 then
                         (p1, p2)::ps_i
                       else ps_i)
                    ps_o)
            []
      let merged_paths = pairs_to_merge |> List.collect (fun ((es1,ep1),(es2,ep2)) -> [(ep1, (List.rev es1)@[e]@es2, ep2); (ep2, (List.rev es2)@[e]@es1, ep1)])
      let loops = ps1_loops |> List.map (fun (es,ep) -> e::es |> List.rev |> fatten_loop ep)
      let new_m = merged_paths |> List.fold (fun m (sp,es,ep) -> add_path sp (es, ep) m) m
      (loops@paths, new_m)

  let close_loops (paths: Species.edge list list, m) ((np1, np2) as e) =
    match Map.tryFind np1 m, Map.tryFind np2 m with
    | Some ps, Some _ -> // only interesting case, check for loops
      let ps_loops = ps |> List.filter (fun (es, ep) -> ep = np2 && List.length es > 2)
      let loops = ps_loops |> List.map (fun (es,_) -> e::es)
      (loops@paths, m)
    | _, _ -> (paths, m)

  ([], m) |> do_all add_edge can_be_added
          |> do_all close_loops can_close_loops // Enable this extension
          |> fst |> Seq.distinct

  //can_be_added |> Seq.fold add_edge ([], m) |> fst |> Seq.distinct

let invert edge_set e =
  if Set.contains e edge_set
  then Set.remove e edge_set
  else Set.add e edge_set

let act_displacing strands es sg =
    let sg_displaced = { sg with Species.edges = es |> List.fold invert sg.edges }
    Species.split_ccs strands sg_displaced |> List.ofSeq // split into connected components

let displacing_actions strands (Species.Rep s as r) =
  let build es = (r, es, act_displacing strands es s.sg) |> Displacing
  displacing_paths strands s |> Seq.map build

let nicking_actions (strands:Strands.t) (before_nick, after_nick) (Species.Rep s as r) =
  let to_nick = Array.append before_nick after_nick
  let shift = before_nick.Length-1
  let nick_sites st = strands.strand_types.[st] |> Seq.windowed to_nick.Length
                                                |> Seq.mapi (fun i w -> i+shift, w = to_nick)
                                                |> Seq.filter snd |> Seq.map fst
                                                |> Seq.toArray
  let sites = s.sg.strands |> Array.mapi (fun i st -> i, nick_sites st)

  let site_valid (i, b) =
    s.sg.edges |> Set.toSeq
               |> Seq.choose (fun (np1, np2) -> if np1 = {Strands.port.strand=i;Strands.port.site=b-shift} then Some np2 else if np2 = {Strands.port.strand=i;Strands.port.site=b-shift} then Some np1 else None)
               |> Seq.filter (fun port -> ([1..to_nick.Length-1] |> List.map (fun j -> {Strands.port.strand=port.strand; Strands.port.site=port.site-j}, {Strands.port.strand=i;Strands.port.site=b-shift+j})) |> List.map Species.normalize_edge
                                           |> List.forall (fun e -> Set.contains e s.sg.edges))
               |> Seq.isEmpty |> not
  
  let build (i,b) =
    let st_i = strands.strand_types.[s.sg.strands.[i]]
    let prefix = Array.init (b+1) (fun d -> st_i.[d])
    let prefix_type = Array.findIndex ((=) prefix) strands.strand_types
    let postfix = Array.init (st_i.Length-b-1) (fun d -> st_i.[d+b+1])
    let postfix_type = Array.findIndex ((=) postfix) strands.strand_types

    let map_port (port:Strands.port) =
      if port.strand = i && port.site > b then {Strands.port.strand=port.strand+1; Strands.port.site=port.site-b-1}
      elif port.strand > i then {Strands.port.strand=port.strand+1; Strands.port.site=port.site}
      else port
    let map_edge (np1:Strands.port, np2:Strands.port) = map_port np1, map_port np2
    //let map_edge_b (e,b) = map_edge e, b

    let strand_type n =
      if n = i then prefix_type
      elif n = i+1 then postfix_type
      elif n > i+1 then s.sg.strands.[n-1]
      else s.sg.strands.[n]
    let new_sg =
      { Species.strands = Array.init (s.sg.strands.Length+1) strand_type
      ; Species.edges = s.sg.edges |> Set.map map_edge }
    
    Nick (r, {Strands.port.strand=i;Strands.port.site=b}, Species.rep strands new_sg)

  sites |> Array.collect (fun (i,bs) -> bs |> Array.map (fun b -> i,b))
        |> Array.filter site_valid
        |> Seq.map build

let mono_actions enzymes strands sg =
  Seq.concat [ isolated_toehold_unbinding_actions strands sg
             ; complementary_binding_actions strands sg
             ; displacing_actions strands sg
             ; enzymes |> Seq.collect (fun e -> nicking_actions strands e sg) ]
   |> Set.ofSeq

let bin_actions enzymes strands sg1 sg2 =
  complementary_cross_binding_actions strands sg1 sg2
    |> Set.ofSeq

let actions enzymes strands sgs sg =
  seq {
    yield mono_actions enzymes strands sg
    yield bin_actions enzymes strands sg sg
    yield! sgs |> Seq.map (bin_actions enzymes strands sg)
  } |> Set.unionMany
  (*
  Seq.concat [ isolated_toehold_unbinding_actions strands sg
             ; complementary_binding_actions strands sg
             ; displacing_actions strands sg
             ; complementary_cross_binding_actions strands sg sg
             ; sgs |> Seq.collect (complementary_cross_binding_actions strands sg)
             ; enzymes |> Seq.collect (fun e -> nicking_actions strands e sg) ]
   |> Set.ofSeq
   *)
let get_binding_rate (toehold_map:Map<string,float*float>) (strands : Strands.t) (sg1 : Species.strand_graph) (sg2 : Species.strand_graph) ((port1,port2) as e:edge) =
  if hidden sg1 (port1.strand, port1.site) || hidden sg2 (port2.strand,port2.site) then 0.0
  else Map.find strands.strand_types.[sg1.strands.[port1.strand]].[port1.site].name toehold_map |> fst

let get_unbinding_rate (toehold_map:Map<string,float*float>) (strands : Strands.t) (sg : Species.strand_graph) ((port1,_) as e:edge) =
  Map.find strands.strand_types.[sg.strands.[port1.strand]].[port1.site].name toehold_map |> snd

let get_rate toehold_map strands = function
  | Isolated_toehold (Species.Rep s, e, _) -> get_unbinding_rate toehold_map strands s.sg e
  | Complement_int (Species.Rep s, e, _) -> get_binding_rate toehold_map strands s.sg s.sg e
  | Complement_ext (Species.Rep s1, Species.Rep s2, e, _) -> get_binding_rate toehold_map strands s1.sg s2.sg e
  | Displacing (_, _, _) -> 1.0 //20.0
  | Nick _ -> 3.0

let to_calc_react toehold_map strands r =
  { Calculus.reactants = r |> reactants
  ; Calculus.products  = r |> products |> List.map fst
  ; Calculus.rate      = r |> get_rate toehold_map strands }
let calculus enzymes toehold_map strands =
  let mono = mono_actions enzymes strands >> Set.map (to_calc_react toehold_map strands)
  let bin s1 s2 = bin_actions enzymes strands s1 s2 |> Set.map (to_calc_react toehold_map strands)
  Calculus.from_mono_bin mono bin

// Visualization

let no _ = false

let to_dot_node strands title attr sid = function
  | Isolated_toehold (r, e, _) ->
    Species.to_dot_node_mod strands r title attr sid ((=) e) Set.empty no
  | Complement_int (r, e, _) ->
    Species.to_dot_node_mod strands r title attr sid no (Set.singleton e) no
  | Complement_ext (r1, r2, (port1,port2), _) ->
    let d1 = Species.to_dot_node_mod strands r1 "subgraph g1" None (sid >> sprintf "s1_%s") no Set.empty no
    let d2 = Species.to_dot_node_mod strands r2 "subgraph g2" None (sid >> sprintf "s2_%s") no Set.empty no
    seq {
      yield sprintf "%s {" title
      yield Option.fold (sprintf "%s %s") "" attr
      yield d1
      yield d2
      yield sprintf "s1_%s:p%d -- s2_%s:p%d%s;" (sid port1.strand) port1.site (sid port2.strand) port2.site Species.add_edge_attr
      yield "}"
    } |> String.concat "\n"
    // add e in green
  | Displacing (r, es, _) ->
    let added, removed = es |> List.mapi (fun i e -> i % 2 = 0, e) |> List.partition fst
    let edges_added = added |> List.map snd |> Set.ofList
    let edges_removed = removed |> List.map snd |> Set.ofList
    let edge_removed e = Set.contains e edges_removed
    Species.to_dot_node_mod strands r title attr sid edge_removed edges_added no
  | Nick (r, p, _) ->
    Species.to_dot_node_mod strands r title attr sid no Set.empty ((=) p)

let to_dot (strands:Strands.t) = to_dot_node strands "graph g" None (sprintf "s%d")

let s_to_dot i render st added_edes =
  let sname = sprintf "cluster%d_%d"
  let style = "style=\"rounded,filled\"\nfillcolor=beige\ncolor=cornsilk3\npenwidth=2.0"
  let sid a = sprintf "st_%d_sp_%d_str%d" i a
  let sp_seq = st |> Map.toSeq
  let sp_id sp =
    let rev_map = sp_seq |> Seq.mapi (fun i (sp,_) -> sp, i) |> Map.ofSeq
    Map.find sp rev_map
  seq {
    yield sprintf "subgraph cluster%d {" i;
    if i = 0 then yield "style=filled\nfillcolor=dodgerblue4"
    yield "color=dodgerblue4"
    yield "penwidth=3.0"
    yield! sp_seq |> Seq.mapi (fun j (s,n) -> render s (sprintf "subgraph %s" (sname i j)) (sprintf "label=\"%d\"\n%s" n style |> Some) (sid j) )
    yield! added_edes |> Set.map (fun ((s1,(ax:Strands.port)),(s2,(by:Strands.port))) -> sprintf "%s:p%d -- %s:p%d%s;" (sid (sp_id s1) ax.strand) ax.site (sid (sp_id s2) by.strand) by.site Species.add_edge_attr)
    yield "}"
  } |> String.concat "\n"

let state_to_dot strands i state = function
  | Isolated_toehold (r, e, _) ->
    let render s title attr sid =
      if s = r then Species.to_dot_node_mod strands r title attr sid ((=) e) Set.empty no
      else Species.to_dot_node strands s title attr sid
    s_to_dot i render state Set.empty
  | Complement_int (r, e, _) ->
    let render s title attr sid =
      if s = r then Species.to_dot_node_mod strands r title attr sid no (Set.singleton e) no
      else Species.to_dot_node strands s title attr sid
    s_to_dot i render state Set.empty
  | Complement_ext (r1, r2, (np1, np2), _) ->
    let render = Species.to_dot_node strands
    s_to_dot i render state (Set.singleton ((r1,np1),(r2,np2)))
  | Displacing (r, es, _) ->
    let added, removed = es |> List.mapi (fun i e -> i % 2 = 0, e) |> List.partition fst
    let edges_added = added |> List.map snd |> Set.ofList
    let edges_removed = removed |> List.map snd |> Set.ofList
    let edge_removed e = Set.contains e edges_removed
    let render s title attr sid =
      if s = r then Species.to_dot_node_mod strands r title attr sid edge_removed edges_added no
      else Species.to_dot_node strands s title attr sid
    s_to_dot i render state Set.empty
  | Nick (r, p, _) ->
    let render s title attr sid =
      if s = r then Species.to_dot_node_mod strands r title attr sid no Set.empty ((=) p)
      else Species.to_dot_node strands s title attr sid
    s_to_dot i render state Set.empty

let trace_to_dot (strands:Strands.t) final rs =
  seq {
    yield "graph g {"
    yield "layout=fdp"
    yield! ["style=filled"; "fillcolor=ivory"]
//    yield! rs |> Seq.mapi (fun i r -> to_dot_node strands (sprintf "subgraph cluster%d" i) None (sprintf "r_%d_str%d" i) r)
    yield! rs |> Seq.mapi (fun i (st,r,_) -> state_to_dot strands i st r)
    yield s_to_dot (rs |> Seq.length) (Species.to_dot_node strands) final Set.empty
    yield "node [style=filled,fillcolor=azure,color=dodgerblue4,penwidth=3.0];"
    yield "edge [color=dodgerblue4,penwidth=3.0];"
    yield! rs |> Seq.mapi (fun i (_,_,p) -> sprintf "cluster%d -- cluster%d; [label=\" %.3f \"];" i (i+1) p)
    yield "}"
  } |> String.concat "\n"

let reindex_sgraph (pi: Map<int,int>) (g: Species.s_graph) =
  let pi_rev = pi |> Map.toSeq |> Seq.map (fun (i, j) -> j, i) |> Map.ofSeq
  { g with
      Species.s_graph.strands = g.strands |> Array.permute (fun i -> Map.find i pi_rev) }

let rep_perm_to_graph strands (r, pi) = Species.to_graph strands r |> reindex_sgraph pi

let stack gs =
  let fold (stacked: Species.s_graph) (g: Species.s_graph) =
    let npoints = stacked.s_points.Length
    let stacked_x, stacked_y = stacked.dim
    let points = Array.append stacked.s_points (g.s_points |> Array.map (fun (x, y) -> x, y + stacked_y))
    let ndoms = stacked.domains.Length
    let strands = Array.append stacked.strands (g.strands |> Array.map (Array.map ((+) ndoms)))
    let g_x, g_y = g.dim
    { Species.s_points = points
    ; Species.s_graph.strands = strands
    ; Species.s_graph.domains = Array.append stacked.domains g.domains
    ; Species.s_graph.dim = max stacked_x g_x, stacked_y + g_y}
  match gs with
  | [] -> failwith "cannot stack empty list of graphs"
  | g::rest -> List.fold fold g rest

let stack_perm strands ss =
  let fold (stacked_m: Map<int,int>) m =
    let n = stacked_m.Count
    let shifted = m |> Map.map (fun _ j -> j + n)
    Map.foldBack Map.add shifted stacked_m
  match ss with
  | [] -> failwith "cannot stack empty list of species"
  | (s,p)::rest -> 
    let rs, ps = rest |> List.unzip
    let stacked_p = List.fold fold p ps
    (s::rs) |> List.map (Species.to_graph strands) |> stack |> reindex_sgraph stacked_p


let to_svg assigned strands = function
  | Isolated_toehold (r, _, ps) ->
    let gr = r |> Species.to_graph strands
    let gp = ps |> stack_perm strands
    Species.s_graphs_to_svg gr gp
  | Complement_int (r, _, p) ->
    let gr = r |> Species.to_graph strands
    let gp = p |> rep_perm_to_graph strands
    Species.s_graphs_to_svg gr gp
  | Complement_ext (r1, r2, _, p) ->
    let gr = [ Species.to_graph strands r1
             ; Species.to_graph strands r2 ] |> stack
    let gp = p |> rep_perm_to_graph strands
    Species.s_graphs_to_svg gr gp
  | Displacing (r, _, ps) ->
    let gr = r |> Species.to_graph strands
    let gp = ps |> stack_perm strands
    Species.s_graphs_to_svg gr gp
  | Nick (r, _, (p,_)) ->
    let gr = r |> Species.to_graph strands
    let gp = p |> Species.to_graph strands
    [ Species.wrap_group (Species.dom_style assigned strands) gr
    ; Species.wrap_group (Species.dom_style assigned strands) gp ]
     |> String.concat "<h1>&#8594;</h1>"