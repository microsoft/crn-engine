// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Species

let safe = true
let debug = false

// A connected strand graph
// The field [strands] refers to strand types in a Strands.t object
type edge = Strands.port * Strands.port

type strand_graph =
  { strands : int array
  ; edges : Set<edge> } // add also loop itervals

type t =
  { sg : strand_graph
  ; proximal : Set<edge>
  ; bound_ports : Set<Strands.port>
  ; admissible_bindings : Set<edge> } // add also loop itervals

type rep = Rep of t

let port_to_string (p:Strands.port) = sprintf "%d:%d" p.strand p.site
let edge_to_string (p1, p2) = sprintf "%s -> %s" (port_to_string p1) (port_to_string p2)

let normalize_edge ((p1, p2) as edge:edge) =
  if p1.strand < p2.strand then edge
  else if p2.strand < p1.strand then (p2, p1)
  else ({Strands.port.strand=p1.strand; Strands.port.site=min p1.site p2.site}, {Strands.port.strand=p2.strand; Strands.port.site=max p1.site p2.site})    

let pairup a b = a, b

let map_edge f (p1:Strands.port, p2:Strands.port) =
    ({Strands.port.strand=f p1.strand; Strands.port.site=p1.site},
     {Strands.port.strand=f p2.strand; Strands.port.site=p2.site}) |> normalize_edge

let permute_edges f = Set.map (map_edge f)

let invert ap x = Array.findIndex ((=) x) ap

// helper type for rep
// invariant:
//   [seen_nr] = [seen_map] |> Map.toSeq |> Seq.length
type seen_state =
  { seen_set : Set<int>
  ; seen_map : Map<int,int>
  ; seen_nr  : int }

let state_of_map seen_map =
  let seen_seq = seen_map |> Map.toSeq
  { seen_set = seen_seq |> Seq.map fst |> Set.ofSeq
  ; seen_map = seen_map
  ; seen_nr  = seen_seq |> Seq.length }

let add_map st s =
  { seen_set = st.seen_set
  ; seen_map = st.seen_map |> Map.add s st.seen_nr
  ; seen_nr  = st.seen_nr + 1 }

let add_seen st s =
  { seen_set = st.seen_set |> Set.add s
  ; seen_map = st.seen_map
  ; seen_nr  = st.seen_nr }

// Unfolding a strand graph from a particular strand
// This is used in the definiton of the canonical representation
let unfold_from s a =
  let get_seen seen a =
    match Map.tryFind a seen.seen_map with
    | None -> add_map seen a, seen.seen_nr
    | Some b -> seen, b
  let rec work (uf, seen) l =
    match l with
    | [] -> (List.rev uf, seen.seen_map)
    | w::ws ->
      let link (port1:Strands.port, port2:Strands.port) =
        let sa, sb = s.strands.[port1.strand], s.strands.[port2.strand]
        match port1.strand=w, port2.strand=w with
        | true, true -> Some (w, if port1.site < port2.site then (port1.strand,sa,port1.site,port2.strand,sb,port2.site) else (port2.strand,sb,port2.site,a,sa,port1.site))
        | true, false -> if Set.contains port2.strand seen.seen_set then None else Some (port2.strand, (port1.strand,sa,port1.site,port2.strand,sb,port2.site))
        | false, true -> if Set.contains port1.strand seen.seen_set then None else Some (port1.strand, (port2.strand,sb,port2.site,port1.strand,sa,port1.site))
        | false, false -> None
      let links = s.edges |> Set.toSeq |> Seq.choose link |> Seq.sortBy (fun (_,(_,_,p, _,_,_)) -> p) // TODO: this could be slow, consider representation with neighbours
      let convert (seen, acc) (_:int, (a,sa,i, b,sb,j)) =
        let (seen, a_mapped) = get_seen seen a
        let (seen, b_mapped) = get_seen seen b
        (seen,  j :: b_mapped :: sb :: i :: a_mapped :: sa :: acc)
      let (seen, new_uf) = links |> Seq.fold convert (seen, uf)
      let new_seen = add_seen seen w
      let new_ws = links |> Seq.choose (fun (a,_) -> if a = w then None else Some a) |> Seq.distinct |> Seq.toList
      work (new_uf, new_seen) (ws@new_ws)
  work ([], Map.empty |> Map.add a 0 |> state_of_map) [a]

let min_pi s =
    // Compute the colour to choose within
  let comp (c:int, is:seq<int>) (k, js:seq<int>) =
    match compare (Seq.length is) (Seq.length js) with
    | 0 -> compare c k
    | r -> r
  let pending = ({0..s.strands.Length-1} |> Seq.groupBy (Array.get s.strands) |> Array.ofSeq |> Array.sortWith comp).[0] |> snd |> Set.ofSeq

  // Run algorithm 2 from "Canonical Labelling of Site Graphs"let v = Set.minElement pending
  let v = Set.minElement pending
  let pending = Set.remove v pending
  let (uf_min, pi_min) = unfold_from s v
  let rec work (uf_min, pi_min) pending =
    if Set.isEmpty pending then pi_min
    else
     let v = Set.minElement pending
     let (uf, pi) = unfold_from s v
     let c = Seq.compareWith compare uf uf_min
     if c < 0 then work (uf, pi) (Set.remove v pending)
     else if c > 0 then work (uf_min, pi_min) (Set.remove v pending)
     else
       let new_pending = pending |> Set.filter (fun i -> Set.exists (fun j -> Map.find i pi = Map.find j pi_min) pending)
       work (uf, pi) (Set.remove v new_pending)
  work (uf_min, pi_min) pending

let normalize s =
  let pi = min_pi s
  let p x = Map.find x pi
  ( { strands = s.strands |> Array.permute p
    ; edges = s.edges |> permute_edges p }
  , pi )

let is_legal_port (strands:Strands.t) (sg:strand_graph) (p:Strands.port) =
  p.site >= 0 && p.site < strands.strand_types.[sg.strands.[p.strand]].Length

let is_legal_edge strands sg (np1:Strands.port, np2) =
  is_legal_port strands sg np1 && is_legal_port strands sg np2

(* Old version of proximal, seems faster on the laptop, possibly it is less memory intensive?
let left_neighbour ((n1, p1), (n2, p2)) = ((n1, p1-1), (n2, p2+1))
let right_neighbour ((n1, p1), (n2, p2)) = ((n1, p1+1), (n2, p2-1))

// Rather than proximal left and proximal right, flip the bond
let rec proximal_neighbour strands (sg:strand_graph) edge_to neighbour e0 =
  let edges = Set.remove e0 sg.edges
  let rec proximal ((np1, np2) as e) =
    if edges |> Set.contains (normalize_edge e) then true else
    let (nnp1, nnp2) as ne = neighbour e
    if is_legal_edge strands sg ne |> not then false
    else if edges |> Set.contains (normalize_edge ne) then true
    else
      match edge_to nnp1 with
      | None -> false
      | Some new_np1 -> 
        let new_e = (new_np1, np2) (*|> Species.normalize_edge*)
        if new_e = e0 then false
        else proximal new_e
  proximal e0

let proximal_left strands sg edge_to = proximal_neighbour strands sg edge_to left_neighbour

let proximal_right strands sg edge_to = proximal_neighbour strands sg edge_to right_neighbour

let proximal strands sg edge_to e = proximal_left strands sg edge_to e || proximal_right strands sg edge_to e
*)

let to_set p x = if p x then Set.singleton x else Set.empty
let to_the_left (port:Strands.port) = {Strands.port.strand=port.strand; Strands.port.site=port.site-1}
let to_the_right (port:Strands.port) = {Strands.port.strand=port.strand; Strands.port.site=port.site+1}

let proximal (strands:Strands.t) (sg:strand_graph) admissible_bindings =
  let tl = to_the_left >> to_set (is_legal_port strands sg)
  let tr = to_the_right >> to_set (is_legal_port strands sg)
  let add_bond (close_left, close_right) (np1, np2) =
    let get_left (v, m) = match Map.tryFind v m with None -> tl v | Some x -> x
    let get_right (v, m) = match Map.tryFind v m with None -> tr v | Some x -> x
    let add_close get b c (p:Strands.port) =
      let cb = get (p, c)
      Map.add p (b + cb) c
    let bagl1, bagr1 = get_left (np1, close_left), get_right (np1, close_right)
    let bagl2, bagr2 = get_left (np2, close_left), get_right (np2, close_right)
    let close_left = bagr2 |> Set.fold (add_close get_left bagl1) close_left
    let close_right = bagl1 |> Set.fold (add_close get_right bagr2) close_right
    let close_left = bagr1 |> Set.fold (add_close get_left bagl2) close_left
    let close_right = bagl2 |> Set.fold (add_close get_right bagr1) close_right
    close_left, close_right
  let close_left, close_right = sg.edges |> Set.fold add_bond (Map.empty, Map.empty)
  close_left
    |> Map.toSeq
    |> Seq.map (fun (p, ps) -> ps |> Set.map (fun x -> (p,x) |> normalize_edge))
    |> Set.unionMany
    |> Set.intersect admissible_bindings

let admissible_bindings (strands:Strands.t) (sg:strand_graph) =
  let c_inv = sg.strands |> Array.mapi pairup |> Array.toSeq |> Seq.groupBy snd |> Map.ofSeq |> Map.map (fun _ s -> Seq.map fst s)
  let translate ((p1:Strands.port),(p2:Strands.port)) =
    match Map.tryFind p1.strand c_inv, Map.tryFind p2.strand c_inv with
    | Some cs1, Some cs2 ->
      let ss1 = cs1 |> Seq.map (fun s -> {Strands.port.strand=s;Strands.port.site=p1.site})
      let ss2 = cs2 |> Seq.map (fun s -> {Strands.port.strand=s;Strands.port.site=p2.site})
      ss1 |> Seq.collect (fun (np1:Strands.port) -> ss2 |> Seq.map (fun np2 -> (np1, np2) |> normalize_edge)) |> Some
    | _ -> None
  strands.admissible_edges |> Map.toSeq |> Seq.choose (fun (e,_) -> translate (e.port1, e.port2)) |> Seq.concat |> Set.ofSeq

// Check certain criteria of the species representation
let check (strands:Strands.t) sg =
  let edge_sorted (p1:Strands.port, p2:Strands.port) = 
    if p1.strand = p2.strand then p1.site < p2.site else p1.strand < p2.strand

  // check that edges are admissible
  let check_edge ((p1, p2) as e :edge) =
    if p1.strand < 0 || p1.strand >= sg.strands.Length || p2.strand < 0 || p2.strand >= sg.strands.Length then failwith (sprintf "illegal edge: %s" (edge_to_string e))
    if p1.site < 0 || p1.site >= strands.strand_types.[sg.strands.[p1.strand]].Length then failwith (sprintf "illegal port: %d on strand %d" p1.site p1.strand)
    if p2.site < 0 || p2.site >= strands.strand_types.[sg.strands.[p2.strand]].Length then failwith (sprintf "illegal port: %d on strand %d" p2.site p2.strand)
    if strands.admissible_edges |> Map.containsKey (normalize_edge ({Strands.port.strand=sg.strands.[p1.strand]; Strands.port.site=p1.site}, {Strands.port.strand=sg.strands.[p2.strand];Strands.port.site= p2.site}) |> fun edge -> {Strands.port_pair.port1=fst edge;Strands.port_pair.port2=snd edge}) |> not then failwith (sprintf "phantom edge: %s" (edge_to_string e))
    if edge_sorted e |> not then failwith (sprintf "unsorted edge: %s" (edge_to_string e))
  sg.edges |> Set.iter check_edge;
  
  // check that ports are only used once
  let check_ports used (p1, p2) =
    if p1 = p2 then failwith (sprintf "port used twice in same edge: %s" (port_to_string p1))
    if Set.contains p1 used then failwith (sprintf "port used twice: %s" (port_to_string p1))
    if Set.contains p2 used then failwith (sprintf "port used twice: %s" (port_to_string p2))
    used |> Set.add p1 |> Set.add p2
  sg.edges |> Set.fold check_ports Set.empty |> ignore

let rep strands s =
  // let debug = s.strands.Length
  let sg, pi = normalize s
  if safe then check strands sg

  //printfn "Species with\n\tStrands: %i Edges: %i" s.strands.Length s.edges.Count 

  let admissible_bindings = admissible_bindings strands sg
  (* Old computation
  let edge_map = sg.edges |> Set.fold (fun m (np1, np2) -> m |> Map.add np1 np2 |> Map.add np2 np1) Map.empty
  let edge_to np = Map.tryFind np edge_map
  let proximal = admissible_bindings |> Set.filter (proximal strands sg edge_to)
  *)
  let proximal = proximal strands sg admissible_bindings
  let bound_ports = sg.edges |> Set.fold (fun ports (p1,p2) -> ports |> Set.add p1 |> Set.add p2) Set.empty
  ( { sg = sg
    ; proximal = proximal
    ; bound_ports = bound_ports
    ; admissible_bindings = admissible_bindings } |> Rep
  , pi )

// Split off a connected component
let split s cc =
  let p x = Set.contains x cc
  let kps, lps = s.strands |> Array.mapi pairup |> Array.partition (fst >> p)
  let kp, ks = kps |> Array.unzip
  let lp, ls = lps |> Array.unzip
  let k_edge (p1:Strands.port,(p2:Strands.port)) = kp |> Array.exists (fun i -> p1.strand=i || p2.strand=i)
  let ke, le = s.edges |> Set.partition k_edge // links across will end up in k
  ( ( { strands = ks
      ; edges = ke |> permute_edges (invert kp) }
    , kp |> Array.mapi (fun i j -> j, i) |> Map.ofArray )
  , ( { strands = ls
      ; edges = le |> permute_edges (invert lp) }
    , lp |> Array.mapi (fun i j -> j, i) |> Map.ofArray ) )

let reachable s a =
  let rec work rs = function
    | [] -> rs
    | w::ws ->
      let r (p1:Strands.port,(p2:Strands.port))  =
        match p1.strand=w, p2.strand=w with
        | false, false
        | true, true -> None
        | false, true -> Some p1.strand
        | true, false -> Some p2.strand
      let is_new a = Set.contains a rs |> not
      let new_ws = s.edges |> Set.toSeq |> Seq.choose r |> Seq.distinct |> Seq.filter is_new |> Seq.toList
      work (Set.add w rs) (new_ws@ws)
  work Set.empty [a]

let compose m1 m2 =
  m1 |> Map.map (fun _ v -> Map.find v m2)

let rep_perm strands (s, p) =
  let r, pi = rep strands s
  r, compose p pi

let break_bond strands s ((p1,p2) as e:edge) =
  let broken_s = { s with edges = Set.remove e s.edges }
  let rs = reachable broken_s p1.strand
  if rs |> Set.contains p2.strand then [rep strands broken_s]
  else
    let s1, s2 = split broken_s rs
    [rep_perm strands s1; rep_perm strands s2]

// [(a,i)] refers to [s1] while [(b,j)] refers to [s2]
let form_bond strands s1 s2 ((p1,p2) as e:edge) =
  { strands = Array.append s1.strands s2.strands
  ; edges = s2.edges
            |> permute_edges ((+) s1.strands.Length)
            |> Set.union s1.edges
            |> Set.add (p1,{Strands.port.strand=p2.strand+s1.strands.Length; Strands.port.site=p2.site}) }
            |> rep strands

let rec split_ccs_perm strands (s,p) =
  if s.strands.Length = 0 then Seq.empty else
  seq {
    let cc = reachable s 0
    if Set.count cc = s.strands.Length then yield rep_perm strands (s,p) else
    let s1, s2 = split s cc
    yield rep_perm strands s1;
    yield! split_ccs_perm strands s2
  }

let split_ccs strands s =
  let p = s.strands |> Array.mapi (fun i _ -> i,i) |> Map.ofArray
  split_ccs_perm strands (s, p)

let add_edge_attr = "[color=lime,penwidth=2.0]"
let remove_edge_attr = "[color=red,penwidth=2.0]"

(* Find nicer way to do this *)
let to_dot_node_mod (strands:Strands.t) (Rep s) title attr sid edge_removed edges_added port_nicked =
  let strand_to_string st =
    let spacer i = if port_nicked {Strands.port.strand=st; Strands.port.site=i} then " | ~~~v~~~" else ""
    strands.strand_types.[st] |> Array.mapi (fun i d -> sprintf "<p%d>%s%s" i (Strands.domain_to_string d) (spacer i)) |> String.concat " | "
  let edge_attr e = if edge_removed e then remove_edge_attr else ""
  seq {
    yield sprintf "%s {" title;
    yield Option.fold (sprintf "%s %s") "" attr;
    yield "  node [shape=record,style=filled,fillcolor=white]";
    yield! s.sg.strands |> Array.mapi (fun i st -> sprintf "  %s [label=\"%s\"];" (sid i) (strand_to_string st));
    yield! s.sg.edges |> Set.map (fun ((p1,p2) as e:edge) -> sprintf "  %s:p%d -- %s:p%d%s;" (sid p1.strand) p1.site (sid p2.strand) p2.site (edge_attr e))
    yield! edges_added |> Set.map (fun (p1:Strands.port,p2:Strands.port) -> sprintf "  %s:p%d -- %s:p%d%s;" (sid p1.strand) p1.site (sid p2.strand) p2.site add_edge_attr)
    yield "}"
  } |> String.concat "\n"

let to_dot_node (strands:Strands.t) (Rep s) title attr sid =
  let strand_to_string st =
    strands.strand_types.[st] |> Array.mapi (fun i d -> sprintf "<p%d>%s" i (Strands.domain_to_string d)) |> String.concat " | "
  seq {
    yield sprintf "%s {" title;
    yield Option.fold (sprintf "%s %s") "" attr;
    yield "  node [shape=record,style=filled,fillcolor=white]";
    yield! s.sg.strands |> Array.mapi (fun i st -> sprintf "  %s [label=\"%s\"];" (sid i) (strand_to_string st));
    yield! s.sg.edges |> Set.map (fun ((p1:Strands.port,p2:Strands.port)) -> sprintf "  %s:p%d -- %s:p%d;" (sid p1.strand) p1.site (sid p2.strand) p2.site )
    yield "}"
  } |> String.concat "\n"

let to_dot (strands:Strands.t) s = to_dot_node strands s "graph g" None (sprintf "s%d")

let to_string_i indent (strands:Strands.t) (Rep s) =
  let bindings,_ =
    s.sg.edges |> Set.fold (fun (m,c) (np1,np2) -> m |> Map.add np1 c |> Map.add np2 c, c+1) (Map.empty, 0)
  let domain port d =
    let b = match Map.tryFind port bindings with
            | Some n -> sprintf "!%d" n
            | None -> ""
    sprintf "%s%s" (Strands.domain_to_string d) b
  let sep = sprintf "\n%s| " (String.replicate indent " ")
  s.sg.strands |> Array.mapi (fun i st -> strands.strand_types.[st] |> Array.mapi (fun j d -> domain {Strands.port.strand=i;Strands.port.site=j} d))
               |> Array.map (String.concat " " >> sprintf "<%s>")
               |> String.concat sep |> sprintf "[ %s ]"

let to_string = to_string_i 0

let to_html_i indent (strands:Strands.t) (Rep s) =
  let bindings,_ =
    s.sg.edges |> Set.fold (fun (m,c) (np1,np2) -> m |> Map.add np1 c |> Map.add np2 c, c+1) (Map.empty, 0)
  let domain port d =
    let b = match Map.tryFind port bindings with
            | Some n -> sprintf "!%d" n
            | None -> ""
    sprintf "%s%s" (Strands.domain_to_string d) b
  let sep = sprintf "\n%s| " (String.replicate indent " ")
  s.sg.strands |> Array.mapi (fun i st -> strands.strand_types.[st] |> Array.mapi (fun j d -> domain {Strands.port.strand=i;Strands.port.site=j} d))
               |> Array.map (String.concat " " >> sprintf "&lt;%s&gt;")
               |> String.concat sep |> sprintf "[ %s ]"

let to_html = to_html_i 0

(* Visualisation *)
type point = float * float
let origo = 0.0, 0.0
let pi = 3.14158
let twopi = 2.0 * pi

(* The point_names are understood as in the local space, the points are in global space *)
(* The ints are pointers to points *)
type line =
  { left  : int
  ; right : int }

type spring = int * int

(* Here are the real points *)
type space =
  { mutable center : point
  ; mutable angle  : float }
let o_space =
  { center = origo
  ; angle  = 0.0 }

let random = System.Random ()
let randomize_space (s: space) =
  s.center <- (300.0 * random.NextDouble(), 300.0 * random.NextDouble())
  s.angle <- twopi * random.NextDouble()

type double =
  { d_space : space
  ; dim     : point
  ; upper   : int
  ; lower   : int }

type single =
  { s_space : space
  ; radius  : float
  ; line    : int }

let left l = 2 * l
let right l = 2 * l + 1

type graph =
  { points : point []
  ; doubles : Set<double>
  ; singles : Set<single> }

type s_graph =
  { s_points : point []
  ; strands : int [] [] // which lines does each strand consist of
  ; domains : Strands.domain []
  ; dim     : point }

(* Helpers *)
let v (x1:float,y1:float) (x2,y2) = (x2 - x1, y2 - y1)
let n (x,y) = x * x + y * y
let d p1 p2 = v p1 p2 |> n |> sqrt
let theta = function
  | (0.0, y) when y < 0.0 -> -pi
  | (0.0, y) -> pi
  | (x, y) -> atan (y/x)
let a (x1:float,y1:float) (x2,y2) = (x2 + x1, y2 + y1)  
let t (px,py) a (vx,vy) =
  (px + vx * cos a - vy * sin a, py + vx * sin a + vy * cos a)

(* Drawing *)
let fontsize = 15
let letterwidth = 8
let linewidth = 2.5
let path_style = sprintf "path { stroke: silver; stroke-width: %f; stroke-linejoin: round; fill: none; }" linewidth
let text_style = sprintf "text { stroke: black; fill: black; stroke-width: 0; font-family: Verdana,Arial,sans-serif; font-size: %dpx; text-anchor: middle; }" fontsize

let palette = [| "red"; "green"; "blue"; "orange"; "purple"
               ; "cyan"; "brown"; "magenta"; "dodgerblue"; "orchid"
               ; "lawngreen"; "lightskyblue"; "darksalmon"; "teal"; "rosybrown"
               ; "navajowhite"; "olive"; "darkseagreen"; "darkgoldenrod"; "pink" |]

let dom_colour i = if i < palette.Length then palette.[i] else "black"

let dom_style (assigned:System.Collections.Generic.Dictionary<string,string>) (strands: Strands.t) =
 let all_doms =
   strands.strand_types |> Array.collect (fun st -> st |> Array.filter (fun d -> d.toehold) |> Array.map (fun d -> d.name))
                        |> Array.toSeq |> Seq.distinct
 let get_assignment d =
   if not (assigned.ContainsKey d) then assigned.Add(d, palette.[assigned.Count % palette.Length])
   (d, assigned.[d])
     
 all_doms |> Seq.map get_assignment
          |> Seq.map (fun (d,c) -> sprintf ".toe_%s { stroke: %s; } .toe_%s_text { fill: %s; }" d c d c)
          |> String.concat " "

let svg_style assigned strands = sprintf "<style> %s %s %s </style>" path_style text_style (dom_style assigned strands)

let s_graph_to_svg g =
  let xs, ys = g.s_points |> Array.unzip
  let domain_class l =
    let d = g.domains.[l]
    if d.toehold then sprintf "toe_%s" d.name else "normal"
  let path (a,b) style =
    let ax, ay = g.s_points.[a]
    let bx, by = g.s_points.[b]
    sprintf "<path d=\"M %f %f L %f %f\"%s/>" ax ay bx by style
  let arrow_path (a,b) style =
    let ax, ay = g.s_points.[a]
    let bx, by = g.s_points.[b]
    let vx, vy = v (ax, ay) (bx, by)
    let l = d (ax, ay) (bx, by)
    let cx, cy = t (bx, by) 2.6 (10.0 * vx / l, 10.0 * vy / l)
    sprintf "<path d=\"M %f %f L %f %f L %f %f\"%s/>" ax ay bx by cx cy style
  let label l =
    let pa = g.s_points.[left l]
    let pb = g.s_points.[right l]
    let vx, vy = v pa pb
    let m = a pa (vx * 0.5, vy * 0.5)
    let r = d pa pb
    let px, py = a m (-12.0 * vy / r, 12.0 * vx / r)
    let angle = theta (vx, vy) * 180.0 / pi
    let label = g.domains.[l] |> Strands.domain_to_string
    sprintf "<text x=\"%f\" y= \"%f\" transform=\"rotate(%f %f,%f)\" dy=\"6\" class=\"%s\">%s</text>" px py angle px py (sprintf "%s_text" (domain_class l)) label
  let line l =
    seq {
      yield label l
      yield path (left l, right l) (sprintf " class=\"%s\"" (domain_class l))
    } |> String.concat "\n"
  let arrow_line l =
    seq {
      yield label l
      yield arrow_path (left l, right l) (sprintf " class=\"%s\"" (domain_class l))
    } |> String.concat "\n"  
  let spring (a,b) = path (right a, left b) " stroke=\"silver\""
  let strand_to_svg s =
    s |> Array.mapi (fun i l -> if i = s.Length-1 then [| arrow_line l |] else [| line l; spring (s.[i],s.[i+1]) |]) |> Array.concat
  g.strands |> Array.collect strand_to_svg |> String.concat "\n"

let wrap_svg dom_style svg (w, h) =
  seq {
    yield sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%f\" height=\"%f\" stroke=\"black\">" (w + 1.0) (h + 1.0)
    yield sprintf "<style> %s %s %s </style>" path_style text_style dom_style
    yield svg
    yield "</svg>"
  } |> String.concat "\n"

let wrap_group dom_style g = wrap_svg dom_style (s_graph_to_svg g) g.dim

let animation_timing = "keyTimes=\"0; 0.2; 0.8; 1\" dur=\"6s\" begin=\"0s\" repeatCount=\"indefinite\""
let animation_values v1 v2 = sprintf "values=\"%s; %s; %s; %s\"" v1 v1 v2 v2

(* create an animted reaction from one graph to another *)
let s_graphs_to_svg g1 g2 =
  let domain_class l =
    let d = g1.domains.[l]
    if d.toehold then sprintf "toe_%s" d.name else "normal"
  let path (a1,a2,b1,b2) style =
    let ax1, ay1 = g1.s_points.[a1]
    let bx1, by1 = g1.s_points.[b1]
    let d1 = sprintf "M %f %f L %f %f" ax1 ay1 bx1 by1
    let ax2, ay2 = g2.s_points.[a2]
    let bx2, by2 = g2.s_points.[b2]
    let d2 = sprintf "M %f %f L %f %f" ax2 ay2 bx2 by2
    seq {
      yield sprintf "<path d=\"%s\"%s>" d1 style
      yield "<animate"
      yield "attributeName=\"d\""
      yield animation_values d1 d2
      yield animation_timing
      yield "additive=\"replace\" fill=\"freeze\"/>"
      yield "</path>"
    } |> String.concat "\n"
    
  let arrow_path (a1,a2,b1,b2) style =
    let ax1, ay1 = g1.s_points.[a1]
    let bx1, by1 = g1.s_points.[b1]
    let vx1, vy1 = v (ax1, ay1) (bx1, by1)
    let l1 = d (ax1, ay1) (bx1, by1)
    let cx1, cy1 = t (bx1, by1) 2.6 (10.0 * vx1 / l1, 10.0 * vy1 / l1)
    let d1 = sprintf "M %f %f L %f %f L %f %f" ax1 ay1 bx1 by1 cx1 cy1
    let ax2, ay2 = g2.s_points.[a2]
    let bx2, by2 = g2.s_points.[b2]
    let vx2, vy2 = v (ax2, ay2) (bx2, by2)
    let l2 = d (ax2, ay2) (bx2, by2)
    let cx2, cy2 = t (bx2, by2) 2.6 (10.0 * vx2 / l2, 10.0 * vy2 / l2)
    let d2 = sprintf "M %f %f L %f %f L %f %f" ax2 ay2 bx2 by2 cx2 cy2
    seq {
      yield sprintf "<path d=\"%s\"%s>" d1 style
      yield "<animate"
      yield "attributeName=\"d\""
      yield animation_values d1 d2
      yield animation_timing
      yield "additive=\"replace\" fill=\"freeze\"/>"
      yield "</path>"
    } |> String.concat "\n"
  let label (l1,l2) =
    let pa1 = g1.s_points.[left l1]
    let pb1 = g1.s_points.[right l1]
    let vx1, vy1 = v pa1 pb1
    let m1 = a pa1 (vx1 * 0.5, vy1 * 0.5)
    let r1 = d pa1 pb1
    let px1, py1 = a m1 (-12.0 * vy1 / r1, 12.0 * vx1 / r1)
    let angle1 = theta (vx1, vy1) * 180.0 / pi
    let rot1 = sprintf "%f %f,%f" angle1 px1 py1
    let t1 = sprintf "x=\"%f\" y=\"%f\"" px1 py1
    let pa2 = g2.s_points.[left l2]
    let pb2 = g2.s_points.[right l2]
    let vx2, vy2 = v pa2 pb2
    let m2 = a pa2 (vx2 * 0.5, vy2 * 0.5)
    let r2 = d pa2 pb2
    let px2, py2 = a m2 (-12.0 * vy2 / r2, 12.0 * vx2 / r2)
    let angle2 = theta (vx2, vy2) * 180.0 / pi
    let rot2 = sprintf "%f %f,%f" angle2 px2 py2
    let t2 = sprintf "%f,%f" (px2-px1) (py2-py1)
    seq {
      yield sprintf "<text %s transform=\"rotate(%s)\" dy=\"6\">" t1 rot1
      yield "<animateTransform attributeName=\"transform\" attributeType=\"XML\""
      yield "type=\"rotate\""
      yield animation_values rot1 rot2
      yield animation_timing
      yield "additive=\"replace\" fill=\"freeze\"/>"
      yield "<animateTransform attributeName=\"transform\" attributeType=\"XML\""
      yield "type=\"translate\""
      yield animation_values "0,0" t2
      yield animation_timing
      yield "additive=\"sum\" fill=\"freeze\"/>"
      yield g1.domains.[l1] |> Strands.domain_to_string |> sprintf "%s</text>"
    } |> String.concat "\n"
  let line (l1,l2) =
    seq {
      yield label (l1,l2)
      yield path (left l1, left l2, right l1, right l2) (sprintf " class=\"%s\"" (domain_class l1))
    } |> String.concat "\n"
  let arrow_line (l1,l2) =
    seq {
      yield label (l1,l2)
      yield arrow_path (left l1, left l2, right l1, right l2) (sprintf " class=\"%s\"" (domain_class l1))
    } |> String.concat "\n"  
  let spring (a1, a2, b1, b2) = path (right a1, right a2, left b1, left b2) " stroke=\"silver\""
  let strand_to_svg (s1,s2) =
    Array.zip s1 s2 |> Array.mapi (fun i (l1,l2) -> if i = s1.Length-1 then [| arrow_line (l1,l2) |] else [| line (l1,l2); spring (s1.[i],s2.[i],s1.[i+1],s2.[i+1]) |]) |> Array.concat
  let w1, h1 = g1.dim
  let w2, h2 = g2.dim
  let w, h = max w1 w2, max h1 h2
  seq {
    yield sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%f\" height=\"%f\" stroke=\"black\">" (w + 1.0) (h + 1.0)
    yield sprintf "<style> %s %s </style>" path_style text_style
    yield! Array.zip g1.strands g2.strands |> Array.collect strand_to_svg
    yield "</svg>"
  } |> String.concat "\n"  

let graph_to_svg g =
  let xs, ys = g.points |> Array.unzip
  let min_x, max_x = Array.min xs, Array.max xs
  let min_y, max_y = Array.min ys, Array.max ys
  let path (a,b) style =
    let ax, ay = g.points.[a]
    let bx, by = g.points.[b]
    sprintf "<path d=\"M %f %f L %f %f\" stroke=\"%s\"/>" (ax - min_x) (ay - min_y) (bx - min_x) (by - min_y) style
  let line l = path (left l, right l) "black"
  let spring (a,b) = path (right a, left b) "grey"
  let double_to_svg (d: double) = [ d.upper; d.lower ] |> Seq.map line
  let single_to_svg (s: single) = line s.line
  seq {
    yield sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%f\" height=\"%f\" stroke=\"black\">" (max_x - min_x + 1.0) (max_y - min_y + 1.0)
    yield! g.doubles |> Set.toSeq |> Seq.collect double_to_svg
    yield! g.singles |> Set.toSeq |> Seq.map single_to_svg
    yield "</svg>"
  } |> String.concat "\n"  

#if JavaScript
#else
let export_graph file_name dom_style g =
  let svg = g |> wrap_group dom_style //|> sprintf "<!DOCTYPE html>\n<html>\n<body>\n%s\n</body>\n</html>"
  System.IO.File.WriteAllText (file_name, svg)
  printfn "Wrote .svg file: %s" file_name
#endif

(* Force constants *)
type force_constants =
  { pull_factor      : float
  ; rot_factor       : float
  ; stiffness        : float
  ; repulsion        : float
  ; repulsion_radius : float }

let forces =
  { pull_factor      = 0.2
  ; rot_factor       = 1.0
  ; stiffness        = 2.0
  ; repulsion        = 0.5
  ; repulsion_radius = 15.0 }

(* Algorithm:
   Maintain
    - array of points and springs between points
    - set of boxes and lines with centres and rotations, knowing which points are their corners and endpoints
   Each step
    - calculate forces on all points
    - translate into translational and rotational forces on boxes and lines
    - move and rotate boxes and lines
    - update point coordinates
   Terminate
    - when forces are all small
    - when set number of iterations have occured
*)
let to_graph_forces force_constants (strands:Strands.t) (Rep s) =
  (* Setup *)
  let port_label (i,j) =
    let st = s.sg.strands.[i]
    strands.strand_types.[st].[j]
  let domains_array = s.sg.strands |> Array.mapi (fun i st -> strands.strand_types.[st] |> Array.mapi (fun j _ -> (i,j))) |> Array.concat
  let domain_map = domains_array |> Array.mapi (fun i d -> {Strands.port.strand=fst d;Strands.port.site=snd d}, i) |> Map.ofArray
  // usage:
  //  domain_left d  = 2 * domain_map.[d]
  //  domain_right d = 2 * domain_map.[d] + 1

  let domain_labels = domains_array |> Array.map port_label
  let dlength d = domain_labels.[d] |> Strands.domain_to_string |> String.length
  let dpadding d = if domain_labels.[d].toehold then 0 else 3
  let line_length d = 8.0 * float (dpadding d + dlength d)


  let all_domains = domains_array |> Seq.map (fun domain -> {Strands.port.strand=fst domain; Strands.port.site=snd domain}) |> Set.ofSeq
  let bound_domains = s.sg.edges |> Set.map (fun (a,b) -> Set.ofList [a; b]) |> Set.unionMany
  let free_domains = all_domains - bound_domains

  let create_single p =
    let d = domain_map.[p]
    { s_space = { center = origo
                ; angle  = 0.0 }
    ; radius  = line_length d
    ; line    = d }
  let singles = free_domains |> Set.map create_single

  let create_double (p1,p2) =
    let d1 = domain_map.[p1]
    let d2 = domain_map.[p2]
    { d_space = { center = origo
                ; angle  = 0.0 }
    ; dim     = max (line_length d1) (line_length d2), 3.0
    ; upper = d1
    ; lower = d2 }
  let doubles = s.sg.edges |> Set.map create_double

  let strand_spring i l =
    [0..l-2] |> List.map (fun j -> right domain_map.[{Strands.port.strand=i;Strands.port.site=j}], left domain_map.[{Strands.port.strand=i;Strands.port.site=j+1}])
  let springs = s.sg.strands |> Array.mapi (fun i st -> strands.strand_types.[st].Length |> strand_spring i) |> Seq.concat

  let points = Array.create (2 * singles.Count + 4 * doubles.Count) origo
  let forces = points |> Array.map (fun _ -> origo)

  (* Step functions *)

  (* - calculate forces on all points *)
  let spring_scale (x,y) = (x * force_constants.pull_factor, y * force_constants.pull_factor)

  let apply_spring (i, j) =
    let vi = v points.[i] points.[j] |> spring_scale
    let vj = v points.[j] points.[i] |> spring_scale
    // here would be a force calculation
    // but f = d*d, so vi would be normalised then scaled back to itself
    forces.[i] <- a forces.[i] vi
    forces.[j] <- a forces.[j] vj

  let apply_springs () =
    [0..forces.Length-1] |> List.iter (fun i -> forces.[i] <- origo)
    springs |> Seq.iter apply_spring


  (* - translate into translational and rotational forces on boxes and lines *)

  let doubleton a b = new Set<int>(seq [a; b])

  let junctioness = points |> Array.map (fun _ -> 2)
  let connect_points classes (i:int, j) =
    let has, has_not = classes |> List.partition (fun c -> Set.contains i c || Set.contains j c)
    match has with
    | [] -> (doubleton i j) :: classes
    | [c] -> Set.union c (doubleton i j) :: has_not
    | cs -> Set.unionMany cs :: has_not
  
  let spring_classes = springs |> Seq.fold connect_points []
  let double_classes = doubles |> Set.toSeq
                               |> Seq.collect (fun d -> [left d.upper, right d.lower; left d.lower, right d.upper])
                               |> Seq.fold connect_points spring_classes
  let count_class c =
    let singles_in = singles |> Set.filter (fun s -> Set.contains (left s.line) c
                                                  || Set.contains (right s.line) c) |> Set.count
    let doubles_in = doubles |> Set.filter (fun d -> Set.contains (left d.upper) c
                                                  || Set.contains (right d.upper) c
                                                  || Set.contains (left d.lower) c
                                                  || Set.contains (right d.lower) c) |> Set.count
    singles_in + doubles_in
  double_classes |> List.iter (fun c -> let n = count_class c in c |> Set.iter (fun i -> junctioness.[i] <- n))

  let stiff_forces = domains_array |> Array.map (fun _ -> 0.0)
  let stiffen_springs iteration =
    [0..stiff_forces.Length-1] |> List.iter (fun i -> stiff_forces.[i] <- 0.0)
    let line_angles = domains_array |> Array.map (fun _ -> 0.0)
    singles |> Set.iter (fun s -> line_angles.[s.line] <- s.s_space.angle)
    doubles |> Set.iter (fun d -> line_angles.[d.upper] <- d.d_space.angle; line_angles.[d.lower] <- d.d_space.angle + pi)
    let to_line p = p / 2
    let stiffen_spring (i, j) =
      let l1 = i |> to_line
      let l2 = j |> to_line
      //let double_spring = doubles |> Set.filter (fun d -> d.upper = l1 || d.upper = l2 || d.lower = l1 || d.lower = l2)
      let target_angle = twopi/(junctioness.[i] |> float) - pi
      let bend = (target_angle + line_angles.[l2] - line_angles.[l1]) % pi
      //let double_stiffness = if bend < 0.0 then double_spring.Count + 1 |> float else 1.0
      stiff_forces.[l1] <- stiff_forces.[l1] + force_constants.stiffness * bend / ((float iteration) * 0.02 + 1.0)
      stiff_forces.[l2] <- stiff_forces.[l2] - force_constants.stiffness * bend / ((float iteration) * 0.02 + 1.0)
    springs |> Seq.iter stiffen_spring

  let repel c =
    let centres =
      let sc = singles |> Set.map (fun s -> s.s_space.center)
      let dc = doubles |> Set.map (fun d -> d.d_space.center)
      sc + dc
    let profile x = 1.0 - 3.0*x*x + 2.0*x*x*x
    let f oc =
      let (vx, vy) = v oc c
      let r = (d oc c) / force_constants.repulsion_radius |> min 1.0
      let m = force_constants.repulsion * profile r
      (m * vx, m * vy)
    centres |> Set.toSeq |> Seq.map f |> Seq.reduce a

  let compute_box_force (d:double) =
    let corners =
      [ left d.lower
      ; right d.lower
      ; left d.upper
      ; right d.upper ]
    let center_force = corners |> List.map (Array.get forces) |> List.fold a origo
    let repel_force = repel d.d_space.center
    let w = n d.dim
    let rot_force i =
      let (vx, vy) = v d.d_space.center points.[i]
      let (fx, fy) = forces.[i]
      (vx*fy - vy*fx)/(w*w)
    let angle_force = corners |> List.sumBy rot_force
    let stiff_force = stiff_forces.[d.lower] + stiff_forces.[d.upper]
    { center = a center_force repel_force
    ; angle = force_constants.rot_factor * angle_force + stiff_force }

  let compute_line_force (s:single) =
    let corners =
      [ left s.line
      ; right s.line ]
    let center_force = corners |> List.map (Array.get forces) |> List.fold a origo
    let repel_force = repel s.s_space.center
    let w = s.radius
    let rot_force i =
      let (vx, vy) = v s.s_space.center points.[i]
      let (fx, fy) = forces.[i]
      (vx*fy - vy*fx)/(w*w)
    let angle_force = corners |> List.sumBy rot_force
    let stiff_force = stiff_forces.[s.line]
    { center = a center_force repel_force
    ; angle = force_constants.rot_factor * angle_force + stiff_force }
  

  (* - move and rotate boxes and lines *)
  let update_box (d, box_force) =
    d.d_space.center <- a d.d_space.center box_force.center
    d.d_space.angle <- d.d_space.angle + box_force.angle

  let update_line (s, line_force) =
    s.s_space.center <- a s.s_space.center line_force.center
    s.s_space.angle <- s.s_space.angle + line_force.angle
  
  let move () =
    let doubles_forces = doubles |> Set.map (fun d -> d, compute_box_force d)
    let singles_forces = singles |> Set.map (fun s -> s, compute_line_force s)
    doubles_forces |> Set.iter update_box
    singles_forces |> Set.iter update_line


  (* - update point coordinates *)
  let update_points () =
    let update_double (d:double) =
      let rx, ry = d.dim
      let p = t d.d_space.center d.d_space.angle
      points.[left d.upper]  <- p (-rx,  ry)
      points.[right d.upper] <- p ( rx,  ry)
      points.[left d.lower]  <- p ( rx, -ry) // lower runs in the other direction
      points.[right d.lower] <- p (-rx, -ry)
    let update_single (s:single) =
      let rx, ry = s.radius, 0.0
      let p = t s.s_space.center s.s_space.angle
      points.[left s.line]  <- p (-rx,  ry)
      points.[right s.line] <- p ( rx,  ry)
    doubles |> Set.iter update_double
    singles |> Set.iter update_single


  (* Set up *)
  doubles |> Set.iter (fun d -> d.d_space |> randomize_space)
  singles |> Set.iter (fun s -> s.s_space |> randomize_space)
  update_points ()

  (* Report *)
  let g d =
    { s_points  = points
    ; strands = s.sg.strands |> Array.mapi (fun i st -> strands.strand_types.[st] |> Array.mapi (fun j _ -> domain_map.[{Strands.port.strand=i;Strands.port.site=j}]))
    ; domains = domain_labels
    ; dim = d }

  (* Iterate *)
  let max_iterations = 501
  let to_debug = [0; 1; 2; 4; 12; 31; 64; 120; 204; 328; 500]
  let mutable i = 0
  let prefix = (System.DateTime.Now.ToLongTimeString ()).Replace(':', '-')
  while i < max_iterations do // TODO: consider forces
#if JavaScript
#else
    if debug && List.exists ((=) i) to_debug then
      let xs, ys = points |> Array.unzip
      let min_x, max_x = Array.min xs - 30.0, Array.max xs + 30.0
      let min_y, max_y = Array.min ys - 30.0, Array.max ys + 30.0
      points |> Array.iteri (fun i (x,y) -> points.[i] <- (x - min_x, y - min_y))
      g (max_x - min_x, max_y - min_y) |> export_graph (sprintf "%s_iteration%d.svg" prefix i) (dom_style (new System.Collections.Generic.Dictionary<string,string>()) strands) // debug
#endif
    apply_springs ()
    stiffen_springs i
    move ()
    update_points ()
    i <- i + 1
  
  (* Normalise *)
  (* - rotation *)
  let doubles_lengths = doubles |> Set.toSeq |> Seq.map (fun d -> fst d.dim, d.d_space.angle)
  let singles_lengths = singles |> Set.toSeq |> Seq.map (fun s -> s.radius, s.s_space.angle)
  let angle = Seq.append doubles_lengths singles_lengths |> Seq.maxBy fst |> snd

  let rotate (x,y) = (x * cos angle + y * sin angle, -x * sin angle + y * cos angle)
  points |> Array.iteri (fun i p -> points.[i] <- rotate p)
  (* - translation *)
  let xs, ys = points |> Array.unzip
  let min_x, max_x = Array.min xs - 30.0, Array.max xs + 30.0
  let min_y, max_y = Array.min ys - 30.0, Array.max ys + 30.0
  points |> Array.iteri (fun i (x,y) -> points.[i] <- (x - min_x, y - min_y))

  g (max_x - min_x, max_y - min_y)
let to_graph = to_graph_forces forces

let to_svg assigned strands = to_graph strands >> wrap_group (dom_style assigned strands)
let to_svg_forces force_constants assigned strands = to_graph_forces force_constants strands >> wrap_group (dom_style assigned strands)
let to_svg_inner strands = to_graph strands >> s_graph_to_svg

#if JavaScript
#else
let export_svg filename strands = to_graph strands >> export_graph filename (dom_style (new System.Collections.Generic.Dictionary<string,string>()) strands)
#endif