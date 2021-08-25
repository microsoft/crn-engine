// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.States

type t<'s> when 's:comparison = Map<'s,int>

let groups_to_svg assigned strands =
  let view_species (max_w, y, svgs) (g: Species.s_graph, i) =
    let w, h = g.dim
    let svg = g |> Species.s_graph_to_svg |> sprintf "<g class=\"species\" transform=\"translate(20,%f)\">\n<text transform=\"translate(0,%f)\">%d </text>%s</g>" y (8.0 + h/2.0) i
    max w max_w, h + y, svg::svgs

  let box_state (width, height, svgs) =
    let w, h = width + 20.0, height
    ( svgs |> String.concat "\n"
           |> sprintf "<rect width=\"%f\" height=\"%f\" style=\"fill:none;stroke:black;stroke-width:4\" />\n%s" w h
           //|> sprintf "<g transform=\"translate(%f,%f)\">%s</g>" (-w/2.0) (-h/2.0)
    , w
    , h )

  Seq.fold view_species (0.0, 0.0, [])
   >> box_state
   >> fun (s, w, h) -> Species.wrap_svg (Species.dom_style assigned strands) (sprintf "<g class=\"state\">%s</g>" s) (w,h)

let to_svg assigned strands = Map.toSeq >> Seq.map (fun (s, i) -> s |> Species.to_graph strands, i) >> groups_to_svg assigned strands

let state_of_species species : 's t = species |> Seq.fold (fun sm (i,s) -> Map.add s i sm) Map.empty

let reactions_on_state enzymes strands (st: Species.rep t) =
  st |> Map.toSeq |> Seq.map fst
     |> Seq.fold (fun (ss,rs) s -> s::ss, rs + Reactions.actions enzymes strands ss s) ([], Set.empty)
     |> snd

let decrement state s : 's t =
  let c = Map.find s state
  if c = 1 then Map.remove s state
  else Map.add s (c-1) state

let increment state s : 's t =
  match Map.tryFind s state with
  | None -> Map.add s 1 state
  | Some c -> Map.add s (c+1) state

let product_conc state = function
  | Reactions.Isolated_toehold (s, _, _)
  | Reactions.Complement_int (s, _, _)
  | Reactions.Displacing (s, _, _)
  | Reactions.Nick (s, _, _) -> Map.find s state
  | Reactions.Complement_ext (Species.Rep s1 as r1, (Species.Rep s2 as r2), _, _) ->
    if s1.sg = s2.sg then
      let c = Map.find r1 state
      (c * (c-1)) / 2
    else Map.find r1 state * Map.find r2 state

let reaction_propensity toehold_map strands state r =
  let rate = Reactions.get_rate toehold_map strands
  rate r * (product_conc state r |> float)

let applies toehold_map strands state = reaction_propensity toehold_map strands state >> (<) 0.0

let apply state = function
  | Reactions.Isolated_toehold (s, _, ss) -> List.fold increment (decrement state s) (ss |> List.map fst)
  | Reactions.Complement_int (s1, _, (s2,_)) -> increment (decrement state s1) s2
  | Reactions.Complement_ext (s1, s2, _, (s3,_)) -> increment (decrement (decrement state s1) s2) s3
  | Reactions.Displacing (s, _, ss) -> List.fold increment (decrement state s) (ss |> List.map fst)
  | Reactions.Nick (s1, _, (s2,_)) -> increment (decrement state s1) s2

let state_space limit enzymes toehold_map strands species =
  let rate = Reactions.get_rate toehold_map strands
  let applies = applies toehold_map strands
  let reactions_on = reactions_on_state enzymes strands

  let rec work transitions states l =
    match l with
    | [] -> states, transitions
    | w::ws ->
      if limit > 0 && (Set.count states > limit) then (w::ws |> Set.ofList |> Set.union states), transitions else
      printfn "States: %d" (Set.count states)
      let rs = reactions_on w |> Set.filter (applies w)
      let ts = rs |> Set.map (fun r -> w, rate r, apply w r)
      let is_new s = Set.contains s states |> not
      let new_ws = ts |> Set.map (fun (_,_,x) -> x) |> Set.filter is_new |> Set.toList
      work (Set.union transitions ts) (Set.add w states) (ws @ new_ws)
  work Set.empty Set.empty [state_of_species species]

let choices (_,l) =
  let n, c = l |> Seq.fold (fun (n,r) (_,i) -> n + 1, r * (i-n)) (0, 1)
  c / n

let state_space_c limit (c: 's Calculus.t) species =
  let propensity (st: 's t) (r: 's Calculus.reaction) =
    r.reactants
    |> Seq.map (fun p -> p, match Map.tryFind p st with None -> 0 | Some c -> c)
    |> Seq.groupBy fst
    |> Seq.map (choices >> float)
    |> Seq.fold (*) r.rate
  let applies st = propensity st >> (<) 0.0
  let apply st (r: 's Calculus.reaction) =
    List.fold increment (List.fold decrement st r.reactants) r.products
  let reactions_on (st: 's t) =
    st |> Map.toSeq |> Seq.map fst
       |> Seq.fold (fun (ss,rs) s -> Set.add s  ss, rs + c.reactions ss s) (Set.empty, Set.empty)
       |> snd

  let rec work transitions states l =
    match l with
    | [] ->
      printfn "States: %d" (Set.count states)
      states, transitions
    | w::ws ->
      let count = states.Count + l.Length
      if limit > 0 && count > limit then (w::ws |> Set.ofList |> Set.union states), transitions else
      printfn "States: %d" count
      let rs = reactions_on w |> Set.filter (applies w)
      printfn "\t(Current state has %i species and %i reactions)" w.Count rs.Count
      let ts = rs |> Set.map (fun r -> w, propensity w r, apply w r)
      let is_new s = not <| Set.contains s states // and not s in ws
      let new_ws = ts |> Set.map (fun (_,_,x) -> x) |> Set.filter is_new |> Set.toList
      work (Set.union transitions ts) (Set.add w states) (ws @ new_ws)
  work Set.empty Set.empty [state_of_species species]

let state_html assigned strands st i =
  st |> Map.toSeq
     |> Seq.map (fun (s, n) -> Species.to_svg assigned strands s |> sprintf "<tr>\n<td><h3>%d of</h3></td>\n<td>%s</td>\n</tr>" n)
     |> String.concat "\n"
     |> sprintf "<h1>State %d</h1>\n<table>\n%s\n</table>" i

let reactions_html assigned strands animations rs =
  let split_reaction =
    function
    | Reactions.Complement_ext (r1, r2, _, p) -> [r1; r2], [p]
    | Reactions.Complement_int (r, _, p) -> [r], [p]
    | Reactions.Displacing (r, _, ps) -> [r], ps
    | Reactions.Isolated_toehold (r, _, ps) -> [r], ps
    | Reactions.Nick (r, _, p) -> [r], [p]
  let reaction_row_data (r,rate) =
    let td_no_border = sprintf "<td style = \"border: 0;\">%s</td>"
    let rs, ps = split_reaction r
    seq {
      if animations then yield r |> Reactions.to_svg assigned strands |> td_no_border
      else
        yield rs |> List.map (Species.to_svg assigned strands) |> String.concat "<h3>+</h3>" |> td_no_border
        yield rate |> sprintf "<h1>&rarr;</h1> (%.3f)" |> td_no_border
        yield ps |> List.map (fst >> Species.to_svg assigned strands) |> String.concat "<h3>+</h3>" |> td_no_border
    } |> String.concat "\n"
  rs |> Array.mapi (fun i r -> sprintf "<tr><td><h3>%d:</h3></td>\n%s</tr>" i (reaction_row_data r))
     |> String.concat "\n"
     |> sprintf "<h2>Reactions</h2>\n<table>\n%s\n</table>"


#if JavaScript
#else
(* Interactive mode *)
let state_space_interactive assigned next_choice limit animations enzymes toehold_map strands species =
  let propensity = reaction_propensity toehold_map strands //Reactions.get_rate toehold_map strands
  let applies = applies toehold_map strands
  let reactions_on = reactions_on_state enzymes strands

  let states = new System.Collections.Generic.Dictionary<Species.rep t,int>(HashIdentity.Structural)
  let current = species |> state_of_species
  states.Add (current, 0)
  let get_state s =
    if states.ContainsKey s then states.[s]
    else
      let n = states.Count
      states.Add (s, n)
      n

  let species_svg = new System.Collections.Generic.Dictionary<Species.rep,_>(HashIdentity.Structural)
  let get_species_svg s =
    if species_svg.ContainsKey s then species_svg.[s]
    else
      let g = Species.to_graph strands s
      species_svg.Add (s, g)
      g

  let states_svg = new System.Collections.Generic.Dictionary<Species.rep t,_>(HashIdentity.Structural)
  let get_state_svg assigned s =
    if states_svg.ContainsKey s then states_svg.[s]
    else
      let svg =
        s |> Map.toSeq |> Seq.map (fun (sp, i) -> get_species_svg sp, i) |> groups_to_svg assigned strands
      states_svg.Add (s, svg)
      svg

  let trace_choices = ref []
  let trace_reactions = ref []
  let trace_states = ref (Set.singleton current)
  let transitions = ref Set.empty
  let trace_probability = ref 1.0

  let port_label (s:Species.t) (port:Strands.port) =
    let d = (strands.strand_types.[s.sg.strands.[port.strand]]).[port.site] |> Strands.domain_to_string
    sprintf "(%d,%d) %s" port.strand port.site d
  let edge_label s (np:Strands.port,_:Strands.port) = port_label s np

  let label i =
    let indent = String.replicate i " "
    function
    | Reactions.Isolated_toehold (Species.Rep s as r, e,_) -> sprintf "Unbind %s on\n%s%s" (edge_label s e) indent (Species.to_string_i i strands r)
    | Reactions.Complement_int (Species.Rep s as r, e,_) -> sprintf "Internal bind %s on\n%s%s" (edge_label s e) indent (Species.to_string_i i strands r)
    | Reactions.Complement_ext (Species.Rep s as r1, r2, e,_) -> sprintf "External bind %s on\n%s%s\n%sand\n%s%s" (edge_label s e) indent (Species.to_string_i i strands r1) indent indent (Species.to_string_i i strands r2)
    | Reactions.Displacing (Species.Rep s as r, es,_) -> es |> List.map (edge_label s) |> String.concat " " |> sprintf "Displace %s on\n%s%s" (Species.to_string_i i strands r) indent
    | Reactions.Nick (Species.Rep s as r,np,_) -> sprintf "Nick %s on\n%s%s" (port_label s np) indent (Species.to_string_i i strands r)

  let export c_s =
    (*
    //let state_space = (states.Keys |> Set.ofSeq, !transitions)
    let state_space = (!trace_states, !transitions)
    let ss_dot = state_space_to_dot strands state_space species
    let ss_out_file = "trace.dot"//System.IO.Path.Combine(System.IO.Path.GetDirectoryName !in_file, sprintf "%s_ss.dot" (System.IO.Path.GetFileNameWithoutExtension !in_file))
    System.IO.File.WriteAllText(ss_out_file, ss_dot);
    printfn "Wrote state space file %s" ss_out_file;
    *)
    let trace_dot = !trace_reactions |> List.rev |> Reactions.trace_to_dot strands c_s
    let trace_out_file = "trace.dot"
    System.IO.File.WriteAllText(trace_out_file, trace_dot)
    printfn "Wrote trace file %s" trace_out_file

  let export_svg cs =
    let title = !trace_probability |> sprintf "Trace with probability %.3f"
    let trace_svg =
      !trace_reactions
       |> List.collect (fun (c,r,p) -> ["<h1>&darr;</h1>"; Reactions.to_svg assigned strands r; sprintf "<h1>&darr;</h1> (%.3f)" p; get_state_svg assigned c ])
       |> fun l -> get_state_svg assigned cs :: l
       |> List.rev
       |> List.map (sprintf "<tr><td>%s</td></tr>")
       |> String.concat "\n"
       |> sprintf
            "<html>\n<head><title>%s</title></head>\n<body>\n<h1>%s</h1>\n<table style=\"text-align: center\" width=\"100%%\">%s</table>\n</body>\n</html>"
            title title   
    let trace_out_file = "trace.html"
    System.IO.File.WriteAllText(trace_out_file, trace_svg)
    printfn "Wrote trace file %s" trace_out_file

  let export_trace () =
    let trace_txt =
      !trace_choices |> List.tail |> // remove the choice to export a trace
         function "e"::cs | "E"::cs | cs -> "q" :: "e" :: cs |> List.rev |> String.concat "\n"
    let trace_out_file = "trace.txt"
    System.IO.File.WriteAllText(trace_out_file, trace_txt)
    printfn "Wrote trace file %s" trace_out_file

  (*
  let export_state st i =
    st |> Map.toSeq
       |> Seq.iteri (fun j (s, n) -> Species.export_svg (sprintf "state_%d_species_%d_count_%d.svg" i j n) strands s)
*)

  let export_state st i rs =
    let state_file = "state.html"
    let html = sprintf "<html>\n<head></head>\n<body>\n%s\n%s\n</body>\n</html>"
                       (state_html assigned strands st i)
                       (reactions_html assigned strands animations rs)

    System.IO.File.WriteAllText(state_file, html)
    printfn "Wrote state file %s" state_file

  let rec work c_s c_i =
    printfn "You are at state %d" c_i
    printfn "Choose your action:"
    let rs = reactions_on c_s |> Set.filter (applies c_s) |> Set.toArray
    let rs_meta = rs |> Array.map (fun r -> let t = apply c_s r in r, c_s, propensity c_s r, t, get_state t)
    export_state c_s c_i (rs_meta |> Array.map (fun (r, c_s, prop, t, id) -> r, prop)) // debug
    let ts = rs_meta |> Array.map (fun (_, cs, prop, t, _) -> cs, prop, t)
    let total_prop = ts |> Array.sumBy (fun (_, prop, _) -> prop)
                        |> function 0.0 -> 1.0 | x -> x // if total_prop is zero, do no division of all the zeroes
    let choices = rs_meta |> Array.map (fun (r, _, prop, t, target_id) -> r, prop / total_prop, t, target_id)
    choices |> Array.iteri (fun i (r, prob,_,target_id) ->
                              let g = sprintf " %d: (%f) Goto state %d via" i prob target_id
                              let l = label (String.length g + 1) r
                              printfn "%s %s" g l
                              )
    printfn "p: Print state"
    printfn "e: Export traversed path to .svg"
    printfn "t: Export choices to .txt"
    printfn "q: Quit"
    let c = next_choice ()
    trace_choices := c :: !trace_choices
    match c with
    | "p" | "P" -> c_s |> Map.iter (fun s i -> printfn " %d * %s" i (Species.to_string_i 5 strands s)); work c_s c_i
    | "e" | "E" -> export_svg c_s; work c_s c_i
    | "t" | "T" -> export_trace (); work c_s c_i
    | "q" | "Q" -> ()
    | _ ->
      try
        let i = int c
        let r, prob, target, target_id = choices.[i]
        trace_reactions := (c_s,r,prob) :: !trace_reactions
        trace_states := Set.add target !trace_states
        transitions := Set.add ts.[i] !transitions
        trace_probability := prob * !trace_probability
        work target target_id
      with _ ->
        printfn "Unrecognized choice %s" c
        work c_s c_i
  work current 0

(* TODO
let state_space_interactive_c next_choice limit animations (c: 's Calculus.t) strands species =
  let propensity (st: 's t) (r: 's Calculus.reaction) =
    r.reactants
    |> Seq.map (fun p -> p, match Map.tryFind p st with None -> 0 | Some c -> c)
    |> Seq.groupBy fst
    |> Seq.map (choices >> float)
    |> Seq.fold (*) r.rate
  let applies st = propensity st >> (<) 0.0
  let apply st (r: 's Calculus.reaction) =
    List.fold increment (List.fold decrement st r.reactants) r.products
  let reactions_on (st: 's t) =
    st |> Map.toSeq |> Seq.map fst
       |> Seq.fold (fun (ss,rs) s -> Set.add s  ss, rs + c.reactions ss s) (Set.empty, Set.empty)
       |> snd

  let states = new System.Collections.Generic.Dictionary<Species.rep t,int>(HashIdentity.Structural)
  let current = species |> state_of_species
  states.Add (current, 0)
  let get_state s =
    if states.ContainsKey s then states.[s]
    else
      let n = states.Count
      states.Add (s, n)
      n

  let species_svg = new System.Collections.Generic.Dictionary<Species.rep,_>(HashIdentity.Structural)
  let get_species_svg s =
    if species_svg.ContainsKey s then species_svg.[s]
    else
      let g = Species.to_graph strands s
      species_svg.Add (s, g)
      g

  let states_svg = new System.Collections.Generic.Dictionary<Species.rep t,_>(HashIdentity.Structural)
  let get_state_svg s =
    if states_svg.ContainsKey s then states_svg.[s]
    else
      let svg =
        s |> Map.toSeq |> Seq.map (fun (sp, i) -> get_species_svg sp, i) |> groups_to_svg strands
      states_svg.Add (s, svg)
      svg

  let trace_choices = ref []
  let trace_reactions = ref []
  let trace_states = ref (Set.singleton current)
  let transitions = ref Set.empty
  let trace_probability = ref 1.0

  let port_label (s:Species.t) (port:Strands.port) =
    let d = (strands.strand_types.[s.sg.strands.[port.strand]]).[port.site] |> Strands.domain_to_string
    sprintf "(%d,%d) %s" port.strand port.site d
  let edge_label s (np:Strands.port,_:Strands.port) = port_label s np

  let label i =
    let indent = String.replicate i " "
    function
    | Reactions.Isolated_toehold (Species.Rep s as r, e,_) -> sprintf "Unbind %s on\n%s%s" (edge_label s e) indent (Species.to_string_i i strands r)
    | Reactions.Complement_int (Species.Rep s as r, e,_) -> sprintf "Internal bind %s on\n%s%s" (edge_label s e) indent (Species.to_string_i i strands r)
    | Reactions.Complement_ext (Species.Rep s as r1, r2, e,_) -> sprintf "External bind %s on\n%s%s\n%sand\n%s%s" (edge_label s e) indent (Species.to_string_i i strands r1) indent indent (Species.to_string_i i strands r2)
    | Reactions.Displacing (Species.Rep s as r, es,_) -> es |> List.map (edge_label s) |> String.concat " " |> sprintf "Displace %s on\n%s%s" (Species.to_string_i i strands r) indent
    | Reactions.Nick (Species.Rep s as r,np,_) -> sprintf "Nick %s on\n%s%s" (port_label s np) indent (Species.to_string_i i strands r)

  let export c_s =
    (*
    //let state_space = (states.Keys |> Set.ofSeq, !transitions)
    let state_space = (!trace_states, !transitions)
    let ss_dot = state_space_to_dot strands state_space species
    let ss_out_file = "trace.dot"//System.IO.Path.Combine(System.IO.Path.GetDirectoryName !in_file, sprintf "%s_ss.dot" (System.IO.Path.GetFileNameWithoutExtension !in_file))
    System.IO.File.WriteAllText(ss_out_file, ss_dot);
    printfn "Wrote state space file %s" ss_out_file;
    *)
    let trace_dot = !trace_reactions |> List.rev |> Reactions.trace_to_dot strands c_s
    let trace_out_file = "trace.dot"
    System.IO.File.WriteAllText(trace_out_file, trace_dot)
    printfn "Wrote trace file %s" trace_out_file

  let export_svg cs =
    let title = !trace_probability |> sprintf "Trace with probability %.3f"
    let trace_svg =
      !trace_reactions
       |> List.collect (fun (c,r,p) -> ["<h1>&darr;</h1>"; Reactions.to_svg strands r; sprintf "<h1>&darr;</h1> (%.3f)" p; get_state_svg c ])
       |> fun l -> get_state_svg cs :: l
       |> List.rev
       |> List.map (sprintf "<tr><td>%s</td></tr>")
       |> String.concat "\n"
       |> sprintf
            "<html>\n<head><title>%s</title></head>\n<body>\n<h1>%s</h1>\n<table style=\"text-align: center\" width=\"100%%\">%s</table>\n</body>\n</html>"
            title title   
    let trace_out_file = "trace.html"
    System.IO.File.WriteAllText(trace_out_file, trace_svg)
    printfn "Wrote trace file %s" trace_out_file

  let export_trace () =
    let trace_txt =
      !trace_choices |> List.tail |> // remove the choice to export a trace
         function "e"::cs | "E"::cs | cs -> "q" :: "e" :: cs |> List.rev |> String.concat "\n"
    let trace_out_file = "trace.txt"
    System.IO.File.WriteAllText(trace_out_file, trace_txt)
    printfn "Wrote trace file %s" trace_out_file

  (*
  let export_state st i =
    st |> Map.toSeq
       |> Seq.iteri (fun j (s, n) -> Species.export_svg (sprintf "state_%d_species_%d_count_%d.svg" i j n) strands s)
*)

  let export_state st i rs =
    let state_file = "state.html"
    let html = sprintf "<html>\n<head></head>\n<body>\n%s\n%s\n</body>\n</html>"
                       (state_html strands st i)
                       (reactions_html strands animations rs)

    System.IO.File.WriteAllText(state_file, html)
    printfn "Wrote state file %s" state_file

  let rec work c_s c_i =
    printfn "You are at state %d" c_i
    printfn "Choose your action:"
    let rs = reactions_on c_s |> Set.filter (applies c_s) |> Set.toArray
    let rs_meta = rs |> Array.map (fun r -> let t = apply c_s r in r, c_s, propensity c_s r, t, get_state t)
    export_state c_s c_i (rs_meta |> Array.map (fun (r, c_s, prop, t, id) -> r, prop)) // debug
    let ts = rs_meta |> Array.map (fun (_, cs, prop, t, _) -> cs, prop, t)
    let total_prop = ts |> Array.sumBy (fun (_, prop, _) -> prop)
                        |> function 0.0 -> 1.0 | x -> x // if total_prop is zero, do no division of all the zeroes
    let choices = rs_meta |> Array.map (fun (r, _, prop, t, target_id) -> r, prop / total_prop, t, target_id)
    choices |> Array.iteri (fun i (r, prob,_,target_id) ->
                              let g = sprintf " %d: (%f) Goto state %d via" i prob target_id
                              let l = label (String.length g + 1) r
                              printfn "%s %s" g l
                              )
    printfn "p: Print state"
    printfn "e: Export traversed path to .svg"
    printfn "t: Export choices to .txt"
    printfn "q: Quit"
    let c = next_choice ()
    trace_choices := c :: !trace_choices
    match c with
    | "p" | "P" -> c_s |> Map.iter (fun s i -> printfn " %d * %s" i (Species.to_string_i 5 strands s)); work c_s c_i
    | "e" | "E" -> export_svg c_s; work c_s c_i
    | "t" | "T" -> export_trace (); work c_s c_i
    | "q" | "Q" -> ()
    | _ ->
      try
        let i = int c
        let r, prob, target, target_id = choices.[i]
        trace_reactions := (c_s,r,prob) :: !trace_reactions
        trace_states := Set.add target !trace_states
        transitions := Set.add ts.[i] !transitions
        trace_probability := prob * !trace_probability
        work target target_id
      with _ ->
        printfn "Unrecognized choice %s" c
        work c_s c_i
  work current 0
*)

// Interactive object
type StateGraph (enzymes, toehold_map, strands, ss) =
  let rate = Reactions.get_rate toehold_map strands
  let applies = applies toehold_map strands
  let reactions_on = reactions_on_state enzymes strands
  let compute_reactions s = reactions_on s |> Set.filter (applies s) |> Set.toArray |> Array.map (fun r -> r, rate r)

  let mutable state = state_of_species ss
  let mutable state_index = 0
  let mutable reactions = compute_reactions state

  let states = (new System.Collections.Generic.Dictionary<Species.rep t,int>(HashIdentity.Structural))
  let _ = states.Add (state, state_index)
  let get_state_index s =
    if states.ContainsKey s then states.[s]
    else
      let n = states.Count
      states.Add (s, n)
      n

  member this.apply_reaction n =
    state <- apply state (fst reactions.[n])
    state_index <- get_state_index state
    reactions <- compute_reactions state

  member this.show_state = state_html (new System.Collections.Generic.Dictionary<string,string>()) strands state state_index

  member this.show_reactions animations = reactions_html (new System.Collections.Generic.Dictionary<string,string>()) strands animations reactions
#endif
