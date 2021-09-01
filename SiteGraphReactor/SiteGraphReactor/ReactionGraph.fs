// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.ReactionGraph

let reaction_graph limit (c: 's Calculus.t) =
  let positivite_propensity (r: 's Calculus.reaction) = r.rate > 0.0
  let rec work rs all_ss l =
    match l with
    | [] -> all_ss, rs
    | w::ws ->
      if limit > 0 && (Set.count all_ss > limit) then (w::ws |> Set.ofList |> Set.union all_ss), rs else
      printfn "Species: %d" (Set.count all_ss)
      let new_rs = c.reactions all_ss w - rs |> Set.filter positivite_propensity
      let is_new s = not <| Set.contains s all_ss
      let new_ss = new_rs |> Seq.collect (fun r -> r.products) |> Seq.distinct |> Seq.filter is_new |> Seq.toList
      work (Set.union new_rs rs) (Set.add w all_ss) (ws @ new_ss)
  work Set.empty Set.empty c.initial_species

#if JavaScript
#else
let reaction_graph_to_svgdot out_dir limit species_to_svg (c: 's Calculus.t) =
  let all_ss, rs = reaction_graph limit c
  let init_ss = c.initial_species |> Set.ofList
  let sname = sprintf "species%d"
  let r_att = "[penwidth=2.0,dir=none]"
  let style = ""
  let init_style = ",penwidth=\"3.0\""
  let sid s = Seq.findIndex ((=) s) all_ss |> sname
  let rr (r: 's Calculus.reaction) = r.rate
  let r_to_n rid r = sprintf "r%d [label=\"%.3f\"];" rid (rr r)
  let r_to_e rid (r: 's Calculus.reaction) =
    let react_str =
      r.reactants
      |> List.map sid
      |> String.concat " "
      |> sprintf "{%s}"
    let prod_str =
      r.products
      |> List.map sid
      |> String.concat " "
      |> sprintf "{%s}"
    [ sprintf "%s -> r%d%s" react_str rid r_att
    ; sprintf "r%d -> %s[arrowhead=onormal]" rid prod_str]
  let export_species i s =
    let out_file = System.IO.Path.Combine(out_dir, (sprintf "%s.svg" (sname i)))
    System.IO.File.WriteAllText (out_file, species_to_svg s)
  all_ss |> Seq.iteri export_species
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

