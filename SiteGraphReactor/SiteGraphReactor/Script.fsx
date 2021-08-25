#r @"bin\Debug\SiteGraphReactor.dll"
let program = "
new 3@ 4.2E-4 , 4.0E-2
new 5@ 6.5E-4 , 4.0E-3

  13 * [ <2 3^ 4> ]
| 10 * [ <4 5^> ]
| 10 * [ <1>[2]:<6>[3^ 4]{5^*} ]"

let enzymes, toehold_map, strands, ss = SiteGraphReactor.Sitegraphs.compile program

let all_ss, rs = SiteGraphReactor.Sitegraphs.reaction_graph 0 enzymes toehold_map strands (ss |> Seq.map snd |> List.ofSeq)

let svg_species = all_ss |> Set.map (SiteGraphReactor.Species.to_svg strands)

type Svg = { svg : string }
let svg t = { svg = t }

fsi.AddHtmlPrinter(fun (s:Svg) -> s.svg |> sprintf "<div style=\"background:white\">%s</div>")
fsi.AddHtmlPrinter(fun (s:seq<Svg>) -> Seq.map (fun s -> s.svg) s |> String.concat "<br/>" |> sprintf "<div style=\"background:white\">%s</div>")

svg_species |> Seq.head |> svg

svg_species |> Seq.map svg

let svg_reactions = rs |> Set.map (SiteGraphReactor.Reactions.to_svg strands)

svg_reactions |> Seq.map svg
