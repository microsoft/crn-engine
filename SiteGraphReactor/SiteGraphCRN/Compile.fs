// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Compile
open Parser
open Microsoft.Research.CRNEngine

let set_plots (crn:Crn) =
  { crn with settings = { crn.settings with simulation = { crn.settings.simulation with plots = crn.initials |> List.map (fun i -> Expression.Key (Key.Species i.species)) } } }

type Render_mode = Default | Circle | Force

let to_crn strands ss species reactions render_mode =
  let init_ss = ss |> Seq.toList |> List.map snd
  let init_map = ss |> Seq.map (fun (n,s) -> s,n) |> Map.ofSeq

  let renderer = match render_mode with
                 | Default -> SiteGraphReactor.Species.to_svg
                 | Circle -> SiteGraphReactor.CirclesRenderer.to_circle_svg
                 | Force -> SiteGraphReactor.Species.to_svg_forces Species.forces
  let renderer = renderer (new System.Collections.Generic.Dictionary<string,string>())

  let crn_attributes_map =
    species |> Set.toSeq
            |> Seq.mapi (fun i s -> s, { Attributes.name = i |> sprintf "species_%i"
                                       ; Attributes.structure = s |> SiteGraphReactor.Species.to_string strands
                                       ; Attributes.svg = s |> renderer strands })
            |> Map.ofSeq in
 
  let crn_attributes s = crn_attributes_map.[s] in
  let crn_species s = (crn_attributes s).name |> Species.create 
  let crn_value = float >> Expression.Float
  let crn_initial s =
    match init_map |> Map.tryFind s with
    | Some n -> Initial.create(false, (crn_value n), (crn_species s), None, None)
    | None -> Initial.create(false, (crn_value 0), (crn_species s), None, None)

  let crn_reaction (r: SiteGraphReactor.Calculus.reaction<Species.rep>) : Reaction<Species,Value,Functional> = 
    Reaction.create(
      [],
      (r.reactants |> List.map crn_species),
      (None, Rate.MassAction (r.rate |> Expression.Float)),   
      (r.products |> List.map crn_species)
    )
  let crn = { Crn.empty with
                initials = species |> Set.toList |> List.map crn_initial
                ; reactions = reactions |> Set.toList |> List.map crn_reaction }
            |> set_plots
            |> Crn.group_reactions
  let allSpecies = crn.saturate_initials().initials
  let attributes = Set.fold (fun m i -> Stringmap.add (crn_attributes i).name (crn_attributes i) m) Stringmap.empty species
  { crn with attributes = attributes} 


let convert_expand limit (enzymes, toehold_map, strands, ss) render_mode =
  let init_ss = ss |> Seq.toList |> List.map snd

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

  let calculus =
    SiteGraphReactor.Reactions.calculus enzymes toehold_map strands init_ss
    |> TimeSeparation.lift fast_mono slow_mono
  let species, reactions = SiteGraphReactor.ReactionGraph.reaction_graph limit calculus

  to_crn strands ss species reactions render_mode

let compile limit = SiteGraphReactor.Sitegraphs.compile >> convert_expand limit

//type bundle = seq<Strands.domain [] * Strands.domain []> * Map<string,(float * float)> * Strands.t * seq<int * Species.rep>

//Remove tuple from interface, I'd like us to push types types into the core code. Colin
type Enzyme =
  { left_of_cut: Strands.domain []
  ; right_of_cut: Strands.domain [] }

type toehold_rate =
  { binding: float
  ; unbinding: float }

type counted_species =
  { count: int
  ; representative: Species.rep }

type sg_settings =
  { inference:    Inference_settings option
    render_mode:  Render_mode }

let default_settings = { inference    = None
                         render_mode  = Default }

type bundle =
  { enzymes:seq<Enzyme>
  ; toehold_map:Map<string,toehold_rate>
  ; strands:Strands.t
  ; initial_species:seq<counted_species>
  ; settings: sg_settings }

let convert_unexpanded (enzymes, toehold_map, strands, ss) settings =
  let init_ss = ss |> Seq.toList |> List.map snd |> Set.ofList
  let enzymes_record = enzymes |> Seq.map (fun enzyme -> { left_of_cut = fst enzyme; right_of_cut = snd enzyme})
  let toehold_rate_record = toehold_map |> Map.map(fun _ value -> {binding=fst value; unbinding=snd value})
  let species_something_record = ss |> Seq.map (fun initial_specie -> {count = fst initial_specie; representative = snd initial_specie})
  ( { enzymes = enzymes_record; toehold_map = toehold_rate_record; strands = strands; initial_species = species_something_record; settings = settings }
  , to_crn strands ss init_ss Set.empty settings.render_mode )

let parse = SiteGraphReactor.Sitegraphs.compile >> convert_unexpanded

let expand limit (bundle:bundle) =
    let enzymes_tuples = bundle.enzymes |> Seq.map (fun enzyme -> enzyme.left_of_cut, enzyme.right_of_cut)
    let toehold_rates_tuples = bundle.toehold_map |> Map.map (fun _ value -> value.binding, value.unbinding)
    let inital_species_tuples = bundle.initial_species |> Seq.map (fun initial_specie -> initial_specie.count, initial_specie.representative)
    convert_expand limit (enzymes_tuples, toehold_rates_tuples, bundle.strands, inital_species_tuples) bundle.settings.render_mode

type sgDirective = Inference of Inference_settings 
                 | Rendering of Render_mode
let parse_enzymes =
  let dirs = many ( pTry (kw "directive" 
                          >>. choice [
                            kw "inference" >>. Inference_settings.parse |>> Inference
                            kw "render_mode" >>. choice [
                                kw "circle"  >>. preturn Circle  |>> Rendering
                                kw "force"   >>. preturn Force   |>> Rendering
                                kw "default" >>. preturn Default |>> Rendering
                              ]
                          ]) )
             |>> List.fold (fun (acc:sg_settings) x -> 
                  match x  with 
                  | Inference i -> { acc with inference   = Some i }
                  | Rendering r -> { acc with render_mode = r }
                ) default_settings
  let es = sepEndBy (ExtendedSyntax.enzyme Syntax.parse_name) (Syntax.kw ";") |> Syntax.bracket "[" "]"
  let enzymes = (SiteGraphReactor.Syntax.kw "enzymes" >>. es) |> opt |>> function Some es -> es | None -> []
  let basic = Syntax.parse_name |> Syntax.parse_site |> ExtendedSyntax.parse_syntax |>> ExtendedSyntax.to_basic
  spaces >>. dirs .>>. enzymes .>>. basic

let parse_with_settings s =
  let (settings,domains),syntax = (parse_enzymes |> Syntax.run_parser) s in
  let sitegraphs = (domains,syntax) |> SiteGraphReactor.Sitegraphs.semantics in
  (settings,sitegraphs)
