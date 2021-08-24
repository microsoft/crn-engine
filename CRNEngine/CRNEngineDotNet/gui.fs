// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
open InferenceSiteGraph

[<JavaScript>]
type Gui = 
  { name: string;
    settings: Crn_settings<string>;
    reactions: Reaction<string,string,string> list;
    initials: Initial<string,string> list;
    attributes: Stringmap.t<Attributes>; }
  static member from_crn (crn:Crn) =
    { name = crn.name;
      settings = crn.settings.map_plots Functional2.to_string Functional2.to_string_plot
      reactions = List.map (Reaction.map Species.to_string (Expression.to_string id) Functional2.to_string) crn.reactions;
      initials = List.map (Initial.to_gui Species.to_string (Expression.to_string id)) crn.initials;
      attributes = crn.attributes }
  member g.to_crn () : Crn = 
    let freactions = Reaction.map Species.from_string (Expression.from_string Parser.name) Functional2.from_string
    let finitials = Initial.from_gui Species.from_string (Expression.from_string Parser.name)
    let settings = g.settings.map_plots Functional2.from_string Functional2.from_string_plot
    { name = g.name;
      settings = settings.map (Expression.map (Crn.species_to_rates settings.rates));
      reactions = 
        List.map freactions g.reactions
        |> List.map (Reaction.map id id (Expression.map (Crn.species_to_rates settings.rates)))
      initials = List.map finitials g.initials;
      attributes = g.attributes }

[<JavaScript>]
type GuiModel =
  { top: Gui;
    systems: Gui list }
  static member from_model (model:Model) =
    { top = Gui.from_crn model.top; systems = List.map (fun (crn:Crn) -> Gui.from_crn crn) model.systems }
  member self.to_model () : Model =
    { top = self.top.to_crn(); systems = List.map (fun (gui:Gui) -> gui.to_crn()) self.systems }

[<JavaScript>]
type GuiIGTargetParameter = { source: string; target: string; prior: PriorKind }
[<JavaScript>]
type GuiIGEdge = { location: Location; parameters: GuiIGTargetParameter list }

/// This is the serialisation type for an inference graph. Note that I'm mapping the key of the edge dictionary, from Location to string. This because dictionaries on non-primitive types are problematic in JS. This would cause problems if a node or model had a '.' character in its name. This should not happen, because the '.' should not be allowed in a valid identifier.
[<JavaScript>]
type GuiIG =
  { task: Task option
    nodes: Map<string, GuiModel>;
    edges: Map<string, GuiIGEdge list>; 
    expanded:bool}
  static member from_ig (ig:IGraph) : GuiIG =
    let map_parameter ((source,target,prior):string*string*PriorKind) =
      { source = source
        target = target
        prior = prior }
    let map_edge ((location,parameters):Location*(string*string*PriorKind)list) =
      { location = location
        parameters = parameters |> List.map map_parameter }
    let map_location (l:Location) =
      match l with
      | NodeLoc nodeID -> nodeID
      | SystemLoc (nodeID,systemID) -> nodeID+"."+systemID
    { task = ig.task
      nodes = ig.nodes |> Map.map (fun k m -> GuiModel.from_model m)
      edges = ig.edges |> Map.toSeq |> Seq.map (fun (k,v) -> (map_location k),(v |> List.map map_edge)) |> Map.ofSeq 
      expanded = ig.expanded}
  member self.to_ig () : IGraph =
    let map_parameter (p:GuiIGTargetParameter) = p.source,p.target,p.prior
    let map_edge (e:GuiIGEdge) = e.location,(e.parameters |> List.map map_parameter)
    let map_location (l:string) : Location =
      match l.Split('.') with
      | [|n|] -> NodeLoc n
      | [|n;s|] -> SystemLoc (n,s)
      | _ -> failwithf "bad location encoding %s" l
    { task = self.task
      nodes = self.nodes |> Map.map (fun k m -> m.to_model())
      edges = self.edges |> Map.toSeq |> Seq.map (fun (k,v) -> (map_location k),(v |> List.map map_edge)) |> Map.ofSeq 
      expanded = self.expanded}