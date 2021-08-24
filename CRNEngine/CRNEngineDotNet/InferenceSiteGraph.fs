// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.InferenceSiteGraph

open System.Diagnostics
open Parser
//open Microsoft.Research.Filzbach.FilzbachAnalysis

type NodeID   = string
type SystemID = string 
type PriorKind =
| [<WebSharper.Constant "Fixed2">] Fixed2
| [<WebSharper.Constant "Normal">] Normal
| [<WebSharper.Constant "LogNormal">] LogNormal
| [<WebSharper.Constant "TruncatedNormal">] TruncatedNormal // TODO: rename Fixed2 to Fixed
                  member x.to_string = 
                      match x with 
                      | Fixed2 -> "Fixed"
                      | Normal -> "Normal"
                      | LogNormal -> "LogNormal"
                      | TruncatedNormal -> "TruncatedNormal"
type Node     = { id      : NodeID
                ; model   : Model }
[<WebSharper.NamedUnionCases>]
type Location = NodeLoc   of NodeLoc:NodeID
              | SystemLoc of SystemLoc:(NodeID * SystemID)
                member x.to_string = 
                  match x with 
                  | NodeLoc(nid) -> nid
                  | SystemLoc(nid,sysid) -> nid + "." + sysid
[<DebuggerDisplay("")>]
type IGraph   = { task : Task option
                  nodes : Map<NodeID, Model>                                                    // Nodes, indexed by ID
                  edges : Map<Location, (Location * (string * string * PriorKind) list) list>   // Adjacency list per source node
                  expanded:bool } //flag to check if IGraph has been expanded. 
                static member node_to_igraph_string (nodeid:NodeID) (model:Model) = 
                    "node " + nodeid + "{" + "systems = ["  + (model.systems |> Lib.string_of_list (fun x -> x.name) "; ")  + "]; " + "inference = " + (Inference_settings.to_string model.top.settings.inference)  + "}"
                static member edge_to_igraph_string (location:Location) (props:(Location * (string * string * PriorKind) list) list) = 
                    let from = "edge " + location.to_string + " -> "
                    let prop_to_string (prop:(Location * (string * string * PriorKind) list)) = 
                        let (toLoc,proplist) = prop
                        "[" + (proplist |> Lib.string_of_list (fun (x,y,priorkind) -> (x + " = " + priorkind.to_string)) "; ") + "] " + toLoc.to_string
                    props |> Lib.string_of_list (fun prop -> (from + (prop_to_string prop)) ) "\n"
                member x.to_string() = 
                    if x.expanded then 
                        failwith "Cannot convert an Inference Graph to string after it has been expanded."
                    let task_string = match x.task with None -> "" | Some t -> (t.to_string() + "\n");
                    match x.nodes.Count with 
                    | 0 -> task_string
                    | _ -> 
                        let models = x.nodes 
                                     |> Map.toSeq 
                                     |> Seq.map(fun (x,y) -> y)
                        let systems = models
                                      |> Seq.map (fun x -> (x.systems |> List.map (fun y -> (y.name,y))))
                                       
                        let crnStringList = 
                            match (Seq.isEmpty systems) with 
                            | true -> []
                            | false -> systems |> Seq.fold (fun acc x -> x@acc) []
                            |> Map.ofList |> Map.toList
                            |> List.map (fun (name,crn) -> "system " + name + " =\n{" + crn.to_string() + "\n}")
                        
                        let systems_string = Lib.string_of_list (fun x -> x) "\n" crnStringList 
                        let top_string = 
                            match (Seq.isEmpty models) with 
                            | true -> ""
                            | false ->
                                let top = ((models |> Seq.toList).Head).top
                                {top with settings = {top.settings with inference = Inference_settings.defaults}}.to_string()
                                //This could be problematic since we lose what top inference was. Deal with it at some point.
                        
                        let node_igraph_string = 
                            x.nodes 
                            |> Map.toList 
                            |> Lib.string_of_list (fun (id,model) -> IGraph.node_to_igraph_string id model) "\n"
                        
                        let edge_igraph_string = 
                            x.edges 
                            |> Map.toList 
                            |> Lib.string_of_list (fun (loc,props) -> IGraph.edge_to_igraph_string loc props) "\n"
                        
                        let igraphtext = 
                            match x.nodes.Count with 
                            | 1 -> 
                                let node = x.nodes |> Map.toSeq |> Seq.head |> snd
                                match node.systems with 
                                | [] -> ""
                                | _ -> node_igraph_string
                            | _ -> node_igraph_string + "\n" + edge_igraph_string
                        task_string + top_string + "\n" + systems_string + "\n" + igraphtext
                        

let defaultIGraph = { task = None
                      nodes = Map.empty 
                      edges = Map.empty 
                      expanded = false}

let toNodeID loc = match loc with 
                   | NodeLoc n        -> n
                   | SystemLoc (n, _) -> n

let printLoc loc = match loc with 
                   | NodeLoc n        -> n
                   | SystemLoc (n, s) -> n + "." + s 

//////////////////////
// Graph operations //
//////////////////////
let getRoots (g:IGraph) = 
  let nodes   = g.nodes |> Map.toSeq |> Seq.map fst |> Set.ofSeq
  let targets = g.edges |> Map.toSeq |> Seq.collect (snd >> List.map (fst >> toNodeID)) |> Set.ofSeq
  
  Set.difference nodes targets

let mapNodes (f: NodeID -> Model -> Model) (g:IGraph) = 
  {g with nodes = g.nodes |> Map.map f}

let chooseNodes (f: NodeID -> Model -> (NodeID * Model) option) (g:IGraph) = 
  let newNodes = g.nodes |> Map.toList |> List.choose (fun (k,v) -> f k v) |> Map.ofList
  let filteredEdges = 
    g.edges 
    |> Map.filter (fun source targets ->
      let allReferences = source :: (List.map fst targets) |> List.map toNodeID
      List.forall (fun k -> Map.containsKey k newNodes) allReferences
    )
  {g with nodes = newNodes; edges = filteredEdges }

let iterNodes (f: NodeID -> Model -> unit) (g:IGraph) = g.nodes |> Map.iter f

let mapEdges (f: Location -> Location -> (string * string * PriorKind) -> (string * string * PriorKind)) (g:IGraph) =
  { g with edges = g.edges
                   |> Map.map (fun source targets -> 
                     targets 
                     |> List.map (fun (target, props) -> 
                       target, props |> List.map (f source target) ))}

let collectEdges (f: Location -> Location -> (string * string * PriorKind) -> (string * string * PriorKind) list) (g:IGraph) =
  { g with edges = g.edges
                   |> Map.map (fun source targets ->
                     targets 
                     |> List.map (fun (target, props) -> 
                       target, props |> List.collect (f source target)))}


let allLocations nodeID graph =
  let node = 
    match Map.tryFind nodeID graph.nodes with 
    | Some k -> k 
    | None -> failwithf "The inference graph does not contain a node with name %s" nodeID
  NodeLoc nodeID :: (node.systems |> List.map (fun sys -> SystemLoc (nodeID, sys.name)))

let deleteNode nodeID graph =
  if not (graph.nodes.ContainsKey nodeID)
    then graph
    else 
      let locs = allLocations nodeID graph
      { graph with nodes = graph.nodes |> Map.remove nodeID 
                   edges = locs
                           |> List.fold (fun acc loc -> Map.remove loc acc) graph.edges 
                           |> Map.map (fun _ targets -> 
                                targets
                                |> List.filter (fun (targetLoc, _) -> not (locs |> List.contains targetLoc))) 
                           |> Map.filter (fun _ edges -> not (edges.IsEmpty)) }

let predecessors graph nodeID =
  let locs = allLocations nodeID graph  
  graph.edges
  |> Map.toList 
  |> List.filter (fun (_, edges) -> 
      edges 
      |> List.exists (fun (targetLoc, _) -> locs |> List.contains targetLoc)
        ) 
  |> List.map (fst >> toNodeID) 

let rec ancestors graph nodeID = 
  let parents = predecessors graph nodeID 
  if parents.IsEmpty 
    then []
    else 
      let ancs = parents |> List.collect (ancestors graph)
      parents @ ancs
  
/////////////
// Parsing //
/////////////
let pid = name .>> spaces
let plocation = pid >>= fun x ->
                  choice [ pchar '.' >>. pid >>= fun y -> preturn (SystemLoc (x, y))
                           preturn (NodeLoc x)]
let pparameter = pid >>= fun x ->
                  choice [ kw "=" >>. choice [ kw "Fixed"            >>. preturn (x, x, Fixed2)
                                               kw "Normal"           >>. preturn (x, x, Normal) 
                                               kw "LogNormal"        >>. preturn (x, x, LogNormal) 
                                               kw "TruncatedNormal"  >>. preturn (x, x, TruncatedNormal) ] 
                           preturn (x, x, TruncatedNormal)
                  ]

let pline (model:Model) (systemDefs:Map<string, Crn>) = 
  Parser.choice [
    kw "edge" 
      >>. plocation .>> kw "->" .>>. Parser.list_of pparameter
      .>>. plocation >>= fun ((loc1, props), loc2) -> preturn (Choice2Of2 <| (loc1, props, loc2) )
    kw "node" >>. pid >>= fun nodeName ->
      let model' = { model with top = {model.top with name = nodeName }}
      record model' [
              "systems", list_of name |>> fun y x -> 
                let newSystems = 
                  y 
                  |> List.map (fun sysName -> 
                      if systemDefs.ContainsKey sysName
                        then systemDefs.[sysName]
                        else failwith <| "Undefined system " + sysName + " in node " + nodeName)
                { x with systems = newSystems }
              "inference", Inference_settings.parse_defaults model.top.settings.inference |>> fun y x -> {x with top = { x.top with settings = { x.top.settings with inference = y}}}
            ]
        |>> Choice1Of2
    ]

let pinferenceGraph task top systemDefs : Parser.t<IGraph> = 
  Parser.spaces >>.
  Parser.many1 (pline top systemDefs)
    |>> fun lines -> 
          let nodes, edges = 
            lines 
            |> List.fold (fun (acc1, acc2) i -> 
                            match i with 
                            | Choice1Of2 x -> x::acc1, acc2
                            | Choice2Of2 x -> acc1,   x::acc2) ([], [])
          { task = task
            nodes = nodes 
                    |> List.map (fun n -> n.top.name, n) |> Map.ofList 
          ; edges = edges // create adjacency list
                    |> List.groupBy (fun (x,_,_) -> x)
                    |> Map.ofList 
                    |> Map.map (fun _ es -> es |> List.map (fun (_,props,target) -> (target,props)))
          ; expanded = false
          }

let parseTask =
  let defaultTask : Task = { task_type = None; copies = 1; copy_id = 1; nodes = 1}
  let ptask = 
    Parser.pTry ( Parser.kw "directive" >>. Parser.kw "task" ) // pTry is necessary in case there is a different keyword than "task" after "directive"
    >>. Parser.record defaultTask ["task",   Parser.choice [ Parser.kw "infer" >>. preturn TaskType.Infer 
                                                             Parser.kw "simulate" >>. preturn TaskType.Simulate
                                                             Parser.kw "parse" >>. preturn TaskType.Parse ] 
                                             |>> fun k s -> { s with task_type = Some k }
                                   "copies", Parser.pint32 |>> fun k s -> { s with copies = k }
                                   "copy_id", Parser.pint32 |>> fun k s -> { s with copy_id = k }
                                   "nodes", Parser.pint32 |>> fun k s -> { s with nodes = k }]
  Parser.spaces >>. Parser.opt ptask

let parse =
  parseTask .>>. Model.parse 
  >>= (fun (task, model:Model) -> 
    let systemsMap = 
      model.systems 
      |> List.map (fun c -> c.name, c) 
      |> Map.ofList
    Parser.choice [
        pinferenceGraph task model systemsMap
        preturn ( { task = task; nodes = Map.ofList [model.top.name, model]; edges = Map.empty ; expanded = false} )
      ]
   )

let from_string = Parser.from_string parse

/////////////
// Lifting //
/////////////
let expandAndLift igraph = 
  let liftedExpandedNodes, allLiftings = 
    igraph.nodes 
    |> Map.toList
    |> List.map (fun (k,m) -> 
        let expandedLiftedModel, liftings = m.expandAndLift () 
        (k,expandedLiftedModel), (m.top.name, liftings))
    |> List.unzip
    |> fun (m, pMaps) -> Map.ofList m, pMaps |> Map.ofList

  let liftedGraph =
    { igraph with nodes = liftedExpandedNodes }
    |> collectEdges (fun source target (sourceParam, targetParam, prior) -> 
        match (source, target) with
        | NodeLoc _, NodeLoc _ -> 
            // single parameter: assume that it's not multiple and do nothing
            [sourceParam, targetParam, prior] 
        | SystemLoc (sourceNodeID, sourceSystemID), SystemLoc (targetNodeID, targetSystemID) -> 
            // single or multiple parameters: expand the multiples out
            let fail msg = failwith <| sprintf "%s in edge %s.%s->%s.%s" msg sourceNodeID sourceSystemID targetNodeID targetSystemID
            let sourceNodeLiftings = match allLiftings.TryFind sourceNodeID with | Some v -> v | None -> fail <| sprintf "Source node %s not found" sourceNodeID
            let targetNodeLiftings = match allLiftings.TryFind targetNodeID with | Some v -> v | None -> fail <| sprintf "Target node %s not found" targetNodeID
            let sourceSystemLiftings = match sourceNodeLiftings.TryFind sourceSystemID with | Some v -> v | None -> fail <| sprintf "Source system %s not found" sourceSystemID
            let targetSystemLiftings = match targetNodeLiftings.TryFind targetSystemID with | Some v -> v | None -> fail <| sprintf "Target system %s not found" targetSystemID
            let sourceMultiples = match sourceSystemLiftings.TryFind sourceParam with | Some v -> v | None -> fail <| sprintf "Source parameter %s not found" sourceParam
            let targetMultiples = match targetSystemLiftings.TryFind targetParam with | Some v -> v | None -> fail <| sprintf "Target parameter %s not found" targetParam
            if sourceMultiples.Length <> targetMultiples.Length then fail <| sprintf "Mismatched multiples count (%d source, %d target)" sourceMultiples.Length targetMultiples.Length
            List.zip sourceMultiples targetMultiples
            |> List.map ( fun (x, y) -> (x,y, prior))
        | NodeLoc sourceID, SystemLoc (targetID, targetSysID) -> 
            failwith <| sprintf "Unsupported edge %s->%s.%s; only node->node and system->system edges are supported." sourceID targetID targetSysID
        | SystemLoc (sourceID, sourceSysID), NodeLoc targetID -> 
            failwith <| sprintf "Unsupported edge %s.%s->%s; only node->node and system->system edges are supported." sourceID sourceSysID targetID
        )

  {liftedGraph with expanded = true}

/////////////////
// Miscellanea //
/////////////////
let load_data dataDir (igraph:IGraph) = { igraph with nodes = igraph.nodes |> Map.map (fun _ model -> model.load_data dataDir) }

let debugString parsed = 
  let models = 
    parsed.nodes 
    |> Map.toList 
    |> List.map (snd >> fun x -> (sprintf "node %s{\n" x.top.name) + x.string()+ "\n}\n") 
    |> String.concat "\n"

  let graph =
    parsed.edges 
    |> Map.toList
    |> List.map (fun (source, targets) -> 
        let printLoc loc =
          match loc with 
          | NodeLoc nID -> nID
          | SystemLoc (nID, sID) -> nID + "." + sID
        let printEdgeProp prop = 
          match prop with 
          | Normal          -> "normal"
          | LogNormal       -> "lognormal"
          | Fixed2          -> "fixed"
          | TruncatedNormal -> "truncated"

        let sourceTxt = printLoc source
        targets 
        |> List.map (fun (target, props) ->
          let targetTxt = printLoc target
          let propsTxt =
            props 
            |> List.map (fun (x,y,z) -> sprintf "%s/%s : %s"  x y (printEdgeProp z))
            |> String.concat "; "
          sprintf "edge %s ->[%s] %s" sourceTxt propsTxt targetTxt 
          )
        |> String.concat "\n"
        )
    |> String.concat "\n"

  models + "\n\n" + graph

/////////////////////
// Graph execution //
/////////////////////
let toPrior newPriorType oldPrior (psummary:ParameterSummary) sourceTxt targetTxt = 
        match newPriorType with 
        | Fixed2 -> None
        | Normal -> 
            let dist = psummary.toNormalDist ()
            match oldPrior with
            | Some oldNormal -> Some { oldNormal with distribution = dist }
            | None           -> failwith <| sprintf "Cannot propagate parameter %s's fixed prior in %s to normal prior in %s" psummary.name sourceTxt targetTxt
        | LogNormal ->
            let dist = psummary.toLogNormalDist ()
            match oldPrior with
            | Some old  -> Some { old with distribution = dist }
            | None      -> failwith <| sprintf "Cannot propagate parameter %s's fixed prior in %s to lognormal prior in %s" psummary.name sourceTxt targetTxt
        | TruncatedNormal -> 
            let dist = psummary.toTruncatedNormalDist ()
            match oldPrior with
            | Some oldNormal -> Some { oldNormal with distribution = dist }
            | None           -> failwith <| sprintf "Cannot propagate parameter %s's fixed prior in %s to truncated normal prior in %s" psummary.name sourceTxt targetTxt

let applyEdgeToNode sourceLoc targetLoc (substitutions:Map<string,(ParameterSummary*PriorKind)>) node = 
  let f = List.map (fun (p:Parameter) -> 
      if substitutions.ContainsKey p.name
        then 
          let (summary, kind) = substitutions.[p.name]
          { p with value = summary.mle; prior = toPrior kind p.prior summary (printLoc sourceLoc) (printLoc targetLoc)}
        else p
      )
  let update_crn (crn:Crn) = crn.update_settings {crn.settings with parameters = f crn.settings.parameters}
  { top = update_crn node.top; systems = node.systems |> List.map update_crn }

/// find the roots of the input IGraph
let getNextCandidates g = 
  if g.nodes.IsEmpty
    then []
    else
      // run inference on all nodes that have no incoming edges
      let candidates = getRoots g 
      if candidates |> Set.isEmpty
        then failwith "Inference graph is not acyclic."
        else candidates |> Set.toList

/// propagate the parameter summaries for node "sourceID" in the input IGraph, and remove the node and its outgoing edges
let updateGraph (g:IGraph) (sourceID : NodeID, results : Inference.Summary) : IGraph =
      let source = g.nodes.[sourceID]
      let edgesToUpdate = 
        // lookup all edges from the source or its subsystems in the graph
        let sourceLoc      = NodeLoc sourceID
        let sysLoc (x:Crn) = SystemLoc (sourceID, x.name)
        sourceLoc :: (source.systems |> List.map sysLoc)
        |> List.collect (fun s -> 
          if g.edges.ContainsKey s
            then 
              g.edges.[s] 
              |> List.map (fun (t, props) -> 
                let newProps = 
                  props 
                  |> List.map (fun (oldP, newP, pType) -> 
                    match Map.tryFind oldP results.parameters with
                    | Some old -> newP, (old, pType)
                    | None     -> failwithf "Error: cannot find parameter %s in results" oldP
                  )
                  |> Map.ofList
                s, t, newProps)
            else [])
  
      // apply substitutions
      let updatedGraph = 
        edgesToUpdate
        |> List.fold (fun tmpGraph (sourceLoc, targetLoc, substitutions) ->
          let target = tmpGraph.nodes.[toNodeID targetLoc]
          let updatedTarget = applyEdgeToNode sourceLoc targetLoc substitutions target      
          // update graph
          {tmpGraph with nodes = tmpGraph.nodes.Add(target.top.name, updatedTarget)}
        ) g
  
      // clean up graph
      let rootNodeLocs = NodeLoc sourceID :: (updatedGraph.nodes.[sourceID ].systems 
                         |> List.map (fun x -> SystemLoc (sourceID, x.name)))
      { updatedGraph with 
          nodes = Map.remove sourceID updatedGraph.nodes
          edges = List.fold (fun x y -> Map.remove y x) updatedGraph.edges rootNodeLocs }  

let rec inferWith (f: Model -> Inference.Summary) (g:IGraph) =
  let candidates = getNextCandidates g
  if candidates.IsEmpty
    then []
    else 
      let candidateResults = 
        candidates
        |> List.map (fun nodeID -> 
            let model = g.nodes.[nodeID]
            nodeID, f model)
  
      // get parameter substitutions from the edges
      let g' = candidateResults |> List.fold updateGraph g
      (List.zip candidates candidateResults) @ inferWith f g'

let infer g = 
  if not g.expanded then 
    failwith "An Inference Graph must be expanded before running inference"
  let f (model:Model) = model.infer().to_summary()
  inferWith f g

let ancestorsSubgraph nodeID graph : IGraph = 
  let subnodes = nodeID :: ancestors graph nodeID
  graph.nodes 
    |> Map.fold (fun accGraph nodeID _ ->
      if not (subnodes |> List.contains nodeID)
        then deleteNode nodeID accGraph
        else accGraph) graph

let tryLoadOrJoinAncestors nodeID (load:Model -> Inference.Summary) g =
  let subset = (ancestorsSubgraph nodeID g).nodes |> Map.remove nodeID
  subset |> Map.map (fun node model -> load model)

let loadThenComputeNode nodeID (load:Model -> Inference.Summary) (compute:Model -> Inference.Summary) g = 
  let subset = ancestorsSubgraph nodeID g
  let f (model:Model) = 
    if model.top.name = nodeID then 
      compute model
    else
      load model
  inferWith f subset

/// Returns a set of igraphs that are collectively the same task as the supplied igraph.
let parallelise (graph:IGraph) : IGraph seq =
  seq {
    match graph.task with
    | None -> yield graph
    | Some task ->
      // Distribute copies across nodes.
      let copiesPerNode = task.copies / task.nodes
      let remainder = task.copies % task.nodes
      for i = 1 to remainder do
        yield { graph with task = Some { task with copies = copiesPerNode+1; copy_id = 1+(i-1)*(copiesPerNode+1) } }
      done
      for i = remainder+1 to task.nodes do
        yield { graph with task = Some { task with copies = copiesPerNode; copy_id = 1+remainder*(copiesPerNode+1)+(i-1)*copiesPerNode } }
      done
  }