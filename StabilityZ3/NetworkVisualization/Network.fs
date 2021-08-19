module Microsoft.Research.Biology.StabilityZ3.Network

let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

type EdgeType = Pos | Neg | Both | Blank


(* Node type conventions:
    0: Morphogen
    1: Internal species
    2: Empty
*)
type CGraph = 
    { Nodes : int[]
    ; Edges : EdgeType[][]
    }
    static member Create (nodes:int[]) = 
        { Nodes = nodes
        ; Edges = Array.init nodes.Length (fun _ -> Array.init nodes.Length (fun _ -> Blank))
        }
    
    member this.AddEdge source target t =                 
        match this.Edges.[source].[target], t with 
        | Blank, _  -> this.Edges.[source].[target]<- t
        | Pos, Neg  -> this.Edges.[source].[target]<- Both
        | Neg, Pos  -> this.Edges.[source].[target]<- Both
        | _         -> ()
    
    member this.Reorder (order:int[]) = 
        let nodes' = order |> Array.map(fun i -> this.Nodes.[i])
        let edges' = order |> Array.map(fun i -> order |> Array.map (fun j -> this.Edges.[i].[j]))
        { Nodes = nodes'
        ; Edges = edges' 
        }        
    
    member this.Sort() = 
        let order = this.Nodes |> Array.indexed |> Array.sortBy snd |> Array.map fst        
        this.Reorder order
        
    //all orders betweeen the same class of nodes
    member this.Isomorphisms() = 
        this.Nodes 
        |> Array.groupBy id
        |> Array.sortBy fst
        |> Array.map(fun (c, L) -> [0..L.Length-1] |> permutations |> List.ofSeq)                        
        |> Array.fold (fun acc L -> 
            let offset = if List.isEmpty acc then 0 else acc |> List.head |> List.length            
            let L' = L |> List.map (fun l -> l |> List.map ((+) offset))            
            let acc' = 
                if List.isEmpty acc then L' 
                else
                    acc 
                    |> List.fold(fun acc l1 -> 
                        let c = L' |> List.map(fun l2-> l1@l2)
                        acc@c) []

            acc'
            )  List.empty
        |> List.map Array.ofList
        |> Array.ofList
            
    static member Distance (a:CGraph) (b:CGraph) = 
        let A = a.Sort()
        let B = b.Sort()                 
       
        let EdgeDistance (eA:CGraph) (eB:CGraph) = //distance //if e<>e' then 1 else 0
            Array.map2 (fun e e' ->if e<>e' then 1 else 0) (Array.concat eA.Edges) (Array.concat eB.Edges)
            |> Array.sum
        
        if A.Nodes<>B.Nodes then 
            failwith "Incompatible CGraphs"
        else                    
            a.Isomorphisms()
            |> Array.map(A.Reorder >> EdgeDistance B)
            |> Array.min
            
    static member DistanceMatrix (G:CGraph[]) =         
        let D =        
            Array.init G.Length (fun _ ->
                Array.init G.Length (fun _ -> 0.0))

        for i in [0..G.Length-2] do 
            for j in [i+1..G.Length-1] do 
                let d = CGraph.Distance G.[i] G.[j] |> float
                D.[i].[j] <- d
                D.[j].[i] <- d
        D
        
        
    static member ToSVG width (g:CGraph) =     
        
        let graph = new Microsoft.Msagl.Drawing.Graph("graph")    
        let ApplyStyle x (node:Microsoft.Msagl.Drawing.Node) = 
            node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Circle
            match x with                 
            | 0 -> //morphogen (D=1.0)            
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.LightBlue
            | 1 -> //morphogen (D=?)
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.AliceBlue
            | 2 -> //factor                    
                ()
            | 3 ->  //empty
                node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.LightGray
            | _ -> failwith "undefined shape"                  
        
        //nodes
        let nodes = 
            g.Nodes 
            |> Array.map (fun n -> 
                let name = System.Guid.NewGuid().ToString()                
                let gn = graph.AddNode(name)                                  
                gn.LabelText <- ""
                ApplyStyle n gn
                name
                )
    
        //interactions
        g.Edges
        |> Array.indexed
        |> Array.map (fun (i,x) -> x |> Array.indexed |> Array.map(fun (j,y) -> (i,j,y)))
        |> Array.concat    
        |> Array.iter(fun (i,j,edge) ->             
            match edge with
            | Blank -> ()
            | Pos -> 
                let edge = graph.AddEdge(nodes.[i], nodes.[j])                
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Green 
            | Neg -> 
                let edge = graph.AddEdge(nodes.[i], nodes.[j])
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red 
            | Both ->
                let edge = graph.AddEdge(nodes.[i], nodes.[j])
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Yellow                
            )
    

    
        let renderer = new Microsoft.Msagl.GraphViewerGdi.GraphRenderer(graph);
        renderer.CalculateLayout();

        let stream = new System.IO.MemoryStream()
        let svgWriter = new Microsoft.Msagl.Drawing.SvgGraphWriter(stream,graph)

        svgWriter.Write()               

        stream.Flush()
        stream.Close()
        
        let height = width*(graph.Height/graph.Width)

        
        stream.ToArray()             
        |> System.Text.Encoding.ASCII.GetString
        |> sprintf "<svg width=\"%f\" height=\"%f\" viewBox=\"0 0 %f %f\">%s</svg>" width height graph.Width graph.Height
    
        
        //System.IO.File.WriteAllText(filename, svg)    
        //member this.SaveSVG filename style =         


let Crn2AbstractCGraph M (SR:int[][]) (SP:int[][]) (rxns: int[])=      //(T:string[])
    let S = SR.Length
    //let M = SR.[0].Length
    //printfn "S=%i, M=%i" S M
    //let rxns = [0..M-1]
    
    let cgraph = 
        [| Array.replicate 1 0     //morphogen nodes (D=1.0)
        ;  Array.replicate (M-1) 1 //morphogen nodes (D=?)
        ;  Array.replicate (S-M) 2 //fixed species nodes
        ;  [|3|]                   //empty node
        |]
        |> Array.concat
        |> CGraph.Create

    //iter through reactants and add edges
    for s in [0..S-1] do
        let prod = rxns |> Seq.exists (fun r -> ([0..S-1] |> Seq.forall (fun s' -> SR.[s'].[r] = 0)) && SP.[s].[r]>0)
        let deg =  rxns |> Seq.exists (fun r -> ([0..S-1] |> Seq.forall (fun s' -> SP.[s'].[r] = 0)) && SR.[s].[r]>0)                
        if prod then 
          cgraph.AddEdge S s Pos
        if deg then 
          cgraph.AddEdge s S Pos //Does this make sense?

        for s' in [0..S-1] do
            let pos =  rxns |> Seq.exists (fun r -> SR.[s].[r] > 0 && (SP.[s'].[r]-SR.[s'].[r])>0 )
            let neg =  rxns |> Seq.exists (fun r -> SR.[s].[r] > 0 && (SP.[s'].[r]-SR.[s'].[r])<0 )            
            
            if pos then 
              cgraph.AddEdge s s' Pos
            if neg then 
              cgraph.AddEdge s s' Neg
              
    (*  
    let crntext = 
        rxns 
        |> Seq.map(fun r -> T.[r])
        |> String.concat "; "
        |> fun x -> System.Text.RegularExpressions.Regex.Replace(x,"\{k\d+_\d+\}","").Replace(" ","")
  
    crntext, 
    *)
    cgraph
    


let GrnFileToGraph M file =     
    file
    |> System.IO.File.ReadAllLines
    |> Array.head
    |> fun x -> 
        let I = x.[2..].Split(';') |> Array.map (fun y -> y.Split ',' |> Array.map int)        
        let S = I.Length
        //let M = 2
        let cgraph = 
            [| Array.replicate 1 0     //morphogen nodes (D=1.0)
            ;  Array.replicate (M-1) 1 //morphogen nodes (D=?)
            ;  Array.replicate (S-M) 2 //fixed species nodes
            ;  [|3|]                   //empty node
            |]
            |> Array.concat
            |> CGraph.Create

        for s in [0..S-1] do
            cgraph.AddEdge S s Pos
            cgraph.AddEdge s s Neg
            for s' in [0..S-1] do                                        
              if I.[s].[s']>0 then 
                  cgraph.AddEdge s s' Pos
              elif I.[s].[s']<0 then 
                  cgraph.AddEdge s s' Neg
              

        let crn_name = file.Substring(file.LastIndexOf("\\")+1)
        cgraph, crn_name

let LoadCrnDefinitionFiles path flag S P R = 
    let Stoich  =
        sprintf @"%s\super_S%i_P%i_stoich.tsv" path S P
        |> System.IO.File.ReadAllLines
        |> Array.map (fun x -> x.Split('\t') |> Array.map int)
    
    let StoichReactant = 
        sprintf @"%s\super_S%i_P%i_stoichReactant.tsv" path S P
        |> System.IO.File.ReadAllLines
        |> Array.map (fun x -> x.Split('\t') |> Array.map int)
    
    let StoichProduct = 
        Stoich
        |> Array.mapi(fun i S -> 
            Array.map2 (fun s s' -> s+s') S StoichReactant.[i]
            )        
    
    let RxnTexts = 
        sprintf "%s\super_S%i_P%i_reactions.csv" path S P
        |> System.IO.File.ReadAllLines
        |> Array.map (fun x -> x.Split(',').[1])
        
    let sat_rxn_ids = 
        sprintf @"%s\%s_S%i_P%i_R%i.csv" path flag S P R
        |> System.IO.File.ReadAllLines    
        |> Array.map (fun x -> x.Split(',') |> Array.map int)
                        
    Stoich, StoichReactant, StoichProduct, sat_rxn_ids, RxnTexts



let LoadCrnsAsCGraph path flag M S P R =    
    let _, SR, SP, sat_rxn_ids, T  = LoadCrnDefinitionFiles path flag S P R                            
    sat_rxn_ids
    |> Array.map(fun I -> 
        let crn = I |> Seq.map(fun i -> T.[i]) |> String.concat " |\n"
        Crn2AbstractCGraph M SR SP I, crn)
       
