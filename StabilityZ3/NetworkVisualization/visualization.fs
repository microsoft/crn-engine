// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.Visualization

module MathNetWrapper = Microsoft.Research.Biology.StabilityZ3.MathNetWrapper

open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.CRNEngine.Graph
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.Visualization


let TryEval (e:NumExpr) = try eval (fun _ -> nan) e |> Float with _ -> e

/// Converts a Microsoft.Research.CRNEngine.Graph object into a MSAGL drawing graph.
let GraphToMSAGL (graph:Graph) =
    let msagl = new Microsoft.Msagl.Drawing.Graph("graph")

    // Turns a string into a MSAGL color.
    let getMsaglColor (color:string) (defaultColor:Microsoft.Msagl.Drawing.Color) =
        match color with
        | "" -> defaultColor
        | color ->
            if color.StartsWith "#" then
                // Hex color. Can be either #RRGGBB or #AARRGGBB
                let alpha = color.Length = 9
                let a = if alpha then System.Byte.Parse(color.Substring(1,2), System.Globalization.NumberStyles.HexNumber) else (byte)255
                let offset = if alpha then 1 else 3
                let r = System.Byte.Parse(color.Substring(offset,2), System.Globalization.NumberStyles.HexNumber)
                let g = System.Byte.Parse(color.Substring(offset+2,2), System.Globalization.NumberStyles.HexNumber)
                let b = System.Byte.Parse(color.Substring(offset+4,2), System.Globalization.NumberStyles.HexNumber)
                Microsoft.Msagl.Drawing.Color(a,r,g,b)
            else
                // Named color. Must be one of the names in Microsoft.Msagl.Drawing.Color.
                let ct = typeof<Microsoft.Msagl.Drawing.Color>
                let props = ct.GetProperties(System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.GetProperty)
                let prop = Array.find (fun (prop:System.Reflection.PropertyInfo) -> prop.Name.ToLower() = color.ToLower()) props
                (prop.GetValue null) :?> Microsoft.Msagl.Drawing.Color

    let addNode (n:Node) =
        let node = msagl.AddNode n.id
        // Search for a MSAGL shape with the same name as my shape.
        node.Attr.Shape <- System.Enum.Parse(typeof<Microsoft.Msagl.Drawing.Shape>, n.shape.ToString()) :?> Microsoft.Msagl.Drawing.Shape
        if n.fill.IsSome then node.Attr.FillColor <- getMsaglColor n.fill.Value Microsoft.Msagl.Drawing.Color.Transparent
        if n.stroke.IsSome then node.Attr.Color <- getMsaglColor n.stroke.Value Microsoft.Msagl.Drawing.Color.Black
        if n.label.IsSome then node.LabelText <- n.label.Value
    
    let addEdge (e:Edge) =
        let edge = msagl.AddEdge(e.source,e.destination)
        if e.label.IsSome then edge.LabelText <- e.label.Value
        if e.stroke.IsSome then edge.Attr.Color <- getMsaglColor e.stroke.Value Microsoft.Msagl.Drawing.Color.Black
        if e.style.IsSome then
            // Search for a MSAGL style with the same name as my style.
            let style = System.Enum.Parse(typeof<Microsoft.Msagl.Drawing.Style>, e.style.Value.ToString()) :?> Microsoft.Msagl.Drawing.Style
            edge.Attr.AddStyle style
        ()

    List.iter addNode graph.nodes
    List.iter addEdge graph.edges

    msagl

let ToMSAGL (S:Dynamical) = ToGraph S |> GraphToMSAGL

let ToInfluenceGraph (S:Dynamical) = 
    //let S = S.MkConcreteParams //substitute generated param values in the system
    
    let sub (e:NumExpr) =         
        match S.solution with        
        | SAT (s,_) -> 
            s 
            |> Map.map(fun _ v -> Float v)            
            |> fun p -> substitute p e
            |> TryEval
            //|> MathNetWrapper.SimplifyNum
        | _ -> TryEval e    

    
    let X = S.State
    let D = 
        S.diffusion 
        |> Map.map(fun _ e -> sub e)
        |> Map.filter (fun _ e -> eval (fun _ -> nan) e <> 0.0)
    
    let J = S.J () |> Matrix.map sub
        
    let graph = new Microsoft.Msagl.Drawing.Graph("graph")            
    
    //add nodes     
    X
    |> Array.iter(fun x -> 
        let node = graph.AddNode x
        node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Ellipse                
        if D.ContainsKey(x) then
            node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.LightSteelBlue
            node.LabelText <- sprintf "%s(%s)" x (ExpressionFunctions.ToText false D.[x])
        else
            node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.IndianRed
        )
    

    let CheckProp (e:BoolExpr) =               
        let E = Array.append S.SysCst [|e|] |> Array.reduce (fun e1 e2 -> BAnd (e1,e2))
        let z3 = new Microsoft.Z3.Context()
        let ze,_ = Encoding.BoolExpr2Real z3 E
        let solver = z3.MkTactic("qfnra-nlsat").Solver
        
        let result = 
            match solver.Check ze with
            | Microsoft.Z3.Status.UNKNOWN -> failwithf "UNKNOWN (%s)" (ExpressionFunctions.BToLatex e)
            | Microsoft.Z3.Status.SATISFIABLE -> true
            | Microsoft.Z3.Status.UNSATISFIABLE -> false
            | _ -> failwith "UNEXPECTED Z3 RESULT!"

        z3.Dispose()
        result


    let CheckProps (e:NumExpr) = 
         let r1 = BGT(e,Float 0.0) |> CheckProp
         let r2 = BLT(e,Float 0.0) |> CheckProp
         let r3 = BEq(e,Float 0.0) |> CheckProp
         r1, r2, r3


    let AddEdges i j (r1, r2, r3) =                 
        let setStyle c (e:Microsoft.Msagl.Drawing.Edge) =                 
            if c then 
                e.Attr.Color <- Microsoft.Msagl.Drawing.Color.Green
            else 
                e.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red 
            if r3 then //zero is possible
                e.Attr.AddStyle(Microsoft.Msagl.Drawing.Style.Dashed)
            else   
                ()
                
        if r1 then //positive
            let edge = graph.AddEdge(X.[i], X.[j])                
            setStyle true edge

        if r2 then 
            let edge = graph.AddEdge(X.[i], X.[j])                
            setStyle false edge     
            
    //add edges            
    J.values
    |> Array.iteri(fun i row ->                                                                                                                 
        row.values
        |> Array.iteri(fun j k->                 
            match k with            
            | Float x -> 
                AddEdges i j (x>0.0, x<0.0, x=0.0)
            | _ ->            
                CheckProps k                
                |> AddEdges i j
            )
        )
                      
    graph



let MsaglToSvg width graph = 
    let renderer = new Microsoft.Msagl.GraphViewerGdi.GraphRenderer(graph)
    renderer.CalculateLayout()
                                              
    let stream = new System.IO.MemoryStream()
    let svgWriter = new Microsoft.Msagl.Drawing.SvgGraphWriter(stream,graph)                
    svgWriter.Write()                       
    stream.Flush()
    stream.Close()

    //let height = width*(graph.Height/graph.Width)
    stream.ToArray()             
    |> System.Text.Encoding.ASCII.GetString
    |> fun x -> x.[x.IndexOf("<svg")..]
    //|> sprintf "<center><svg width=\"%f\" height=\"%f\" viewBox=\"0 0 %f %f\">%s</svg></center>" width height graph.Width graph.Height

let ToSVG width (S:Dynamical) = S |> ToMSAGL |> MsaglToSvg width

let ToInfluenceSVG width (S:Dynamical) = S |> ToInfluenceGraph |> MsaglToSvg width


let TopologyToDynamical M topology : Dynamical = 
  let N = Array.length topology

  let odes = 
    topology 
    |> Array.mapi (fun i row -> 
      ( sprintf "X%d" i
      , row |> Array.mapi (fun j rij -> if rij > 0.0 then Key (sprintf "X%d" j) else Float 0.0) |> List.ofArray |> Plus 
      )
    )
  let diffusion = Seq.init M (fun i -> sprintf "X%d" i, Float 1.0) |> Map.ofSeq
  Dynamical.Create (odes, diffusion)

let TopologyToParameterisedDynamical (topology:int[][]) : Dynamical = 
  let odes = 
    topology 
    |> Array.mapi (fun i row -> 
      ( sprintf "X%d" i
      , row |> Array.mapi (fun j rij -> if rij > 0 then Key (sprintf "k%d%d" i j) * Key (sprintf "X%d" j) else Float 0.0) |> List.ofArray |> Plus 
      )
    )
  Dynamical.Create (odes, Map.empty)
