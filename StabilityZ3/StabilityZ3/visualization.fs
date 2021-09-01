// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.Visualization
open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.CRNEngine.Graph

let TryEval (e:NumExpr) = try eval (fun _ -> nan) e |> Float with _ -> e

/// Returns the graph for a specific solution (pass None for non-bistable solutions).
let ToGraphSolution (S:Dynamical) (i:int option) =
    //let S = S.MkConcreteParams //substitute generated param values in the system    

    let solution =
        match S.solution with
        | SAT (s,_) ->
            match i with
            | None -> Some s
            | Some i ->
                let ensureVar (e:Map<string,float>) (v:string) =
                    if Map.containsKey v e then e else
                    match Map.tryFind (sprintf "%s_%i" v i) e with
                    | None -> e
                    | Some valueFor -> Map.add v valueFor e
                Set.fold ensureVar s S.GetVars |> Some
        | _ -> None

    let sub (e:NumExpr) =
        match solution with        
        | Some s -> 
            s 
            |> Map.map(fun _ v -> Float v)            
            |> fun p -> substitute p e
            |> TryEval
            |> MathNetWrapper.SimplifyNum
        | _ -> TryEval e    

    let X = S.State
    let D = 
        S.diffusion 
        |> Map.map (fun _ e -> sub e)
        |> Map.filter (fun _ e -> eval (fun _ -> nan) e <> 0.0)

    let jac = S.J ()
    let J = Matrix.map sub jac
    
    // Add nodes 
    let makeNode x =
        // Note that if there is no diffusion, then I'm going to use the name directly.
        let label = if D.ContainsKey x then sprintf "%s(%s)" x (ExpressionFunctions.ToTextWithPrinter false (sprintf "%.2g") D.[x]) |> Some else
                    if D.Count = 0 then Some x else
                    None
        let fill = if D.ContainsKey x || D.Count = 0 then "lightsteelblue" else "indianred"
        { id = x
          label = label
          fill = Some fill
          stroke = None
          shape = None }
    let nodes = Array.map makeNode X |> Array.toList

    // Add edges
    let mapEdges i (row:Vector) =
        let mapEdge j k =
            match k with
            | Float 0.0 -> None
            | _ -> 
                { source = X.[i]
                  destination = X.[j]
                  style = None
                  stroke = None
                  label = ExpressionFunctions.ToTextWithPrinter false (sprintf "%.2g") k |> Some } |> Some
        Array.mapi mapEdge row.values |> Array.choose id
    let edges = Array.mapi mapEdges J.values |> Array.concat |> Array.toList
                  
    { nodes = nodes ; edges = edges }

let ToGraph (S:Dynamical) = ToGraphSolution S None
