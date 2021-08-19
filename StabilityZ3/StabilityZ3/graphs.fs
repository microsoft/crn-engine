module Microsoft.Research.Biology.Graph


type Graph = Set<int>[] // adj. list representation (output edges are stored as a set)
    
let allVisited (graph:Graph)= 
    let visited = Array.init graph.Length (fun _ -> false)
    let rec rdfs i  =
        visited.[i] <- true
        graph.[i] |> Set.filter (fun i -> not visited.[i]) |> Set.iter rdfs
    rdfs 0
    visited |> Array.forall id

let reverse (graph:Graph)= 
    let graph' = Array.init graph.Length (fun _ -> Set.empty)
        
    graph
    |> Array.iteri(fun i targets -> 
        targets 
        |> Set.iter(fun j -> graph'.[j] <- graph'.[j].Add i))
        
    graph'
   
    
let isStronglyConnected (graph:Graph) = 
    (allVisited graph) && allVisited (reverse graph)
