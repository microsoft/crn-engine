module Microsoft.Research.Biology.StabilityZ3.NotebookAPI.RDnets

open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.Biology.StabilityZ3

//let And = Array.reduce (fun a b -> BAnd (a,b))

let toMatrix (m,i,j) = m |> Array.map (Array.map (float >> Float)) |> Matrix.Create, i, j
//solution set encoding 3x3 matrix * id * class (int)

let rdNets242 = 
  [ [|[|1;1|]; [|1;1|]|], 0, 1 ]

//; [|[|1;0;1|];   [|0;1;1|];  [|1;0;1|]|],  0, 2 
let rdNets362 = 
    //   w             v           u
  [ [|[|0;0;1|];   [|1;1;1|];  [|1;1;0|]|],  0, 1    
  ; [|[|0;1;1|];   [|0;1;1|];  [|1;1;0|]|],  1, 1    
  ; [|[|0;1;1|];   [|0;0;1|];  [|1;1;1|]|],  2, 1           
  ; [|[|1;1;0|];   [|0;0;1|];  [|1;1;1|]|],  3, 1   
  ; [|[|0;0;1|];   [|0;1;1|];  [|1;1;1|]|],  4, 1   
  ; [|[|1;0;1|];   [|1;0;1|];  [|0;1;1|]|],  5, 1   
  ; [|[|1;1;0|];   [|0;1;1|];  [|1;0;1|]|],  6, 1        
  ; [|[|1;0;1|];   [|0;1;1|];  [|1;1;0|]|],  0, 2   
  ; [|[|1;1;0|];   [|1;0;1|];  [|0;1;1|]|],  1, 2
  ; [|[|0;1;0|];   [|1;1;1|];  [|0;1;1|]|],  2, 2
  ; [|[|0;1;0|];   [|1;0;1|];  [|1;1;1|]|],  3, 2
  ; [|[|0;1;1|];   [|1;0;1|];  [|0;1;1|]|],  4, 2
  ; [|[|0;1;0|];   [|1;1;1|];  [|1;0;1|]|],  5, 2
  ; [|[|1;1;0|];   [|1;0;1|];  [|1;0;1|]|],  6, 2   
  ; [|[|0;1;1|];   [|1;1;1|];  [|0;1;0|]|],  0, 3
  ; [|[|0;1;0|];   [|1;1;1|];  [|1;1;0|]|],  1, 3
  ; [|[|1;1;0|];   [|0;1;1|];  [|1;1;0|]|],  2, 3
  ; [|[|1;0;1|];   [|1;1;1|];  [|0;1;0|]|],  3, 3
  ; [|[|1;1;0|];   [|1;0;1|];  [|1;1;0|]|],  4, 3
  ; [|[|1;1;1|];   [|1;0;1|];  [|0;1;0|]|],  5, 3
  ; [|[|1;1;0|];   [|1;1;1|];  [|0;1;0|]|],  6, 3
  ]


let rdNets472 = 
    //    z            w            v            u
  [ [| [|0;1;1;0|]; [|1;0;0;0|]; [|0;0;0;1|]; [|1;0;1;1|] |], 0, 1
  ; [| [|0;1;0;1|]; [|1;0;0;0|]; [|1;0;0;1|]; [|0;0;1;1|] |], 1, 1
  ; [| [|0;1;0;1|]; [|1;0;0;1|]; [|1;0;0;0|]; [|0;0;1;1|] |], 2, 1
  ; [| [|0;1;1;0|]; [|1;0;1;0|]; [|0;0;0;1|]; [|1;0;0;1|] |], 3, 1
  ; [| [|0;1;1;0|]; [|0;0;0;1|]; [|1;0;0;0|]; [|1;0;1;1|] |], 4, 1
  ; [| [|0;1;0;1|]; [|1;0;0;0|]; [|1;1;0;0|]; [|0;0;1;1|] |], 5, 1
  ; [| [|0;0;0;1|]; [|1;1;0;0|]; [|1;0;1;0|]; [|0;1;1;0|] |], 6, 1
  ; [| [|0;0;0;1|]; [|1;0;0;0|]; [|1;0;1;1|]; [|0;1;1;0|] |], 7, 1
  ; [| [|0;1;0;1|]; [|1;1;0;0|]; [|1;0;0;0|]; [|0;0;1;1|] |], 8, 1
  ; [| [|0;0;1;1|]; [|1;0;0;0|]; [|1;0;1;0|]; [|0;1;1;0|] |], 9, 1
  ; [| [|0;0;0;1|]; [|0;1;1;0|]; [|1;1;0;0|]; [|1;0;1;0|] |], 10, 1
  ; [| [|0;1;1;0|]; [|1;1;0;0|]; [|0;0;0;1|]; [|1;0;0;1|] |], 11, 1
  ; [| [|0;0;0;1|]; [|1;1;0;0|]; [|1;1;0;0|]; [|1;0;1;0|] |], 12, 1
  ; [| [|0;0;0;1|]; [|1;0;0;0|]; [|0;1;1;0|]; [|1;1;1;0|] |], 13, 1
  ; [| [|0;0;0;1|]; [|1;0;0;0|]; [|1;0;1;0|]; [|1;1;1;0|] |], 14, 1
  ; [| [|0;0;1;1|]; [|1;0;0;0|]; [|1;1;0;0|]; [|1;0;0;1|] |], 15, 1
  ; [| [|0;1;1;1|]; [|1;0;0;0|]; [|0;1;0;0|]; [|1;0;0;1|] |], 16, 1
  ; [| [|0;0;1;1|]; [|1;0;0;0|]; [|0;1;1;0|]; [|1;0;0;1|] |], 17, 1
  ; [| [|0;1;0;1|]; [|0;0;1;0|]; [|1;0;1;0|]; [|1;0;0;1|] |], 18, 1
  ; [| [|0;1;0;1|]; [|1;0;0;0|]; [|1;0;1;0|]; [|0;0;1;1|] |], 0, 2
  ; [| [|1;0;0;1|]; [|0;0;1;0|]; [|1;1;0;0|]; [|0;0;1;1|] |], 1, 2
  ; [| [|0;1;1;0|]; [|1;0;0;0|]; [|0;0;1;1|]; [|1;0;0;1|] |], 2, 2
  ; [| [|0;0;0;1|]; [|0;0;1;0|]; [|1;1;0;0|]; [|1;0;1;1|] |], 3, 2
  ; [| [|0;0;1;1|]; [|0;0;1;0|]; [|0;1;0;1|]; [|1;0;0;1|] |], 4, 2
  ; [| [|0;1;0;1|]; [|1;0;0;0|]; [|1;0;1;0|]; [|0;1;1;0|] |], 5, 2
  ; [| [|0;0;0;1|]; [|1;1;0;0|]; [|1;0;0;1|]; [|0;1;1;0|] |], 6, 2
  ; [| [|0;1;0;1|]; [|1;0;1;0|]; [|1;0;0;0|]; [|0;0;1;1|] |], 7, 2
  ; [| [|0;1;1;0|]; [|1;0;0;0|]; [|0;1;0;1|]; [|1;0;0;1|] |], 8, 2
  ; [| [|0;1;0;1|]; [|0;0;1;0|]; [|1;0;0;0|]; [|1;0;1;1|] |], 9, 2
  ; [| [|0;0;1;1|]; [|1;0;0;0|]; [|0;1;0;1|]; [|1;0;0;1|] |], 10, 2
  ; [| [|0;1;0;1|]; [|1;1;0;0|]; [|0;0;0;1|]; [|0;1;1;0|] |], 0, 3
  ; [| [|0;1;0;0|]; [|1;1;0;1|]; [|0;0;0;1|]; [|1;0;1;0|] |], 1, 3
  ; [| [|0;1;0;1|]; [|1;0;0;0|]; [|0;0;1;1|]; [|0;1;1;0|] |], 2, 3
  ; [| [|0;1;0;0|]; [|1;0;0;1|]; [|0;0;1;1|]; [|1;0;1;0|] |], 3, 3
  ; [| [|0;0;1;1|]; [|1;1;0;0|]; [|0;0;0;1|]; [|0;1;1;0|] |], 4, 3
  ; [| [|0;1;1;0|]; [|0;0;1;0|]; [|0;1;0;1|]; [|1;0;0;1|] |], 5, 3
  ; [| [|0;0;0;1|]; [|1;0;1;0|]; [|1;1;0;0|]; [|0;0;1;1|] |], 6, 3
  ; [| [|0;1;0;0|]; [|0;1;1;1|]; [|0;0;0;1|]; [|1;0;1;0|] |], 7, 3
  ; [| [|0;0;0;1|]; [|1;1;0;0|]; [|0;1;0;1|]; [|0;1;1;0|] |], 8, 3
  ; [| [|0;0;1;0|]; [|0;0;1;1|]; [|0;1;0;1|]; [|1;0;0;1|] |], 9, 3
  ; [| [|0;1;0;0|]; [|0;1;0;1|]; [|1;0;0;1|]; [|1;0;1;0|] |], 10, 3
  ; [| [|0;0;0;1|]; [|0;0;1;0|]; [|1;1;0;0|]; [|0;1;1;1|] |], 11, 3
  ; [| [|0;1;0;1|]; [|0;0;1;0|]; [|1;1;0;0|]; [|0;0;1;1|] |], 12, 3
  ; [| [|0;0;1;0|]; [|1;0;1;0|]; [|0;1;0;1|]; [|1;0;0;1|] |], 13, 3
  ; [| [|0;0;1;0|]; [|1;1;0;0|]; [|0;0;0;1|]; [|1;1;1;0|] |], 14, 3
  ; [| [|0;1;0;0|]; [|1;0;0;1|]; [|1;0;1;0|]; [|1;0;1;0|] |], 15, 3
  ; [| [|0;1;0;1|]; [|1;0;0;0|]; [|0;1;1;0|]; [|0;1;1;0|] |], 16, 3
  ; [| [|0;0;0;1|]; [|1;1;0;0|]; [|1;1;0;1|]; [|0;0;1;0|] |], 17, 3
  ; [| [|0;1;1;0|]; [|0;0;1;0|]; [|1;0;0;1|]; [|0;0;1;1|] |], 18, 3
  ; [| [|0;0;1;0|]; [|1;0;0;0|]; [|1;1;0;1|]; [|0;0;1;1|] |], 19, 3
  ; [| [|0;1;0;1|]; [|0;0;1;0|]; [|1;0;0;1|]; [|0;0;1;1|] |], 20, 3
  ; [| [|0;0;1;0|]; [|1;0;0;0|]; [|0;1;0;1|]; [|1;0;1;1|] |], 21, 3
  ; [| [|0;0;1;0|]; [|0;1;1;0|]; [|0;1;0;1|]; [|1;0;0;1|] |], 22, 3
  ; [| [|0;0;0;1|]; [|0;1;1;0|]; [|1;1;0;0|]; [|0;0;1;1|] |], 23, 3
  ; [| [|0;0;0;1|]; [|1;1;0;0|]; [|0;0;1;1|]; [|0;1;1;0|] |], 24, 3
  ; [| [|0;1;0;0|]; [|0;1;0;1|]; [|0;0;1;1|]; [|1;0;1;0|] |], 25, 3
  ; [| [|0;1;0;0|]; [|1;0;1;0|]; [|1;0;0;1|]; [|0;0;1;1|] |], 26, 3
  ; [| [|0;1;1;0|]; [|1;0;0;0|]; [|0;1;0;1|]; [|0;0;1;1|] |], 27, 3
  ; [| [|0;0;1;0|]; [|1;1;0;0|]; [|0;1;0;1|]; [|1;0;0;1|] |], 28, 3
  ; [| [|0;1;0;1|]; [|0;1;1;0|]; [|1;0;0;0|]; [|0;0;1;1|] |], 29, 3
  ; [| [|1;1;0;0|]; [|0;0;0;1|]; [|0;1;1;0|]; [|1;0;1;0|] |], 30, 3
  ; [| [|1;0;0;1|]; [|0;1;0;1|]; [|1;1;0;0|]; [|0;0;1;0|] |], 31, 3
  ; [| [|0;0;1;0|]; [|1;0;0;0|]; [|1;1;0;1|]; [|1;0;0;1|] |], 32, 3
  ; [| [|0;1;1;1|]; [|0;0;1;0|]; [|1;0;0;0|]; [|0;0;1;1|] |], 33, 3
  ]


let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs) 


let findPositions (test: 'T -> bool) (matrix: 'T [][]) : (int * int) list =
  [ for i in [0..matrix.Length-1] do
      for j in [0..matrix.[i].Length-1] do
        if test matrix.[i].[j]
          then yield (i, j) ]

let testKnown nSpecies nReactions solver settings =
  let networks = 
    match (nSpecies,nReactions) with 
    | (2,4) -> rdNets242
    | (3,6) -> rdNets362 
    | (4,7) -> rdNets472 
    | _ -> failwithf "No solutions logged for %d species and %d reactions" nSpecies nReactions
  let found = 
    networks
    |> List.map (fun (network, id, category) -> 
      let dyn = Visualization.TopologyToParameterisedDynamical network
      let dyn' = { dyn with diffusion = Map.ofList ["X1",Float 1.0; "X2",Key "Dr"] }
      let result = Solver.CheckTuring true solver settings dyn'
      let found_ij = result.solution.isSAT
      System.Console.Error.WriteLine("Category {0}, ID {1}:\t{2}", category, id, found_ij)
      found_ij, id, category
    )
    |> List.groupBy (fun (_,_,cat) -> cat)
    |> List.map (fun (_,matches_i) -> List.map (fun (network,_,_) -> network) matches_i)

  let res x = if x then "YES" else "<font color=\"red\">NO</font>"
  
  found
  |> List.mapi (fun i matches_i -> 
    matches_i 
    |> List.mapi (fun j m -> sprintf "<tr><td>%i</td><td>%s</td></tr>" j (res m)) 
    |> String.concat "\n" 
    |> sprintf "<table><tr><th>ID</th><th>Type %d</th></tr>%s</table>" (i+1)
  )
  |> List.map (sprintf "<td>%s</td>")
  |> String.concat "\n"
  |> sprintf "<table><tr>%s</tr></table>"

let testTopologies nSpecies nReactions (topologies:(int * float[][])[]) =
  //let allTopologies = xs |> Array.mapi (fun i d -> i, d.Topology)
  let networks = 
    match (nSpecies,nReactions) with 
    | (2,4) -> rdNets242
    | (3,6) -> rdNets362 
    | (4,7) -> rdNets472 
    | _ -> failwithf "No solutions logged for %d species and %d reactions" nSpecies nReactions
    |> List.map toMatrix
  
  // get all type i permutations
  let ps = 
    match nSpecies with 
    | 2 -> [ [|0;1|] ]
    | 3 -> [ [|0;1;2|]; [|1;0;2|] ] 
    | 4 -> [ [|1;2;0;3|]; [|1;2;3;0|]; [|2;1;0;3|]; [|2;1;3;0|] ] 
    | _ -> failwithf "No solutions logged for %d species" nSpecies
  let allperms = networks |> List.collect (fun (m,i,j) -> ps |> List.fold (fun acc p -> (m.Slice p p, i, j) :: acc) [])
  
  (* matches.[i].[j] will contain the list of Z3 generated CRNs of 
     type i that matches the topology at row j in RDNets. *)
  let counts = Array.init 3 (fun i -> networks |> List.filter (fun (_,_,j) -> j=(i+1)) |> List.length)
  let matches : (int * Matrix) list [][] = counts |> Array.map (fun nm -> (Array.init nm (fun j -> [])))
  
  (* find CRNs not listed in RDNets *)
  let newDiscoveries = 
    topologies
    |> Array.fold (fun ns (id, topology)->
        let topology =  topology 
                        |> Array.map (Array.map Float) 
                        |> Matrix.Create
        // update the matrix of CRNs that match Marcon's, and return a list of new CRNs
        let cperms = 
            allperms 
            |> List.filter (fun (m,_,_) -> topology = m)
            
        match cperms with
          | [] -> (id,topology) :: ns
          | ps -> // update the list of matches
                  ps 
                  |> List.iter (fun (_,j,i) -> matches.[i-1].[j] <- (id,topology)::matches.[i-1].[j])                  
                  ns
      ) []
  
  // print a report

  if not newDiscoveries.IsEmpty
    then 
      printfn "New topologies found:"
      for (id, topology) in newDiscoveries do
        printfn "%d: %s" id (Matrix.toString topology)
          
  
  let res (xs: (int * Matrix) list) = if not xs.IsEmpty then xs |> List.unzip |> fst |> sprintf "%A" else "<font color=\"red\">MISSING</font>"
  
  matches
  |> Array.mapi (fun i matches_i -> 
    matches_i 
    |> Array.mapi (fun j m -> sprintf "<tr><td>%i</td><td>%s</td></tr>" j (res m)) 
    |> String.concat "\n" 
    |> sprintf "<table><tr><th>ID</th><th>Type %d</th></tr>%s</table>" (i+1)
  )
  |> Array.map (sprintf "<td>%s</td>")
  |> String.concat "\n"
  |> sprintf "<table><tr>%s</tr></table>"

    
let testDynamical nSpecies nReactions (xs:Dynamical[]) =   
  let topologies = xs |> Array.mapi (fun i x -> (i,x.Topology ()))
  testTopologies nSpecies nReactions topologies
