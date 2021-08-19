module Microsoft.Research.Biology.StabilityZ3.GRNs.Program

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.GRNs.GRN
    
let GenerateGRNs (species:string[], M:int) (hypothesis:GRN.GrnHypothesis) =         
    GRN.Generate species M        
    |> fst    
    |> Array.map (GRN.ToCRN hypothesis)        

let GenerateAllGRNs filename = 
    let GRNs = 
        [| [|"A"; "B"|], 2         
         ; [|"A"; "B"; "C"|], 2
         ; [|"A"; "B"; "C"|], 3
        |]           

    let allGRNs = 
        GRNs
        |> Array.collect (fun (S,M) -> 
            GRN.Generate S M        
            |> fst
            |> Array.map GRN.ToArguments                        
            )
            
    System.IO.File.WriteAllLines(filename, allGRNs)                    
            
//Note that Marcon et al. consider 3N2M and 4N2M classes
let GRNs = 
    [|[|"A"; "B"|], 2         
    ; [|"A"; "B"; "C"|], 2
    ; [|"A"; "B"; "C"|], 3
    |]     
        
let GenerateAllGRNsAndHypotheses filename =           
    let coops = [0.5; 1.0; 2.0]
    let Hs = [|"Competitive"; "NonCompetitive"; "Mixed"|]    
    let allGRNs = 
        GRNs
        |> Array.collect (fun (S,M) -> 
            GRN.Generate S M        
            |> fst
            |> Array.map GRN.ToArguments                                    
            |> Array.collect (fun g -> 
                [| for c in coops do
                    for h in Hs do
                        yield sprintf "%s %.1f %s" g c h
                |]
                )
            )
            
    System.IO.File.WriteAllLines(filename, allGRNs)         

let GenerateAllLCRNs filename =           
    let allLCRNs =                 
        GRNs
        |> Array.collect (fun (S,M) ->
            let lcrns, _ = GRN.Generate S M
            printfn "%i networks of class %iN%iM" lcrns.Length S.Length M
            lcrns
            |> Array.map GRN.ToArguments                                    
            |> Array.map (sprintf "%s 0.0 Linear")                                
        )
            
    System.IO.File.WriteAllLines(filename, allLCRNs)   

let Process solver settings (hypothesis:GrnHypothesis option) grn = 
    let hypotheses = 
        match hypothesis with 
        | Some h -> [|h|]
        | None ->  //run all
            [| GrnHypothesis.Create Linear 0.0
             ; GrnHypothesis.Create Competitive 0.5
             ; GrnHypothesis.Create Competitive 1.0
             ; GrnHypothesis.Create Competitive 2.0
             ; GrnHypothesis.Create NonCompetitive 0.5
             ; GrnHypothesis.Create NonCompetitive 1.0
             ; GrnHypothesis.Create NonCompetitive 2.0
             ; GrnHypothesis.Create Mixed 0.5
             ; GrnHypothesis.Create Mixed 1.0
             ; GrnHypothesis.Create Mixed 2.0
             |]
    
    let results = 
        hypotheses
        |> Array.map (fun h -> 
            let result, _ = GRN.CheckTuring h solver settings grn
            let log = sprintf "%s,%s,%.1f,%1.3f,%s" grn.InteractionsString (h.regulation.ToString()) h.coop result.solution.Time result.solution.Status
            printfn "%s" log
            log
        )

    results