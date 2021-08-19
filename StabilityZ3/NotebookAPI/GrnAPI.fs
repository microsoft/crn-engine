module Microsoft.Research.Biology.StabilityZ3.NotebookAPI.GrnAPI

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.GRNs.GRN

let GrnToGraph (g:GRN) = 
    let graph = new Microsoft.Msagl.Drawing.Graph("graph")            
                        
    g.species
    |> Array.iteri(fun i s ->             
        let node = graph.AddNode s
        node.Attr.Shape <- Microsoft.Msagl.Drawing.Shape.Circle
            
        if g.M>0 && i=0 then //diffuser with rate 1.0
            node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.LightBlue
        elif i<g.M then  //slower diffusers
            node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.AliceBlue
        else //non-diffusers
            node.Attr.FillColor <- Microsoft.Msagl.Drawing.Color.Gray
        )

    g.interactions
    |> Array.iteri(fun i L -> 
        L
        |> Array.iteri(fun j interaction -> 
            if interaction > 0 then                 
                let edge = graph.AddEdge(g.species.[i], g.species.[j])
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Green
            elif interaction < 0 then
                let edge = graph.AddEdge(g.species.[i], g.species.[j])
                edge.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red                            
            //edge.LabelText <- this.GetInteractionString i
            )
        )
                                
    graph //return


let GrnToBitmap (g:GRN) = 
    let graph = g |> GrnToGraph
    let renderer = new Microsoft.Msagl.GraphViewerGdi.GraphRenderer(graph)
    renderer.CalculateLayout()        
        
    let width = 1000.0
    let bitmap = new System.Drawing.Bitmap((int) width, (int)(graph.Height * (width / graph.Width)))//, System.Drawing.P PixelFormat.Format32bppPArgb);
    renderer.Render(bitmap);
    bitmap 
    //bitmap.Save(filename);    
        
let GrnToSvg (g:GRN) = 
    let graph = g |> GrnToGraph
    let renderer = new Microsoft.Msagl.GraphViewerGdi.GraphRenderer(graph)
    renderer.CalculateLayout()        
        
    let stream = new System.IO.MemoryStream()
    let svgWriter = new Microsoft.Msagl.Drawing.SvgGraphWriter(stream,graph)                
    svgWriter.Write()                       
    stream.Flush()
    stream.Close()

    let width = 100.0
    let height = width*(graph.Height/graph.Width)
        
    stream.ToArray()             
    |> System.Text.Encoding.ASCII.GetString
    |> sprintf "<svg width=\"%f\" height=\"%f\" viewBox=\"0 0 %f %f\">%s</svg>" width height graph.Width graph.Height   


let ExportTuring name c C (grn:string[])= 
    let solver = Solver.solverType.MkDefaultSolver (uint32 10000)
    let settings = TuringSymbolic.TuringAnalysisSettings.Default
    let R = Regulation.fromString C
    let h = GrnHypothesis.Create R c
    let S, crn = 
        grn
        |> GRN.FromArguments
        |> GRN.CheckTuring h solver settings

    match S.solution with 
    | SAT _ -> 
        let wmax = DynamicalAPI.GetWMax S |> fst
        let svg = DynamicalAPI.ToSVG S
        let sat_crn,_ = CrnAPI.MkSatCrn wmax (Microsoft.Research.CRNEngine.Crn.from_string crn.Value) S
             
        System.IO.File.WriteAllText(sprintf "%s.crn" name, sat_crn.to_string())
        System.IO.File.WriteAllText(sprintf "%s.svg" name, svg)  

    | UNSAT _  -> printfn "System cannot produce Turing patterns"
        
    | _  -> printfn "result is UNKNOWN"            
                
let VennR htmlView name (sets:Map<string,string list>) =   
    let RPath = @"C:\Program Files\R\R-4.0.2\bin\x64"

    let keys = sets |> Map.toList |> List.map fst

    let rec powerset = 
        function
        | [] -> [[]]
        | (x::xs) -> 
          let xss = powerset xs 
          List.map (fun xs' -> x::xs') xss @ xss

    let P = (powerset keys |> Set.ofSeq) - ([] |> Set.singleton)

    let setsCnt =
        P 
        |> Set.toArray
        |> Array.map(fun s -> 
            let other = 
                (keys |> Set.ofSeq) - (s |> Set.ofSeq)
                |> Set.toArray
                |> Array.map(fun ss -> sets.[ss] |> Set.ofSeq)
                |> Set.unionMany

            let S = 
                s 
                |> Seq.map(fun ss -> sets.[ss] |> Set.ofSeq)
                |> Set.intersectMany

            let n1 = S |> Set.count
            let n2 = (S - other) |> Set.count            
            s, n1, n2
            )            

    let setsStr = 
        setsCnt 
        |> Array.map(fun (S,_,n) -> sprintf "\"%s\"=%i" (S |> String.concat "&") n)
        |> String.concat ", "                
    

    let RScript = 
        [ sprintf "svg(\"%s.svg\",width=14,height=7)" name
        ; "library(eulerr)"
        ; sprintf "fit <- euler(c(%s), shape = \"ellipse\")" setsStr
        ; "plot(fit, quantities = TRUE)"
        ; "dev.off()" 
        ] |> String.concat "\n"
        
    System.IO.File.WriteAllText("temp.R", RScript)
    let cmd = System.Diagnostics.Process.Start(sprintf @"%s\R.exe" RPath,"< temp.R --no-save")
    cmd.WaitForExit()
    
    let svg = System.IO.File.ReadAllText(sprintf "%s.svg" name)
    svg |> htmlView
    
    
let MDS M (distances:float[][]) =
    let N = distances.Length              
    
    let D = (MathNet.Numerics.LinearAlgebra.CreateMatrix.DenseOfRowArrays distances).Map(fun x -> x**2.0) //squared distances
    let I = MathNet.Numerics.LinearAlgebra.CreateMatrix.DenseIdentity(N)
    let one = MathNet.Numerics.LinearAlgebra.CreateMatrix.Dense(N, N, 1.0)
    
    let J = I - (1.0/ (N|>float)) * one //centering matrix
    let B = -0.5 * J * D * J
    
    let evd = B.Evd() //eigenvectors are already sorted in decreasing order
        
    let eigval = 
        [| for i in [0..M-1] do
              yield evd.EigenValues.[i].Real
        |]
    
    let eigvec = 
        [ for i in [0..M-1] do
              yield evd.EigenVectors.Column(i)
        ] 
        |> MathNet.Numerics.LinearAlgebra.DenseMatrix.ofColumns
    
    let A = MathNet.Numerics.LinearAlgebra.DenseMatrix.ofDiagArray eigval
    
    (eigvec * A).ToRowArrays()        



  

type BenchmarkResult = 
    | SAT
    | UNSAT 
    | UNKNOWN 
    | FAILED
    
type Benchmark = 
    { grn        : string //grn arguments
    ; result     : BenchmarkResult
    ; t          : float option //computation time
    ; Regulation : string
    ; coop       : float
    ; N          : int //number of species
    ; M          : int //number of diffusers
    ; source     : string option //source file
    }
    static member Parse(s:string) = 
        let f = s.Split(',')
        if f.Length<>8 then failwithf "Incorrect file format (%i fields)" f.Length

        let result, t = 
            let ParseResult x = 
                match x with 
                | "SAT"     -> SAT
                | "UNSAT"   -> UNSAT
                | "UNKNOWN" -> UNKNOWN
                | "FAILED"  -> FAILED
                | _         -> failwithf "Unknown result: %s (%s)" s x
                
            let ParseTime x = 
                let tt = float x
                if tt >= 0.0 then Some tt
                else None

            ParseResult f.[7],  ParseTime f.[6]                                            
            //match f.Length with 
            //| 8 -> ParseResult f.[7],  ParseTime f.[6]                                            
            //| 7 -> ParseResult f.[6],  None
            //| _ -> failwithf "Unknown result: %s (%i fields)" s f.Length

        let grn = f.[3]
        let ff = grn.Split(' ')        
        { grn        = grn
        ; result     = result
        ; t          = t
        ; Regulation = f.[5]
        ; coop       = float f.[4]
        ; N          = int ff.[0]
        ; M          = int ff.[1]
        ; source     = if f.[0].Trim() <> "" then Some (f.[0].Trim()) else None
        }        
    member this.Key = this.grn, this.coop, this.Regulation
    
    static member toString (b:Benchmark) = sprintf "%s %.1f %s" b.grn b.coop b.Regulation
    
    static member serialize (b:Benchmark) =         
        let t  = 
            match b.t with 
            | Some x -> x
            | None   -> -1.0
        let source = 
            match b.source with 
            | Some x -> x
            | None   -> ""

        sprintf "%s,%i,%i,%s,%.1f,%s,%f,%A" source b.N b.M b.grn b.coop b.Regulation t b.result        

    static member serializeToBenchmark (b:Benchmark) =         
        sprintf "%s %.1f %s" b.grn b.coop b.Regulation
        
    static member Store resultsSummaryFile (benchmarks:Benchmark seq) = 
        let content = 
            benchmarks
            |> Seq.map Benchmark.serialize
            |> String.concat "\n"
            |> sprintf "Source File,Number of species (N),Number of diffusers (M),GRN,Cooperativity,Regulation,Time,Result\n%s"            
        System.IO.File.WriteAllText(resultsSummaryFile, content)        

    static member StoreBenchmarks resultsSummaryFile (benchmarks:Benchmark seq) = 
        let content = 
            benchmarks
            |> Seq.map Benchmark.serializeToBenchmark
            |> String.concat "\n"                
        System.IO.File.WriteAllText(resultsSummaryFile, content)        



    static member Load resultsSummaryFile = 
        resultsSummaryFile
        |> System.IO.File.ReadAllLines
        |> Seq.skip 1 //headers
        |> Seq.map Benchmark.Parse         
        |> Seq.map (fun b -> (b.grn,b.coop,b.Regulation), b)
        |> Map.ofSeq
        
    //load from separate files (e.g. as from AzureBatch)
    static member LoadFromOutputs folder = 
        folder
        |> System.IO.Directory.EnumerateFiles        
        |> Seq.map(fun file ->                         
            //Read outputs, split CRN models, filter out warnings
            let content = System.IO.File.ReadAllText(file)                        
            let breakId = content.IndexOf("###")
            let lines = 
                content.[0..breakId-1].Split([|"\n"|], System.StringSplitOptions.RemoveEmptyEntries)                                    
                |> Array.filter(fun line -> not (line.Contains "WARNING"))                                        
                
            //global benchmark parameters                                                                
            let grn = lines.[0]
            let grnFields = grn.Split(' ')
            let N = int grnFields.[0]
            let M = int grnFields.[1]
            lines.[1..]
            |> Array.map(fun line ->                 
                let output = line.Split(',')
                let coop = float output.[0]
                let regulation = output.[1]//Microsoft.Research.Biology.TuringZ3.GRN.Regulation.fromString output.[1]
                let time = float output.[2]
                let result = 
                    let r = AnalysisResult.from_string output.[3]
                    match r with  //use analysis result instead of converting?
                    | AnalysisResult.SAT _        -> SAT
                    | AnalysisResult.UNSAT _      -> UNSAT
                    | AnalysisResult.UNKNOWN _    -> UNKNOWN
                    | AnalysisResult.FAILED _     -> FAILED
                    | AnalysisResult.INCOMPLETE _ -> UNKNOWN
                    
                { grn        = grn
                ; result     = result
                ; t          = if time > 0.0 then Some time else None
                ; Regulation = regulation
                ; coop       = coop
                ; N          = N
                ; M          = M
                ; source     = Some file
                })                                
            )
        |> Seq.concat




//Merge a collection of GRN output files into a single summary file
let SummarizeResults folder outFile = 
    let benchmarks = Benchmark.LoadFromOutputs folder                                
    let failed = 
        benchmarks
        |> Seq.filter(fun b -> match b.result with FAILED | UNKNOWN -> true | _ -> false)                        
    Benchmark.Store (sprintf "%s.csv" outFile) benchmarks
    Benchmark.Store (sprintf "%s_FAILED.csv" outFile) failed
   

(***************************************************************************************
  *  Fill in gaps within a GRN file (as UNKNOWNs) based on a definition of benchmarks   *
  *      - benchmarks: definitions of the benchmarks that were executed                 *
  *      - resultsFolder: output files (e.g. from AzureBatch)                           *
  *      - output: where the summarized results will be stored                          *
  ***************************************************************************************)
let GenerateSummary benchmarks resultsFolder output = 
    let resultsMap = 
        resultsFolder
        |> Benchmark.LoadFromOutputs                                   
        |> Seq.groupBy (fun b -> b.Key)
        |> Seq.map (fun (key, B) -> 
            if Seq.length B <> 1 then 
                failwithf "Multiple or no benchmarks found for %A" key
            else 
                key, Seq.head B                
            )
        |> Map.ofSeq                        
        
    let mutable missing = 0    
    let MkUnknown grn M N R c =                                          
        { grn        = grn
        ; result     = UNKNOWN
        ; t          = None
        ; Regulation = R
        ; coop       = c
        ; N          = N
        ; M          = M
        ; source     = None
        }

    let allBenchmarks = 
        benchmarks
        |> System.IO.File.ReadAllLines
        |> Array.map(fun line ->                 
            let f = line.Split(' ')                
            let N = int f.[0]
            let M = int f.[1]
            let k = f.Length
            let coop = float f.[k-2]
            let regulation = f.[k-1]
            let grn = f.[0..k-3] |> String.concat " "               
            let benchmark = MkUnknown grn M N regulation coop
            if resultsMap.ContainsKey benchmark.Key then                    
                resultsMap.[benchmark.Key]
            else
                missing <- missing + 1       
                benchmark   //return as Unknown to have the complete list of benchmarks
            )                                        
    printfn "%i missing benchmarks" missing        
    allBenchmarks |> Benchmark.Store output        


let Compare showDetails (files:string[]) =                 
    let data = files |> Array.map Benchmark.Load
    let keys = data |> Array.map (Map.toSeq >> Seq.map fst) |> Seq.concat |> Seq.distinct
    
    keys
    |> Seq.map(fun key -> 
        let flag = 
            data 
            |> Array.map(fun D -> if D.ContainsKey key then sprintf "%A" D.[key].result else "N/A")
        let times = 
            data 
            |> Array.map(fun D -> if D.ContainsKey key && D.[key].t.IsSome then D.[key].t.Value else -1.0)            
        flag, times
        )                                
    |> Seq.groupBy fst
    |> Seq.map(fun (key, L) -> key, L |> Seq.length, L |> Seq.map(fun b -> b))
    |> Seq.sortBy (fun (_, n, _) -> n)
    |> Seq.iter(fun (key,n,times) -> 
        printfn "%s\t%i" (String.concat "\t" key) n
        if showDetails then 
            times   |> Seq.iter(fun (_,t) -> printfn "\t%A" t)
        )        