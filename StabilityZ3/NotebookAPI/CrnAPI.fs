// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.NotebookAPI.CrnAPI

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine
open XPlot.Plotly

//let EnumerateEquilibria mx (s:string) = s |> Dynamical.fromCRN |> DynamicalAPI.EnumerateEqulibria mx 

let SimCRN disp (crn:Crn) = 
    let crn' = crn.update_settings { crn.settings with simulator = Oslo }
    let sim = crn'.simulate_case () |> List.head
    
    let X = sim.table.times
    sim.table.columns
    |> List.map(fun c -> 
        XPlot.Plotly.Graph.Scatter(x = X, y = c.values, name = c.name)
        )
    |> XPlot.Plotly.Chart.Plot
    |> disp
    
let SimCRNStr disp (s:string) = s |> Crn.from_string |> SimCRN disp

let MkSatCrn wmax (crn:Crn) (S:Dynamical) =     

    match S.solution with 
    | SAT (sat_pars, _) -> 
        let paras = 
            crn.settings.parameters
            |> List.map (fun p -> 
                match sat_pars.TryFind p.name with
                | Some v -> { p with value = v}
                | None -> p
            )
        let species = crn.all_species ()
        let initials : Initial<Species, Expression.t<string>> list = 
            species
            |> List.map (fun sp -> 
                match sat_pars.TryFind sp.name with
                | Some v -> Initial.create (false, (Expression.Float v), sp, None, None)
                | None   -> failwith "Missing a species assignment for initial condition generation"
            )
        let diffusibles = 
            S.diffusion 
            |> Map.toList 
            |> List.map (fun (sp,v) -> 
                sp |> Species.create |> Key.Species |> Expression.Key, 
                match v with 
                | NumExpr.Float f -> f
                | NumExpr.Key sym -> sat_pars.[sym]
                | _ -> failwith ""
                |> Expression.Float
            )

                
        let pde_settings = { crn.settings.spatial with 
                                diffusibles = diffusibles;
                                //dimensions  = dim
                                nx          = 101
                                xmax        = 10.0 * 2.0 * System.Math.PI / System.Math.Sqrt(wmax) //calculate suitable domain size
                                random      = 0.02                                    
                                }
        let plottables =  S.diffusion |> Map.toList |> List.map (fst >>  Species.create >> Key.Species >> Expression.Key)
        let sim_settings = { crn.settings.simulation with plots = plottables; points=100}                    
            
        let caption =
            let parsStr  = paras |> List.map(fun p -> sprintf "%s = %.2g" p.name p.value) |> String.concat "; " |> sprintf "Parameters: %s"
            let initsStr = species |> List.map (fun sp -> sprintf "%s = %.2g" sp.name sat_pars.[sp.name]) |> String.concat "; " |> sprintf "Steady state: %s"
            let diffStr = 
                S.diffusion 
                |> Map.toList 
                |> List.map (fun (sp,v) ->                 
                    let vv = 
                        match v with 
                        | NumExpr.Float f -> f
                        | NumExpr.Key sym -> sat_pars.[sym]
                        | _ -> failwith ""
                    sprintf "%s (%.2g)" sp vv)
                |> String.concat "; " 
                |> sprintf "Diffusible species (diffusion rates): %s"
            sprintf "%s<br>%s<br>%s<br>Max wavenumber: %f" diffStr initsStr parsStr wmax

        let sat_crn = 
            { crn with 
                        initials = initials; 
                        settings = { crn.settings with 
                                        parameters = paras; 
                                        spatial = pde_settings; 
                                        simulation = sim_settings                                                     
                                        simulator  = PDE
                                        };                                    
            }

        sat_crn, caption
    | _ -> failwith "Cannot construct CRN for Turing patterns (no solution available)"


let TryRunSpatial htmlView solveFirst crn =         
    let prepared = 
        if solveFirst 
        then
            let S = 
                crn 
                |> Dynamical.fromCRN
                |> DynamicalAPI.CheckTuringAndReturn htmlView TuringSymbolic.TuringAnalysisSettings.Default

            match S.solution with 
            | SAT _ -> 
                System.Console.Error.WriteLine("Found solution! Checking with numerical procedure...")
                let wmax = DynamicalAPI.GetWMax S |> fst
                MkSatCrn wmax crn S |> Some
            | _ -> None
        else
            (crn, "") |> Some
    match prepared with 
    | Some (crn,caption) ->
        System.Console.Error.WriteLine("Simulating...")
        (crn.simulate_case() |> List.head, caption, crn.settings.spatial.dimensions) |> Some
    | None -> None

let ProcessSpatial1D result = 
    System.Console.Error.WriteLine("Processing 1D simulation values...")
    let T = result.table.times
    result.table.columns
    |> List.map (fun c -> 
        let f = c.name.Split([|" ";",";")";"(";"="|], System.StringSplitOptions.RemoveEmptyEntries)                    
        let x = float f.[2]
        f.[0],(x,c.values)
    )
    |> List.groupBy fst
    |> List.map (fun (s,L) ->
        let X, Z = L |> List.map (fun (_,xz) -> fst xz, snd xz) |> List.unzip
        System.Console.Error.WriteLine("Species {0} (1 frames)...", s)
        s, X, T, Z |> LinearAlgebra.transpose
    )
            
let ProcessSpatial2D result =
    System.Console.Error.WriteLine("Processing 2D simulation values...")
    let xyz = 
        result.table.columns                
        |> List.map(fun c ->                                
            let f = c.name.Split([|" ";",";")";"(";"="|], System.StringSplitOptions.RemoveEmptyEntries)                    
            let x = float f.[2]
            let y = float f.[4]
            f.[0],(x,y,c.values |> Array.ofSeq)
        )
    let vals = 
        xyz
        |> List.groupBy fst
        |> List.map(fun (s,L) ->                     
            let D = L |> List.map snd
            let X,Y = D |> List.map(fun (x,y,_) -> x,y) |> List.unzip
            let X = X |> List.distinct |> List.sort
            let Y = Y |> List.distinct |> List.sort
            let GetDataMatrix (i:int) =                                 
                let data = D |> List.map(fun (x,y,z) -> (x,y), z.[i]) |> Map.ofSeq
                Array.init X.Length (fun i -> Array.init Y.Length (fun j -> data.[X.[i],Y.[j]]))                                            
            let n = 
                let _,_,Z = D |> List.head 
                Z.Length
            System.Console.Error.WriteLine("Species {0} ({1} frames)...", s, n)

            let Z = Array.init n GetDataMatrix
            //let Zf = GetDataMatrix (n-1)
            s, X, Y, Z //species * coords * 2D matrix of concentrations
        )

    //printfn "%s" (crn'.to_string())
    vals

let SimPattern htmlView solveFirst (crn:Crn) =   
    let PlotHeatmap s dim X Y Z = 
        XPlot.Plotly.Graph.Heatmap (x=X, y=Y, z=Z, name = s)
        |> XPlot.Plotly.Chart.Plot
        |> XPlot.Plotly.Chart.WithTitle (sprintf "Spatio-temporal dynamics of %s in %dD" s dim)
        |> XPlot.Plotly.Chart.WithHeight 400
        |> XPlot.Plotly.Chart.WithWidth  400
        |> XPlot.Plotly.Chart.WithXTitle "Distance (mm)"
        |> XPlot.Plotly.Chart.WithYTitle (if dim = 2 then "Distance (mm)" elif dim = 1 then "Time (sec)" else failwithf "Cannot simulate with dimension %i" dim)
        |> fun x -> x.GetInlineHtml()
                                                         
    match TryRunSpatial htmlView solveFirst crn with
    | None -> printfn "No solution found"            
    | Some (result, caption, dim) ->
        match dim with
        | 1 -> 
            let vals = ProcessSpatial1D result
            System.Console.Error.WriteLine("Plotting ({0} species)...", (List.length vals))
            vals
            |> List.map (fun (s,X,T,Z) ->
                sprintf "<td>%s</td>" (PlotHeatmap s dim X T Z)
            )
        | 2 -> 
            let vals = ProcessSpatial2D result    
            System.Console.Error.WriteLine("Plotting ({0} species)...", (List.length vals))

            vals 
            //|> List.map(fun (s,(X,Y),(Z0,Zf)) ->                                                             
            //    sprintf "<tr><td>%s</td><td>%s</td></tr>" (PlotHeatmap s X Y Z0) (PlotHeatmap s X Y Zf)
            //    )
            //|> String.concat "\n"
            //|> sprintf "<table><caption>%s</caption><tr><th>Initial</th><th>Final</th></tr>%s</table>" caption
            |> List.map(fun (s,X,Y,zframes) ->                                                             
                sprintf "<td>%s</td>" (PlotHeatmap s dim X Y (zframes |> Array.last))
            )
        | _ -> failwithf "Unknown number of dimensions: %d" dim
        |> String.concat "\n"
        |> sprintf "<table><caption>%s</caption><tr>%s</tr></table>" caption
        |> htmlView      

(*let SimPatternMovie htmlView tfinal dt (s:string) =
    let EncodeMovie(images:System.Drawing.Bitmap[]) = 
        let encoder = new System.Windows.Media.Imaging.GifBitmapEncoder()                        
        for img in images do            
            let bmp = img.GetHbitmap()                
            System.Windows.Interop.Imaging.CreateBitmapSourceFromHBitmap(bmp, System.IntPtr.Zero, System.Windows.Int32Rect.Empty, System.Windows.Media.Imaging.BitmapSizeOptions.FromEmptyOptions())
            |> System.Windows.Media.Imaging.BitmapFrame.Create
            |> encoder.Frames.Add     
            //DeleteObject(bmp); // recommended, handle memory leak
                        
        //use fs = new System.IO.FileStream("test.gif", System.IO.FileMode.Create)
        //encoder.Save(fs)
        use stream = new System.IO.MemoryStream()            
        encoder.Save(stream)

        //Add bytes for looping
        let applicationExtension = [33; 255; 11; 78; 69; 84; 83; 67; 65; 80; 69; 50; 46; 48; 3; 1; 0; 0; 0] |> Seq.map byte |> Array.ofSeq
        let imgBytes = stream.ToArray()            
        let bytes = 
            [| imgBytes |> Array.take 13
            ;  applicationExtension
            ;  imgBytes |> Array.skip 13
            |]
            |> Array.concat             
        sprintf "<img src=\"data:image/gif;base64,%s\">" (System.Convert.ToBase64String(bytes,System.Base64FormattingOptions.None))
        

    match TryRunSpatial 2 tfinal dt s with
    | None -> printfn "No solution found"            
    | Some(result,caption) -> 
        result
        |> ProcessSpatial2D
        |> List.map(fun (s,_,_,Z) ->             
            let n = Z.[0].Length            
            let mx = Z |> Array.concat |> Array.concat |> Array.max
            let mn = Z |> Array.concat |> Array.concat |> Array.min
            let MkImg k = 
                let img = new System.Drawing.Bitmap(n,n)
                for i in [0..n-1] do
                    for j in [0..n-1] do
                        let v  = 255.0*(Z.[k].[i].[j]-mn)/(mx - mn)
                        //printfn "%i,%i,%i\t%f (%i)" k i j v (int v)
                        img.SetPixel(i, j, System.Drawing.Color.FromArgb(0, 0, int v))
                img
            Array.init Z.Length MkImg
            |> EncodeMovie           
            |> sprintf "<td>%s</td>"
            )
        |> String.concat "\n"
        |> sprintf "<table><tr>%s</tr></table>"            
        |> htmlView
  *)      



let PlotBistability htmlView (crn:Crn) solution (spX, spY) num_points = 
    
    let updated_parameters = 
        crn.settings.parameters
        |> List.map (fun p ->
            match Map.tryFind p.name solution with
            | Some x -> {p with value = x}
            | None   -> p
        )
    let plots = [spX; spY] |> List.map (Species.create >> Key.Species >> Expression.Key)
    let UpdatedCrn = crn.update_settings {
        crn.settings with 
            parameters = updated_parameters;
            simulation = { crn.settings.simulation with plots = plots }
        }

    // Locate equilibria
    let x1 = solution.[spX + "_1"]
    let x2 = solution.[spX + "_2"]
    let y1 = solution.[spY + "_1"]
    let y2 = solution.[spY + "_2"]

    // Produce slightly expanded ranges
    let xmax = (Array.max [|x1; x2|]) * 1.5
    let ymax = (Array.max [|y1; y2|]) * 1.5

    // Random initial conditions
    let rand = match crn.settings.simulation.seed with Some seed -> new Rng.Random(seed) | None -> new Rng.Random()
    let xs = List.init num_points (fun _ -> rand.NextDouble() * xmax)
    let ys = List.init num_points (fun _ -> rand.NextDouble() * ymax)

    // Function to initialise the simulator
    let init xi yi = 
      UpdatedCrn.initials 
      |> List.map (fun i ->
          if i.species.name.Equals(spX)
          then {i with value = xi}
          elif i.species.name.Equals(spY)
          then {i with value = yi}
          else i
      )

    let ics = List.zip xs ys
    let sims = 
        ics
        |> List.map (fun (x,y) -> 
            let crn = {UpdatedCrn with initials = init (Expression.Float x) (Expression.Float y)}
            let ode = crn.to_sundials ()
            let table = ode.simulate ()
            let xf = List.last (table.columns.[0].values)
            let yf = List.last (table.columns.[1].values)
            let dist_1 = (x1 - xf)**2.0 + (y1-yf)**2.0
            let dist_2 = (x2 - xf)**2.0 + (y2-yf)**2.0
            let color = if dist_1 < dist_2 then "red" else "blue"
            table.columns.[0], table.columns.[1], color
        )

    let layout =
        Layout(
            xaxis = Xaxis(title = spX),
            yaxis = Yaxis(title = spY),
            legend = Legend()
        )
    let eq1 = Scatter (x=[x1], y=[y1], mode="markers", marker = Marker(size = 12, color="red"))
    let eq2 = Scatter (x=[x2], y=[y2], mode="markers", marker = Marker(size = 12, color="blue"))
    let lines  = sims |> List.map (fun (xsim, ysim, color) -> Scatter (x=xsim.values, y=ysim.values, mode = "lines", line = Line(color = color, width = 0.5)))
    let points = ics  |> List.map (fun (x0, y0) -> Scatter (x=[x0], y=[y0], mode="markers", marker = Marker(size=3, color="black")))
    let charts = List.concat [lines; points; [eq1; eq2]]
    Chart.Plot (charts, layout)
    |> Chart.WithLegend(false)
    |> fun x -> x.GetInlineHtml()
    |> htmlView


let Export name (crn:Crn) (S:Dynamical) =                 
    let wmax = DynamicalAPI.GetWMax S |> fst
    let svg = DynamicalAPI.ToSVG S
    let sat_crn,_ = MkSatCrn wmax crn S
        
    System.IO.File.WriteAllText(sprintf "%s.crn" name, sat_crn.to_string())
    System.IO.File.WriteAllText(sprintf "%s.svg" name, svg)  

let ExportTuring name (crn:Crn) =
    let S = 
        crn
        |> Microsoft.Research.Biology.StabilityZ3.Dynamical.fromCRN
        |> DynamicalAPI.TuringAnalysisDefault
    Export name crn S                         
