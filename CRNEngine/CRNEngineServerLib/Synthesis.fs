module Microsoft.Research.CRNEngineServerLib.Synthesis

open Microsoft.Research.CRNEngineServerLib.Serialisation
open System.Net.WebSockets
open Messages
open Microsoft.Research.CRNEngine
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine.Graph
open Microsoft.Research.CRNEngine.JSAPI

let processGetBistabilityPlot (crn:Gui) (solution:Map<string,float>) (spX:string) (spY:string) (num_points:int) (webSocket:WebSocket) =
    let sendObject = sendObject webSocket
    let crn = crn.to_crn()
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
    let result =
        if solution.ContainsKey (spX+"_1") && solution.ContainsKey (spX+"_2") && solution.ContainsKey (spY+"_1") && solution.ContainsKey (spY+"_2") then
            let x1 = solution.[spX + "_1"]
            let x2 = solution.[spX + "_2"]
            let y1 = solution.[spY + "_1"]
            let y2 = solution.[spY + "_2"]

            // Produce slightly expanded ranges
            let xmax = (Array.max [|x1; x2|]) * 1.5
            let ymax = (Array.max [|y1; y2|]) * 1.5

            // Random initial conditions
            let rand = match crn.settings.synthesis.seed with Some seed -> new Rng.Random(seed) | None -> new Rng.Random()
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
                    let state1 = dist_1 < dist_2
                    table.columns.[0], table.columns.[1], state1
                )
            let sims1, sims2 = List.partition (fun (_,_,state1) -> state1) sims
            let sims1X, sims1Y, _ = List.unzip3 sims1
            let sims2X, sims2Y, _ = List.unzip3 sims2
    
            { speciesX = spX
              speciesY = spY
              state1x = x1
              state1y = y1
              state2x = x2
              state2y = y2
              initialsX = xs
              initialsY = ys
              state1simsX = List.map (fun (c:Column<float>) -> c.values) sims1X
              state1simsY = List.map (fun (c:Column<float>) -> c.values) sims1Y
              state2simsX = List.map (fun (c:Column<float>) -> c.values) sims2X
              state2simsY = List.map (fun (c:Column<float>) -> c.values) sims2Y }
        else
            { speciesX = spX
              speciesY = spY
              state1x = 0.0
              state1y = 0.0
              state2x = 0.0
              state2y = 0.0
              initialsX = []
              initialsY = []
              state1simsX = []
              state1simsY = []
              state2simsX = []
              state2simsY = [] }
    sendObject { Response_Bistability.mtype = "bistability"
                 Response_Bistability.plot = result }

let processSynthesisRequest (ig:GuiIG) (nodeId:string) (crnId:string) (webSocket:WebSocket) = 
    match ig.task with Some t -> (match t.task_type with Some TaskType.Parse -> failwith "this model is designed for parsing only" | _ -> ()) | _ -> ()
    let model = ig.nodes.Item(nodeId)
    let crn = List.find (fun (crn:Gui) -> crn.name = crnId) (model.top::model.systems)
    let crn = crn.to_crn()
    let dyn = Dynamical.fromCRN crn
    let timeout = crn.settings.synthesis.timeout
    let solver = 
        match crn.settings.synthesis.solver with
        | Z3Solver.NLSat -> match timeout with Some t -> Solver.NlsatTO ((uint32 t)*1000u) | None -> Solver.Nlsat
        | Z3Solver.Portfolio -> match timeout with Some t -> Solver.PortfolioTO ((uint32 t)*1000u) | None -> Solver.Portfolio
    let res = 
        match crn.settings.synthesis.mode with
        | Synthesis_mode.Multistability ->
            printfn "Synthesizing parameters for multistability..."
            dyn |> Solver.CheckBistability true solver false crn.settings.synthesis.seed
        | Synthesis_mode.Turing ->
            printfn "Synthesizing parameters for Turing instability..."
            let turingSettings = TuringSymbolic.TuringAnalysisSettings.Default
            dyn |> Solver.CheckTuring true solver { turingSettings with print_status = false; seed = crn.settings.synthesis.seed }
    let message =
        match res.solution with
        | AnalysisResult.UNSAT _ -> "Unsatisfiable"
        | AnalysisResult.UNKNOWN b -> sprintf "No solution found after %1.1f seconds" b.time
        | AnalysisResult.FAILED _  -> "Synthesis procedure failed"
        | AnalysisResult.INCOMPLETE _ -> "Synthesis procedure did not complete"
        | AnalysisResult.SAT (_, b) -> sprintf "Solution found after %1.1f seconds" b.time
    let values =
        match res.solution with
        |AnalysisResult.SAT (v,_) ->
            let addBounds (k:string) v =
                match Map.tryFind k res.bounds with
                | None -> {value=v;lowerBound=None;upperBound=None}
                | Some (lb,ub) -> {value=v;lowerBound=lb;upperBound=ub}
            Map.map addBounds v
        | _ -> Map.empty
    let code, updatedCrn =
        match res.solution with
        | AnalysisResult.SAT (v,_) ->
            let updateParameter (p:Parameter) =
                match Map.tryFind p.name v with
                | Some value -> { p with value = value }
                | None -> p
            let updatedCrnParameters = List.map updateParameter crn.settings.parameters 
            let updatedCrnSettings = { crn.settings with parameters = updatedCrnParameters }
            let updatedCrn = { crn with settings = updatedCrnSettings }
            updatedCrn.to_string() |> Some, Gui.from_crn updatedCrn |> Some
        | _ -> None, None
    let ode_str,Jstr,Dstr,cst_str = res.ToEquationsSplit true true
    let isTuring = crn.settings.synthesis.mode = Synthesis_mode.Turing
    let Dstr = if isTuring then "" else Dstr
    let equations = { rateEquations=ode_str;jacobian=Jstr;diffusion=Dstr;csts=cst_str }
    let dispersion:SynthesisDispersionResult option =
        match res.solution with
        | AnalysisResult.SAT _ ->
            let X = MathNet.Numerics.Generate.LogSpaced(101,-2.0,2.0)
            let Y = TuringNumerical.dispersion X res
            if Y.Length = 0 || System.Double.IsNaN(Y.[0]) then None else
            let x0, y0 = X.[0],Y.[0]
            let x1, y1 = Array.zip X Y |> Array.maxBy snd
            let xMin = match crn.settings.plot.x_min with Some f -> f | None -> let m = (min x0 x1) in if m < 0. then m * 1.1 else m * 0.9
            let xMax = match crn.settings.plot.x_max with Some f -> f | None -> (max x0 x1) * 2.
            let yMin = match crn.settings.plot.y_min with Some f -> f | None -> let m = (min y0 y1) in if m < 0. then m * 1.1 else m * 0.9
            let yMax = match crn.settings.plot.y_max with Some f -> f | None -> (max y0 y1) * 2.
            Some { markersX = [|x0;x1|]; markersY = [|y0;y1|]; plotX = X; plotY = Y; xMin = xMin; xMax = xMax; yMin = yMin; yMax = yMax }
        | _ -> None
    let jacobian:Graph option =
        match res.solution with
        | AnalysisResult.SAT (s,_) ->
            let bistable = res.GetVars |> Seq.exists (fun e -> s.ContainsKey e |> not)
            if bistable then
                let graph1 = Visualization.ToGraphSolution res (Some 1)
                let graph2 = Visualization.ToGraphSolution res (Some 2)
                let map graph i =
                    { nodes = List.map (fun n->{n with id=sprintf "%s_%d" n.id i}) graph.nodes
                      edges = List.map (fun e->{e with source=sprintf "%s_%d" e.source i; destination=sprintf "%s_%d" e.destination i}) graph.edges }
                let mgraph1 = map graph1 1
                let mgraph2 = map graph2 2
                { nodes = mgraph1.nodes@mgraph2.nodes; edges = mgraph1.edges@mgraph2.edges } |> Some
            else
                Visualization.ToGraph res |> Some
        | _ -> None
    let result:SynthesisResult = { message=message; values=values; equations=equations; dispersion=dispersion; jacobian=jacobian; code=code; crn=updatedCrn }
    let sendObject = sendObject webSocket
    sendObject { Response_Synthesis.mtype = "synthesis"
                 Response_Synthesis.result = result }
