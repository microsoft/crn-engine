[<JavaScript>]
module Microsoft.Research.ReactionDiffusion.Solver2d

open Microsoft.Research.ReactionDiffusion.Lib

type point = 
  { t : float
  ; u : float[][][]
  }


// General 2d solver
let solver (t0:float) (u0:float[][][]) (f:float -> float[] -> float[]) (nx:int) (numspecies:int)
    (r:float[]) 
    (inverter:(float[] -> float[]) []) 
    (Ba:float[][]) (Bb:float[][]) (Bc:float[][]) 
    (uplot:float[][][] -> float[][][])
    (settings:settings) = 
    
    let dt2 = settings.dt * 0.5
    let mutable timerec = t0
    let utemp = Array.init<float[][]> numspecies (fun _ -> Array.init nx (fun _ -> Array.zeroCreate nx))
    let ui = Array.zeroCreate numspecies

    let u = Array.zeroCreate<float[][]> numspecies
    for k in 0..(numspecies-1) do
        let u_k = Array.init nx (fun _ -> Array.zeroCreate nx)
        for i in 0..(nx-1) do
            for j in 0..(nx-1) do
                u_k.[i].[j] <- u0.[i].[j].[k]
        u.[k] <- u_k

    let rhs = Array.init numspecies (fun _ -> Array.zeroCreate nx)

    seq {
        yield {t=timerec; u=uplot u}

        while (timerec < settings.tmax) do

            for t in 0..(settings.thin-1) do
            
                timerec <- timerec + settings.dt

                // Step 1 of ADI - Differentiate in the x-direction
                for j in 0..(nx-1) do
                    for i in 0..(nx-1) do
                        for k in 0..(numspecies-1) do
                            ui.[k] <- u.[k].[i].[j]     // ND: Costly. Perhaps switch dimensions of array so its u.[i].[j].[k]
                    
                        let fi = f timerec ui
                        for k in 0..(numspecies-1) do
                            if (r.[k] > 0.0) then
                                let left = u.[k].[i].[(j-1+nx) % nx]
                                let right = u.[k].[i].[(j+1) % nx]
                                rhs.[k].[i] <- Ba.[k].[j] * left + Bb.[k].[j] * ui.[k] + Bc.[k].[j] * right + dt2 * fi.[k]
                            else
                                utemp.[k].[i].[j] <- ui.[k] + dt2 * fi.[k];

                    for k in 0..(numspecies-1) do
                        if (r.[k] > 0.0) then
                            let x = inverter.[k] rhs.[k]
                            for i in 0..(nx-1) do
                                utemp.[k].[i].[j] <- x.[i]

                // Step 2 of ADI - Differentiate in the x-direction
                for i in 0..(nx-1) do
                    for j in 0..(nx-1) do                    
                        for k in 0..(numspecies-1) do
                            ui.[k] <- utemp.[k].[i].[j];
                        
                        let fi = f timerec ui
                        for k in 0..(numspecies-1) do                        
                            if (r.[k] > 0.0) then
                                let left = utemp.[k].[(i-1+nx) % nx].[j]    // F# doesn't calculate modulus properly, so need to add nx-2 here
                                let right = utemp.[k].[(i+1) % nx].[j]
                                rhs.[k].[j] <- Ba.[k].[i] * left + Bb.[k].[i] * ui.[k] + Bc.[k].[i] * right + dt2 * fi.[k]
                            else
                                u.[k].[i].[j] <- ui.[k] + dt2 * fi.[k]

                    for k in 0..(numspecies-1) do
                        if (r.[k] > 0.0) then
                            u.[k].[i] <- inverter.[k] rhs.[k]

            yield {t=timerec; u=uplot u}
    }

/// Numerical solution of 2d reaction-diffusion equations with periodic boundaries
let periodic_solve (t0:float) (u0:float[][][]) (f:float -> float[] -> float[]) (settings:settings) : seq<point> =

    let numspecies = settings.numspecies
    let nx = settings.nx
    let dt2 = settings.dt * 0.5
    let dx = (settings.xmax - settings.xmin) / (float)(settings.nx-1) // TODO: should be nx-1 according to Solver2D?
    let r = settings.D |> Array.map (fun rate -> rate * dt2 / (dx * dx))
    
    // Set up B matrices for right hand side processing
    let Ba = Array.init numspecies (fun i -> Array.create (nx-1) r.[i])
    let Bb = Array.init numspecies (fun i -> Array.create (nx-1) (1.0-2.0*r.[i]))
    let Bc = Array.init numspecies (fun i -> Array.create (nx-1) r.[i])
    
    // Set up inverter functions (Thomas' algorithm)
    let inverter = Array.init numspecies (fun i -> 
        
        let ri = r.[i]
        let aj = -ri
        let bj = (1.0+2.0*ri)
        let cj = -ri
        
        let a_i = Array.create (nx-1) aj
        let b_i = [| [| 2.0*bj |]; Array.create (nx-3) bj; [| bj + aj*cj/bj |] |] |> Array.concat
        let c_i = Array.create (nx-1) cj
        let beta = -bj        
        let v_i = [| [| beta |]; Array.zeroCreate (nx-3); [| cj |] |] |> Array.concat
        let w_i = [| [| 1.0 |]; Array.zeroCreate (nx-3); [| aj/beta |] |] |> Array.concat
        let z_i = Solver.TridiagonalAlgorithm a_i b_i c_i v_i
     
        fun rhs -> Solver.ApplySMInvertMatrix a_i b_i c_i rhs w_i z_i
    )

    // Extract the plots from a solution point u
    let nplots = settings.plotlocs.Length    
    let uplot (u:float[][][]) = 
        Array.init nx (fun ix -> 
            let jx = if ix < nx-1 then ix else 0
            Array.init nx (fun iy ->
                let jy = if iy < nx-1 then iy else 0
                Array.init nplots (fun j -> 
                    u.[settings.plotlocs.[j]].[jx].[jy]
                )
            )
        )
    
    // General 2d solver
    solver t0 u0 f (nx-1) numspecies r inverter Ba Bb Bc uplot settings

 /// Numerical solution of 2d reaction-diffusion equations with Neumann boundaries
let neumann_solve (t0:float) (u0:float[][][]) (f:float -> float[] -> float[]) (settings:settings) : seq<point> =

    let numspecies = settings.numspecies
    let nx = settings.nx
    let dt2 = settings.dt*0.5
    let dx = (settings.xmax - settings.xmin) / (float)(settings.nx-1) // TODO: should be nx-1 according to Solver2D?
    let r = settings.D |> Array.map (fun rate -> rate * dt2 / (dx * dx))
    
    // Specify rhs matrices
    let Ba = Array.init numspecies (fun i -> 
        [| [| 0.0 |]; (Array.create (nx-1) r.[i]) |] |> Array.concat)
    let Bb = Array.init numspecies (fun i -> 
        [| [| 1.0-r.[i] |]; Array.create (nx-2) (1.0-2.0*r.[i]); [| 1.0-r.[i] |] |] |> Array.concat)
    let Bc = Array.init numspecies (fun i -> 
        [| Array.create (nx-1) r.[i]; [| 0.0 |] |] |> Array.concat)
    
    // Set up inverter functions (Sherman-Morrison) and assign B entries as a side effect
    let inverter = Array.init numspecies (fun i -> 
        let ri = r.[i]
        let aj = -ri
        let bj = (1.0+2.0*ri)
        let cj = -ri
        
        let a_i = Array.create nx aj
        let b_i = [| [| 1.0+ri |]; Array.create (nx-2) bj; [| 1.0+ri |] |] |> Array.concat
        let c_i = Array.create nx cj

        Solver.TridiagonalAlgorithm a_i b_i c_i
    )

    // Extract the plots from a solution point u
    let nplots = settings.plotlocs.Length
    let uplot (u:float[][][]) = 
        Array.init nx (fun ix -> 
            Array.init nx (fun iy ->
                Array.init nplots (fun j -> 
                    u.[settings.plotlocs.[j]].[ix].[iy]
                )
            )
        )
    
    // General 2d solver
    solver t0 u0 f nx numspecies r inverter Ba Bb Bc uplot settings

    
let solve (t0:float) (u0:float[]) (f:float -> float[] -> float[]) (settings:settings) : point[] =
  failwith "Not implemented yet"