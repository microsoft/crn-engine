// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.ReactionDiffusion.Solver1d

open Microsoft.Research.ReactionDiffusion.Lib

type point = 
  { t : float
  ; u : float[][]
  }

let solver (t0:float) (u0:float[][]) (f:float -> float[] -> float[]) (nx:int) (numspecies:int) 
    (r:float[]) 
    (inverter:(float[] -> float[]) []) 
    (Ba:float[][]) (Bb:float[][]) (Bc:float[][]) 
    (uplot:float[][] -> float[][])
    (settings:settings) = 
    
    let mutable timerec = t0;
    let u = Array.init numspecies (fun _ -> Array.zeroCreate nx)
    let fv = Array.init numspecies (fun _ -> Array.zeroCreate nx) 
    let ui = Array.zeroCreate numspecies
    let rhs = Array.zeroCreate nx
    let dt = settings.dt

    for j in 0..(numspecies-1) do
        for ix in 0..(nx-1) do
            u.[j].[ix] <- u0.[ix].[j]

    seq {
        yield {t=timerec; u=uplot u}

        while timerec < settings.tmax do
            for _ in 0..(settings.thin-1) do
                timerec <- timerec + dt;

                for ix in 0..(nx-1) do
                    for j in 0..(numspecies-1) do
                        ui.[j] <- u.[j].[ix];
                    let fi = f timerec ui
                    for j in 0..(numspecies-1) do
                        fv.[j].[ix] <- fi.[j] * dt;

                for j in 0..(numspecies-1) do
                    if (r.[j] > 0.0) then
                        for k in 0..(nx-1) do
                            let left = u.[j].[(k-1+nx) % nx]
                            let right = u.[j].[(k+1) % nx]
                            rhs.[k] <- Ba.[j].[k] * left + Bb.[j].[k] * u.[j].[k] + Bc.[j].[k] * right + 2.0 * fv.[j].[k]
                        u.[j] <- inverter.[j] rhs
                    else
                        u.[j] <- Array.map2 (+) u.[j] fv.[j]

            yield {t=timerec; u=uplot u}

        ()
    }


let periodic_solve (t0:float) (u0:float[][]) (f:float -> float[] -> float[]) (settings:settings) : seq<point> =
    
    let numspecies = settings.numspecies
    let nx = settings.nx-1
    let dt = settings.dt
    let dx = (settings.xmax - settings.xmin) / (float)(settings.nx-1) // TODO: should be nx-1 according to Solver2D?
    let r = settings.D |> Array.map (fun rate -> rate * dt / (dx * dx))

    // Set up inverter functions (Sherman-Morrison)
    let inverter = Array.init numspecies (fun i -> 
        
        let ri = r.[i]
        let aj = -ri
        let bj = 2.0*(1.0+ri)
        let cj = -ri
        
        let a_i = Array.create nx aj
        let b_i = [| [| 2.0*bj |]; Array.create (nx-2) bj; [| bj + aj*cj/bj |] |] |> Array.concat
        let c_i = Array.create nx cj
        let beta = -bj        
        let v_i = [| [| beta |]; Array.zeroCreate (nx-2); [| cj |] |] |> Array.concat
        let w_i = [| [| 1.0 |]; Array.zeroCreate (nx-2); [| aj/beta |] |] |> Array.concat
        let z_i = Solver.TridiagonalAlgorithm a_i b_i c_i v_i
     
        fun rhs -> Solver.ApplySMInvertMatrix a_i b_i c_i rhs w_i z_i
    )

    // Do the right-hand side Matrix (using Sparse matrices)
    let Ba = Array.init numspecies (fun i -> Array.create nx r.[i])
    let Bb = Array.init numspecies (fun i -> Array.create nx (2.0*(1.0-r.[i])))
    let Bc = Array.init numspecies (fun i -> Array.create nx r.[i])

    let nplots = settings.plotlocs.Length;
    let uplot (u:float[][]) = 
        Array.init settings.nx (fun ix -> 
            Array.init nplots (fun j -> 
                if ix < nx 
                then u.[settings.plotlocs.[j]].[ix] 
                else u.[settings.plotlocs.[j]].[0]
            )
        )

    // General 1d solver
    solver t0 u0 f nx numspecies r inverter Ba Bb Bc uplot settings



let neumann_solve (t0:float) (u0:float[][]) (f:float -> float[] -> float[]) (settings:settings) order : seq<point> =
    let numspecies = settings.numspecies
    let nx = settings.nx
    let dt = settings.dt
    let dx = (settings.xmax - settings.xmin) / (float)(settings.nx-1) // TODO: should be nx-1 according to Solver2D?
    let r = settings.D |> Array.map (fun rate -> rate * dt / (dx * dx))
    
    // Specify rhs matrices
    let Ba = Array.init numspecies (fun i -> 
        [| [| 0.0 |]; (Array.create (nx-1) r.[i]) |] |> Array.concat)
    let Bb = Array.init numspecies (fun i -> 
        [| [| 2.0-r.[i] |]; Array.create (nx-2) (2.0-2.0*r.[i]); [| 2.0-r.[i] |] |] |> Array.concat)
    let Bc = Array.init numspecies (fun i -> 
        [| Array.create (nx-1) r.[i]; [| 0.0 |] |] |> Array.concat)
    
    // Set up inverter functions (Thomas' algorithm)
    let inverter = Array.init numspecies (fun i -> 
        let ri = r.[i]
        let aj = -ri
        let bj = 2.0+2.0*ri
        let cj = -ri
        
        let a_i, b_i, c_i = 
            match order with
            | 1 -> 
                ( Array.create nx aj
                , [| [| 2.0+ri |]; Array.create (nx-2) bj; [| 2.0+ri |] |] |> Array.concat
                , Array.create nx cj
                )
            | 2 ->
                ( [| [| -ri*2.0/3.0 |]; Array.create (nx-1) aj |] |> Array.concat
                , [| [| 2.0+ri*2.0/3.0 |]; Array.create (nx-2) bj; [| 2.0+ri*2.0/3.0 |] |] |> Array.concat
                , [| Array.create (nx-1) cj; [| -ri*2.0/3.0 |] |] |> Array.concat
                )
            | _ -> failwithf "No order %d approximation defined for zero-flux 1d solver" order
        Solver.TridiagonalAlgorithm a_i b_i c_i
    )

    let nplots = settings.plotlocs.Length;
    let uplot (u:float[][]) = Array.init nx (fun ix -> Array.init nplots (fun j -> u.[settings.plotlocs.[j]].[ix]))

    // General 1d solver
    solver t0 u0 f nx numspecies r inverter Ba Bb Bc uplot settings
    

let solve (t0:float) (u0:float[]) (f:float -> float[] -> float[]) (settings:settings) : point[] =
  failwith "Not implemented yet"

