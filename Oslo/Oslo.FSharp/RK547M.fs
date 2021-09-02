// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Oslo.RK547M
open System
open Microsoft.FSharp.Core

(*[<JavaScript>]
module Proxies =
    [<Proxy(typeof<System.ArgumentException>)>]
    type ArgumentExceptionProxy [<JavaScript>](message: string) =
        inherit exn(message)

        new () = ArgumentExceptionProxy("ArgumentException")

        *)

type corrStruct =
    {
        x : Vector
        dt : double
        prevDt : double
        e : double
        k : Matrix
    }

/// <summary>Implementation of Runge-Kutta algoritm with per-point accurancy control from
/// Dormand and Prince article.
/// (J.R.Dormand, P.J.Prince, A family of embedded Runge-Kuttae formulae)</summary>
/// <example>Let our problem will be: 
/// dx/dt=y+1,
/// dy/dt=-x+2.
/// x(0)=0, y(0)=1.
/// To solve it, we just have to write
/// <code>
/// var sol=Ode.RK547M(0,new Vector(0,1),(t,x)=>new Vector(y+1,-x+2));
/// </code>
/// and then enumerate solution point from <see cref="System.IEnumerable"/> 'sol'.
/// </example>
/// <returns>Endless sequence of solution points</returns>
// Options name should match if possible: http://www.mathworks.com/help/techdoc/ref/odeset.html
type options = {
    InitialStepSize : double // Initial step for solution. Default value 0 means that initial step is computed automatically                
    AbsoluteTolerance : double // Absolute error tolerance used in automatic step size calculations.                
    RelativeTolerance : double // Relative error tolerance used in automatic step size calculations.                
    MaxStep : double // Sets maximal step value.                
    MinStep : double // Sets minimal step value.                
}

type state = {
    t: double;
    x: Vector;
    f: double->Vector->Vector;
    dt: double;
    k: Matrix;
    prevErr: double;
    prevT: double;
    prevX: Vector;
    opts: options;
}
        
let defaults () = {
    InitialStepSize = 0.0; 
    AbsoluteTolerance = 1e-6; 
    RelativeTolerance = 1e-3; 
    MaxStep = Double.MaxValue
    MinStep = 1e-12; 
}

let init t0 x0 f opts = 
    let ks = 7
    let n = Vector.length x0
    let state = { t = t0; x = x0; f = f; dt = 0.0; opts = opts; prevErr = 1.0
                  k = Matrix.zeros ks n; prevT = 0.0; prevX = x0 }

    // Compute initial step (see E. Hairer book)
    let dt0 = 
        let t0 : double = t0
        let x0 : Vector = x0
        if opts.InitialStepSize = 0.0 
        then
            let sc = x0 |> Vector.map (fun a -> (opts.AbsoluteTolerance + opts.RelativeTolerance * Math.Abs(a)))
            let f0 = state.f t0  x0
            let d0 = (Vector.map2 (fun a b -> Math.Abs((float a / float b))) x0 sc) |> Vector.max
            let d1 = (Vector.map2 (fun a b -> Math.Abs((float a / float b))) f0 sc) |> Vector.max
            let h0 = if Math.Min(d0, d1) < 1e-5  then (1e-6)  else (1e-2 * (d0 / d1))
            let f1 = state.f (t0 + h0) (x0 +^ (h0 *< f0))
            let f2 = Vector.subtract f0 f1
            let sc2 = Vector.scale h0 sc
            let d2 : double = (Vector.map2 (fun a b -> Math.Abs((float a / float b))) f2 sc2) |> Vector.max
            let dt = if Math.Max(d1, d2) <= 1e-15 then Math.Max(1e-6, h0 * 1e-3) else Math.Pow(1e-2 / Math.Max(d1, d2), 1.0 / 5.0)
            let dt1 = if dt > 100.0 * h0 then 100.0 * h0 else dt
            dt1
        else
            opts.InitialStepSize;      
    { t=t0; x=x0; f = f; dt = dt0; opts = opts; prevErr = 1.0; k = Matrix.zeros ks n; prevT = 0.0; prevX = x0; }
                   

// <summary>Interpolation for Runge-Kutta 5(4)7M method</summary>
// <param name="s">Relative locations in the interval (t,t+dt)<\param>
// <param name="y">Solution at start time<\param>
// <param name="k">Runge-Kutta points<\param>
let rec interp (s : Vector) (y : Vector) (k : Matrix) =
    if (box k = null) then failwith "interpolation array k is empty."
    let n = Vector.length y
    let nk = Matrix.numRows k
    let ns = Vector.length s

    let ys = Matrix.zeros ns n
    for i = 0 to ns - 1 do
        Vector.copyTo y ys.[i]

    // Create cumulative product of intervals
    let sp = Matrix.zeros 4 ns
    for i = 0 to ns - 1 do
        sp.[0].[i] <- s.[i]
        for j = 1 to 3 do
            sp.[j].[ i] <- s.[i] * sp.[j - 1].[i]

    // Define matrix of interpolation coefficients
    let BI = [|[| 1.0; -183.0 / 64.0; 37.0 / 12.0; -145.0 / 128.0 |];
              [| 0.0; 0.0; 0.0; 0.0 |];
              [| 0.0; 1500.0 / 371.0; -1000.0 / 159.0; 1000.0 / 371.0 |];
              [| 0.0; -125.0 / 32.0; 125.0 / 12.0; -375.0 / 64.0 |];
              [| 0.0; 9477.0 / 3392.0; -729.0 / 106.0; 25515.0 / 6784.0 |];
              [| 0.0; -11.0 / 7.0; 11.0 / 3.0; -55.0 / 28.0 |];
              [| 0.0; 3.0 / 2.0; -4.0; 5.0 / 2.0 |] |]
              |> Matrix.ofArray

    // Construct intermediate matrix
    let mutable kBI = Matrix.zeros n 4
    let mutable kBIs = Matrix.zeros n ns
    for i1 = 0 to ns - 1 do
        for i2 = 0 to n - 1 do
            kBIs.[i2].[i1] <- 0.0
            for j = 0 to 3 do
                kBI.[i2].[j] <- 0.0
                for l = 0 to nk - 1 do
                    kBI.[i2].[j] <- kBI.[i2].[j] + k.[l].[i2] * BI.[l].[j]
                kBIs.[i2].[i1] <- kBIs.[i2].[i1] + kBI.[i2].[j] * sp.[j].[i1]
            ys.[i1].[i2] <- ys.[i1].[i2] +  kBIs.[i2].[i1]
    ys

// Output interpolated points (set by Refine property) between rk_state_prev.t and rk_state_curr.t         
let add_interpolate rk_state_prev rk_state_curr = 
    let Refine : int = 4;
    let S = Vector.zeros (Refine - 1)
    for i = 0 to Refine - 2 do
        S.[i] <- ((float i + float 1) / (float Refine))
    let ts = Array.create Refine 0.0
    for i = 0 to Refine - 2 do
        Array.set ts i  (rk_state_prev.t + (rk_state_curr.t - rk_state_prev.t) * S.[i])

    let ys = interp S rk_state_prev.x rk_state_curr.k
    let rec create_interp_list (count : int) =
            if count >= Refine  - 1
                    then []
                    else 
                          let rk = {t = ts.[count]; x = ys.[count]; dt = rk_state_prev.dt
                                    prevErr = rk_state_prev.prevErr
                                    opts = rk_state_prev.opts; f = rk_state_prev.f; k = rk_state_prev.k
                                    prevX = if count = 0 then rk_state_prev.x else ys.[count - 1]
                                    prevT = if count = 0 then rk_state_prev.t else ts.[count - 1]}
                          rk :: create_interp_list (count + 1)
                                                
                        
    (create_interp_list 0)

let private addTo k v2 v1 =
    Vector.setAddedScaled v1 k v2
    v1

let private scaleTo k v1 =
    Vector.setScaled k v1
    v1

let advance rk_state =
    // Safety factors for automatic step control
    // See Solving Ordinary Differential Equations I Non stiff problems by E. Hairer, S.P. Norsett, G.Wanner
    // 2nd edition, Springer.

    // Safety factor is recommended on page 168
    let SafetyFactor : double = 0.8
    let MaxFactor : double = 5.0 // Maximum possible step increase
    let MinFactor : double = 0.2 // Maximum possible step decrease
                    
    let t = rk_state.t      
    let x = rk_state.x // Lower order approximation
    
    // Create formulae parameters(a's,b's and c's)
    // In original article, a is a step array.
    let a = [| [|1.0 / 5.0|];
                [|3.0 / 40.0; 9.0 / 40.0|];
                [|44.0 / 45.0; -56.0 / 15.0; 32.0 / 9.0|];
                [|19372.0 / 6561.0; -25360.0 / 2187.0; 64448.0 / 6561.0; -212.0 / 729.0|];
                [|9017.0 / 3168.0; -355.0 / 33.0; 46732.0 / 5247.0; 49.0 / 176.0; -5103.0 / 18656.0|];
                [|35.0 / 384.0; 0.0; 500.0 / 1113.0; 125.0 / 192.0; -2187.0 / 6784.0; 11.0 / 84.0|] |]
           
    // Coeffs for higher order
    let b1 = [|5179.0 / 57600.0; 0.0; 7571.0 / 16695.0; 393.0 / 640.0; -92097.0 / 339200.0; 187.0 / 2100.0; 1.0 / 40.0|]

    // Coeffs for lower order
    let b = [|35.0 / 384.0; 0.0; 500.0 / 1113.0; 125.0 / 192.0; -2187.0 / 6784.0; 11.0 / 84.0; 0.0|];

    let c = [|0.0; 1.0 / 5.0; 3.0 / 10.0; 4.0 / 5.0; 8.0 / 9.0; 1.0; 1.0|]
                    
    let prevX = rk_state.x
    let prevErr = rk_state.prevErr

    let rec correction (currIter : corrStruct) =
            // Initial step value. During computational process, we modify it.
            let dt = currIter.dt
            let prevDt = currIter.dt
            let k0 =
                Vector.copy x
                |> rk_state.f (t + dt * c.[0])
                |> scaleTo dt
            let k1 =
                let a0 = a.[0]
                Vector.copy x
                |> addTo k0 a0.[0]
                |> rk_state.f (t + dt * c.[1])
                |> scaleTo dt
            let k2 =
                let a1 = a.[1]
                Vector.copy x
                |> addTo k0 a1.[0]
                |> addTo k1 a1.[1]
                |> rk_state.f (t + dt * c.[2])
                |> scaleTo dt
            let k3 =
                let a2 = a.[2]
                Vector.copy x
                |> addTo k0 a2.[0]
                |> addTo k1 a2.[1]
                |> addTo k2 a2.[2]
                |> rk_state.f (t + dt * c.[3])
                |> scaleTo dt
            let k4 =
                let a3 = a.[3]
                Vector.copy x
                |> addTo k0 a3.[0]
                |> addTo k1 a3.[1]
                |> addTo k2 a3.[2]
                |> addTo k3 a3.[3]
                |> rk_state.f (t + dt * c.[4])
                |> scaleTo dt
            let k5 =
                let a4 = a.[4]
                Vector.copy x
                |> addTo k0 a4.[0]
                |> addTo k1 a4.[1]
                |> addTo k2 a4.[2]
                |> addTo k3 a4.[3]
                |> addTo k4 a4.[4]
                |> rk_state.f (t + dt * c.[5])
                |> scaleTo dt
            let k6 =
                let a5 = a.[5]
                Vector.copy x
                |> addTo k0 a5.[0]
                |> addTo k1 a5.[1]
                |> addTo k2 a5.[2]
                |> addTo k3 a5.[3]
                |> addTo k4 a5.[4]
                |> addTo k5 a5.[5]
                |> rk_state.f (t + dt * c.[6])
                |> scaleTo dt
            let k = Matrix.ofRows [| k0; k1; k2; k3; k4; k5; k6 |]
            let xLow =
                Vector.copy prevX
                |> addTo k0 b.[0]
                |> addTo k1 b.[1]
                |> addTo k2 b.[2]
                |> addTo k3 b.[3]
                |> addTo k4 b.[4]
                |> addTo k5 b.[5]
                |> addTo k6 b.[6]
            let xHigh =
                Vector.copy prevX
                |> addTo k0 b1.[0]
                |> addTo k1 b1.[1]
                |> addTo k2 b1.[2]
                |> addTo k3 b1.[3]
                |> addTo k4 b1.[4]
                |> addTo k5 b1.[5]
                |> addTo k6 b1.[6]
            // Compute error (see p. 168 of book indicated above)
            // error compulation in L-infinity norm is commented
                           
            let e = 
                Vector.mapi2 (fun i y1 y2 -> 
                    Math.Abs (1.0 * (y1 - y2)) / Math.Max(rk_state.opts.AbsoluteTolerance, rk_state.opts.RelativeTolerance * Math.Max ( Math.Abs(prevX.[i]), Math.Abs(float y2) ))
                ) xLow xHigh
                |> Vector.max
                            
            // PI-filter. Beta = 0.08                       
            let dtNew = 
                if e = 0.0 
                then dt 
                else dt * Math.Min(MaxFactor, Math.Max(MinFactor, SafetyFactor * Math.Pow(1.0 / e, 1.0 / 5.0) * Math.Pow(prevErr, 0.08) ) )
            
            let dtFin = if rk_state.opts.MaxStep < (Double.MaxValue) then Math.Min(dtNew , rk_state.opts.MaxStep) else dtNew
                            
            if Double.IsNaN(dtFin) 
            then raise (System.ArgumentException("Derivatives function returned NaN"))
            else
                if (dtFin < 1e-12) 
                then raise (System.ArgumentException("Cannot generate numerical solution"))
                else
                    let newIter = {x = xHigh; prevDt = prevDt; dt = dtFin; e = e; k = k}
                    if e <= 1.0 then newIter else newIter |> correction


    let nextState = correction {x = rk_state.x; e = 0.0; dt = rk_state.dt; prevDt = rk_state.dt; k = rk_state.k}                   
    {
        t = rk_state.t + nextState.prevDt
        x = nextState.x
        f = rk_state.f
        dt = nextState.dt
        prevErr = nextState.e
        opts = rk_state.opts
        k = nextState.k
        prevT = rk_state.t
        prevX = rk_state.x
    }

/// <summary>Solve using Runge-Kutta 5(4) method</summary>
/// <param name="t0">Left end of current time span</param>
/// <param name="t1">Right end of current time span</param>
/// <param name="x0">Initial phase vector value</param>
/// <param name="f">System right parts vector function</param>
/// <param name="opts">Options used by solver</param>
/// <returns>List of solution points x(t)</returns>
let solve t0 t1 x0 f opts = 
    let mutable s = init t0 x0 f opts
    let res = new System.Collections.Generic.List<_>()
    while s.t < t1 do
        s <- advance s
        res.Add (s.t, s.x) 
    res |> Seq.toList
