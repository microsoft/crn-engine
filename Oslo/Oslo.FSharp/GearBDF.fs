// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
//Implementation of Gear's BDF method with dynamically changed step size and order.
module Oslo.GearBDF

open System
open Microsoft.FSharp.Core
open SparseMatrix
open Gauss
open System.Collections

type jacobian = Absent | Dense of Matrix | Sparse of SparseMatrix
//Order changes between 1 and 4.
type options =
    {
        /// Initial step for solution. Default value 0 means that initial step is computed automatically.
        InitialStepSize : double
        /// Absolute error tolerance used in automatic step size calculations.
        AbsoluteTolerance : double
        /// Relative error tolerance used in automatic step size calculations.
        RelativeTolerance : double
        /// Sets maximal step value.
        MaxStep : double
        /// Sets minimal step value.
        MinStep : double
        /// Gets or sets maximal scale factor value.
        MaxScale : double
        /// Gets or sets maximal scale factor value.
        MinScale : double
        /// Gets or sets NumberOfIterations value.
        NumberOfIterations : int
        // Jacobian
        Jacobian : jacobian
        /// Gets or sets pre-defined Jacobian matrix
        //Jacobian : Matrix 
        /// Gets or sets pre-defined Sparse Jacobian matrix
        //SparseJacobian : SparseMatrix
    }

type state =
    {
          /// current time
          t : double
          /// current state vector
          x : Vector
          /// right parts
          f : double -> Vector -> Vector
          /// current time step
          dt : double
          /// current Nordsieck history matrix
          z : Matrix
          /// current method order
          q : int
          /// current step size computation variable
          Dq : double
          /// current step size computation variable
          delta : double
          /// current step size scale factor
          r : double
          /// maximum order, usually 4 or 5
          qmax : int
          /// number of coincide successful steps
          nsuccess : int
          /// current error vector
          e : Vector
          /// iterations failed flag
          isIterationFailed : bool
          /// options
          opts : options
    }

let inline defaults() = {
    InitialStepSize = 0.0
    AbsoluteTolerance = 1e-6 
    RelativeTolerance = 1e-3 
    MaxStep = Double.MaxValue
    MinStep = 0.0 
    MaxScale = 1.1 
    MinScale = 0.9 
    NumberOfIterations = 5
    Jacobian = Absent
}

/// The following function compute Nordsieck's matrix Zn0 = Z(n-1)*A 
/// in more effective way than just multiply two matrixes.
///  Current algoritm is taken from book of 
/// Krishnan Radhakrishnan and Alan C.Hindmarsh "Description and Use of LSODE,
/// the Livermore Solver of Ordinary Differential Equations"
/// </summary>
/// <param name="arg">previous value of Nordsieck's matrix, so-called Z(n-1)</param>
/// <returns>So-called Zn0,initial vaue of Z in new step</returns>
let NordsieckMatrix_init (arg : Matrix) : Matrix =
    let q = Matrix.numCols arg
    let n = Matrix.numRows arg
    let res = Matrix.copy arg
    for k = 0 to q - 2 do
        for j = k + 1 to q - 1 do
            let k1 = k
            let j1 = j
            let j2 = q + k1 - j1
            for i = 0 to n - 1 do
                let temp = res.[i, j2]  + res.[i, j2 - 1]
                res.[i, j2 - 1] <- temp
    res

/// The following function rescales Nordsieck's matrix in more effective way than
/// only compute two matrixes. Current algorithm is taken from book of 
/// Krishnan Radhakrishnan and Alan C.Hindmarsh "Description and Use of LSODE,
/// the Livermore Solver of Ordinary Differential Equations"
/// </summary>
/// <param name="arg">Previous value of history matrix</param>
/// <param name="r">(New time step)/(Old time step)</param>
/// <returns>Rescaled history matrix</returns>
let NordsieckMatrix_rescale (arg: Matrix) (r: double) : Matrix =
    let mutable R = 1.0
    let q = Matrix.numCols arg
    let n = Matrix.numRows arg
    let mutable res = Matrix.copy arg
    for j = 1 to q - 1 do
        R <- R * r
        for i = 0 to n - 1 do
            res.[i, j] <- res.[i, j] * R
    res

//Compute Jacobian of f in given point (t,x) v.r.t x variations
let NordsieckMatrix_Jacobian (t: double) (x: Vector) (f: double -> Vector -> Vector) : Matrix =
    let n = Vector.length x
    let J = Matrix.zeros n n
    let vars = Array.create n 0.0
    let xvars = Vector.zeros n
    for i = 0 to n - 1 do
        vars.[i] <- sqrt (1e-6 * max (1e-5) (abs x.[i]))
        xvars.[i] <- x.[i]
    let fx = f t x
    let fvars = Matrix.zeros n n
    for i = 0 to n - 1 do
        xvars.[i] <- xvars.[i] + vars.[i]
        let fv = f t xvars
        for j = 0 to n - 1 do
            fvars.[i, j] <- fv.[j]
        xvars.[i] <- xvars.[i] - vars.[i]
    for i = 0 to n - 1 do
        let J_i = J.[i]
        for j = 0 to n - 1 do
            J_i.[j] <- (fvars.[j, i] - fx.[i]) / vars.[j]
    J

let init (t0: double) x0 f (opts : options) = 
    let n = Vector.length x0
    let dx0 = f t0 x0

    // Compute initial step (see LSODE handbook)
    let dt0 = 
        if opts.InitialStepSize = 0.0 then
                let tol = opts.RelativeTolerance;
                let ywt = x0 |> Vector.map (fun (a : double) -> (1.0 / opts.RelativeTolerance) *(opts.AbsoluteTolerance + opts.RelativeTolerance * Math.Abs(a)))
                let sum = Vector.sum (Vector.map2 (fun a b -> a * a / b / b) dx0 ywt)
                Math.Min(Math.Sqrt( tol / (1.0 / (ywt.[0] * ywt.[0]) + 1.0 / (float n) * sum)), opts.MaxStep)
                else
                Math.Min(opts.InitialStepSize, opts.MaxStep)

    let qmax = 5
    let q = 2
    let z0 = Matrix.zeros n (qmax + 1)
    for i = 0 to n - 1 do
        z0.[i, 0] <- x0.[i]
        z0.[i, 1] <- dt0 * dx0.[i]
    {t=t0; x=x0; f = f; dt = dt0; opts = opts; e = Vector.zeros n; z = z0; r = 1.0; Dq = 0.0; delta = 0.0; qmax = qmax; q = q; nsuccess = 0; isIterationFailed = false}


type iteration =
    {
          e : Vector
          Dq : double
          DqUp : double
          DqDown : double
          delta : double
          x : Vector
          z : Matrix
          count : int
    }

let rec Factorial (arg: int) =
    if arg < 0 then -1
    elif arg = 0 then 1
    else arg * Factorial (arg - 1)

let ToleranceNorm (v1: Vector) (absTol: double) (relTol: double) (v2 : Vector) : double =
    let n1 = Vector.length v1
    let n2 = Vector.length v2
    let mutable m1 = 0.0
    let mutable m2 = 0.0
    for i = 0 to n1 - 1 do
        let tmp = Math.Abs v1.[i]
        m1 <- if m1 > tmp then m1 else tmp
    for i = 0 to n2 - 1 do
        let tmp = Math.Abs v2.[i]
        m2 <- if m2 > tmp then m2 else tmp
    m1 / (absTol + relTol * m2)

let Corrector (gear_state_predicted : state) = 
             
    let NumberOfIterations = gear_state_predicted.opts.NumberOfIterations
           
    // Vector l for Nordsieck algorithm (orders 1 to 5)
    let l = [|[|1.0; 1.0|];
              [|2.0 / 3.0; 1.0; 1.0 / 3.0|];
              [|6.0 / 11.0; 1.0; 6.0 / 11.0; 1.0 / 11.0|];
              [|24.0 / 50.0; 1.0; 35.0 / 50.0; 10.0 / 50.0; 1.0 / 50.0|];
              [|120.0 / 274.0; 1.0; 225.0 / 274.0; 85.0 / 274.0; 15.0 / 274.0; 1.0 / 274.0|];
              [|720.0 / 1764.0; 1.0; 1624.0 / 1764.0; 735.0 / 1764.0; 175.0 / 1764.0; 1.0 / 1764.0|]|]
            |> Array.map Vector.ofArray
    // Vector Beta for Nordsieck algorithm (orders 1 to 5)
    let b = [|1.0; 2.0 / 3.0; 6.0 / 11.0; 24.0 / 50.0; 120.0 / 274.0; 720.0 / 1764.0 |]
            |> Vector.ofArray

    let n = Vector.length gear_state_predicted.x
    let f = gear_state_predicted.f
    let x0 = gear_state_predicted.x
    let qcurr = gear_state_predicted.q
    let qmax = gear_state_predicted.qmax
    let dt = gear_state_predicted.dt
    let t = gear_state_predicted.t
    let z0 = Matrix.copy gear_state_predicted.z

    //Tolerance computation factors 
    let Cq = 1.0 / (float qcurr + (float 1));
    let tau = 1.0 / (Cq * (float (Factorial qcurr)) * l.[qcurr - 1].[qcurr])

    //Scaling factors for the step size changing
    //with new method order q' = q, q + 1, q - 1, respectively
    let mutable rSame = 0.0
    let mutable rUp = 0.0 
    let mutable rDown = 0.0

    (*let isJacobianSparse = false
    let JSparse = if gear_state_predicted.opts.SparseJacobian <> null
                                then
                                    gear_state_predicted.opts.SparseJacobian
                                else
                                    null

    let J =  if box gear_state_predicted.opts.Jacobian = null
                then 
                    NordsieckMatrix_Jacobian (t + dt) x0 f
                else
                    gear_state_predicted.opts.Jacobian

    let P = Oslo.Matrix.subtract (Oslo.Matrix.identity n n) (Oslo.Matrix.matrMultScal J (dt * b.[qcurr - 1]))
            
    let PSparse = if isJacobianSparse = false
                    then
                        null 
                    else
                        let M1 = JSparse.times (dt * b.[qcurr - 1])
                        let M2 = SparseMatrix.Identity(n, n)
                        M2.minus M1*)

    let computeDenseP J = Oslo.Matrix.subtract (Oslo.Matrix.identity n n) (Oslo.Matrix.matrMultScal J (dt * b.[qcurr - 1]))
    let P = match gear_state_predicted.opts.Jacobian with
            | Absent   -> NordsieckMatrix_Jacobian (t + dt) x0 f |> computeDenseP |> Dense
            | Dense J  -> computeDenseP J |> Dense
            | Sparse J -> SparseMatrix.Identity(n, n).minus (J.times(dt * b.[qcurr - 1])) |> Sparse

    let absTol = gear_state_predicted.opts.AbsoluteTolerance
    let relTol = gear_state_predicted.opts.RelativeTolerance

    let rec iterations (par : iteration) = 
            let xprev = par.x
            let gm = Vector.zeros n
            let dx = f (t + dt) par.x
            for i = 0 to n - 1 do
                gm.[i] <- dt * dx.[i] - z0.[i].[1] - par.e.[i]
            let resX = match P with 
                        | Absent -> failwith "Failed to evaluate predictor matrix P"
                        | Dense P -> SolveCore P gm
                        | Sparse P -> SparseSolveCore P gm

            let ecurr = Vector.zeros n
            for i = 0 to n - 1 do
                ecurr.[i] <- par.e.[i] + resX.[i]
            let xcurr = Vector.zeros n
            for i = 0 to n - 1 do
                xcurr.[i] <- x0.[i] + b.[qcurr - 1] * ecurr.[i]
            //Row dimension is smaller than zcurr has
            let M = ecurr $ l.[qcurr - 1]
            //So, "expand" the matrix
            let MBig = Matrix.zeros n (qmax + 1)
            for i = 0 to n - 1 do
                for j = 0 to qmax do
                    if i < Matrix.numRows M && j < Vector.length l.[qcurr - 1] then
                        MBig.[i, j] <- M.[i, j]
                    else
                        MBig.[i, j] <- 0.0

            let zcurr = Matrix.add z0 MBig
            let Dq =  ToleranceNorm ecurr absTol relTol xprev 
                            
            let deltaE = Vector.zeros n
            for i = 0 to n - 1 do
                deltaE.[i] <- ((ecurr.[i] - (gear_state_predicted.e.[i])) * ((1.0 / ((float qcurr) + 2.0)) * l.[qcurr - 1].[qcurr - 1]))
            let DqUp = ToleranceNorm deltaE absTol relTol xcurr
                            
            let zcurrColumn = Matrix.copyCol zcurr (qcurr - 1)
                            
            let DqDown = ToleranceNorm zcurrColumn absTol relTol xcurr
            let delta = Dq / (tau / (2.0 * (float qcurr + 2.0)))
                    
            let newState = {Dq = Dq; DqUp = DqUp; DqDown = DqDown; z = zcurr; x = xcurr; e = ecurr; count = par.count + 1; delta = delta} 
            if (delta > 1.0 && newState.count < NumberOfIterations) 
                                                            then 
                                                                (newState |> iterations) 
                                                            else 
                                                                newState


    let iterResult = iterations {delta = 0.0; count = 0; e = gear_state_predicted.e; Dq = 0.0; DqUp = 0.0; DqDown = 0.0; x = x0; z = z0}
    let mutable nsuccess = if iterResult.count < NumberOfIterations then (gear_state_predicted.nsuccess + 1) else 0
            
            
    let mutable deltaFin = iterResult.delta
    let mutable isIterationFailedFin = false
    let mutable nsuccessFin = 0
    let mutable zFin = iterResult.z
    let mutable eFin = iterResult.e
    let mutable xFin = iterResult.x
    let mutable DFin = iterResult.Dq
    let mutable qFin = gear_state_predicted.q
    let mutable rFin = 1.0
            
    let Dq = Math.Abs(iterResult.Dq)
    let DqUp =  Math.Abs(iterResult.DqUp)
    let DqDown = Math.Abs(iterResult.DqDown)


    if (iterResult.count < NumberOfIterations)
            then
                isIterationFailedFin <- false
                zFin <- iterResult.z
                for i = 0 to n - 1 do
                    xFin.[i] <- iterResult.z.[i].[0]
                eFin <- iterResult.e
            else
                isIterationFailedFin <- true
                zFin <- gear_state_predicted.z
                for i = 0 to n - 1 do
                    xFin.[i] <- gear_state_predicted.z.[i].[0]
                eFin <- gear_state_predicted.e

    //Compute step size scaling factors
    rUp <- 0.0
            
    if (gear_state_predicted.q < gear_state_predicted.qmax) then
        rUp <- 1.0 / 1.4 / (Math.Pow(DqUp, 1.0 / (float (qcurr + 2))) + 1e-6)
                        
            
    rSame <- 1.0 / 1.2 / (Math.Pow(Dq, 1.0 / (float (qcurr + 1))) + 1e-6)
            
    rDown <- 0.0
            
    if (gear_state_predicted.q > 1) then
            rDown <- 1.0 / 1.3 / (Math.Pow(DqDown, 1.0 / (float qcurr)) + 1e-6)
                        
            
    //======================================
    nsuccessFin <- if nsuccess >= gear_state_predicted.q then 0 else nsuccess;
    //Step size scale operations
            
    if (rSame >= rUp) 
        then
            if (rSame <= rDown && nsuccess >= gear_state_predicted.q && gear_state_predicted.q > 1) 
                then    
                    qFin <- gear_state_predicted.q - 1
                    DFin <- DqDown
            
                    for i = 0 to n - 1 do    
                        for j = qFin + 1 to qmax do
                                zFin.[i].[j] <- 0.0

                    nsuccess <- 0
                    rFin <- rDown
                            
                else
                    qFin <- gear_state_predicted.q
                    DFin <- Dq
                    rFin <- rSame
        else
            if (rUp >= rDown)
                then
                    if (rUp >= rSame && nsuccess >= gear_state_predicted.q && gear_state_predicted.q < gear_state_predicted.qmax)
                        then
                            qFin <- gear_state_predicted.q + 1
                            DFin <- DqUp
                            rFin <- rUp
                            nsuccess <- 0
                        else   
                            qFin <- gear_state_predicted.q
                            DFin <- Dq
                            rFin <- rSame
                else
                    if (nsuccess >= gear_state_predicted.q && gear_state_predicted.q > 1)
                        then
                            qFin <- gear_state_predicted.q - 1;
                            DFin <- DqDown;
            
                            for i = 0 to (n - 1) do
                                for j = qFin + 1 to qmax do
                                    zFin.[i].[j] <- 0.0
                                        
                            nsuccess <- 0
                            rFin <- rDown
                        else 
                            qFin <- gear_state_predicted.q
                            DFin <- Dq
                            rFin <- rSame
                                        
              
            
    { Dq = DFin;
      x = xFin; 
      z = zFin; 
      delta = deltaFin; 
      e = eFin;
      t = gear_state_predicted.t; 
      isIterationFailed = isIterationFailedFin; 
      opts = gear_state_predicted.opts;
      nsuccess = nsuccessFin; 
      r = rFin; 
      f = gear_state_predicted.f;
      dt = gear_state_predicted.dt;
      q = qFin;
      qmax = gear_state_predicted.qmax}

// Predictor - Corrector scheme
let rec PredictorCorrector (gear_state : state) =
                     
    //Predictor step
    let z0 = Matrix.copy gear_state.z
    let xn = Matrix.copyCol z0 0
    let n = Vector.length xn
    let init_z = NordsieckMatrix_init gear_state.z
            
    let init_state = 
        { Dq = gear_state.Dq;
          x = Matrix.copyCol init_z 0;
          z = init_z;
          delta = gear_state.delta;
          e = Vector.zeros n;
          t = gear_state.t;
          isIterationFailed = false; opts = gear_state.opts;
          nsuccess = gear_state.nsuccess; r = gear_state.r; f = gear_state.f; dt = gear_state.dt;
          q = gear_state.q; qmax = gear_state.qmax
        }
            
    let MinStep = gear_state.opts.MinStep
    let MaxScale = gear_state.opts.MaxScale
    let MinScale = gear_state.opts.MinScale

    //Corrector step
    let mod_state = Corrector init_state

    if mod_state.isIterationFailed 
    then
        let mod_z = z0
        let mod_nsuccess = 0
        let mod_dt = mod_state.dt / 2.0

        if mod_dt < 1e-12 
        then failwith "Cannot generate numerical solution"
        else 
            {Dq = mod_state.Dq; 
              x = mod_state.x; 
              z = NordsieckMatrix_rescale mod_z 0.5; 
              delta = mod_state.delta; 
              e = mod_state.e;
              t = mod_state.t; 
              isIterationFailed = false; opts = mod_state.opts;
              nsuccess = mod_nsuccess; r = mod_state.r; f = mod_state.f; 
              dt = mod_dt;
              q = mod_state.q; qmax = mod_state.qmax} |> PredictorCorrector
    else
        let mutable r = Math.Min(1.1, Math.Max(0.2, mod_state.r))
        if (mod_state.delta >= 1.0) 
        then
            if (mod_state.opts.MaxStep < Double.MaxValue) then
                    r <- (Math.Min(r, mod_state.opts.MaxStep / mod_state.dt))
                                    
            if (MinStep > 0.0) then
                    r <- (Math.Max(r, MinStep / mod_state.dt))
                                
                        
            r <- Math.Min(r, MaxScale)
            r <- Math.Max(r, MinScale)
                  
            { Dq = mod_state.Dq; 
              x = mod_state.x; 
              z = NordsieckMatrix_rescale mod_state.z r; 
              delta = mod_state.delta; 
              e = mod_state.e;
              t = mod_state.t; 
              isIterationFailed = false; opts = mod_state.opts;
              nsuccess = mod_state.nsuccess; r = mod_state.r; f = mod_state.f; 
              dt = mod_state.dt * r // Decrease step;
              q = mod_state.q; qmax = mod_state.qmax
            } |> PredictorCorrector
        else
            if (mod_state.opts.MaxStep < Double.MaxValue) then
                    r <- (Math.Min(r, mod_state.opts.MaxStep / mod_state.dt))
                                    
            if (MinStep > 0.0) then
                    r <- (Math.Max(r, MinStep / mod_state.dt))
                                
                        
            r <- Math.Min(r, MaxScale)
            r <- Math.Max(r, MinScale)
                       
            let fin_state = {Dq = mod_state.Dq; x = mod_state.x; 
                              z =  NordsieckMatrix_rescale mod_state.z r; 
                              delta = mod_state.delta; e = mod_state.e;
                              t = mod_state.t + mod_state.dt; isIterationFailed = mod_state.isIterationFailed; 
                              opts = mod_state.opts;
                              nsuccess = mod_state.nsuccess; r = mod_state.r;
                              f = mod_state.f; dt = mod_state.dt * r;
                              q = mod_state.q;qmax = mod_state.qmax}
            fin_state

let advance (gear_state : state) = 
    let n = Vector.length gear_state.x
    let state : state = { Dq = gear_state.Dq; x = gear_state.x; z = gear_state.z; 
                          delta = gear_state.delta; e = Vector.zeros n;
                          t = gear_state.t; isIterationFailed = false;
                          opts = gear_state.opts;
                          nsuccess = gear_state.nsuccess; r = gear_state.r; f = gear_state.f;
                          dt = gear_state.dt; q = gear_state.q; qmax = gear_state.qmax
                        }
    PredictorCorrector state

let solve t0 t1 x0 f opts = 
    let mutable s = init t0 x0 f opts
    let res = new System.Collections.Generic.List<_>()
    while s.t < t1 do
        s <- advance s
        res.Add (s.t, s.x) 
    res |> Seq.toList
