// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.FSharp

open Oslo
open Oslo.RK547M

[<JavaScript>]
module Samples =

    let private mkVec1 x =
        let r = Vector.zeros 1
        r.[0] <- x
        r

    let private mkVec x y =
        let r = Vector.zeros 2
        r.[0] <- x
        r.[1] <- y
        r

    // Solves Brusselator dynamic system on time segment [t0,t1]
    //
    // dx1/dt = 1 + x1*x1*x2 - 4*x1
    // dx2/dt = 3*x1 - x1*x1*x2
    //
    // Returns tuple with three 1D arrays: t,x1,x2
    // Note: this sample uses recursion that is not translated well by WebSharper 2.4
    // Large time intervals may cause stack overflows
    let Brusselator t0 t1 = 

        let x0 = mkVec 1.01 3.0 // Initial point

        let f (t:double) (x: Vector) = // Brusselator's right part
            mkVec (1.0 + x.[0] * x.[0] * x.[1] - 4.0 * x.[0]) (3.0 * x.[0] - x.[0] * x.[0] * x.[1])
      
        let ts, xs = solve t0 t1 x0 f {defaults() with MaxStep = 0.01} |> List.unzip

        ( ts
        , xs |> List.map (fun v -> v.[0])
        , xs |> List.map (fun v -> v.[1])
        )

    let RM_init =
        let x0 = mkVec 0.2 0.4 // Initial point
        let RM (t:double) (x: Vector) : Vector =
            let alpha = 0.2
            let kappa = 0.14
            let sigma = 0.15
            mkVec (x.[0] * (1.0 - x.[0]) - x.[0] * x.[1] / (kappa + x.[0]))
                  (alpha * x.[0] * x.[1] / (kappa + x.[0]) - sigma * x.[1])
        init 0.0 x0 RM { defaults() with MaxStep = 4.0 }

    let RM_solve (rm : state) count =
        let t = Array.zeroCreate count
        let x1 = Array.zeroCreate count
        let x2 = Array.zeroCreate count
        let mutable r = rm
        for i = 0 to count - 1 do
            Array.set t i r.t
            Array.set x1 i r.x.[0]
            Array.set x2 i r.x.[1]
            r <- advance r
        (t,x1,x2,r)

    let RM_rk547_benchmark n =
        let x0 = mkVec 0.2 0.4 // Initial point
        let RM (t:double) (x: Vector) : Vector =
            let alpha = 0.2
            let kappa = 0.14
            let sigma = 0.15
            mkVec
                (x.[0] * (1.0 - x.[0]) - x.[0] * x.[1] / (kappa + x.[0]))
                (alpha * x.[0] * x.[1] / (kappa + x.[0]) - sigma * x.[1])
        let mutable solver = init 0.0 x0 RM { defaults() with MaxStep = 0.01}
        for i = 0 to n - 1 do
            solver <- advance solver

    let Trigonometric t0 t1 =
        let t0 = 0.0
        let x0 = mkVec1 1.0
        let f (t: double) (x: Vector) = mkVec1 (- sin t)
        let ts, xs = solve t0 t1 x0 f { defaults() with MaxStep = 0.1 } |> List.unzip

        (ts |> List.toArray, xs |> List.map (fun x -> x.[0]) |> List.toArray)
        
