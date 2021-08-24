// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Solver1Dtest

open Xunit
open FsUnit

open Microsoft.Research.ReactionDiffusion
open Microsoft.Research.ReactionDiffusion.Lib

[<Fact(DisplayName="Periodic 1d - Smoke test")>]
let periodic1d_smoke () =
    
    let t0 = 0.0
    let u0 = [|[|0.0|]
               [|0.2|]
               [|0.4|]
               [|0.3|]
               [|0.1|]|]

    let f (t:float) (x:float[]) =
        x

    let settings =
        { numspecies = 1
          thin = 5
          nx = 5
          xmin = 0.0
          xmax = 5.0
          ymin = 0.0
          ymax = 5.0
          tmax = 1.0
          dt = 0.1
          D = [| 0.1 |]
          plotlocs = [| 0 |]
      }

    let res =
        Solver1d.periodic_solve t0 u0 f settings
        |> Array.ofSeq
    
    ()

[<Fact(DisplayName="Neumann 1d - Smoke test")>]
let neumann1d_smoke () =
    
    let t0 = 0.0
    let u0 = [|[|0.0|]
               [|0.2|]
               [|0.4|]
               [|0.3|]
               [|0.1|]|]

    let f (t:float) (x:float[]) =
        x

    let settings =
        { numspecies = 1
          thin = 5
          nx = 5
          xmin = 0.0
          xmax = 5.0
          ymin = 0.0
          ymax = 5.0
          tmax = 1.0
          dt = 0.1
          D = [| 0.1 |]
          plotlocs = [| 0 |]
      }

    let res =
        Solver1d.neumann_solve t0 u0 f settings 1
        |> Array.ofSeq
    
    ()

[<Fact(DisplayName="Neumann 1d (2nd order) - Smoke test")>]
let neumann1d_2nd_smoke () =
    
    let t0 = 0.0
    let u0 = [|[|0.0|]
               [|0.2|]
               [|0.4|]
               [|0.3|]
               [|0.1|]|]

    let f (t:float) (x:float[]) =
        x

    let settings =
        { numspecies = 1
          thin = 5
          nx = 5
          xmin = 0.0
          xmax = 5.0
          ymin = 0.0
          ymax = 5.0
          tmax = 1.0
          dt = 0.1
          D = [| 0.1 |]
          plotlocs = [| 0 |]
      }

    let res = Solver1d.neumann_solve t0 u0 f settings 2
    res |> Array.ofSeq
    
    ()

[<Fact(DisplayName="Heat equation - Periodic 1d")>]
let periodic1d_sinusoid () = 
    
    let L = System.Math.PI
    let nx = 201
    let dx = 2.0 * L / (float (nx-1))
    
    let settings : Lib.settings = 
        { numspecies = 1
          thin = 1
          nx = nx
          xmin = -L
          xmax = L
          ymin = -L     // Not used
          ymax = L      // Not used
          tmax = 20.0
          dt = 0.01
          D = [| 1.0 |]
          plotlocs = [| 0 |]
        }
    
    let f (t:float) (x:float[]) = 
        Array.zeroCreate x.Length
    
    let u0 = Array.init nx (fun i -> [|1000.0 * (sin(settings.xmin + float i * dx))|])
    
    let sol = Solver1d.periodic_solve 0.0 u0 f settings
   
    sol |> Seq.takeWhile (fun sp -> sp.t < settings.tmax)
        |> Seq.iter (fun sp ->
        
        let t = sp.t
        let realsolution = u0 |> Array.map (fun ui -> ui.[0] * exp (-t)) 
        let simulated = sp.u |> Array.map (Array.item 0)
        let es = Array.map2 (fun sim real -> abs (sim - real)) simulated realsolution
        let emax = Array.max es
        
        Assert.InRange(emax, 0.0, 10.0*sp.t+1e-10))


[<Fact(DisplayName="Heat equation - Neumann 1d")>]
let neumann1d_sinusoid () = 
    
    //let L = 2.0 * System.Math.PI
    let L = 1.0
    let nx = 201
    let dx = L / (float (nx-1))
    
    let settings = 
        { numspecies = 1
          thin = 1
          nx = nx
          xmin = 0.0
          xmax = L
          ymin = 0.0     // Not used
          ymax = L      // Not used
          tmax = 20.0
          dt = 0.01
          D = [| 1.0 |]
          plotlocs = [| 0 |]
        }
    
    let f (t:float) (x:float[]) = 
        Array.zeroCreate x.Length
    
    let k = 2.0 * System.Math.PI / L
    let u0 = Array.init nx (fun i -> [| cos(k * float i * dx)|])
    
    let sol = Solver1d.neumann_solve 0.0 u0 f settings 1
   
    sol |> Seq.takeWhile (fun sp -> sp.t < settings.tmax)
        |> Seq.iter (fun sp ->
        
            let t = sp.t
            let realsolution = u0 |> Array.map (fun ui -> ui.[0] * exp (-k*k*t)) 
            let simulated = sp.u |> Array.map (Array.item 0)
            let es = Array.map2 (fun sim real -> abs (sim - real)) simulated realsolution
            let emax = Array.max es

            Assert.InRange(emax, 0.0, 0.01))
        