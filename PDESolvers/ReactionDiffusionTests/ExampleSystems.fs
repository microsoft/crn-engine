// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module ExampleSystems

open Xunit
open FsUnit

open Microsoft.Research.ReactionDiffusion
open Microsoft.Research.ReactionDiffusion.Lib

[<Fact(DisplayName="Autocatalytic example - Neumann 1d")>]
let neumann1d_autocatalytic () = 
    
    let settings =
        { numspecies = 2
          thin = 100
          nx = 51
          xmin = 0.0
          xmax = 0.1
          ymin = 0.0
          ymax = 0.1
          tmax = 150000.0
          dt = 200.0
          D = [| 1e-10; 1e-10 |]
          plotlocs = [| 0 |]
        }

    let t0 = 0.0
    let u0 = Array.init settings.nx (fun i -> [| (if (i < 11 || i > 21) then 0.0 else 1.0); 5.0 |])
    let f _ (u:float[]) = 
        let prop = 5e-5*u.[0]*u.[1]
        [| prop; -prop |]

    let res =
        Solver1d.neumann_solve t0 u0 f settings 1
        |> Seq.takeWhile (fun sp -> sp.t < settings.tmax)
        |> Array.ofSeq
     
    // Make sure the right half of the domain is strictly decreasing
    let last = (res |> Array.last).u |> Array.map (Array.item 0)
    let dudx = Array.init 25 (fun i -> last.[25+i] - last.[24+i])
    Assert.InRange(Array.max(dudx), System.Double.MinValue, 0.0)

    // Return
    let dx = settings.xmax / (float settings.nx - 1.0)
    let xs = Array.init settings.nx (fun i -> (float i) * dx)
    xs, [|"X"|], res

let neumann2d_autocatalytic () = 
    
    let settings : Lib.settings =
        { numspecies = 2
          thin = 100
          nx = 101
          xmin = 0.0
          xmax = 0.1
          ymin = 0.0
          ymax = 0.1
          tmax = 200000.0
          dt = 200.0
          D = [| 1e-10; 1e-10 |]
          plotlocs = [| 0 |]
        }

    let t0 = 0.0
    let u0 = Array.init settings.nx (fun i -> Array.init settings.nx (fun j -> [| (if (i > 10 && i < 21 && j>10 && j<21) then 1.0 else 0.0); 5.0 |]))
    let f _ (u:float[]) = 
        let prop = 5e-5*u.[0]*u.[1]
        [| prop; -prop |]

    let res = 
        Solver2d.periodic_solve t0 u0 f settings
        |> Seq.takeWhile (fun sp -> sp.t < settings.tmax)
        |> Array.ofSeq

    // Return
    let dx = settings.xmax / (float settings.nx - 1.0)
    let xs = Array.init settings.nx (fun i -> (float i) * dx)
    xs, xs, [|"X"|], res

let am _ (u:float[]) = 
    let x = u.[0]
    let y = u.[1]
    let b = u.[2]
    let r = 1e-4
    [| r*x*(b - y)
     ; r*y*(b - x)
     ; r*(2.0*x*y - b*x - b*y)
     |]

let am1d_periodic settings =
    
    let plot_species = [|"X"|]
    let noise = 0.5
    let rand = System.Random ()
    let t0 = 0.0
    let u0 = 
        Array.init settings.nx (fun i -> 
            [| max (5.0*(1.0 + noise*(rand.NextDouble()-0.5))) 0.0
             ; max (5.0*(1.0 + noise*(rand.NextDouble()-0.5))) 0.0
             ; 0.0 
             |]
        )

    let res = 
        Solver1d.periodic_solve t0 u0 am settings
        |> Seq.takeWhile (fun sp -> sp.t < settings.tmax)
        |> Array.ofSeq
    let dx = settings.xmax / (float settings.nx - 1.0)
    let xs = Array.init settings.nx (fun i -> (float i) * dx)

    xs, plot_species, res
     

let am2d_periodic settings =
    let plot_species = [|"X"|]
    let noise = 0.5
    let rand = System.Random ()
    let t0 = 0.0
    let u0 = 
        Array.init settings.nx (fun i -> 
            Array.init settings.nx (fun j -> 
                [| max (5.0*(1.0 + noise*(rand.NextDouble()-0.5))) 0.0
                 ; max (5.0*(1.0 + noise*(rand.NextDouble()-0.5))) 0.0
                 ; 0.0 
                 |]
            )
        )


    let res = 
        Solver2d.periodic_solve t0 u0 am settings
        |> Seq.takeWhile (fun sp -> sp.t < settings.tmax)
        |> Array.ofSeq
    let dx = settings.xmax / (float settings.nx - 1.0)
    let xs = Array.init settings.nx (fun i -> (float i) * dx)

    xs, xs, plot_species, res
     