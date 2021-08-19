module Solver2Dtest
open Xunit
open FsUnit

open Microsoft.Research.ReactionDiffusion
open Microsoft.Research.ReactionDiffusion.Lib

[<Fact(DisplayName="Periodic 2d - Smoke test")>]
let startPeriodic2d () =
    
    let t0 = 0.0
    let u0 = [|[|[|0.0|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.2|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.4|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.3|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.1|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]|]

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
        Solver2d.periodic_solve t0 u0 f settings
        |> Array.ofSeq
    
    ()

[<Fact(DisplayName="Neumann 2d - Smoke test")>]
let startNeumann2d () =
    
    let t0 = 0.0
    let u0 = [|[|[|0.0|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.2|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.4|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.3|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]
               [|[|0.1|];[|0.1|];[|0.3|];[|0.4|];[|0.5|]|]|]

    let f (t:float) (x:float[]) = x

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
        Solver2d.neumann_solve t0 u0 f settings
        |> Array.ofSeq
    
    ()

[<Fact(DisplayName="Heat equation - Periodic 2d with initial sinusoid", Skip="ND: I don't believe this test is correct anyway")>]
//[<Fact>]
let periodic2d_initial_sinusoid () = 
    
    let L = System.Math.PI
    let gridPoints = 20
    let dx = 2.0 * L / (float gridPoints)

    let settings = 
        { numspecies = 1
          thin = 1
          nx = gridPoints + 1
          xmin = -L
          xmax = L
          ymin = -L
          ymax = L
          tmax = 200.0
          dt = 0.01
          D = [| 1.0 |]
          plotlocs = [| 0 |]
        }
    
    let f (t:float) (x:float[]) = 
        Array.zeroCreate x.Length
    
    let u0 = Array.init gridPoints (fun i -> 
        let x = -L + (float i * dx)
        Array.init gridPoints (fun j -> 
            let y = -L + (float j * dx) 
            [|1000.0 * (sin x + cos y)|]))
    
    let sol = Solver2d.periodic_solve 0.0 u0 f settings
   
    sol |> Seq.takeWhile (fun sp ->sp.t < settings.tmax)
        |> Seq.iter (fun sp ->
        
            let t = sp.t
            let realsolution = u0 |> Array.map (fun ui -> ui |> Array.map (fun uij -> uij.[0] * exp (-t))) 
            let error = 
                realsolution 
                |> Array.mapi (fun i ri -> ri |> Array.mapi (fun j rij -> abs (sp.u.[0].[i].[j] - rij))) 
                |> Array.concat
                |> Array.max
                (*|> Array.collect (fun ar -> Array.sub ar 0 gridPoints)
                |> Array.map (fun ar -> Array.sub ar 0 gridPoints)
                |> Array.mapi (fun i alldata ->
                  alldata.[0..(gridPoints-1)] |> Array.mapi (fun j el ->
                        let vl = u0.[i].[j].[0] * exp (-t)
                        let erri = el - vl
                        erri * erri
                    )
                  )
                |> Array.concat
                |> Array.sum
                |> sqrt*)

            Assert.Equal(0.0, error, 3))


let compare_two_heat_equations bc settings1 settings2 = 

    let f (t:float) (x:float[]) = Array.zeroCreate x.Length
    
    let width = 0.21
    let u0 = 
      Array.init settings1.nx (fun i -> 
        Array.init settings1.nx (fun j -> 
          if (Lib.distance_from_xy (0.5,0.5) i j settings1.nx < (width / 2.0))
          then [|1.0|]
          else [|0.0|]
        )
      )
    
    let sol1 = 
        match bc with
        | Neumann _ -> 
            Solver2d.neumann_solve 0.0 u0 f settings1 |> Seq.takeWhile (fun sp -> sp.t < settings1.tmax)
        | Periodic -> 
            Solver2d.periodic_solve 0.0 u0 f settings1 |> Seq.takeWhile (fun sp -> sp.t < settings1.tmax)
   
    let sol2 = 
        match bc with
        | Neumann _ -> 
            Solver2d.neumann_solve 0.0 u0 f settings2 |> Seq.takeWhile (fun sp -> sp.t < settings2.tmax)
        | Periodic -> 
            Solver2d.periodic_solve 0.0 u0 f settings2 |> Seq.takeWhile (fun sp -> sp.t < settings2.tmax)
   
    (sol1,sol2) 
    ||> Seq.iter2 (fun sp1 sp2 ->        
        let error = 
          (sp1.u.[0], sp2.u.[0])
            ||> Array.map2 (fun u1i u2i -> (u1i,u2i) ||> Array.map2 (fun u1ij u2ij -> abs (u1ij - u2ij)))
            |> Array.concat
            |> Array.max
            |> sqrt
        printfn "t = %1.3f, max error = %1.3f" sp1.t error
        Assert.Equal(0.0, error, 3))
    
    //Assert.InRange(Seq.length sol1, 100, System.Int32.MaxValue)
    //Assert.InRange(Seq.length sol2, 100, System.Int32.MaxValue)
        
let rescale_time sf bc = 
    
    let gridPoints = 10
    let dx = 1.0
    let L = (float gridPoints)*dx

    let settings1 = 
        { numspecies = 1
          thin = 1
          nx = gridPoints+1
          xmin = 0.0
          xmax = L
          ymin = 0.0
          ymax = L
          tmax = 5.0
          dt = 0.02
          D = [| 1.0 |]
          plotlocs = [| 0 |]
        }
    let settings2 = { settings1 with tmax = settings1.tmax/sf; dt = settings1.dt/sf; D = [| sf |] }
    compare_two_heat_equations bc settings1 settings2

//[<Fact(DisplayName="Neumann 2d - Invariant to rescaling time", Skip="Currently failing due to erroneous Neumann solver implementation.")>]
[<Fact(DisplayName="Neumann 2d - Invariant to rescaling time")>]
let neumann2d_rescale_time () = rescale_time 0.1 (Neumann 1)

[<Fact(DisplayName="Periodic 2d - Invariant to rescaling time")>]
let periodic2d_rescale_time () = rescale_time 0.1 Periodic    


let rescale_space sf bc = 
    
    let gridPoints = 20
    let dx = 1.0
    let L = (float gridPoints)*dx

    let settings1 = 
        { numspecies = 1
          thin = 1
          nx = gridPoints+1
          xmin = 0.0
          xmax = L
          ymin = 0.0
          ymax = L
          tmax = 5.0
          dt = 0.02
          D = [| 1.0 |]
          plotlocs = [| 0 |]
        }
    let settings2 = { settings1 with xmax = L * sf; ymax = L * sf; D = [| sf*sf |] }
    compare_two_heat_equations bc settings1 settings2


//[<Fact(DisplayName="Neumann 2d - Invariant to rescaling space", Skip="Currently failing due to erroneous Neumann solver implementation.")>]
[<Fact(DisplayName="Neumann 2d - Invariant to rescaling space")>]
let neumann2d_rescale_space () = rescale_space 10.0 (Neumann 1)

[<Fact(DisplayName="Periodic 2d - Invariant to rescaling space")>]
let periodic2d_rescale_space () = rescale_space 10.0 Periodic


[<Fact(DisplayName="Heat equation - Neumann 2d with initial sinusoid", Skip="ND: I don't believe this test is correct anyway")>]
//[<Fact(DisplayName="Neumann 2d - Initial sinusoid")>]
let neumann2d_initial_sinusoid () = 
    
    let L = 2.0 * System.Math.PI
    let gridPoints = 20
    let dx = L / (float gridPoints)

    let settings = 
        { numspecies = 1
          thin = 1
          nx = gridPoints + 1
          xmin = 0.0
          xmax = L
          ymin = 0.0
          ymax = L
          tmax = 200.0
          dt = 0.01
          D = [| 1.0 |]
          plotlocs = [| 0 |]
        }
    
    let f (t:float) (x:float[]) = 
        Array.zeroCreate x.Length
    
    let u0 = Array.init settings.nx (fun i -> Array.init settings.nx (fun j -> [|1000.0 * (sin(float i * dx) + cos(float j * dx))|]))
    
    let sol = Solver2d.neumann_solve 0.0 u0 f settings
   
    sol |> Seq.takeWhile (fun sp ->sp.t < settings.tmax)
        |> Seq.iter (fun sp -> 
        
            let error = 
                sp.u
                |> Array.collect (fun ar -> Array.sub ar 0 gridPoints)
                |> Array.map (fun ar -> Array.sub ar 0 gridPoints)
                |> Array.mapi (fun i alldata ->
                  alldata.[0..(gridPoints-1)] |> Array.mapi (fun j el ->
                        let vl = u0.[i].[j].[0] * exp (-sp.t)
                        let erri = el - vl
                        erri * erri
                    )
                  )
                |> Array.concat
                |> Array.sum
                |> sqrt

            Assert.Equal(0.0, error, 3))


