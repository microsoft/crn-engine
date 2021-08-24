// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Program

open Microsoft.Research.ReactionDiffusion

let write1d fname (xs,species,res:Solver1d.point[]) = 
  let header   = 
    Array.collect (fun sp -> Array.map (fun x -> sprintf "%s (x=%f)" sp x) xs) species
    |> Array.append [|"Time"|]
    |> String.concat ","
  let contents = 
    res 
    |> Array.map (fun p -> 
      p.t :: (p.u |> Array.concat |> List.ofArray)
      |> List.map (sprintf "%1.6g") 
      |> String.concat ","
    )
  System.IO.File.WriteAllLines (fname, Array.append [|header|] contents)

let write2d fname (xs,ys,species,res:Solver2d.point []) = 
  let header = 
    Array.collect (fun sp -> Array.collect (fun y -> Array.map (fun x -> sprintf "%s (x=%f, y=%f)" sp x y) xs) ys) species 
    |> Array.append [|"Time"|]
    |> String.concat ","
  let contents = 
    res 
    |> Array.map (fun p -> 
      p.t :: (p.u |> Array.concat |> Array.concat |> List.ofArray)
      |> List.map (sprintf "%1.6g") 
      |> String.concat ","
    )
  System.IO.File.WriteAllLines (fname, Array.append [|header|] contents)



[<EntryPoint>]
let main(args) = 
  
  let am_settings : Lib.settings =
      { numspecies = 3
        thin = 1
        nx = 51
        xmin = 0.0
        xmax = 0.05
        ymin = 0.0
        ymax = 0.05
        tmax = 100000.0
        dt = 1000.0
        D = [| 1e-10; 1e-10; 1e-10 |]
        plotlocs = [| 0 |]
      }

  //ExampleSystems.neumann1d_autocatalytic () |> write1d "autocatalytic1d.csv"
  ExampleSystems.neumann2d_autocatalytic () |> write2d "autocatalytic2d.csv"
  //ExampleSystems.am1d_periodic am_settings |> write1d "am1d_periodic.csv"
  //ExampleSystems.am2d_periodic am_settings |> write2d "am2d_periodic.csv"

  printfn "Done";
  0