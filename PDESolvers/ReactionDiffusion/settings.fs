// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.ReactionDiffusion.Lib

type boundary_condition = Neumann of int | Periodic

type settings = 
  { numspecies : int
  ; thin : int
  ; nx : int
  ; xmin : float
  ; xmax : float
  ; ymin : float
  ; ymax : float
  ; tmax : float
  ; dt : float
  ; D : float[]
  ; plotlocs : int[]
  }

let default_settings = 
  { numspecies = 1
  ; thin = 10
  ; nx = 21
  ; xmin = 0.0
  ; xmax = 1.0
  ; ymin = 0.0
  ; ymax = 1.0
  ; tmax = 1.0
  ; dt = 0.1
  ; D = [|1.0|]
  ; plotlocs = [|0|] }

let distance_from_xy (x,y) i j nx = sqrt ((( (float i)/(float (nx-1)) - x)**2.0) + (( (float j)/(float (nx-1)) - y)**2.0))
   