module Microsoft.Research.CRNEngine.Tests.SpatialTests

open Xunit
open FsUnit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

let degradation () = 
  "directive simulation { final=100.0; plots=[X] }
  directive simulator pde
  directive parameters [DX=0.1]
  directive spatial { nx = 10; dt = 1.0; diffusibles = [ X = DX ]; random = 0.2 }
  
  init X 1.0 |
  X ->{0.1}"
  |> Parser.from_string Crn.parse


[<Fact(DisplayName="Spatial - Degradation - 1d periodic")>]
let degradation_1d_periodic () = 
  let crn = degradation ()
  let table = crn.simulate_case()
  ()

let autocatalytic () = 
  "directive simulation { final=12000.0; points=200; plots=[X] }
directive simulator pde
directive spatial { dt=200.0; xmax=0.1; nx=41; default_diffusion = 1e-10; random = 0.1; boundary=ZeroFlux }
directive parameters [k = 5e-5]

init X { spatial = { centralcore = { inner = 1.0; width = 0.1; outer = 0.0 } } } |
init Y 5 |
X + Y ->{k} 2X"
  |> Parser.from_string Crn.parse

[<Fact(DisplayName="Spatial - Autocatalytic 1d neumann")>]
let autocatalytic_1d_neumann () = 
  let pde = autocatalytic().to_pde1d()
  let xs, table = Pde<float[]>.simulate_1d pde
  ()

[<Fact(DisplayName="Spatial - Autocatalytic 2d neumann")>]
let autocatalytic_2d_neumann () = 
  let pde = autocatalytic().to_pde2d()
  let xs, table = Pde<float[][]>.simulate_2d pde
  ()

let am dim = 
  let nx = if dim=1 then 101 else 31
  sprintf "directive simulation { final=36000.0; points=1000; plots=[B] }
directive parameters [r = 1e-4]
directive simulator pde
directive spatial { nx=%d; xmax=0.05; dt=200.0; default_diffusion=1e-10; random=0.2; dimensions=%d }

X + Y ->{r} X + B |
Y + X ->{r} Y + B |
X + B ->{r} X + X |
Y + B ->{r} Y + Y |

init X 5 |
init Y 5" nx dim
  |> Parser.from_string Crn.parse

[<Fact(DisplayName="Spatial - AM 1d")>]
let am_1d_periodic () = 
  let crn = am 1
  let pde = crn.to_pde1d()
  let xs, table = Pde<float[]>.simulate_1d pde
  ()

[<Fact(DisplayName="Spatial - AM 2d")>]
let am_2d_periodic () = 
  let crn = am 2
  let pde = crn.to_pde2d()
  let xs, table = Pde<float[][]>.simulate_2d pde
  ()