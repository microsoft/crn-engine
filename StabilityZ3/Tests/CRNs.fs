module Tests.CRNs

open Xunit
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.TuringSymbolic
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Expression

let turingSettings : TuringAnalysisSettings = 
    { prevent_oscillations = true
    ; prevent_noise_amp    = Strict
    ; use_Lienarad_Chipart = false
    ; group_terms          = None
    ; model_reduction      = Off
    ; timeout              = Some 10
    ; seed                 = None
    ; print_status         = true
    ; zero_cross_cst       = true
    }

let CheckTuring S = 
    let solver = Solver.solverType.PortfolioTO 240000u
    Solver.CheckTuring true solver turingSettings S

[<Fact(DisplayName="Gierer-Meinhardt with v > 0")>]
let gierer () = 
    let crn = 
        Examples.gierermeinhardt 
        |> Dynamical.setParam "a" 1.0
        |> Dynamical.setParam "c" 2.0
        |> Dynamical.setParam "d" 0.0
        |> Dynamical.setParam "D_{u}" 1.0
        |> Dynamical.setAllLB 0.0
        |> Dynamical.addCst (BGT (Key "v", Float 0.0))
    let S = crn |> CheckTuring
    Assert.Equal("SAT", S.solution.Status)

[<Fact(DisplayName="CRNs - Bimolecular Brusselator")>]
let beta () =     
    System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    let full = Examples.brusselator ()
    //let reduced, repl = full.ModelReduceKeep ["X";"Y"]
    let solver = Solver.solverType.PortfolioTO 240000u
    let res = full |> Solver.CheckTuring true solver turingSettings
    Assert.Equal("SAT", res.solution.Status)

[<Fact(DisplayName="Gauss-Jordan elimination - Simple")>]
let gaussJordan_simple () = 
    let A = [| [| 1.0; 1.0; 1.0; 0.0 |]; [| 1.0; 1.0; 0.0; 1.0 |] |]
    let C = [| Key "C0"; Key "C1" |]

    let rep = Invariants.GaussJordanElimination A C
    ()


[<Fact(DisplayName="CRNs - Conservation in non-diffusibles, UNSAT")>]
let conservedCRN () = 
    let crn = "directive spatial { diffusibles=[A=1.0; B=DB] }
    A ->{k1} 2B |
    B + D ->{k2} A + D |
    C + E ->{k3} D + F |
    D + F ->{k4} C + E" 
    let S = crn |> Crn.from_string |> Dynamical.fromCRN |> CheckTuring
    Assert.Equal("UNSAT", S.solution.Status)

[<Fact(DisplayName="CRNs - Conservation in non-diffusibles, SAT")>]
let conserved_brusselator () = 
    let crn = "
directive spatial { diffusibles=[X=1.0; Y=DY] }
directive parameters [
    A=1.0, {variation=Random};
    B=1.0, {variation=Random};
    DY=10.0, {variation=Random};
]

| ->{A} X 
| X ->{1.0} 
| X ->{B} Y 
| X + X <->{1}{1} Z
| Z + Y ->{1} Z + X 
| Z + Z1 -> Z + Z2
| Z + Z2 -> Z + Z1"
    let solver = Solver.solverType.PortfolioTO 10000u
    let S = crn |> Crn.from_string |> Dynamical.fromCRN
    
    let S_out = S |> Solver.CheckTuring true solver turingSettings
    Assert.Equal("UNSAT", S_out.solution.Status)

[<Fact(DisplayName="CRN - Stephen 2 species")>]
let stephen_crn () = 
    let S = 
        """
        directive parameters [
            k1 = 1.0, {variation=Random};
            k2 = 1.0, {variation=Random};
            k3 = 1.0, {variation=Random};
            k4 = 1.0, {variation=Random};
            k5 = 1.0, {variation=Random};
        ]
        directive spatial { diffusibles = [ A=1.0; B=DB ] }

        | A+B ->{k1} A
        | 2B  ->{k2} 3B
        | B   ->{k3} A+B
        | 2B  ->{k4} A+2B
        | A   ->{k5} 
        """
    let res = S |> Crn.from_string |> Dynamical.fromCRN |> CheckTuring
    Assert.Equal("SAT", res.solution.Status)