module Tests.Numerical

open Xunit
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.TuringNumerical

[<Fact(DisplayName="Numerical - Analyze Brusselator")>]
let bruss () =     
    let sys = Examples.brusselator ()
    let paras = ["A", 1.0; "B", 2.0; "f1", 1.0; "f2", 1.0; "DY", 7.0] |> Map.ofList
    let ws = MathNet.Numerics.Generate.LogSpaced (101, -2.0, 2.0)
    let _, _, eigs, _, _ = analyze Ode sys paras ws false
    Assert.InRange (Array.max eigs, 0.0, 10.0)

[<Fact(DisplayName="Numerical - Sample Brusselator (first match)")>]
let bruss_first () =     
    let ws = MathNet.Numerics.Generate.LogSpaced (101, -2.0, 2.0)
    let sys = 
        "directive spatial { diffusibles=[X=1.0; Y=DY] }
        directive parameters [
            A=1.0, {variation=Random; distribution=Uniform(1e-2,1e2); interval=Log};
            B=1.0, {variation=Random; distribution=Uniform(1e-2,1e2); interval=Log};
            f1=1.0;
            f2=1.0;
            DY=7.0
        ]

        ->{A} X |
        X ->{1.0} |
        X ->{B} Y |
        X + X <->{1.0}{f1} Z | 
        Z + Y ->{f2} Z + X"
        |> Microsoft.Research.CRNEngine.Crn.from_string 
        |> Dynamical.fromCRN 
    sample_first (System.Random 0) Ode sys ((=) TuringStable) ws 500 false

[<Fact(DisplayName="Numerical - Oscillatory, extended Brusselator")>]
let extended () = 
    let S2 = Examples.extended_brusselator ()    
    let ws = MathNet.Numerics.Generate.LinearSpaced(101,0.0,10.0)
    sample_first (System.Random 0) Ode S2 ((=) TuringOscillatory) ws 500 false