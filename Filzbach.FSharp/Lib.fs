// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.Filzbach

#if JavaScript
open WebSharper
#endif


[<JavaScript>]
module Lib =    

    let fst3 (a,_,_) = a

    let logOfNormPDF mean stdev v =
        let pi2 = 6.283185307179586476925286 // 2π
        let sqrt2pi = sqrt(pi2)
        let dev = (mean-v)/stdev 
        -0.5*dev*dev - log(sqrt2pi*stdev)

    let erf x = 
        let t = 1.0 / (1.0 + 0.5*(abs x))
        let coeffs = [| -1.26551223; 1.00002368; 0.37409196; 0.09678418; -0.18628806; 0.27886807; -1.13520398; 1.48851587; -0.82215223; 0.17087277 |]
        let series, _ = coeffs |> Array.fold (fun (acc,ti) ci -> acc + ci*ti, ti*t) (-x*x,1.0)      
        let tau = t * (exp series)
        if x < 0.0 then tau - 1.0 else 1.0 - tau
    
    // Inverse error-function.
    // Translated from Table 5 at https://people.maths.ox.ac.uk/gilesm/files/gems_erfinv.pdf
    let erfinv x = 
        let w = -log((1.0-x)*(1.0+x))
        let cs, dw = 
          if (w < 5.0)
          then 
            let dw = w - 2.5
            let cs = [2.81022636e-08; 3.43273939e-07; -3.5233877e-06; -4.39150654e-06; 0.00021858087; -0.00125372503; -0.00417768164; 0.246640727; 1.50140941]
            cs, dw
          else 
            let dw = sqrt w - 3.0
            let cs = [-0.000200214257; 0.000100950558; 0.00134934322; -0.00367342844; 0.00573950773; -0.0076224613; 0.00943887047; 1.00167406; 2.83297682]
            cs, dw
        let p = List.reduce (fun c0 c1 -> c0*dw + c1) cs
        p*x

    let normCDF mean stdev v = 
        let x = (v - mean) / stdev
        0.5*(1.0 + erf(x / (sqrt 2.0)))

    #if JavaScript
    [<Inline "$0">]
    #endif
    let inline uint32 x = uint32 x

    let improbable = System.Double.Epsilon // 5e-324
    let log_improbable = log(improbable) // -745

    type IRng =
        interface
            abstract member NextInt32 : unit -> uint32*IRng
            abstract member NextDouble : unit -> float*IRng
        end

    //can be redefined in different code compilations. This definition is to be translated to JS with WebSharper
    //Seeded linear congruential generator
    
    let ``lcg.a`` = 1664525.0 //from Numerical Recipes by  William H. Press, Saul A. Teukolsky, William T. Vetterling and Brian P. Flannery
    let ``lcg.c`` = 1013904223.0
    let ``lcd.m`` = 2.0**32.0
    let ``lcg.d`` = 1.0/``lcd.m``

    type LCGRng(seed) =
        interface IRng with
            member s.NextInt32() =
                let next = uint32 ((((``lcg.a``*((float)seed))%``lcd.m``)+``lcg.c``)%``lcd.m``) //We can't use uint32 to enable mod 2^32 automatic arithmetic as WebSharper translates uint32 into number (thus no mod 32)
                next,upcast LCGRng(next)

            member s.NextDouble() =
                let nextInt,nextRng = (s :> IRng).NextInt32()
                let nextFloat = (float)(nextInt)*``lcg.d`` //scaling 0..2^32-1 to 0.0 .. 1.0
                nextFloat,nextRng
    
    type NormalGeneratorState = {
        rnorm_phase:bool
        rnorm_2:float
        rnorm_f:float
    }

    //Adopted from Angara.ComputeRuntime
    type NormalGenerator(state,mean,stdev) =
        new (mean,stdev) =
            let newState =
                {
                    rnorm_phase=false
                    rnorm_2=0.0
                    rnorm_f=0.0
                }
            NormalGenerator(newState,mean,stdev)

        member s.Mean = mean
        member s.Sigma = stdev
        member s.State = state

        member s.nextDouble(rng) =
            let z,newState,rng =
                if state.rnorm_phase then
                    state.rnorm_2*state.rnorm_f, {state with rnorm_phase=false; },rng
                else
                    let rec get_S_rnorm1_state state (rng:IRng) =
                        let rn1,rng = rng.NextDouble()
                        let rn2,rng = rng.NextDouble()
                        let rnorm_1 = rn1*2.0-1.0
                        let rnorm_2 = rn2*2.0-1.0
                        let s = rnorm_1*rnorm_1 + rnorm_2*rnorm_2
                        if s<1.0 then
                            let newState = {state with rnorm_2=rnorm_2}
                            s,rnorm_1,newState,rng
                        else
                            get_S_rnorm1_state {state with rnorm_2=rnorm_2 } rng
                    let s,rnorm_1,newState,rng = get_S_rnorm1_state state rng
                    let rnorm_f = sqrt(-2.0*log(s)/s)
                    rnorm_1*rnorm_f,{newState with rnorm_phase=true; rnorm_f=rnorm_f}, rng
            z*stdev+mean, NormalGenerator(newState,mean,stdev),rng

            