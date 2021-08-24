// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Sundials

open System.Runtime.InteropServices
open System.Security
open Microsoft.FSharp.NativeInterop

#if JavaScript
open WebSharper
#endif

module internal external_sundials =          
#if JavaScript
    type SundialsOutput = delegate of float * float array -> unit

    type FunctionalRates = delegate of float * float array -> float array

    [<Inline "var sunArg = {'sN': $sN, 'rNM': $rNM,'stoichM':$stoichM, 'powersM': $powersM, 'ratesM': $ratesM, 'rNF': $rNF, 'stoichF': $stoichF,'functionalRates':$functionalRates,
            'concs': $concs, 't': $t, 'tN': $tN, 'sundialsOutput':$sundialsOutput, 'stiff': $stiff, 'abstol': $abstol, 'reltol': $reltol}; return [WS_EM_GLUE.fnSundialsSolver(sunArg),sunArg.conc];">]
    let fnSundialsSolver (sN:int,
                          rNM:int,
                          stoichM:int[],
                          powersM:int[],
                          ratesM:double[],
                          rNF:int,
                          stoichF:int[],
                          functionalRates:FunctionalRates,
                          concs:double[],
                          t:double[],
                          tN:int,
                          sundialsOutput:SundialsOutput,
                          stiff:bool,
                          abstol:double,
                          reltol:double) : (int*(float array)) = 1030,[||] // Getting exit code 1030 means that you are executing a WebSharper intended file with .NET CLR. Reference "CRNEngine" instead of "CRNEngingJS".

    [<Inline "var sunArg = {'stateN': $stateN, 'transN':$transN, 'propensities':$propensities, 'sources':$sources, 'targets':$targets, 'diagonals':$diagonals, 'prob_mass':$prob_mass, 'timesToReturnParam':$timesToReturnParam,
              'numTimesToReturnParam':$numTimesToReturnParam, 'sundialsOutput':$sundialsOutput, 'stiff': $stiff, 'abstol': $abstol, 'reltol': $reltol  }; return [WS_EM_GLUE.fnSundialsCMESolver(sunArg),sunArg.prob_mass];">]
    let fnSundialsCMESolver(stateN:int,
                            transN:int,
                            propensities:double[],
                            sources:int[],
                            targets:int[],
                            diagonals:double[],
                            prob_mass:double[],
                            timesToReturnParam:double[],
                            numTimesToReturnParam:int,
                            sundialsOutput:SundialsOutput,
                            stiff:bool,
                            abstol:double,
                            reltol:double) : (int*(float array)) = 1030,[||] // Getting exit code 1030 means that you are executing a WebSharper intended file with .NET CLR. Reference "CRNEngine" instead of "CRNEngingJS". 
#else
    //The pointer is to a C allocated array... take care! It's to be allocation-free for perf. CG
    type FunctionalRates = delegate of float * nativeptr<float> * nativeptr<float> -> unit

    type SundialsOutput = delegate of float * nativeptr<float> -> unit

    //DLL must be discoverable for this to work e.g. copy it to your exe's directory
    [<SuppressUnmanagedCodeSecurityAttribute>]
    [<DllImport("SundialsSolver15.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern int fnSundialsSolver(
        int speciesNumber,
        int massActionRatesNumber,
        int[] stoichMatrix,
        int[] powers,
        double[] massActionRates,
        int functionalRatesNumber,
        int[] stoichF,
        FunctionalRates functionalRates,
        double[] x0,
        double[] timesToReturn,
        int timesToReturnNumber,
        SundialsOutput sundialsOutput,
        bool stiff,
        double abstol,
        double reltol)

    [<SuppressUnmanagedCodeSecurityAttribute>]
    [<DllImport("SundialsSolver15.dll", CallingConvention = CallingConvention.Cdecl)>]
    extern int fnSundialsCMESolver(
        int stateN,
        int transN,
        double[] propensities,
        int[] sources,
        int[] targets,
        double[] diagonals,
        double[] p0,
        double[] timesToReturn,
        int numTimesToReturn,
        SundialsOutput sundialsOutput,
        bool stiff,
        double abstol,
        double reltol)

#endif