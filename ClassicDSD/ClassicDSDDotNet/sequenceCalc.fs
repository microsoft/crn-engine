// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.SequenceCalc
open Microsoft.Research.DNA
open Microsoft.Research.DNA.Measures


module Prim = Microsoft.Research.CRNEngine.Expression
type prim = Microsoft.Research.CRNEngine.Expression.t<string>
module Species = Microsoft.Research.DNA.Species

(* Functions to convert floats with units to raw floats *)
let entalpy (dH:float<kcal/mol>)  = dH |> float |> Prim.Float
let entropy (dS:float<cal/K mol>) = dS |> float |> Prim.Float
let temperature (t:float<K>)      = t |> float |> Prim.Float // "T" |> Prim.Key
let e (_:float)                   = 2.7182818284590452353602 |> float |> Prim.Float // "e" |> Prim.Key
let r (_:float<kcal/(K mol)>)     = 1.9872041E-3 |> float |> Prim.Float // "R" |> Prim.Key
let KcalInCal (x:float<cal/kcal>) = x |> float |> Prim.Float
let CalInKcal (x:float<kcal/cal>) = x |> float |> Prim.Float
let prim_to_string = Prim.to_string id

(* learning F#. Frits Dannenberg 20/08/2014
Rip from DNA self-assembly project that I created. 
Simple function that takes in sequence and produces dH, dS, dG, G37, G60 and unbinding rate 
as a function of binding sequence, binding rate, salt concentration(s) and temperature 

 FD: 2014/09/08: Extending class to contain new thermodynamic model suited to DSD, 
    created to automatically infer rates from a sequence-description of domains.
    Consists of energy constants and related functions. *)

// TYPES
type domain = Domain.t
type gibbs = { dG : float<kcal/mol> } with
    override this.ToString() = sprintf "dG = %.4f%s" (float this.dG) " kcal/mol"
end

type energy = { dH : float<kcal/mol>; dS : float<cal/(mol*K)> } with
(* Member operator-overloads aren't support by WebSharper at present: https://github.com/intellifactory/websharper/issues/102#issuecomment-83112701
    static member (+) (a : energy, b: energy) = { dH = a.dH + b.dH; dS = a.dS + b.dS }
    static member (-) (a : energy, b: energy) = { dH = a.dH - b.dH; dS = a.dS - b.dS }
    static member (*) (a : energy, b: float) = { dH = b*a.dH; dS = b * a.dS }
    static member (*) (b: float, a : energy) = { dH = b*a.dH; dS = b * a.dS }
*)
    override this.ToString() = sprintf "dH = %f%s, dS = %f%s, dG37 = %f%s" (float this.dH) " kcal/mol" (float this.dS) " cal/(mol*K)"  (float (this.dH - 0.001<kcal/cal> * (273.15<K> + 37.0<K>) * this.dS ))  " kcal/mol-37"
end

let energy_plus (a : energy) (b: energy) = { dH = a.dH + b.dH; dS = a.dS + b.dS }
let energy_minus (a : energy) (b: energy) = { dH = a.dH - b.dH; dS = a.dS - b.dS }
let energy_rmul (a : energy) (b: float) = { dH = b*a.dH; dS = b * a.dS }
let energy_lmul (b : float) (a: energy) = { dH = b*a.dH; dS = b * a.dS }

type energyExpression = { dHE : prim; dSE : prim } with
(*
    static member (+) (a : energyExpression, b: energyExpression) = { dHE = Prim.add a.dHE  b.dHE; dSE = Prim.add a.dSE b.dSE }
    static member (/) (b: energyExpression, a : energy)           = { dHE = Prim.add (entalpy a.dH) b.dHE; dSE = Prim.add (entropy a.dS ) b.dSE }  //FD: This used to be ++ instead of /
    static member (-) (a : energyExpression, b: energyExpression) = { dHE = Prim.sub a.dHE  b.dHE; dSE = Prim.sub a.dSE b.dSE }
    static member (*) (a : energyExpression, b: float)            = { dHE = Prim.mul (Prim.Float b) a.dHE; dSE = Prim.mul (Prim.Float b) a.dSE }
    static member (*) (b: float, a : energyExpression)            = { dHE = Prim.mul (Prim.Float b) a.dHE; dSE = Prim.mul (Prim.Float b) a.dSE }
*)
    override this.ToString() = sprintf "dH = %s%s, dS = %s%s, dG = %f%s" (prim_to_string this.dHE) " kcal/mol" (prim_to_string this.dSE) " cal/(mol*K)"     ( (float (prim_to_string this.dHE)) - 0.001 * (273.15 + 25.0) * (float (prim_to_string this.dSE) ))      " kcal/mol - 25.0"  //  - 0.001 * (273.15 + 37.0) * (float (prim_to_string this.dSE) ) 
end

let energyExpression_plus (a : energyExpression) (b: energyExpression) = { dHE = Prim.add a.dHE  b.dHE; dSE = Prim.add a.dSE b.dSE }
let energyExpression_div (b : energyExpression) (a: energy) = { dHE = Prim.add (entalpy a.dH) b.dHE; dSE = Prim.add (entropy a.dS ) b.dSE }
let energyExpression_minus (a : energyExpression) (b: energyExpression) = { dHE = Prim.sub a.dHE  b.dHE; dSE = Prim.sub a.dSE b.dSE }
let energyExpression_rmul (a : energyExpression) (b: float) = { dHE = Prim.mul (Prim.Float b) a.dHE; dSE = Prim.mul (Prim.Float b) a.dSE }
let energyExpression_lmul (b : float) (a: energyExpression) = { dHE = Prim.mul (Prim.Float b) a.dHE; dSE = Prim.mul (Prim.Float b) a.dSE }

type dnabase = Sequence.dnabase


// CONSTANTS
let calIntoKcal (input : float<cal/mol>) =  0.001<kcal/cal> * input
let kcalIntoCal (input : float<kcal/mol>) =  1000.0<cal/kcal> * input

let tempCintoK (input : float<C>) = input * 1.0<K/C> + 273.15<K>

let inferDSFromDG (input:float<kcal/mol>) temperature : energy =
    {dH = 0.0<kcal/mol>; dS = (kcalIntoCal input) / -temperature}

let inferDSEFromDGE (input:prim) t : energyExpression =
    let newVal = Prim.div ( Prim.mul (KcalInCal 1000.0<cal/kcal>) input ) (temperature -t) in
    {dHE = entalpy 0.0<kcal/mol>; dSE = newVal }

let makeExpression (input: energy) : energyExpression =
        {dHE = entalpy input.dH ; dSE = entropy input.dS }


let R = 1.9872<cal /(mol*K)>
let temp25C = 273.15<K> + 25.0<K>
let temp37C = 273.15<K> + 37.0<K>
let temp60C = 273.15<K> + 60.0<K>

let diffCorrection (input:float<kcal/mol>) = inferDSFromDG input temp60C

// SantaLucia 2004 constants
// Nearest Neighbours
let AA : energy = { dH = -7.6<kcal/mol>; dS = -21.3<cal/(mol*K)>}
let AT : energy = { dH = -7.2<kcal/mol>; dS = -20.4<cal/(mol*K)>}
let TA : energy = { dH = -7.2<kcal/mol>; dS = -21.3<cal/(mol*K)>}
let CA : energy = { dH = -8.5<kcal/mol>; dS =  -22.7<cal/(mol*K)>}
let GT : energy = { dH = -8.4<kcal/mol>; dS = -22.4<cal/(mol*K)>}
let CT : energy = { dH = -7.8<kcal/mol>; dS = -21.0<cal/(mol*K)>}
let GA : energy = { dH = -8.2<kcal/mol>; dS = -22.2<cal/(mol*K)>}
let CG : energy = { dH = -10.6<kcal/mol>; dS = -27.2<cal/(mol*K)>}
let GC : energy = { dH = -9.8<kcal/mol>; dS =  -24.4<cal/(mol*K)>}
let GG : energy = { dH = -8.0<kcal/mol>; dS = -19.9<cal/(mol*K)>}

let init (opts:Options.t)  : energy   = { dH =  0.2<kcal/mol>; dS = -5.7<cal/(mol*K)>} |> energy_plus <| diffCorrection (Options.getStabilityCorrection opts)
let termAT  : energy = { dH =  2.2<kcal/mol>; dS =  6.9<cal/(mol*K)>}
let termCG  : energy = { dH =  0.0<kcal/mol>; dS =  0.0<cal/(mol*K)>}
let empty : energy = { dH =  0.0<kcal/mol>; dS =  0.0<cal/(mol*K)>}

//  salt correction
let concMg =  12.5E-3                     // no units since they won't be used   (here in M)
let concTris =0.5*40.0E-3               // no units since they won't be used   (here in M)
let concNa = 0.0                        // no units since they won't be used   (here in M)
let preCalc = 0.5 * concTris + concNa + 3.3 * sqrt(concMg)
let saltCorr = (1.0<cal/(mol*K)> * 0.368 * System.Math.Log preCalc)  //
let saltCorrectionPerNN : energy = { empty with  dS =  saltCorr}      // FOR TESTING PUT SALTCORRECTION TO ZERO  //
//let saltCorrectionPerNN : energy = { empty with  dS = 0.0<cal/(K mol)>}     





// Dangling ends - Bommarito 2000
// fiveXY   means sequence 5'-XY-3' where 5'-X is dangling and Y is watson-crick base-paired
let fiveAA : energy = { dH = 0.2<kcal/mol>; dS = 2.3<cal/(mol*K)>}
let fiveAC : energy = { dH = -6.3<kcal/mol>; dS = -17.1<cal/(mol*K)>}
let fiveAG : energy = { dH = -3.7<kcal/mol>; dS = -10.0<cal/(mol*K)>}
let fiveAT : energy = { dH = -2.9<kcal/mol>; dS = -7.6<cal/(mol*K)>}
let fiveCA : energy = { dH =  0.6<kcal/mol>; dS =  3.3<cal/(mol*K)>}
let fiveCC : energy = { dH = -4.4<kcal/mol>; dS = -12.6<cal/(mol*K)>}
let fiveCG : energy = { dH = -4.0<kcal/mol>; dS = -11.9<cal/(mol*K)>}
let fiveCT : energy = { dH = -4.1<kcal/mol>; dS = -13.0<cal/(mol*K)>}
let fiveGA : energy = { dH = -1.1<kcal/mol>; dS = -1.6<cal/(mol*K)>}
let fiveGC : energy = { dH = -5.1<kcal/mol>; dS = -14.0<cal/(mol*K)>}
let fiveGG : energy = { dH = -3.9<kcal/mol>; dS = -10.9<cal/(mol*K)>}
let fiveGT : energy = { dH = -4.2<kcal/mol>; dS = -15.0<cal/(mol*K)>}
let fiveTA : energy = { dH = -6.9<kcal/mol>; dS = -20.0<cal/(mol*K)>}
let fiveTC : energy = { dH = -4.0<kcal/mol>; dS = -10.9<cal/(mol*K)>}
let fiveTG : energy = { dH = -4.9<kcal/mol>; dS = -13.8<cal/(mol*K)>}
let fiveTT : energy = { dH = -0.2<kcal/mol>; dS = -0.5<cal/(mol*K)>}

// XYthree   means sequence 5'-XY-3' where X is watson-crick base-paired and Y-3' is dangling
let TAthree : energy = { dH = -0.7<kcal/mol>; dS =  -0.8<cal/(mol*K)>}
let GAthree : energy = { dH = -2.1<kcal/mol>; dS =  -3.9<cal/(mol*K)>}
let CAthree : energy = { dH = -5.9<kcal/mol>; dS = -16.5<cal/(mol*K)>}
let AAthree : energy = { dH = -0.5<kcal/mol>; dS =  -1.1<cal/(mol*K)>}
let TCthree : energy = { dH =  4.4<kcal/mol>; dS =  14.9<cal/(mol*K)>}
let GCthree : energy = { dH = -0.2<kcal/mol>; dS = -0.1<cal/(mol*K)>}
let CCthree : energy = { dH = -2.6<kcal/mol>; dS = -7.4<cal/(mol*K)>}
let ACthree : energy = { dH =  4.7<kcal/mol>; dS = 14.2<cal/(mol*K)>}
let TGthree : energy = { dH = -1.6<kcal/mol>; dS = -3.6<cal/(mol*K)>}
let GGthree : energy = { dH = -3.9<kcal/mol>; dS = -11.2<cal/(mol*K)>}
let CGthree : energy = { dH = -3.2<kcal/mol>; dS = -10.4<cal/(mol*K)>}
let AGthree : energy = { dH = -4.1<kcal/mol>; dS = -13.1<cal/(mol*K)>}
let TTthree : energy = { dH =  2.9<kcal/mol>; dS = 10.4<cal/(mol*K)>}
let GTthree : energy = { dH = -4.4<kcal/mol>; dS = -13.1<cal/(mol*K)>}
let CTthree : energy = { dH = -5.2<kcal/mol>; dS = -15.0<cal/(mol*K)>}
let ATthree : energy = { dH = -3.8<kcal/mol>; dS = -12.6<cal/(mol*K)>}

// nicks 5'-XY-3' means there is a nick between base X and Y. dG_37 is given here  (T=310K)
// FD: although these appear correlated, they are individual estimates for each situation. 
// FD: Reduction to an essential 10 parameters is not appropriate here. (as the nick occurs at opposite sites)
// source: Protozanova 2004
let nickAA  : gibbs = {dG = -1.04<kcal/mol>}
let nickAT  : gibbs = {dG = -1.27<kcal/mol>}
let nickAG  : gibbs = {dG = -1.29<kcal/mol>}
let nickAC  : gibbs = {dG = -2.04<kcal/mol>}
let nickTA  : gibbs = {dG = -0.12<kcal/mol>}
let nickTT  : gibbs = {dG = -1.04<kcal/mol>}
let nickTG  : gibbs = {dG = -0.78<kcal/mol>}
let nickTC  : gibbs = {dG = -1.66<kcal/mol>}
let nickGA  : gibbs = {dG = -1.66<kcal/mol>}
let nickGT  : gibbs = {dG = -2.04<kcal/mol>}
let nickGG  : gibbs = {dG = -1.97<kcal/mol>}
let nickGC  : gibbs = {dG = -2.70<kcal/mol>}
let nickCA  : gibbs = {dG = -0.78<kcal/mol>}
let nickCT  : gibbs = {dG = -1.29<kcal/mol>}
let nickCG  : gibbs = {dG = -1.44<kcal/mol>}
let nickCC  : gibbs = {dG = -1.97<kcal/mol>}

// split gibbs free energy into dH dS assuming T= 37 C and dS = (0.0027 / K) dH    
// this is from Zhang 2009, 0.0027 is fitted for data to compute dH dS from dG for nicked data (see above)
let interpolatedEnergy (input:gibbs)  : energy = 
    let factor = 0.0027</K> in
    let dHnew = input.dG  /  (1.0-temp37C*factor) in
    let dSnew = factor * kcalIntoCal dHnew  in
    {dH = dHnew; dS = dSnew}



// Thomas Ouldridge's experimental observations for single dangle and double dangle
//let coaxialDangleTerm       = inferDSFromDG 1.2<kcal/mol> temp25C //inferDSFromDG 1.2<kcal/mol> temp25C  ---double coaxial old value = 3.2
//let doubleCoaxialDangleTerm = inferDSFromDG 3.2<kcal/mol> temp25C

let coaxialDangleTerm (opts:Options.t)                      = inferDSFromDG (Options.getCoaxialDangle opts) temp25C 
let doubleCoaxialDangleTerm (opts:Options.t)                = inferDSFromDG (Options.getDoubleCoaxialDangle opts) temp25C 
let coaxialDangleFivePrimeCorrection (opts:Options.t)       = inferDSEFromDGE  (  Options.getCoaxialCorrection opts ) temp25C
 


// FUNCTIONS

let getEnergies l r =
    match l, r with 
    | Sequence.Adenine, Sequence.Adenine    -> AA |> energy_plus <| saltCorrectionPerNN
    | Sequence.Thymine, Sequence.Thymine    -> AA |> energy_plus <| saltCorrectionPerNN
    | Sequence.Adenine, Sequence.Thymine    -> AT |> energy_plus <| saltCorrectionPerNN
    | Sequence.Thymine, Sequence.Adenine    -> TA |> energy_plus <| saltCorrectionPerNN
    | Sequence.Cytosine, Sequence.Adenine   -> CA |> energy_plus <| saltCorrectionPerNN
    | Sequence.Thymine, Sequence.Guanine    -> CA |> energy_plus <| saltCorrectionPerNN
    | Sequence.Guanine, Sequence.Thymine    -> GT |> energy_plus <| saltCorrectionPerNN
    | Sequence.Adenine, Sequence.Cytosine   -> GT |> energy_plus <| saltCorrectionPerNN
    | Sequence.Cytosine, Sequence.Thymine   -> CT |> energy_plus <| saltCorrectionPerNN
    | Sequence.Adenine, Sequence.Guanine    -> CT |> energy_plus <| saltCorrectionPerNN
    | Sequence.Guanine, Sequence.Adenine    -> GA |> energy_plus <| saltCorrectionPerNN
    | Sequence.Thymine, Sequence.Cytosine   -> GA |> energy_plus <| saltCorrectionPerNN
    | Sequence.Cytosine, Sequence.Guanine   -> CG |> energy_plus <| saltCorrectionPerNN
    | Sequence.Guanine, Sequence.Cytosine   -> GC |> energy_plus <| saltCorrectionPerNN
    | Sequence.Guanine, Sequence.Guanine    -> GG |> energy_plus <| saltCorrectionPerNN
    | Sequence.Cytosine, Sequence.Cytosine  -> GG |> energy_plus <| saltCorrectionPerNN

let dangleFive (opts:Options.t) l r = 
    let output =
      match l, r with 
      | Sequence.Adenine, Sequence.Adenine    -> fiveAA 
      | Sequence.Thymine, Sequence.Thymine    -> fiveTT
      | Sequence.Adenine, Sequence.Thymine    -> fiveAT
      | Sequence.Thymine, Sequence.Adenine    -> fiveTA
      | Sequence.Cytosine, Sequence.Adenine   -> fiveCA
      | Sequence.Thymine, Sequence.Guanine    -> fiveTG
      | Sequence.Guanine, Sequence.Thymine    -> fiveGT
      | Sequence.Adenine, Sequence.Cytosine   -> fiveAC
      | Sequence.Cytosine, Sequence.Thymine   -> fiveCT
      | Sequence.Adenine, Sequence.Guanine    -> fiveAG
      | Sequence.Guanine, Sequence.Adenine    -> fiveGA
      | Sequence.Thymine, Sequence.Cytosine   -> fiveTC
      | Sequence.Cytosine, Sequence.Guanine   -> fiveCG
      | Sequence.Guanine, Sequence.Cytosine   -> fiveGC
      | Sequence.Guanine, Sequence.Guanine    -> fiveGG
      | Sequence.Cytosine, Sequence.Cytosine  -> fiveCC

    (Options.getTerminalDangleFactor opts) |> energy_lmul <| output 

let dangleThree (opts:Options.t) r l =   // dangleThree X Y means 3'-X-Y-5' where X is dangling and Y is base-paired
    let output =
      match l, r with 
      | Sequence.Adenine, Sequence.Adenine    -> AAthree
      | Sequence.Thymine, Sequence.Thymine    -> TTthree
      | Sequence.Adenine, Sequence.Thymine    -> ATthree
      | Sequence.Thymine, Sequence.Adenine    -> TAthree
      | Sequence.Cytosine, Sequence.Adenine   -> CAthree
      | Sequence.Thymine, Sequence.Guanine    -> TGthree
      | Sequence.Guanine, Sequence.Thymine    -> GTthree
      | Sequence.Adenine, Sequence.Cytosine   -> ACthree
      | Sequence.Cytosine, Sequence.Thymine   -> CTthree
      | Sequence.Adenine, Sequence.Guanine    -> AGthree
      | Sequence.Guanine, Sequence.Adenine    -> GAthree
      | Sequence.Thymine, Sequence.Cytosine   -> TCthree
      | Sequence.Cytosine, Sequence.Guanine   -> CGthree
      | Sequence.Guanine, Sequence.Cytosine   -> GCthree
      | Sequence.Guanine, Sequence.Guanine    -> GGthree
      | Sequence.Cytosine, Sequence.Cytosine  -> CCthree

    (Options.getTerminalDangleFactor opts) |> energy_lmul <| output 

let nick l r =      // nick 5'-LEFT-'- RIGHT-3'
    let nickTerm =
      match l, r with 
      | Sequence.Adenine, Sequence.Adenine    -> interpolatedEnergy nickAA
      | Sequence.Thymine, Sequence.Thymine    -> interpolatedEnergy nickTT
      | Sequence.Adenine, Sequence.Thymine    -> interpolatedEnergy nickAT
      | Sequence.Thymine, Sequence.Adenine    -> interpolatedEnergy nickTA
      | Sequence.Cytosine, Sequence.Adenine   -> interpolatedEnergy nickCA
      | Sequence.Thymine, Sequence.Guanine    -> interpolatedEnergy nickTG
      | Sequence.Guanine, Sequence.Thymine    -> interpolatedEnergy nickGT
      | Sequence.Adenine, Sequence.Cytosine   -> interpolatedEnergy nickAC
      | Sequence.Cytosine, Sequence.Thymine   -> interpolatedEnergy nickCT
      | Sequence.Adenine, Sequence.Guanine    -> interpolatedEnergy nickAG
      | Sequence.Guanine, Sequence.Adenine    -> interpolatedEnergy nickGA
      | Sequence.Thymine, Sequence.Cytosine   -> interpolatedEnergy nickTC
      | Sequence.Cytosine, Sequence.Guanine   -> interpolatedEnergy nickCG
      | Sequence.Guanine, Sequence.Cytosine   -> interpolatedEnergy nickGC
      | Sequence.Guanine, Sequence.Guanine    -> interpolatedEnergy nickGG
      | Sequence.Cytosine, Sequence.Cytosine  -> interpolatedEnergy nickCC

    1.0 |> energy_lmul <| nickTerm



let getTailEnergy dnabase =     // terminal AT selector
    match dnabase with
    | Sequence.Adenine | Sequence.Thymine ->  termAT
    | Sequence.Cytosine | Sequence.Guanine -> termCG

let rec freeEnergyProcessorOnlyNN (output : energy) (bases : dnabase list) = 
    match bases with
    | []            -> output
    | [_]           -> output                                                                         // single base, ignore terminal terms
    | l :: tail     -> freeEnergyProcessorOnlyNN (output |> energy_plus <| getEnergies l (List.head tail)) tail       // at least two bases


let rec freeEnergyProcessor (output : energy) (bases : dnabase list) =
    match bases with
    | [l] -> output |> energy_plus <| (getTailEnergy l)     // single base
    | l :: tail -> freeEnergyProcessor (output |> energy_plus <| getEnergies l (List.head tail)) tail           // at least two bases
    | _ -> failwith "error in freeEnergyProcessor: this line should not be reached"             // empty 

let freeEnergy (opts : Options.t) (bases: Sequence.dnabase list) = 
    let startEnergy = (init opts) |> energy_plus <| getTailEnergy (List.head bases) in
    freeEnergyProcessor startEnergy bases

let gTemp input  (temp : float<K>) : gibbs = { dG = (input.dH -  calIntoKcal  ( temp * input.dS))  }
let g37 (opts : Options.t) sequence =  gTemp (freeEnergy opts sequence) temp37C
let g60 (opts : Options.t) sequence =  gTemp (freeEnergy opts sequence) temp60C

let freeEnergyFromString  (input: string) =  // helper function
    let sequence =  Sequence.parse_sequence input in
    (freeEnergy Options.default_options sequence)


let unbindRate (temp : float<K>) (rate : float<s^-1>) ( energy : energy ) = 
    let upper   = (kcalIntoCal energy.dH) -  (temp *energy.dS) in
    let lower   = R * temp in
    let expVal  = exp(upper / lower) in
    (rate * expVal)

let getDG ( temp : float<K> ) ( input : energyExpression )= 
    let tempE = temperature temp in
    Prim.sub input.dHE (Prim.mul tempE (CalInKcal 0.001<kcal/cal>) |> Prim.mul input.dSE)

let exponent  ( temp : float<K> ) ( input : energyExpression ) = 
    let upper = getDG temp input in
    let lower = Prim.mul (r 1000000000.0<kcal/(mol*K) >) (temperature temp)  in
    Prim.div upper lower

let unbindRateExpression (temp : float<K>) (rate : prim) ( input : energyExpression ) = 
    let factor = Prim.power (e 100000000.0) (exponent temp input) in       // Prim.E <float> returns mathematical e constant here
    (Prim.mul rate factor) |> Prim.simplify


let unbindRateFromSequence (temp1 : float<K>) (rate : float</s>)  (sequence : string) = 
    let freeEnergy = freeEnergyFromString sequence in
    unbindRate temp1 rate freeEnergy

let getDomain mapping input =
    match Sequence.get_domain mapping input with
    | Some (Some []) -> failwith "error in getDomain: empty sequence"             // empty 
    | Some (Some bases) -> bases
    | None
    | Some None -> failwith "error in getDomain: sequence unknown"

let selectLeftMostBase (mapping: Sequence.mapping) (input: Segment.domain)  = 
// FD: mapping already corrects for complement/star domains -- somehow the GUI does not reflect this, but either way we do not have to compensate for it here

    List.head (getDomain mapping input)   
 
 

let selectRightMostBase (mapping: Sequence.mapping) (input: Segment.domain)  = 
 // FD: mapping already corrects for complement/star domains -- somehow the GUI does not reflect this, but either way we do not have to compensate for it here
  
    List.head (List.rev (getDomain  mapping input)) 


let leftMostBaseTop (mapping: Sequence.mapping)  (dangle:Segment.domain list)  = 
    match dangle with
    |  head :: _    ->  selectLeftMostBase mapping head
    |  _            ->  failwith " did not find base in top dangle -- are all domains mapped to a sequence? "

let leftMostBaseBottom (mapping: Sequence.mapping)  (dangle:Segment.domain list)  = 
    match dangle with
    |  head :: _    ->  selectRightMostBase mapping  head
    |  _            ->  failwith " did not find base in bottom dangle -- are all domains mapped to a sequence? "


let rightMostBaseTop  (mapping: Sequence.mapping)  (dangle:Segment.domain list)  = 
    match dangle with
    |  _ :: _       ->  selectRightMostBase mapping (List.head (List.rev dangle))
    |  _            ->  failwith " did not find base in top dangle -- are all domains mapped to a sequence? "

let rightMostBaseBottom  (mapping: Sequence.mapping)  (dangle:Segment.domain list)  = 
    match dangle with
    |  _ :: _       ->  selectLeftMostBase mapping (List.head (List.rev dangle))
    |  _            ->  failwith " did not find base in top dangle -- are all domains mapped to a sequence? "


let rightTerminalTerm (mapping : Sequence.mapping) (dsSegment : Segment.domain list) =
    getTailEnergy (rightMostBaseTop mapping dsSegment)


let leftTerminalTerm (mapping : Sequence.mapping) (dsSegment : Segment.domain list) =
    getTailEnergy (leftMostBaseTop mapping dsSegment)


let dangleEnergyTopLeft (opts:Options.t) (mapping: Sequence.mapping) (dsSegments:Segment.domain list) (dangle:Segment.domain list)  = 

    let dangleBase = leftMostBaseTop mapping dangle in
    let pairedBase = rightMostBaseTop mapping dsSegments in

    dangleThree opts dangleBase pairedBase |> energy_plus <| rightTerminalTerm mapping dsSegments    // note: ignore terminal term for bottom strands as convention


let dangleEnergyTopRight (opts:Options.t) (mapping: Sequence.mapping) (dangle:Segment.domain list)   (dsSegments:Segment.domain list) = 

    let dangleBase = rightMostBaseTop mapping dangle in
    let pairedBase = leftMostBaseTop mapping dsSegments in

    dangleFive opts dangleBase pairedBase |> energy_plus <| leftTerminalTerm mapping dsSegments      // note: ignore terminal term for bottom strands as convention


let dangleEnergyBottomLeft (opts:Options.t) (mapping: Sequence.mapping) (dsSegments:Segment.domain list) (dangle:Segment.domain list)  = 

    let dangleBase = leftMostBaseBottom mapping dangle in
    let pairedBase = rightMostBaseTop mapping dsSegments in

    dangleFive opts dangleBase (Sequence.complement_dnabase pairedBase)


let dangleEnergyBottomRight (opts:Options.t) (mapping: Sequence.mapping) (dangle:Segment.domain list)   (dsSegments:Segment.domain list) = 

    let dangleBase = rightMostBaseBottom mapping dangle
    let pairedBase = leftMostBaseTop mapping dsSegments

    dangleThree opts dangleBase (Sequence.complement_dnabase pairedBase)

let bottomDangleEnergyConnectedViaTop (opts:Options.t) (mapping: Sequence.mapping) leftDoubleStranded rightDoubleStranded bottomLeftDangle bottomRightDangle = 
    
    let output =
      match bottomLeftDangle with
      | [] -> empty 
      | _  -> dangleEnergyBottomLeft opts mapping leftDoubleStranded bottomLeftDangle

    match bottomRightDangle with
    | [] -> output
    | _  -> output |> energy_plus <| dangleEnergyBottomRight opts mapping bottomRightDangle rightDoubleStranded


let topDangleEnergyConnectedViaTop (opts:Options.t) (mapping: Sequence.mapping) leftDoubleStranded rightDoubleStranded topDangle  =

    // contributions from the top dangling strands
    let output = dangleEnergyTopLeft opts mapping leftDoubleStranded topDangle
    let output = output |> energy_plus <| dangleEnergyTopRight opts mapping topDangle rightDoubleStranded
    
    output

let leftDangles (opts:Options.t) mapping topDangle bottomDangle doubleStranded =
    
    let output =
      match topDangle with
      | [] -> leftTerminalTerm mapping doubleStranded                       // terminal correction on top strand only
      | _  -> dangleEnergyTopRight opts mapping topDangle doubleStranded

    match bottomDangle with
    | [] -> output                                                       
    | _  -> output |> energy_plus <| dangleEnergyBottomRight opts mapping bottomDangle doubleStranded



let rightDangles (opts:Options.t)  mapping topDangle bottomDangle doubleStranded =
    
    let output =
      match topDangle with
      | [] -> rightTerminalTerm mapping doubleStranded                      // terminal correction on top strand only
      | _  -> dangleEnergyTopLeft opts mapping doubleStranded topDangle

    match bottomDangle with
    | [] -> output
    | _  -> output |> energy_plus <| dangleEnergyBottomLeft opts  mapping doubleStranded bottomDangle



let topDangleEnergyConnectedViaBottom (opts:Options.t) (mapping: Sequence.mapping) leftDoubleStranded rightDoubleStranded topLeftDangle topRightDangle = 
    
    let output =
      match topLeftDangle with
      | [] -> empty 
      | _  -> dangleEnergyTopLeft opts mapping leftDoubleStranded topLeftDangle

    match topRightDangle with
    | [] -> output
    | _  -> output |> energy_plus <| dangleEnergyTopRight opts mapping topRightDangle rightDoubleStranded


let bottomDangleEnergyConnectedViaBottom (opts:Options.t) (mapping: Sequence.mapping) leftDoubleStranded rightDoubleStranded bottomDangle  =

    // contributions from the top dangling strands
    let output = dangleEnergyBottomLeft opts mapping leftDoubleStranded bottomDangle
    let output = output |> energy_plus <| dangleEnergyBottomRight opts mapping bottomDangle rightDoubleStranded
    
    output

let nickAtBottom (mapping: Sequence.mapping)  (left:Segment.domain list) (right:Segment.domain list) : energy = 
// a nick occurs at the bottom between the rightmost domain of the left dsSegment and the leftmost domain of the right dsSegment

    let leftBase = rightMostBaseTop mapping left in     // top bases
    let rightBase = leftMostBaseTop mapping right in    // top bases, nick occurs at bottom

    nick (Sequence.complement_dnabase rightBase) (Sequence.complement_dnabase leftBase)

let nickAtTop (mapping: Sequence.mapping)  (left:Segment.domain list) (right:Segment.domain list) = 
// a nick occurs at the top strand between the rightmost domain of the left dsSegment and the leftmost domain of the right dsSegment

    let leftBase = rightMostBaseTop mapping left in     // top bases
    let rightBase = leftMostBaseTop mapping right in    // top bases, nick occurs at top

    nick leftBase rightBase

let testCoaxialDangle left right = 
    
    match left, right with
    | [], [] -> false
    | _, _   -> true 

let coaxialDangleTop (opts:Options.t) left right = 
    
    match left, right with
    | [], [] -> makeExpression empty
    | _, []  -> (0.5 |> energyExpression_lmul <| coaxialDangleFivePrimeCorrection opts) |> energyExpression_div <| (coaxialDangleTerm opts)  // left is dangling from 3' end
    | [], _  -> (-0.5 |> energyExpression_lmul <| coaxialDangleFivePrimeCorrection opts) |> energyExpression_div <| coaxialDangleTerm opts  // right is dangling from 5' end 
    | _, _   -> makeExpression (doubleCoaxialDangleTerm opts)

let coaxialDangleBottom (opts:Options.t) left right : energyExpression = 
    
    match left, right with
    | [], [] ->  makeExpression empty
    | _, []  ->  (-0.5 |> energyExpression_lmul <| coaxialDangleFivePrimeCorrection opts) |> energyExpression_div <| (coaxialDangleTerm opts)     // left is dangling from 5' end 
    | [], _  ->  (0.5 |> energyExpression_lmul <| coaxialDangleFivePrimeCorrection opts) |> energyExpression_div <| (coaxialDangleTerm opts)      // right is dangling from 3' end 
    | _, _   ->  makeExpression (doubleCoaxialDangleTerm opts)


let joinsViaTop  (opts:Options.t) (leftSegment : Segment.t) (rightSegment : Segment.t) (mapping: Sequence.mapping) : energyExpression =
// two segments that are linked via top. Compute dangle and terminal terms, and if not dangling, compute nick energies.

    let leftDoubleStranded = Segment.double_stranded_region leftSegment
    let rightDoubleStranded = Segment.double_stranded_region rightSegment

    let topDangle  = Segment.top_right_overhang leftSegment @ Segment.top_left_overhang rightSegment

    let bottomLeftDangle = Segment.bottom_right_overhang leftSegment
    let bottomRightDangle = Segment.bottom_left_overhang rightSegment

    match leftDoubleStranded, topDangle, rightDoubleStranded with
    // empty double stranded domain on the left, so return free dangles top and bottom
    | [], _ , _       ->     makeExpression (leftDangles opts mapping topDangle bottomRightDangle rightDoubleStranded)
    // empty double stranded domain on the right, so return free dangles top and bottom
    | _ , _ , []      ->     makeExpression (rightDangles opts mapping topDangle bottomLeftDangle leftDoubleStranded)
    // the dangle contains no domains, so we are dealing with a nick. Return nicking energy + any coaxial dangling term
    | _ , [], _       ->    (coaxialDangleBottom opts bottomLeftDangle bottomRightDangle) |> energyExpression_div <| (nickAtBottom mapping leftDoubleStranded rightDoubleStranded)
    // actual dangle found, return dangle energy from top + dangles on either left and right bottom 
    | _ , dangle, _   ->    makeExpression (topDangleEnergyConnectedViaTop opts mapping leftDoubleStranded rightDoubleStranded dangle 
                            |> energy_plus <| bottomDangleEnergyConnectedViaTop opts mapping leftDoubleStranded rightDoubleStranded bottomLeftDangle bottomRightDangle)
    


let joinsViaBottom  (opts:Options.t) (leftSegment: Segment.t) (rightSegment : Segment.t) (mapping: Sequence.mapping ) : energyExpression =
// two segments that are linked via bottom. Compute dangle and terminal terms, and if not dangling, compute nick energies.
    
    let leftDoubleStranded = Segment.double_stranded_region leftSegment in
    let rightDoubleStranded = Segment.double_stranded_region rightSegment in

    let bottomDangle  = Segment.bottom_right_overhang leftSegment @ Segment.bottom_left_overhang rightSegment in

    let topLeftDangle = Segment.top_right_overhang leftSegment in
    let topRightDangle = Segment.top_left_overhang rightSegment in

    match leftDoubleStranded, bottomDangle, rightDoubleStranded with
    // empty double stranded domain on the left, so return free dangles top and bottom
    | [], _ , _       ->    makeExpression (leftDangles opts mapping bottomDangle topRightDangle rightDoubleStranded)
    // empty double stranded domain on the right, so return free dangles top and bottom
    | _ , _ , []      ->    makeExpression (rightDangles opts mapping bottomDangle topLeftDangle leftDoubleStranded)
    // the dangle contains no domains, so we are dealing with a nick. Return nicking energy + any coaxial dangling term
    | _ , [], _       ->    coaxialDangleTop opts topLeftDangle topRightDangle |> energyExpression_div <| nickAtTop mapping leftDoubleStranded rightDoubleStranded 
    // actual dangle found, return dangle energy from top + dangles on either left and right bottom 
    | _ , dangle, _   ->    makeExpression (bottomDangleEnergyConnectedViaBottom opts mapping leftDoubleStranded rightDoubleStranded dangle 
                            |> energy_plus <| topDangleEnergyConnectedViaBottom opts mapping leftDoubleStranded rightDoubleStranded topLeftDangle topRightDangle)



let rec appendSequence (domainList : Segment.domain list) (mapping: Sequence.mapping) (output:dnabase list) = 
    
    match domainList with 
    | []            -> output
    | head::tail    -> output  @ (getDomain mapping  head) 
                       |> appendSequence tail mapping 


let NNTermsInSegment (segment:Segment.t) (mapping: Sequence.mapping) =
// takes the segment, returns summation of nearest neighbour energy terms

    let dsDomains = Segment.double_stranded_region segment in
    let sequence = appendSequence dsDomains mapping List.empty in
    freeEnergyProcessorOnlyNN empty sequence


let rec segmentIterator (opts: Options.t) (gate : Gate.t) (mapping: Sequence.mapping) (input: energyExpression) =    
// maps all segments in the gate to a free energy
    
    match gate with
    // Option 1. Final segment, and no other inner-list to follow up. The final segment is empty per contract.
    | [[_]]                     ->      input
    // Option 2. Final segment of inner-list with another inner-list available, so joins via top.
    | [head] :: other           ->      (input |> energyExpression_plus <| (joinsViaTop opts head (List.head (List.head other)) mapping |> energyExpression_div <| NNTermsInSegment head mapping))
                                        |> segmentIterator opts other mapping       
    // Option 3. Two segments exist in an innerlist, so they join via the bottom.
    | (head :: tail) :: other   ->      (input |> energyExpression_plus <| (joinsViaBottom opts head (List.head tail) mapping |> energyExpression_div <| NNTermsInSegment head mapping))
                                        |> segmentIterator opts (tail :: other) mapping 
    | _                         ->      failwith "should not reach - segment iterator" 


let initializeTerms (opts:Options.t) gate =   // init terms, one for each strand that is bound to the gate 
    
    let strandList = Species.melt (Species.GATE gate)   in
    let strandCount = List.length strandList  in

    (init opts) |> energy_rmul <| (float (strandCount-1))


let getGateEnergy (opts: Options.t) (mapping: Sequence.mapping) (gate : Gate.t) = 

    let output = makeExpression (initializeTerms opts gate) in
    
    let appendedGate = ([Segment.empty] :: gate) in
    let appendedGate = appendedGate @ [[Segment.empty]] in

    let output = segmentIterator opts appendedGate mapping output in

    output


let getSpeciesEnergy (opts:Options.t) (mapping: Sequence.mapping) (species : Species.t) =
    
  match species with
  | Species.GATE g -> getGateEnergy opts mapping g
  | Species.STRAND _ -> makeExpression (empty)
  | _ -> failwith "only gates supported for energy calculation"


