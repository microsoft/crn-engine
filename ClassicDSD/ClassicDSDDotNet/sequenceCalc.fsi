// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.SequenceCalc
open Microsoft.Research.DNA

open Microsoft.Research.DNA.Measures

type prim = Microsoft.Research.CRNEngine.Expression.t<string>
module Species = Microsoft.Research.DNA.Species

(* Simple function that takes in sequence and produces dH, dS, dG, G37, G60 and unbinding rate 
  as a function of binding sequence, binding rate, salt concentration(s) and temperature *)
    
type domain = Domain.t
type energy = { dH : float<kcal/mol>; dS : float<cal/(mol*K)> }(* with
    static member (+) : energy * energy -> energy
    static member (-) : energy * energy -> energy
    static member (*) : float * energy  -> energy 
    static member (*) : energy * float  -> energy 
*)

type energyExpression = { dHE : prim; dSE : prim }(* with
    static member (+)  : energyExpression * energyExpression ->  energyExpression
    static member (/)  : energyExpression * energy           ->  energyExpression //FD: This used to be ++ instead of /
    static member (-)  : energyExpression * energyExpression ->  energyExpression
    static member (*)  : float            * energyExpression ->  energyExpression
    static member (*)  : energyExpression * float            ->  energyExpression
*)
val energyExpression_plus : a:energyExpression -> b:energyExpression -> energyExpression
val energyExpression_div : b:energyExpression -> a:energy -> energyExpression
val energyExpression_minus : a:energyExpression -> b:energyExpression -> energyExpression
val energyExpression_rmul : a:energyExpression -> b:float -> energyExpression
val energyExpression_lmul : b:float -> a:energyExpression -> energyExpression

type gibbs = {dG : float<kcal/mol>}
val temp37C : float<K>
val temp25C : float<K>
val temp60C : float<K>

val tempCintoK : float<C> -> float<K>
val getEnergies : Sequence.dnabase ->  Sequence.dnabase -> energy
val freeEnergy :  Options.t ->  Sequence.t  -> energy
val g37 :  Options.t -> Sequence.t -> gibbs
val g60 :  Options.t -> Sequence.t -> gibbs
val getGateEnergy : Options.t -> Sequence.mapping -> Gate.t -> energyExpression
val getSpeciesEnergy : Options.t -> Sequence.mapping -> Species.t -> energyExpression
val unbindRate : float<K> -> float<s^-1> -> energy -> float</s>
val unbindRateExpression : float<K> -> prim -> energyExpression -> prim
//val unbindRateMaximizedExpression : float<K> -> prim -> energyExpression -> prim
val unbindRateFromSequence : float<K> -> float</s> -> string -> float</s>
val getDomain : Sequence.mapping -> Sequence.domain  -> Sequence.t
//val getDG : float<K> -> energyExpression -> prim
val gTemp : energy -> float<K> -> gibbs

