// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.CRNTests

open Xunit
open FsUnit
open Microsoft.Research.CRNEngine

let mhc () = """
//directive simulation { initial=-60000.0; final=7200.0; points=30000; plots=[NeP1;NeP2;NeP3]}
//directive simulation { initial=-60000.0; final=7200.0; points=30000}
directive simulation { initial=-60000.0; final=7200.0; points=30000; plots=[N;TN;NP1;NP2;NP3;NeP1;NeP2;NeP3]}
directive simulator sundials
directive deterministic {stiff=true}
directive parameters [
  dMe = 9.329349e-5;
  b = 3.177334e-11;
  c = 8.302928e-8;
  g1 = 20930.81;
  g2 = 17592.64;
  g3 = 10639.4;
  dP = 0.13;
  u1 = 0.000876407;
  u2 = 5.658359e-6;
  u3 = 4.176587e-7;
  uT = 1.184643e-6;
  vT = 0.0011091974705091;
  bT = 1.662768e-9;
  gT = 1505;
  dT = 0.001725968;
  e = 0.1141804;
  gM = 150.5;
  dM = 7.9892e-5;
  q = 21035;
]

module Pep(Pi,M,TM,MPi,TMPi,MePi,Me,ui) = 
{ M + Pi ->{b} MPi |
  MPi ->{ui} M + Pi |
  TM + Pi ->{c} TMPi |
  TMPi ->{ui*q} TM + Pi |
  TMPi ->{vT} T + MPi |
  MPi ->{e} MePi |
  MePi ->{ui} Me |
  Me ->{dMe} 
}

init radio 1 @ -10.0 |
init radio -1 @ 0.0 |

->{gT} T | T ->{dT} |
->[gM*(1.0-[radio])] M | M ->{dM} |
->[gM*[radio]] N | N ->{dM} |
T + M ->{bT} TM |
T + N ->{bT} TN |
TM ->{uT} T + M |
TN ->{uT} T + N |

<->{g1}{dP} P1 |
<->{g2}{dP} P2 |
<->{g3}{dP} P3 |

Pep(P1,M,TM,MP1,TMP1,MeP1,Me,u1) |
Pep(P2,M,TM,MP2,TMP2,MeP2,Me,u2) |
Pep(P3,M,TM,MP3,TMP3,MeP3,Me,u3) |
Pep(P1,N,TN,NP1,TNP1,NeP1,Ne,u1) |
Pep(P2,N,TN,NP2,TNP2,NeP2,Ne,u2) |
Pep(P3,N,TN,NP3,TNP3,NeP3,Ne,u3)"""
 
[<Fact(DisplayName="CRN - Modules - MHC")>]
let modules_MHC () = 
  let crn = mhc () |> Crn.from_string
  let result = crn.simulate_case () //|> ignore
  ()

[<Fact(DisplayName="CRN - compare simulate_sundials_single and simulate_oslo_single, degradation example")>]
let compare_simulate_sundials_single_and_simulate_oslo_single_degradation_example () =
  Lib.check64bit()
  let crn:Crn = Microsoft.Research.CRNEngine.Tests.CrnTest.degradation_example ()
  let times = [0.1; 0.2; 0.3]
  let crn:Crn = {crn with settings = {crn.settings with simulation = {crn.settings.simulation with times = times}}}
  let oslo_table = crn.to_oslo().simulate()
  let oslo_column = Table.find_column "X" oslo_table
  let sundials_table = crn.to_sundials().simulate()
  let sundials_column = Table.find_column "X" sundials_table
  let equalLists = Microsoft.Research.CRNEngine.Tests.CrnTest.equal_list 0.1 oslo_column.values sundials_column.values
  Assert.True(equalLists)

[<Fact(DisplayName="CRN - simulate_sundials_single, activation example")>]
let simulate_sundials_single_activation_example () =
  Lib.check64bit()
  let crn:Crn = Microsoft.Research.CRNEngine.Tests.CrnTest.activation_example ()
  let table:Table<float> = crn.to_sundials().simulate()
  let final:float = Table.find_column_last "X" table
  Assert.Equal(76.0, final, 1)

[<Fact(DisplayName="CRN - simulate_sundials_single, degradation example")>]
let simulate_sundials_single_degradation_example () =
  Lib.check64bit()
  let crn:Crn = Microsoft.Research.CRNEngine.Tests.CrnTest.degradation_example ()
  let table:Table<float> = crn.to_sundials().simulate()
  let final:float = Table.find_column_last "X" table
  Assert.Equal(50.0, final,1)