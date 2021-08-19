module Microsoft.Research.DNA.DsdParserTests

open FsUnit
open Xunit

open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open Parser

module TU  = TestUtils
module Lib = Microsoft.Research.CRNEngine.Lib

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - catalytic (single strand)")>]
let ``catalytic`` () =
  let catalytic = 
    "(13 * <2 3^ 4>)"
  Dsd.compile catalytic
    |> ignore

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - transmission lines")>]
let ``transmission lines`` () =
  let transmission_lines =
    """ directive polymers
        directive localconcentrations [ (a, 100000); (b, 100000) ]

        dom a0 = { colour = "red" }
        dom x  = { colour = "green" }
        dom y  = { colour = "blue" }
        dom r  = { colour = "purple" }
        dom Q  = { colour = "black" }
        dom F  = { colour = "black" }

        def input()    = <a0^ s>
        def fuel()     = <y^*>[s*]{x^>
        def probe()    = <r^*>[s*]<Q^>{F^}
        def origami()  = [[ {tether(a) a0^*}[s]{y^>
                          | {tether(a,b) x^*}[s]{y^>
                          | {tether(b) x^*}[s]{r^> ]]
        def reporter() = {s F^}

        ( input()
        | 2 of fuel()
        | probe()
        | origami()
        | 0 of reporter()
        )"""
  Dsd.compile transmission_lines
   |> ignore

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - overhangs")>]
let ``Parsing, overhangs`` () =
  let test_overhangs =
    """(<x>{y}[z] | [z]{y}<x> | <x>[z]{y} | <a>{b}[z]{y}<b>:<x>[z]{y} | <x>[z]{y}::<a>[z]{y}<b>)"""
  Dsd.compile test_overhangs
    |> ignore

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - dom")>]
let ``Parsing, dom`` () =    
  let test_dom_keyword =
    """ directive simulation {initial=0; final=7000}

        dom 3 = {bind=4.2E-4; unbind=4.0E-2}
        new 5@ 6.5E-4 , 4.0E-3

        ( 13 * <2 3^ 4>
        | 10 * <4 5^>
        | 10 * <1>[2]:<6>[3^ 4]{5^*}
        )"""
  Dsd.compile test_dom_keyword
    |> ignore

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - dom with spatial initials")>]
let ``Parsing, dom with spatial initials`` () =    
  let test_dom_keyword =
    """ ( <2 3^ 4>[5] with { value = 13; spatial =  { centralcore = { inner = 1.0; width = 0.1; outer = 0.0 } } })"""
  let result = Dsd.compile test_dom_keyword
  ignore result


[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - melting with non-trivial double segment")>]
let ``Melting with non-trivial double segment`` () =
    let s =
      "{x*}<x3^* z1>[z^* x1]<y*>{z3^ x2}::{y3^ z1}<x4^* y1>[y2]<z4*>{x*}"
    Dsd.compile s
    |> ignore

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - catalytic")>]
let ``simulation test`` () =
  let test = """directive simulation { 
      final = 7000.0; 
      points = 1000; 
      plots = [<2 3^ 4>; <4 5^>; <1 2>; <6 3^ 4>; <1>[2]:<6>[3^ 4]{5^*}];
    }
    directive deterministic { abstolerance = 1.0E-6 }
    directive stochastic {scale = 500.0}
    directive leak 1.0E-9 
    directive tau 0.1126 
    directive migrate 8000.0 
    directive lengths 6 20 

    new 3@ 4.2E-4 , 4.0E-2
    new 5@ 6.5E-4 , 4.0E-3
    ( 13 * <2 3^ 4>
    | 10 * <4 5^>
    | 10 * <1>[2]:<6>[3^ 4]{5^*}
    )
    """
  Dsd.compile test |> ignore

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - hairpins")>]
let ``hairpins`` () =
  let test = """
    directive localconcentrations [ (l, local) ]
directive polymers
directive parameters [
  ka=1;
  kx=1;
  ky=1;
  kr=1;
  krl=1;
  ky=1;
  kl=1;
  ro=1;
  kt = 1;
  NF = 1; NI=1;NR=1;
  local = 1000;
  ]


dom a = { colour = "blue"; bind = ka }
dom a0 = { colour = "blue"; bind = ka }
dom b0 = { colour = "blue"; bind = ka }
dom c0 = { colour = "blue"; bind = ka }
dom y  = { colour = "green"; bind = ky }
dom xs = { colour = "orange"; bind = kx*ro }
dom i  = { colour = "orange"}
dom x  = { colour = "orange"; bind = kx; subdomains = [xs;i]; }
dom r  = { colour = "red"; bind = kr }
dom th = { colour = "purple"; bind = kt }
dom Q  = { colour = "black" }
dom F  = { colour = "yellow" }

def Fuel() = {y^*}[s*]{i^* xs^*>
def HP2() = [[ <tether(l) a0^>[s]{y^> | {t*}<tether(l) xs^>[s]{r^> ]]
def Reporter() = {r^*}[s* t*]<Q^>{F^}
def Strand() = {a0^* s*}
( NF of Fuel()
| NI of HP2()
| NR of Reporter()
| 0 of {a0^* s*}
)

  """
  Dsd.compile test |> ignore

[<Fact(DisplayName="Classic DSD - Parsing - DNA Consensus network")>]
let ``DNA Consensus network`` () = 
  let code = "

directive compilation infinite (* detailed, infinite *)
directive declare
(*
directive sequenceRates
directive temperature 25.0
directive migrate mrate (* mrate, 2250000 *)
directive time h
directive concentration nM
*)

directive simulator sundials
directive deterministic {reltolerance = 1e-8 } 
directive inference { 
  burnin=5000; samples=10000; thin=5; seed=0; 
  noise_model=proportional; 
  partial=true; timer=false;
}
directive parameters [
  kt = 2.39907822407132, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};

(* Sequence Rates 
  mRepJoinBX = 94470.1172302799, {distribution = Uniform(10.0, 10000000.0); interval = Log; Variation = Fixed};
  mRepJoinBY = 52196.1191896697, {distribution = Uniform(10.0, 10000000.0); interval = Log; variation = Fixed};
  mRepJoinXY = 81043.0257126739, {distribution = Uniform(10.0, 10000000.0); interval = Log; variation = Fixed};
  mRepB = 389793.823309916, {distribution = Uniform(10.0, 10000000.0); interval = Log; variation = Fixed};
  mRepX = 2664367.04606196, {distribution = Uniform(10.0, 10000000.0); interval = Log; variation = Fixed};
  mRepFork2B = 200701.403947464, {distribution = Uniform(10.0, 10000000.0); interval = Log; variation = Fixed};
  mRepFork2X = 7943820.8433103, {distribution = Uniform(10.0, 10000000.0); interval = Log; variation = Fixed};
  mRepFork2Y = 93267.3816879742, {distribution = Uniform(10.0, 10000000.0); interval = Log; variation = Fixed};
  t0Rep_rxyu1 = 0.108296104031841, {distribution = Uniform(0.0833, 0.16211); interval = Log; variation = Random};
  t0Rep_rbxu1 = 0.112778476997811, {distribution = Uniform(0.0833, 0.15467); interval = Log; variation = Random};
  t0Rep_rbyu1 = 0.110360387975028, {distribution = Uniform(0.0833, 0.16094); interval = Log; variation = Random};
  t0Rep_u3pb = 0.083866882273023, {distribution = Uniform(0.0833, 0.15131); interval = Log; variation = Random};
  t0Rep_u3px = 0.0851138939412723, {distribution = Uniform(0.0833, 0.156); interval = Log; variation = Random};
  t0Rep_u3py = 0.0834042212173622, {distribution = Uniform(0.0833, 0.15036); interval = Log; variation = Random};
  t0Rep_tb = 0.0955030075982879, {distribution = Uniform(0.0833, 0.14628); interval = Log; variation = Random};
  t0Rep_tx = 0.107492594451185, {distribution = Uniform(0.0833, 0.13692); interval = Log; variation = Random};
  mrate = 6069.834764, {distribution = Uniform(1000.0,10000.0); interval = Log; variation = Fixed};
*)

(* Unique context *)
  mRepJoinBX = 0.330409478372343, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  mRepJoinBY = 0.195022555302614, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  mRepJoinXY = 0.28792982640174, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  mRepB = 0.936212548050403, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  mRepX = 1.89623194461531, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  mRepFork2B = 0.705992340255082, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  mRepFork2X = 2.33651032891356, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  mRepFork2Y = 0.390195181800155, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  t0Rep_rxyu1 = 0.107939489147304, {distribution = Uniform(0.0833, 0.16211); interval = Log; variation = Fixed};
  t0Rep_rbxu1 = 0.113303470294027, {distribution = Uniform(0.0833, 0.15467); interval = Log; variation = Fixed};
  t0Rep_rbyu1 = 0.111461253376227, {distribution = Uniform(0.0833, 0.16094); interval = Log; variation = Fixed};
  t0Rep_u3pb = 0.0836562120255087, {distribution = Uniform(0.0833, 0.15131); interval = Log; variation = Fixed};
  t0Rep_u3px = 0.0877810388094365, {distribution = Uniform(0.0833, 0.156); interval = Log; variation = Fixed};
  t0Rep_u3py = 0.0834781314697647, {distribution = Uniform(0.0833, 0.15036); interval = Log; variation = Fixed};
  t0Rep_tb = 0.0942529381496787, {distribution = Uniform(0.0833, 0.14628); interval = Log; variation = Fixed};
  t0Rep_tx = 0.105530280335228, {distribution = Uniform(0.0833, 0.13692); interval = Log; variation = Fixed};
  
  leakBX2X_J = 0.0, {distribution = Uniform(0.0, 0.3); interval = Real; variation = Random};
  leakBY2Y_J = 0.0, {distribution = Uniform(0.0, 0.3); interval = Real; variation = Random};
  
  kJ1 = 0.0204964555980685, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kJ1r = 0.585039224523331, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kJ2 = 1.42853444981608, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kJ2r = 0.237572367092801, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kJ3 = 1.48828869653376, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kJ3r = 0.0196729632011412, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kF1 = 0.079210535760631, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kF1r = 0.0651919573526697, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kF2 = 0.0788609544911219, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kF2r = 0.0651919573526697, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  kF3 = 0.102518024423917, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};
  kF3r = 0.0651919573526697, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  kF4 = 0.102518024423917, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  kF5 = 0.102518024423917, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Fixed};
  kF2Y1 = 0.0137493238867182, {distribution = Uniform(0.01, 100.0); interval = Log; variation = Random};

  kl = 2.51055199657701E-05, {distribution = Uniform(1.0E-8, 1.0E-3); interval = Log; variation = Fixed};

  (* Separate rate per fluorophore 
  mRepJoin =75892.794458, {distribution = Uniform(10.0,10000000.0); interval = Log; variation = Random};
  *)

  (* Detailed model
  ku1 = 1.0, {distribution = Uniform(0.01,100.0); interval = Log; variation = Random};
  ku2 = 1.0, {distribution = Uniform(0.01,100.0); interval = Log; variation = Random};
  ku3 = 1.0, {distribution = Uniform(0.01,100.0); interval = Log; variation = Random};
  ut = 100.0, {distribution = Uniform(100.0,10000.0); interval = Log; variation = Random};
  uu1 = 100.0, {distribution = Uniform(100.0,10000.0); interval = Log; variation = Random};
  uu2 = 100.0, {distribution = Uniform(100.0,10000.0); interval = Log; variation = Random};
  uu3 = 100.0, {distribution = Uniform(100.0,10000.0); interval = Log; variation = Random};
  *)

  (* Concentrations *)
  X  = 50.0; NG  = 75.0;  NH  = 100.0; NR  = 150.0; (* Default *)
  Xv = 40.0; NGv = 120.0; NHv = 120.0; NRv = 120.0; (* Reversibles *)
  Xh = 10.0; NGh = 15.0;  NHh = 20.0;  NRh = 30.0;  (* Halves *)
  Xi = 10.0; NGi = 30.0;  NHi = 30.0;  NRi = 30.0; NMi = 100.0; (* Intermediates *)
  Xr = 4.0; NRr = 12.0; (* Reporters *)
  (*Xm = 40.0; NGm = 80.0; NHm = 80.0; NRm = 120.0; *)
  Xm = 80.0; NGm = 160.0; NHm = 160.0; NRm = 240.0; (* Consensus *)
  N = 0.0; Nx = 0.0; Ny = 0.0;
]
(* Trying to constrain the fork rates *)
def kF2r = kF1r
def kF3r = kF1r
def kF4 = kF3
def kF5 = kF3

def kJBX1 = kJ1 def kJBX1r = kJ1r def kJBX2 = kJ2 def kJBX2r = kJ2r def kJBX3 = kJ3 def kJBX3r = kJ3r
def kJBY1 = kJ1 def kJBY1r = kJ1r def kJBY2 = kJ2 def kJBY2r = kJ2r def kJBY3 = kJ3 def kJBY3r = kJ3r
def kJXY1 = kJ1 def kJXY1r = kJ1r def kJXY2 = kJ2 def kJXY2r = kJ2r def kJXY3 = kJ3 def kJXY3r = kJ3r
def kF2B1 = kF1 def kF2B1r = kF1r def kF2B2 = kF2 def kF2B2r = kF2r def kF2B3 = kF3 def kF2B3r = kF3r def kF2B4 = kF4 def kF2B5 = kF5 
def kF2X1 = kF1 def kF2X1r = kF1r def kF2X2 = kF2 def kF2X2r = kF2r def kF2X3 = kF3 def kF2X3r = kF3r def kF2X4 = kF4 def kF2X5 = kF5 
                def kF2Y1r = kF1r def kF2Y2 = kF2 def kF2Y2r = kF2r def kF2Y3 = kF3 def kF2Y3r = kF3r def kF2Y4 = kF4 def kF2Y5 = kF5 
(*def kF2Y1 = kF1*) 

(* Separate rate for fluorophore
def mRepFork2B = mRepX
def mRepJoinBX = mRepJoin
def mRepJoinBY = mRepJoin
def mRepJoinXY = mRepJoin
*)

def ku1 = kt def ku2 = kt def ku3 = kt 
def ut =0.0 def uu1=0.0 def uu2 = 0.0 def uu3=0.0

(* Sequence Rates 
def kJBX1 = 0.0 def kJBX1r = 0.0 def kJBX2 = 0.0 def kJBX2r = 0.0 def kJBX3 = 0.0 def kJBX3r = 0.0
def kJBY1 = 0.0 def kJBY1r = 0.0 def kJBY2 = 0.0 def kJBY2r = 0.0 def kJBY3 = 0.0 def kJBY3r = 0.0
def kJXY1 = 0.0 def kJXY1r = 0.0 def kJXY2 = 0.0 def kJXY2r = 0.0 def kJXY3 = 0.0 def kJXY3r = 0.0
def kF2B1 = 0.0 def kF2B1r = 0.0 def kF2B2 = 0.0 def kF2B2r = 0.0 def kF2B3 = 0.0 def kF2B3r = 0.0 def kF2B4 = 0.0 def kF2B5 = 0.0
def kF2X1 = 0.0 def kF2X1r = 0.0 def kF2X2 = 0.0 def kF2X2r = 0.0 def kF2X3 = 0.0 def kF2X3r = 0.0 def kF2X4 = 0.0 def kF2X5 = 0.0 
def kF2Y1 = 0.0 def kF2Y1r = 0.0 def kF2Y2 = 0.0 def kF2Y2r = 0.0 def kF2Y3 = 0.0 def kF2Y3r = 0.0 def kF2Y4 = 0.0 def kF2Y5 = 0.0

*)

(* Base *)
new X0 new Y0

def t0JoinBX_B = 1.0 def leakJoinBX_B = 0.24322014  def badJoinBX_B = (6.0 + 0.32425231 - 5.9000668) / 6.0 
def t0JoinBX_X = 1.0 def leakJoinBX_X = 0.11946842  def badJoinBX_X = (6.0 + 0.24609909 - 6.5566584) / 6.0 (* negative *)
def t0JoinBX_H = 1.0 def leakJoinBX_H = 0.097396333 def badJoinBX_H = (6.0 + 0.108521 - 5.9101173) / 6.0 
def t0JoinBX_R1 = 0.5 def leakJoinBX_R1 = 0.5971527  def badJoinBX_R1 = (6.0 + 0.61825693 - 5.3644739) / 6.0 
def t0JoinBX_R2 = 0.5 def leakJoinBX_R2 = 0.78391903 def badJoinBX_R2 = (6.0 + 1.0986007 - 6.1368419) / 6.0 
def t0JoinBX_R3 = 0.5 def leakJoinBX_R3 = 0.79910888 def badJoinBX_R3 = (6.0 + 1.5157708 - 7.0041442) / 6.0 (* negative *)
def t0JoinBY_B = 1.0 def leakJoinBY_B = 0.48863012  def badJoinBY_B = (6.0 + 0.79579046 - 6.0939745) / 6.0 (* 15.9 *)
def t0JoinBY_Y = 1.0 def leakJoinBY_Y = 0.27052827  def badJoinBY_Y = (6.0 + 0.38219716 - 6.6560031) / 6.0 (* negative 4.9 *)
def t0JoinBY_H = 1.0 def leakJoinBY_H = 0.055916679 def badJoinBY_H = (6.0 + 0.038363162 - 7.3458785) / 6.0 (* negative 2.3 *)
def t0JoinBY_R1 = 0.5 def leakJoinBY_R1 = 0.75069688  def badJoinBY_R1 = (6.0 + 0.80157264 - 5.3179004) / 6.0 (* 2.5 *)
def t0JoinBY_R2 = 0.5 def leakJoinBY_R2 = 0.88612285 def badJoinBY_R2 = (6.0 + 1.3410298 - 6.3513315) / 6.0 (* 6.4 *) 
def t0JoinBY_R3 = 0.5 def leakJoinBY_R3 = 1.1365071 def badJoinBY_R3 = (6.0 + 2.0343225 - 5.3272129) / 6.0 (* 12.9 *)
def t0JoinXY_X = 1.0 def leakJoinXY_X = 1.6945078  def badJoinXY_X = (6.0 + 1.9423518 - 7.9032565) / 6.0 (* 12.5 *)
def t0JoinXY_Y = 1.0 def leakJoinXY_Y = 0.93308561  def badJoinXY_Y = (6.0 + 0.97638337 - 7.7032293) / 6.0 (* 3.5 *)
def t0JoinXY_H = 1.0 def leakJoinXY_H = 0.0 def badJoinXY_H = (6.0 + 0.0 - 5.4342962) / 6.0  (* 3.5 *)
def t0JoinXY_R1 = 0.5 def leakJoinXY_R1 = 0.3997306  def badJoinXY_R1 = (6.0 + 0.4573801 - 4.5074007) / 6.0 (* 2.1 *)
def t0JoinXY_R2 = 0.5 def leakJoinXY_R2 = 0.50340259 def badJoinXY_R2 = (6.0 + 0.85738814 - 4.7758843) / 6.0 (* 5.0 *) 
def t0JoinXY_R3 = 0.5 def leakJoinXY_R3 = 0.73484885 def badJoinXY_R3 = (6.0 + 1.4824177 - 4.896728) / 6.0 (* 12.6 *)
def t0Fork2X_R = 1.0 def leakFork2X_R = 0.470958  def badFork2X_R = (6.0 + 0.9155104 - 6.2511144) / 6.0 (* negative 14.9 *)
def t0Fork2X_H1 = 1.0 def leakFork2X_H1 = 0.24226597 def badFork2X_H1 = (6.0 + 0.42083542 - 5.9889533) / 6.0 (* 8.2 *)
def t0Fork2X_H2 = 1.0 def leakFork2X_H2 = 0.12073307  def badFork2X_H2 = (6.0 + 0.1512095 - 6.1596074) / 6.0 (* 6.1 *)
def t0Fork2B_R = 1.0 def leakFork2B_R = 0.77312019  def badFork2B_R = (6.0 + 0.850197 - 4.8604263) / 6.0 (* 4.4 *)
def t0Fork2B_H1 = 1.0 def leakFork2B_H1 = 0.94868054 def badFork2B_H1 = (6.0 + 1.1182619 - 5.3043056) / 6.0 (* 11.1 *)
def t0Fork2B_H2 = 1.0 def leakFork2B_H2 = 0.88826117 def badFork2B_H2 = (6.0 + 0.90927962 - 5.5840612) / 6.0 (* 4.4 *)
def t0Fork2Y_R = 0.0833 def leakFork2Y_R = 0.18573219 def badFork2Y_R = (6.0 + 0.40505528 - 5.8409924) / 6.0 (* 19.2 *)
def t0Fork2Y_H1 = 1.0 def leakFork2Y_H1 = 0.12224817 def badFork2Y_H1 = (6.0 + 0.18764882 - 5.0428647) / 6.0 (* 24.7 *)
def t0Fork2Y_H2 = 1.0 def leakFork2Y_H2 = 0.092309817  def badFork2Y_H2 = (6.0 + 0.0798445 - 5.2368405) / 6.0 (* 16.9 *)

def t0BX2X = 0.0833 def leakBX2X_F = 0.087 (*def leakBX2X_J = 0.0*) (* leakJoinBX_X*5.0 *) def badBX2X = 0.0 (*0.087 / 50.0*)
def t0BY2Y = 0.0833 def leakBY2Y_F = 0.96 (*def leakBY2Y_J = 0.0*) (* leakJoinBY_Y*5.0 *)  def badBY2Y = 0.0 (*0.96 / 50.0 *)
def t0XY2B = 0.0833 def leakXY2B_F = 1.195 def leakXY2B_J = 0.0 def badXY2B =  (30.0 + 3.5 - 30.35) / 30.0

def Input(ta,a) = <ta^ a>
def Catalyst(tb,b) = <tb^ b>
def Helper(tr,r) = <tr^ r>
def Output(tc,c) = <tc^ c>
def Translator(r,tq) = <r tq^>
def InputR(a,tb) = <a tb^>
def OutputR(c,tr) = <c tr^>
def CatalystR(b,tr) = <b tr^>
def Output1R(b,tr) = <b tr^>
def Output2R(c,tb) = <c tb^>
def Output3R(d,tc) = <d tc^>
def End(i,tc) = <i tc^>
def Signal_Fork(c,fl) = <c fl^>
def Signal_Join(fl,r) = <fl^ r>
def Rep_Fork(tc,c,fl) = {tc^*}[c]<fl^>
def Rep_Join(fl,r,tq) = <fl^>[r]{tq^*}
def Join_AB(ta,a,tb,b,tr,r,tq) = {ta^*}[a tb^]:[b tr^]:[r tq^]
def Join_AB1(ta,a,tb,b,tr,r,tq) = [ta^ a]:{tb^*}[b tr^]:[r tq^]
def Join_AB2(ta,a,tb,b,tr,r,tq) = [ta^ a]:[tb^ b]:{tr^*}[r tq^]
def Join_AB3(ta,a,tb,b,tr,r,tq) = [ta^ a]:[tb^ b]:[tr^ r]{tq^*}
def Fork_C(i,tc,c,tr,r,tq) = [i]:[tc^ c]:[tr^ r]{tq^*}
def Fork_BC(i,tc,c,tb,b,tr,r,tq) = [i]:[tc^ c]:[tb^ b]:[tr^ r]{tq^*}
def Fork_BCD(i,td,d,tc,c,tb,b,tr,r,tq) = [i]:[td^ d]:[tc^ c]:[tb^ b]:[tr^ r]{tq^*}
def Fork_BCD1(i,td,d,tc,c,tb,b,tr,r,tq) = [i]:[td^ d]:[tc^ c]:[tb^ b]:{tr^*}[r tq^]
def Fork_BCD2(i,td,d,tc,c,tb,b,tr,r,tq) = [i]:[td^ d]:[tc^ c]:{tb^*}[b tr^]:[r tq^]
def Fork_BCD3(i,td,d,tc,c,tb,b,tr,r,tq) = [i]:[td^ d]:{tc^*}[c tb^]:[b tr^]:[r tq^]
def Fork_BCD4(i,td,d,tc,c,tb,b,tr,r,tq) = [i]:{td^*}[d tc^]:[c tb^]:[b tr^]:[r tq^]
def Fork_BCD5(i,td,d,tc,c,tb,b,tr,r,tq) = [i td^]:[d tc^]:[c tb^]:[b tr^]:[r tq^]

(* Module definitions *)
def Join(NA,TA,NB,TB,NH,TH,NG,NV,Leak,ta,a,tb,b,tr,r,tq,kJ1,kJ1r,kJ2,kJ2r,kJ3,kJ3r) =
( 1.0 * (NG-Leak) * Join_AB(ta,a,tb,b,tr,r,tq)
| Leak * Translator(r,tq)
| NA * Input(ta,a) @ TA 
| NB * Catalyst(tb,b) @ TB 
| NH * Helper(tr,r) @ TH
| NV * InputR(a,tb)
| NV * CatalystR(b,tr)
(* Slow leak 
| rxn [ta^ a]:[tb^ b]:[tr^ r]{tq^*} + <a tb^> ->{kl} [ta^]<a>:[a]<tb^>:[tb^ b]:[tr^ r]{tq^*}
*)
(*
| rxn [ta^ a]:[tb^ b]:[tr^ r]{tq^*} + <a tb^> <->{kl,kJ1} {ta^*}[a]<tb^>:[tb^ b]:[tr^ r]{tq^*} + <ta^ a>
| rxn {ta^*}[a]<tb^>:[tb^ b]:[tr^ r]{tq^*} + <r tq^> <->{kJ3r,kJ3} {ta^*}[a]<tb^>:[tb^ b]:{tr^*}[r tq^] + <tr^ r>
| rxn {ta^*}[a]<tb^>:[tb^ b]:{tr^*}[r tq^] + <ta^ a> ->{kJ1} [ta^ a]:[tb^ b]:{tr^*}[r tq^] + <a tb^>
| rxn {ta^*}[a]<tb^>:[tb^ b]:{tr^*}[r tq^] + <b tr^> ->{kJ2r} {ta^*}[a tb^]:[b tr^]:[r tq^] + <tb^ b>
*)
(* Unique Context *)
| rxn <ta^ a> + Join_AB(ta,a,tb,b,tr,r,tq) <->{kJ1,kJ1r} <a tb^> + Join_AB1(ta,a,tb,b,tr,r,tq)
| rxn <tb^ b> + Join_AB1(ta,a,tb,b,tr,r,tq) <->{kJ2,kJ2r} <b tr^> + Join_AB2(ta,a,tb,b,tr,r,tq)
| rxn <tr^ r> + Join_AB2(ta,a,tb,b,tr,r,tq) <->{kJ3,kJ3r} <r tq^> + Join_AB3(ta,a,tb,b,tr,r,tq)
)
def JoinRev(NA,TA,NB,TB,NH,TH,NG,NV,Leak,ta,a,tb,b,tr,r,tq,kJ1,kJ1r,kJ2,kJ2r,kJ3,kJ3r) =
( 1.0 * (NG-Leak) * Join_AB3(ta,a,tb,b,tr,r,tq)
| Leak * Input(ta,a)
| NA * Translator(r,tq) @ TA
| NB * CatalystR(b,tr) @ TB
| NH * InputR(a,tb) @ TH
| NV * Helper(tr,r)
| NV * Catalyst(tb,b)
(* Slow leak 
| rxn [ta^ a]:[tb^ b]:[tr^ r]{tq^*} + <a tb^> ->{kl} [ta^]<a>:[a]<tb^>:[tb^ b]:[tr^ r]{tq^*}
*)
(*
| rxn [ta^ a]:[tb^ b]:[tr^ r]{tq^*} + <a tb^> <->{kl,kJ1} {ta^*}[a]<tb^>:[tb^ b]:[tr^ r]{tq^*} + <ta^ a>
| rxn {ta^*}[a]<tb^>:[tb^ b]:[tr^ r]{tq^*} + <r tq^> <->{kJ3r,kJ3} {ta^*}[a]<tb^>:[tb^ b]:{tr^*}[r tq^] + <tr^ r>
| rxn {ta^*}[a]<tb^>:[tb^ b]:{tr^*}[r tq^] + <ta^ a> ->{kJ1} [ta^ a]:[tb^ b]:{tr^*}[r tq^] + <a tb^>
| rxn {ta^*}[a]<tb^>:[tb^ b]:{tr^*}[r tq^] + <b tr^> ->{kJ2r} {ta^*}[a tb^]:[b tr^]:[r tq^] + <tb^ b>
*)
(* Unique Context *)
| rxn <ta^ a> + Join_AB(ta,a,tb,b,tr,r,tq) <->{kJ1,kJ1r} <a tb^> + Join_AB1(ta,a,tb,b,tr,r,tq)
| rxn <tb^ b> + Join_AB1(ta,a,tb,b,tr,r,tq) <->{kJ2,kJ2r} <b tr^> + Join_AB2(ta,a,tb,b,tr,r,tq)
| rxn <tr^ r> + Join_AB2(ta,a,tb,b,tr,r,tq) <->{kJ3,kJ3r} <r tq^> + Join_AB3(ta,a,tb,b,tr,r,tq)
)
def Fork3(N,T,NH1,T1,NH2,T2,NH3,NH4,NG,Leak,i,td,d,tc,c,tb,b,tr,r,tq,kF1,kF1r,kF2,kF2r,kF3,kF3r,kF4,kF5) =
( 1.0 * (NG-Leak) * Fork_BCD(i,td,d,tc,c,tb,b,tr,r,tq)
(* Alternative to reporter leak
| Leak * Output(tc,c)
| Leak * Output(td,d)
*)
| N * Translator(r,tq) @ T 
| NH1 * Output1R(b,tr) @ T1 
| NH2 * Output2R(c,tb) @ T2 
| NH3 * Output3R(d,tc)
| NH4 * End(i,td)
| 0 * Helper(tr,r)
(* Unique Context *)
| rxn <r tq^> + Fork_BCD(i,td,d,tc,c,tb,b,tr,r,tq) <->{kF1,kF1r} <tr^ r> + Fork_BCD1(i,td,d,tc,c,tb,b,tr,r,tq)
| rxn <b tr^> + Fork_BCD1(i,td,d,tc,c,tb,b,tr,r,tq) <->{kF2,kF2r} <tb^ b> + Fork_BCD2(i,td,d,tc,c,tb,b,tr,r,tq)
| rxn <c tb^> + Fork_BCD2(i,td,d,tc,c,tb,b,tr,r,tq) <->{kF3,kF3r} <tc^ c> + Fork_BCD3(i,td,d,tc,c,tb,b,tr,r,tq)
| rxn <d tc^> + Fork_BCD3(i,td,d,tc,c,tb,b,tr,r,tq) <->{kF4,kF2r} <td^ d> + Fork_BCD4(i,td,d,tc,c,tb,b,tr,r,tq)
| rxn <i td^> + Fork_BCD4(i,td,d,tc,c,tb,b,tr,r,tq) ->{kF5} Fork_BCD5(i,td,d,tc,c,tb,b,tr,r,tq) + <i>
)
def RepJoin(N,T,NR,r,t,f,km) =
( N * Translator(r,t) @ T 
| NR * Rep_Join(f,r,t)
| 0 * Signal_Join(f,r)
| rxn Translator(r,t) + Rep_Join(f,r,t) ->{km} Signal_Join(f,r) + [r t^]
(*| rxn <f^>[r]:<r>[t^] ->{km} Signal_Join(f,r) + [r t^]*)
)
def RepFork(N,T,NR,t,c,f,km) =
( N * Output(t,c) @ T
| NR * Rep_Fork(t,c,f)
| 0 * Signal_Fork(c,f)
| rxn Output(t,c) + Rep_Fork(t,c,f) ->{km} Signal_Fork(c,f) + [t^ c]
(*| rxn [t^]<c>:[c]<f^> ->{km} Signal_Fork(c,f) + [t^ c]*)
)

dom t = {seq = CTGATC; bind=kt; unbind=ut; colour=\"red\"}
dom u1 = {seq = CTTCAG; bind=ku1; unbind=uu1; colour=\"orange\"}
dom u2 = {seq = CCATAC; bind=ku2; unbind=uu2; colour=\"blue\"}
dom u3 = {seq = ATACCC; bind=ku3; unbind=uu3; colour=\"green\"}
dom x = {seq = CATTGCTTTATTTACCGAGTCTTAT}
dom y = {seq = CATTGCCTAACCCACCGAGTCCTTT}
dom b = {seq = CATTGCCAATTCCTACGAGTCTACC}
dom pb = {seq = CATTGCATTATATTCCGAGTCCTAC}
dom px = {seq = CATTGCCTTCCCACTAGAGTCTCAC}
dom py = {seq = CATTGCACCACCCTAAGAGTCTAAC}
dom rxy = {seq = CATTGCTACCACCTCCGAGTCTAAC}
dom rbx = {seq = CATTGCCAAACCATTAGAGTCAAAC}
dom rby = {seq = CATTGCACCCTAATACGAGTCTCAC}
dom ig = {seq = CTGAAATAAATAAATAGAGTCTACC}
dom FAM = {seq = T; bind=0.0; unbind = 0.0; colour = \"yellow\"}
dom TYE665 = {seq = T; bind=0.0; unbind = 0.0; colour = \"yellow\"}
dom ROX = {seq = T; bind=0.0; unbind = 0.0; colour = \"yellow\"}
dom ALEX488 = {seq = T; bind=0.0; unbind = 0.0; colour = \"yellow\"}
dom TAMRA = {seq = T; bind=0.0; unbind = 0.0; colour = \"yellow\"}
dom ALEX647 = {seq = T; bind=0.0; unbind = 0.0; colour = \"yellow\"}

(* Circuits that have been measured 
system Rep_rxyu1 = {
  directive sweeps [s = [N = [0.2,0.4,0.6,0.8]]]
  directive simulation {final = 9.9; plots = [Signal_JoinXY()]}
  directive data [Rep_rxyu1]
  RepJoinXY(N*Xr,t0Rep_rxyu1,4.0*Xr)
}
*)

new X
<X>
  "
  Dsd.compile code |> ignore

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - duplicate initials with time")>]
let duplicateInitialsWithTime () =
  let code = 
    "def Translator(r,tq) = <r tq^>
( Translator(r,t) @ 1.0
| Translator(r,t))"
  let crn = Dsd.compile code
  Assert.Equal(2, crn.initials.Length)



[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - spatial join circuit")>]
let initialsEqualsSyntax () =
  let code = """
directive simulation { final = 600; plots = [Signal()]; }
directive simulator pde
directive spatial { 
  boundary = ZeroFlux; 
  dimensions = 2;
  diffusibles = [Input1() = 0.5; Input2() = 0.5]; 
  xmax = 50; 
  nx = 101; 
  dt = 1;
}
directive parameters [ 
  k = 0.003;
  u = 0.1; 
]
directive compilation infinite
dom tb = {bind = k; unbind = u; colour = "red"}
dom tx = {bind = k; unbind = u; colour = "green"}
dom to = {bind = k; unbind = u; colour = "blue"}
def Input1() = <tb^ b>
def Input2() = <tx^ x>
def Output() = <x to^>
def Join() = {tb^*}[b tx^]:[x to^]
def Reporter() = <fl^>[x]{to^*}
def Signal() = <fl^ x>
( Input1() = { spatial = { points = [ {x=0.3; y=0.3; width=0.4; value=10.0}] } }
| Input2() = { spatial = { points = [ {x=0.7; y=0.7; width=0.4; value=10.0}] } }
| 0 Output()
| 100 Join()
| 100 Reporter()
| 0 Signal()
)"""
  let crn = Dsd.compile code
  let debug = crn.to_string ()
  ()




[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - change settings after parsing")>]
let changeSettingsAfterParsing () =
  let code = """
directive simulation {
  final = 600; 
  plots = [Input1(); Input2(); Output(); Signal()];
}
directive simulator deterministic
directive parameters [ 
  k = 0.003;
  u = 0.1; 
]
directive compilation infinite
dom tb = {bind = k; unbind = u; colour = "red"}
dom tx = {bind = k; unbind = u; colour = "green"}
dom to = {bind = k; unbind = u; colour = "blue"}
def Input1() = <tb^ b>
def Input2() = <tx^ x>
def Output() = <x to^>
def Join() = {tb^*}[b tx^]:[x to^]
def Reporter() = <fl^>[x]{to^*}
def Signal() = <fl^ x>
( 10 Input1()
| 10 Input2()
| 0 Output()
| 100 Join()
| 100 Reporter()
| 0 Signal()
)"""
  let bundle = Dsd.parse code
  let modelWithInfiniteRules = Dsd.convert_expand bundle
  if modelWithInfiniteRules.reactions.Length <> 3 then failwith "unexpected number of reactions with infinite rules"
  let options = Dsd.get_options bundle
  let options = {options with rules = Options.Detailed}
  let bundle = Dsd.set_options bundle options
  let modelWithDetailedRules = Dsd.convert_expand bundle
  if modelWithDetailedRules.reactions.Length <> 8 then failwith "unexpected number of reactions with detailed rules"
  ()


[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - species names assignment")>]
let speciesNamesAssignment () =
  let code = """
directive simulation { final=30000; points=1000; plots = [<a^ A>; <b^ B>; <c^ C>; <d^ D>; < _ h^ X>; <k^ W>; <4 5^ 2>] }
directive simulator stochastic
directive compilation infinite
( 0 * <a^ A>  (* input A *)
| 0 * <b^ B>  (* input B *)
| 10 * <c^ C>   (* input C *)
| 95 * <d^ D>  (* input D *)
| 1 * {a^*}[A i^]<Y>
| 1 * {b^*}[B j^]<Z>
| 1 * {j^*}[Z i^]:[Y h^]<X> 
| 1 * {c^*}[C h^]<X>
| 200 * {d^*}[D k^]:[W]
| 100 * <k^ W>
| 200 * [D]{k^*}
| 1 * {h^*}[X k^]:[W]<1^>
| 10 * <X k^>
| 1 * <4>[5^ W]{1^*}
| 1300 * <5^ 2 3^ 4>
| 1000 * <4>[5^ 2]:<6>[3^ 4]{5^*}
)
"""
  let bundle = Dsd.parse code
  let unexpanded = Dsd.convert_unexpanded bundle
  let name_of_4_5_2_from_unexpanded = Map.findKey (fun _ (att:Attributes) -> att.structure = "<4 5^ 2>") unexpanded.attributes
  let expanded = Dsd.convert_expand bundle
  let name_of_4_5_2_from_expanded = Map.findKey (fun _ (att:Attributes) -> att.structure = "<4 5^ 2>") expanded.attributes
  if name_of_4_5_2_from_expanded <> name_of_4_5_2_from_unexpanded then failwith (sprintf "name of <4 5^ 2> from unexpanded = %s; from expanded = %s" name_of_4_5_2_from_unexpanded name_of_4_5_2_from_expanded)
  ()


[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - empty plot list")>]
let emptyPlotList () =
  let code = """
def Input1() = <tb^ b>
def Input2() = <tx^ x>
def Output() = <x to^>
def Join() = {tb^*}[b tx^]:[x to^]
def Reporter() = <fl^>[x]{to^*}
def Signal() = <fl^ x>
( 10 Input1() | 10 Input2() | 0 Output() | 100 Join() | 100 Reporter() | 0 Signal() )
"""
  // Expected behavior: the unexpanded model should have an empty plot list; the expanded model should have a full plot list.
  let bundle = Dsd.parse code
  let unexpanded = Dsd.convert_unexpanded bundle
  if unexpanded.settings.simulation.plots.Length <> 0 then failwith "non-empty plot list in unexpanded"
  let expanded = Dsd.convert_expand bundle
  if expanded.settings.simulation.plots.Length <> 12 then failwith "unexpected plot list length in expanded"
  ()

[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing expression with a sum")>]
let summationPlot () =
  let code = """
directive compilation infinite
directive simulation {final=3600.0; plots=[[<F^ x2>]+Y0]}
directive parameters [Y0 = 1.0]

( 30 * [b]{t^*}:[x1 t^]:[x2 t^]:[x2]:[c]
| 30 * <F^>[x2]{t^*}
| 100 * <t^ x1>
| 100 * <t^ x2>
)
"""
  // Expected behavior: both the unexpanded and expanded models should have a single plot, which is the sum of a species and a parameter
  let bundle = Dsd.parse code
  let term = Dsd.convert_unexpanded bundle
  if term.settings.simulation.plots.Length <> 1 then failwith "Did not identify a single plot expression"
  let expanded = Dsd.convert_expand bundle
  if expanded.settings.simulation.plots.Length <> 1 then failwith "Incorrect expansion"
  ()


[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Parsing - generated species names in CTMC")>]
let generatedSpeciesNamesInCTMC () =
  let code = """
directive simulation {initial=0; final=18000; points=1000; plots=[<C>]}
directive simulator cme
directive parameters [
  in1 = 0, {interval=Real; distribution=Uniform(0,0); variation=Fixed};
  in2 = 0, {interval=Real; distribution=Uniform(0,0); variation=Fixed};
  c0 = 50, {interval=Real; distribution=Uniform(50,50); variation=Fixed};
]
directive sweeps [
  mysweep = [in1 = [0; 1]; in2 = [0; 1]];
]
directive compilation infinite
directive tau 1
def bind = 5E-05
def unbind = 26.0
def STRAND(T, s) = <T^ s>
def OR(Ti1, Input1, Ti2, Input2, To, Output) = 
  (( {Ti1^*}[Input1 To^]<Output>
  | {Ti2^*}[Input2 To^]<Output>))
def REPORTER(Ti, Input) = {Ti^*}[Input]
new T  @ (c0 * bind), unbind
( in1 * STRAND(T,A)
| in2 * STRAND(T,B)
| OR(T,A,T,B,T,C)
| REPORTER(T,C))
"""
  let bundle = Dsd.parse code
  let expanded = Dsd.convert_expand bundle
  // Verify that the initial species list does not contain duplicate names.
  let names = List.map (fun (i:Initial<Species,Value>) -> i.species.name) expanded.initials
  Assert.Equal(names, (Seq.distinct names))
  ()