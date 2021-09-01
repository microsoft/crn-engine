// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.Examples

open Microsoft.Research.Biology.StabilityZ3
//open Microsoft.Msagl.GraphViewerGdi

open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Expression

let brusselator () = 
    "directive spatial { diffusibles=[X=1.0; Y=DY] }
directive parameters [
    A=1.0, {variation=Random; distribution=Uniform(1e-1,1e1); interval=Log};
    B=1.0, {variation=Random; distribution=Uniform(1e-1,1e1); interval=Log};
    //s=1.0, {variation=Random};
    f1=1.0;//, {variation=Random};
    f2=1.0;//, {variation=Random};
    DY=7.0
]

init X 1 |
init Y 1 |

->{A} X |
X ->{1.0} |
X ->{B} Y |
X + X <->{1.0}{f1} Z | 
Z + Y ->{f2} Z + X"
    |> Crn.from_string
    |> Dynamical.fromCRN 

let extended_brusselator () = 
    "directive spatial { diffusibles=[X=0.01; Y=0.1; Z=1] }
    directive parameters [
        a=1.0, {variation=Random; distribution=Uniform(0.92,1.08); interval=Real};
        b=1.0, {variation=Random; distribution=Uniform(2.8,3.2); interval=Real};
        c=1.0;
        d=1.0
    ]

    ->{a} X |
    X ->{1.0} |
    X ->{b} Y |
    X + X + Y ->{1.0} 3X |
    X <->{c}{d} Z"
    |> Crn.from_string
    |> Dynamical.fromCRN 

// Schnakenberg example
let schnakenberg =     
    let a = Key("a")
    let b = Key("b")
    let u = Key("u")
    let v = Key("v")  
    
    let vu2 = v * u ** 2.0
    let odes = 
        [| "u", a - u + vu2
        ; "v", b - vu2
        |]        
    
    Dynamical.Create(odes, ["u"; "v"])


let gierermeinhardt =
    let a = Key("a")
    let b = Key("b")
    let c = Key("c")
    let d = Key("d")
    let u = Key("u")
    let v = Key("v")  

    let au2 = a * u**2.0
    let odes = 
        [| "u", au2 / v - b*u + d
        ; "v", au2 - c * v
        |]
    
    Dynamical.Create(odes, ["u"; "v"])


let gierermeinhardt_param1 =
  [ "a", 0.3
  ; "b", 0.1
  ; "c", 0.3 
  ; "d", 0.1
  ]
  |> Map.ofSeq



let one = Float 1.0


// Hill functions
let hillA x (n:NumExpr) (eps:NumExpr) =   
  let xn = x ** n
  (xn + eps) / ( xn +  one)
    
let hillI (x:NumExpr) (n:NumExpr) (eps:NumExpr) =   
  let xn = x ** n
  (eps * xn + one) / (xn + one)  


//Alpha system: all parameters are symbolic
let alpha0 = 
    let i  = Key("i")
    let a  = Key("a")
    let c6  = Key("c6")
    let c12 = Key("c12")

    
    let kA  = Key("kA")
    let k6  = Key("k6")
    let k12 = Key("k12")
    let dC6 = Key("dC6")
    let dC12 = Key("dC12")
    let dA   = Key("dA")
    let dI   = Key("dI")

    
    let n   = Key("n")
    let nA   =Key("nA")
    let e6  = Key("e6")
    let e12  = Key("e12")
    let eI  = Key("eI")    

    let odes = 
        [|"a",   kA * (hillA c6 n e6) - dA*a
        ; "i",   (hillA a nA eI) * (hillI c12 n e12) - dI*i
        ; "c6",  k6*i - dC6*c6
        ; "c12", k12*i - dC12*c12
        |]        
            
    Dynamical.Create(odes, ["c6"; "c12"])

//Alpha system: some parameters have been instantiated
let alpha1 = 
    let i  = Key("i")
    let a  = Key("a")
    let c6  = Key("c6")
    let c12 = Key("c12")

    
    let kA  = Key("kA")
    let k6  = Key("k6")
    let k12 = Key("k12")
    let dC6 = Key("dC6")
    let dC12 = Key("dC12")
    let dA   = Key("dA")
    let dI   = Key("dI")

    
    let n   = Float 1.0
    let nA  = Float 2.0
    let e6  = Float 0.0
    let e12 = Float 0.0
    let eI  = Float 0.0    

    let odes = 
        [|"a",   kA * (hillA c6 n e6) - dA*a
        ; "i",   (hillA a nA eI) * (hillI c12 n e12) - dI*i
        ; "c6",  k6*i - dC6*c6
        ; "c12", k12*i - dC12*c12
        |]        
            
    Dynamical.Create(odes, ["c6"; "c12"])


//Alpha system: all parameters have been instantiated
let alpha2 = 
    let i  = Key("i")
    let a  = Key("a")
    let c6  = Key("c6")
    let c12 = Key("c12")
    
    let kA   = Float 17.07
    let k6   = Float 0.1947
    let k12  = Float 62.7
    let dC6  = Float 52.35
    let dC12 = Float 3.421
    let dA   = Float 0.1286
    let dI   = Float 0.01013
    
    let n   = Float 1.0
    let nA  = Float 2.0
    let e6  = Float 0.0
    let e12 = Float 0.0
    let eI  = Float 0.0    

    let odes = 
        [|"a",   kA * (hillA c6 n e6) - dA*a
        ; "i",   (hillA a nA eI) * (hillI c12 n e12) - dI*i
        ; "c6",  k6*i - dC6*c6
        ; "c12", k12*i - dC12*c12
        |]        
            
    Dynamical.Create(odes, ["c6"; "c12"])



let alpha_param1 = 
   [ "n",   1.0
   ; "nA",  2.0
   ; "e6",  0.0
   ; "e12", 0.0
   ; "eI",  0.0
   ]
   |> Map.ofSeq

let alpha_param2 = 
   [ "kA",   17.07
   ; "k6",   0.1947
   ; "k12",  62.7
   ; "dC6",  52.35
   ; "dC12", 3.421
   ; "dA",   0.1286
   ; "dI",   0.01013
   ; "n",   1.0
   ; "nA",  2.0
   ; "e6",  0.0
   ; "e12", 0.0
   ; "eI",  0.0
   ]
   |> Map.ofSeq




//Orthogonal signalling channels model
let rC(x6:NumExpr, x12:NumExpr, n:NumExpr) =     
    (x6**n + x12**n) / (1.0 + x6 + x12)**n

let f(a0:NumExpr, fr:NumExpr, fs:NumExpr) =
    let a1R = Float 18.47
    let a1S = Float 8.24
    (a0 + a1R*fr + a1S*fs) / (1.0 + fr + fs)

let orthogonal(r:NumExpr, s:NumExpr, c6:NumExpr, c12:NumExpr, n:NumExpr) =         
    let a0_76 = Float 0.086
    let a0_81 = Float 0.264
    let kGR76 = Float 8.657e-2
    let kGS76 = Float 4.788e-4
    let kGR81 = Float 3.329e-3
    let kGS81 = Float 4.249e-1
    let KR6   = Float 2.076e-4
    let KS6   = Float 1.71e-8
    let KR12  = Float 4.937e-7
    let KS12  = Float 8.827e-3
    
    let luxR_C = rC(c6*KR6, c12*KR12, n)
    let lasR_C = rC(c6*KS6, c12*KS12, n)
    //let f(a0:NumExpr,kGR:NumExpr,kGS:NumExpr) = (a0 + a1R*kGR*r**2.0*luxR_C + a1S*kGS*s**2.0*lasR_C) / (1.0 + kGR*r**2.0*luxR_C + kGS*s**2.0*lasR_C)
    
    let f76 = f(a0_76,kGR76*r**2.0*luxR_C,kGS76*s**2.0*lasR_C)
    let f81 = f(a0_81,kGR81*r**2.0*luxR_C,kGS81*s**2.0*lasR_C)
    (f76, f81)


let betaCrnTransport () = 
    "directive simulation { final=1.0; points=1000; plots=[X] }
directive parameters [
	aR   =1, {variation = Random}; dR   =1, {variation = Random};
	aS   =1, {variation = Random}; dS   =1, {variation = Random};
	bR6  =1, {variation = Random}; uR6  =1, {variation = Random};
	bS12 =1, {variation = Random}; uS12 =1, {variation = Random};
	bD6  =1, {variation = Random}; uD6  =1, {variation = Random};
	bE12 =1, {variation = Random}; uE12 =1, {variation = Random};
	bGD6 =1, {variation = Random}; uGD6 =1, {variation = Random};
	bGE12=1, {variation = Random}; uGE12=1, {variation = Random};
	a076 =1, {variation = Random}; a081 =1, {variation = Random}; a1   =1, {variation = Random};
	dI   =1, {variation = Random}; dA   =1, {variation = Random};
	bA   =1, {variation = Random}; uA   =1, {variation = Random};
	kA6  =1, {variation = Random}; kA12 =1, {variation = Random};
	k6   =1, {variation = Random}; k12  =1, {variation = Random};
	dC   =1, {variation = Random};
	eta6 =1, {variation = Random}; eta12=1, {variation = Random}
	]
directive simulator pde
directive spatial { nx=101; xmax=0.05; dt=200.0; diffusibles=[C6e=1.0;C12e=DC12]; random=0.2 }

 <->{aR}{dR} luxR |
 <->{aS}{dS} lasR |
 C6+luxR<->{bR6}{uR6}R6 |
 C12+lasR<->{bS12}{uS12}S12 |
 R6+R6<->{bD6}{uD6}D6 |
 S12+S12<->{bE12}{uE12}E12 |
 GOLux+D6<->{bGD6}{uGD6}GOLuxD6 |
 GOLas+E12<->{bGE12}{uGE12}GOLasE12 |
 GOLux->{a076}GOLux+aiiA |
 GOLas->{a081}GOLas+luxI+lasI |
 GOLuxD6->{a1}GOLuxD6+aiiA |
 GOLasE12->{a1}GOLasE12+luxI+lasI |
 luxI->{dI} |
 lasI->{dI} |
 aiiA->{dA} |
 aiiA+C6<->{bA}{uA} aiiAC6 |
 aiiAC6->{kA6} aiiA |
 aiiA+C12<->{bA}{uA}aiiAC12 |
 aiiAC12->{kA12}aiiA |
 luxI->{k6}luxI+C6 |
 lasI->{k12}lasI+C12 |
 C6->{dC} |
 C12->{dC} |
 C6<->{eta6}{eta6}C6e |
 C12<->{eta12}{eta12}C12e |

init luxR 1 |
init lasR 1 |
init GOLux 1 |
init GOLas 1"
    |> Crn.from_string
    |> Dynamical.fromCRN 

let betaCrn () = 
    "directive simulation { final=1.0; points=1000; plots=[X] }
directive parameters [
	aR   =1, {variation = Random}; dR   =1, {variation = Random};
	aS   =1, {variation = Random}; dS   =1, {variation = Random};
	bR6  =1, {variation = Random}; uR6  =1, {variation = Random};
	bS12 =1, {variation = Random}; uS12 =1, {variation = Random};
	bD6  =1, {variation = Random}; uD6  =1, {variation = Random};
	bE12 =1, {variation = Random}; uE12 =1, {variation = Random};
	bGD6 =1, {variation = Random}; uGD6 =1, {variation = Random};
	bGE12=1, {variation = Random}; uGE12=1, {variation = Random};
	a076 =1, {variation = Random}; a081 =1, {variation = Random}; a1   =1, {variation = Random};
	dI   =1, {variation = Random}; dA   =1, {variation = Random};
	bA   =1, {variation = Random}; uA   =1, {variation = Random};
	kA6  =1, {variation = Random}; kA12 =1, {variation = Random};
	k6   =1, {variation = Random}; k12  =1, {variation = Random};
	dC   =1, {variation = Random};
	]
directive simulator pde
directive spatial { nx=101; xmax=0.05; dt=200.0; diffusibles=[C6=1.0;C12=DC12]; random=0.2 }

 <->{aR}{dR} luxR |
 <->{aS}{dS} lasR |
 C6+luxR<->{bR6}{uR6}R6 |
 C12+lasR<->{bS12}{uS12}S12 |
 R6+R6<->{bD6}{uD6}D6 |
 S12+S12<->{bE12}{uE12}E12 |
 GOLux+D6<->{bGD6}{uGD6}GOLuxD6 |
 GOLas+E12<->{bGE12}{uGE12}GOLasE12 |
 GOLux->{a076}GOLux+aiiA |
 GOLas->{a081}GOLas+luxI+lasI |
 GOLuxD6->{a1}GOLuxD6+aiiA |
 GOLasE12->{a1}GOLasE12+luxI+lasI |
 luxI->{dI} |
 lasI->{dI} |
 aiiA->{dA} |
 aiiA+C6<->{bA}{uA} aiiAC6 |
 aiiAC6->{kA6} aiiA |
 aiiA+C12<->{bA}{uA}aiiAC12 |
 aiiAC12->{kA12}aiiA |
 luxI->{k6}luxI+C6 |
 lasI->{k12}lasI+C12 |
 C6->{dC} |
 C12->{dC} |

init luxR 1 |
init lasR 1 |
init GOLux 1 |
init GOLas 1"
    |> Crn.from_string
    |> Dynamical.fromCRN

(* Actual beta model *)
let beta1 = 
    let KA6  = Float 0.0560
    let KA12 = Float 1.9124
    let   dA = Float 0.0718
    let   dI = Float 0.0255
    let   dC = Float 0.0537
    let   d6 = Float 0.0211
    let  d12 = Float 1.1626
    let   k6 = Float 0.0405
    let  k12 = Float 0.0955
    let luxR = Float 5.8900
    let lasR = Float 2.9700
    let n = Float 0.797
    
    let a  = Key("a")
    let i  = Key("i")    
    let c6  = Key("c6")
    let c12 = Key("c12")

    let f76, f81 = orthogonal(luxR, lasR, c6, c12, n)
    let funcA = a/(1.0+KA6*c6+KA12*c12)
    
    let odes = 
        [|"a",   f76 - dA*a
        ; "i",   f81 - dI*i
        ; "c6",  k6*i - d6*funcA*c6 - dC*c6
        ; "c12", k12*i - d12*funcA*c12 - dC*c12
        |]        
    
    Dynamical.Create(odes, ["c6"; "c12"])

(* Approximate beta model with integer exponents*)
let beta2 = 
    let KA6  = Float 0.0560
    let KA12 = Float 1.9124
    let   dA = Float 0.0718
    let   dI = Float 0.0255
    let   dC = Float 0.0537
    let   d6 = Float 0.0211
    let  d12 = Float 1.1626
    let   k6 = Float 0.0405
    let  k12 = Float 0.0955
    let luxR = Float 5.8900
    let lasR = Float 2.9700
    let n = Float 1.0
    
    let a  = Key("a")
    let i  = Key("i")    
    let c6  = Key("c6")
    let c12 = Key("c12")

    let f76, f81 = orthogonal(luxR, lasR, c6, c12, n)
    let funcA = a/(1.0+KA6*c6+KA12*c12)
    
    let odes = 
        [|"a",   f76 - dA*a
        ; "i",   f81 - dI*i
        ; "c6",  k6*i - d6*funcA*c6 - dC*c6
        ; "c12", k12*i - d12*funcA*c12 - dC*c12
        |]        
    
    Dynamical.Create(odes, ["c6"; "c12"])


let beta3 = 
    let KA6  = Key "KA6"
    let KA12 = Key "KA12"
    let   dA = Key "dA"
    let   dI = Key "dI"
    let   dC = Key "dC"
    let   d6 = Key "d6"
    let  d12 = Key "d12"
    let   k6 = Key "k6"
    let  k12 = Key "k12"
    let luxR = Key "luxR"
    let lasR = Key "lasR"
    let n = Float 1.0
    
    let a  = Key("a")
    let i  = Key("i")    
    let c6  = Key("c6")
    let c12 = Key("c12")

    let f76, f81 = orthogonal(luxR, lasR, c6, c12, n)
    let funcA = a/(1.0+KA6*c6+KA12*c12)
    
    let odes = 
        [|"a",   f76 - dA*a
        ; "i",   f81 - dI*i
        ; "c6",  k6*i - d6*funcA*c6 - dC*c6
        ; "c12", k12*i - d12*funcA*c12 - dC*c12
        |]        
    
    Dynamical.Create(odes, ["c6"; "c12"])

let gamma1 = 
    let a   = Key "a"
    let l   = Key "l"
    let c6  = Key "c6"
    let c12 = Key "c12"

    let    n = Float 1.0
    let   nA = Float 2.0
    let delA = Float 1.0
    let delI = Float 0.0
    let   kA = Float 3.9208
    let   k6 = Float 0.0274
    let  k12 = Float 10.7387
    let  dC6 = Float 0.3769
    let dC12 = Float 0.0138
    let   dA = Float 0.0178
    let   dL = Float 0.0110

    let odes = 
        [| "a",   kA*c6**n/(1.0+c6**n)/(1.0+c12**n) - dA*a
        ;  "l",   (delA*a**nA + delI) / (1.0+a**nA) - dL*l
        ;  "c6",  k6*l - dC6*c6
        ;  "c12", k12*l - dC12*c12
        |]

    Dynamical.Create(odes, ["c6"; "c12"])
    


let gamma2 = 
    let a   = Key "a"
    let l   = Key "l"
    let c6  = Key "c6"
    let c12 = Key "c12"

    let    n = Float 1.0
    let   nA = Float 2.0
    let delA = Float 0.0
    let delI = Float 1.0
    let   kA = Float 13.1235
    let   k6 = Float 0.2490
    let  k12 = Float 6.8079
    let  dC6 = Float 0.2101
    let dC12 = Float 2.9123
    let   dA = Float 0.6848
    let   dL = Float 0.0752

    let odes = 
        [| "a",   kA*c6**n/(1.0+c6**n)/(1.0+c12**n) - dA*a
        ;  "l",   (delA*a**nA + delI) / (1.0+a**nA) - dL*l
        ;  "c6",  k6*l - dC6*c6
        ;  "c12", k12*l - dC12*c12
        |]

    Dynamical.Create(odes, ["c6"; "c12"])



let gamma3 = 
    let a   = Key "a"
    let l   = Key "l"
    let c6  = Key "c6"
    let c12 = Key "c12"

    let    n = Float 1.0
    let   nA = Float 2.0
    let delA = Float 0.0
    let delI = Float 1.0
    let   kA = Key "kA"
    let   k6 = Key "k6"
    let  k12 = Key "k12"
    let  dC6 = Key "dC6"
    let dC12 = Key "dC12"
    let   dA = Key "dA"
    let   dL = Key "dL"

    let odes = 
        [| "a",   kA*c6**n/(1.0+c6**n)/(1.0+c12**n) - dA*a
        ;  "l",   (delA*a**nA + delI) / (1.0+a**nA) - dL*l
        ;  "c6",  k6*l - dC6*c6
        ;  "c12", k12*l - dC12*c12
        |]

    Dynamical.Create(odes, ["c6"; "c12"])

let Selkov = 
    (* 
    ->{k1} A1
    A1 + A2 <->{k2, k2r} A3
    A3 ->{k3} A2 + A4
    A4 ->{k4}
    2*A4  + A5 ->{k5, k5r} A2
    *)
    
    let A1  = Key "A1"
    let A2  = Key "A2"
    let A3  = Key "A3"
    let A4  = Key "A4"
    let A5  = Key "A5"
    
    let k1 = Key "k1"
    let k2 = Key "k2"
    let k2r = Key "k2r"
    let k3 = Key "k3"
    let k4 = Key "k4"
    let k5 = Key "k5"
    let k5r = Key "k5r"
            
    let odes = 
        [| "A1", k1 - k2*A1*A2 + k2r*A3
        ;  "A2", k2r*A3 - k2*A1*A2 + k3*A3 + k5*A5*A4**2.0 - k5r*A2
        ;  "A3", k2*A1*A2 - k2r*A3 - k3*A3
        ;  "A4", k3*A3 - k4*A4 - 2.0*k5*A4**2.0*A5 + 2.0*k5r*A2
        ;  "A5", k5r*A2 - k5*A5*A4**2.0
        |]

    Dynamical.Create(odes, ["A1"; "A2"; "A3"; "A4"; "A5"])


let BetaFullModel = 
    //state vars
    let R         = Key "R"
    let R_6       = Key "R_6"
    let D_6       = Key "D_6"
    let GOLuxD_6  = Key "GOLuxD_6"
    let GOLux     = Key "GOLux"
    let S         = Key "S"
    let S_12      = Key "S_12"
    let C_12      = Key "C_12"
    let E_12      = Key "E_12"
    let GOLasE_12 = Key "GOLasE_12"
    let GOLas     = Key "GOLas"    
    let G_76      = Key "G_76"
    let A_C_6     = Key "A_C_6"
    let A_C_12    = Key "A_C_12"
    let A         = Key "A"
    let G_81      = Key "G_81"
    let I         = Key "I"
    let J         = Key "J"
    let C_6       = Key "C_6"
    let C_6_e     = Key "C_6_e"
    let I_6       = Key "I_6"
    let I_12      = Key "I_12"
    let C_12_e    = Key "C_12_e"
    
    //param vars
    let a_R   = Key "a_R"
    let u_R6  = Key "u_R6"    
    let d_R   = Key "d_R"
    let gamma = Key "gamma"
    let b_R6  = Key "b_R6"    
    let u_D6  = Key "u_D6"
    let b_D6  = Key "b_D6"
    let u_GD6 = Key "u_GD6"
    let b_GD6 = Key "b_GD6"
    let n_G   = Key "n_G"
    let a_S   = Key "a_S"
    let u_S12 = Key "u_S12"
    let d_S   = Key "d_S"
    let b_S12 = Key "b_S12"
    let u_E12 = Key "u_E12"
    let b_E12 = Key "b_E12"
    let u_GE12 = Key "u_GE12"
    let b_GE12 = Key "b_GE12"
    let a_0_76 = Key "a_0_76"
    let a_1    = Key "a_1"
    let u_A    = Key "u_A"
    let k_A6   = Key "k_A6"
    let k_A12  = Key "k_A12"
    let d_A    = Key "d_A"
    let b_A    = Key "b_A"
    let a_0_81 = Key "a_0_81"
    let d_I    = Key "d_I"
    let k_6    = Key "k_6"
    let eta_6  = Key "eta_6"
    let d_C    = Key "d_C"
    let k_12   = Key "k_12"
    let eta_12 = Key "eta_12"
    let k_A    = Key "k_A"
    let V_i    = Key "V_i"
    let V_e    = Key "V_e"
            
    //odes: check G_76*D_6 vs G_76_D_6 (also for 81)
    let dR         = a_R + u_R6*R_6 - R*(d_R + gamma + b_R6*C_6)
    let dR_6       = b_R6*R*C_6 + 2.0*u_D6*D_6 - R_6*(gamma + u_R6 + 2.0*b_D6*R_6)    
    let dD_6       = b_D6*R_6**2.0 + u_GD6*GOLuxD_6 - D_6*(gamma + u_D6 + b_GD6*GOLux)
    let dGOLux     = n_G + u_GD6*GOLuxD_6 - (gamma + b_GD6*D_6)*GOLux
    let dGOLuxD_6  = b_GD6*GOLux*D_6 - (gamma + u_GD6)*GOLuxD_6
    let dS         = a_S + u_S12*S_12 - S*(d_S + gamma + b_S12*C_12)
    let dS_12      = b_S12*S*C_12 + 2.0*u_E12*E_12 - S_12*(gamma + u_S12 + 2.0*b_E12*S_12)
    let dE_12      = b_E12*S_12**2.0 + u_GE12*GOLasE_12 - E_12*(gamma + u_E12 + b_GE12*GOLas)
    let dGOLas     = n_G + u_GE12*GOLasE_12 - (gamma + b_GE12*E_12)*GOLas
    let dGOLasE_12 = b_GE12*GOLas*E_12 - (gamma + u_GE12)*GOLasE_12
    let dA         = a_0_76*G_76 + a_1*G_76*D_6 + (u_A + k_A6)*A_C_6 + (u_A + k_A12)*A_C_12 - (d_A + gamma + b_A*C_6 + b_A*C_12)*A
    let dI_6       = a_0_81*G_81 + a_1*G_81*E_12 - (d_I + gamma)*I
    let dI_12      = a_0_81*G_81 + a_1*G_81*E_12 - (d_I + gamma)*J
    let dC_6       = k_6*I_6 + u_A*A_C_6 + eta_6*(C_6_e - C_6) - (d_C + gamma + b_A*A)*C_6
    let dC_12      = k_12*I_12 + u_A*A_C_12 + eta_12*(C_12_e - C_6) - (d_C + gamma + b_A*A)*C_12 
    let dA_C_6     = b_A*A*C_6 - (gamma + u_A + k_A)*A_C_6
    let dA_C_12    = b_A*A*C_12 - (gamma + u_A + k_A)*A_C_12
    let dC_6_e     = (V_i/V_e)*eta_6*(C_6 - C_6_e)
    let dC_12_e    = (V_i/V_e)*eta_12*(C_12 - C_6_e)

    let odes = 
          [| "R"         , dR        
           ; "R_6"       , dR_6      
           ; "D_6"       , dD_6      
           ; "GOLux"     , dGOLux    
           ; "GOLuxD_6"  , dGOLuxD_6 
           ; "S"         , dS        
           ; "S_12"      , dS_12     
           ; "E_12"      , dE_12     
           ; "GOLas"     , dGOLas    
           ; "GOLasE_12" , dGOLasE_12
           ; "A"         , dA        
           ; "I_6"       , dI_6      
           ; "I_12"      , dI_12     
           ; "C_6"       , dC_6      
           ; "C_12"      , dC_12     
           ; "A_C_6"     , dA_C_6    
           ; "A_C_12"    , dA_C_12   
           ; "C_6_e"     , dC_6_e    
           ; "C_12_e"    , dC_12_e
           |]
           
    Dynamical.Create(odes, ["C_6"; "C_12"])


let epsilon_code n6 n12 nL nT = 
    sprintf """directive spatial { nx=101; xmax=0.01; dt=0.01; diffusibles=[c6=1.0;c12=1.0]; random=0.01 }
directive rates [
	P76 = (a076 + a1R*(K6*[luxR]*[c6])^%d) / (1.0 + (K6*[luxR]*[c6])^%d);
  P81 = (a081 + a1S*(K12*[lasR]*[c12])^%d) / (1.0 + (K12*[lasR]*[c12])^%d);
  hlacI = 1/(1+[lacI]^%d);
  htetR = 1/(1+[tetR]^%d);
  x = 1
]

// Cell growth
luxR ->[r*[luxR]] |
lasR ->[r*[lasR]] |
lacI ->[r*[lacI]] |
tetR ->[r*[tetR]] |
luxI ->[r*[luxI]] |
lasI ->[r*[lasI]] |

// \"Total\" LuxR and LasR 
->[aR*[htetR]] luxR | //luxR ->{dR} |
->[aS*[hlacI]] lasR | //lasR ->{dR} |

// TetR (P81) 
->[aT*[P81]] tetR | tetR ->{dT} |

// LacI (P76)
->[aL*[P76]] lacI | lacI ->{dL} | 

// LuxI/LasI
->[klas*[hlacI]] lasI | //lasI->{dlasI} |
->[klux*[hlacI]] luxI | //luxI->{dluxI} | 

// AHL synthesis
->[kC6*[luxI]*[x]] c6 |
->[kC12*[lasI]*[x]] c12 |

// Lactonase-mediated degradation rate
c6 ->[dC6*[x]*[c6]] |
c12 ->[dC12*[x]*[c12]]""" n6 n6 n12 n12 nL nT



let epsilon n6 n12 nL nT = 
    """directive parameters 
[ a076=21.10919565 , {variation=Random}
; a081=17.3565057  , {variation=Random}
; a1R=450.6898246  , {variation=Random}
; a1S=159.0559592  , {variation=Random}
; K6=0.399067219   , {variation=Random}
; K12=0.580588336  , {variation=Random}
; aR=0.002106209   , {variation=Random}
; aS=0.001694342   , {variation=Random}
//; dR=0.961375723   , {variation=Random}
; aL=0.079635013   , {variation=Random}
; aT=0.447961441   , {variation=Random}
; dL=98.32474142   , {variation=Random}
; dT=99.94263916   , {variation=Random}
// Scaled parameters
; klux=1.0         , {variation=Random}
; klas=1.0         , {variation=Random}
; kC6=73.3571      , {variation=Random}
; kC12=92.5462     , {variation=Random}
//; dluxI=0.0028     , {variation=Random}
//; dlasI=0.0028     , {variation=Random}
; dC6=28.9863      , {variation=Random}
; dC12=28.9863     , {variation=Random}
; r = 1.0          , {variation=Random}
]

""" + epsilon_code n6 n12 nL nT
    |> Crn.from_string
    |> Dynamical.fromCRN
    |> Dynamical.setAllLB 0.0

let epsilon_ranges n6 n12 nL nT = 
    """directive parameters 
[ a076=21.10919565 , {variation=Random; distribution=Uniform(10.0,50.0)}
; a081=17.3565057  , {variation=Random; distribution=Uniform(10.0,50.0)}
; a1R=450.6898246  , {variation=Random; distribution=Uniform(100.0,500.0)}
; a1S=159.0559592  , {variation=Random; distribution=Uniform(100.0,200.0)}
; K6=0.399067219   , {variation=Random; distribution=Uniform(0.1,0.5)}
; K12=0.580588336  , {variation=Random; distribution=Uniform(0.5,1.0)}
; aR=0.002106209   , {variation=Random; distribution=Uniform(0.001,0.003)}
; aS=0.001694342   , {variation=Random; distribution=Uniform(0.001,0.003)}
; dR=0.961375723   , {variation=Random; distribution=Uniform(0.9,1.0)}
; aL=0.079635013   , {variation=Random; distribution=Uniform(0.05,0.1)}
; aT=0.447961441   , {variation=Random; distribution=Uniform(0.1,0.5)}
; dL=98.32474142   , {variation=Random; distribution=Uniform(10.0,100.0)}
; dT=99.94263916   , {variation=Random; distribution=Uniform(10.0,100.0)}
// Scaled parameters
; klux=1.0         , {variation=Random}
; klas=1.0         , {variation=Random}
; kC6=73.3571      , {variation=Random; distribution=Uniform(10.0,100.0)}
; kC12=92.5462     , {variation=Random; distribution=Uniform(10.0,100.0)}
//; dluxI=0.0028     , {variation=Random; distribution=Uniform(0.001,0.003)}
//; dlasI=0.0028     , {variation=Random; distribution=Uniform(0.001,0.003)}
; dC6=28.9863      , {variation=Random; distribution=Uniform(10.0,30.0)}
; dC12=28.9863     , {variation=Random; distribution=Uniform(10.0,30.0)}
; r = 1.0
]""" + epsilon_code n6 n12 nL nT
    |> Crn.from_string
    |> Dynamical.fromCRN 
    |> Dynamical.setAllLB 0.0


let epsilon_reduce4 n6 n12 nL nT = 
    
    let one = Float 1.0
    let lacI = Key "lacI"
    let tetR = Key "tetR"
    let c6 = Key "c6"
    let c12 = Key "c12"
    let a076 = Key "a076"
    let a081 = Key "a081"
    let a1R = Key "a1R"
    let a1S = Key "a1S"
    let K6 = Key "K6"
    let K12 = Key "K12"
    
    let hlacI = 
      match nL with 
      | 1 -> one / (one + lacI)
      | 2 -> one / (one + lacI*lacI)
      | 3 -> one / (one + lacI*lacI*lacI)
      | 4 -> one / (one + lacI*lacI*lacI*lacI)
      | _ -> let n = Key "nL" in one / (one + lacI**n)
    let htetR = 
      match nT with 
      | 1 -> one / (one + tetR) 
      | 2 -> one / (one + tetR*tetR) 
      | 3 -> one / (one + tetR*tetR*tetR) 
      | 4 -> one / (one + tetR*tetR*tetR*tetR) 
      | _ -> let n = Key "nT" in one / (one + tetR**n)
      
    let klux = Key "klux"
    let klas = Key "klas"
    let aR = Key "aR"
    let aS = Key "aS"
    let r = Key "r"
    let kC6 = Key "kC6"
    let kC12 = Key "kC12"
    let dC6 = Key "dC6"
    let dC12 = Key "dC12"
    let aT = Key "aT"
    let aL = Key "aL"
    let dT = Key "dT"
    let dL = Key "dL"
    
    let luxI = klux*hlacI / r
    let lasI = klas*hlacI / r
    let luxR = aR*htetR / r
    let lasR = aS*hlacI / r

    let P76 = 
      match n6 with 
      | 1 -> (a076 + a1R*(K6*luxR*c6)) / (one + (K6*luxR*c6))
      | 2 -> (a076 + a1R*(K6*luxR*c6)*(K6*luxR*c6)) / (one + (K6*luxR*c6)*(K6*luxR*c6))
      | _ -> let n = Key "n6" in (a076 + a1R*(K6*luxR*c6)**n) / (one + (K6*luxR*c6)**n)
    let P81 = 
      match n12 with 
      | 1 -> (a081 + a1S*(K12*lasR*c12)) / (one + (K12*lasR*c12))
      | 2 -> (a081 + a1S*(K12*lasR*c12)*(K12*lasR*c12)) / (one + (K12*lasR*c12)*(K12*lasR*c12))
      | _ -> let n = Key "n12" in (a081 + a1S*(K12*lasR*c12)**n) / (one + (K12*lasR*c12)**n)
    
    let x = Key "x"
    
    let odes = 
        [| "c6",   kC6*luxI*x - c6*dC6*x
           "c12",  kC12*lasI*x - c12*dC12*x
           "lacI", aL*P76 - (dL+r)*lacI
           "tetR", aT*P81 - (dT+r)*tetR
        |]
    let diffusibles = ["c6",Float 1.0; "c12", Float 1.0] |> Map.ofList
    Dynamical.Create(odes,diffusibles)
    |> Dynamical.setAllLB 0.0


let epsilontet_simplified_nogrowth () = 
    "directive spatial { nx=101; xmax=0.01; dt=0.005; diffusibles=[c6=D6;c12=D12]; random=0.01; dimensions=2 }
directive parameters 
[ D6=0.0000018, {distribution=Uniform(1e-8,1e-6); interval=Log; variation=Random}
; D12 = 0.0000009, {distribution=Uniform(1e-8,1e-6); interval=Log; variation=Random}
; a076=21.10919565
; a081=17.3565057
; a1R=450.6898246
; a1S=159.0559592
; K6=0.399067219
; K12=0.580588336
; n6=0.662458724
; n12=1.261673783
; aR=0.002106209
; aS=0.001694342
; dR=0.961375723
; aL=0.079635013
; aT=0.447961441
; dL=98.32474142
; dT=99.94263916
; nL=3.994805091
; nT=2.206056173
// Scaled parameters
; klux=1.0; klas=1.0
//Epsilon tet parameters degratio 0.1 optimized
; kC6=25.3602; kC12=3.1399; dluxI=0.0219; dlasI=0.0219; dC6=11.1159; dC12=1.1116
; c0 = 0.002; r = 0.2; K = 2.0; rc = 40.0
]

directive rates [
    //growth = r*(1-[x]/K);
    growth = r;
	P76 = (a076 + (a1R*((K6*[luxR]*[c6])^n6))) / (1.0 +((K6*[luxR]*[c6])^n6));
    P81 = (a081 + (a1S*((K12*[lasR]*[c12])^n12))) / (1.0 +((K12*[lasR]*[c12])^n12))
]

//Epsilon tet parameters degratio 0.1 optimized
init luxR 0.002556 |
init lasR 0.009976 |
init laci 1.484729 |
init tetr 4.483313 |
init luxI 6.351476 |
init lasI 6.351476 |
init c6 14.490451 |
init c12 17.941032 |

// Cell growth
luxR ->[[growth]*[luxR]] |
lasR ->[[growth]*[lasR]] |
laci ->[[growth]*[laci]] |
tetr ->[[growth]*[tetr]] |
luxI ->[[growth]*[luxI]] |
lasI ->[[growth]*[lasI]] |

// LuxR and LasR
->[rc*aR/(1+[tetr]^nT)] luxR | luxR ->{dR} |
->[rc*aS/(1+[laci]^nL)] lasR | lasR ->{dR} |

// TetR (P81) and LacI (P76)
->[rc*aT*[P81]] tetr | tetr ->{dT} |
->[rc*aL*[P76]] laci | laci ->{dL} | 

// LuxI and LasI
->[rc*klas/(1+[tetr]^nT)] lasI | lasI->{dlasI}|
->[rc*klux/(1+[tetr]^nT)] luxI | luxI->{dluxI}|

// AHL synthesis
->[kC6*[luxI]] c6 |
->[kC12*[lasI]] c12 |

// Lactonase-mediated degradation rate
c6->[dC6*[c6]] |
c12->[dC12*[c12]]"

let epsilontet_nogrowth () = 
  "
directive spatial { nx=101; xmax=0.01; dt=0.005; diffusibles=[c6=D6;c12=D12]; random=0.01; dimensions=1 }
directive parameters 
[  klux=2.76, {distribution=Uniform(1e-6,1e1); interval=Log; variation=Random}
    ; klas=0.516, {distribution=Uniform(1e-6,1e1); interval=Log; variation=Random}
    ; D6=0.0000018
    ; D12 = 0.0000009
    ; a0_76 = 7.37
    ; a0_81 = 8.97
    ; a1R = 1.86e+03	
    ; a1S = 704	
    ; KGR_76 = 0.00145301678863517
    ; KGS_76 = 1.02514521585902E-06
    ; KGR_81 = 1.23654786633501E-06
    ; KGS_81 = 1.00086836995962E-05
    ; KR6 = 3.50695213045775E-08
    ; KS6 = 7.18882099861015E-08
    ; KR12 = 1.39161085147457E-10
    ; KS12 = 0.0095893786931253
    ; nR=0.699
    ; nS=1.25
    ; aR33=9.1
    ; aS175=3.58
    ; rho=0.182
    ; dR=0.267	
    ; dS=0.319
    ; dyfp=0.0967	
    ; dcfp=0.129	
    ; yb0=341
    ; cb0=3320
    ; aL=0.00117	
    ; aT=0.000951	
    ; dL=0.000117	
    ; dT=0.666	
    ; nL=0.929
    ; nT=4.0
    ; iAtc=0.106
    ; iIptg=273
    ; kC6=1.0
    ; kC12=1.0
    ; dluxI=1.0
    ; dlasI=1.0
    ; autoY=0.1
    ; autoC=3.31
    ; dC6 = 0.1, {distribution=Uniform(1e-6,1e1); interval=Log; variation=Random}
    ; dC12 = 0.01, {distribution=Uniform(1e-6,1e1); interval=Log; variation=Random}
    ; C6 = 0.0
    ; C12 = 0.0
    ; r = 1.0
    ; K = 2.0
    ; rc = 25.0
    ; es=1.0
    //; ATC=0.0
    //; IPTG=0.0
    ]

directive rates [	
    growth = r;
	capacity = rc*([growth]+es);
	boundLuxR = [luxR]^2 * ((KR6*[c6])^nR + (KR12*[c12])^nR);
	boundLasR = [lasR]^2 * ((KS6*[c6])^nS + (KS12*[c12])^nS);
	P76 = (a0_76 + a1R*KGR_76*[boundLuxR] + a1S*KGS_76*[boundLasR]) / (1.0 + KGR_76*[boundLuxR] + KGS_76*[boundLasR]);
	P81 = (a0_81 + a1R*KGR_81*[boundLuxR] + a1S*KGS_81*[boundLasR]) / (1.0 + KGR_81*[boundLuxR] + KGS_81*[boundLasR])
]

init c12 C12 |
init c6 C6   | 
init luxR 350 | 
init lasR 47.0 |
init laci 2.05 |
init tetr 0.3 |
init luxI 1050 |
init lasI 28.0 |

// Cell growth and dilution
luxR ->[[growth]*[luxR]] |
lasR ->[[growth]*[lasR]] |
laci ->[[growth]*[laci]] |
tetr ->[[growth]*[tetr]] |

// Total LuxR and LasR 
->[[capacity]*aR33/(1+[tetr]^nT)] luxR | luxR ->{dR} |
->[[capacity]*aS175/(1+[laci]^nL)] lasR | lasR ->{dS} |

// TetR (P81) 
->[[capacity]*aT*[P81]] tetr | tetr ->{dT} |

// LacI (P76)
->[[capacity]*aL*[P76]] laci | laci ->{dL} |

//ATC and IPTG
//tetr ->[iAtc*[tetr]*ATC]  |
//laci ->[iIptg*[laci]*IPTG] |

// LuxI and LasI
->[[capacity]*klas/(1+[tetr]^nT)] lasI | lasI->{dlasI}|
->[[capacity]*klux/(1+[tetr]^nT)] luxI | luxI->{dluxI}|

//C6 and C12 production
->[kC6*[luxI]] c6 |
->[kC12*[lasI]] c12 |

// Lactonase-mediated degradation rate
c6->[dC6*[c6]] |
c12->[dC12*[c12]] 
  "
