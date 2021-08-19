module Microsoft.Research.CRNEngine.Tests.ExportTests

open Xunit
open FsUnit
open Microsoft.Research.CRNEngine

let parse = Crn.parse |> Parser.from_string

let yfp_text = """directive sweeps [
  sweep_130607_Pcat_R123 = [ 
    (rc,r,K,c0,C6) = [(2100,0.7102,1.879,0.006692,1e+06); (1568,0.5727,2.099,0.02208,3.333e+05); (1407,0.5976,2.22,0.01858,1.111e+05); (1469,0.5766,2.068,0.01932,3.704e+04); (1580,0.6025,2.089,0.01442,1.235e+04); (1579,0.6588,2.146,0.01193,4115); (1762,0.6642,1.907,0.01219,1372); (2152,0.801,1.902,0.003548,457.2); (2438,0.9116,1.815,0.001758,152.4); (2855,1.046,1.687,0.0005095,50.81); (3028,1.117,1.737,0.0003199,16.94); (3856,1.161,1.427,0.0001771,0)]; 
    rs = [139]; 
    Kc = [0.9046]; 
    nc = [44.61]; 
    b0 = [6239]; 
    yb0 = [123] 
  ] 
]
directive simulation { final=20.0; points=1000; plots=[[x]*[yfp]+[yback]] }
directive simulator deterministic
directive deterministic { reltolerance=1e-5 }
directive parameters 
[ rsY=1e0,  { interval=Log; distribution=Uniform(1e-2,1e4); variation=Random }
; eps=0.0,  { interval=Real; distribution=Uniform(0.0,0.1); variation=Random }
; KC=1e3,   { interval=Log; distribution=Uniform(1e2,1e4); variation=Random }
; n=0.8,    { interval=Real; distribution=Uniform(0.6,2.0); variation=Random }
; aY=1e0, 	{ interval=Log; distribution=Uniform(1e-3,1e3); variation=Random }
; dyfp=0.1, { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random }
; yb0=1e3,  { interval=Real; distribution=Uniform(0.0,5e3); variation=Random }
; C6 = 100000.0; c0 = 0.001; r = 1.0; K = 2.0; rc = 1000.0; rs = 100.0; Kc=0.9; nc=10.0; b0=0.0 ]

init yback yb0 | init x c0 |

->[r*(1 - [x] / K)*[x]] x |
x ~ yfp ->[[yfp]*r*(1 - [x] / K)] |
x ~ iyfp ->[[iyfp]*r*(1 - [x] / K)] |

->[(rc*Kc^nc+rsY*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aY*(C6^n + eps*KC^n) / (C6^n + KC^n)] iyfp | iyfp ->{dyfp} |
iyfp ->{1.0} yfp | yfp ->{dyfp}""".Replace("\r\n", "\n")

let lactonase_text = """directive simulation { final=25.0; points=1000; plots=[[x]*[yfp]+yb0; [x]*[cfp]+cb0] }
directive deterministic {reltolerance=1e-8; abstolerance=1e-10}
directive simulator sundials
directive parameters 
[ a0_76=0.21; a0_81=0.264; a1R=18.47; a1S=8.24
; KGR_76=8.657e-2; KGS_76=4.788e-4; KGR_81=3.329e-3; KGS_81=4.249e-1
; KR6=2.076e-4; KS6=1.71e-8  ; KR12=4.937e-7; KS12=8.827e-3
; n=0.797; aR33=0.0589; aS175=0.0297; rho=1e0; dR=0.1; dS=0.1; dyfp=0.1; dcfp=0.1
; dA=0.1,   { interval=Log; distribution=Uniform(1e-4,1e-1); variation=Random }
; yb0=400,  { interval=Real; distribution=Uniform(0.0,5e3); variation=Random }
; cb0=3000, { interval=Real; distribution=Uniform(0.0,1e4); variation=Random }
; dA6=1e-5,  { interval=Log; distribution=Uniform(1e-6,1e-3); variation=Random }
; dA12=1e-5, { interval=Log; distribution=Uniform(1e-6,1e-3); variation=Random }
; KA6=1e-5, { interval=Log; distribution=Uniform(1e-8,1e-3); variation=Random }
; KA12=1e-5,{ interval=Log; distribution=Uniform(1e-8,1e-3); variation=Random }
; rs=1e3,   { interval=Log; distribution=Uniform(1e-2,1e4); variation=Random }
; Kc=0.9,   { interval=Real; distribution=Uniform(0.5,1.0); variation=Random }
; nc=1.0,   { interval=Log; distribution=Uniform(1.0,20.0); variation=Random }
; autoC = 5.0; autoY = 0.1
; C6 = 25000.0; C12 = 0.0; Ara = 5.0; c0 = 0.001; r = 1.0; K = 2.0; rc = 60.0; tlag=5]

directive rates [
	growth =  [grow]*r*(1 - [x] / K);
	capacity = (rc+rs*([x]/K/Kc)^nc)/(1.0+([x]/K/Kc)^nc);
	boundLuxR = [luxR]^2 * ((KR6*[c6])^n + (KR12*[c12])^n) / ((1.0 + KR6*[c6] + KR12*[c12])^n);
	boundLasR = [lasR]^2 * ((KS6*[c6])^n + (KS12*[c12])^n) / ((1.0 + KS6*[c6] + KS12*[c12])^n);
	P76 = (a0_76 + a1R*KGR_76*[boundLuxR] + a1S*KGS_76*[boundLasR]) / (1.0 + KGR_76*[boundLuxR] + KGS_76*[boundLasR]);
	P81 = (a0_81 + a1R*KGR_81*[boundLuxR] + a1S*KGS_81*[boundLasR]) / (1.0 + KGR_81*[boundLuxR] + KGS_81*[boundLasR])
]

init x c0 | init c6 C6 | init c12 C12 | init grow 1 @ tlag |

// Cell growth
->[[growth]*[x]] x |
yfp ->[[growth]*[yfp]] |
cfp ->[[growth]*[cfp]] |
luxR ->[[growth]*[luxR]] |
lasR ->[[growth]*[lasR]] |
aiia ->[[growth]*[aiia]] |

// "Total" LuxR and LasR 
->[[capacity]*aR33] luxR | luxR ->{dR} |
->[[capacity]*aS175] lasR | lasR ->{dS} |

// HSL degradation
c6 -> [[x]*dA6*[c6]*[aiia]/(1+KA6*[c6]+KA12*[c12])] |
c12 -> [[x]*dA12*[c12]*[aiia]/(1+KA6*[c6]+KA12*[c12])] |

//PBad aiiA
->[[capacity]*Ara] aiia | aiia ->{dA} |

// YFP (P81)
->[[capacity]*[P81]] yfp | 

// CFP (P76)
->[[capacity]*rho*[P76]] cfp |

// FP Degradation
yfp ->{dyfp} |
cfp ->{dcfp} |

// Autofluorescence
->[[capacity]*autoY] yfp |
->[[capacity]*autoC] cfp"""

[<Fact(DisplayName="Export - yfp")>]
let export_yfp () =
  let crn = parse yfp_text
  let saturated = crn.saturate_initials().create_blank_attributes()
  let code = saturated.to_string()
  let sbml = Sbml.to_xml <| saturated.to_sbml ()
  let svg = Svg.to_string "" <| saturated.to_svg ()
  ()

(*
[<Fact>]
let ``Export lactonase``() =
  let crn = parse lactonase_text
  let saturated = Crn.saturate_initials crn |> Crn.create_blank_attributes
  let code = Crn.to_string saturated
  let sbml = Sbml.to_xml <| Crn.to_sbml saturated
  let svg = Svg.to_string "" <| Crn.to_svg saturated
  ()
*)

let AM_with_rates () = """
directive simulation {initial=-1.0; final=2.0; points=300}
directive simulator deterministic
directive parameters [ k = 1.0 ]
directive rates [ encounter = k*[A]*[B] ]

init A 10 |
init B 5 |
A + B ->[[encounter]] I + A |
B + A ->[[encounter]] I + B |
A + I ->{k} A + A |
B + I ->{k} B + B""" |> Crn.from_string

[<Fact(DisplayName="Export - Matlab - AM")>]
let matlab_export_AM() =
  
  let crn = AM_with_rates ()
  let expectedMatlab =
    """function sol = crn_export()

tstart = -1; % Initial time
tfinal = 2; % Final time
species = {'A','B','I'}; % The list of all species
plots = {'A','B','I'};
n = length(species);

% Write out the parameters
p.k = 1;

% Assign initial conditions
x0 = zeros(n,1);
x0(1) = 10;		% A
x0(2) = 5;		% B

% Solve the ODEs
[t,x__] = ode15s(@odes,[tstart tfinal],x0,[],p);

% Write out a solution structure to be returned by the function
for i = 1:n
  sol.(species{i}) = x__(:,i);
end

% Apply plot expressions
plotsValues = zeros(length(x__),3);
plotsValues(:,1) = sol.('A');
plotsValues(:,2) = sol.('B');
plotsValues(:,3) = sol.('I');

% Produce a plot
figure;
plot(t, plotsValues)
xlabel('Time')
ylabel('Concentration')
box off
legend(plots,'box','off')

return

%%%

function dxdt = odes(t,x__,p)

% Write out the parameters
k = p.k;

% Assign states
A = x__(1);
B = x__(2);
I = x__(3);

% Define rate expressions
encounter = ((k * A) * B);

% Define reaction propensities
r_0 = encounter;
r_1 = encounter;
r_2 = (A * k * I);
r_3 = (B * k * I);

% Assign derivatives
dA =  -r_1 + r_2;
dB =  -r_0 + r_3;
dI =  r_0 + r_1 - r_2 - r_3;

dxdt = [dA;dB;dI];

return"""

  let matlab = crn.to_matlab()

  Assert.Equal(expectedMatlab,matlab)