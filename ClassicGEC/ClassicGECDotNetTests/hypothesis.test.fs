module Microsoft.Research.GEC.HypothesisTest

open Xunit
open FsUnit.Xunit
open System.Diagnostics

open Parser
open Microsoft.Research.GEC
open Microsoft.Research.CRNEngine


[<Fact(DisplayName="GEC - GEC CRN Parser - Directives")>]
let ``directivesParser``()=
    let directiveString = """directive simulator sundials
//directive simulator deterministic
//directive deterministic {stiff = true}
//directive inference {name=target; burnin=400000; samples=400000; thin=50; noise_model=proportional}
directive inference {name=target; burnin=10; samples=10; thin=1; noise_model=proportional}
directive sweeps [ 
  sweepC6C12 = [
    (condition,C6,C12) = [
      (1,25000,0); (2,8333.33333333333,0); (3,2777.77777777778,0); (4,925.925925925926,0); (5,308.641975308642,0); (6,102.880658436214,0); (7,34.2935528120713,0); (8,11.4311842706904,0); (9,3.81039475689681,0); (10,1.27013158563227,0); (11,0.423377195210757,0); (12,0,0); 
      (13,0,25000); (14,0,8333.33333333333); (15,0,2777.77777777778); (16,0,925.925925925926); (17,0,308.641975308642); (18,0,102.880658436214); (19,0,34.2935528120713); (20,0,11.4311842706904); (21,0,3.81039475689681); (22,0,1.27013158563227); (23,0,0.423377195210757); (24,0,0);
    ]; 
  ];
  sweepC6C12double = [
    (condition,C6,C12) = [
      (1,25000,0); (2,8333.33333333333,0); (3,2777.77777777778,0); (4,925.925925925926,0); (5,308.641975308642,0); (6,102.880658436214,0); (7,34.2935528120713,0); (8,11.4311842706904,0); (9,3.81039475689681,0); (10,1.27013158563227,0); (11,0.423377195210757,0); (12,0,0); 
      (13,0,25000); (14,0,8333.33333333333); (15,0,2777.77777777778); (16,0,925.925925925926); (17,0,308.641975308642); (18,0,102.880658436214); (19,0,34.2935528120713); (20,0,11.4311842706904); (21,0,3.81039475689681); (22,0,1.27013158563227); (23,0,0.423377195210757); (24,0,0);
      (1,25000,0); (2,8333.33333333333,0); (3,2777.77777777778,0); (4,925.925925925926,0); (5,308.641975308642,0); (6,102.880658436214,0); (7,34.2935528120713,0); (8,11.4311842706904,0); (9,3.81039475689681,0); (10,1.27013158563227,0); (11,0.423377195210757,0); (12,0,0); 
      (13,0,25000); (14,0,8333.33333333333); (15,0,2777.77777777778); (16,0,925.925925925926); (17,0,308.641975308642); (18,0,102.880658436214); (19,0,34.2935528120713); (20,0,11.4311842706904); (21,0,3.81039475689681); (22,0,1.27013158563227); (23,0,0.423377195210757); (24,0,0);
    ];
  ]; 
]

directive simulation { final=20; points=250 }

directive rates [
  boundLuxR = [luxR]^2 * ((KR6*[c6])^nR + (KR12*[c12])^nR) / ((1.0 + KR6*[c6] + KR12*[c12])^nR);
  boundLasR = [lasR]^2 * ((KS6*[c6])^nS + (KS12*[c12])^nS) / ((1.0 + KS6*[c6] + KS12*[c12])^nS);
  P76 = (e76 + KGR_76*[boundLuxR] + KGS_76*[boundLasR]) / (1.0 + KGR_76*[boundLuxR] + KGS_76*[boundLasR]);
  P81 = (e81 + KGR_81*[boundLuxR] + KGS_81*[boundLasR]) / (1.0 + KGR_81*[boundLuxR] + KGS_81*[boundLasR]);
  PBad = (Ara^nA+eA*KAra^nA)/(Ara^nA+KAra^nA);
  PTet = 1/(1+[tetR]^nT);
  PLac = 1/(1+[lacI]^nL);
  plot_od = [x]+x0;
  plot_fp = [x]*[fp]+f0;
  plot_yfp = [x]*([yfp]+[f500])+yb0;
  plot_cfp = [x]*([cfp]+[f430])+cb0;
]

directive parameters [ 
  // Background
  c0 = 0.001, { distribution=Uniform(1e-4,3e-1) };
  x0 = 0.1, { interval=Real; distribution=Uniform(0,0.2) };  
  f0=5000.0,{ interval=Real; distribution=Uniform(0.0,10000.0) }; 
  yb0=1e3, { interval=Real; distribution=Uniform(0.0,5e3) };
  cb0=1e3, { interval=Real; distribution=Uniform(0.0,1e4) };	
  
  // Autofluorescence
  dfp=0.1,  { distribution=Uniform(1e-3,1e0) };
  autoYFP=1e0, 	{ distribution=Uniform(1e-3,1e3) };
  autoCFP=1e0, 	{ distribution=Uniform(1e-3,1e3) };
  
  // Standard
  dCFP=1e-2,  { distribution=Uniform(1e-3,1e0) };
  dYFP=1e-2,  { distribution=Uniform(1e-3,1e0) };
  
  // Receivers
  KR6=1e-2,   { distribution=Uniform(1e-8,1e0) };
  KS6=1e-4,   { distribution=Uniform(1e-8,1e0) };
  KR12=1e-3,  { distribution=Uniform(1e-8,1e0) };
  KS12=1e-2,  { distribution=Uniform(1e-8,1e0) };
  nR=0.797,  	{ interval=Real; distribution=Uniform(0.5,2.0) };
  nS=0.797,  	{ interval=Real; distribution=Uniform(0.5,2.0) };
  aR33=1.0, 	{ distribution=Uniform(1e0,1e2) };
  aS175=1.0, 	{ distribution=Uniform(1e0,1e2) };
  aRS100=1.0, { distribution=Uniform(1e0,1e2) };
  aS32=1.0, 	{ distribution=Uniform(1e0,1e2) };
  nL=0.797,  	{ interval=Real; distribution=Uniform(0.5,2.0) };
  nT=0.797,  	{ interval=Real; distribution=Uniform(0.5,2.0) };
  
  dR=0.1,    	{ distribution=Uniform(1e-2,1e1) };
  //dS=0.1,   { distribution=Uniform(1e-2,1e2) };
  e76=1e-2,  	{ distribution=Uniform(1e-4,1.0) };
  KGR_76=1e-2,{ distribution=Uniform(1e-4,1e0) };
  KGS_76=1e-6,{ distribution=Uniform(1e-8,1e0) };
  e81=1e-2,   { distribution=Uniform(1e-4,1.0) };
  KGR_81=1e-6,{ distribution=Uniform(1e-8,1e0) };
  KGS_81=1e-2,{ distribution=Uniform(1e-4,1e0) };
  aCFP=1e3,   { distribution=Uniform(1e0,1e5) };
  aYFP=1e3, 	{ distribution=Uniform(1e0,1e5) };
  
  // Relays
  kC6=1e0,  { distribution=Uniform(1e0,1e6) };
  Klux=1.0, { distribution=Uniform(1e0,1e6) };
  dluxI=0.1,{ distribution=Uniform(1e-3,1e1) };
  kC12=1e0, { distribution=Uniform(1e0,1e6) };
  Klas=1.0, { distribution=Uniform(1e0,1e6) };
  dlasI=0.1,{ distribution=Uniform(1e-3,1e1) };
  
  // Arabinose
  KAra=1.0, { distribution=Uniform(1e-2,1e2) };
  nA=1.0,   { interval=Real; distribution=Uniform(0.5,3.0) };
  eA=0.1,   { interval=Real; distribution=Uniform(0.0,0.5) };
    
  // Degrader
  dA6=1e-1,  { distribution=Uniform(1e-3,1e1) };
  dA12=1e-1, { distribution=Uniform(1e-3,1e1) };
  daiiA=0.1, { distribution=Uniform(1e-3,1e1) };
  
  C6=0.0; C12=0.0; tau=0.0; aR=1.0; aS=1.0; ATC=0.0; IPTG=0.0;
  aYFP_PL=1000.0; aCFP_PL=1000.0; Ara=0.0; condition = 0;
]"""
    let directives = Parser.from_string Hypothesis.parse_crnSettings directiveString
    Debug.Write(directives.to_string Functional2.to_string Functional2.to_string_plot)
    ()

[<Fact(DisplayName="GEC - GEC CRN Parser - Modules")>]
let ``moduleParser``()=
    let moduleString = """
module Control(growth,capacity) = { 
//| Growth(growth,tlag)
  | fp ->[[growth]*[fp]] // Dilution
  | ->[[capacity]] fp    // Transcription/translation		
  | fp ->{dfp}           // Degradation
}
module cells(growth,tlag,capacity) = { 
  | init luxR 0 | init lasR 0 | init lacI 0 | init tetR 0 
  | init yfp 0 | init cfp 0 | init f430 0 | init f500 0 
  | init c6 C6 @ tau | init c12 C12 @ tau // Hack to prevent greedy evaluation of rates from hurting us
  | Growth(growth,tlag)
  // Autofluorescence
  | ->[[capacity]*autoYFP] f500
  | f500 ->[[growth]*[f500]]
  | ->[[capacity]*autoCFP] f430
  | f430 ->[[growth]*[f430]]
}

module AiiA(P,aI,growth,capacity) = {
  | ->[[capacity]*aI*[P]] aiiA 
  | aiiA ->{daiiA} 
  | aiiA ->[[growth]*[aiiA]] 
  //| c6 -> [[x]*dA6*[c6]*[aiiA]/(1+KA6*[c6]+KA12*[c12])] 
  //| c12 -> [[x]*dA12*[c12]*[aiiA]/(1+KA6*[c6]+KA12*[c12])] 
  | c6 -> [[x]*dA6*[c6]*[aiiA]] 
  | c12 -> [[x]*dA12*[c12]*[aiiA]]
}
"""
    let modules = Parser.from_string (Hypothesis.parse_crnModules Crn_settings.defaults) moduleString
    
    Debug.WriteLine(Hypothesis.modules_to_string 0.0 modules)
    
    ()

[<Fact(DisplayName="GEC - GEC CRN Parser - Directive Device Parser")>]
let ``deviceDirectiveParser``()=
    let device0 = "device [LuxR]"
    let device1 = "device [LuxR;pTet]"
    let device2 = "device [cfp; gfp]"
    let dev0 = Parser.from_string Hypothesis.parse_hypothesisDirective device0
    let dev1 = Parser.from_string Hypothesis.parse_hypothesisDirective device1
    let dev2 = Parser.from_string Hypothesis.parse_hypothesisDirective device2

    ()

[<Fact(DisplayName="GEC - GEC CRN Parser - Device Definition Parser")>]
let ``deviceDefinitionParser``()=
    let device0 = "device PRFP(growth,capacity) = { Control(growth,capacity) }"
    let device1 = "device PLacYFPCFP(growth,capacity) = { YFP(PLac,aYFP,growth,capacity) | CFP(PLac,aCFP,growth,capacity) }"
    let dev0 = Parser.from_string Hypothesis.parse_device device0
    let dev1 = Parser.from_string Hypothesis.parse_device device1
    Debug.WriteLine(Hypothesis.deviceDefinition_to_string dev0)
    Debug.WriteLine(Hypothesis.deviceDefinition_to_string dev1)
    ()


[<Fact(DisplayName="GEC - GEC CRN Parser - System Parser")>]
let ``systemParser``()=
    let system0 = """system growth = {
  directive simulation { final=20; points=250 }
  | Growth(growth,tlag)
}"""
    let sys0 = Parser.from_string Hypothesis.parse_crn_system system0
    
    let system1 = """system growth = {
  directive simulation { final=20; points=250 }
  directive device [pTet];
  | Growth(growth,tlag)
}"""
    
    let sys1 = Parser.from_string Hypothesis.parse_crn_system system1

    let system2 = """system growth = {
  control with directive data [R33S175_Y81C76_mRFP1_proc141021] 
  directive device [pTet];
}"""
    let sys2 = Parser.from_string Hypothesis.parse_crn_system system2
    ()

[<Fact(DisplayName="GEC - GEC CRN Parser - IGraph Element Parser")>]
let ``igElementParser``()=
    let element0 = """edge Receivers_growth.Receiver0_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver0_control"""
    let element1 = """edge Relays ->[KR6;KS6;KR12;KS12;nR;nS;aR33;aS175;dR;e76;KGR_76;KGS_76;e81;KGR_81;KGS_81;aCFP;aYFP;] Degrader"""
    let element2 = """edge Auto_control ->[dfp] Standard_control"""
    let element3 = """edge Auto_control ->[r=Fixed;dfp] Standard_control"""
    let element4 = """node Auto { systems = [Auto] }"""
    let element5 = """node Receivers_growth { systems = [Receiver0_growth; Receiver1_growth; Receiver2_growth; Receiver3_growth] }"""
    let element6 = """node Auto_Growth { systems = [growth]; inference = {burnin=1000; samples=1000; partial=true} }"""
    let element7 = """node Auto_Target { systems = [auto]; inference = {burnin=50000; samples=50000} }"""
    
    let elem0 = Parser.from_string Hypothesis.parse_igraphElement element0
    let elem1 = Parser.from_string Hypothesis.parse_igraphElement element1
    let elem2 = Parser.from_string Hypothesis.parse_igraphElement element2
    let elem3 = Parser.from_string Hypothesis.parse_igraphElement element3
    let elem4 = Parser.from_string Hypothesis.parse_igraphElement element4
    let elem5 = Parser.from_string Hypothesis.parse_igraphElement element5
    let elem6 = Parser.from_string Hypothesis.parse_igraphElement element6
    let elem7 = Parser.from_string Hypothesis.parse_igraphElement element7

    ()

[<Fact(DisplayName="GEC - Hypothesis Test")>]
let ``hypothesisTest``()=
    let database = """i723017,pcr,codes(xylR;0.001)
i723024,pcr,codes(phzM;0.001)
e0040,pcr,codes(gfp;0.01)
c0099,pcr,codes(cviR;0.01)
i723025,pcr,codes(phzS;0.001)
i723028,pcr,codes(pca;0.001)
c0051,pcr,codes(cI;0.01)
c0040,pcr,codes(tetR;0.01)
c0080,pcr,codes(araC;0.01)
c0012,pcr,codes(lacI;0.01)
cunknown2,pcr,codes(unknown2;0.001)
c0061,pcr,codes(luxI;0.01)
c0062,pcr,codes(luxR;0.01)
c0079,pcr,codes(lasR;0.01)
c0078,pcr,codes(lasI;0.01)
cunknown3,pcr,codes(ccdB;0.005)
cunknown4,pcr,codes(ccdA;0.1)
i723020,prom,pos(toluene::xylR;0.001;0.001;1.0);con(0.0001)
r0051,prom,neg(cI;1.0;0.5;0.00005);con(0.12)
r0040,prom,neg(tetR;1.0;0.5;0.00005);con(0.09)
runknown1,prom,neg(unknown1;1.0;0.005;0.001);con(0.04)
b0034,rbs,rate(0.1)
b0015,ter
j06504,pcr,codes(mCherry;0.1)
PRFP,device,components[P;R;RFP;T]
PTetRS100LuxR,device,components[PTet;RS100;LuxR;T] 
DRR33S175,device,components[PRFP;PTetRS100LuxR]
DRRS,device,components[DRR33S175|LuxR]
EC10G,device,components[P]
PLPL,device,components[P;R;eYFP;T;P;R;eCFP;T]"""
    
    let hypothesis_content = """directive simulator sundials
directive parameters [ 
	C6=0.0; C12=0.0; c0 = 0.002; r = 1.0; K = 2.0; rc = 1000.0; tlag=0.0; tau=0.0;
	// Autofluorescence
	autoYFP=1e0, 	{ interval=Log; distribution=Uniform(1e-3,1e3); variation=Random };
	autoCFP=1e0, 	{ interval=Log; distribution=Uniform(1e-3,1e3); variation=Random };
	// FP
	dRFP=0.1,  { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random };
	dCFP=1e-2,  { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random };
	dYFP=1e-2,  { interval=Log; distribution=Uniform(1e-3,1e0); variation=Random };
]
directive inference { thin=100; noise_model = proportional }
directive simulation {multicore=True}
directive rates [
	growth =  [grow]*r*(1 - [x] / K);
	capacity = rc;
]

module CFP(a) = {
    ->[[capacity]*a] cfp | 
    cfp ->{dCFP} | 
    cfp ->[[growth]*[cfp]] 
} 
module YFP(a) = {
    ->[[capacity]*a] yfp | 
    yfp ->{dYFP} | 
    yfp ->[[growth]*[yfp]] 
} 
module cells() = { 
	init x c0 | 
	init grow 1 @ tlag |
	init c6 C6 @ tau | 
	init c12 C12 @ tau |
	->[[growth]*[x]] x
}
module autofluorescence() = {	
	->[[capacity]*autoYFP] f530 |
	f530 ->[[growth]*[f530]] |
	->[[capacity]*autoCFP] f480 |
	f480 ->[[growth]*[f480]]
}
module Control() = { 
//| Growth(growth,tlag)
  | fp ->[[growth]*[fp]] // Dilution
  | ->[[capacity]] fp    // Transcription/translation		
  | fp ->{dRFP}          // Degradation
}

device PRFP() = { Control() }
device PLPL() = { YFP(aYFP) | CFP(aCFP) }

system growth = {
	directive simulation { final=36.0; points=250; plots=[[x]+x0] }
	directive parameters [ 
		r = 1, { interval=Real; distribution=Uniform(0.1,10); variation=Multiple }; 
		K = 2, { interval=Real; distribution=Uniform(0.1,5); variation=Multiple }; 
		tlag = 1, { interval=Real; distribution=Uniform(0,10); variation=Multiple }; 
		x0=0.1,{ interval=Real; distribution=Uniform(0.0,0.2); variation=Random };
	]
	directive rates [growth =  [grow]*r*(1 - [x] / K)]
	cells()
}
system control = {
	directive simulation { final=36.0; points=250; plots=[[x]*[fp]+f0]; plotcolours=["#FF0000"] }
	directive parameters [ 
		r = 1, { interval=Real; distribution=Uniform(0.1,10); variation=Multiple }; 
		K = 2, { interval=Real; distribution=Uniform(0.1,5); variation=Multiple }; 
		tlag = 1, { interval=Real; distribution=Uniform(0,10); variation=Multiple }; 
		rc=1e2,   { interval=Log; distribution=Uniform(1e0,1e5); variation=Multiple };
		f0=100.0,{ interval=Real; distribution=Uniform(0.0,10000.0); variation=Random };
	]
	directive rates [growth =  [grow]*r*(1 - [x] / K);capacity = rc]	
	directive device [PRFP]
	cells()
}
system auto = {
	directive simulation { final=36.0; points=250; plots=[[x]*[f530]+yb0; [x]*[f480]+cb0]; plotcolours=["#FFDF00"; "#ADD8E6"] }
	directive parameters [ 
		r = 1, { interval=Real; distribution=Uniform(0.1,10); variation=Multiple }; 
		K = 2, { interval=Real; distribution=Uniform(0.1,5); variation=Multiple }; 
		tlag = 1, { interval=Real; distribution=Uniform(0,10); variation=Multiple }; 
		rc=1e2,   { interval=Log; distribution=Uniform(1e0,1e5); variation=Multiple };
		yb0=1e3,  { interval=Real; distribution=Uniform(0.0,5e3); variation=Random };
		cb0=1e3,  { interval=Real; distribution=Uniform(0.0,1e4); variation=Random };
	]		
	directive rates [growth =  [grow]*r*(1 - [x] / K);capacity = rc]	
	| cells()
	| autofluorescence()
}
system prpr = {
	directive simulation { final=36.0; points=250; plots=[[x]*([yfp]+[f530])+yb0; [x]*([cfp]+[f480])+cb0]; plotcolours=["#FFDF00"; "#ADD8E6"] }
	directive parameters [ 
		r = 1, { interval=Real; distribution=Uniform(0.1,10); variation=Multiple }; 
		K = 2, { interval=Real; distribution=Uniform(0.1,5); variation=Multiple }; 
		tlag = 1, { interval=Real; distribution=Uniform(0,10); variation=Multiple }; 
		rc=1e2,   { interval=Log; distribution=Uniform(1e0,1e5); variation=Multiple };
		aCFP=1e3,   { interval=Log; distribution=Uniform(1e0,1e5);  variation=Random };
		aYFP=1e3, 	{ interval=Log; distribution=Uniform(1e0,1e5);  variation=Random };
		yb0=1e3,  { interval=Real; distribution=Uniform(0.0,5e3); variation=Random };
		cb0=1e3,  { interval=Real; distribution=Uniform(0.0,1e4); variation=Random };	
	]
	directive rates [growth =  [grow]*r*(1 - [x] / K);capacity = rc]	
	directive device [PLPL]
	| cells()
	| autofluorescence()
}

//node Auto_Growth { systems = [growth] }
//node Auto_Control { systems = [control] }

node Auto_Growth { systems = [growth]; inference = {burnin=1000; samples=1000; partial=true} }
node Auto_Control { systems = [control]; inference = {burnin=1000; samples=1000; partial=true} }
node Auto_Target { systems = [auto]; inference = {burnin=50000; samples=50000} }

node PRPR_Growth { systems = [growth]; inference = {burnin=1000; samples=1000; partial=true} }
node PRPR_Control { systems = [control]; inference = {burnin=100000; samples=100000; partial=true} }
node PRPR_Target { systems = [prpr]; inference = {burnin=100000; samples=100000} }

edge Auto_Growth.growth ->[r=Fixed;K=Fixed;tlag=Fixed] Auto_Control.control
edge Auto_Control.control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Auto_Target.auto

edge PRPR_Growth.growth ->[r=Fixed;K=Fixed;tlag=Fixed] PRPR_Control.control
edge PRPR_Control.control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] PRPR_Target.prpr

edge Auto_Control ->[dRFP=TruncatedNormal] PRPR_Control
edge Auto_Target ->[autoYFP=TruncatedNormal;autoCFP=TruncatedNormal] PRPR_Target

"""
    let lib = Parser.from_string Database.parse database
    let hypothesis = Parser.from_string Hypothesis.parse_hypothesis_content hypothesis_content

    Debug.WriteLine(Hypothesis.hypothesis_to_crn_program hypothesis lib.devices)

    ()

