// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.InferenceSiteGraphTest

open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.InferenceSiteGraph

[<Fact(DisplayName = "Inference Graph - compile CRN")>]
let crnCompile () = 
  let code = """<->X"""
  let graph = from_string code
  Assert.Equal (1, graph.nodes.Count)

[<Fact(DisplayName = "Inference Graph - update edge (local multiple)")>]
let update_edge_local () =
  let code = """
    directive parameters [i=0]
    directive sweeps [ a = [i = [1;2;3;4;5]] ]
    system Growth  = { 
      directive simulation {sweeps=[a]}
      directive parameters [
        k=1.0, {variation=Multiple}
      ]
      | A ->{k} B 
    }
    system Control = { 
      directive simulation {sweeps=[a]}
      directive parameters [
        k=1.0, {variation=Multiple}
      ]
      | B ->{k} C 
    }    

    node Growth  {systems = [Growth]}
    node Control {systems = [Control]}

    edge Growth.Growth  ->[k=Fixed] Control.Control
    """
  let parsed = from_string code
  let graph = expandAndLift parsed
  let ps = [1..5] |> List.map (sprintf "Growth.k_%d") |> List.mapi (fun i n -> n, { ParameterSummary.defaults with name=n; mle = 1.0+0.1*(float i) })
  let result : Inference.Summary = { mcmc = Inference.McmcSummary.defaults; parameters = Map.ofList ps }
  let updated = updateGraph graph ("Growth",result)
  let p2s = updated.nodes.["Control"].systems.Head.settings.parameters |> List.map (fun p -> p.value)
  let p1s = 1.0 :: (List.map (fun (k,v) -> v.mle) ps)
  Assert.Equal<float list>(p1s, p2s)

[<Fact(DisplayName = "Inference Graph - update edge (global multiple)", Skip="Not supporting global multiples at present")>]
let update_edge_global () =
  let code = """
    directive sweeps [ a = [i = [1;2;3;4;5]] ]
    directive parameters [
      k=1.0, {variation=Multiple}
    ]
    system Growth  = { 
      directive simulation {sweeps=[a]}
      | A ->{k} B 
    }
    system Control = { 
      directive simulation {sweeps=[a]}
      | B ->{k} C 
    }    
    edge Growth  ->[k=Fixed] Control
    """
  let parsed = from_string code
  let graph = expandAndLift parsed
  let ps = [1..5] |> List.map (sprintf "k_%d") |> List.mapi (fun i n -> n, { ParameterSummary.defaults with name=n; mle = 1.0+0.1*(float i) })
  let result : Inference.Summary = { mcmc = Inference.McmcSummary.defaults; parameters = Map.ofList ps }
  let updated = updateGraph graph ("Growth",result)
  let p2s = updated.nodes.["Control"].systems.Head.settings.parameters |> List.map (fun p -> p.value)
  let p1s = 1.0 :: (List.map (fun (k,v) -> v.mle) ps)
  Assert.Equal<float list>(p1s, p2s)
  
[<Fact(DisplayName = "Inference Graph - ancestors subgraph of a node")>]
let ancestorsSubgraph () =
  let code = 
    """
    system Growth  = { | A ->{k} B }
    system Control = { | B ->{k} C }
    system Target  = { | C ->{k} D } 

    node Growth  {systems = [Growth]}
    node Control {systems = [Control]}
    node Target  {systems = [Target]}

    edge Growth.Growth  ->[k=Fixed] Control.Control
    edge Control.Control ->[k=Fixed] Target.Target
    """
  let graph     = InferenceSiteGraph.from_string code
  let subgraph  = InferenceSiteGraph.ancestorsSubgraph "Control" graph
  Assert.Equal (2, subgraph.nodes.Count)
  Assert.True  (subgraph.nodes.ContainsKey "Growth")
  Assert.True  (subgraph.nodes.ContainsKey "Control")
  Assert.False (subgraph.nodes.ContainsKey "Target")
  Assert.Equal (1, subgraph.edges.Count)
  Assert.True  (subgraph.edges.ContainsKey (InferenceSiteGraph.SystemLoc ("Growth", "Growth")))

[<Fact(DisplayName = "Inference Graph - Parsing")>]
let parseInferenceGraph () =
  let code = """directive simulator sundials
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
]

module LuxR(aR,growth,capacity) = {
  | ->[[capacity]*aR*[PTet]] luxR
  | luxR ->{dR}
  | luxR ->[[growth]*[luxR]]
}
module LasR(aS,growth,capacity) = {
  | ->[[capacity]*aS*[PLac]] lasR
  | lasR ->{dR}
  | lasR ->[[growth]*[lasR]] 
} 
module TetR(P,growth,capacity) = {
  | ->[[capacity]*aT*[P]] tetR
  | tetR ->{dT}
  | tetR ->[[growth]*[tetR]]
  | tetR ->{iA*ATC}
}
module LacI(P,growth,capacity) = {
  | ->[[capacity]*aL*[P]] lacI
  | lacI ->[[growth]*[lacI]]
  | lacI ->{dL}
  | lacI ->{iI*IPTG}
}
module LuxI(P,growth,capacity) = {
  | ->[[capacity]*[P]] luxI
  | luxI ->{dluxI}
  | luxI ->[[growth]*[luxI]]
  | ->[kC6*[capacity]*[x]*[luxI]/(1+[luxI]/Klux)] c6
} 
module LasI(P,growth,capacity) = {
  | ->[[capacity]*[P]] lasI
  | lasI ->{dlasI}
  | lasI ->[[growth]*[lasI]]
  | ->[kC12*[capacity]*[x]*[lasI]/(1+[lasI]/Klas)] c12 
} 
module CFP(P,a,growth,capacity) = {
  | ->[[capacity]*a*[P]] cfp
  | cfp ->{dCFP}
  | cfp ->[[growth]*[cfp]] 
} 
module YFP(P,a,growth,capacity) = {
  | ->[[capacity]*a*[P]] yfp
  | yfp ->{dYFP}
  | yfp ->[[growth]*[yfp]] 
} 
module Growth(growth,tlag) = {
  | init x c0 
  | init grow 1 @ tlag 
  | ->[[growth]*[x]] x 
}
module Control(growth,tlag,capacity) = { 
  | Growth(growth,tlag)
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
module DR(aR,aS,growth,tlag,capacity) = {
  | cells(growth,tlag,capacity)
  | LuxR(aR,growth,capacity)
  | LasR(aS,growth,capacity)
  | YFP(P81,aYFP,growth,capacity)
  | CFP(P76,aCFP,growth,capacity)
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

system growth = {
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}

system control = {
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ r=1; K=2; tlag=1; rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Auto_growth      = { 
  directive data [R33S175_Y81C76_OD_proc141021] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}
system Standard_growth  = { 
  directive data [R33S175_Y81C76_OD_proc141021] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}
system Relay1_growth    = { 
  directive data [R33S175_Y81C76_OD_proc141021] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}
system Relay2_growth    = { 
  directive data [R33S175_Y81C76_OD_proc141021] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}

system Arabinose_growth = { 
  directive data [R33S175_Y81C76_OD_proc141021] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}

system Degrader_growth  = { 
  directive data [R33S175_Y81C76_OD_proc141021] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}

system Receiver0_growth = { 
  directive data [R33S175_Y81C76_OD_proc141021] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}
system Receiver1_growth = { 
  directive data [R33S32_Y81C76_OD_proc140916] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}
system Receiver2_growth = { 
  directive data [RS100S32_Y81C76_OD_proc140916] 
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}
system Receiver3_growth = {
  directive data [Pcat_Y81C76_OD_proc141006]
  directive simulation { plots = [plot_od]; sweeps = [sweepC6C12double] }
  directive parameters [ 
    r = 1, { distribution=Uniform(0.1,10); variation=Multiple }; 
    K = 2, { distribution=Uniform(0.1,5); variation=Multiple }; 
    tlag = 1, { distribution=Uniform(0,10); variation=Multiple }; 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K) ]
  | Growth(growth,tlag)
}

system Auto_control = { 
  directive data [R33S175_Y81C76_mRFP1_proc141021] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Standard_control = { 
  directive data [R33S175_Y81C76_mRFP1_proc141021] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Relay1_control = { 
  directive data [R33S175_Y81C76_mRFP1_proc141021] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Relay2_control = { 
  directive data [R33S175_Y81C76_mRFP1_proc141021] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Arabinose_control = { 
  directive data [R33S175_Y81C76_mRFP1_proc141021] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
                         K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
                         tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
                         rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Degrader_control = { 
  directive data [R33S175_Y81C76_mRFP1_proc141021] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}



system Receiver0_control = { 
  directive data [R33S175_Y81C76_mRFP1_proc141021] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Receiver1_control = { 
  directive data [R33S32_Y81C76_mRFP1_proc140916] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}
system Receiver2_control = { 
  directive data [RS100S32_Y81C76_mRFP1_proc140916] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}
system Receiver3_control = {
  directive data [Pcat_Y81C76_mRFP1_proc141006] 
	directive simulation { plots=[plot_fp]; sweeps = [sweepC6C12double] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  | Control(growth,tlag,capacity)
}

system Auto = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ] 
  directive data [ Pcat_Y81C76_mRFP1_proc141006 ] 
  | cells(growth,tlag,capacity)
}
system Standard = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]  
  directive data [ Pcat_Y81C76_mRFP1_proc141006 ] 
  | cells(growth,tlag,capacity)
  | YFP(PLac,aYFP,growth,capacity)
  | CFP(PLac,aCFP,growth,capacity)
}

system Receiver0 = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  directive data [R33S175_Y81C76_EYFP_ECFP_proc141021] 
  | DR(aR33,aS175,growth,tlag,capacity) 
}
system Receiver1 = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ] 
  directive data [R33S32_Y81C76_EYFP_ECFP_proc140916] 
  | DR(aR33,aS32,growth,tlag,capacity) 
}
system Receiver2 = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  directive data [RS100S32_Y81C76_EYFP_ECFP_proc140916] 
  | DR(aRS100,aS32,growth,tlag,capacity) 
}
system Receiver3 = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12double] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]  
  directive data [Pcat_Y81C76_EYFP_ECFP_proc141006] 
  | DR(1,1,growth,tlag,capacity) 
}

system Relay1= {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]  
  directive data [ Pcat_Y81C76_mRFP1_proc141006 ] 
  | DR(aR33,aS175,growth,tlag,capacity) 
  | LasI(P76,growth,capacity) 
}
system Relay2 = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } 
  ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]
  directive data [ Pcat_Y81C76_mRFP1_proc141006 ] 
  | DR(aR33,aS175,growth,tlag,capacity) 
  | LuxI(P81,growth,capacity)
}
system Arabinose = {
  directive simulation { plots=[plot_yfp]; sweeps = [sweepC6C12] }
  directive parameters [ r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
                         K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
                         tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
                         rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ] 
  directive data [ RS100S32_Y81C76_EYFP_ECFP_proc140916 ] 
  | cells(growth,tlag,capacity)
  | YFP(PBad,aYFP,growth,capacity) 
}
system Degrader = {
  directive simulation { plots=[plot_yfp; plot_cfp]; sweeps = [sweepC6C12] }
  directive parameters [ 
    r=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    K=2, { distribution=Uniform(1,100000); variation=Multiple }; 
    tlag=1, { distribution=Uniform(1,100000); variation=Multiple }; 
    rc=100, { distribution=Uniform(1,100000); variation=Multiple } ]
  directive rates [ growth = [grow]*r*(1-[x]/K); capacity = rc ]  
  directive data [ Pcat_Y81C76_mRFP1_proc141006 ] 
  | DR(aR33,aS175,growth,tlag,capacity) 
  | AiiA(PBad,1.0,growth,capacity) 
}

node Receivers_growth { systems = [Receiver0_growth; Receiver1_growth; Receiver2_growth; Receiver3_growth] }
node Receivers_control { systems = [Receiver0_control; Receiver1_control; Receiver2_control; Receiver3_control] }
node Receivers { systems = [Receiver0; Receiver1; Receiver2; Receiver3] }

edge Receivers_growth.Receiver0_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver0_control
edge Receivers_control.Receiver0_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver0
edge Receivers_growth.Receiver1_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver1_control
edge Receivers_control.Receiver1_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver1
edge Receivers_growth.Receiver2_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver2_control
edge Receivers_control.Receiver2_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver2
edge Receivers_growth.Receiver3_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver3_control
edge Receivers_control.Receiver3_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver3



(*
//Receivers only
node Receivers_growth { systems = [Receiver0_growth; Receiver1_growth; Receiver2_growth; Receiver3_growth] }
node Receivers_control { systems = [Receiver0_control; Receiver1_control; Receiver2_control; Receiver3_control] }
node Receivers { systems = [Receiver0; Receiver1; Receiver2; Receiver3] }

edge Receivers_growth.Receiver0_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver0_control
edge Receivers_control.Receiver0_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver0
edge Receivers_growth.Receiver1_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver1_control
edge Receivers_control.Receiver1_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver1
edge Receivers_growth.Receiver2_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver2_control
edge Receivers_control.Receiver2_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver2
edge Receivers_growth.Receiver3_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver3_control
edge Receivers_control.Receiver3_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver3


// Assume normal distribution by default for chained parameters
// NB parameters with variation = Multiple also need to be expanded in the inference graph
// Nodes
node Auto_growth { systems = [Auto_growth] }
node Auto_control { systems = [Auto_control] }
node Auto { systems = [Auto] }

node Standard_growth { systems = [Standard_growth] }
node Standard_control { systems = [Standard_control] }
node Standard { systems = [Standard] }

//node Receivers_growth { systems = [Receiver0_growth; Receiver1_growth; Receiver2_growth; Receiver3_growth] }
//node Receivers_control { systems = [Receiver0_control; Receiver1_control; Receiver2_control; Receiver3_control] }
//node Receivers { systems = [Receiver0; Receiver1; Receiver2; Receiver3] }

node Relays_growth { systems = [Relay1_growth; Relay2_growth] }
node Relays_control { systems = [Relay1_control; Relay2_control] }
node Relays { systems = [Relay1; Relay2] }

node Degrader_growth { systems = [Degrader_growth] }
node Degrader_control { systems = [Degrader_control] }
node Degrader{ systems = [Degrader] }

node Auto_growth { systems = [Auto_growth] }
node Auto_control { systems = [Auto_control] }
node Auto { systems = [Auto] }

// Control and Growth parameters
edge Auto_growth.Auto_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Auto_control.Auto_control
edge Auto_control.Auto_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Auto.Auto

edge Standard_growth.Standard_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Standard_control.Standard_control
edge Standard_control.Standard_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Standard.Standard

edge Receivers_growth.Receiver0_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver0_control
edge Receivers_control.Receiver0_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver0
edge Receivers_growth.Receiver1_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver1_control
edge Receivers_control.Receiver1_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver1
edge Receivers_growth.Receiver2_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver2_control
edge Receivers_control.Receiver2_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver2
edge Receivers_growth.Receiver3_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Receivers_control.Receiver3_control
edge Receivers_control.Receiver3_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Receivers.Receiver3

edge Relays_growth.Relay1_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Relays_control.Relay1_control
edge Relays_control.Relay1_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Relays.Relay1
edge Relays_growth.Relay2_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Relays_control.Relay2_control
edge Relays_control.Relay2_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Relays.Relay2

edge Arabinose_growth.Arabinose_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Arabinose_control.Arabinose_control
edge Arabinose_control.Arabinose_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Arabinose.Arabinose

edge Degrader_growth.Degrader_growth ->[r=Fixed;K=Fixed;tlag=Fixed] Degrader_control.Degrader_control
edge Degrader_control.Degrader_control ->[r=Fixed;K=Fixed;tlag=Fixed;rc=Fixed] Degrader.Degrader

// Other parameters
edge Auto_control ->[dfp] Standard_control
edge Standard_control ->[dfp] Receivers_control
edge Receivers_control ->[dfp] Relays_control
edge Relays_control ->[dfp] Arabinose_control
edge Arabinose_control ->[dfp] Degrader_control

edge Auto ->[autoYFP;autoCFP] Standard
edge Standard ->[autoYFP;autoCFP;dCFP;dYFP] Receivers
edge Receivers ->[autoYFP;autoCFP;dCFP;dYFP; KR6;KS6;KR12;KS12;nR;nS;aR33;aS175;dR;e76;KGR_76;KGS_76;e81;KGR_81;KGS_81;aCFP;aYFP] Relays
edge Relays ->[KR6;KS6;KR12;KS12;nR;nS;aR33;aS175;dR;e76;KGR_76;KGS_76;e81;KGR_81;KGS_81;aCFP;aYFP;] Degrader 
edge Relays ->[autoYFP;autoCFP;dCFP;dYFP] Arabinose
edge Arabinose ->[autoYFP;autoCFP;dCFP;dYFP;KAra;nA;eA] Degrader  *)
"""
  // let expected = failwith ""
  let systems = [ "Auto_growth"; "Auto_control"; "Auto"; "Standard_growth"; "Standard_control"; "Standard"
                ; "Receiver0_growth"; "Receiver1_growth"; "Receiver2_growth"; "Receiver3_growth"
                ; "Receiver0_control"; "Receiver1_control"; "Receiver2_control"; "Receiver3_control"
                ; "Receiver0"; "Receiver1"; "Receiver2"; "Receiver3"
                ; "Relay1_growth"; "Relay2_growth"
                ; "Relay1_control"; "Relay2_control"
                ; "Relay1"; "Relay2"
                ; "Degrader_growth"
                ; "Degrader_control"
                ; "Degrader" ]
              |> List.map (fun x-> x, { Crn.empty with name = x })
              |> Map.ofList

  let parsed   = InferenceSiteGraph.from_string code 

  // debug strings
  let models = 
    parsed.nodes 
    |> Map.toList 
    |> List.map (snd >> fun x -> (sprintf "node %s{\n" x.top.name) + x.string()+ "\n}\n") 
    |> String.concat "\n"

  let graph =
    parsed.edges 
    |> Map.toList
    |> List.map (fun (source, targets) -> 
        let printLoc loc =
          match loc with 
          | InferenceSiteGraph.NodeLoc nID -> nID
          | InferenceSiteGraph.SystemLoc (nID, sID) -> nID + "." + sID
        let printEdgeProp prop = 
          match prop with 
          | InferenceSiteGraph.Normal          -> "normal"
          | InferenceSiteGraph.Fixed2          -> "fixed"
          | InferenceSiteGraph.TruncatedNormal -> "truncated"
          | InferenceSiteGraph.LogNormal       -> "lognormal"

        let sourceTxt = printLoc source
        targets 
        |> List.map (fun (target, props) ->
          let targetTxt = printLoc target
          let propsTxt =
            props 
            |> List.map (fun (x,y,z) -> sprintf "%s/%s : %s"  x y (printEdgeProp z))
            |> String.concat "; "
          sprintf "edge %s ->[%s] %s" sourceTxt propsTxt targetTxt 
          )
        |> String.concat "\n"
        )
    |> String.concat "\n"
  
  //let res = InferenceSiteGraph.infer parsed
  ()