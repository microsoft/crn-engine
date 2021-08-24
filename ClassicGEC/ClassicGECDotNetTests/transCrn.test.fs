// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.TransCrnTest

open Microsoft.Research.GEC.GECEngine
open Microsoft.Research.GEC.JSAPI
open Microsoft.Research.GEC.TransCrn
open Microsoft.Research.CRNEngine
open Xunit
open FsUnit.Xunit
open System.Xml
open System.Diagnostics
open Parser
open FSBOL



let databaseText = """i723017,pcr,codes(xylR;0.001)
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
r0080,prom,neg(araC;1.0;0.000001;0.0001);pos(araC::arabinose;0.001;0.001;1.0);con(0.1)
r0011,prom,neg(lacI;1.0;0.5;0.00005);con(0.1)
r0062,prom,pos(lasR::m3OC12HSL;1.0;0.8;0.1);pos(luxR::m3OC6HSL;1.0;0.8;0.1);con(0.01)
r0090,prom,pos(lasR::m3OC12HSL;1.0;0.8;0.1);con(0.01)
r0099,prom,pos(cviR::m3OC6HSL;1.0;0.8;0.1);con(0.01)
b0034,rbs,rate(0.1)
b0015,ter
cunknown5,pcr,codes(ccdA2;10.0)
runknown5,prom,con(10.0)
j06504,pcr,codes(mCherry;0.1)
prpr,device,components[pr;rbs34;eyfp;ter1;pr;rbs34;ecfp;ter1]
drPcat,device,components[pCat;rbs34;luxR;rbs34;lasR;ter1;pLas81;rbs34;eyfp;ter1;plux76;rbs34;ecfp;ter1]
drRS100S32,device,components[pTet;rbss100;luxR;ter1;pLac;rbs32;lasR;ter1;pLas81;rbs34;eyfp;ter1;plux76;rbs34;ecfp;ter1]
drR33S32,device,components[pTet;rbs33;luxR;ter1;pLac;rbs32;lasR;ter1;pLas81;rbs34;eyfp;ter1;plux76;rbs34;ecfp;ter1]
drR33S175,device,components[pTet;rbs33;luxR;ter1;pLac;rbsS175;lasR;ter1;pLas81;rbs34;eyfp;ter1;plux76;rbs34;ecfp;ter1]
relayP76LasI,device,components[pLux76;rbs900;lasI;l3s2p21]
relayP81LuxI,device,components[pLas81;rbs32;luxI;l3s2p21]
pBadYFP,device,components[pBad;rbs34;eyfp;l3s2p21]
lactonase,device,components[pBad;rbs34;aiia;l3s2p21]"""
    
let reactions = """toluene + xylR ->{1.0} toluene::xylR
phzM ~ pca ->{1.0} metPCA
phzS ~ metPCA ->{1.0} pyo
luxR + m3OC6HSL ->{0.5} luxR::m3OC6HSL
lasR + m3OC12HSL ->{0.5} lasR::m3OC12HSL
cviR + m3OC6HSL ->{0.5} cviR::m3OC6HSL
cviR + m3OC12HSL ->{0.5} cviR::m3OC12HSL
luxI ~ ->{1.0} m3OC6HSL
lasI ~ ->{1.0} m3OC12HSL
ccdA ~ ccdB ->{1.0}
c[m3OC6HSL] ->{0.5} m3OC6HSL
m3OC6HSL ->{0.5} c[m3OC6HSL]
c[m3OC12HSL] ->{0.5} m3OC12HSL
m3OC12HSL ->{0.5} c[m3OC12HSL]
luxR::m3OC6HSL ->{1.0} luxR + m3OC6HSL
cviR::m3OC6HSL ->{1.0} cviR + m3OC6HSL
cviR::m3OC12HSL ->{1.0} cviR + m3OC12HSL
lasR::m3OC12HSL ->{1.0} lasR + m3OC12HSL
ccdA2 ~ ccdB ->{0.00001}
lacI + iptg ->{1.0} lacI::iptg
tetR + aTc ->{1.0} tetR::aTc"""



[<Fact(DisplayName="GEC - Constant Ratiometric Test")>]
let constantRatiometricTest() = 
    System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    let (cancel_flag: bool ref) = ref false

    let devices = """device prpr() = { cells() | autofluorescence() | YFP(aYFP) | CFP(aCFP) }
device drPcat() = { cells() | autofluorescence() | LuxR(1.0) | LasR(1.0) | YFP(P81,aYFP) | CFP(P76,aCFP) }
device drRS100S32() = { cells() | autofluorescence() | LuxR(aRS100) | LasR(aS32) | YFP(P81,aYFP) | CFP(P76,aCFP) }
device drR33S32() = { cells() | autofluorescence() | LuxR(aR33) | LasR(aS32) | YFP(P81,aYFP) | CFP(P76,aCFP) }
device drR33S175() = { cells() | autofluorescence() | LuxR(aR33) | LasR(aS175) | YFP(P81,aYFP) | CFP(P76,aCFP) }
device relayP76LasI() = { cells() | autofluorescence() | LuxR(aR33) | LasR(aS175) | LasI(P76) | YFP(P81,aYFP) | CFP(P76,aCFP) }
device relayP81LuxI() = { cells() | autofluorescence() | LuxR(aR33) | LasR(aS175) | LuxI(P81) | YFP(P81,aYFP) | CFP(P76,aCFP) }
device pBadYFP() = { cells() | autofluorescence() | YFP(PBad,aYFP) }
device lactonase() = { cells() | autofluorescence() | LuxR(aR33) | LasR(aS175) | AiiA(PBad,1.0) | YFP(P81,aYFP) | CFP(P76,aCFP)}
"""
   
    let d = Parser.from_string (Parser.many Program.parse_device) devices
    let prog = Parser.from_string Program.parse ProgramParserTest.constant_ratiometric

    let basicsolution = GECEngine.solveGEC cancel_flag ProgramParserTest.constant_ratiometric databaseText reactions
    (*match basicsolution.solution.solution with 
    |Some(_,sol,_,_,_) -> Assert.Equal(4,sol.numSolutions)
    | _ -> failwith "Error in solving basic program"*)
    
    ()

[<Fact(DisplayName="GEC - Solution Count Test")>]
let gecSolutionCount() =    
    
    let db_from_string (s:string) = Parser.from_string Database.parse s
    let partstable = db_from_string databaseText

    let reactionListParser = Parser.sepBy Gecreaction.parseReaction Parser.newline
    let reactiondb_from_string (s:string) = Parser.from_string reactionListParser s

    let createReactionEntry reaction = 
        let (reactionEntry:Gecreaction.t Database.entry) ={value=reaction;enabled=true;comments=""}
        reactionEntry

    let reactiondb = reactiondb_from_string reactions |> List.map (fun(x) -> createReactionEntry(x))

    let table = {partstable with reactions = reactiondb}

    let (cancel_flag: bool ref) = ref false
    
    //basic
    //repressilator
    //repressilatorSimilar
    //repressilatorModules
    //repressilatorModulesSimilar
    //receiverDevice
    //predatorPrey
    //bandDetector

    let basicsolution = GECEngine.solveGEC cancel_flag ProgramParserTest.basic_program  databaseText reactions
    match basicsolution.solution.solution with 
    |Some(_,sol,_,_,_) -> Assert.Equal(4,sol.numSolutions)
    | _ -> failwith "Error in solving basic program"
    

    let repressilatorsolution = GECEngine.solveGEC cancel_flag ProgramParserTest.repressilator  databaseText reactions
    match repressilatorsolution.solution.solution with 
    | Some(_,sol,_,_,_) -> Assert.Equal(24,sol.numSolutions)
    | _ -> failwith "Error in solving repressilator program"

    let repressilatorSimilarsolution = GECEngine.solveGEC cancel_flag ProgramParserTest.repressilator_similar  databaseText reactions
    match repressilatorSimilarsolution.solution.solution with 
    | Some(_,sol,_,_,_) -> Assert.Equal(6,sol.numSolutions)
    | _ -> failwith "Error in solving repressilator Similar program"

    let repressilatorModulessolution = GECEngine.solveGEC cancel_flag ProgramParserTest.repressilator_modules  databaseText reactions
    match repressilatorModulessolution.solution.solution with 
    | Some(_,sol,_,_,_) -> Assert.Equal(24,sol.numSolutions)
    | _ -> failwith "Error in solving repressilator Modules program"

    let repressilatorModulesSimilarsolution = GECEngine.solveGEC cancel_flag ProgramParserTest.repressilator_similar  databaseText reactions
    match repressilatorModulesSimilarsolution.solution.solution with 
    | Some(_,sol,_,_,_) -> Assert.Equal(6,sol.numSolutions)
    | _ -> failwith "Error in solving repressilator Modules similar program"

    let receiverDevicesolution = GECEngine.solveGEC cancel_flag ProgramParserTest.reciever_device  databaseText reactions
    match receiverDevicesolution.solution.solution with 
    | Some(_,sol,_,_,_) -> Assert.Equal(34,sol.numSolutions)
    | _ -> failwith "Error in solving receiver Device program"

    let predatorPreysolution = GECEngine.solveGEC cancel_flag ProgramParserTest.predator_prey  databaseText reactions
    match predatorPreysolution.solution.solution with 
    | Some(_,sol,_,_,_) -> Assert.Equal(16,sol.numSolutions)
    | _ -> failwith "Error in solving predator-Prey program"

    let bandDetectorsolution = GECEngine.solveGEC cancel_flag ProgramParserTest.band_detector  databaseText reactions
    match bandDetectorsolution.solution.solution with 
    | Some(_,sol,_,_,_) -> Assert.Equal(1,sol.numSolutions)
    | _ -> failwith "Error in solving band Detector program"

    Debug.WriteLine("End of test") 



[<Fact(DisplayName="GEC - Abstract CRN Template Test JSAPI")>]
let ``CrnTemplateTest_JSAPI_AbstractCRN``() = 
    let prog = ProgramParserTest.basic_program
    match Microsoft.Research.GEC.JSAPI.compile prog databaseText reactions with 
    | (ClassicGEC solve_result) as sr ->
      let crngec = solve_result.model.nodes |> Map.toSeq |> Seq.head |> snd
      (*let crngec = 
          match solve_result.model with 
          | CRNgui(x) -> x*)
          
      let validateReaction (r:Reaction<string,string,string>) =
          let validateSpecies sp = List.exists (fun (i:Initial<string,string>) -> i.species = sp) crngec.top.initials |> Assert.True
          List.iter validateSpecies (Mset.elements r.reactants) 
          List.iter validateSpecies (Mset.elements r.products) 
          List.iter validateSpecies (Mset.elements r.catalysts)
      List.iter validateReaction crngec.top.reactions
      let crn = crngec.top.to_crn()
      let solution_result = Microsoft.Research.GEC.JSAPI.get_solution sr 1
      let crn = crngec.top.to_crn()
      Debug.WriteLine("End of test")
    | LogicGEC _ -> failwith "Unexpected Logic GEC program."