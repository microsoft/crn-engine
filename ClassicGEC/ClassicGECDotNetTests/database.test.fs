module Microsoft.Research.GEC.DatabaseTest

open Microsoft.Research.GEC.Database
open FSBOL
open FSBOL.SBOLDocument
open FSBOL.XmlSerializer
open Xunit
open FsUnit.Xunit
open System.Diagnostics
open System.Xml
open System.IO
open System.Text

[<Fact(DisplayName="GEC - Database Parser")>]
let databaseParserText() = 
    let sampledb0 = 
        "i723017,pcr,codes(xylR;0.001)\n"+
        "i723024,pcr,codes(phzM;0.001)\n"+
        "e0040,pcr,codes(gfp;0.01)\n"+
        "c0099,pcr,codes(cviR;0.01)\n"+
        "i723025,pcr,codes(phzS;0.001)\n"+
        "i723028,pcr,codes(pca;0.001)\n"+
        "c0051,pcr,codes(cI;0.01)\n"+
        "c0040,pcr,codes(tetR;0.01)\n"+
        "c0080,pcr,codes(araC;0.01)\n"+
        "c0012,pcr,codes(lacI;0.01)\n"+
        "cunknown2,pcr,codes(unknown2;0.001)\n"+
        "c0061,pcr,codes(luxI;0.01)\n"+
        "c0062,pcr,codes(luxR;0.01)\n"+
        "c0079,pcr,codes(lasR;0.01)\n"+
        "c0078,pcr,codes(lasI;0.01)\n"+
        "cunknown3,pcr,codes(ccdB;0.005)\n"+
        "cunknown4,pcr,codes(ccdA;0.1)\n"+
        "i723020,prom,pos(toluene::xylR;0.001;0.001;1.0);con(0.0001)\n"+
        "r0051,prom,neg(cI;1.0;0.5;0.00005);con(0.12)\n"+
        "r0040,prom,neg(tetR;1.0;0.5;0.00005);con(0.09)\n"+
        "runknown1,prom,neg(unknown1;1.0;0.005;0.001);con(0.04)\n"+
        "b0034,rbs,rate(0.1)\n"+
        "b0015,ter\n" +
        "j06504,pcr,codes(mCherry;0.1)"
    
    let sampledb1 = 
        "i723017,pcr,codes(xylR;0.001)\n"+
        "i723024,pcr,codes(phzM;0.001)\n"+
        "e0040,pcr,codes(gfp;0.01)\n"+
        "c0099,pcr,codes(cviR;0.01)\n"+
        "i723025,pcr,codes(phzS;0.001)\n"+
        "i723028,pcr,codes(pca;0.001)\n"+
        "c0051,pcr,codes(cI;0.01)\n"+
        "c0040,pcr,codes(tetR;0.01)\n"+
        "c0080,pcr,codes(araC;0.01)\n"+
        "c0012,pcr,codes(lacI;0.01)\n"+
        "cunknown2,pcr,codes(unknown2;0.001)\n"+
        "c0061,pcr,codes(luxI;0.01)\n"+
        "c0062,pcr,codes(luxR;0.01)\n"+
        "c0079,pcr,codes(lasR;0.01)\n"+
        "c0078,pcr,codes(lasI;0.01)\n"+
        "cunknown3,pcr,codes(ccdB;0.005)\n"+
        "cunknown4,pcr,codes(ccdA;0.1)\n"+
        "i723020,prom,pos(toluene::xylR;0.001;0.001;1.0);con(0.0001)\n"+
        "r0051,prom,neg(cI;1.0;0.5;0.00005);con(0.12)\n"+
        "r0040,prom,neg(tetR;1.0;0.5;0.00005);con(0.09)\n"+
        "runknown1,prom,neg(unknown1;1.0;0.005;0.001);con(0.04)\n"+
        "b0034,rbs,rate(0.1)\n"+
        "b0015,ter\n" +
        "j06504,pcr,codes(mCherry;0.1)\n"+
        "PRFP,device,components[P;R;RFP;T]\n" + 
        "PTetRS100LuxR,device,components[PTet;RS100;LuxR;T]" 
        
        
    let from_string (s:string) = Parser.from_string parse s
    let table0 = from_string sampledb0
    let table1 = from_string sampledb1
    let sbol = Database.convertTableToSBOLDocument table0
    //Debug.WriteLine(sbolXmlString sbol)

    let fwsw = new StreamWriter("gecSBOLdb.xml",false)
    let fwxwSettings = new XmlWriterSettings()
    fwxwSettings.Indent <- true
    fwxwSettings.Encoding <- Encoding.UTF8
    let fwxw = XmlWriter.Create(fwsw,fwxwSettings)
    (XmlSerializer.sbolToXml sbol).WriteTo(fwxw)
    fwxw.Close()

    Assert.Equal(table0.parts.Count,24)
    Assert.True(table0.parts.ContainsKey("b0015"))
    Assert.True(table0.parts.ContainsKey("b0034"))
    Assert.True(table0.parts.ContainsKey("i723020"))
    Assert.True(table0.parts.ContainsKey("r0040"))
    Assert.True(table0.parts.ContainsKey("c0012"))

    Assert.Equal(table0.devices.Length,0)
    Assert.Equal(table1.devices.Length,2)

    Debug.WriteLine("END OF TEST")


[<Fact(DisplayName="GEC - Database Parser - PCR")>]
let ``PCRParserTest``() = 
    let pcrEntry = 
        "i723017,pcr,codes(xylR; 0.001)"
        
    let from_string (s:string) = Parser.from_string Database.partParser s
    let dnacomp = from_string pcrEntry
    let (id,pcr) = match dnacomp with
                   | Part(x,y) -> (x,y)
                   | _ -> failwith "Unexpected Device found"
                     
    Assert.Equal(id,"i723017")
    match pcr with
    | Database.PCR(Database.CODES(codes,rate)) -> 
        Assert.Equal(codes.Length,1)
        Assert.Equal(codes.Head,"xylR")
        Assert.Equal(rate,0.001)
    | _ -> 
        Debug.WriteLine("Error")
        Assert.Equal(true,false)

    Debug.WriteLine("End of Test")


[<Fact(DisplayName="GEC - Database Parser - Regulation promoter")>]
let ``RegulationPromParserTest``() = 
    let negativeRegulation = 
        "r0051,prom,neg(cI;1.0;0.5;0.00005);con(0.12)"
        
    let from_string (s:string) = Parser.from_string Database.partParser s
    let dnacomp = from_string negativeRegulation
    let (id,negProm) = 
      match dnacomp with 
      | Part(x,y) -> (x,y)
      | _ -> failwith "Unexpected Device found"

    Assert.Equal(id,"r0051")
    match negProm with
    | Database.PROM(props) -> 
        Assert.Equal(props.Length,2)
        match props with
            first::remaining ->
                Assert.Equal(props.Head,Database.NEG(["cI"],1.0,0.5,0.00005))
                Assert.Equal(remaining.Head,Database.CON(0.12))
          | _ -> failwith ""
    | _ -> 
        Debug.WriteLine("Unexpected Part type found")
        Assert.Equal(true,false)

    Debug.WriteLine("End of Test")

[<Fact(DisplayName="GEC - Database Parser - Terminator")>]
let ``TerminatorParserTest``() = 
    let ter = "b0015,ter"
    
    let from_string (s:string) = Parser.from_string Database.partParser s
    let dnacomp = from_string ter
    let (id,ter) = 
      match dnacomp with 
      | Part(x,y) -> (x,y)
      | _ -> failwith "Unexpected Device found"

    Assert.Equal(id,"b0015")
    Assert.Equal(ter,Database.TER)

    Debug.WriteLine("End of Test")


