// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.FSBOL.CircuitTest

open Microsoft.Research.FSBOL.Component
open Microsoft.Research.FSBOL.ComponentDefinition
open Microsoft.Research.FSBOL.Sequence
open Microsoft.Research.FSBOL.Terms
open Microsoft.Research.FSBOL.Range
open Microsoft.Research.FSBOL.SequenceAnnotation
open Microsoft.Research.FSBOL.FunctionalComponent
open Microsoft.Research.FSBOL.Interaction
open Microsoft.Research.FSBOL.ModuleDefinition
open Microsoft.Research.FSBOL.Participation

open Microsoft.Research.FSBOL.SBOLDocument

open Microsoft.Research.FSBOL.XmlSerializer
open Microsoft.Research.FSBOL.JsonSerializer

open Xunit
open FsUnit.Xunit
open System.Diagnostics
open System.Xml
open System.IO
open System.Text


[<Fact>]
let ``CreateCircuit``() = 
    let urlPrefix = "http://www.microsoft.com/gec"
    let version = "1"

    let pTetSequence = "aattccggggaaaa"
    let rbsSequence = "aaaaaatttttggggg"
    let tetRSequence = "aatttaaatttatatatatattaa"
    let termSequence = "aaatttttttttgc"

    let pTetSeq = new Sequence("pTet_sequence",urlPrefix,"r0040_sequence",version,pTetSequence,Terms.dnasequence)
    let rbsSeq = new Sequence("rbs_sequence",urlPrefix,"b0034_sequence",version,rbsSequence,Terms.dnasequence)
    let tetRSeq = new Sequence("tetR_sequence",urlPrefix,"c0040_sequence",version,tetRSequence,Terms.dnasequence)
    let termSeq = new Sequence("term_sequence",urlPrefix,"b0015_sequence",version,termSequence,Terms.dnasequence)
    
    let pTetCD = new ComponentDefinition("pTet",urlPrefix,"r0040",version,[Terms.dnaRegion],[Terms.promoter],[pTetSeq],[],[])
    let rbsCD = new ComponentDefinition("rbs",urlPrefix,"b0034",version,[Terms.dnaRegion],[Terms.rbs],[rbsSeq],[],[])
    let tetRCD = new ComponentDefinition("tetR",urlPrefix,"c0040",version,[Terms.dnaRegion],[Terms.cds],[tetRSeq],[],[])
    let termCD = new ComponentDefinition("term",urlPrefix,"b0015",version,[Terms.dnaRegion],[Terms.terminator],[termSeq],[],[])

    let protCD = new ComponentDefinition("tetR_protein",urlPrefix,"c0040p",version,[Terms.protein],[],[],[],[])


    let tuName = "tu1"
    
    let tuPrefix = urlPrefix + "/" + tuName 

    let pTetComp = new Component("pTet",tuPrefix,"r0040",version, Terms.privateAccess, pTetCD.uri)
    let rbsComp = new Component("rbs",tuPrefix,"b0034",version, Terms.privateAccess, rbsCD.uri)
    let tetRComp = new Component("tetR",tuPrefix,"c0040",version, Terms.privateAccess, tetRCD.uri)
    let termComp = new Component("term",tuPrefix,"b0015",version, Terms.privateAccess, termCD.uri)


    //Range then Sequence Annotation
    let pTetRange = new Range("range",tuPrefix + "annotation1/range", "r0040_range",version,1,pTetSequence.Length,Terms.inlineOrientation)
    let rbsRange = new Range("range",tuPrefix + "annotation2/range", "b0034_range",version,pTetRange.endIndex+1, pTetRange.endIndex + rbsSequence.Length ,Terms.inlineOrientation)
    let tetRRange = new Range("range",tuPrefix + "annotation3/range", "c0040_range",version,rbsRange.endIndex+1, rbsRange.endIndex + tetRSequence.Length ,Terms.inlineOrientation)
    let termRange = new Range("range", tuPrefix + "annotation4/range", "b0015_range",version, tetRRange.endIndex+1,tetRRange.endIndex + termSequence.Length,Terms.inlineOrientation)

    let pTetsa = new SequenceAnnotation("annotation1",tuPrefix,"annotation1",version,pTetComp,[Location.Range(pTetRange)])
    pTetsa.description <- "prom"
    let rbssa = new SequenceAnnotation("annotation2",tuPrefix,"annotation2",version,rbsComp,[Location.Range(rbsRange)])
    rbssa.description <- "rbs"
    let tetRsa = new SequenceAnnotation("annotation3",tuPrefix,"annotation3",version,tetRComp,[Location.Range(tetRRange)])
    tetRsa.description <- "pcr"
    let termsa = new SequenceAnnotation("annotation4",tuPrefix,"annotation4",version,termComp,[Location.Range(termRange)])
    termsa.description <- "ter"

    let tetRFC = new FunctionalComponent("tetR_FC",urlPrefix,"c0040_FC",version,Terms.privateAccess,Terms.fcInOut,tetRCD.uri)
    let tetRProtFC = new FunctionalComponent("tetR_protein_FC",urlPrefix,"c0040p_FC",version,Terms.privateAccess,Terms.fcInOut,protCD.uri)
    let tetRCmplxFC = new FunctionalComponent("tetR_complex_FC",urlPrefix,"c0040c_FC",version,Terms.privateAccess,Terms.fcInOut,protCD.uri)
    let pTetFC = new FunctionalComponent("pTet_FC",urlPrefix,"r0040_FC",version,Terms.privateAccess,Terms.fcInOut,pTetCD.uri)
    
    let tetRPart = new Participation("tetR_P",urlPrefix,"c0040_Part",version,[Terms.template],tetRFC)
    let tetRProtPart = new Participation("tetRProt_P",urlPrefix,"c0040p_Part",version,[Terms.product],tetRProtFC)

    let tetRCmplxPart = new Participation("tetRCmplx_P",urlPrefix,"c0040c_Part",version,[Terms.inhibitor],tetRCmplxFC)
    let pTetPart = new Participation("pTet_P",urlPrefix,"r0040_Part",version,[Terms.inhibited],pTetFC)

    let interactionProd = new Interaction("tetR_production_interaction",urlPrefix,"production",version,[Terms.production],[tetRPart;tetRProtPart])
    let interactionInh = new Interaction("pTet_inhibition_interaction",urlPrefix,"inhibition",version,[Terms.inhibition],[tetRCmplxPart;pTetPart])

    let mdProd = new ModuleDefinition("TetR_Production",urlPrefix,"production_md",version,[tetRFC;tetRProtFC],[interactionProd])
    let mdIntr = new ModuleDefinition("pTet_Inhibition",urlPrefix,"inhibition_md",version,[tetRCmplxFC;pTetFC],[interactionInh])


    let tuSeq = new Sequence(tuName + "_sequence",urlPrefix,"tu007_sequence",version, pTetSequence + rbsSequence + tetRSequence + termSequence  ,Terms.dnasequence)

    //let tuCD = new ComponentDefinition(tuName,urlPrefix,"tu007",version,[Terms.dnaRegion],[Terms.engineeredRegion], tuSeq,[pTetComp;rbsComp;tetRComp;termComp],[pTetsa;rbssa;tetRsa;termsa])
    let tuCD = ComponentDefinition.createHigherFunction(tuName,urlPrefix,"tu007",version,[Terms.dnaRegion],[Terms.engineeredRegion],[pTetCD;rbsCD;tetRCD;termCD])

    let xdoc = new XmlDocument();
    let rootXml = xdoc.CreateElement(QualifiedName.rdfQN,Terms.rdfns) 
    xdoc.AppendChild(rootXml) |> ignore
    xdoc.DocumentElement.SetAttribute(QualifiedName.sbolQN,Terms.sbolns)
    xdoc.DocumentElement.SetAttribute(QualifiedName.dctermsQN,Terms.dctermsns)
    xdoc.DocumentElement.SetAttribute(QualifiedName.provQN,Terms.provns)


    rootXml.AppendChild(XmlSerializer.serializeComponentDefinition xdoc pTetCD) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeComponentDefinition xdoc rbsCD) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeComponentDefinition xdoc tetRCD) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeComponentDefinition xdoc termCD) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeComponentDefinition xdoc protCD) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeSequence xdoc pTetSeq) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeSequence xdoc rbsSeq) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeSequence xdoc tetRSeq) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeSequence xdoc termSeq) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeComponentDefinition xdoc tuCD) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeSequence xdoc tuSeq) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeModuleDefinition xdoc mdProd) |> ignore
    rootXml.AppendChild(XmlSerializer.serializeModuleDefinition xdoc mdIntr) |> ignore

    let sw = new StringWriter()
    let xwSettings = new XmlWriterSettings()
    xwSettings.Indent <- true
    xwSettings.Encoding <- Encoding.UTF8
    let xw = XmlWriter.Create(sw,xwSettings)
    xdoc.WriteTo(xw)
    xw.Close()

    let doc = new SBOLDocument([TopLevel.ComponentDefinition(pTetCD);TopLevel.ComponentDefinition(rbsCD);TopLevel.ComponentDefinition(tetRCD);TopLevel.ComponentDefinition(termCD);TopLevel.ComponentDefinition(protCD);TopLevel.ComponentDefinition(tuCD);TopLevel.ModuleDefinition(mdProd);TopLevel.ModuleDefinition(mdIntr)])

    Debug.WriteLine(sbolXmlString doc)

    let docback = SBOLDocumentFromXML (XmlSerializer.serializeSBOLDocument doc)

    let jsonSbol = JsonSerializer.serializeSBOLDocument doc

    let fwsw = new StreamWriter("circuit.xml",false)
    let fwxwSettings = new XmlWriterSettings()
    fwxwSettings.Indent <- true
    fwxwSettings.Encoding <- Encoding.UTF8
    let fwxw = XmlWriter.Create(fwsw,fwxwSettings)
    (XmlSerializer.serializeSBOLDocument doc).WriteTo(fwxw)
    fwxw.Close()


    

    //Debug.WriteLine(sw.ToString())

    Debug.WriteLine("Success")


