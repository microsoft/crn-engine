// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.ClassicGECWebServer.GEC

open System.Net.WebSockets
open Messages
open Microsoft.Research.GEC
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI
open Microsoft.Research
open Microsoft.Research.CRNEngineServerLib.Messages
open Microsoft.Research.CRNEngineServerLib.Serialisation
open Microsoft.Research.GEC.GECEngine
open FSBOL
open System.IO
open System.Xml

let mutable currentResult : GEC.JSAPI.solve_result option = None
let mutable currentSolution : GEC.JSAPI.solution_result option = None

let serializeSBOLDocumentToXML sbol =
  let xml = XmlSerializer.sbolToXml sbol
  let stringWriter = new StringWriter()
  let settings = new XmlWriterSettings();
  settings.Indent <- true;
  let xmlWriter = XmlWriter.Create(stringWriter, settings);
  xml.WriteTo(xmlWriter);
  xmlWriter.Flush();
  let ret = stringWriter.GetStringBuilder().ToString();
  xmlWriter.Dispose();
  stringWriter.Dispose();
  ret

let processCompileGECRequest code dbparts dbreactions (webSocket:WebSocket) = 
    let sendObject o = sendObject webSocket o
    try
      match GEC.JSAPI.compile code dbparts dbreactions with 
      | GEC.JSAPI.LogicGEC _ -> failwith "Logic GEC JS not supported yet."
      | GEC.JSAPI.ClassicGEC result ->
      currentResult <- Some (GEC.JSAPI.ClassicGEC result)
      currentSolution <- None
      let model = result.model
      sendObject { Response_Program.mtype = "program"
                   Response_Program.model = model }
      sendObject { Response_GECSolutions.mtype = "gec.solutions"
                   Response_GECSolutions.count = (match result.solution.solution with Some (_,s,_,_,_) -> s.numSolutions | _ -> 0) }
      //sendObject { Response_SBOL.mtype = "gec.jsbol"
      //             Response_SBOL.document = result.jsbol }
    with e -> match e with
              | :? CompileException as e -> match (e.Data0, e.Data1) with
                                            | parser, (:? Parser.Exception as e) -> sendObject { mtype = "error"
                                                                                                 error = { message = parser+": "+e.Message; positions=Some e.Errors } }
                                            | _ -> sendObject { mtype = "error"
                                                                error = { message = e.Message; positions = None } }
              | :? Parser.Exception as e -> sendObject { mtype = "error"
                                                         error = { message = e.Message; positions = Some e.Errors } }
              | _ -> sendObject { mtype = "error"
                                  error = { message = e.Message; positions = None } }

let processGetSolution idx (webSocket:WebSocket) =
    let sendObject o = sendObject webSocket o
    let result = JSAPI.get_solution (match currentResult with Some res -> res | None -> failwith "no result") idx
    currentSolution <- Some result
    let model = result.model
    sendObject { Response_GECSolution.mtype = "gec.solution"
                 Response_GECSolution.solution = { model = model; code = result.crnstring } }
    (*
    sendObject { Response_SBOL.mtype = "gec.jsbol"
                 Response_SBOL.document = result.jsbol }
    let xsbol = serializeSBOLDocumentToXML result.sbol
    sendObject { Response_Export.mtype = "export"
                 Response_Export.export = { content_type = "text/plain"
                                            id = "sbol"
                                            node_id = None
                                            instance = None
                                            display_name = "SBOL"
                                            content = Some [|xsbol|]
                                            save_content = None } }
    *)

(*
let getXMLExport () =
    let xsbol = match currentSolution with
                | Some solution -> serializeSBOLDocumentToXML solution.sbol |> Some
                | None -> match currentResult with
                          | Some (GEC.JSAPI.ClassicGEC result) -> serializeSBOLDocumentToXML result.sbol |> Some
                          | Some (GEC.JSAPI.LogicGEC _) -> failwith "Logic GEC is not supported yet."
                          | None -> None
    let export = match xsbol with
                 | Some xsbol -> 
                 { Response_Export.mtype = "export"
                   Response_Export.export = { content_type = "text/plain"
                                              id = "sbol"
                                              node_id = None
                                              instance = None
                                              display_name = "SBOL"
                                              content = Some [|xsbol|]
                                              save_content = None } } |> Some
                 | None -> None
    export
*)