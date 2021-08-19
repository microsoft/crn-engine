module Microsoft.Research.ClassicGECWebServer.Program

open System
open System.Net.WebSockets
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open System.Threading
open Microsoft.Research.CRNEngineServerLib
open Microsoft.Research.CRNEngineServerLib.Serialisation
open Microsoft.Research.CRNEngineServerLib.Messages
open Microsoft.Research.CRNEngineServerLib.Server
open Microsoft.Research.ClassicGECWebServer

let processRequest (webSocket:WebSocket) (mtype:string) (json:string) (cancel:bool ref) =
    //TODO: switch to codegen of interface rather than relying on strings
    match mtype with
    | "statespace" ->
        let stateSpace = WebSharper.Json.Deserialize<Messages.Request_StateSpace> json
        if stateSpace.jit then failwith "GEC does not support JIT state space exploration"
        StateSpace.processStateSpaceRequest stateSpace.model webSocket
    | "gec.compile" ->
        let compileGec = WebSharper.Json.Deserialize<Messages.Request_GECCompile> json
        GEC.processCompileGECRequest compileGec.code compileGec.parts compileGec.reactions webSocket
    | "gec.getsolution" ->
        let getSolution = WebSharper.Json.Deserialize<Messages.Request_GECGetSolution> json
        GEC.processGetSolution getSolution.idx webSocket
    | "generateexports" ->
        let generateExports = WebSharper.Json.Deserialize<Messages.Request_GenerateExports> json
        Exports.processGenerateExportsRequest generateExports.model generateExports.nodeId webSocket
        match GEC.getXMLExport () with
        | Some export -> sendObject webSocket export
        | None -> ()
    | _ -> defaultProcessRequest webSocket mtype json cancel
    
let homeFolder = "ClassicGECHTML5"
let jobsFolder = "CRNJobsManager"
let port = 8085

[<EntryPoint>]
let main argv =
    Server.main processRequest homeFolder jobsFolder port
    0