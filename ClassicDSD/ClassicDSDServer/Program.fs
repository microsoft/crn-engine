module Microsoft.Research.ClassicDSDServer.Program

open System
open System.Net.WebSockets
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Research.CRNEngineServerLib
open Microsoft.Research.CRNEngineServerLib.Serialisation
open Microsoft.Research.CRNEngineServerLib.Messages
open Microsoft.Research.CRNEngineServerLib.Server
open Microsoft.Research.DNA
open System.Threading
open ClassicDSDServer.BuildSettings

let processRequest (webSocket:WebSocket) (mtype:string) (json:string) (cancel:bool ref) =
    try
        let isJIT _ = match DSD.currentResult with None -> false | Some result -> JSAPI.is_jit result.bundle
        let getJITclassic _ _ = let result = match DSD.currentResult with None -> failwith "" | Some result -> result in JSAPI.get_jit_classic result.bundle
        let getJITforSSclassic _ = let result = match DSD.currentResult with None -> failwith "" | Some result -> result in JSAPI.get_jit_classic result.bundle
        let getJITrules _ _ = let result = match DSD.currentResult with None -> failwith "" | Some result -> result in JSAPI.get_jit_rules result.bundle
        let getJITforSSrules _ = let result = match DSD.currentResult with None -> failwith "" | Some result -> result in JSAPI.get_jit_rules result.bundle

        //TODO: switch to codegen of interface rather than relying on strings
        match mtype with
        | "simulategui" -> 
            let simulategui = WebSharper.Json.Deserialize<Messages.Request_SimulateGui> json
            match DSD.currentResult with
            | None ->
                Simulation.runSimulateGui (fun _ -> false) (fun _ _ -> failwith "") simulategui.model simulategui.nodeId simulategui.pool webSocket cancel
            | Some result ->
                if JSAPI.is_classic result.bundle then
                    Simulation.runSimulateGui isJIT getJITclassic simulategui.model simulategui.nodeId simulategui.pool webSocket cancel
                else
                    Simulation.runSimulateGui isJIT getJITrules simulategui.model simulategui.nodeId simulategui.pool webSocket cancel
        | "statespace" ->
            let stateSpace = WebSharper.Json.Deserialize<Messages.Request_StateSpace> json
            let result = match DSD.currentResult with None -> failwith "" | Some result -> result
            if stateSpace.jit then
                if JSAPI.is_classic result.bundle then
                    StateSpace.processStateSpaceRequestJIT getJITforSSclassic stateSpace.model webSocket
                else
                    StateSpace.processStateSpaceRequestJIT getJITforSSrules stateSpace.model webSocket
            else
                StateSpace.processStateSpaceRequest stateSpace.model webSocket
        | "dsd.compile" ->
            let compileDsd = WebSharper.Json.Deserialize<Messages.Request_CompileDSD> json
            DSD.processCompileDSDRequest compileDsd.code compileDsd.oldSyntax webSocket
        | "dsd.parse" ->
            let parseDsd = WebSharper.Json.Deserialize<Messages.Request_ParseDSD> json
            DSD.processParseDSDRequest parseDsd.code parseDsd.oldSyntax webSocket
        | "dsd.expand" ->
            let expandDsd = WebSharper.Json.Deserialize<Messages.Request_ExpandDSD> json
            DSD.processExpandDSDRequest expandDsd.code expandDsd.oldSyntax expandDsd.model expandDsd.settings webSocket
        | _ -> defaultProcessRequest webSocket mtype json cancel
    with e -> sendObject webSocket { mtype = "error"; error = { message = e.Message; positions = None } }

[<EntryPoint>]
let main argv =
    Server.main processRequest homeFolder jobsFolder port
    0