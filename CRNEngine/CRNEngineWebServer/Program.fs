// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineWebServer.Program

open System
open System.Net.WebSockets
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Research.CRNEngineServerLib
open Microsoft.Research.CRNEngineServerLib.Serialisation
open Microsoft.Research.CRNEngineServerLib.Messages
open Microsoft.Research.CRNEngineServerLib.Server
open System.Threading

open CRNEngineWebServer.BuildSettings

[<EntryPoint>]
let main argv =
    Server.main defaultProcessRequest homeFolder jobsFolder port
    0