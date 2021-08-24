// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineServerLib.Parse

open Microsoft.Research.CRNEngineServerLib.Serialisation
open System.Net.WebSockets
open Messages
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI
open Microsoft.Research.Filzbach

let processParseCodeRequest program (webSocket:WebSocket) = 
    let sendObject o = sendObject webSocket o
    try
        let ig = JSAPI.parse_code program
        let gui = GuiIG.from_ig ig
        sendObject { Response_Program.mtype = "model"
                     Response_Program.model = gui }
    with e -> match e with
                | :? Parser.Exception as e -> sendObject { mtype = "error"
                                                           error = { message = e.Message; positions = Some e.Errors } }
                | _ -> sendObject { mtype = "error"
                                    error = { message = e.Message; positions = None } }