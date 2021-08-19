module Microsoft.Research.CRNEngineServerLib.StateSpace

open Microsoft.Research.CRNEngineServerLib.Serialisation
open System.Net.WebSockets
open Messages
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI

let processStateSpaceRequest (ig:GuiIG) (webSocket:WebSocket) = 
    match ig.task with Some t -> (match t.task_type with Some TaskType.Parse -> failwith "this model is designed for parsing only" | _ -> ()) | _ -> ()
    let sendObject = sendObject webSocket
    let state_space = JSAPI.user_state_space ig
    sendObject { Response_StateSpace.mtype = "statespace"
                 Response_StateSpace.statespace = state_space }

let processStateSpaceRequestJIT (getJIT:GuiIG->JSAPI.jit<'s>) (ig:GuiIG) (webSocket:WebSocket) = 
    match ig.task with Some t -> (match t.task_type with Some TaskType.Parse -> failwith "this model is designed for parsing only" | _ -> ()) | _ -> ()
    let sendObject = sendObject webSocket
    let jit = getJIT ig
    let state_space = JSAPI.user_state_space_jit jit
    sendObject { Response_StateSpace.mtype = "statespace"
                 Response_StateSpace.statespace = state_space }