module Microsoft.Research.CRNEngineServerLib.Serialisation

open System
open System.Threading
open System.Net.WebSockets
open System.Threading.Tasks

//Consider using something faster e.g https://google.github.io/flatbuffers/flatbuffers_benchmarks.html
let sendObject (webSocket:WebSocket) o = 
    let json = WebSharper.Json.Serialize o
    let bytes =
        json
        |> System.Text.Encoding.UTF8.GetBytes
    let segment = new ArraySegment<byte>(bytes)
    let task = webSocket.SendAsync(segment, WebSocketMessageType.Text, true, CancellationToken.None)
    ignore task