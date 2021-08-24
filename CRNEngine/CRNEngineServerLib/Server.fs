// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineServerLib.Server

open System
open Giraffe
open Giraffe.HttpStatusCodeHandlers.ServerErrors
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.StaticFiles
open Microsoft.AspNetCore.Hosting
open FSharp.Control.Tasks.ContextInsensitive
open System.Net.WebSockets
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Research.CRNEngineServerLib
open Microsoft.Research.CRNEngineServerLib.Messages
open Microsoft.Research.CRNEngineServerLib.Serialisation
open Microsoft.Research.CRNEngine
open Microsoft.Extensions.FileProviders
open System.Threading
open System.Diagnostics
open System.Runtime.InteropServices
open Microsoft.AspNetCore.Http
open Microsoft.Research.CRNEngineCloudLib
open Utility

// This exception handler can be used by the request processor.
let handleException sendObject (e:Exception) =
    printfn "%s" e.Message
    let err = { message = e.Message
                positions = None }
    let msg = { mtype = "error"
                error = err }
    sendObject msg
    
let openBrowser url =
    if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
        Process.Start(new ProcessStartInfo("cmd", "/c start " + url))
    elif (RuntimeInformation.IsOSPlatform(OSPlatform.Linux)) then
        Process.Start("xdg-open", url) //Needs testing on Linux
    elif (RuntimeInformation.IsOSPlatform(OSPlatform.OSX)) then
        Process.Start("open", url) // Needs testing on macOS
    else
        Process.Start("")

let reqCounter = ref 0
let msgCounter = ref 0

// The type of a request processor. It takes a WebSocket, a request in the form of a type string and json, and a cancel flag. It is supposed to execute the request, send results on the socket, and terminate as soon as possible if the cancel flag gets set.
type RequestProcessor = WebSocket -> string -> string -> bool ref -> unit

let private runRequestProcessor (processor:RequestProcessor) (webSocket : WebSocket) (data: byte[]) (token:CancellationToken) =
    let task = async {
        //Consider using something faster e.g https://google.github.io/flatbuffers/flatbuffers_benchmarks.html
        let sendObject o = sendObject webSocket o
    
        let json = System.Text.Encoding.UTF8.GetString data
        let req = WebSharper.Json.Deserialize<Messages.GeneralMessage> json

        try
            let start = DateTime.Now
            let reqN = System.Threading.Interlocked.Increment reqCounter

            let! token = Async.CancellationToken
            let cancel = ref false
            // The F# async mechanism can handle cancellation automatically. However, CRNEngine functions do not use that pattern; they just use a bool ref instead. This won't give async a chance to cancel them. So I need to hook up to token cancellation and set a cancel flag. Note that if cancellation is requested, the final lines of this function (e.g. logging "request handled") will not be executed, because the F# async mechanism will get a chance to cancel before getting there (right after the final sendObject call).
            let setCancelFlag () =
                cancel := true
                let finish = DateTime.Now
                printlog "Request #%d canceled after %f msec" reqN (finish-start).TotalMilliseconds
            // The "use" keyword will ensure the hook is removed if the request completes successfully.
            use reg = token.Register (new Action(setCancelFlag))

            printlog "Handling request #%d (%s)..." reqN req.mtype
            processor webSocket req.mtype json cancel
            sendObject { GeneralMessage.mtype = "finished" }
            let finish = DateTime.Now
            printlog "Request #%d handled in %f msec" reqN (finish-start).TotalMilliseconds
        with
            e -> handleException sendObject e
    }
    Async.Start (task,token)

let socketReceiveBuffer = 65536

// A middleware that dispatches requests to a given processor.
type WebSocketMiddleware(next:RequestDelegate, processor:RequestProcessor) =
    member __.Invoke(ctx : HttpContext) =
        task {
            if ctx.Request.Path = PathString("/websocket") then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
                    let! webSocket = ctx.WebSockets.AcceptWebSocketAsync()
                    let token = new CancellationTokenSource()
                    let bufferSize = socketReceiveBuffer
                    let chunk : byte [] = Array.zeroCreate bufferSize
                    let! ct = Async.CancellationToken |> Async.StartAsTask
                    let mutable message = [||]
                    while (webSocket.State = WebSocketState.Open) do
                        let msgN = System.Threading.Interlocked.Increment msgCounter
                        let! request = webSocket.ReceiveAsync (new ArraySegment<byte>(chunk), ct)
                                        |> Async.AwaitTask
                        printlog "Handling message #%d (%O)" msgN request.MessageType
                        if (request.MessageType = WebSocketMessageType.Close) then
                            token.Cancel()
                            webSocket.CloseAsync (WebSocketCloseStatus.NormalClosure, "", ct)
                            |> Async.AwaitTask |> ignore
                        else
                            // If the request did not fill the entire buffer, then only the portion that was written to should be added to the message. Otherwise, we may be adding a portion of a previous buffer.
                            let buffer = if request.Count = bufferSize then chunk else chunk.[..(request.Count-1)]
                            message <- Array.concat [message;buffer]
                            if (request.EndOfMessage) then
                                runRequestProcessor processor webSocket message token.Token
                                message <- [||]
                        printlog "Message #%d handled" msgN
                | false -> ctx.Response.StatusCode <- 400
            else
                // Handing off
                return! next.Invoke(ctx)
        }

let host = "127.0.0.1"
let argument = "?server=true"

let startWebServer (processor:RequestProcessor) baseFolder jobsFolder port : IWebHost =
    let url = sprintf "http://%s:%i/" host port
    // We have issues with caching, it's not useful for the run-on-localhost version so disable it here
    // https://biology.visualstudio.com/BiologyGit/_workitems/edit/843
    let nocaching : HttpHandler =
        setHttpHeader "Cache-Control" "no-cache, no-store, must-revalidate"
        >=> setHttpHeader "Pragma" "no-cache"
        >=> setHttpHeader "Expires" "0"
    // Determine some file positions.
    let directory =
        System.Reflection.Assembly.GetEntryAssembly().Location
        |> System.IO.Path.GetDirectoryName
        |> System.IO.Path.GetFullPath
    let homeFolder =
        (directory, baseFolder)
        |> System.IO.Path.Combine
        |> System.IO.Path.GetFullPath
    let indexHtmlPath =
        let homeFolder = homeFolder
        (homeFolder,  "index.html")
        |> System.IO.Path.Combine
        |> System.IO.Path.GetFullPath
    let jobsFolder =
        (directory, jobsFolder)
        |> System.IO.Path.Combine
        |> System.IO.Path.GetFullPath
    let jobsHtmlPath =
        let jobsFolder = jobsFolder
        (jobsFolder,  "index.html")
        |> System.IO.Path.Combine
        |> System.IO.Path.GetFullPath
    // Enable some additional file extensions to be served.
    let fileExtensions = new FileExtensionContentTypeProvider()
    fileExtensions.Mappings.[".crn"] <- "text/crn";
    fileExtensions.Mappings.[".csv"] <- "text/csv";
    fileExtensions.Mappings.[".ts"] <- "application/x-typescript";
    let fileOptions = new StaticFileOptions()
    fileOptions.ContentTypeProvider <- fileExtensions
    // Prepare to also map the /jobs URL to the jobs app folder.
    let jobsFileOptions = new StaticFileOptions()
    jobsFileOptions.ContentTypeProvider <- fileExtensions
    jobsFileOptions.FileProvider <- new PhysicalFileProvider(jobsFolder)
    jobsFileOptions.RequestPath <- new PathString("/jobs")
    // Add the caching suppressor, and map / to index.html.
    let webApp = nocaching >=> GET >=> choose [
        route "/" >=> htmlFile indexHtmlPath
        // Redirect /jobs to /jobs/index.html; ignore trailing slash.
        routex "/jobs(/?)" >=> redirectTo true "/jobs/index.html"
    ]
    // Handle network errors by logging.
    let errorHandler (ex : Exception) (logger : ILogger) =
        match ex with
        | _ ->
            logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
            clearResponse >=> INTERNAL_ERROR ex.Message
    let webSocketOptions = new WebSocketOptions()
    webSocketOptions.ReceiveBufferSize <- socketReceiveBuffer
    // Configure the web app.
    let configureApp (app : IApplicationBuilder) =
        app
            .UseGiraffeErrorHandler(errorHandler)
            .UseDefaultFiles()
            .UseStaticFiles(fileOptions)
            .UseStaticFiles(jobsFileOptions)
            .UseWebSockets(webSocketOptions)
            .UseMiddleware<WebSocketMiddleware>(processor)
            .UseGiraffe webApp
    // Add the default Giraffe service.
    let configureServices (services : IServiceCollection) = services.AddGiraffe() |> ignore
    // Log to the console and to the debug output.
    let configureLogging (loggerBuilder : ILoggingBuilder) = loggerBuilder.AddFilter(fun lvl -> lvl >= LogLevel.Error).AddConsole().AddDebug() |> ignore
    // Create the server.
    let server = WebHost.CreateDefaultBuilder().UseWebRoot(homeFolder)
                  .UseContentRoot(homeFolder)
                  .UseKestrel()
                  .Configure(Action<IApplicationBuilder> configureApp)
                  .ConfigureServices(configureServices)
                  .ConfigureLogging(configureLogging)
                  .UseUrls(url).Build()
    printlog "Starting web server on %s" url
    // Start the server. This is synchronous, so the server is ready after this.
    server.Start()
    printlog "Server started"
    server

let starturl port = sprintf "http://%s:%i%s" host port argument

/// Time during which the server can't be stopped by pressing a key, right after startup.
let stopServerRefractory = System.TimeSpan.FromSeconds(5.);

// Starts the server, launches the browser, and waits until a key is pressed.
let main processor baseFolder jobsFolder port =
    let starturl = starturl port
    System.Globalization.CultureInfo.DefaultThreadCurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    let server:IWebHost = startWebServer processor baseFolder jobsFolder port;
    openBrowser starturl |> ignore
    // Prevent me from accidentally closing the server if I'm typing somewhere else as it starts up...
    let t = System.DateTime.UtcNow;
    while System.DateTime.UtcNow - t < stopServerRefractory do
        System.Console.ReadKey() |> ignore
    done
    printlog "Stopping server"
    server.StopAsync() |> ignore

// The default request processor for basic CRN actions.
let defaultProcessRequest (webSocket:WebSocket) (mtype:string) (json:string) (cancel:bool ref) =
    try
        //TODO: switch to codegen of interface rather than relying on strings
        match mtype with
        | "parsecode" ->
            let parseCode = WebSharper.Json.Deserialize<Messages.Request_ParseCode> json
            Parse.processParseCodeRequest parseCode.code webSocket
        | "simulategui" -> 
            let simulategui = WebSharper.Json.Deserialize<Messages.Request_SimulateGui> json
            Simulation.runSimulateGui (fun _ -> false) (fun _ _ -> failwith "") simulategui.model simulategui.nodeId simulategui.pool webSocket cancel
        | "infercode" ->
            let inferCode = WebSharper.Json.Deserialize<Messages.Request_InferCode> json
            Inference.processInferCodeRequest inferCode.code inferCode.datasets inferCode.pool webSocket cancel
        | "infergui" ->
            let inferGui = WebSharper.Json.Deserialize<Messages.Request_InferGui> json
            Inference.processInferGuiRequest inferGui.model inferGui.pool webSocket cancel
        | "generateexports" ->
            let generateExports = WebSharper.Json.Deserialize<Messages.Request_GenerateExports> json
            Exports.processGenerateExportsRequest generateExports.model generateExports.nodeId webSocket
        | "generateexport" ->
            let generateExports = WebSharper.Json.Deserialize<Messages.Request_GenerateExport> json
            Exports.processGenerateExportRequest generateExports.model generateExports.nodeId generateExports.id generateExports.instance webSocket
        | "statespace" ->
            let stateSpace = WebSharper.Json.Deserialize<Messages.Request_StateSpace> json
            if stateSpace.jit then failwith "CRNEngineServer cannot run JIT state space analysis"
            StateSpace.processStateSpaceRequest stateSpace.model webSocket
        | "getprobabilitymap" ->
            let getProbabilities = WebSharper.Json.Deserialize<Messages.Request_GetProbabilityMap> json
            let map = JSAPI.getProbabilityMap getProbabilities.probabilities getProbabilities.species getProbabilities.lowerBound
            sendObject webSocket { mtype = "probabilitymap" ; map = map }
        (*
        | "synthesis" ->
            let synthesis = WebSharper.Json.Deserialize<Messages.Request_Synthesis> json
            Synthesis.processSynthesisRequest synthesis.model synthesis.nodeId synthesis.crnId webSocket
        | "bistability" ->
            let bistability = WebSharper.Json.Deserialize<Messages.Request_Bistability> json
            Synthesis.processGetBistabilityPlot bistability.crn bistability.solution bistability.spX bistability.spY bistability.numPoints webSocket
        *)
        | "getcloudcapabilities" ->
            sendObject webSocket { mtype= "pools" ; account = Azure.batchAccountName ; pools = Azure.getPoolNames() |> Seq.toArray };
        | "getjobs" ->
            let getJobs = WebSharper.Json.Deserialize<Messages.Request_GetJobs> json
            Jobs.processGetJobsRequest webSocket getJobs.allFiles
        | "stopjob" ->
            let stopjob = WebSharper.Json.Deserialize<Messages.Request_StopJob> json
            AzureJobsManagement.stopJob stopjob.id
        | "deletejob" ->
            let deletejob = WebSharper.Json.Deserialize<Messages.Request_DeleteJob> json
            AzureJobsManagement.deleteJob deletejob.id
        | x -> failwithf "Unexpected request: %s" x
    with e -> sendObject webSocket { mtype = "error"; error = { message = e.Message; positions = None } }