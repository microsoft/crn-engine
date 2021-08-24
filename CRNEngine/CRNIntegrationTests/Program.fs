// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

open System
open System.IO
open Expecto
open Microsoft.AspNetCore.Hosting
open Microsoft.Research.CRNEngineServerLib
open Microsoft.Research.CRNEngineWebServer
open canopy.classic
open Microsoft.Research.CRNIntegrationTestLib

[<EntryPoint>]
let main args =
    System.Globalization.CultureInfo.DefaultThreadCurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    let (groups,timeout,args) = Program.separateArgs args

    let server = Server.startWebServer Server.defaultProcessRequest CRNEngineWebServer.BuildSettings.homeFolder CRNEngineWebServer.BuildSettings.jobsFolder CRNEngineWebServer.BuildSettings.port
    printfn "Server started"
        
    let result =
        try
            let url = Server.starturl CRNEngineWebServer.BuildSettings.port
            BrowserSetup.configureCanopy timeout
            let tests = Tests.tests groups Tests.Localhost url
            Program.run args tests
        finally
            printfn "Shutting down server..."
            server.StopAsync() |> ignore
            canopy.classic.quit()

    if System.Diagnostics.Debugger.IsAttached then
        printf "Press any key to exit"
        System.Console.ReadKey() |> ignore
    
    result