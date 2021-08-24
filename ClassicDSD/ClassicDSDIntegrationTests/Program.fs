// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

open System
open System.IO
open Expecto
open canopy
open canopy.classic
open canopy.types
open Microsoft.AspNetCore.Hosting
open Microsoft.Research.CRNEngineServerLib
open Microsoft.Research.ClassicDSDServer
open Microsoft.Research.CRNIntegrationTestLib

[<EntryPoint>]
let main args =
    System.Globalization.CultureInfo.DefaultThreadCurrentCulture <- System.Globalization.CultureInfo.InvariantCulture

    let (groups,timeout,args) = Program.separateArgs args

    let server = Server.startWebServer Program.processRequest ClassicDSDServer.BuildSettings.homeFolder ClassicDSDServer.BuildSettings.jobsFolder ClassicDSDServer.BuildSettings.port
    
    let result =
        try
            let url = Server.starturl ClassicDSDServer.BuildSettings.port
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
