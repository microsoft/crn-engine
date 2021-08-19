module Program

open System
open System.Diagnostics
open Expecto
open canopy
open canopy.types
open canopy.classic
open Microsoft.Research.CRNIntegrationTestLib

[<EntryPoint>]
let main args =
    let (dir,groups,timeout,args) = Program.separateArgsDist args

    // Start a web server
    printfn "Starting 'dotnet serve' on %s" dir
    let serverProcess = Process.Start("dotnet", sprintf "serve -d %s" dir)
    printfn "'dotnet serve' started, PID = %d" serverProcess.Id
    
    let result =
        try
            let url = "http://localhost:8080"
            BrowserSetup.configureCanopy timeout
            let tests = Tests.tests groups Tests.Worker url
            Program.run args tests
        finally
            printfn "Shutting down 'dotnet serve'..."
            try serverProcess.Kill() with _ -> ()
            canopy.classic.quit()

    if System.Diagnostics.Debugger.IsAttached then
        printf "Press any key to exit"
        System.Console.ReadKey() |> ignore

    result