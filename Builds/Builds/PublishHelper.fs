// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module PublishHelper

open Fake.Core
open Fake.IO

let copySundials projectDir targetDir configuration =
    if Environment.isWindows then
        let sundials =
            System.IO.Path.Combine [|
                projectDir
                ".."
                "x64"
                configuration
                "SundialsSolver15.dll"
            |]
        let sundialsPdb =
            System.IO.Path.Combine [|
                projectDir
                ".."
                "x64"
                configuration
                "SundialsSolver15.pdb"
            |]
        Shell.copyFile targetDir sundials
        Shell.copyFile targetDir sundialsPdb
    else
        printfn "On non-Windows this script doesn't manage Sundials for you"

let copyWebStuff projectDir outDir targetForWeb =
    let webPublicFolder = Path.combine outDir targetForWeb
    let webStuff =
        System.IO.Path.Combine [|
            projectDir
            ".."
            targetForWeb
            "dist"
        |]
    Shell.copyDir webPublicFolder webStuff (fun _ -> true)

let copyJobStuff projectDir outDir targetForJobs =
    let jobsPublicFolder = Path.combine outDir targetForJobs
    let jobsStuff =
        System.IO.Path.Combine [|
            projectDir
            "..\..\CRNEngine"
            targetForJobs
            "dist"
        |]
    Shell.copyDir jobsPublicFolder jobsStuff (fun _ -> true)

let parseSettings raw =
    raw
    |> List.map (fun (setting:string) ->
        let asArray = setting.Replace("'", "").Split('=')
        if asArray.Length > 2 then failwithf "Too many '=' in %s" setting
        asArray.[0], asArray.[1])
    |> Map.ofList    