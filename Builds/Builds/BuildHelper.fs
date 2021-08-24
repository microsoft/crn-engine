// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//Lightweight version of https://fake.build/fake-fake5-custom-modules.html consider moving later?

module BuildHelper

//This is an experiment of extracting shared parts of FAKE fsx build scripts.
//If it gets too full consider some structure

open System
open System.IO
open System.Text
open System.Net

open Fake.Core
open Fake.SystemHelper
open Fake.IO
open Fake.DotNet
open Fake.IO.Globbing.Operators


let allReferences = Paket.getDependenciesForReferencesFile("paket.references")

let versionNumberFor referenceName =

    let versionNumberCandidates = 
        allReferences
        |> Seq.filter (fun (name, _) -> name = referenceName)
        |> Seq.map snd
        |> Seq.distinct
    
    match versionNumberCandidates |> Seq.length with
    | 1 -> ()
    | 0 -> failwithf "Can't find '%s' in your paket.references" referenceName
    | _ -> failwithf "Too many copies of '%s' in your paket.references" referenceName

    let versionNumber = Seq.exactlyOne versionNumberCandidates

    let points = versionNumber |> String.filter(fun chr -> chr = '.') |> String.length
    match points with
    | 2 -> versionNumber
    | 1 -> versionNumber + ".0"
    | 0 -> versionNumber + ".0.0"
    | _ -> failwithf "Unexpected number of '.' in '%s'" versionNumber

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | _ ->
        let errorMsg =
            tool + " was not found in path. " +
            "Please install it and make sure it's available from your path. "
        failwith errorMsg

let runTool toolPath arguments =
    let result =
        (toolPath, arguments)
        ||> CreateProcess.fromRawCommand 
        |> Proc.run

    printfn "%O" result.Result

    if result.ExitCode <> 0 then failwithf "Tool failed(%i): %s" result.ExitCode toolPath

//Does this exist as a setting on FAKE or Paket somewhere?
//https://docs.microsoft.com/en-us/nuget/consume-packages/managing-the-global-packages-and-cache-folders
let globalNugetPath =
    if Environment.isUnix then
        "~/.nuget/packages"
    else
       """%userprofile%\.nuget\packages"""

//Can we use Paket so as to not need to specify verion here and there?
let nugetTool nugetName version pathInNuGet tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    let path =
        [| globalNugetPath; nugetName; version; pathInNuGet|]
        |> Path.Combine
        |> Environment.ExpandEnvironmentVariables
        |> Path.getFullName
    
    match ProcessUtils.tryFindFile [path] tool with
    | Some t -> t
    | _ -> failwithf "Couldn't find '%s' in path: %s" tool path

let nodeNugetTool =

    let nodeJSVersionNumber = versionNumberFor "Node.js.redist"

    let nodeDirectory =
        if Environment.isLinux then
            "linux-x64"
        elif Environment.isMacOS then
            "osx-x64"
        elif Environment.isWindows then
            if Environment.Is64BitProcess then
                "win-x64"
            else
                "win-x86"
        else
            failwith "Unknown operating system when trying to locate Node"

    nugetTool "node.js.redist" nodeJSVersionNumber (Path.combine "tools" nodeDirectory) "node" "node.exe"

let yarnJS = 
    let yarnVersionNumber = versionNumberFor "Yarn.MSBuild"
    nugetTool "yarn.msbuild" yarnVersionNumber (Path.combine "dist" "bin") "yarn.js" "yarn.js"

let runNode : (seq<string>->unit) = runTool nodeNugetTool

let runToolWithPermissibleError toolPath arguments errorCode =
    let result =
        (toolPath, arguments)
        ||> CreateProcess.fromRawCommand 
        |> Proc.run

    if (result.ExitCode <> errorCode && result.ExitCode <> 0) then failwithf "Tool failed(%i): %s" result.ExitCode toolPath

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let openBrowser url =
    let result =
        Command.ShellCommand url
        |> CreateProcess.fromCommand
        |> Proc.run

    if result.ExitCode <> 0 then failwithf "opening browser failed"

//Empty argument for FAKE target compatibility
let yarnInstall _ =
    let res = CreateProcess.fromRawCommand nodeNugetTool [yarnJS; "install"] |> Proc.run
    if res.ExitCode <> 0 then failwith "yarn failed"
    