#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open BlackFox.Fake

#load "../../Builds/Builds/PublishHelper.fs"
open PublishHelper
#load "../ClassicDSDServer/BuildSettings.fs"

let copyRequiredFilestoOutput projectDir outDir configuration =
    copySundials projectDir outDir configuration
    copyWebStuff projectDir outDir ClassicDSDServer.BuildSettings.homeFolder
    copyJobStuff projectDir outDir ClassicDSDServer.BuildSettings.jobsFolder

let build = BuildTask.createFn "Build" [] (fun args ->
    let settings = parseSettings args.Context.Arguments
    copyRequiredFilestoOutput settings.["ProjectDir"] settings.["OutDir"] settings.["Configuration"]
)

//Example:
//dotnet fake build --target Publish ProjectDir="E:\dev\biocomputing\ClassicDSD\ClassicDSDServer\" PublishDir="bin\Debug\netcoreapp3.0\publish\"  Configuration=Debug
let publish = BuildTask.createFn "Publish" [] (fun args ->
    let settings = parseSettings args.Context.Arguments
    copyRequiredFilestoOutput settings.["ProjectDir"] settings.["PublishDir"] settings.["Configuration"]
)

let nothing = BuildTask.createEmpty "Nothing" []

BuildTask.runOrDefaultWithArguments nothing