#r "paket: groupref Build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#endif

open Fake.Core
open Fake.Core.TargetOperators

#load "../../Builds/Builds/BuildHelper.fs"
open BuildHelper

let localPort = "18850"
let projectFile = "HTML5CRN.csproj"
let projectFolder = "."

Target.create "Run" (fun _ ->
    runDotNet ("serve -d dist -p "+localPort+" -o") projectFolder
)

Target.create "Clean" (fun _ ->
    runDotNet "clean" projectFolder
)

Target.create "Build" (fun _ ->
    runDotNet "build" projectFolder
)

Target.create "Yarn" yarnInstall

"Clean"
    ==> "Yarn"
    ==> "Build"
    ==> "Run"

Target.runOrDefault "Build"
