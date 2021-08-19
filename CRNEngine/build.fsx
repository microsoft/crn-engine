open Fake.Core
//https://fake.build/
#r "paket: groupref Build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open Fake.Core
open Fake.SystemHelper
open Fake.IO
open Fake.DotNet
open Fake.Core.TargetOperators

let projectPath = Path.getFullName "CRNEngineDotNet"
let projectFile = "CRNEngineDotNet/CRNEngineDotNet.fsproj"
let buildDir = "./build/"
let sundialsBuildDir = "../SundialsSolver/SundialsSolver15/x64/Release/"

//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
//let fakeExe =  "../packages/FAKE/tools/FAKE.exe"
//let sundialsBuild = "../SundialsSolver/SundialsSolver15/build.fsx"

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

// --------------------------------------------------------------------------------------
// Clean build results
Fake.Core.Target.create "Clean" (fun _ ->
    Shell.cleanDirs [ "build"; "bin"; "obj" ]
)

Fake.Core.Target.create "Release" (fun _ ->

    let properties _ =
      [ "Configuration", "Release"
        "Platform", "x64"]

    //TODO: on Linux and macOS do something different see build.fsx in Sundials
    ["../SundialsSolver/SundialsSolver15/SundialsSolver15.vcxproj"]
    |> MSBuild.runWithProperties id buildDir "Rebuild" properties
    |> ignore

    let onlyDLLOrPDB (path:string) =
        let extension = (System.IO.Path.GetExtension path).ToLowerInvariant()
        extension = ".dll" || extension = ".pdb"

    Shell.copyDir buildDir sundialsBuildDir onlyDLLOrPDB

    runDotNet "restore" projectPath

    [projectFile]
    |> MSBuild.runWithProperties id buildDir "Rebuild" properties
    |> ignore
)

Fake.Core.Target.create "NuGet" (fun _ ->

    (*Paket.pack (fun p -> 
        { p with 
            ToolPath = "bin/merged/paket.exe" 
            Version = "0.2"
            ReleaseNotes = toLines release.Notes })*)

    let template = "0.3"
    let version =
        match Environment.environVarOrNone "Build_BuildNumber" with
        | Some buildNumber -> sprintf "%s.%s-alpha" template buildNumber
        | None -> sprintf "%s.%s" template "DEV"

    (*Paket.pack (fun p -> 
        { p with 
            ToolPath = "../.paket/paket.exe"
            Version = version })*)

    //Workaround Paket.pack not setting output
    runDotNet (sprintf "paket pack --version %s temp" version) "."
)

Fake.Core.Target.create "Debug" (fun _ ->
    [projectFile]
    |> MSBuild.runDebug id buildDir "Rebuild"
    |> ignore
)

// These are targets for spawning out to build Sundials, nesting FAKE like this is problematic
// so this need reconsideration

(*Fake.Core.Target.create "Sundials" (fun _ ->
  let buildTarget = "All"
  let target = sprintf "TargetBuildDir=%s" (Path.Combine(System.Environment.CurrentDirectory,  buildDir))
  let runSundialsTarget (info:ProcessStartInfo) =
    info.FileName <- fakeExe
    info.Arguments <- String.concat " " [ sundialsBuild; buildTarget; target ]
    ()
  printfn "%s" (String.concat " " [ sundialsBuild; buildTarget; target ] )
  ExecProcess runSundialsTarget (TimeSpan.FromSeconds 30.0) |> ignore
)

Fake.Core.Target.create "CopySundials" (fun _ ->
  let build_target = "All"
  let target = sprintf "TargetBuildDir=%s" (Path.Combine(System.Environment.CurrentDirectory,  buildDir))
  let run_sundials_target (info:ProcessStartInfo) =
    info.FileName <- fakeExe
    info.Arguments <- String.concat " " [ sundialsBuild; build_target; target ]
    ()
  printfn "%s" (String.concat " " [ sundialsBuild; build_target; target ] )
  ExecProcess run_sundials_target (TimeSpan.FromSeconds 30.0) |> ignore
)*)

Fake.Core.Target.create "Batch" (fun _ ->
    // I'm loading the required Azure keys up front, so if they aren't there I won't waste time building.
    let azureXml = System.Xml.Linq.XDocument.Load("azure.xml")
    let storageAccountName = azureXml.Root.Element(System.Xml.Linq.XName.Get "storageAccountName").Value
    let storageAccountKey = azureXml.Root.Element(System.Xml.Linq.XName.Get "storageAccountKey").Value

    runDotNet "publish --output batch_executable_publish --self-contained --configuration Release --runtime win-x64" "CliCRN"
    let version = System.IO.File.ReadAllText @"CRNEngineDotNet\version.txt"
    let version = version.Trim()
    let zipName = "clicrn."+version+".zip"
    let dirName = @"CliCRN\batch_executable_publish"
    printfn "Self-contained executable built. Zipping to %s..." zipName
    System.IO.Compression.ZipFile.CreateFromDirectory (dirName, zipName)
    printfn "Zip file created as %s, deleting publish folder..." zipName
    System.IO.Directory.Delete(dirName, true)

    // Construct the Storage account connection string.
    printfn "Connecting to %s..." storageAccountName
    let storageConnectionString = sprintf "DefaultEndpointsProtocol=https;AccountName=%s;AccountKey=%s" storageAccountName storageAccountKey
    // Retrieve the storage account.
    let storageAccount = Microsoft.WindowsAzure.Storage.CloudStorageAccount.Parse(storageConnectionString)
    // Create the blob client.
    let blobClient = storageAccount.CreateCloudBlobClient()
    // Create/retrieve the container.
    let container = blobClient.GetContainerReference("programs")
    container.CreateIfNotExistsAsync().Wait()
    // Upload the file.
    printfn "Uploading %s..." zipName
    let blobData = container.GetBlockBlobReference(zipName)
    blobData.UploadFromFileAsync(zipName).Wait()
    // Delete the local file.
    printfn "Upload successful, deleting %s" zipName
    System.IO.File.Delete(zipName)
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override
Fake.Core.Target.create "All" ignore

"Clean"
    ==> "Release"
    ==> "NuGet"
    ==> "All"

Fake.Core.Target.runOrDefault "All"
