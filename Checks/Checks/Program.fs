// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

open Expecto

open System.IO

let rec getAllFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in Directory.EnumerateDirectories(dir) do
              yield! getAllFiles d pattern }

let tests =

    //It's not clear what path we're in: https://github.com/dotnet/project-system/issues/3619 so figure it out
    let repositoryRootPath =
        let currentPath = DirectoryInfo(Directory.GetCurrentDirectory()).Name
        if currentPath.Contains("netcoreapp") then
            "../../../../../"
        else
            "../../"
        |> Path.GetFullPath

    let allOurFsproj = 
        getAllFiles repositoryRootPath "*.fsproj"
        |> Seq.filter(fun path -> not (path.Contains ".fable"))
        |> Array.ofSeq

    let allOurCsproj = 
        getAllFiles repositoryRootPath "*.csproj"
        |> Array.ofSeq

    let allOurTypeScriptProjects =
        allOurCsproj
        |> Seq.filter (fun csProjPath ->
            //let projectContent = csProjPath |> File.ReadAllText
            let directory = Path.GetDirectoryName(csProjPath)
            Directory.EnumerateFiles(directory, "*.ts", SearchOption.AllDirectories ) |> Seq.isEmpty |> not
        )

    
    testList "Project files" [
        test "All fsproj files have a paket.references file" {

            Expect.isGreaterThan allOurFsproj.Length 10 "There probably should be more F# projects"

            let missingPaketReferences =
                allOurFsproj
                |> Seq.filter(fun fsprojPath ->
                    Path.GetDirectoryName fsprojPath
                    |> (fun (other:string) -> Path.Combine(other,"paket.references"))
                    |> (fun path -> not (File.Exists path))
                    )
                |> Array.ofSeq

            Expect.isEmpty missingPaketReferences (sprintf "These projects are missing paket.references %A" missingPaketReferences)
        }
        test "All fsproj paket.references have FSharp.Core" {

            let allOurPaketRefences = 
                allOurFsproj
                |> Seq.map(fun fsprojPath ->
                    Path.GetDirectoryName fsprojPath
                    |> (fun (other:string) -> Path.Combine(other,"paket.references"))
                )
                |> Array.ofSeq

            Expect.isGreaterThan allOurPaketRefences.Length 10 "really?"

            let paketRefencesWithoutFSharpCore =
                allOurPaketRefences
                |> Seq.filter(fun paketReferencesPath ->
                    let text = File.ReadAllText paketReferencesPath

                    not (text.ToLower().Contains("fsharp.core"))
                    )
                |> Array.ofSeq

            Expect.isEmpty paketRefencesWithoutFSharpCore (sprintf "These paket.referencs don't have FSharp.Core %A" paketRefencesWithoutFSharpCore)
        }
        test "No nuget.config" {

            let anyNuGet = 
                getAllFiles repositoryRootPath "nuget.config"
                |> Seq.filter(fun path -> not (path.Contains ".fable"))
                |> Array.ofSeq

            Expect.isEmpty anyNuGet (sprintf "We use Paket not NuGet %A" anyNuGet)
        }
        test "No packages.config" {

            let anyNuGet = 
                getAllFiles repositoryRootPath "packages.config"
                |> Seq.filter(fun path -> not (path.Contains ".fable"))
                |> Array.ofSeq

            Expect.isEmpty anyNuGet (sprintf "We use Paket not NuGet %A" anyNuGet)
        }
        test "Our fsproj shouldn't know about NuGet" {

            Expect.isGreaterThan allOurFsproj.Length 10 "There probably should be more F# projects"

            let talkingAboutNuget =
                allOurFsproj
                |> Seq.filter(fun fsprojPath ->
                    let text = File.ReadAllText fsprojPath
                    text.ToLower().Contains("nuget")
                    )
                |> Array.ofSeq

            Expect.isEmpty talkingAboutNuget (sprintf "These projects know about NuGet %A" talkingAboutNuget)
        }
        test "No Any CPU in Sundials Dependent projects" {
            
            let getprojid (str:string) :string = 
              let eq = (str.Substring(str.IndexOf("=")+1)).Trim()
              let parts = eq.Split(",")
              let idfull = (parts.[parts.Length-1]).Trim()
              idfull.Substring(1,idfull.Length-2)
            
            let startsWithId (str:string) (idlist:string list) =  
              let id = idlist |> List.filter(fun x -> str.StartsWith(x)) 
              match id with 
              | [] -> false
              | _ -> true

            let allOurSln = 
                getAllFiles repositoryRootPath "*.sln"
                |> Seq.filter( fun slnfile -> 
                   let content = File.ReadAllText slnfile
                   if content.Contains("Sundials") then 
                     if (content.Contains("Any CPU.ActiveCfg") || content.Contains("Any CPU.Build.0")) then 
                       //Now check and see if the Any CPU config is for an fs or cs proj. cs ok. fs not ok. 
                       let lines = File.ReadLines slnfile |> List.ofSeq
                       let csproj = lines |> List.filter(fun x -> x.StartsWith("Project(") && x.Contains(".csproj"))
                       match csproj with 
                       | [] -> false //Project does not have any csprojects, hence ANY CPU test must fail. 
                       | _ -> 
                         //let csids = csproj |> List.map(fun x -> getprojid x)
                         let fsproj = lines |> List.filter(fun x -> x.StartsWith("Project(") && x.Contains(".fsproj"))                       
                         let fsids = fsproj |> List.map(fun x -> getprojid x)
                         let anycpufs = lines |> List.filter(fun x -> 
                                          if (x.Contains("Debug|Any CPU.ActiveCfg = Debug|Any CPU") || x.Contains("Debug|Any CPU.Build.0 = Debug|Any CPU") || x.Contains("Release|Any CPU.ActiveCfg = Release|Any CPU") || x.Contains("Release|Any CPU.Build.0 = Release|Any CPU")) then 
                                            startsWithId (x.Trim()) fsids
                                          else 
                                            false
                                        )
                         match anycpufs with 
                         | [] -> false
                         | _ -> true
                     
                     else
                       //This sln file does not have "Any CPU.ActiveCfg" or "Any CPU.Build.0" in the configuration file
                       false
                   else
                     //This sln file does not have "Sundials" in the configuration file
                     false
                   )
                |> Array.ofSeq
              
            Expect.isEmpty allOurSln (sprintf "These Solutions should not have Any CPU in their configuration ::  %A" allOurSln)
        }
        test "All TypeScript project files have a paket.references file" {

            Expect.isGreaterThan (Seq.length allOurTypeScriptProjects) 10 "There probably should be more TypeScript projects"

            let missingPaketReferences =
                allOurTypeScriptProjects
                |> Seq.filter(fun projectPath ->
                    Path.GetDirectoryName projectPath
                    |> (fun (other:string) -> Path.Combine(other,"paket.references"))
                    |> (fun path -> not (File.Exists path))
                    )
                |> Array.ofSeq

            Expect.isEmpty missingPaketReferences (sprintf "These projects are missing paket.references %A" missingPaketReferences)
        }
        test "All TypeScript project paket.references use NuGet TypeScript" {

            Expect.isGreaterThan (Seq.length allOurTypeScriptProjects) 10 "There probably should be more TypeScript projects"

            let missingPaketReferences =
                allOurTypeScriptProjects
                |> Seq.filter(fun projectPath ->
                    Path.GetDirectoryName projectPath
                    |> (fun (other:string) -> Path.Combine(other,"paket.references"))
                    |> (fun path ->
                        let fileContent = File.ReadAllText path
                        fileContent.IndexOf("Microsoft.TypeScript.MSBuild") < 0)
                    )
                |> Array.ofSeq

            Expect.isEmpty missingPaketReferences (sprintf "These projects are missing Microsoft.TypeScript.MSBuild in their paket.references %A" missingPaketReferences)
        }
        test "All TypeScript project paket.references use node.js.redist (for Yarn)" {

            Expect.isGreaterThan (Seq.length allOurTypeScriptProjects) 10 "There probably should be more TypeScript projects"

            let missingPaketReferences =
                allOurTypeScriptProjects
                |> Seq.filter(fun projectPath ->
                    Path.GetDirectoryName projectPath
                    |> (fun (other:string) -> Path.Combine(other,"paket.references"))
                    |> (fun path ->
                        let fileContent = File.ReadAllText path
                        fileContent.IndexOf("Node.js.redist") < 0)
                    )
                |> Array.ofSeq

            Expect.isEmpty missingPaketReferences (sprintf "These projects are missing Microsoft.TypeScript.MSBuild in their paket.references %A" missingPaketReferences)
        }
        test "All TypeScript project files should run Yarn (via managed node.js)" {

            Expect.isGreaterThan (Seq.length allOurTypeScriptProjects) 10 "There probably should be more TypeScript projects"

            let missingPaketReferences =
                allOurTypeScriptProjects
                |> Seq.filter(fun projectPath ->
                    let text = File.ReadAllText projectPath
                    not(text.ToLower().Contains("YarnBuildCommand".ToLower())) || not(text.ToLower().Contains("NodeJsExecutablePath".ToLower()))
                    )
                |> Array.ofSeq

            Expect.isEmpty missingPaketReferences (sprintf "These projects are missing paket.references %A" missingPaketReferences)
        }
    ]

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args tests