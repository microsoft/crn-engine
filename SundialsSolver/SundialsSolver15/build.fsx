// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r @"../../packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.FileHelper
open Fake.EnvironmentHelper
open Fake.AssemblyInfoFile
open System
open System.Text
open System.IO
open System.Diagnostics

let projectFile  = __SOURCE_DIRECTORY__ + """/SundialsSolver15.vcxproj"""
let Sundials_build_dir = "./x64/Release"


System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let solution_dir = System.IO.Directory.GetParent(__SOURCE_DIRECTORY__).ToString ()

let (targetBuildDir:string) = getBuildParamOrDefault "TargetBuildDir" "../../ModellingEngine/ModellingEngineDotNet/bin/Release"
printfn "THE TARGET IS %s" targetBuildDir


Target "Clean" (fun _ ->
    CleanDirs [ Sundials_build_dir ]
)

Target "CleanTarget" (fun _ ->
    if isLinux then 
      printfn "I thinks linux"
      DeleteFiles [ 
          (Path.Combine(targetBuildDir, "libSundialsSolver15.dll.so"))
        ; (Path.Combine(targetBuildDir, "libsundials_cvode.so.1")) 
        ; (Path.Combine(targetBuildDir, "libsundials_nvecserial.so.0"))
        ]
    elif isMacOS then 
      printfn "I thinks osX"
      DeleteFiles [ 
          (Path.Combine(targetBuildDir, "libSundialsSolver15.dll.dylib"))
        ; (Path.Combine(targetBuildDir, "libsundials_cvode.1.dylib")) 
        ; (Path.Combine(targetBuildDir, "libsundials_nvecserial.0.dylib"))
        ]
    else
      printfn "I thinks Windows"
      Fake.IO.File.delete (Path.Combine(targetBuildDir, "SundialsSolver15.dll"))
      Fake.IO.File.delete (Path.Combine(targetBuildDir, "SundialsSolver15.pdb"))
)


let run_make (info:ProcessStartInfo) = 
  info.FileName <- "make" 
  info.Arguments <- String.concat " " [ "all"; 
                                        "TARGET_DIR="+ Sundials_build_dir]
  ()

// Build library 
Target "Build" (fun x ->
  Fake.IO.Directory.ensure Sundials_build_dir
  if not (projectFile |> Fake.IO.File.exists) then failwithf "Not found:%s" projectFile
  if isUnix then 
    printfn "Start make"
    let exit_code = ExecProcess run_make (TimeSpan.FromSeconds 60.0)
    printfn "Exit code from make: %i" exit_code
    ()
  else
    printfn "building %s into %s" projectFile Sundials_build_dir
    let properties = [ "Configuration", "Release"
                     ; "Platform", "x64"
                     ; "OutDir", Sundials_build_dir]
    !! projectFile
    |> Fake.DotNet.MSBuild.run id Sundials_build_dir "Rebuild" properties 
    printfn "Build completed"
)

//Target "Debug" (fun _ ->
//  if is_unix then 
//    printf "%s" "no debug build in linux"
//    ()
//  else
//    !! projectFile
//    //|> MSBuildDebug "x64/Debug" "Rebuild"
//    |> MSBuildDebug Sundials_build_dir "Rebuild"
//    |> ignore
//)

let copy_and_rename source_dir source_name target_dir target_name = 
    CopyFile 
      target_dir 
      (Path.Combine(source_dir, source_name))
    Rename 
      ( Path.Combine(target_dir, target_name))
      ( Path.Combine(target_dir, source_name))

let get_distro_info (info:ProcessStartInfo) = 
  info.FileName <- "lsb_release" 
  info.Arguments <- "-sri" //-sd gives "Ubuntu 15.10"
  info.StandardOutputEncoding <- Encoding.ASCII // fake switched to defaulting to UTF8 
  ()

Target "Deploy" (fun _ ->
  CreateDir targetBuildDir 
  if isLinux then 
    let distro_info = ExecProcessAndReturnMessages get_distro_info (TimeSpan.FromSeconds 15.0)
    let (distro, version) = distro_info.Messages.[0], distro_info.Messages.[1]
    printfn "%s %s" distro version
    //compatibility test 
    let version = if version = "17.10" then "17.04" else version 
    let sundials_lib = sprintf "../../Lib/Sundials_v2.6.2_64bit/lib_linux_%s%s" distro version
    [| (Sundials_build_dir,"SundialsSolver15.so", targetBuildDir,  "libSundialsSolver15.dll.so")
     ; (sundials_lib, "libsundials_cvode.so.1.0.0", targetBuildDir, "libsundials_cvode.so.1")
     ; (sundials_lib, "libsundials_nvecserial.so.0.0.2", targetBuildDir, "libsundials_nvecserial.so.0")
    |]  |> Array.iter (fun (sd, sf, td, tf) -> copy_and_rename sd sf td tf)
    printfn "After copy Linux"
    
  elif isMacOS then 
    let sundials_lib = "../../Lib/Sundials_v2.6.2_64bit/lib_osx_Darwin"
    [| (Sundials_build_dir,"SundialsSolver15.dylib", targetBuildDir,  "libSundialsSolver15.dll.dylib")
     ; (sundials_lib, "libsundials_cvode.1.0.0.dylib", targetBuildDir, "libsundials_cvode.1.dylib")
     ; (sundials_lib, "libsundials_nvecserial.0.0.2.dylib", targetBuildDir, "libsundials_nvecserial.0.dylib")
    |]  |> Array.iter (fun (sd, sf, td, tf) -> copy_and_rename sd sf td tf) 
    printfn "After copy MacOS"
  else 
    CopyFile targetBuildDir  (Path.Combine(Sundials_build_dir, "SundialsSolver15.dll"))
    CopyFile targetBuildDir  (Path.Combine(Sundials_build_dir, "SundialsSolver15.pdb"))
    printfn "After copy Windows"
  ()
)

Target "All" DoNothing

"Clean"
  ==> "Build"
  ==> "CleanTarget"
  ==> "Deploy"
  ==> "All"

RunTargetOrDefault "All"