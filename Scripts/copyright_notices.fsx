// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

open System.IO

(* This script checks whether copyright notices have been added to all source files, inserting where necessary. *)
let skipDirectories = ["node_modules"; "Lib"; "bin"; "obj"; "paket-files"]
let enumerateIncludedDirectories dir = 
    Directory.EnumerateDirectories dir 
    |> Seq.filter(fun d -> 
        skipDirectories |> List.forall (fun sd -> d.Contains ("\\" + sd + "\\") |> not)
    )
    
let rec getAllFiles dir pattern =
    seq { yield! Directory.EnumerateFiles(dir, pattern)
          for d in enumerateIncludedDirectories(dir) do
              yield! getAllFiles d pattern }

let update (notice: string[]) file = 
    let contents = File.ReadAllLines file
    if (contents.[0] <> notice.[0])
    then
        printfn "Updating %s" file
        File.WriteAllLines (file, Array.concat [|notice; [|""|]; contents|])

// C-style comments (.fs, .fsi, .h, .cpp, .ts, .js)
let cNotice = [|"// Copyright (c) Microsoft Corporation."; "// Licensed under the MIT License." |]

let cFiles = ["fs"; "fsi"; "cpp"; "h"; "ts"; "js"] |> Seq.collect (fun ext -> getAllFiles "." ("*." + ext))

// Test
update cNotice (Seq.head cFiles)

// Run all
for file in cFiles do
    update cNotice file