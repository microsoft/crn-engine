// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Io
open Microsoft.Research.CRNEngine

#if JavaScript
//This probably won't work in JavaScript...
let load_data (dataDir:string) (settings:Crn_settings<'a>) = 
  failwith "Not implemented in JavaScript yet"
#else
open System.IO

let load_file (file_name:string) = 
  let contents = File.ReadAllText file_name
  contents

let write_file (file_name:string) (file_contents:string) =
  file_name |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
  File.WriteAllText(file_name,file_contents)

let set_working_dir prog = 
    let programName = Path.GetFileNameWithoutExtension prog
    let programDir = Path.GetDirectoryName prog
    let outDir = Path.Combine(programDir, programName + "_inference")
    let rec empty_trial trial =     // Append the trial sub-folder
      if (Directory.Exists (Path.Combine(outDir, "Trial" + trial.ToString() + "/"))) 
      then empty_trial (trial + 1) 
      else trial
    let trial = empty_trial 0
    let outDir = Path.Combine(outDir, "Trial" + trial.ToString() + "/")
    ignore(Directory.CreateDirectory outDir);     // Create the working directory (with trial sub-folder appended)
    outDir

let load_data dataDir (settings:Crn_settings<'a>) = 
  let num_cols_global = settings.simulation.plots.Length
  let num_cols_local  = 
    settings.simulations
    |> List.collect (fun sim_settings ->
      let num_plots = sim_settings.plots.Length
      List.map (fun d -> d, num_plots) sim_settings.data
    )
    |> Map.ofList
  let datasets = 
    settings.data 
    |> List.map (fun d -> 
      let files = Directory.GetFiles (dataDir, d.file + ".*")
      if files.Length < 1 then failwithf "No files matching %s" (dataDir + "/" + d.file + ".*");
      if files.Length > 1 then failwithf "Multiple files matching %s" (dataDir + "/" + d.file + ".*");
      let file_ext = Path.GetExtension files.[0]
      let num_cols = match Map.tryFind d.file num_cols_local with Some n -> n | None -> num_cols_global
      let data = 
        match file_ext with
        | ".txt" -> load_file (dataDir + "/" + d.file + ".txt") |> Table<float>.parse_multiple_tsv num_cols
        | ".tsv" -> load_file (dataDir + "/" + d.file + ".tsv") |> Table<float>.parse_multiple_tsv num_cols
        | ".csv" -> load_file (dataDir + "/" + d.file + ".csv") |> Table<float>.parse_multiple_csv num_cols
        | _     -> failwithf "Cannot load file with extension %s" file_ext
      Dataset.create d.file data
    )
  { settings with data = datasets }

#endif
