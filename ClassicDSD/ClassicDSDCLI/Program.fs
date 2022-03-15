// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DSD.Classic

open Microsoft.Research.CliLibrary
open Microsoft.Research.CRNEngine
open Microsoft.Research.DNA
open Argu 

type DSDArgs = 
  | Svg
with
  interface IArgParserTemplate with
    member s.Usage = 
      match s with
      | Svg     -> "Create SVG image of the species"

[<EntryPoint>]
let main args =
  // Enforce invariant culture (prevents issues with decimal separators on international systems).
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  
  let dsd_parser = ArgumentParser.Create(programName = "ClassicDSD")
  let dsd_results = dsd_parser.Parse(args, ignoreUnrecognized=true)
  let export_svg = dsd_results.Contains Svg

  let parser code : Microsoft.Research.CRNEngine.InferenceSiteGraph.IGraph = 
    let crn = Dsd.compile code

    // DSD-specific actions on a compiled program
    if export_svg
    then
      let svg = crn.to_svg() |> Svg.to_string Crn.default_svg_style
      let name = if crn.name = "" then System.IO.Path.GetFileNameWithoutExtension (Array.last args) else crn.name
      System.IO.File.WriteAllText (name + "_initials.svg", svg)
      printfn "Wrote file %s_initials.svg" name

    // Must return InferenceSiteGraph type for generic CLI tool
    { task = None
    ; nodes = Map.ofList ["DSDmodel", { top = crn; systems = [] }]
    ; edges = Map.empty
    ; expanded = false 
    }

  let parsedArgs = Program.args_results args
  match Program.get_program_name parsedArgs with 
  | None -> -1
  | Some path ->
    let code = Program.string_of_file path
    let bundle = code |> Dsd.parse
    if Dsd.is_jit bundle 
      then 
        if Program.hasSimulate parsedArgs
          then 
            let results = 
              match Dsd.to_jit bundle with
              | Choice1Of2 classicJit -> 
                let calculus = Dsd.makeDsdCalculus bundle
                Microsoft.Research.CRNEngine.Jit.simulate classicJit calculus |> snd
              
              | Choice2Of2 logicJit -> 
                let calculus = Dsd.to_rules_calculus bundle
                Microsoft.Research.CRNEngine.Jit.simulate logicJit calculus |> snd
            let _, fileOptions = Program.getFileOptions path parsedArgs  
            let to_file = if fileOptions.horizontal then Io.to_file_horizontal else Io.to_file_vertical
            let outDir = Simulation.prepareOutputDirectory fileOptions.name fileOptions.copy
            // let simDir = Simulation.prepareSimulationDirectory outDir i.model i.sweep
            to_file (System.IO.Path.Combine (outDir, "instance.tsv")) "\t" string results
            0
          else -1
      else
        let crn:Crn = Dsd.convert_expand bundle
        let () = Io.write_file ("./" + path + ".crn") (crn.to_string())
        let ret = Program.main parser args
        ret

  (*
  let do_rg = ref true
  let limit = ref 0
  let do_ss = ref false
  let do_ss_interactive = ref false
  let trace_input = ref None
  let limit_ss = ref 0
  let animate_reactions = ref false
  let pause = ref false
  let in_file = ref ""
  let verbose = ref false
  let switches = [ ArgInfo ("-no_rg", ArgType.Clear do_rg, "Do not compute reaction graph")
                 ; ArgInfo ("-limit", ArgType.Int (fun i -> limit := i), "Limit exploration of reaction graph")
                 ; ArgInfo ("-ss", ArgType.Set do_ss, "Compute state space")
                 ; ArgInfo ("-limit_ss", ArgType.Int (fun i -> limit_ss := i), "Limit exploration of state space")
                 ; ArgInfo ("-ss_interactive", ArgType.Set do_ss_interactive, "Explore state space interactively")
                 ; ArgInfo ("-animate_reactions", ArgType.Set animate_reactions, "Generate animated reactions")
                 ; ArgInfo ("-trace_input", ArgType.String (fun s -> trace_input := Some s), "Read interactive input from file")
                 ; ArgInfo ("-pause", ArgType.Set pause, "Pause after execution")
                 ; ArgInfo ("-v", ArgType.Set verbose, "Print .dot to screen") ]
  let anon_fun s = in_file := s
  let message = "provide one filename followed by options:"
  try
    ArgParser.Parse (switches, anon_fun, message);
  with Failure s -> printfn "%s" s

  if !in_file = "" then ArgParser.Usage (switches, message); 1
  else
    // run_rudimentary ();
    let program = System.IO.File.ReadAllText(!in_file)
    let enzymes, toehold_map, strands, ss = SiteGraphReactor.Sitegraphs.compile program
  
    if !do_rg then
      let dot = SiteGraphReactor.Sitegraphs.reaction_graph_to_svgdot !limit enzymes toehold_map strands (ss |> Seq.map snd |> List.ofSeq)
      // let dot = Sitegraphs.to_dot strands ss
      let out_file = System.IO.Path.ChangeExtension(!in_file, "dot")
      System.IO.File.WriteAllText(out_file, dot);
      printfn "Wrote file %s" out_file;
      if !verbose then printfn "%s" dot
  
    if !do_ss then
      let state_space = SiteGraphReactor.States.state_space !limit_ss enzymes toehold_map strands ss
      let ss_dot = SiteGraphReactor.Sitegraphs.state_space_to_svgdot strands state_space ss
      let ss_out_file = System.IO.Path.Combine(System.IO.Path.GetDirectoryName !in_file, sprintf "%s_ss.dot" (System.IO.Path.GetFileNameWithoutExtension !in_file))
      System.IO.File.WriteAllText(ss_out_file, ss_dot);
      printfn "Wrote state space file %s" ss_out_file;
      if !verbose then printfn "%s" ss_dot

    if !do_ss_interactive then
      let next_choice =
        match !trace_input with
        | None -> System.Console.ReadLine
        | Some tf ->
          let choices = System.IO.File.ReadAllLines tf
          let n = ref 0
          fun () ->
            let c = choices.[!n]
            n := 1 + !n
            c
      SiteGraphReactor.States.state_space_interactive next_choice !limit_ss !animate_reactions enzymes toehold_map strands ss
  
    if !pause then System.Console.ReadLine () |> ignore;
    
    0 // return an integer exit code
    *)
    
