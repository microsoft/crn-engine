module Microsoft.Research.GEC.Classic

open Microsoft.Research.CliLibrary
open Microsoft.Research.CRNEngine
open Argu

type GECArgs = 
  | Parts of string
  | Reactions of string
  | DE
  | MG of int
  | CE

with
  interface IArgParserTemplate with
    member s.Usage = 
      match s with
      | Parts _     -> "Parts database"
      | Reactions _ -> "Reactions database"
      | DE          -> "Enumerate possible constructs from the given parts and constraints."
      | MG _        -> "Generate CRN model for the nth device enumerated by GEC."
      | CE          -> "Export the generated CRN model into an SVG image."


let printSVG crn =
  printf """<svg xmlns="http://www.w3.org/2000/svg" height="100%%" width="100%%">
  <style>
  .crnsvgvalue {
      text-anchor: end;
      stroke: black;
      fill: black;
      stroke-width: 0;
      font-family: Verdana, Arial, sans-serif;
      font-size: 15px;
  }
  .crnsvgrate {
      text-anchor: middle;
      stroke: black;
      fill: black;
      stroke-width: 0;
      font-family: Verdana, Arial, sans-serif;
      font-size: 15px;
  }
  path.crnsvgrate { stroke-width: 2; fill: none; stroke-linejoin: round }
  .crnsvgplus { text-anchor: start }
  .crnsvgspeciestext { text-anchor: start }
  </style>
  
  """

  crn 
  |> Crn.reactions_to_svg
  |> Svg.to_element_string
  |> printfn "%s"

  printfn "<text>"
  crn.all_species ()
  |> List.iter (fun s -> match crn.attributes.TryFind s.name with 
                         | None -> ()
                         | Some a -> printfn "<tspan x=\"400\" dy=\"1.2em\" font-size=\"1.3em\">%s = %s</tspan>" a.name (System.Web.HttpUtility.HtmlEncode a.structure |> String.filter(fun c -> c <> '\"')))
  printfn "</text>"
  printfn "</svg>"


[<EntryPoint>]
let main args =
  // Enforce invariant culture (prevents issues with decimal separators on international systems).
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  
  // Start by extracting the parts and reactions databases from the argument list
  let gec_parser = ArgumentParser.Create(programName = "ClassicGEC")
  let gec_results = gec_parser.Parse(args, ignoreUnrecognized=true)
  let dbParts = 
    match gec_results.TryGetResult Parts with 
    | Some f -> System.IO.File.ReadAllText f
    | None   -> Databases.defaultParts
  let dbReactions = 
    match gec_results.TryGetResult Reactions with
    | Some f -> System.IO.File.ReadAllText f
    | None   -> Databases.defaultReactions

  // Here we assume that we are only interested in the first solution, equivalent to how the GUI handles inference from a GEC program
  let parser code = 
    match Main.parse code with 
    | Program.t.ClassicGec _ -> 
      let solveResult = GECEngine.solveGEC (ref false) code dbParts dbReactions
      let firstSolution = GECEngine.getCrnAssignment solveResult.graph solveResult.solution 0 
      firstSolution.model
    | Program.t.LogicGec bundle -> 
      let cle = LogicGEC.cle
      if gec_results.Contains DE
        // generate all models
        then 
          LogicGEC.enumerateDevices cle bundle.rules bundle.program
          |> List.map (fun p -> p |> List.map (LogicGEC.Instruction.ToString cle) |> String.concat " | ")
          |> List.iteri (fun i s -> printfn "Solution %i: %s" i s)

          // return empty graph
          { task  = None
            nodes = Map.empty 
            edges = Map.empty
            expanded  =false }

      elif gec_results.Contains MG
        then 
          let i = gec_results.GetResult MG

          // get ith model
          let solutions = LogicGEC.enumerateDevices cle bundle.rules bundle.program
          let max = solutions.Length
          if 0 <= i && i < max
            then 
              // generate ith model
              let sol = solutions.Item i
              let crn = LogicGEC.generateCRN cle bundle.rules sol
              
              // return IGraph with a single node
              let crnSettings = Crn_settings.defaults.from_default_directive_list bundle.settings.directives  

              if gec_results.Contains CE
                then printSVG crn
                else 
                  printf "%s" (crn.to_string ())
                  printfn "\n"
                  crn.all_species ()
                  |> List.iter (fun s -> match crn.attributes.TryFind s.name with 
                                         | None -> ()
                                         | Some a -> printfn "%s = %s" a.name (a.structure |> String.filter (fun c -> c <> '"')))
              { task  = None
                nodes = Map.ofList [sprintf "Model_%i" i, Model.create crnSettings [crn]] 
                edges = Map.empty
                expanded  =false }
            else failwithf "Input index %i is out of bound, the total number of solutions found is %i" i max
      else failwith "Please specify whether to do construct assembly or model generation for the given Logic GEC program."
  // Call generic CLI program
  try 
    let ret = Program.main parser args
    ret
  with e -> 
    match e with 
    | :? Parser.Exception as e -> match e.Errors with 
                                  | [| {row=r; column=c; text=t} |] -> printfn "Parsing error at %i, %i: expecting %s" r c t
                                                                       -1
                                  | _ -> raise e
    | _ -> raise e