module Microsoft.Research.CliLibrary.Io

module Stringbuilder = Microsoft.Research.CRNEngine.Stringbuilder

open System
open System.IO
open Microsoft.Research.CRNEngine

let append_file (file_name:string) (file_contents:string) = 
  File.AppendAllText(file_name, file_contents)
      
let print (s:string) = 
  Console.Write s
  //append_file "log" s 
  //Console.Out.Flush() //This seem excessive and we aren't doing it elsewhere
       
let println (s:string) = 
  Console.WriteLine s
  //append_file "log" (s + Environment.NewLine)
       
let write_file (file_name:string) (file_contents:string) = 
  File.WriteAllText(file_name,file_contents)

let read_file (file_name:string) : string =
  File.ReadAllText file_name

let make_valid_filename (filename:string) : string = 
  let invalid_chars : char [] = System.IO.Path.GetInvalidFileNameChars()
  let valid_parts = filename.Split invalid_chars
  String.Join("_", valid_parts)

(* --- Display functions --- *)
(* Produce the table header (i.e. species names) *)
let display_simulation_header (sb:Stringbuilder.t) (crn:Crn) species_namer std =
  let settings = crn.settings
  let headers = match settings.simulator with
                | Simulator.PDE -> 
                        let xmax = settings.spatial.xmax
                        let nx = settings.spatial.nx
                        let gridpointsX = List.init nx (fun i -> (float i)*xmax/((float nx)-1.0))
                        match settings.spatial.dimensions with
                        | 1 -> List.map string gridpointsX
                        | 2 -> 
                            let ymax = xmax
                            let ny = nx
                            let gridpointsY = List.init ny (fun i -> (float i)*ymax/((float ny)-1.0))
                            let gridpoints = Lib.cartesian gridpointsX gridpointsY
                            List.map (fun (x,y) -> "x=" + string x + ", y=" + string y) gridpoints
                        | _ -> failwith "Only dimensions 1 and 2 are supported"
                | _ -> List.map (Expression.to_string species_namer) settings.simulation.plots
  let append (s:string) = Stringbuilder.append sb ("\t" + s)
  Stringbuilder.append sb "Time";
  //let species_finder = Populations.get_calculus_species (Term.get_populations eng) in
  if std 
  then 
    let temp = List.iter append (List.map (fun h -> h + " (Mean)") headers)
    List.iter append (List.map (fun h -> h + " (Std)") headers)
  else List.iter append headers

(* Display the trace of species populations over time as rows in a table. *)
let display_pointstore (sb:Stringbuilder.t) (pointstore:float list list) =
  let longest = Lib.fold_left (fun x points -> max x (List.length points)) 0 pointstore
  let showLine (points:float list) =
    match points with
      | [] -> ()
      | t::points ->
          Stringbuilder.append sb (string t);
          let append (f:float) = Stringbuilder.append sb ("\t" + (string f))
          List.iter append points;
          Lib.repeat (longest - (List.length (t::points))) (fun _ -> append 0.0)
  List.iter (fun (fs:float list) -> Stringbuilder.append sb "\r\n"; showLine fs) pointstore

(* Display simulation time course data. *)
let display_simulation_results crn species_namer (pointstore:float list list) std =
  let sb = Stringbuilder.empty ()
  display_simulation_header sb crn species_namer std
  display_pointstore sb (List.rev pointstore)
  Stringbuilder.value sb

(* Create an simulation/inference directory structure for saving *)
let prepare_files_directories program key (env:Environment.t) tag = 
  let programName = Path.GetFileNameWithoutExtension program
  let programDir = Path.GetDirectoryName program
  let outDir = Path.Combine(programDir, programName + "_simulation")
  let sweepDir = Path.Combine(outDir, key)
  
  let filename = make_valid_filename tag

  if env = Environment.empty
  then 
    Directory.CreateDirectory outDir |> ignore
    Path.Combine(outDir, filename + ".tsv")
  else 
    let _ = Directory.CreateDirectory sweepDir
    let assignment = Environment.to_string env
    if assignment = ""
    then Path.Combine(sweepDir, filename + ".tsv")
    else Path.Combine(sweepDir, filename + "_" + assignment + ".tsv")

let exportSimJson result =
    let fileName = "results.json";
    printfn "Saving results to %s" fileName
    let json = WebSharper.Json.Serialize result
    let sb = new System.IO.StreamWriter(fileName)
    sb.Write(json);
    sb.Close()

let to_file_vertical (fname:string) (separator:string) (value_to_string:'v -> string) (tab:Table<'v>) = 
    let sb = new System.IO.StreamWriter(fname)
    sb.WriteLine ("Time"::(Table.get_column_names tab) |> String.concat separator)
    Table.to_rows tab |> List.iter (fun row -> sb.WriteLine(Row.to_string separator value_to_string row))
    sb.Close()

let to_file_horizontal (fname:string) (separator:string) (value_to_string:'v -> string) (tab:Table<'v>) = 
    let sb = new System.IO.StreamWriter(fname)
    sb.WriteLine ("Time"::(tab.times |> List.map string) |> String.concat separator)
    tab.columns |> List.iter (fun col -> sb.WriteLine (col.name :: (col.values |> List.map value_to_string) |> String.concat separator))   
    sb.Close()

(* TODO: decide which of the following definitions are still needed. Some are superseded by Table.to_string. 

(* Write a file for a Table with specified crn/sweep keys *)
let string_of_table_ordered delim (table:float Table) (keys:string[]) : string = 
  let data = Map.toList table.data in
  let data = keys 
             |> Seq.map (fun k -> data |> List.filter (fun (key,col) -> key.Contains k)) 
             |> List.concat in
  string_of_columnarrays delim table.time data


(* Write a file for a Table with an unspecified order *)
let string_of_table_unordered delim (table:float Table) : string = 
  let data = Map.toList table.data in
  string_of_columnarrays delim table.time data

let write_pmap program sweep_key env species_str (crn:Crn) times store min max =
  let filename = prepare_files_directories program sweep_key env (Crn.string_of_simulator crn.settings.simulator + "_" + species_str) in
  
  let sb = Stringbuilder.empty () in
  
  let append (s:string) = Stringbuilder.append sb ("\t" + s) in
  Stringbuilder.append sb "Time";
  
  for i = min to max do append (species_str + " = " + string i) done;

  display_pointstore sb (List.map2 (fun t x -> t :: (List.ofArray x)) times store);

  write_file filename (Stringbuilder.value sb);
  println("Wrote file: " + filename)
*)