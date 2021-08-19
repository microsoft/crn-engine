module Microsoft.Research.CliLibrary.Inference

open System.IO
open Microsoft.Research.CRNEngine
open System.Security.Cryptography
//open System.Runtime.Remoting.Messaging

let set_seed_dir seed prog = 
  let programName = Path.GetFileNameWithoutExtension prog
  let programDir = Path.GetDirectoryName prog
  let outDir = Path.Combine(programDir, programName)
  let outDir = Path.Combine(outDir, "Seed" + (string seed) + "/")
  ignore(Directory.CreateDirectory outDir);
  outDir

/// Write a program containing the optimized parameters
let write_optimized (model:Model) popt outDir =     
    //AP//let expanded_model = model.prepare_for_inference () //No longer needed. NB double-check this
    let expanded_model = model
    let optimized_parameters = expanded_model.top.settings.parameters |> Parameters.substitute popt
    let model_optimized = 
        { expanded_model 
          with 
            // Write the optimized parameters into the TOP settings.
            top = expanded_model.top.update_settings {model.top.settings with parameters = optimized_parameters };
            // Remove all parameters from systems, as they were produced by the sweep expansion, and are not needed
            systems = expanded_model.systems |> List.map (fun sys -> sys.update_settings {sys.settings with parameters = []})
        }
    let fname = sprintf "%s_optimized.crn" model.top.settings.inference.name
    Io.write_file (Path.Combine(outDir, fname)) (model_optimized.string())

let process_mcmc (outDir:string) writeHtml (model:Model) (result:Inference.mcmc_result) = 
    let settings = model.top.settings
    
    let writeSamples fname (samples:Map<string,float> list) = 
        let pnames = ["Iteration"; "logLikelihood"; "logPrior"] @ (result.parameters |> List.map (fun p -> p.name))
        let header = String.concat "\t" pnames
        let strsamples = samples |> List.map (fun s -> pnames |> List.map (fun p -> string s.[p]) |> String.concat "\t")     
        Io.write_file fname (String.concat "\r\n" (header :: strsamples))
    writeSamples (Path.Combine(outDir, sprintf "%s_posterior.tsv" settings.inference.name)) result.posterior
    writeSamples (Path.Combine(outDir, sprintf "%s_burnin.tsv" settings.inference.name)) result.burnin
  
    let combineWrite tag =
        Result<_>.group_sweeps
        >> List.iter (fun (rs:Result<_> list) -> 
            let combined_table = rs |> Seq.map (fun r -> r.table) |> List.ofSeq |> Table.concat
            let instance = rs.Head.instance
            let fname = 
                if instance.model = "" 
                then (if instance.sweep = "" then "sim" else instance.sweep) 
                else sprintf "%s_%s" instance.model instance.sweep
                |> sprintf "%s_%s.tsv" tag
            Table.to_string "\t" string combined_table
            |> Io.write_file (Path.Combine(outDir, fname))
        ) 
    // Write files for MLE simulations
    combineWrite "mle" result.mlesims 
    combineWrite "post" result.postsims
    
    // Write log file
    let logFilename = Path.Combine(outDir, "summary.txt")
    Io.write_file logFilename (result.to_summary().to_string())

    // Write HTML file
    if (writeHtml && not result.posterior.IsEmpty)
    then 
        let html_results = Html.mcmc_to_results settings model result 
        let html_content = Html.results_to_html_content html_results
        let html_string = Plotting.structured_tabbed "" "" html_content
        let html_fname = sprintf "%s.html" settings.inference.name
        Io.write_file (Path.Combine(outDir,html_fname))  html_string

let sample_prior num_samples (outDir:string) (model:Model) =  
    let result = model.samplePrior num_samples
    let simulations = 
        result.Head.simulations |> List.mapi (fun i r0 ->
            { instance = r0.instance
            ; table = result |> List.map (fun res -> res.simulations.[i].table) |> Table<float>.qsummary
            }
        )
        |> Result<_>.group_sweeps
    
    // Write HTML summary
    let data = (model.top :: model.systems) |> List.collect (fun crn -> crn.settings.data)
    let html_string = Html.prior_sample_to_html model data result
    let html_fname = sprintf "%s.html" model.top.settings.inference.name
    Io.write_file (Path.Combine(outDir,html_fname))  html_string

    // Write summary statistics for each instance in a combined table
    simulations
    |> List.iter (fun (rs:Result<_> list) -> 
        let combined_table = rs |> Seq.map (fun r -> r.table) |> List.ofSeq |> Table.concat
        let instance = rs.Head.instance
        let filetag = 
            if instance.model = "" 
            then (if instance.sweep = "" then "default" else instance.sweep) 
            else sprintf "%s_%s" instance.model instance.sweep
        Table.to_string "\t" string combined_table
        |> Io.write_file (Path.Combine(outDir, filetag + ".tsv"))) 

    // Write file containing the parameters and log-likelihood scores
    let header = result.Head.parameters |> Map.toList |> List.map fst |> List.append ["Sample"; "log-Likelihood"] |> String.concat "\t"
    let body = 
        result
        |> List.mapi (fun i r ->       
            (string i :: string r.loglikelihood :: (r.parameters |> Map.toList |> List.map (snd >> string))) |> String.concat "\t"
        )
    let contents = String.concat "\r\n" (header::body)
    Io.write_file (Path.Combine(outDir, "samples.tsv")) contents
  
    // Return the result
    result

//let internal do_neldermead outDir model = Map.empty, []

type algorithm = MCMC | Direct
type dirMode = Fresh | Seed
type options = 
    { algorithm     : algorithm
    ; writeHtml     : bool
    ; directoryMode : dirMode
    }
let do_inference options (model:Model) programName (copy:int option) programCode = 
    let outDir = 
      match options.directoryMode with
      | Fresh -> Io.set_working_dir programName
      | Seed  -> set_seed_dir model.top.settings.inference.seed programName
    Io.write_file (Path.Combine(outDir, "model.crn")) programCode; 
    
    Io.println ("Starting inference")
    let result = model.infer () 
    process_mcmc outDir options.writeHtml model result
    write_optimized model result.mle outDir
    result

/// Load parameter summaries
let load_summaries path = 
    Directory.EnumerateFiles(path, "*summary.txt", SearchOption.AllDirectories)
    |> Seq.map (fun f -> 
      let summary = f |> Io.load_file |> Inference.Summary.from_string
      printfn "- Seed %d: log-likelihood %1.3f" summary.mcmc.seed summary.mcmc.mle
      summary.mcmc.seed, summary
    )
    |> Map.ofSeq

let join_summaries lowerbound seeds path = 
    let combined = Inference.Summary.combine lowerbound seeds (load_summaries path)
    combined.to_string() |> Io.write_file (Path.Combine (path,"summary.txt"))