[<JavaScript>]
module Microsoft.Research.CRNEngine.Html
open Microsoft.Research.CRNEngine
open FSharpIDD;

type Comparison = { sim: Result<float> list; dat: Table<float> list; name : string; xlabel : string; ylabel : string }

/// General multi-plot for HTML
let subplots (width,height) ncols (charts:Chart.Chart list) =
    let nrows = 1+(charts.Length / ncols)
    Subplots.createSubplots nrows ncols (fun r c -> if charts.Length > r*ncols+c then Some charts.[r*ncols+c] else None)
    |> Subplots.setSubplotSize width height

let gridplot size ncols charts = subplots size ncols charts |> HTML.ofSubplots

/// General multi-plot for HTML (with a specified title) 
let gridplot_with_title title size ncols charts = subplots size ncols charts |> Subplots.setTitle title |> HTML.ofSubplots

// Generate HTML table
let table headers rows = 
    let hdr = headers |> List.map (sprintf "\t\t<th>%s</th>") |> String.concat "\n" |> sprintf "\t<tr>\n%s\n\t</tr>"
    let body = 
        rows
        |> List.map (fun els -> 
            els 
            |> List.map (sprintf "\t\t<td>%s</td>")
            |> String.concat "\n"
            |> sprintf "\t<tr>\n%s\n\t</tr>")
        |> String.concat "\n"
    sprintf "<table id=\"pTable\">\n%s\n%s\n</table>" hdr body    


/// Convert string representing CRN code into HTML
let codify (code:string) = 
    let keywords = ["directive"; "simulation"; "deterministic"; "inference"; "simulator"; "parameters"; "system"; "sweeps"; "data"]
    let general = ("<font face=\"courier\">" + code + "<font>").Replace("\n","<br> \n")
    keywords |> List.fold (fun (acc:string) kw -> acc.Replace(kw, "<font color=\"blue\">" + kw + "</font>")) general
    
//let get_samples (chain:Microsoft.Research.Filzbach.Parameters.Chain) (p:string) = chain |> List.map (fun sample -> sample.values.[p]) |> Array.ofList

type Results = 
    { model : string
    ; comparisons : Comparison list
    ; correlations : float[][]
    ; keys : (string * string list) list
    ; parameters : ParameterSummary list
    ; marginals : Map<string,float[]>
    ; multiple_paras : string list
    }

/// Create HTML code for insertion in tabbed format (name,content)
let mcmc_to_results (global_settings:Crn_settings<Functional>) (model:Model) (result:Inference.mcmc_result) : Results =    
    let marginals = result.posterior.Head |> Map.map (fun k v -> result.posterior |> List.map (fun sample -> sample.[k]) |> Array.ofList)
    let pnames = result.parameters |> List.map (fun p -> p.name)
    let pvalues = pnames |> List.map (fun n -> marginals.[n])
    let correlations = Statistics.correlation_pearson (Array.ofList pvalues)
     
    // Compile the inference result
    let crns = model.top :: model.systems
    let datasets = crns |> List.collect (fun crn -> crn.settings.data)
    let data_references = crns |> List.collect (fun crn -> crn.settings.simulations |> List.collect (fun sim -> sim.data))
    let selected_datasets = 
        if data_references = []
        then datasets
        else data_references |> List.map (fun dref -> List.find (fun ds -> ds.file = dref) datasets)
    let sims = Result<float>.group_sweeps result.mlesims
    let comparisons = 
      if sims.Length = selected_datasets.Length
      then
        (sims, selected_datasets)
        ||> List.map2 (fun s d -> 
            let settings = (crns |> List.find (fun crn -> crn.name = s.Head.instance.model)).settings
            { sim=s; dat=d.data; name=d.file; xlabel=settings.plot.x_label; ylabel=settings.plot.y_label }
        ) 
      else
        failwith "HTML report internal error: incompatible specification of simulations and data files"
    //let keys = result.mlesims |> List.map (fun s -> s.instance.name) |> Array.ofList
    let keys = selected_datasets |> List.map (fun d -> d.file, d.data |> List.map (fun di -> di.columns.Head.name.Split('(').[0]))

    // Determine which parameters are Multiple (unexpanded model)
    let multiple_paras = global_settings.parameters |> List.filter (fun p -> match p.prior with Some pr -> pr.variation = Variation.Multiple | None -> false) |> List.map (fun p -> p.name)
    
    // Determine which parameters were Multiple before expansion (TODO: hack)
    let num_instances = keys |> List.map (snd >> List.length) |> List.sum
    let all_parameters = model.top :: model.systems |> List.collect (fun sys -> sys.settings.parameters) //|> List.distinctBy (fun p -> p.name)
    let candidates = 
        all_parameters 
        |> List.filter (fun p -> p.name.Contains("_") && Parameter.variable p) 
        |> List.map (fun p -> 
            let elems = (p.name.Split('.') |> Array.last).Split('_')
            Array.sub elems 0 (elems.Length-1) |> String.concat "_"
        ) 
        |> List.groupBy id
    let expanded_paras = candidates |> List.filter (fun (mp,ps) -> ps.Length = num_instances) |> List.map fst
    let all_multiples = multiple_paras @ expanded_paras |> List.distinct

    { model = codify (model.string())
    ; comparisons = comparisons 
    ; correlations = correlations
    ; keys = keys
    ; parameters = result.parameters
    ; marginals = marginals
    ; multiple_paras = all_multiples
    }

let results_to_html_content (result : Results) : Plotting.html_content =     
    // Create marginal posterior plot
    let elements = ("logLikelihood",false) :: (result.parameters |> List.map (fun p -> p.name, p.ptype = Log))
    let marginals_plots = elements |> List.map (fun (p,logscale) -> Plotting.plot_marginal result.marginals.[p] p logscale) 

    // Create trace plots
    let trace_plots = List.map (fun (p,_) -> Plotting.plot_trace result.marginals.["Iteration"] result.marginals.[p] p) elements
    
    // Create variation plots
    let pnames = result.parameters |> List.map (fun p -> p.name)
    let variation_plots = 
        result.multiple_paras
        |> List.choose (fun mp -> 
            let ps = pnames |> List.filter (fun ps -> ps.Contains(mp+"_"))
            if List.isEmpty ps
            then None
            else
              let charts, width = Plotting.plot_variation result.marginals result.keys ps
              Some ( "Parameter variation: " + mp, gridplot (width,500) 1 charts)
        )
    
    // Create correlation plot
    let correlation_plot = Plotting.plot_correlation result.correlations pnames

    // Create comparison plots
    let comparison_tabs = 
        result.comparisons
        |> List.mapi (fun i c -> 
            let charts = Plotting.plot_comparisons c.sim c.dat None c.xlabel c.ylabel
            let cname = if result.comparisons.Length > 1 then sprintf "Comparison %d" (i+1) else "Comparison"
            cname, gridplot_with_title c.name (200,150) 6 charts 
        )

    // Create parameter summary table
    let header = ["Parameter name"; "Type"; "Lower bound"; "Upper bound"; "MLE"; "Posterior mean"; "Posterior stdev"; "Posterior l95"; "Posterior l68"; "Posterior median"; "Posterior u68"; "Posterior u95"]
    let rows = 
        result.parameters
        |> List.map (fun p -> 
#if JavaScript
            p.name :: Interval.to_string p.ptype :: (List.map (sprintf "%1.3f") [p.lb; p.ub; p.mle; p.mean; sqrt p.variance; p.l95; p.l68; p.median; p.u68; p.u95])
#else
            p.name :: Interval.to_string p.ptype :: (List.map (sprintf "%1.3g") [p.lb; p.ub; p.mle; p.mean; sqrt p.variance; p.l95; p.l68; p.median; p.u68; p.u95])
#endif
        )

    // Write HTML file
    { model = result.model
    ; comparisons = comparison_tabs
    ; posterior = gridplot (200,150) 6 marginals_plots
    ; correlation = HTML.ofChart correlation_plot
    ; traces = gridplot (300,150) 4 trace_plots 
    ; psummary = table header rows 
    ; variations = variation_plots 
    }

let prior_sample_to_html (model:Model) (data:Dataset list) (samples:Inference.sample_prior_result list) = 
    let simulations = 
        samples.Head.simulations |> List.mapi (fun i r0 ->
            { r0 with table = samples |> List.map (fun res -> res.simulations.[i].table) |> Table<float>.average_columns }
        )
        |> Result<_>.group_sweeps
    let plot_settings = model.top.settings.plot
    let comparisons = (simulations, data) ||> List.map2 (fun s d -> { sim=s; dat=d.data; name=d.file; xlabel=plot_settings.x_label; ylabel=plot_settings.y_label })
    let comparison_tabs = 
        comparisons
        |> List.mapi (fun i c -> 
            let charts = Plotting.plot_comparisons c.sim c.dat None c.xlabel c.ylabel
            let cname = if comparisons.Length > 1 then sprintf "Comparison %d" (i+1) else "Comparison"
            cname, gridplot_with_title c.name (200,150) 6 charts 
        )
    
    ("Model", codify (model.string())) :: comparison_tabs
    |> Plotting.arbitrary_tabbed "Posterior predictive distribution"