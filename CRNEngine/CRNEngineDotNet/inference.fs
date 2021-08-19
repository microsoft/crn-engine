[<JavaScript>]
module Microsoft.Research.CRNEngine.Inference
open Microsoft.Research.Filzbach
open Parser
open Microsoft.Research.Filzbach.Filzbach

type table = Table<float>
type result = Microsoft.Research.CRNEngine.Result<float>
type full = {
  likefn: DataStructures.AssociativeArray<float> -> float;
  simfn:  bool -> DataStructures.AssociativeArray<float> -> result list;
  parameters: Parameters.Parameter list;
  options: Filzbach.RunOptions;
  numData: int;
}
type partial = {
  likefn: Parameters.Ids -> DataStructures.AssociativeArray<float> -> float list;
  simfn:  bool -> Parameters.Ids -> DataStructures.AssociativeArray<float> -> result list;
  parameters: Parameters.Parameter list;
  dependencies: Parameters.Ids list;
  options: Filzbach.RunOptions;
  numData: int;
}
type t = Full of full | Partial of partial
type McmcSummary = {
  seed : uint32
  burnin : int
  samples : int
  thin : int
  mle : float
  aic : float
  bic : float
  dic : float
  numData : int
}
with
  static member defaults = {seed=0u; burnin=0; samples=0; thin=1; mle=0.0; aic=0.0; bic=0.0; dic=0.0; numData=0}
  member ms.to_string () = 
    sprintf "mcmcSummary {seed = %d; burnin = %d; samples = %d; thin = %d; mle = %f; aic = %f; bic = %f; dic = %f; numData = %d}" ms.seed ms.burnin ms.samples ms.thin ms.mle ms.aic ms.bic ms.dic ms.numData
  static member parse : Parser.t<McmcSummary> =
    kw "mcmcSummary" >>.
      Parser.record McmcSummary.defaults
        [
          "seed"    , Parser.pint32 |>> fun d x -> { x with seed = uint32 d }
          "burnin"  , Parser.pint32 |>> fun d x -> { x with burnin  = d }
          "samples" , Parser.pint32 |>> fun d x -> { x with samples = d }
          "thin"    , Parser.pint32 |>> fun d x -> { x with thin = d }
          "mle"     , Parser.pfloat |>> fun d x -> { x with mle     = d }
          "aic"     , Parser.pfloat |>> fun d x -> { x with aic   = d }
          "bic"     , Parser.pfloat |>> fun d x -> { x with bic   = d }
          "dic"     , Parser.pfloat |>> fun d x -> { x with dic   = d }
          "numData" , Parser.pint32 |>> fun d x -> { x with numData = d }
        ] 
  static member from_string = Parser.from_string McmcSummary.parse


type Summary = { mcmc: McmcSummary; parameters: Map<string,ParameterSummary> }
with
  member s.to_string () = s.mcmc.to_string() :: (s.parameters |> Map.toList |> List.map (snd >> ParameterSummary.to_string)) |> String.concat "\r\n"
  static member parse : Parser.t<Summary> = 
    McmcSummary.parse .>>. many ParameterSummary.parse 
    |>> fun (mcmc,paras) -> 
      let pmap = paras |> List.map (fun p -> p.name, p) |> Map.ofList
      { mcmc=mcmc; parameters=pmap}
  static member from_string = Parser.from_string Summary.parse
  static member select_summaries_by_seeds seeds summaries = 
    seeds
    |> List.map (fun seed -> 
      match Map.tryFind seed summaries with 
      | Some ps -> ps 
      | None -> failwithf "Results for seed %d not available" seed
    )    
  static member filter_summaries_by_lowerbound lb = List.filter (fun (s:Summary) -> s.mcmc.mle > lb)
  static member combine lowerbound seeds (unfiltered:Map<uint32,Summary>) =
    let summaries = 
      unfiltered
      |> match seeds with
          | [] -> Map.toList >> List.map snd
          | seeds -> Summary.select_summaries_by_seeds seeds
      |> match lowerbound with
          | Some lowerbound -> Summary.filter_summaries_by_lowerbound lowerbound
          | None -> id              
    let pnames = summaries.Head.parameters |> Map.toList |> List.map fst
    let samples = (float summaries.Head.mcmc.samples) / (float summaries.Head.mcmc.thin)
    let numChains = summaries.Length
    let mle = summaries |> List.maxBy (fun s -> s.mcmc.mle)
    let parameters = 
      pnames |> List.map (fun pname -> 
        let (pooled_var,pooled_mean,pooled_samples) = summaries |> List.map (fun ps -> ps.parameters.[pname].variance, ps.parameters.[pname].mean, samples) |> Statistics.pooled_variance
        pname, { mle.parameters.[pname] with mean=pooled_mean; variance=pooled_var }    // Adopting median and percentile values from MLE chain
      )
    { mcmc={ mle.mcmc with samples=mle.mcmc.samples*numChains; burnin=mle.mcmc.burnin*numChains}; parameters=Map.ofList parameters }

type mcmc_result = {
  parameters: ParameterSummary list
  posterior: Map<string,float> list
  burnin: Map<string,float> list
  pfixed: Map<string,float>
  mle: Map<string,float>
  mlesims: result list
  postsims: result list
  summary: McmcSummary
}
with 
  member r.to_summary () : Summary = 
    let pfixed = r.pfixed |> Map.map (fun k v -> { ParameterSummary.defaults with name=k; mean=v; mle=v })
    { mcmc=r.summary; parameters=r.parameters |> List.fold (fun acc p -> Map.add p.name p acc) pfixed }
type mcmc_intermediate_result = {
  iteration: int;
  state: Filzbach.RunPhase;
  mlesims: result list;
  summary: string
}

let env_of_aa (assignments:DataStructures.AssociativeArray<float>) env =
  assignments.Names |> Array.fold (fun m s -> Map.add s assignments.[s] m) env

let fb_parameters parameters = List.map Parameter.to_filzbach parameters
let options (settings:Inference_settings) : Filzbach.RunOptions = { 
    burninLen = settings.burnin; 
    sampleLen = settings.samples; 
    thinning = settings.thin; 
    seed = settings.seed;
    print_console = settings.print_console
    print_summary = settings.print_summary
  } 
let penalty noise_model sigma sim_val data_val =
  if System.Double.IsNaN data_val then 0.0 else           // Missing numbers can be inserted into datasets as nans.
  match noise_model with
  | Constant -> Statistics.log_of_normal_density(data_val, sim_val, sigma)
  | Proportional -> 
    if sim_val <= 0.0 
    then 
      printfn "Cannot evaluate a proportional-error noise model for non-positive simulation values."
      raise (Errors.negative_simulation ())
    else Statistics.log_of_normal_density(data_val, sim_val, (sqrt sim_val) * sigma)   

let penalties num_plottables (settings:Inference_settings) (parameters:DataStructures.AssociativeArray<float>) = 
  match settings.noise_parameter with 
  | Multiple -> List.init num_plottables (fun i -> penalty settings.noise_model parameters.[sprintf "sigma_%d" i])
  | _        -> List.replicate num_plottables (penalty settings.noise_model parameters.["sigma"])

let variable_sigma name : Parameters.Parameter = 
  { name = name; range = { pType = Parameters.ParameterType.Log; lb = 1e-6; ub = 1e6 }; initValue = None; prior = None; summary = true }

let generate_sigmas noise_parameter (plottables:'a list) : Parameters.Parameter list =
  match noise_parameter with
  | Fixed f -> [ { name = "sigma"; range = { pType = Parameters.ParameterType.Fixed; lb = f; ub = f }; initValue = Some f; prior = None; summary = false } ]
  | Random  -> [ variable_sigma "sigma" ]
  | Multiple -> List.init plottables.Length (fun i -> variable_sigma (sprintf "sigma_%d" i))

let num_datapoints data = data |> List.sumBy (fun dataset -> dataset.columns |> List.sumBy (fun col -> col.values.Length))

let create (settings:Inference_settings) (simulate:(bool -> Environment.t -> result list)) (plottables:Functional list list) (data:table list) (parameters:Parameter list) : t = 
  let unique_plottables = plottables |> List.concat |> List.distinct
  let plottable_ids = plottables |> List.map (List.map (fun pl -> unique_plottables |> List.findIndex (fun u -> pl=u)))
  let sigmas = generate_sigmas settings.noise_parameter unique_plottables
  let simfn with_times aa = 
    let env = env_of_aa aa Map.empty
    simulate with_times env
  let mutable errors = 0
  let likefn (parameters_others:DataStructures.AssociativeArray<float>) =
    let pens = penalties unique_plottables.Length settings parameters_others
    try
    #if JavaScript
      let sim:table list = List.map result.to_table <| simfn true parameters_others
    #else
      //let stopwatch = System.Diagnostics.Stopwatch.StartNew()
      let sim:table list = List.map result.to_table <| simfn true parameters_others
      //stopwatch.Stop()
      //let duration = (float stopwatch.ElapsedMilliseconds)/1e3
      //if settings.timer then printfn "\n%s: %1.3f seconds" (env_of_aa parameters_others Map.empty |> Environment.to_string) duration
    #endif    
      List.mapi2 (fun id s d -> Table.likelihood (List.map (fun i -> pens.[i]) plottable_ids.[id]) s d) sim data
      |> List.sum
    with 
    | :? Errors.SimulatorErrorException -> 
        errors <- errors + 1;
        System.Double.MinValue
    | :? Errors.EngineException as ex -> failwith ex.Message
  in
  Full { 
    likefn = likefn; 
    simfn = simfn; 
    parameters = sigmas @ fb_parameters parameters
    options = options settings; 
    numData = num_datapoints data;
  }

let create_with_dependencies (settings:Inference_settings) (simulate:(bool -> Environment.t -> Parameters.Ids -> result list)) (plottables:Functional list list) (data:table list) (parameters:Parameter list) dependencies = 
  let unique_plottables = plottables |> List.concat |> List.distinct
  let plottable_ids = plottables |> List.map (List.map (fun pl -> unique_plottables |> List.findIndex (fun u -> pl=u)))
  let sigmas = generate_sigmas settings.noise_parameter unique_plottables
  let sigma_dependencies = 
    match settings.noise_parameter with 
    | Fixed _ -> []
    | Random -> [Parameters.All]
    | Multiple -> List.replicate sigmas.Length Parameters.All
  let simfn with_times ids aa = simulate with_times (env_of_aa aa Map.empty) ids  
  let mutable errors = 0
  let likefn subset (parameters_others:DataStructures.AssociativeArray<float>) =
    let pens = penalties unique_plottables.Length settings parameters_others
    try
    #if JavaScript
      let sim:table list = List.map result.to_table <| simfn true subset parameters_others 
    #else
      //let stopwatch = System.Diagnostics.Stopwatch.StartNew()
      let sim:table list = List.map result.to_table <| simfn true subset parameters_others
      //stopwatch.Stop()
      //let duration = (float stopwatch.ElapsedMilliseconds)/1e3
      //if settings.timer then printfn "\n%s: %1.3f seconds" (env_of_aa parameters_others Map.empty |> Environment.to_string) duration
    #endif    
      match subset with 
      | Parameters.Subset ids -> List.map2 (fun s id -> Table.likelihood (List.map (fun i -> pens.[i]) plottable_ids.[id]) s data.[id]) sim ids
      | Parameters.All        -> List.mapi2 (fun id s d -> Table.likelihood (List.map (fun i -> pens.[i]) plottable_ids.[id]) s d) sim data
    with
    | :? Errors.SimulatorErrorException -> 
      errors <- errors + 1;
      List.init data.Length (fun _ -> System.Double.MinValue)
    | :? Errors.EngineException as ex -> failwith ex.Message
  Partial { 
    likefn = likefn; 
    simfn = simfn; 
    parameters = sigmas @ fb_parameters parameters 
    dependencies = sigma_dependencies @ dependencies
    options = options settings; 
    numData = num_datapoints data;
  }

let evToMap (ev:Parameters.EvaluatedValues) = 
  let extras = Map.ofArray [|"Iteration", float ev.iteration; "logLikelihood", ev.logLikelihood; "logPrior", ev.logPrior |]
  ev.values.ToMap() |> Map.foldBack Map.add extras
let analyse_chain sampling = 
  let psummary = FilzbachAnalysis.getSummary sampling |> List.ofArray |> List.map ParameterSummary.fromFilzbach
  let posterior = sampling.chain |> List.rev |> List.map evToMap
  let burnin = sampling.burnin |> List.rev |> List.map evToMap
  let mle = evToMap sampling.mle
  posterior, mle, psummary, burnin

let getSummary (o:Filzbach.RunOptions) numData sampling = 
  let ics = FilzbachAnalysis.informationCriteria sampling numData
  { seed = o.seed; burnin = o.burninLen; samples = o.sampleLen; thin = o.thinning; mle = sampling.mle.logLikelihood; aic = ics.AIC; bic = ics.BIC; dic = ics.DIC; numData = numData }

let convert_sampling simfn mlesims parameters o numData sampling : mcmc_result =
  let summary = getSummary o numData sampling
  let pfixed = parameters |> List.choose (fun (p:Parameters.Parameter) -> if p.isFixed() then Some (p.name, p.initValue.Value) else None) |> Map.ofList
  let posterior, mle, psummary, burnin = analyse_chain sampling
  let simulations = sampling.chain |> List.rev |> List.choose (fun p -> try Some (simfn p.values) with _ -> None)
  let postsims =
    if simulations = [] then [] else
        simulations.Head |> List.mapi (fun i r0 ->
            { instance = r0.instance
            ; table = simulations |> List.map (fun sim -> sim.[i].table) |> Table<float>.qsummary
            }
        )
  { parameters = psummary; pfixed = pfixed; posterior = posterior; burnin = burnin; mle = mle; summary = summary; mlesims = mlesims; postsims = postsims }


/// Run MCMC
let run_mcmc (i:t) =
  let sampling, parameters, o, numData, simfn = 
    match i with
    | Full s -> Filzbach.run s.likefn s.parameters s.options, s.parameters, s.options, s.numData, s.simfn true
    | Partial m -> FilzbachMultiple.run m.likefn m.parameters m.dependencies m.options, m.parameters, m.options, m.numData, m.simfn true Parameters.All
  let mlesims = try simfn sampling.mle.values; with :? Errors.SimulatorErrorException -> []
  //let summary = FilzbachAnalysis.stringOfSummary (Filzbach.SamplingPhase sampling) options numData;
  convert_sampling simfn mlesims parameters o numData sampling


/// Run MCMC, returning a sequence of results (for GUI)
let run_mcmc_seq problem (last_result:mcmc_result->unit) =
  let mutable lastLk = System.Double.MinValue
  let mutable lastResults = []
  let mutable lastRunPhase = None
  let states, parameters, o, numData, simfn = 
    match problem with 
    | Full s    -> Filzbach.run_seq s.likefn s.parameters s.options, s.parameters, s.options, s.numData, s.simfn
    | Partial m -> FilzbachMultiple.run_seq m.likefn m.parameters m.dependencies m.options, m.parameters, m.options, m.numData, fun wt -> m.simfn wt Parameters.All
  seq {
      let mutable c = 0
      for state in states do
        let summary = FilzbachAnalysis.stringOfSummary state o numData
        lastRunPhase <- Some state
        //let summary = { burnin = o.burninLen; samples = o.sampleLen; mle = state.logLikelihood; aic = ics.AIC; bic = ics.BIC; dic = ics.DIC }
        let mlesims =
          let mle = 
            match state with
            | Filzbach.BurninPhase(burnin)      -> burnin.mle
            | Filzbach.SamplingPhase(sampling)  -> sampling.mle
          try 
            let results = 
              if mle.logLikelihood > lastLk 
              then 
                simfn false mle.values
              else lastResults
            lastLk <- mle.logLikelihood
            lastResults <- results
            results
          with 
          | :? Errors.SimulatorErrorException -> lastResults
        c <- c+1
        yield { iteration = c; state = state; mlesims = mlesims; summary = summary }
      done
      match lastRunPhase with
      | Some (Filzbach.SamplingPhase s) ->
        let result = convert_sampling (simfn true) lastResults parameters o numData s
        last_result result
      | _ -> failwith "inference did not return a sampling"
  }

type sample_prior_result = {
  loglikelihood: float;
  parameters: Environment.t;
  simulations: result list
}

let prior_prediction num_samples (settings:Inference_settings) (simulate:(bool -> Environment.t -> result list)) (plottables:Functional list list) (data:table list) (parameters:Parameter list) = 
  let unique_plottables = plottables |> List.concat |> List.distinct
  //let plottable_ids = plottables |> List.map (List.map (fun pl -> unique_plottables |> List.findIndex (fun u -> pl=u)))
  let simfn ps = 
    try simulate true ps
    with 
    | :? Errors.SimulatorErrorException -> []
    | :? Errors.EngineException as ex -> failwith ex.Message
  let likefn sims (parameters:Environment.t) =
    let sigma i = 
      match settings.noise_parameter with
      | Fixed f -> f
      | Random  -> 
        match Map.tryFind "sigma" parameters with 
        | Some s -> s 
        | None -> printfn "** Warning: No noise parameter found, using value 1.0 **"; 1.0
      | Multiple -> failwith "Not supported yet" //parameters.[sprintf "sigma_%d" i]
    let pens = List.init unique_plottables.Length (fun i -> penalty settings.noise_model (sigma i))
    if List.isEmpty sims
    then nan
    else List.fold2 (fun acc s d -> acc + Table.likelihood pens s.table d) 0.0 sims data
    
  let rng = Rng.Random (int settings.seed)
  let rec sampler count res =
    if count > num_samples
    then List.rev res
    else 
      let ps = 
        parameters 
        |> List.map (fun p -> 
          ( p.name
          , match p.prior with
            | Some pr -> pr.distribution.sample rng
            | None -> p.value
          )
        )
        |> Map.ofList
      match simfn ps with
      | [] -> sampler count res
      | sims ->
        try 
          let lgLike = likefn sims ps 
          printfn "- Sample %d: log-likelihood %f" count lgLike
          let result : sample_prior_result = { loglikelihood = lgLike; parameters = ps; simulations = sims } 
          sampler (count+1) (result::res)
        with :? Errors.SimulatorErrorException -> 
          printfn "- Errors during simulation. Trying a new sample..."
          sampler count res
  sampler 1 []