//module Microsoft.Research.CRNEngine.Model
namespace Microsoft.Research.CRNEngine
open Operators
open Inference

//open WebSharper.Core.ContentTypes

[<JavaScript>]
type Model = 
  { top:Crn; systems:Crn list }
  static member create (s:Crn_settings<Functional>) (systems:Crn list) = 
    { top = {Crn.empty with settings = s}; systems = systems }
  member model.update_settings (s:Crn_settings<Functional>) = 
    { model with top = model.top.update_settings s }
  member model.merge_shared_settings () =
    // Propagate shared settings to individual crns. 
    let systems' = 
      model.systems 
      |> List.map (fun m -> 
          { m with settings = { m.settings with parameters = model.top.settings.parameters @ m.settings.parameters }})
    { model with systems = systems' }
  member model.map_crns (mapper:Crn->Crn) : Model =
    { top = mapper model.top; systems = List.map mapper model.systems }
  member model.simulate () = 
    let systems' = model.merge_shared_settings().systems
    let crns = (model.top :: systems')
    List.collect (fun (crn:Crn) -> crn.simulate_case()) crns
  //TODO: implement definition of local parameters for fitting. 
  ///We assume global parameters and inference settings, and local sweeps and data.
  member model.used_parameters () = 
    let used_names = (model.top :: model.systems) |> List.collect (fun crn -> crn.all_used_parameters()) |> List.distinct
    (model.top :: model.systems) 
    |> List.collect (fun crn -> crn.settings.parameters)
    |> List.filter (fun p -> List.exists (fun p' -> p' = p.name) used_names)
    |> List.distinct
  member model.species_to_rates () =
    let fix_crn (crn:Crn) =
      crn.disambiguate_species_to_rates()
    { top = fix_crn model.top 
      systems = model.systems |> List.map fix_crn }
  member model.trim_rates () = 
    if model.systems.IsEmpty
      then { model with top = { model.top with settings = { model.top.settings with rates = model.top.all_used_rates() } }}
      else { top = model.top.update_settings {model.top.settings with rates = Map.empty}
           ; systems = model.systems |> List.map (fun crn -> crn.update_settings {crn.settings with rates = crn.all_used_rates()})
           }
  member model.saturate_initials () =
    { top = model.top.saturate_initials()
    ; systems = List.map (fun (crn:Crn) -> crn.saturate_initials()) model.systems }
  member model.to_list_infer () =
    let filtered_model = { model with systems = model.systems |> List.filter (fun crn -> not crn.settings.data.IsEmpty) }.trim_rates ()
    let parameters = filtered_model.used_parameters()
    let odes = 
      (filtered_model.top::filtered_model.systems) 
      |> List.map (fun (crn:Crn) -> crn.saturate_initials().to_ode()) 
      |> List.filter (fun ode -> not ode.settings.data.IsEmpty)
    { filtered_model.top.settings with parameters = parameters }, odes 
  member model.infer () =  
    let global_settings, odes = model.to_list_infer ()
    Ode.list_infer global_settings odes
  member model.infer_seq (final_result:mcmc_result->unit) =
    let (settings,odes) = model.to_list_infer ()
    let i : Inference.t = Ode.list_to_inference settings odes
    
    // Retrieve the inference parameters, so I can return them.
    let parameters = 
        match i with
        | Inference.Full s -> s.parameters
        | Inference.Partial m -> m.parameters
    // Get the results stream.    
    let results = Inference.run_mcmc_seq i final_result
    
    // Below there is a bunch of results sequence transformations. The aim of this is adding mle_lglk_increased flag to each element. This flag is used on the GUI side for optimisation purposes.
    
    // Extend each result with mle lglk value and attach it to elements in the sequence.
    let results = 
        results |> Seq.map (fun r -> 
                       let mleLglk = 
                           match r.state with
                           | Microsoft.Research.Filzbach.Filzbach.BurninPhase p -> p.mle.logLikelihood
                           | Microsoft.Research.Filzbach.Filzbach.SamplingPhase p -> p.mle.logLikelihood
                       r, mleLglk)
    
    // Form the mleLglkIncreased flag and attach it to elements in the sequence.
    let results = 
        let folder (prev : Inference.mcmc_intermediate_result option * float * bool) (curr : Inference.mcmc_intermediate_result * float) = 
            let res, lglk = curr
            let prev_res, prev_lglk, _ = prev
            let increased = 
                match prev_res with
                | Some(r) -> lglk > prev_lglk
                | None -> true // Previous result 
            Some(res), lglk, increased
        results |> Seq.scan folder (None, -infinity, false)
    
    results, parameters
  member model.samplePrior num_samples = 
    let odes = (model.top::model.systems) |> List.map (fun (crn:Crn) -> crn.to_ode()) |> List.filter (fun ode -> not ode.settings.data.IsEmpty)
    Ode.prior_prediction num_samples model.top.settings odes
  member model.string () = 
    let systems_string = model.systems |> List.map (fun crn -> sprintf "system %s = { \n%s\n}" crn.name (crn.to_string()))
    (*let settings_directives_string = model.top.settings.to_string Functional2.to_string Functional2.to_string_plot
    String.concat "\n\n" (settings_directives_string :: systems_string)*)
    let top_string = model.top.to_string()
    String.concat "\n\n" (top_string :: systems_string)
  member model.to_sbml() =
    match model.systems with
    | [] -> model.top.to_sbml()
    | _ -> failwith "SBML export not implemented for multi-system models."
  member model.to_matlab() =
    match model.systems with
    | [] -> model.top.to_matlab()
    | _ -> failwith "Matlab export not implemented for multi-system models."
  static member stack_crn_svg_exports generator (model:Model) =
    let top_box = generator model.top
    let make_system_box (system:Crn) =
      let box = generator system
      let label = Svg.label_box (Some "c-export__system-header") 20.0 system.name
      Svg.stackv {x=0.0;y=0.0} 15.0 [label;box]
    let systems_boxes = model.systems |> List.map make_system_box
    let all_boxes = top_box::systems_boxes
    Svg.stackv {x=0.0;y=0.0} 50.0 all_boxes
  static member list_crn_svg_exports (generator:Crn -> Svg.box list) (model:Model) =
    let top_boxes = generator model.top
    let make_system_boxes (system:Crn) =
      let boxes = generator system
      let label = Svg.label_box (Some "c-export__system-header") 20.0 system.name
      boxes@[label]
    let systems_boxes = model.systems |> List.map make_system_boxes |> List.concat
    let all_boxes = top_boxes@systems_boxes
    all_boxes
  static member initials_to_svg (model:Model) : Svg.box = Model.stack_crn_svg_exports Crn.initials_to_svg model
  static member reactions_to_svg (model:Model) : Svg.box = Model.stack_crn_svg_exports Crn.reactions_to_svg model
  static member reactions_to_svgs (model:Model) : Svg.box list =
    let generator crn =
      List.map (Crn.reaction_to_svg crn) crn.reactions
    Model.list_crn_svg_exports generator model

  static member parse_with_systems (topSettings:Crn_settings<Functional>) moduleDefinitionList (systemList:Crn list) = 
    
    let parse_system (name:string) = 
        Crn.parse_with_modules_defaultSettings moduleDefinitionList {topSettings with parameters = []; rates = Map.empty; sweeps=[]} 
        |>> fun(crn,_) ->   
            (* Because of ambiguity in the grammar, species and rates are both defined as names within square brackets. 
               The CRN parser initially assumes that all names inside square brackets are species, and later distinguishes 
               between species and rates. If rates are defined both globally in the top settings and locally in a system, 
               rate names defined globally do not automatically replace species in the system. This is done manually here. *)
            let reactions' = crn.reactions 
                             |> List.map (Reaction.map id id (Expression.map (Crn.species_to_rates topSettings.rates)))
            let settings'  = crn.settings.map (Expression.map (Crn.species_to_rates topSettings.rates))
            {crn with name=name; settings=settings'; reactions = reactions'}
    
    let parse_system_with (name:string) (systemList:Crn list) = 
      Parser.name .>> Parser.spaces .>> Parser.kw "with" >>= fun(withSystemName) ->
        match (systemList |> List.tryFind (fun crn -> crn.name = withSystemName)) with 
        | Some(withCrn) -> 
          let withSettings = withCrn.settings
          Crn.parse_with_modules_defaultSettings moduleDefinitionList withSettings
          |>> fun (crn,_) -> 
            let reactions' = crn.reactions 
                             |> List.map (Reaction.map id id (Expression.map (Crn.species_to_rates topSettings.rates))) 
            let settings'  = crn.settings.map (Expression.map (Crn.species_to_rates topSettings.rates))
            let attributes' = Stringmap.fold (fun (acc:Stringmap.t<Attributes>) key value -> acc.Add(key,value)) crn.attributes withCrn.attributes 
            {crn with name=name;settings=settings';reactions=(withCrn.reactions@reactions');attributes=attributes';initials = (withCrn.initials@crn.initials)}
        | None -> failwith ("System " + withSystemName + " not defined")
            

    let systemParser = 
      Parser.kw "system" >>. (Parser.name .>> Parser.spaces)
      .>> Parser.kw "=" >>= fun(systemName) -> 
        Parser.plookAheadWith(Parser.choice[
          Parser.pTry(Parser.kw "{" >>. Parser.name .>> Parser.spaces .>> Parser.kw "with" |>> fun _ -> true)
          Parser.preturn(false)
          ]) >>= fun(hasWith) -> 
            if hasWith then
              Parser.braces (parse_system_with systemName systemList)
            else
              Parser.braces (parse_system systemName) 
    
    systemParser
   
  static member parse_systems (topSettings:Crn_settings<Functional>) moduleDefinitionList =
    let systemsParser =
      Parser.pfix (fun (x:Crn list) -> 
        (Model.parse_with_systems topSettings moduleDefinitionList x) 
        >>= (fun y -> Parser.preturn(x@[y]))) 
    systemsParser []  

  static member parse_model ((top:Crn),(moduleDefinitionList)) = 
    Parser.spaces >>. (Model.parse_systems top.settings moduleDefinitionList)
    |>> fun crnList -> {top = top; systems = crnList}
  
  static member parse:Parser.t<Model> = 
    (Crn.parse_with_modules [] Crn_settings.defaults)>>= fun s-> Model.parse_model s

  static member from_string = Parser.from_string Model.parse

  member model.load_data dataDir = 
(*#if JavaScript
    failwith "Can't load data using the Io module in JavaScript"
#else*)
    { systems = model.systems |> List.map (fun crn -> crn.update_settings (Io.load_data dataDir crn.settings))
    ; top     = model.top.update_settings (Io.load_data dataDir model.top.settings) 
    }
//#endif

  member model.simulate_with_uncertainity nsamples = 
      let crns = model.top :: model.systems
      // Fix the times so that simulations can be aggregated
      let crns_fixed_times = 
        crns 
        |> List.map (fun crn -> 
          let sim_settings = crn.settings.simulation
          let times = 
            let starttime = sim_settings.initial
            let printinterval = sim_settings.get_print_interval()
            // ND: Make the time vector one larger than requested, to ensure the final time is included.
            List.init (sim_settings.points+1) (fun index -> starttime + float index * printinterval)
          { crn with settings = { crn.settings with simulation = crn.settings.simulation.update_times times } }
        )
      
      let rng = Rng.Random()
      let parameters = model.top.settings.parameters
      let groups_simulations = 
        List.init nsamples (fun _ -> 
          let new_parameters = parameters |> Parameters.sample_from_prior rng
          crns_fixed_times
          |> List.collect (fun crn -> 
            { crn with settings = { crn.settings with parameters = new_parameters } }.simulate (fun crn -> crn.to_oslo().simulate()))
          |> List.groupBy (fun r -> (r.instance.model, r.instance.sweep))
          |> List.unzip
      )
      let groups, first_simulations = groups_simulations.Head
      let all_simulations = groups_simulations |> List.map snd 
      List.mapi (fun i group ->
        group, List.init first_simulations.Head.Length (fun j ->   // For each instance
          all_simulations |> List.map (fun sims -> sims.[i].[j]) |> Result<float>.qsummary
        )
      ) groups

  member this.expandAndLift () =
    let expandedSystems, pMaps = 
      this.systems
      |> List.map (fun crn -> 
            let ss, ps, paramsMap = 
              let filteredSweeps =
                if List.isEmpty crn.settings.simulation.sweeps 
                then crn.settings.sweeps
                else
                  ( crn.settings.sweeps @ this.top.settings.sweeps)
                  |> List.filter (fun s -> crn.settings.simulation.sweeps |> List.contains s.name)
              Parameters.expand_multiples [filteredSweeps] crn.settings.parameters
              |> fun (x,y,z) -> x.Head, y, z
            let lift pName = crn.name + "." + pName
            // lift parameter names
            let ps' = ps |> List.map (fun p -> { p with name = lift p.name})
            let paramsMap' = paramsMap |> Map.map (fun _ -> List.map lift)

            // lift sweep assignments
            let ss' = ss |> List.map (fun sweep -> 
              { sweep with assignments = 
                            sweep.assignments
                              |> List.map (fun a ->
                                  let updateValues =
                                            List.map 
                                                (List.map (Expression.map (fun x -> 
                                                  if ps |> List.exists (fun p -> p.name = x)
                                                  then lift x
                                                  else x)))
                                  { a with values = a.values |> updateValues
                                           variables = a.variables |> List.map (fun p -> if paramsMap.ContainsKey p then lift p else p)}
              )})

            // lift initials
            let liftParam p = if crn.settings.parameters |> List.exists (fun q -> q.name = p) then lift p else p
            let liftValue = Expression.map liftParam
            let initials' = crn.initials |> List.map (Initial.mapValues liftValue)

            // lift reactions
            let liftFunctional = Expression.map (fun k ->
              match k with 
              | Key.Parameter p -> Key.Parameter (liftParam p)
              | Key.Rate r      -> if crn.settings.rates.ContainsKey r
                                    then lift r
                                    else r
                                    |> Key.Rate
              | _               -> k)
            let reactions' = crn.reactions |> List.map (Reaction.map id liftValue liftFunctional)
                        
            // lift plots
            let plots' = 
              crn.settings.simulation.plots
              |> List.map liftFunctional

            // lift rates
            let rates' = crn.settings.rates 
                          |> Map.toList
                          |> List.map (fun (r,def) -> lift r, liftFunctional def)
                          |> Map.ofList
                          |> fun x -> Map.foldBack Map.add x this.top.settings.rates

            { crn with settings  = { crn.settings with sweeps      = ss'
                                                       parameters  = ps'     
                                                       simulation  = { crn.settings.simulation with plots = plots'} 
                                                       rates       = rates' }
                       initials  = initials'
                       reactions = reactions'}, (crn.name, paramsMap'))
      |> List.unzip
    // disambiguate rates from species, and trim rates after that
    { this with systems = expandedSystems |> List.map (fun sys -> sys.disambiguate_species_to_rates())
    }.trim_rates()
    , pMaps |> Map.ofList