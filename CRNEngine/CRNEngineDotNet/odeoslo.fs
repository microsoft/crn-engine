namespace Microsoft.Research.CRNEngine
open Oslo
[<JavaScript>]
type OdeOslo = 
  { 
    name: string
    simulator: Simulation<Functional>
    plots: Expression.t<Inlined<int>> array
    settings: Deterministic_settings
    stoich: Oslo.Matrix
    powers: (int*float) list array
    rateDs: Rate<float,Lambda<Inlined<int>>> array
    concs: Oslo.Vector
  }
  member ode.simulate_callback (cancel_flag:bool ref) (output:Row<float> -> unit) (output_finals:Populations<Species,float>->unit) =
    let rate_power x p = x ** p //assume deterministic dynamic semantics
    /// Get the population of a particular species from the concentrations vector
    let get_conc (concs:Oslo.Vector) (s:int) : float = concs.[s]
    let stoich=ode.stoich
    let powers=ode.powers
    let rateDs=ode.rateDs
    let fluxes = VectorFS.create (Array.length rateDs) 1.0
    //Function to do the matrix multiplications at each step
    let f (time:float) (x:Oslo.Vector) =
      let rec keySolver k = 
        match k with 
        | Inlined.Species sp -> get_conc x sp
        | Inlined.Time       -> time
        //| Inlined.IRate r     -> Expression.eval keySolver ode.rateExpressions.[r]
      for i = 0 to ((Array.length rateDs) - 1) do
        fluxes.[i] <- 
          match rateDs.[i] with
          | Rate.MassAction r -> List.fold (fun t (j,p) -> t * rate_power x.[j] p) r powers.[i]
          | Rate.Function f -> f.key keySolver
      Oslo.Matrix.matrMultVec stoich (Oslo.Vector.ofArray (fluxes.ToArray())) 
    let grab_data time concs = 
      let rec keySolver k = 
        match k with 
        | Inlined.Species sp -> get_conc concs sp
        | Inlined.Time       -> time    
      ode.plots 
      |> Array.map (Expression.eval keySolver)
    let integrator = 
      Oslo_integrator.create ode.name ode.simulator grab_data Row<float>.interpolate_float ode.settings ode.concs f
    let updated_integrator = Oslo_integrator.simulate_callback cancel_flag output integrator
    updated_integrator.concs |> Vector.toArray |> Array.iteri (fun i v -> updated_integrator.simulator.populations.set_population i v)
    output_finals updated_integrator.simulator.populations
    { ode with simulator = updated_integrator.simulator }
  member ode.process_simulation sim_data = 
    let times:float list = ode.simulator.settings.times
    let plots:string list = List.map Functional2.to_string_plot ode.simulator.settings.plots
    if List.isEmpty times
    then Table.from_rows_reverse plots sim_data
    else 
      let int_data = Row.interpolate_reverse Row<float>.interpolate_float (List.rev sim_data) times
      Table.from_rows_reverse plots int_data
  member ode.simulate_state () =
    let cancel = ref false
    let result = ref []
    let output (row:Row<float>) = result := row::!result
    let output_export _ = ()
    let final_sim = ode.simulate_callback cancel output output_export
    final_sim, ode.process_simulation !result
  member ode.simulate () = snd <| ode.simulate_state()

[<JavaScript>]
type OsloAbstract = 
  {
    name: string
    settings: Crn_settings<Functional>
    matrix: float[][]
    powers: (int*float) list array
    rates: Rate<Value,Expression.t<Key<int>>> array
    species: Map<Species,int>
    initials: Initial<Species,Value> list
  }
  static member empty = {
    name = "None"
    settings = Crn_settings<Functional>.defaults
    matrix   = [||]
    powers   = [||]
    rates   = [||]
    species = Map.empty
    initials = []
  }
  static member create name settings stoich powers rateDs (species_ids:Map<Species,int>) initials = {
    name = name
    settings = settings
    matrix = stoich
    powers = powers
    rates = rateDs
    species = species_ids
    initials = initials 
  }
  member ode.evaluate () : OdeOslo = // Updates the ode parameters with environment env
    let env:Environment.t = Parameters.to_env ode.settings.parameters
    let (populations:Populations<Species,float>), events = Initial<Species,Value>.to_initialpops_events env ode.settings.simulation.initial ode.initials
    let speciesToIndex sp = 
      match populations.tryFind_index sp with
      | Some pop -> pop 
      | None -> failwithf "Attempting to look up non-existent species %s" sp.name
    let concs = populations.get_pop_array 
    let simulator = Simulation.create populations events ode.settings.simulation 1.0
    let ratesEnv:Map<string,Expression.t<Inlined<int>>>  = 
      ode.settings.rates 
      |> Map.map (fun _ -> Expression.map (Key.map speciesToIndex))
      |> Key.inline_rates_env env
    let rs = Array.map (Rate.map2 (Key<int>.eval_massaction env) (Key<int>.eval_function env ratesEnv) ) ode.rates
    let inlineExpression = Expression.expand (Key.map speciesToIndex >> Key.inline_keys env ratesEnv) >> Expression.simplify
    { 
      name = ode.name
      simulator = simulator; 
      plots = 
        simulator.settings.plots 
        |> List.map inlineExpression
        |> Array.ofList
      settings = ode.settings.deterministic; 
      stoich = (Oslo.Matrix.ofArray ode.matrix); 
      powers = ode.powers; 
      rateDs = rs; 
      concs = Oslo.Vector.ofArray concs
    }

