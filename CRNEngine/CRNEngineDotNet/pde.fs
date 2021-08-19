namespace Microsoft.Research.CRNEngine

open Microsoft.Research.ReactionDiffusion
open Microsoft.Research.ReactionDiffusion.Lib
open System.Threading

[<JavaScript>]
type Pde<'a> when 'a:equality = 
  { 
    name: string
    species_index: Species -> int
    populations: Populations<Species,'a>
    sim_settings: Simulation_settings<Functional>
    settings: Spatial_settings<Functional>
    stoich: Oslo.Matrix
    powers: (int*float) list array
    rates: Rate<Value,Expression.t<Key<int>>> array
    rateExpressions: Map<string, Functional>
    rng: Rng.Random
  }
  static member create name (populations:Populations<Species,'a>) (sim_settings:Simulation_settings<Functional>) settings stoich powers rates rateExpressions rng = 
    { name = name
      species_index = populations.find_index
      populations = populations
      sim_settings = sim_settings
      settings = settings
      stoich = Oslo.Matrix.ofArray stoich
      powers = powers
      rates = rates 
      rateExpressions = rateExpressions
      rng = rng
      }
  static member reaction_term stoich (powers:(int*float)list []) rateDs : (float -> float[] -> float[]) = 
    let get_conc (concs:float[]) (s:int) : float = concs.[s]
    //We generally have integer powers, the general purpose routines aren't optimised when passing to those as floats
    let rate_power x p =
        match p with
        | 1.0 -> x
        | 2.0 -> x * x
        | _ -> x ** p //assume deterministic dynamic semantics
    //let fluxes = VectorFS.create (Array.length rateDs) 1.0
    let fluxes = Oslo.Vector.ofArray(Oslo.Vector.fastCreate (Array.length rateDs))

    //Allocation free approach, we don't need the thread safety yet but I don't want to leave a trap.
    //Failing to dispose is non-ideal but it makes a significant speed up difference (2x).
    //https://www.red-gate.com/simple-talk/blogs/subterranean-il-the-threadlocal-type/
    #if JavaScript
    let resultAsArray = Oslo.Vector.fastCreate (Oslo.Matrix.numRows stoich) 
    #else
    let x = new ThreadLocal<_>(fun () -> Oslo.Vector.fastCreate (Oslo.Matrix.numRows stoich))
    let resultAsArray = x.Value
    #endif

    let powersPowers = powers |> Array.map Array.ofList
    let powerIndices = powersPowers |> Array.map (fun nested -> nested |> Array.map fst)
    let powerValues = powersPowers |> Array.map (fun nested -> nested |> Array.map snd)

    let f (time:float) (x:float[]) =
      
      let rec keySolver k = 
        match k with 
        | Inlined.Species sp -> get_conc x sp
        | Inlined.Time       -> time
      for i in 0..((Array.length rateDs) - 1) do
          fluxes.[i] <-
              match rateDs.[i] with
              | Rate.MassAction r ->
                Array.fold2 (fun acc index power -> acc * rate_power x.[index] power) r powerIndices.[i] powerValues.[i]
              | Rate.Function (f) -> f keySolver
            
      Oslo.Matrix.matrMultVecStore stoich fluxes resultAsArray
      resultAsArray
    f
  static member map_settings env plotlocs (pde:Pde<'a>) = 
    let s = pde.settings
    let numspecies = pde.populations.get_count 
    let pops = pde.populations
    let D = 
      pops.index_to_species 
      |> Array.map (fun sp ->      
        match Map.tryFind (Expression.Key (Key.Species sp.species)) (Map.ofList s.diffusibles) with 
        | Some d -> d |> Expression.eval (Environment.find env)
        | None -> s.default_diffusion
      ) 
    let dx = float s.xmax / (float s.nx - 1.0)
    let dt = if s.dt > 0.0 then s.dt else 0.2*dx*dx / (Array.max D)
    { numspecies = numspecies
      thin = ((pde.sim_settings.final - pde.sim_settings.initial) / dt / (pde.sim_settings.points |> float)) |> ceil |> int
      nx = s.nx
      xmin = 0.0
      xmax = s.xmax
      ymin = 0.0
      ymax = s.xmax
      tmax = pde.sim_settings.final
      dt = dt
      D = D
      plotlocs = Array.ofList plotlocs
    }
  static member generate_initialconditions1d (rng:Rng.Random) (settings:Spatial_settings<'a>) (pops:Population<Species,float[]>[]) : float[][] = 
    let perturb = 
      let r = settings.random
      if r > 0.0
      then fun v -> v * (1.0 + r * (rng.NextDouble() - 0.5))
      else id
    let perturbed = pops |> Array.map (fun pop -> pop.value |> Array.map perturb)
    Array.init settings.nx (fun i -> Array.init pops.Length (fun sp -> perturbed.[sp].[i]))

  static member generate_initialconditions2d (rng:Rng.Random) (settings:Spatial_settings<'a>) (pops:Population<Species,float[][]>[]) : float[][][] = 
    let perturb = 
      let r = settings.random
      if r > 0.0
      then fun v -> v * (1.0 + r * (rng.NextDouble() - 0.5))
      else id
    let perturbed = pops |> Array.map (fun pop -> pop.value |> Array.map (Array.map perturb))
    Array.init settings.nx (fun i -> Array.init settings.nx (fun j -> Array.init pops.Length (fun sp -> perturbed.[sp].[i].[j])))

  static member perturb (rng:Rng.Random) r = if r > 0.0 then fun v -> v * (1.0 + r * (rng.NextDouble() - 0.5)) else id
  (***********************************************)
  (*** Grid initialization helper functions ***)
  (***********************************************)
  static member accumulator_1d (rng:Rng.Random) nx (initials:Initial<'s,float> seq) = 
    let spis, nsis = initials |> List.ofSeq |> List.partition (fun i -> i.spatial.IsSome)
    let v0 = nsis |> List.sumBy (fun i -> i.value)
    let x0 = Array.init nx (fun i -> (float i)/(float (nx-1)), v0) 
    match spis with
    | [] -> Array.map snd x0
    | [i] -> 
      let sp = i.spatial.Value
      match sp.core with Some core -> Array.map core.apply1d x0 | None -> x0
      |> List.foldBack (fun (point:Spatial_initial.point) x0 -> Array.map point.apply1d x0) sp.points
      |> List.foldBack (fun (rect:Spatial_initial.rectangle) x0 -> Array.map rect.apply1d x0) sp.rectangles
      |> Array.map (fun (_,value) -> Pde<'a>.perturb rng sp.random value)
    | _ -> failwith "Received more than one spatial initial condition for species _"   // TODO: Find a way to report which species causes the problem
  static member internal applyIC_rectangle2d (rect:Spatial_initial.rectangle) c0 = 
    c0 
    |> Array.map (fun c0i -> 
      c0i 
      |> Array.map (fun ((xpos,ypos),value) -> 
        if (xpos >= rect.xmin && xpos <= rect.xmax && ypos >= rect.ymin && ypos <= rect.ymax)
        then (xpos,ypos), rect.value + value
        else (xpos,ypos), value
      )
    )
  static member accumulator_2d (rng:Rng.Random) nx (initials:Initial<'s,float> seq) = 
    let spis, nsis = initials |> List.ofSeq |> List.partition (fun i -> i.spatial.IsSome)
    let v0 = nsis |> List.sumBy (fun i -> i.value)
    let x0 = Array.init nx (fun i -> Array.init nx (fun j -> ( (float i)/(float (nx-1)), (float j)/(float (nx-1))), v0))
    match spis with
    | [] -> Array.map (Array.map snd) x0
    | [i] -> 
      let sp = i.spatial.Value
      match sp.core with Some core -> Array.map (Array.map core.apply2d) x0 | None -> x0
      |> List.foldBack (fun (p:Spatial_initial.point) c0 -> Array.map (Array.map p.apply2d) c0) sp.points 
      |> List.foldBack (fun (r:Spatial_initial.rectangle) c0 -> Array.map (Array.map r.apply2d) c0) sp.rectangles
      |> Array.map (Array.map (snd >> Pde<'a>.perturb rng sp.random))
    | _ -> failwith "Received more than one spatial initial condition for species _"   // TODO: Find a way to report which species causes the problem

  (********************************************)
  (*** Simulation functions ***)
  (********************************************)
  static member inline_parameters pde = 
    let env = Parameters.to_env pde.settings.parameters
    let ratesEnv = 
      pde.rateExpressions 
      |> Key.inline_rates_env env 
      |> Map.map (fun _ -> Expression.map (Inlined.map (fun sp -> match pde.populations.tryFind_index sp with Some i -> i | None -> failwithf "Could not locate species %s" sp.name)))
    let evaluated_rates = pde.rates |> Array.map ((Rate.map2 (Key<int>.eval_massaction env) (Key<int>.eval_function env ratesEnv) ) >> (fun ra -> ra.map id (fun l -> l.key))) 
    let f = Pde<'a>.reaction_term pde.stoich pde.powers evaluated_rates
    let plotSpecies = 
      pde.sim_settings.plots
      |> List.collect (Expression.mentions >> 
        List.choose (fun k -> 
          match k with 
          | Key.Species sp -> Some sp
          | Key.Rate _  -> failwith "Not implemented yet"
          | _ -> None
        )
      )
      |> List.distinct
    let speciesToPlotIndex sp = List.findIndex (fun ps -> ps = sp) plotSpecies
    let inlineExpression = Expression.expand (Key.map speciesToPlotIndex >> Key.inline_keys env ratesEnv) >> Expression.simplify  
    let plots = pde.sim_settings.plots |> List.map inlineExpression |> Array.ofList  
    let plotlocs = plotSpecies |> List.map pde.species_index
    let solver_settings = Pde.map_settings env plotlocs pde
    f, solver_settings, plots

  static member simulate_1d_callback (cancel:bool ref) (output:Row<float[]> -> unit) pde =
    let f, settings, plots = Pde<'a>.inline_parameters pde
    let grab_data (time:float) (concs:float[][]) = 
      let rec keySolver i k = 
        match k with 
        | Inlined.Species sp -> concs.[i].[sp]
        | Inlined.Time       -> time    
      plots |> Array.map (fun pl -> Array.init settings.nx (fun i -> Expression.eval (keySolver i) pl))
    let u0 = Pde<Functional>.generate_initialconditions1d pde.rng pde.settings pde.populations.index_to_species
    let point_to_row (p:Solver1d.point) = { Row.time = p.t; Row.values = grab_data p.t p.u }
    let solution = 
      match pde.settings.boundary with 
      | Boundary.Periodic -> Solver1d.periodic_solve pde.sim_settings.initial u0 f settings
      | Boundary.ZeroFlux  -> Solver1d.neumann_solve pde.sim_settings.initial u0 f settings 1  // Use 1st order approximation (2nd order is available for 1d)
    solution
    |> Seq.iter (point_to_row >> output)

  static member simulate_2d_callback (cancel:bool ref) (output:Row<float[][]> -> unit) pde =  
    let f, settings, plots = Pde<'a>.inline_parameters pde  
    let grab_data time (concs:float[][][]) =     
      plots |> Array.map (fun pl -> Array.init settings.nx (fun i -> Array.init settings.nx (fun j -> 
        let concs_ij = concs.[i].[j]
        let rec keySolver k = 
          match k with 
          | Inlined.Species sp -> concs_ij.[sp]
          | Inlined.Time       -> time    
        Expression.eval keySolver pl))
      )
    let u0 = Pde<Functional>.generate_initialconditions2d pde.rng pde.settings pde.populations.index_to_species
    let point_to_row (p:Solver2d.point) = { Row.time = p.t; Row.values = grab_data p.t p.u }
    match pde.settings.boundary with
    | Boundary.Periodic -> Solver2d.periodic_solve pde.sim_settings.initial u0 f settings
    | Boundary.ZeroFlux -> Solver2d.neumann_solve pde.sim_settings.initial u0 f settings
    |> Seq.map point_to_row
    |> Seq.iter output
  static member simulate (callback:bool ref -> (Row<'a> -> unit) -> Pde<'a> -> unit) (pde:Pde<'a>) = 
    let result = ref []
    let output row = result := row::!result  
    callback (ref false) output pde;  
    //let times = pde.simulator.settings.times  
    let plots = pde.sim_settings.plots |> List.map Functional2.to_string_plot
    let xs = List.init pde.settings.nx (fun i -> (float i) * pde.settings.xmax / (float (pde.settings.nx-1)))
    xs, Table.from_rows_reverse plots !result

    (* Interpolate? *)
    (*if times = []
    then 
      Table.from_rows_reverse plots sim_data
    else 
      let int_data = Row.interpolate_reverse Row.interpolate_float (List.rev sim_data) times
      Table.from_rows_reverse plots int_data*)
  static member simulate_1d (pde:Pde<float []>) = Pde.simulate Pde<float []>.simulate_1d_callback pde
  static member simulate_2d (pde:Pde<float [][]>) = Pde.simulate Pde<float [][]>.simulate_2d_callback pde 