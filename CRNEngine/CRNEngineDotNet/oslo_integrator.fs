[<JavaScript>]
module Microsoft.Research.CRNEngine.Oslo_integrator

open Oslo
open Microsoft.Research.CRNEngine

type key = Key<int>
type simulator = Simulation<Functional>
type lambda = Lambda<Inlined<int>>
type event = Event<Species, float, float>
type rate = Rate<float,lambda>
type value = Expression.t<string>
type cancel = bool ref
//type row = Row<'a>
//type table = Table<'a>
//type output = row -> unit

type t<'a> = {
  name: string
  simulator: simulator
  //plots: Expression.t<Inlined<int>> array
  grab_data: float -> Vector -> 'a[]
  interpolator: float -> float -> float -> 'a -> 'a -> 'a
  settings: Deterministic_settings
  f: (float -> Vector -> Vector)
  concs: Vector
}
let empty () : t<'a> = {
  name = "None"
  simulator = Simulation.empty ()
  settings = Deterministic_settings.defaults
  f = fun _ x -> Vector.zeros 1
  grab_data = fun _ _ -> [||]
  interpolator = fun _ _ _ (x:'a) _ -> x
  concs = Vector.zeros 1
}

let create name simulator grab_data interpolator settings concs f = 
  { name = name
    simulator = simulator
    settings = settings
    grab_data = grab_data
    interpolator = interpolator
    concs = concs
    f = f }

let skin_plottable = function
  | Expression.Key (k:int) -> k
  | _ -> failwith "Not a single species"
let locate_plottable (pops:Populations<Species,float>) p = skin_plottable (pops.to_key_plottables p)

let printspecies (pops:Populations<Species,float>) ps =
  //let pops = Term.get_populations t //NB: fix?
  //let ps = Term.compute_all_printspecies t  //NB: fix?
  List.map pops.to_key_plottables ps
  |> List.toArray

(* The following type and function are if we want to store the stoich and powers as MSU solver-compatible matrices *)
(* Datatype for storing setup data for the ODE solver. *)
(* Get the population of a particular species from the concentrations vector *)
let get_conc (concs:Vector) (s:int) : float = concs.[s]

let applyPertubation target (concs: Vector) (pops:Populations<Species,float>) = 
  match target with
  | Target.Species ev_pop ->
    let loc_map =
      ev_pop.get_pop_info_list
      |> List.map (fun pi -> 
                      ( match pops.tryFind_index pi.species with
                      | Some i -> i
                      | None   -> failwith ("Unknown species in event"))      // Would be better to also return the string of the species, but need a namer.
                      , pi.value)
      |> Map.ofList
    let vals = Array.mapi (fun i x -> match Map.tryFind i loc_map with Some perturbation -> x + perturbation | None -> x) (Vector.toArray concs)
    Vector.ofArray vals
  //| Event.Parameter _ -> concs
  | Target.OutputPoint -> concs

type Stepper<'solver> = {
    init    : 'solver
    current : 'solver -> 'solver
    advance : 'solver -> 'solver
    refine : ('solver -> 'solver -> 'solver list) option
    get_time : 'solver -> float
    get_x : 'solver -> Vector
}

let EuclideanNorm vec =
  Seq.sumBy
    (fun x -> x * x)
    vec
  |> System.Math.Sqrt

let Lerp t (t0 : float) (v0 : Vector) (t1 : float) (v1 : Vector) =
  //(v0 * (t1 - t) + v1 * (t - t0)) / (t1 - t0)
  (v0 *> (t1 - t) +^ v1 *> (t - t0)) *> (1.0 / (t1 - t0))

let rec stepper stepper_parameter state (first_loop:bool) (currenttime:float) (printtime:float) (eventtime:float) (concs:Vector) (stepsdone:int) settings env grab_data printinterval cancel_flag output =
  (* Move the enumerator forward and ensure that a step has been taken *)
  (*let moved = 
    try spEnum.MoveNext()
    with :? InvalidOperationException as ex -> Errors.ode_solver_error "Solver failed. Try altering tolerances."
  if not moved then
    Errors.ode_solver_error "Solver did not take a step. Try altering tolerances.";*)
  let next_state = stepper_parameter.advance state
  let x = stepper_parameter.get_x next_state
  let t = stepper_parameter.get_time next_state
  if System.Double.IsNaN(x.[0]) then
    Errors.ode_solver_error "Solver produced bad solution. Try altering tolerances.";
  (* If we reach the endpoint, stop and calculate the next concentration. *)
  let next_currenttime, next_concs =
    if Simulator.shouldStop eventtime t stepsdone cancel_flag None then
      //eventtime, Vector.Lerp(eventtime, currenttime, concs, t, x)
      eventtime, (Lerp eventtime currenttime concs t x)
    else t, x

  if currenttime = eventtime then
    let rr = 0.99999999
    let output_time = rr * eventtime + (1.0 - rr) * (currenttime - printinterval)
    output output_time settings env (grab_data concs)
  let next_printtime =
    (* Determine whether the current solution point needs to be written to the simulation point store *)
    if Simulator.shouldPrint eventtime next_currenttime printtime None first_loop currenttime then
      output currenttime settings env (grab_data concs)
      min (next_currenttime + printinterval) eventtime
    else printtime

  (* Stop if the "cancel flag" is triggered. *)
  if (!cancel_flag) || (currenttime = eventtime) then 
    next_currenttime, concs, stepsdone
  else
    stepper stepper_parameter next_state false next_currenttime next_printtime eventtime next_concs (stepsdone+1) settings env grab_data printinterval cancel_flag output

let internal runSolver stepper currenttime nextprinttime eventtime concs stepsdone grab_data printinterval cancel_flag (output:Row<'a>->unit) =
  (* Main solver function. *)
  let mutable first_loop = true
  let mutable currenttime = currenttime
  let mutable printtime = nextprinttime
  let concs = ref (concs)
  let mutable stepsdone = stepsdone
  let mutable prev_state = stepper.init
  let mutable next_state = stepper.init
  let mutable keepIterating = true
  while keepIterating do
    (* Move the enumerator forward and ensure that a step has been taken *)
    (*let moved = 
      try spEnum.MoveNext()
      with :? InvalidOperationException as ex -> Errors.ode_solver_error "Solver failed. Try altering tolerances."
    if not moved then
      Errors.ode_solver_error "Solver did not take a step. Try altering tolerances.";*)
    prev_state <- next_state
    next_state <- stepper.advance prev_state;
    let interpolants = 
      match stepper.refine with
      | Some interpolater -> interpolater prev_state next_state @ [next_state]
      | None -> [next_state]

    interpolants
    |> List.fold (fun _ state ->
      if not keepIterating
      then ()
      else   
        let x = stepper.get_x state
        let t = stepper.get_time state
        if System.Double.IsNaN(x.[0]) then
          Errors.ode_solver_error "Solver produced bad solution. Try altering tolerances."
        (* If we reach the endpoint, stop and calculate the next concentration. *)
        let next_currenttime, next_concs =
          if (Simulator.shouldStop eventtime t stepsdone cancel_flag None) then
            //eventtime, Vector.Lerp(eventtime, currenttime, concs, t, x)
            eventtime, (Lerp eventtime currenttime !concs t x)
          else t, x
            
        if (currenttime = eventtime) then
          let rr = 0.99999999
          let output_time = rr * eventtime + (1.0 - rr) * (currenttime - printinterval)
          output {time=output_time;values=(grab_data currenttime !concs)}

        let next_printtime =
          (* Determine whether the current solution point needs to be written to the simulation point store *)
          if (Simulator.shouldPrint eventtime next_currenttime printtime None first_loop currenttime) then
            output {time=currenttime;values=(grab_data currenttime !concs)}
            min (next_currenttime + printinterval) eventtime
          else printtime
            
        if ((!cancel_flag) || (currenttime = eventtime)) then
          keepIterating <- false
          currenttime <- next_currenttime
        else
          (* Modify the concentrations *)
          first_loop <- false
          currenttime <- next_currenttime
          printtime <- next_printtime
          concs := next_concs
          stepsdone <- stepsdone + 1
      ) ()

  (currenttime, !concs, stepsdone)

let simulate_callback (cancel_flag:cancel) (output:Row<'a>->unit) (ode:t<'a>) =
  let settings = ode.settings
  let simulator = ode.simulator
  let sim_settings = simulator.settings
  let pops = simulator.populations
  let printinterval = sim_settings.get_print_interval()
  let allevents = simulator.events
  let event_matters (ev:event) = // RLP: These things are currently ensured by the simulator
    ev.time >= simulator.currenttime &&
    match ev.target with
    | Target.Species pop -> Array.exists (fun p -> p <> 0.0) pop.get_pop_array
    | _ -> true
  let events = List.filter event_matters allevents  
  let concs = ode.concs  
  let f = ode.f

  let simulate_event_worth (currenttime,nextprinttime, concs, stepsdone) (ev:event) =
    let eventtime = ev.time
    let duration = eventtime - currenttime
    (* Actually run the simulator loop *)
    let (timeaftersimulation, concsaftersimilation, _) =
      (* Determine whether the first step will produce non-zero derivatives 
          i.e. are we at an equilibrium? This is currently not identifed explicitly by the solver, which can lead to an unresponsive UI *)
      let f0 = f currenttime concs
      //if (f0.EuclideanNorm <= 0.0) then 
      if (EuclideanNorm (Vector.toArray f0) <= 0.0) then 
        let next_currenttime = eventtime
        output {time=currenttime;values=ode.grab_data currenttime concs}
        output {time=next_currenttime;values=ode.grab_data next_currenttime concs}
        (next_currenttime, concs, simulator.stepsdone + 1)
      else  (* If we observe non-zero derivatives, proceed *)
        if settings.stiff then
          let options = { GearBDF.defaults() with AbsoluteTolerance = settings.abstolerance; RelativeTolerance = settings.reltolerance }
          let gear =
            { init = GearBDF.init currenttime concs f options
            ; current = (fun g -> g)
            ; advance = GearBDF.advance
            ; refine = None
            ; get_time = (fun g -> g.t)
            ; get_x = (fun g -> g.x) } 
                
          runSolver gear currenttime nextprinttime eventtime concs simulator.stepsdone ode.grab_data printinterval cancel_flag output
        else
          let options = { RK547M.defaults() with AbsoluteTolerance = settings.abstolerance; RelativeTolerance = settings.reltolerance; MinStep = 1e-4 }
          let rk =
            { init = RK547M.init currenttime concs f options
            ; current = (fun (g: RK547M.state) -> g)
            ; advance = RK547M.advance
            ; refine = Some RK547M.add_interpolate
            ; get_time = (fun g -> g.t)
            ; get_x = (fun g -> g.x) } 
                
          runSolver rk currenttime nextprinttime eventtime concs simulator.stepsdone ode.grab_data printinterval cancel_flag output

    let concsafterpertubation = pops |> applyPertubation ev.target concsaftersimilation
    (* Convert populations back to the correct format. *)
    //Array.iteri (Populations.set_population simulator.populations) (Vector.toArray concsafterpertubation)
    (* Update the simulator data structure. *)
    (*let simulator = { simulator with currenttime = timeaftersimulation; nextprinttime = timeaftersimulation + printinterval; stepsdone = stepsdone }
    let ode = {ode with simulator = simulator}*)
    (timeaftersimulation, timeaftersimulation + printinterval, concsafterpertubation, stepsdone)
  
  (* Loop over the events in order of time *)
  let (currenttime,nextprinttime, concs, stepsdone) =
    List.fold simulate_event_worth (simulator.currenttime, simulator.nextprinttime, concs, simulator.stepsdone) events
  (* Update the simulator data structure. *)
  let simulator = { simulator with currenttime = currenttime; nextprinttime = nextprinttime; stepsdone = stepsdone }
  {ode with simulator = simulator; concs = concs}

let simulate (ode:t<'a>) =
  let cancel = ref false
  let result = ref []
  let output (row:Row<'a>) = result := row::!result
  let final_sim = simulate_callback cancel output ode
  let times:float list = ode.simulator.settings.times
  let plots:string list = List.map Functional2.to_string_plot ode.simulator.settings.plots
  let sim_data:Row<'a> list = !result //NB rows are in reverse order
  if List.isEmpty times
  then final_sim, Table.from_rows_reverse plots sim_data
  else 
      let int_data:Row<'a> list = Row.interpolate_reverse ode.interpolator (List.rev sim_data) times
      final_sim, Table.from_rows_reverse plots int_data

