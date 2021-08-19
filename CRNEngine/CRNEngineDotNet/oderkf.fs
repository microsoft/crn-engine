[<JavaScript>]
module Microsoft.Research.CRNEngine.Oderkf
(*
open Oslo

type species = Species
type simulator = Sim.t<species>
type populations = Populations<species, float>
type lambda = Expression.lambda<int>
type event = Event<species, float>
type rate = Rate<float,lambda>
type environment = Environment.t
type settings = {
  stiff: bool;
  abstolerance: float;
  reltolerance: float;
}
//type matrix = Oslo.Matrix
//type vector = Oslo.Vector
type t = { 
  simulator: simulator;
  settings: settings;
  stoich: Matrix;
  powers: (int*float) list array;
  rateDs: rate array;
  concs: Vector;
}
type cancel = bool ref
type row = float Row
type column = float array
type output = row -> unit

type Stepper<'solver> = {
    init    : 'solver;
    current : 'solver -> 'solver;
    advance : 'solver -> 'solver;
    get_time : 'solver -> float;
    get_x : 'solver -> Vector
}

type SolverType =
    | GearBDF of Stepper<Oslo.ODE.GearBDF>
    | RK547M of Stepper<Oslo.ODE.RK547M>

(* ****************************************************************************************************** *)
(* This is a relatively old RKF45 determinisitic solver, we keep it to validate the other solvers against *)

(* Datatype for storing setup data for the ODE solver. *)
type ode_RKF54_data =
  { stoich : matrixFS
  ; powers : (int * float) list array
  ; rateDs : Rate<float,lambda> array
  ; concs : vectorFS }

(* Translate a list of sreactions (with species populations) into the corresponding matrices and vectors for the ODE solver. *)
let ode_RKF54_setup (rs:Reaction<int,float,lambda> list) (populations:('species,float) Populations) : ode_RKF54_data =
  (* Get all species from populations data structure *)
  let numReactions = List.length rs in
  let numSpecies = Populations.get_count populations in
  (* Create and populate the stoichiometry matrix *)
  let stoich = MatrixFS.create numSpecies numReactions 0.0 in
  for s in 0..(numSpecies-1) do
    List.iteri (fun ri r -> let constant = Populations.is_constant populations s in
                            stoich.[s,ri] <- Reaction.getStoich constant s r) rs done;
  (* Create and populate the powers array *)
  let powers = Array.ofList (List.map
    (fun (r: Reaction<int,float,lambda> ) -> Mset.collectm (fun (id, n) -> (id, (float) n)) r.reactants)
    rs)
  in
  (* Create and populate the rates array *)
  let rateDs = Array.ofList (List.map (fun (r:Reaction<int,float,lambda> ) -> r.rate) rs) in
  (* Create and populate the initial concentrations vector *)
  let concs = VectorFS.ofList (List.map (fun (pi:('species,float) Population) -> pi.value) (Populations.get_pop_info_list populations)) in
  (* Return the list of all species, along with two matrices and two vectors *)
  { stoich=stoich; powers=powers; rateDs=rateDs; concs=concs }

(* Compute the largest absolute value in a vector *)
let absoluteMaximum (v:vectorFS) = VectorFS.fold (fun maxval x -> max maxval (abs_float x)) 0.0 v

(* Deterministic simulation algorithm for non-stiff systems. *)
let simulateRKF54NonStiff (env:environment) (ode:t) (cancel_flag:cancel) (output:output) (reactions) =
  let settings = ode.settings in
  let simulator = ode.simulator in
  let sim_settings = simulator.settings in 
  let endtime = simulator.settings.final in
  let pops = simulator.populations in
  let printspecies = Sim.get_printspecies simulator |> Array.ofList in 
  let printinterval = Sim.get_print_interval sim_settings in
  let events = simulator.events in    
  (* Make sure we have no events *)
  if events |> List.exists (fun ev -> match ev.target with Event.OutputPoint _ -> false | _ -> true)
  then failwith "Events not supported for this deterministic simulator (use RK547M or GearBDF)";
  
  (* Translate the sreactions into the data structures needed for the ODE solver. *)
  let sreactions = List.map Lib.fst3 (Reaction.get_sim_reactions_products 1.0 pops env reactions) in
  let {ode_RKF54_data.stoich=stoich; ode_RKF54_data.powers=powers; ode_RKF54_data.rateDs=rateDs; ode_RKF54_data.concs=concentrations} = 
    ode_RKF54_setup sreactions pops
  in
  (* Set up tolerances and step sizes for the ODE solver loop *)
  let tolerance = settings.abstolerance in (* Default is set in lib.ml but can be changed in code. *)
  let h = endtime / 100.0 in
  let hmin = 1.0 / 10000.0 in
  let hmax = 100.0 in (* %%% Was originally 0.1 but this was limiting the solver so we increased it. *)
  let h = min h hmax in
  let h = max h hmin in
    (* Compute the next h value for the loop *)
  let next_h (error:float) (tend:float) (t0:float) (h:float) =
    let s = (0.5 * tolerance / error) ** 0.25 in
    let s = if (s < 0.1) then 0.1 else if (s > 4.0) then 4.0 else s in
    let h = h * s in
    let h = min h hmax in
    let h = max h hmin in
    if (h > tend - t0) then tend - t0 else h
  in
  
  let fluxes = VectorFS.create (Array.length rateDs) 1.0 in
  let rate_power x p = x ** p
  (* Function to do the matrix multiplications at each step *)
  let f ((_:float),(concs:vectorFS)) : vectorFS = (* RLP: What is the first unused argument? *)
    for i = 0 to ((Array.length rateDs) - 1) do
      fluxes.[i] <- match rateDs.[i] with
                  | Rate.MassAction r -> List.fold (fun t (j,p) -> t * rate_power concs.[j] p) r powers.[i]
                  | Rate.Function f -> f (fun id -> concs.[id])
                     
      done;
    (* Computation pre-functional rates
    let fluxes = Vector.copy rates in
    for i = 0 to (rates.Length - 1) do 
       fluxes.[i] <- List.fold (fun f (j,p) -> f * (concs.[j] ** p)) fluxes.[i] powers.[i]
    done; *)
        MatrixFS.Multi(stoich,fluxes)
        //stoich * fluxes
  in
  (* Main solver function. *)
  let mutable first_loop = false in
  let mutable currenttime = simulator.currenttime in
  let mutable nextprinttime = simulator.nextprinttime in
  let concs = ref (concentrations) in
  let mutable h = h in
  let mutable stepsdone = simulator.stepsdone in

  let mutable keepIterating = true in

  while keepIterating do
    let local_concs = !concs in

    (* Compute the error etc for the next potential step *)

    //
    let ( *< ) (f:float) (v:vectorFS) : vectorFS = VectorFS.Multiply(f,v) in
    let ( *> ) (v:vectorFS) (f:float) : vectorFS = VectorFS.Multiply(v,f) in
    let ( +^ ) (v1: vectorFS) (v2: vectorFS) : vectorFS = VectorFS.Add(v1,v2) in
    let ( -^ ) (v1: vectorFS) (v2: vectorFS) : vectorFS = VectorFS.Subtract(v1,v2) in

    let k1 = h *< f(currenttime, local_concs) in
    let k2 = h *< f((currenttime + 0.25 * h), (local_concs +^ 0.25 *< k1)) in
    let k3 = h *< f((currenttime + 3.0 * h / 8.0), (local_concs +^ 3.0 *< k1 *> (1.0 / 32.0) +^ 9.0 *< k2 *> (1.0 / 32.0))) in
    let k4 = h *< f((currenttime + 12.0 * h / 13.0), (local_concs +^ 1932.0 *< k1 *> (1.0 / 2197.0) -^ 7200.0 *< k2 *> (1.0 / 2197.0) +^ 7296.0 *< k3 *> (1.0 / 2197.0))) in
    let k5 = h *< f((currenttime + h), (local_concs +^ 439.0 *< k1 *> (1.0 / 216.0) -^ 8.0 *< k2 +^ 3680.0 *< k3 *> (1.0 / 513.0) -^ 845.0 *< k4 *> (1.0 / 4104.0))) in
    let k6 = h *< f((currenttime + 0.5 * h), (local_concs -^ 8.0 *< k1 *> (1.0 / 27.0) +^ 2.0 *< k2 -^ 3544.0 *< k3 *> (1.0 / 2565.0) +^ 1859.0 *< k4 *> (1.0 / 4104.0) -^ 11.0 *< k5 *> (1.0 / 40.0))) in
    let e1 = (k1 *> (1.0 / 360.0) -^ 128.0 *< k3 *> (1.0 / 4275.0) -^ 2197.0 *< k4 *> (1.0 / 75240.0) +^ k5 *> (1.0 / 50.0) +^ 2.0 *< k6 *> (1.0 / 55.0)) *> (1.0 / h) in

    
    (*let k1 = h * f(currenttime, local_concs) in
    let k2 = h * f((currenttime + 0.25 * h), (local_concs + 0.25 * k1)) in
    let k3 = h * f((currenttime + 3.0 * h / 8.0), (local_concs + 3.0 * k1 * (1.0 / 32.0) + 9.0 * k2 * (1.0 / 32.0))) in
    let k4 = h * f((currenttime + 12.0 * h / 13.0), (local_concs + 1932.0 * k1 * (1.0 / 2197.0) - 7200.0 * k2 * (1.0 / 2197.0) + 7296.0 * k3 * (1.0 / 2197.0))) in
    let k5 = h * f((currenttime + h), (local_concs + 439.0 * k1 * (1.0 / 216.0) - 8.0 * k2 + 3680.0 * k3 * (1.0 / 513.0) - 845.0 * k4 * (1.0 / 4104.0))) in
    let k6 = h * f((currenttime + 0.5 * h), (local_concs - 8.0 * k1 * (1.0 / 27.0) + 2.0 * k2 - 3544.0 * k3 * (1.0 / 2565.0) + 1859.0 * k4 * (1.0 / 4104.0) - 11.0 * k5 * (1.0 / 40.0))) in
    let e1 = (k1 * (1.0 / 360.0) - 128.0 * k3 * (1.0 / 4275.0) - 2197.0 * k4 * (1.0 / 75240.0) + k5 * (1.0 / 50.0) + 2.0 * k6 * (1.0 / 55.0)) * (1.0 / h) in*)

    let error = absoluteMaximum e1 in
    (* Compute the next value of h *)
    let next_h_value = next_h error endtime currenttime h in
    (* Loop with the new value of h if the error is too high... *)
    if (error >= tolerance) then
        h <- next_h_value
    else
        (* Calculate the time to the next step. *)
        let next_currenttime = currenttime + h in
        let next_nextprinttime =
            if (Sim.shouldPrint endtime next_currenttime nextprinttime None first_loop currenttime) then
                let new_data = Array.map
                                    (fun p -> Expression.eval (fun x -> (!concs).[x]) p)
                                    printspecies
                in
                output {time=currenttime;values=new_data};
                next_currenttime + printinterval
            else
                nextprinttime
        in
        (* Stop if we reach the endpoint or the "cancel flag" is triggered. *)
        if (Sim.shouldStop endtime next_currenttime stepsdone cancel_flag None) then
            keepIterating <- false

        else
            (* Modify the concentrations *)

            (first_loop <- false;
            currenttime <- next_currenttime;
            nextprinttime <- next_nextprinttime;

            concs := local_concs +^ 25.0 *< k1 *> (1.0 / 216.0) +^ 1408.0 *< k3 *> (1.0 / 2565.0) +^ 2197.0 *< k4 *> (1.0 / 4104.0) -^ 0.2 *< k5;
            //concs := local_concs + 25.0 * k1 * (1.0 / 216.0) + 1408.0 * k3 * (1.0 / 2565.0) + 2197.0 * k4 * (1.0 / 4104.0) - 0.2 * k5;
            
            h <- next_h_value;
            stepsdone <- stepsdone + 1)
            
  done;

  (* Convert populations back to the correct format. *)
  Array.iteri (fun id v -> Populations.set_population simulator.populations id v) ((!concs).ToArray());
  (* Update the simulator data structure. *)
  (* RLP: Why do we do this? *)
  let simulator = 
  { simulator with currenttime = currenttime; nextprinttime = currenttime + printinterval; stepsdone = stepsdone }
  in {ode with simulator = simulator}


  *)