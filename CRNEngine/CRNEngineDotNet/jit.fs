[<JavaScript>] 
module Microsoft.Research.CRNEngine.Jit


type settings     = Stochastic_settings
type lambda       = Lambda<int> 

//type initial<'s> when 's:equality = Initial<'s, Expression.t<string>>

type reaction<'s> when 's : equality = Reaction<'s, Expression.t<string>, Expression.t< Key<'s>>>
type JitCalculus<'s> when 's : equality = 's list                // list of known species
                                           * 's                  // a (single) new species
                                           -> reaction<'s> list  // new reactions, made available by the interaction between old and new species
                                                                 // (new species always occurs in the new reactions' reactants)

type SID          = int // species ID
type RID          = int // reaction ID
type multiplicity = int
type inlinedExp<'s> when 's : equality = Expression.t<Inlined<'s>>

// names for visualization purposes
let SidPrefix = "sp_" 
let RidPrefix = "rx_" 

// JIT wrapper for polymorphic species
type JitSpecies<'s> when 's : equality =   
  { id                  : SID
  ; payload             : 's                     // the wrapped species (e.g. CRN species or DSD strand)
  ; mutable population  : float
  ; mutable isExplored  : bool                   // has the calculus been applied to this species?
  ; isConstant          : bool
  ; mutable deps        : Collections.Set<RID> } // reactions with this species as a reactant

// JIT wrapper for polymorphic Reaction
type JitReaction<'s, 'v, 'e> when 's : equality // 's is a species
                              and 'v : equality // 'v is a mass action rate
                              and 'e : equality // 'e is a functional rate
 = 
  { id            : RID
  ; payload       : Reaction<'s, 'v, 'e>
  ; reacts        : (SID * multiplicity) list
  ; prods         : (SID * multiplicity) list
  ; mutable prop  : float }                     // reactions sharing this 
  
// bookkeeping structures for species and reactions 
type dictionary<'t1, 't2> = System.Collections.Generic.Dictionary<'t1, 't2>

// types related to output
type row = float Row
type output = row -> unit
type newplottable = {
  name:string;
  structural:string option;
  svg:string option;
}
type outputplottable = newplottable -> unit

// JIT simulation data
type t<'s> when 's : equality =
  { simulation            : Simulation_settings<Expression.t<Inlined<'s>>>
  ; stochastic            : Stochastic_settings
  ; rates                 : Map<string, Expression.t<Inlined<'s>>>
  ; parameters            : Parameter list
  //
  ; random                : Rng.Random
  ; sidDict               : dictionary<'s,  SID>
  ; spDict                : dictionary<SID, JitSpecies<'s>>
  ; rxDict                : dictionary<RID, JitReaction<'s, float, inlinedExp<'s>>>
  ; plotDict              : dictionary<'s,  SID list> (* maps a plot pattern to possibly one or more species (e.g. "<_ y z>"*)
  ; mustPlotAll           : bool                      (* if true, plots the population of all species *)
  ; printSpecies          : 's -> newplottable
  ; equalsSpecies         : 's -> 's -> bool
  ; matchesSpecies        : 's -> 's -> bool
  ; mutable now           : float (* current time *)
  ; mutable stepsDone     : int
  ; mutable nextPrintTime : float 
  ; mutable events        : Event<'s,float,float> list
  ; mutable totalProp     : float
  ; mutable nextSID       : int (* next species ID *)
  ; mutable nextRID       : int (* next reaction ID *) 
  ; mutable dataPoints    : Row<float> list} (* plotted data points. The number of columns in each rows is variable,
                                                  depending on the number of new species created during the simulation. 
                                                  For efficiency reasons, data points are stored in reverse order. *)

type cancel = bool ref
let createRow t v : row = { time   = t; 
                            values = v }
let empty printSpecies equalsSpecies matchesSpecies : t<'s> when 's : equality =
  let comparer = { new System.Collections.Generic.IEqualityComparer<'s>
                        with member this.Equals(x, y)      = equalsSpecies x y
                             member this.GetHashCode(that) = that.GetHashCode() }

                        //IEqualityComparer(equalsSpecies) 
  let emptySim = Simulation.empty ()
  { simulation      = Simulation_settings.defaults
  ; stochastic      = Stochastic_settings.defaults
  ; rates           = Map.empty
  ; parameters      = []
  ; random          = new Rng.Random()
  ; spDict          = new dictionary<SID, JitSpecies<'s>>()
  ; sidDict         = new dictionary<'s, SID>(comparer)
  ; rxDict          = new dictionary<RID, JitReaction<'s, float, inlinedExp<'s>>>()
  ; plotDict        = new dictionary<'s, SID list>(comparer)
  ; mustPlotAll     = true
  ; printSpecies    = printSpecies 
  ; equalsSpecies   = equalsSpecies
  ; matchesSpecies  = matchesSpecies
  ; now             = emptySim.currenttime
  ; stepsDone       = emptySim.stepsdone
  ; nextPrintTime   = emptySim.nextprinttime
  ; events          = []
  ; totalProp       = 0.0
  ; nextSID         = 0
  ; nextRID         = 0
  ; dataPoints      = [] }

let newSID jit = 
  let sid = jit.nextSID
  jit.nextSID <- sid + 1 // update the ID generator
  sid
        
let newRID jit = 
  let rid = jit.nextRID
  jit.nextRID <- rid + 1 // update the ID generator
  rid

(* Compute the time until the next reaction - assume an exponential distribution. *)
let calculate_time_to_reaction_exponential (prop:float) (random:Rng.Random) = log(1.0 / random.NextDouble()) / prop
  
let computeTotalProp jit = List.fold 
                            (fun acc x -> acc + x.prop) 
                            0.0 
                            (jit.rxDict |> Seq.map (fun kv -> kv.Value) |> List.ofSeq)

(***********************************************************************)
(* %%%%%% Gillespie stochastic simulation algorithm - Direct method.   *)
(***********************************************************************)

(* Randomly pick the next reaction to execute, with probability proportional to the propensities. *)
let pickNextReaction jit = 
  let totalProp = jit.totalProp
  let rIndex    = jit.random.NextDouble() * totalProp
  let rs        = jit.rxDict

  //Hashtable.
  let rec pick_reaction (acc:float) (index:int) =
    let choice = rs.[index]
    //let debugd = choice.payload.string (fun x ->  let z = jit.printSpecies x
    //                                              match z.structural with
    //                                              | Some s -> s
    //                                              | None -> z.name) (fun _ -> "") (fun _ -> "")
    let currProp = acc + choice.prop
    if  currProp > rIndex
      then Some choice
      else 
        if (index+1) >= rs.Count
          then (* Detect whether this is a propensity problem or not *)
            if currProp = 0.0
              then None
              elif currProp <> totalProp
                then failwith "Incorrect total propensity used." 
                else failwith "Index outside range"
          else pick_reaction currProp (index+1)
  in    
  pick_reaction 0.0 0

let calculate_reaction_prop (jit : t<'s>) 
                            (r   : JitReaction<'s, float, Expression.t<Inlined<'s>>>) : float =
  let getpop sid = (jit.spDict.[sid]).population
  match r.payload.rate with
    // mass action case
    | Rate.MassAction rate -> 
       match r.reacts with
        | []       -> rate
        | [(a, 1)] -> rate * (getpop a)
        (* two members of the same species *)
        | [(a, 2)] ->  let pop = getpop a in
                       rate * pop * (pop - 1.0) * 0.5
        (* three members of the same species *)
        | [(a, 3)] ->  let pop = getpop a in
                       rate * pop * (pop - 1.0) * (pop - 2.0) / 6.0
        (* simple case *)
        | [(a, 1); (b, 1)] -> rate * (getpop a) * (getpop b)
        (* general case: use binomial coefficients *)
        | aa      -> aa |> Lib.fold_left
                            (fun acc entry -> 
                              let binomial = Lib.binom ((int32) (fst entry |> getpop )) (snd entry)
                              in acc * (float) binomial) 
                            rate
                  
     (* functional case *)
     | Rate.Function (f) -> 
         let scale              = jit.stochastic.scale 
         let get_unscaled_pop a = (getpop a) / scale in
         let envUnscaled (key:Inlined<'s>) = 
          match key with 
          | Inlined.Species   s -> get_unscaled_pop jit.sidDict.[s]
          | Inlined.Time        -> jit.now
          //| Key.IRate r      -> ratesEnv.[r]
       
         let evalUnscaled       = Expression.eval envUnscaled
         (evalUnscaled f) * scale

(* Update propensities for given reactions as a side effect, for the Direct method. Return the new total propensity. *)
let update_props_direct jit (is : RID list) : float =
  (* Start by calculating the sum of the deltas, before adding to the total propensity, for numerical stability. *)
  //  let scale     = (!jit).settings.scale  //TODO: check why this is unused
  // FP: I had to change this from a Lib.fold_left to a List.iter with a mutable, because of https://github.com/intellifactory/websharper/issues/695 (this can be reverted once the bug is fixed).
  let mutable delta = 0.
  // update each reaction propensity, and compute the propensity delta at the same time
  List.iter (
    fun i -> 
      let r = jit.rxDict.[i]
      let newProp = calculate_reaction_prop jit r
      delta <- delta + (newProp - r.prop)
      r.prop <- newProp 
    ) is
  
  let newTotalProp = jit.totalProp + delta
  if  newTotalProp < 0.0 
    then 0.0 
    else newTotalProp


(* Execute the given reaction by updating the populations. *)
let executeReaction (jit : t<'s>) (r : JitReaction<'s, float, Expression.t<Inlined<'s>>>) =
  // remove reactants
  let rs1 = 
    List.choose (fun (sid, mul) -> 
      let sp = jit.spDict.[sid]
      if sp.isConstant 
        then None
        else
          sp.population <- sp.population - (float) mul
          Some sp.deps
          ) r.reacts

  
  // add products
  let rs2 = 
    List.choose (fun (sid, mul) -> 
      let sp = jit.spDict.[sid]
      if sp.isConstant 
        then None
        else
          sp.population <- sp.population + (float) mul
          Some sp.deps
          ) r.prods

  
  // collect impacted reaction propensities
  let impactedRs = rs1 @ rs2 |> Set.unionMany |> Set.toList
  
  (* Compute the new total propensity - side effect is to update propensities of reactions as necessary. *)
  let newTotalProp = update_props_direct jit impactedRs

  // store new total propensity
  jit.totalProp <- newTotalProp

let evaluatePlots (jit:t<'s>) =
  if jit.mustPlotAll
    then jit.spDict.Values 
          // (FP): do not output unexplored species, because the outputplottable callback has not reported them yet.
          |> Seq.where (fun x -> x.isExplored)
          |> Seq.map (fun x -> x.population)
          |> Seq.toArray
    else 
      (* in ClassicDSD, a plot might match more than one species. 
         For example, plot "<_ x y>" matches both "<a x y>" and "<b x y>".
         Therefore, "<_ x y>" is plotted as the sum of "<a x y>" and "<b x y>". *)
      let speciesPop (plot:'s) = 
        let matchingSpecies = jit.plotDict.[plot]
        List.fold (fun acc s -> acc + jit.spDict.[s].population) 0.0 matchingSpecies
      let keySolver k   = match k with
                          | Inlined.Time       -> jit.now
                          | Inlined.Species sp -> speciesPop sp
                          //| Inlined.IRate r     -> ratesEnv.[r]
      jit.simulation.plots 
          |> List.map (Expression.eval keySolver)
          |> List.toArray
  



(* Inner loop for testing if the next step surpasses any events, and processes them in case. *)
let rec enactEvents lu1 (output:output) (jit: t<'s>) = 
  // get elapsed time
  let dt  = lu1 / jit.totalProp

  // handle next event
  match jit.events with  
  | [] -> dt
  | (next_event: Event<'s, float, float>) :: events ->
    if next_event.time >= jit.now + dt
      then dt // events are sorted by time, so there is no need to checked 'events'
      else
        jit.events <- events

        (* Output the point at jit.current time _and_ infinitesimally close to the actual event *)
        // let ratesEnv    = computeRatesEnv jit
        let new_data    = evaluatePlots jit
        let rr          = 0.99999999 
        let output_time = rr * next_event.time + (1.0 - rr) * jit.now
        
        // output data points before and after the event
        let dataPointNow   = createRow jit.now new_data
        let dataPointEvent = createRow output_time new_data
        output dataPointNow
        output dataPointEvent
        jit.dataPoints <- dataPointEvent :: dataPointNow :: jit.dataPoints

        (* Run simulation until the event, and keep track of how much of the random number has been used up. *)
        let dt1      = next_event.time - jit.now
        let old_prop = jit.totalProp
        
        (* Enact the event *)
        match next_event.target with
          | Target.Species (ev_pop: Populations<'s,float>) ->
              for pi in ev_pop.index_to_species
                do 
                  let sid = jit.sidDict.[pi.species]
                  let entry = jit.spDict.[sid]
                  jit.spDict.[sid] <- {entry with population = entry.population + pi.value}
                done
          | Target.OutputPoint -> ()
        
        (* Compute the new total propensity - side effect is to update propensities of reactions as necessary. *)
        // TODO: update prop?

        (* Run for the remaining portion of the random number *)
        let new_lu1          = lu1 - dt1 * old_prop
        // TODO: uncomment?
        //let new_current_time = next_event.time 
        //jit := { !jit with total_propensity = totalprop; 
        //simulator = {sim jit with currenttime = new_current_time}} 
        enactEvents 
          new_lu1 
          output
          jit

let getFinalStep jit = 
  match jit.stochastic.steps with 
      None    -> None 
    | Some n  -> Some <| jit.stepsDone + n

// is it time to print plottables?
let isPrintTime jit next_currenttime first_loop = 
  Simulator.shouldPrint jit.simulation.final 
                         next_currenttime 
                         jit.nextPrintTime
                         (getFinalStep jit)
                         first_loop 
                         jit.now

// is the simulation time over?
let isStopTime jit cancel = 
  Simulator.shouldStop 
          jit.simulation.final 
          jit.now
          jit.stepsDone 
          cancel 
          (getFinalStep jit)


(* Compute the time until the next reaction - assume an exponential distribution. *)
let timeToNextReaction jit = 
    let u = jit.random.NextDouble()
    // Treat a 0.0 as if it were a 1.0
    in  if u > 0.0 
          then log(1.0 / u) // TODO shouldn't also divide by total_propensity?
          else 0.0

let init (jit : t<'s>) : Unit = 
  (* Raise an exception if the populations aren't integer (or constant) *)
  let allIntegerOrConstant = jit.spDict.Values 
                             |> Seq.toList 
                             |> List.fold (fun s p -> 
                                   let pval = p.population in
                                   ((round pval) - pval = 0.0 || p.isConstant) && s
                                   ) true 
  if not allIntegerOrConstant
    then failwith "Can't use non-integer initial conditions without specifying the species as constant"
    else ();
  
  // sort events by time
  let sortedEvs = jit.events |> List.sortBy (fun e -> e.time) 
  jit.events <- sortedEvs

/// Updates jit.plotDict for any new species that matches a plot pattern (e.g. a new species "<a b>" might match the pattern "<_ b>" in some plot). Also reports any new plottable to a callback.
let extendPlotPatterns (jit:t<'s>) (newSpecies : 's list)  =
  jit.simulation.plots 
    |> List.iter (fun p ->              // for each plot
        Expression.mentions p
          |> List.iter (fun plotKey ->  // for each key in the plot
            match plotKey with
            | Inlined.Species plotPattern ->
              newSpecies
                |> Seq.iter (fun sp ->  // for each new species
                    if jit.matchesSpecies plotPattern sp
                      then 
                        // if a species matches a plot pattern, add it to the list of plot pattern matches
                        let previousMatches = jit.plotDict.[plotPattern]
                        let sid = jit.sidDict.[sp]
                        jit.plotDict.[plotPattern] <- previousMatches@[sid]  // TODO: optimize with rev?
                      else ())
            | Inlined.Time -> ()
            ))

let scaleReaction (jit:t<'s>) (r:Reaction<'s, float, 'e>) = 
  let scale_float (scale:float) (power:int) (f:float) = f * (scale ** (1.0 - (float) power))
  r.scale scale_float jit.stochastic.scale id //TODO: does not scale functional rates. 

(************ JIT creation **********************)
let create
           (simulation      : Simulation_settings<Expression.t<Inlined<'s>>>)
           (stochastic      : Stochastic_settings)
           (rates           : Map<string, Expression.t<Inlined<'s>>>)
           (parameters      : Parameter list)
           (explored        : 's list)
           (initials        : Initial<'s, Expression.t<string>> list) 
           (reactions       : Reaction<'s, float, Expression.t<Inlined<'s>>> list ) 
           (printSpecies    : 's -> newplottable ) 
           (equalsSpecies   : 's -> 's -> bool ) 
           (matchesSpecies  : 's -> 's -> bool ) 
           =
  
  // translate initials into populations and events
  let env  = Parameters.to_env parameters 
  let eval = Expression.eval (Environment.find env) 
  let pops, evs = 
    let (p:Populations<'s,float>), e = 
      Initial<'s,Value>.to_initialpops_events env simulation.initial initials
    if stochastic.scale <> 1.0
      then Populations<'s,float>.scale p stochastic.scale, e
      else p, e
  
  
  // create jit instance
  let jit = { empty printSpecies equalsSpecies matchesSpecies  
                with simulation=simulation; stochastic=stochastic; rates=rates; parameters=parameters
                     events      = evs
                     // plot all species if no plots are specified
                     mustPlotAll = simulation.plots.IsEmpty }
  // set the random seed, if required
  let jit = match simulation.seed with None -> jit | Some s -> { jit with random = Rng.Random s }

  // we assume that all reactant species have been explored already
  let exploredSpecies = Set.ofList explored

  // convert initials to JIT species
  let makeJitSpecies  (i:Initial<'s, Expression.t<string>>) = 
    let sid = newSID jit
    
    // find population count in the populations
    let spIndex  = pops.find_index i.species
    let popCount = pops.get_population spIndex

    // return the JitSpecies
    { id          = sid
    ; payload     = i.species
    ; population  = popCount
    ; isExplored  = Set.exists (jit.equalsSpecies i.species) exploredSpecies
    ; isConstant  = i.constant
    ; deps        = Set.empty   }   (* reaction dependencies are added below, 
                                       when reactions are converted           *)
  
  // populate spDict
  initials 
    |> List.filter (fun i -> match i.time with Some t -> eval t = 0.0 | None -> true)
    |> List.iter (makeJitSpecies >> (fun sp -> jit.spDict.Add (sp.id, sp))) 
  
  // populate sidDict
  jit.spDict 
    |> Seq.map  (fun kv -> kv.Value) 
    |> Seq.iter (fun sp -> jit.sidDict.Add(sp.payload, sp.id))

  // initialize and populate plotDict
  jit.simulation.plots 
    |> List.iter (fun plot -> 
      Expression.mentions plot
        |> List.iter (fun k -> 
          match k with  
          | Inlined.Species sp -> jit.plotDict.Add(sp, [])
          | Inlined.Time       -> ()))
  extendPlotPatterns jit (jit.sidDict.Keys |> List.ofSeq)
  
  // convert reactions to JIT reactions
  let fromMultiset (e:Mset.entry<'s>) = 
    (jit.sidDict.[e.element], e.multiplicity)
  
  let makeJitReaction (crnReaction:Reaction<'s, float, Expression.t<Inlined<'s>>>) = 
    let rid = newRID jit
    let reactants = List.map fromMultiset crnReaction.reactants
    let products  = List.map fromMultiset crnReaction.products
    
    // add a dependency to this reaction in each reactant species
    let updateDeps sid = 
      let oldDeps = jit.spDict.[sid].deps
      jit.spDict.[sid].deps <- oldDeps.Add rid
    List.iter (fst >> updateDeps) reactants
    
    // apply scaling
    let scaledPayload = scaleReaction jit crnReaction

    // return JitReaction
    let r = { id      = rid
            ; payload = scaledPayload
            ; reacts  = reactants
            ; prods   = products
            ; prop    = 0.0 }
    r.prop <- calculate_reaction_prop jit r
    r

  // populate rxDict
  for crnReaction in reactions do
    crnReaction
    |> makeJitReaction
    |> (fun r -> jit.rxDict.Add (r.id, r))
  done

  // compute total_prop
  jit.totalProp <- computeTotalProp jit

  // return jit
  jit

let get_species (jit:t<'a>) =
  let as_initial (sp:JitSpecies<'s>) : Initial<'s,Expression.t<string>> = 
    Initial.create(sp.isConstant, Expression.Float sp.population, sp.payload, None, None)
  jit.spDict.Values |> Seq.map as_initial |> Seq.toList

let get_reactions (jit:t<'a>) = jit.rxDict.Values |> Seq.map (fun r -> r.payload) |> Seq.toList

(************ /JIT creation **********************)

let fromCRN (crn : Crn) : t<Species> = 
  // setup environment to resolve parameter and rate symbols
  let paramEnv  = Parameters.to_env crn.settings.parameters
  // NB: the following method does not terminate with mutually recursive rates
  // TODO: termination check by testing whether the rate dependency graph is acyclic
  let rec inlineKeys (k:Key<Species>) = 
    match k with
    | Key.Species   s -> Expression.Key (Inlined.Species s)
    | Key.Time        -> Expression.Key (Inlined.Time)
    | Key.Parameter p -> Expression.Float (Environment.find paramEnv p)
    | Key.Rate      r -> Expression.expand inlineKeys crn.settings.rates.[r]
    
  // inline parameters and rates in reactions and plots
  let evalMassRates = Expression.eval (Environment.find paramEnv) 
  let evalFuncRates = Expression.expand inlineKeys
  let iReactions    = crn.reactions |> List.map (Reaction.map id evalMassRates evalFuncRates)

  let s:Crn_settings<Expression.t<Inlined<Species>>> = crn.settings.map (Expression.expand inlineKeys >> Expression.simplify) 
  let basicNewPlottable s = { name = Species.to_string s; structural = None; svg = None }
  let spEquality (x:Species) (y:Species) =  (x.name = y.name)
  create s.simulation s.stochastic s.rates s.parameters (crn.initials |> List.map (fun i -> i.species)) crn.initials iReactions basicNewPlottable spEquality spEquality


(* Main Gillespie function - Direct Method. With hashtables *)
let simulate_callback   (cancel          : cancel    ) 
                        (output          : output    ) 
                        (outputplottable : outputplottable)
                        (jit             : t<'s>     )
                        (calculus        : Calculus<'s>) =
  // sanity check and jit initialization
  init jit
  
  // prepare an environment to inline parameters later, when new reactions are discovered
  let env  = Parameters.to_env jit.parameters //jit.settings.parameters
  let eval = Expression.eval (Environment.find env) 
  
  (* MAIN SIMULATOR LOOP. *)
  let rec loop (first_loop:bool) =
    
    (* Pick a random reaction time *)
    let lu1 = timeToNextReaction jit   // total prop is not applied here, because events 
                                       // between now and now+dt might change concentrations

    (* Calculate the propensities of all reactions in the system and the time to the next reaction. *)
    let dt      = enactEvents lu1 output jit
    let nextNow = jit.now + dt
    
    (* Print data point *)
    if isPrintTime jit nextNow first_loop
      then 
        let dataPointNow = createRow jit.now (evaluatePlots jit)// (computeRatesEnv jit)
        output dataPointNow
        jit.dataPoints <- dataPointNow :: jit.dataPoints
        
        let printInterval = jit.simulation.get_print_interval()
        jit.nextPrintTime <- nextNow + printInterval
      else ()
      
    (* Stop if we reach the endpoint, the "cancel flag" is triggered or no simulation is applicable (adjusted to numerical drift). *)
    if isStopTime jit cancel || jit.totalProp = 0.0
      then jit // returns the simulation
      else
        // update the simulation time
        jit.now <- nextNow
        
        (* Pick a reaction *)
        let maybeReaction = pickNextReaction jit
        match maybeReaction with
        | None    -> jit
        | Some r  ->
          (* Modify populations according to that reaction. *)
          executeReaction jit r
          
          (* JIT CORE: expand the reactions set if a product has not been explored yet *)
          // filter out unexplored species
          let freshSpecies = r.prods 
                              |> List.choose (fun (prodSID, _) -> 
                                                let sp = jit.spDict.[prodSID]
                                                if sp.isExplored
                                                  then None
                                                  else Some sp)
          
          // extend plots with new species
          if jit.mustPlotAll then
            freshSpecies |> List.map (fun sp -> jit.printSpecies sp.payload) |> List.iter outputplottable
          elif not freshSpecies.IsEmpty 
            then extendPlotPatterns jit (freshSpecies |> List.map (fun sp -> sp.payload))

          for prodSp in freshSpecies do
            // get a list of explored species
            let exploredSpecies = jit.sidDict
                                  |> Seq.filter (fun y -> jit.spDict.[y.Value].isExplored)
                                  |> Seq.map (fun y -> y.Key)
                                  |> Seq.toList
            
            // set fresh species as explored
            prodSp.isExplored <- true
            
            // find new reactions
            for newReaction in calculus.react exploredSpecies prodSp.payload do
                      
              // store any new species
              for prod in newReaction.products do
                let isOld (x:Mset.entry<'s>) = jit.sidDict.ContainsKey x.element
                if not (isOld prod)
                  then 
                    let newJitSpecies = { id = newSID jit
                                        ; payload    = prod.element
                                        ; population = 0.0
                                        ; isExplored = false
                                        ; isConstant = false
                                        ; deps = Set.empty }
                    jit.sidDict.Add(prod.element,    newJitSpecies.id)
                    jit.spDict.Add(newJitSpecies.id, newJitSpecies)

                    // recompute 
                          
              // setup reactants and products
              let makeJitSp (x:Mset.entry<'s>) = (jit.sidDict.[x.element], x.multiplicity)
              let jitReacts = List.map makeJitSp newReaction.reactants 
              let jitProds  = List.map makeJitSp newReaction.products
              let flambda fRate =
                    let keyInliner = Key.inline_keys env jit.rates 
                    fRate |> Expression.expand keyInliner |> Expression.simplify
              let inlinedReaction = Reaction.map id eval flambda newReaction
              // apply scaling
              let scaledReaction = scaleReaction jit inlinedReaction

              // store the new reaction
              let tmpReaction = { id      = newRID jit
                                ; payload = scaledReaction
                                ; reacts  = jitReacts 
                                ; prods   = jitProds
                                ; prop    = 0.0 (* temporary value *) }

              // calculate actual propensity
              let rProp       = calculate_reaction_prop jit tmpReaction
              let jitReaction = {tmpReaction with prop = rProp }
                      
              // update the total propensity
              jit.totalProp <- jit.totalProp + rProp

              // update the reactions dictionary
              jit.rxDict.Add(jitReaction.id, jitReaction)
                    
              // add dependencies to the new reaction in each species
              for sid in List.map fst (jitReacts @ jitProds) do
                let sp      = jit.spDict.[sid]
                let newDeps = sp.deps.Add jitReaction.id
                sp.deps <- newDeps

          (* increase the steps count *)
          jit.stepsDone <- jit.stepsDone + 1
        
          loop false
  loop true |> ignore


let createResultsTable (jit:t<'s>) : Table<float> =
  let speciesTotal      = 
    if jit.mustPlotAll
      then let lastRow = List.last jit.dataPoints in lastRow.values.Length
      else jit.simulation.plots.Length

  let (columnNames:string list) = 
    if jit.mustPlotAll
      then jit.spDict.Values
            |> Seq.toList
            |> List.take (min speciesTotal jit.spDict.Values.Count)
            |> List.map (fun z -> (jit.printSpecies z.payload).name)
      else jit.simulation.plots 
            |> List.map (Expression.to_string (Inlined.to_string (fun s->(jit.printSpecies s).name)))
                          
  let extendedResults   = jit.dataPoints 
                            |> List.map (fun x -> if x.values.Length = speciesTotal
                                                   then x
                                                   else let dummySpecies = [1..speciesTotal-x.values.Length]
                                                        let allPops = (x.values |> List.ofArray ) 
                                                                      @ (dummySpecies |> List.map (fun _ -> 0.0))
                                                        Row.create x.time allPops)
  Table.from_rows columnNames extendedResults
  
let simulate (jit:t<'s>) calculus =
  let cancel = ref false
  let result = ref []
  let output row = result := row::!result
  let outputplottable _ = ()
  simulate_callback cancel output outputplottable jit calculus
  jit.dataPoints <- List.rev jit.dataPoints
  let resultsTable = createResultsTable jit
  jit, resultsTable

let to_ctmc (jit : t<'s>) (calc: Calculus<'s>) =
  let env = jit.parameters |> Parameters.to_env
  let populations = jit.spDict.Values
                    |> Seq.map (fun i -> i.payload, i.population |> int)
                    |> List.ofSeq
  let rates = jit.rates |> Map.toList
                        |> List.map (fun (k, v) -> k, v |> Expression.map ( fun z ->match z with
                                                                                    | Inlined.Species x -> Key.Species x
                                                                                    | Inlined.Time   -> Key.Time ))
                        |> Map.ofList
  let initial_state, applicable_reactions, adapter = calc.initialise_ctmc populations
  let ctmc_result = calc.to_ctmc (Hashtable.ofMap rates) jit.stochastic.scale initial_state applicable_reactions adapter
  let attributer (x:'s) : Attributes =
    let attr = jit.printSpecies x
    { name       = attr.name
      structure  = match attr.structural with Some x -> x | None -> ""
      svg        = match attr.svg        with Some x -> x | None -> ""}
  (ctmc_result, attributer)

let to_crn (jit : t<'s>) (original : Crn): Crn =
  let attributer (x:'s) : Attributes =
    let attr = jit.printSpecies x
    { name       = attr.name
      structure  = match attr.structural with Some x -> x | None -> ""
      svg        = match attr.svg        with Some x -> x | None -> ""}

  // initials
  let translateSpecies = jit.printSpecies >> fun x-> { name = x.name } : Species
  let zeroTimeInitials : Initial<Species, Expression.t<string>> list = 
    jit.spDict.Values
    |> Seq.map (fun i -> Initial.create(i.isConstant, Expression.Float i.population, translateSpecies i.payload, None))
    |> List.ofSeq
  
  let timedInitials = 
    jit.events 
      |> List.collect (fun e -> 
        if jit.now >= e.time
          then []
          else 
            let newTime = e.time - jit.now
            match e.target with 
            | Target.Species pop  -> 
                pop.index_to_species
                |> Array.toList
                |> List.map (fun pop -> 
                  let sp = translateSpecies pop.species
                  Initial.create(pop.constant, Expression.Float pop.value, sp, Some (Expression.Float newTime)))
            | OutputPoint -> [] )
  
  let initials = zeroTimeInitials @ timedInitials

  // reactions
  let reactions : Reaction<Species,Value,Functional> list 
    = jit.rxDict.Values
                  |> Seq.map (fun x->  x.payload 
                                        |> Reaction.map translateSpecies
                                                        Expression.Float
                                                        (Expression.map (fun x -> match x with 
                                                                                  | Inlined.Time -> Key.Time
                                                                                  | Inlined.Species z -> Key.Species <| translateSpecies z))
                                        
                                        )
                    |> Seq.toList
  
  // attributes
  let attributes =
    jit.spDict.Values 
    |> Seq.toList 
    |> List.map (fun sp -> let sp' = sp.payload |> attributer
                           sp'.name, sp')
    |> Stringmap.of_list

  // new CRN
  // scale has already been applied during JIT simulation, so it's removed in the converted CRN
  let settings' = { original.settings with stochastic = {original.settings.stochastic with scale = 1.0}}
  Crn.create original.name settings' reactions initials attributes true