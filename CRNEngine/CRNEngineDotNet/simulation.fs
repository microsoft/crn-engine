namespace Microsoft.Research.CRNEngine

[<JavaScript>]
type Simulation<'s> when 's:equality = 
  { 
    settings: Simulation_settings<'s>;
    populations: Populations<Species,float>;
    events: Event<Species, float, float> list;
    currenttime: float;
    nextprinttime: float;
    stepsdone: int;
  }
  static member empty () : Simulation<'s> = {
    settings    = Simulation_settings.defaults
    populations = Populations.empty ()
    events = []
    currenttime = 0.0
    nextprinttime = 0.0
    stepsdone = 0
  }
  static member create 
    (oldPop:Populations<Species,float>) 
    (events:Event<Species, float, float> list) 
    (settings:Simulation_settings<Expression.t<Key<'s>>>) 
    (scale:float) =
    let scale_event_pops (ev:Event<Species, float, float>) =
      match ev.target with
      | Target.Species pop -> 
        { Event.target = Target.Species (Populations<Species,float>.scale pop scale) ; Event.time = ev.time }
      | _ -> ev
    let populations,events = 
      if scale <> 1.0 
      then (Populations<Species,float>.scale oldPop scale), (List.map scale_event_pops events)
      else oldPop,events
    let event_matters (ev:Event<Species, float, float>) =
      match ev.target with //Events are sorted by time, evaluated by the environment
      | Target.Species pop -> Array.exists (fun p -> p <> 0.0) (pop.get_pop_array)
      | _ -> true 
    let out_event = { Event.target = Target.OutputPoint; Event.time = settings.final}
    let real_events = out_event :: List.filter event_matters events 
    let first_pop_times = 
      real_events |> List.choose (fun e -> match e.target with Target.Species _ -> Some e.time | _ -> None)
    let populations, events =
      if first_pop_times.IsEmpty then
        ( populations, real_events |> List.sortBy (fun e -> e.time) )
      else
        let first_pop_time = List.min first_pop_times 
        if first_pop_time < settings.initial 
        then
          let init_events, later_events =
            real_events 
            |> List.partition (fun e -> match e.target with Target.Species _ -> e.time < settings.initial | _ -> false) 
          ( init_events |> List.choose (fun e -> match e.target with Target.Species p -> Some p | _ -> None)
                        |> List.reduce (Populations.merge (+))
                        |> Populations.merge (+) populations
          //, { Event.time = settings.initial; Event.target = Event.Species populations }::later_events 
          //|> List.sortBy (fun e -> e.time) )
          , later_events |> List.sortBy (fun e -> e.time) )
        elif first_pop_time = settings.initial then
          let init_events, later_events =
            real_events 
            |> List.partition (fun e -> match e.target with Target.Species _ -> e.time = first_pop_time | _ -> false) 
          ( init_events |> List.choose (fun e -> match e.target with Target.Species p -> Some p | _ -> None)
                        |> List.reduce (Populations.merge (+))
                        |> Populations.merge (+) populations
          , later_events |> List.sortBy (fun e -> e.time) )
        else
          ( populations
          , real_events |> List.sortBy (fun e -> e.time) ) 
    in { 
      settings = settings;
      populations = populations;
      events = events;  
      currenttime = settings.initial; 
      nextprinttime = settings.initial;
      stepsdone = 0; 
    }




