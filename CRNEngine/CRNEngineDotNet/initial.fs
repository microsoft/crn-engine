// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
open Parser

[<JavaScript>]
type Initial<'s,'v> when 's:equality and 'v:equality = 
  {constant:bool; value:'v; species:'s; time:'v option; spatial:Spatial_initial.t option}
  static member create(constant:bool, value:'v, species:'s, time:'v option, spatial:Spatial_initial.t option) =
    {constant = constant; value=value; species=species; time=time; spatial=spatial}
  static member create(c:bool, v:'v, s:'s, t:'v option) = Initial.create(c,v,s,t,None)   
  static member create(v:'v, s:'s, t:'v option, sp:Spatial_initial.t option) = Initial.create(false,v,s,t,sp) 
  static member create(v:'v, s:'s, t:'v option) = Initial.create(false,v,s,t,None) 
  static member create(v:'v, s:'s) = Initial.create(false, v, s, None, None)
  static member mapSpecies f (i:Initial<'s, 'v>) = 
    Initial.create(i.constant, i.value, (f i.species), i.time, i.spatial)
  static member mapValues f (i:Initial<'s, 'v>) = 
    Initial.create(i.constant, (f i.value), i.species, (Option.map f i.time), i.spatial)
  static member collectSpecies (f : 'a -> 'b list) (is : Initial<'a, 'v>) : Initial<'b, 'v> list =
    let newSpecies = f is.species
    List.map (fun sp -> Initial.create(is.constant, is.value, sp, is.time, is.spatial)) newSpecies
  static member to_string (fs:'s -> string) (fv:'v -> string) (not_initial:'v->bool) (p:Initial<'s,'v>) =
    (if p.constant then "constant " else "") + (fv p.value) + " " + (fs p.species) + 
    match p.spatial with
      | None -> match p.time with Some t -> " @ " + fv t | None -> ""
      | Some sp -> 
          if sp <> Spatial_initial.defaults
          then "{ spatial = " + Spatial_initial.to_string sp + "}"
          else ""
  member i.scale (scale_value:'v -> 'v) = {i with value = scale_value i.value}
  ///displays an initial in Classic DSD syntax *) // TODO: CS: can display be merged with to_string?
  static member display (fs:'s -> string) (fv:'v -> string) (not_initial:'v->bool) (p:Initial<'s,'v>) =
    (if p.constant then "constant " else "") +
    (match p.spatial with
      | None -> 
          (fv p.value)// +
          //(if (not_initial p.time) then (" @ " + (value_to_string p.time)) else "")
      | Some sp -> 
          if sp <> Spatial_initial.defaults
          then "{ spatial = " + Spatial_initial.to_string sp + " }"
          else "") +
    // (value_to_string p.value) +
    " * " +
    (fs p.species) +
    (match p.time with Some t -> " @ " + fv t | None -> "")
  static member to_gui (fs:'s -> string) (fv:'v -> string) (p:Initial<'s,'v>) = 
    Initial.create(p.constant, fv p.value, fs p.species, Option.map fv p.time, p.spatial)    // TODO: Check whether fv needs updating?
  static member from_gui (ps:string -> 's) (pv:string -> 'v) (g:Initial<string,string>) = 
    Initial.create(g.constant, pv g.value, ps g.species, Option.map pv g.time, g.spatial)
  static member eval (fv:'v -> 'output) (i:Initial<'s,'v>) = 
    Initial.create(i.constant, fv i.value, i.species, Option.map fv i.time, i.spatial)
  ///Sum up the initial conditions to define initial values for each population, and separate timed events for downstream consumption. 
  ///This initialization method is generic in both the species 's of the population and the resulting format 'a 
  ///in which populations are stored (e.g. the 2d spatial simulator stores populations along a grid (cartesian plane), 
  ///so 'a = float [][]; non-spatial CRNs only store the concentration of a species, so 'a = float).
  static member to_initialpops_events_poly (env:Environment.t) (initial_time:float) (is:Initial<'s,Expression.t<string>> list) (accumulator: Initial<'s,float> seq -> 'a) =
    let initials = is |> List.map (Initial.eval (Expression.eval (Environment.find env))) 
    let event_inits = initials |> List.filter (fun i -> match i.time with Some t -> t <> initial_time | None -> false) 
    //let ev2p (e : t<'s,'a>) : Population<'s,'a> = {Population.create e.constant e.value e.species with initial = false} in
    let mk_event (t:float, is:seq<Initial<'s,float>>) : Event<'s,float,'a> =
      is 
      |> Seq.groupBy (fun i -> i.species) 
      |> Seq.map (fun (sp,is) -> {Population.create((is |> Seq.exists (fun e -> e.constant)),(accumulator is),sp) with initial = false})
      |> Seq.toList |> Populations.create
      |> fun x -> Event<'s,float,'a>.create x t 
    let events = 
      event_inits 
      |> Seq.groupBy (fun i -> i.time.Value) 
      |> Seq.map mk_event
      |> Seq.toList
    let p2p (s:'s, v:'a, c) = Population.create(c,v,s) 
    let constant_flag (is: Initial<'s,float> seq) =
      match is |> Seq.filter (fun i -> i.constant) |> Seq.length with
      | 0 -> false
      | 1 -> if is |> Seq.length = 1 then true else failwith "Constancy overload"
      | _ -> failwith "Constancy overload" 
    let inits :  seq<'s * 'a * bool> = 
      initials 
      |> Seq.groupBy (fun i -> i.species) 
      |> Seq.map (fun (s,is) -> (s, is |> Seq.filter (fun i -> match i.time with Some t -> t = initial_time | None -> true) |> accumulator, constant_flag is)) 
    let populations = inits |> Seq.map p2p |> Seq.toList |> Populations.create 
    populations, events
  static member to_initialpops_events (env:Environment.t) (initial_time:float) (is:Initial<'s,Expression.t<string>> list) = 
    Initial<'s,Expression.t<string>>.to_initialpops_events_poly env initial_time is (Seq.sumBy (fun i -> i.value)) 
  
  static member parse (species:Parser.t<'s>) (value:Parser.t<'v>) (initialValue:'v) = 
    let CONSTANT = Parser.kw "constant" 
    let INIT     = Parser.kw "init"
    let SPATIAL  = Parser.kw "spatial"
    let AT       = Parser.kw "@"
    let returnInitial ((((cFlag, sp), v), t), spatial) = Parser.preturn (Initial.create(cFlag, v, sp, t, spatial))
    let time = Parser.opt (AT >>+ value) 
    let isConstant = Parser.opt CONSTANT |>> Option.isSome
    let pspatial = Parser.braces ( SPATIAL >>. Parser.kw "=" >>. Spatial_initial.parser )
    let precord sp = Parser.record    
                      (Initial.create(false, initialValue, sp, None, None))
                      [ "spatial",  Spatial_initial.parser  |>> fun si initial -> { initial with spatial = Some si }
                        "time",     value                   |>> fun t' initial -> { initial with time = Some t' }
                        "value",    value                   |>> fun v' initial -> { initial with value = v' }
                        "constant", Parser.pbool .>> spaces |>> fun b' initial -> { initial with constant = b' } ] <?> "{InitialFields}"
    Parser.choice [ 
      INIT >>. isConstant +>>+ species .>> Parser.spaces >>= 
        fun x -> 
          Parser.choice [ 
            // TODO: timed spatial events not implemented in the simulator, so they're blocked here for now
            precord (snd x) |>> fun i -> Initial.create(fst x, i.value, i.species, i.time, i.spatial) 
            //kw "=" >>. preturn (snd x) >>= precord
            //                              |>> fun i -> Initial.create(fst x, i.value, i.species, i.time, i.spatial) 
            value +>>+ time  >>= fun ( v, t) -> returnInitial (((x, v), t), None) 
          ]; 
      Parser.pTry (species  +>> kw "=" >>= precord) <?> "<species> = <record>"
      Parser.pTry (
        ((isConstant .>>. value) +>>+ species ) +>>+ time) <?> "<value> <species>"
        .>>. Parser.opt pspatial
        >>= fun ((((a,b),c),d),sp) ->  returnInitial ((((a,c),b),d), sp) 
    ] 