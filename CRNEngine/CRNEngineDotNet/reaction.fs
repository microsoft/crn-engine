namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>] 
type Reaction<'s ,'v,'e> when 's:equality and 'v:equality and 'e:equality =
  {catalysts:Mset.t<'s>; reactants:Mset.t<'s>; reverse:Rate<'v,'e> option; rate: Rate<'v,'e>; products:Mset.t<'s>} 
  static member create(catalysts:Mset.t<'s>,reactants:Mset.t<'s>,reverse: Rate<'v,'e> option, rate: Rate<'v,'e>,products:Mset.t<'s>) = 
    {catalysts=catalysts;reactants=reactants;reverse=reverse;rate=rate;products=products}
  static member create(reactants:Mset.t<'s>, reverse:Rate<'v,'e> option, rate:Rate<'v,'e>, products:Mset.t<'s>) = 
    Reaction<'s ,'v,'e>.create(Mset.empty,reactants,reverse,rate,products)
  static member create(catalysts:'s list, reactants:'s list, (reverse:Rate<'v,'e> option, rate:Rate<'v,'e>), products:'s list) = 
    Reaction<'s ,'v,'e>.create(Mset.from_list catalysts,Mset.from_list reactants,reverse,rate,Mset.from_list products)
  static member create(reactants:'s list, (reverse:Rate<'v,'e> option, rate:Rate<'v,'e>), products:'s list) = 
    Reaction.create(Mset.empty,Mset.from_list reactants,reverse,rate,Mset.from_list products)
  static member create(text:string, ps:Parser.t<'s>, pv:Parser.t<'v>, pe:Parser.t<'e>, defaultMassActionRate:'v) = 
    Parser.from_string (Reaction.parse ps pv pe defaultMassActionRate) text 
  static member map (fs:'s -> 's2) (fv:'v -> 'v2) (fe:'e -> 'e2) (r:Reaction<'s,'v,'e>) = 
    Reaction.create(
      Mset.map fs r.catalysts,
      Mset.map fs r.reactants,
      Lib.option_map2 (Rate.map2 fv fe) r.reverse,
      Rate.map2 fv fe r.rate,
      Mset.map fs r.products
    )
  member r.string (species_to_string:'s -> string) (value_to_string:'v -> string) (expression_to_string:'e -> string) = 
    let rate_to_string (r:Rate<'v,'e>) = 
      match r.to_string value_to_string expression_to_string with
      | "{1.0}" -> ""
      | str     -> str in 
    let multiset_to_string m = Mset.to_string species_to_string " + " m in
    let catalysts:string = if Mset.is_empty r.catalysts then "" else (multiset_to_string r.catalysts) + " ~" in
    let reactants:string = multiset_to_string r.reactants in
    let arrow:string =
      match r.reverse with
      | None -> "->" + rate_to_string r.rate 
      | Some(v) -> "<->" + rate_to_string r.rate + rate_to_string v 
    in
    let products:string = multiset_to_string r.products in
    let strs = List.filter (fun s -> s <> "") [catalysts; reactants; arrow; products] in
      Lib.string_of_list id " " strs
  member r.tuple = (r.catalysts, r.reactants, r.reverse, r.rate, r.products)
  member r.rate_units time_unit conc_unit = //Compute a string representing the reaction rate unit
    let nr_reactants = Mset.size r.reactants in
    let nr_products = Mset.size r.products in
    let nr_catalysts = Mset.size r.catalysts in
    let nr_reactants = nr_reactants + nr_catalysts in //TODO: check if this is correct
    let nr_products = nr_products + nr_catalysts in
    let reactants_induced_unit nr_r =
      if nr_r = 1 then ""
      else if nr_r = 2 then "/" + conc_unit
      else "/" + conc_unit + "\^" + (string (nr_r - 1)) in
    let forward_unit = "/" + time_unit + (reactants_induced_unit nr_reactants) in
    let backward_unit = "/" + time_unit + (reactants_induced_unit nr_products) in
      (forward_unit, backward_unit)
  member r.scale (scale_value:float -> int -> 'v -> 'v) (s:float) (scale_functional:'e -> 'e) =
    if s=1.0 then r
    else
      let forward_power = (Mset.size r.reactants) + (Mset.size r.catalysts) in
      let reverse_power = (Mset.size r.products) + (Mset.size r.catalysts) in 
      let scale_rate (power:int) (rate:Rate<'v,'e>) = rate.map (scale_value s power) scale_functional in
      let rate = scale_rate forward_power r.rate
      let reverse = Lib.option_map2 (scale_rate reverse_power) r.reverse
      Reaction.create(r.catalysts, r.reactants, reverse, rate, r.products)
  member r.normalise () = //Eliminates catalysts and reversibles
    let forward = 
      Reaction.create(
        Mset.empty,
        Mset.union (=) r.reactants r.catalysts,
        None,
        r.rate,
        Mset.union (=) r.products r.catalysts
      ) in
    let reverse =
      match r.reverse with 
      | None -> None
      | Some f ->
        Some (Reaction.create(Mset.empty, forward.products, None, f, forward.reactants) )
    (forward, reverse)
  static member to_string fs fv fe (r:Reaction<'s,'v,'e>) = r.string fs fv fe
  static member normalise_list (rs:Reaction<'s,'v,'e> list) =
    let collect_pair (nrs:Reaction<'s,'v,'e> list) (r:Reaction<'s,'v,'e>) =
      let (fr, br_opt) = r.normalise ()
      match br_opt with
      | None -> fr::nrs
      | Some br -> br::fr::nrs 
    List.rev (List.fold collect_pair [] rs)
  ///Number of species in reactants
  member r.numInReactants (s:'s) = Mset.get_mult (=) r.reactants s 
  ///Number of species in products
  member r.numInProducts (s:'s) = Mset.get_mult (=) r.products s 
  member r.getStoich (constant:bool) (s:'s) = if constant then 0.0 else (float)((r.numInProducts s) - (r.numInReactants s)) //Get species stochiometry
  member r.allSpecies = Mset.elements (Mset.union (=) (Mset.union (=) r.reactants r.products) r.catalysts)
  ///reverses a reaction if reversible (e.g. reverse(A <-> B) = B <-> A)
  member r.reverse_reaction () = 
    match r.reverse with
    | Some (rev:Rate<'v,'e>) -> Some (Reaction.create(r.catalysts, r.products, Some r.rate, rev,  r.reactants))
    | None -> None
  //Need parametric defaults in order to do this...
  static member parse_irreversible (pv:Parser.t<'v>) (pe:Parser.t<'e>) defaultRate = 
    Parser.kw "->" >>. (Rate.parse pv pe |>> fun r -> (r,None)
                        <|> Parser.preturn (Rate.MassAction defaultRate, None))
  static member parse_reversible (pv:Parser.t<'v>) (pe:Parser.t<'e>) defaultRate = 
    Parser.kw "<->" >>. (Parser.pTry (Rate.parse pv pe +>>+ Rate.parse pv pe)
                        <|> (Parser.braces (pv +>> Parser.kw "," +>>+ pv) 
                        <|> Parser.preturn (defaultRate, defaultRate)
                        |>> fun (x,y) -> (Rate.MassAction x, Rate.MassAction y)))
      |>> fun (r,r') -> (r,Some r')
  static member parse (ps:Parser.t<'s>) (pv:Parser.t<'v>) (pe:Parser.t<'e>) (defaultRate:'v) : Parser.t<Reaction<'s ,'v,'e>>= 
    Parser.pTry(
      //parse reactants
      Mset.parse ps +>>+
        Parser.choice [ 
          Parser.kw "~" >>. Mset.parse ps |>> Some
          Parser.preturn None
        ]
         //lookup arrows
         +>> (Parser.lookAhead "<->" <|> Parser.lookAhead "->")
    ) >>= fun (mset1, mset2) ->
     //parse arrows and rate
     Parser.choice [
       Reaction<'s ,'v,'e>.parse_reversible pv pe defaultRate
       Reaction<'s ,'v,'e>.parse_irreversible pv pe defaultRate
     ] 
      //parse products
      +>>+ (Mset.parse ps) >>= fun ((fwRate, bwRate), products) ->
        //create reaction
        Parser.preturn (
          match mset2 with
          | None           -> Reaction.create(mset1,bwRate,fwRate,products)
          | Some reactants -> Reaction.create(mset1,reactants,bwRate,fwRate,products)
        )
  ///NB the code below is for specific instantiations of the reaction type
  static member private convert_simplify (sc:float) (pops:Populations<Species,'a>) env ratesEnv (r:Reaction<Species,Value,Functional>) =
    let scale_value (scale:float) (power:int) (v:Value) = //TODO: move outside of reaction.ml 
      Expression.Times [v; Expression.Power{base_=Expression.Float scale;exponent=Expression.Minus{sub1=Expression.Float 1.0;sub2=Expression.Float ((float) power)}}]
    let scale_float (scale:float) (power:int) (f:float) = f * (scale ** (1.0 - (float) power))
    let fs (s:Species) = 
      match pops.tryFind_index s with
      | Some s -> s
      | None -> failwith ("Species " + s.name + " not found in a rate expression")
    let indexedRatesEnv = 
      ratesEnv
      |> Map.map (fun _ -> Expression.map (Inlined.map fs)) 
    let fv (v:Value) = Expression.eval (Environment.find env) v
    let fe (e:Functional) = e |> Expression.expand (Key.map fs >> Key.inline_keys env indexedRatesEnv) |> Expression.simplify
    ((Reaction.map fs fv fe r).scale scale_float sc id).normalise ()
    //normalise (scale scale_float sc (map fs fr r))
  static member get_sim_reactions_products 
    (scale:float)
    (pops:Populations<Species,'a>) 
    (env:Environment.t) 
    (ratesEnv:Map<string,Expression.t<Inlined<Species>>>) 
    (reactions:Reaction<Species,Value,Functional> list) =
    let to_lambda (r:Reaction<int,float,Expression.t<Inlined<int>>>) = Reaction.map id id Expression.to_lambda r
    let fold = 
      fun acc r ->
        let (fwd,bwd_opt) = Reaction<Species,Value,Functional>.convert_simplify scale pops env ratesEnv r
        let fwd_prods = Mset.union (=) fwd.products fwd.catalysts
        match bwd_opt with
        | None -> (to_lambda fwd,fwd_prods,fwd.rate)::acc
        | Some bwd ->
          let bwd_prods = Mset.union (=) bwd.reactants bwd.catalysts
          (to_lambda bwd,bwd_prods,bwd.rate)::(to_lambda fwd,fwd_prods,fwd.rate)::acc
    List.rev (Lib.fold_left fold [] reactions) (* RLP: is List.rev essential? *)
  member r.conversion_factor = Mset.fold_left_m (fun prod m -> prod * Lib.fac m.multiplicity) 1 r.reactants |> float  
  member r.deterministic_to_stochastic v = Expression.div v (Expression.Float (r.conversion_factor))
  member r.stochastic_to_deterministic v = Expression.mul v (Expression.Float (r.conversion_factor))