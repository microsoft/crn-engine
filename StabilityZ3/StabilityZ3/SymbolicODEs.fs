// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.SymbolicODEs

open Microsoft.Research.CRNEngine
open MathNet.Symbolics

type t<'s> when 's : equality = 
    {
      inits     : Initial<'s, Expression> list
      gradients : Map<Symbol, Expression>
      // TODO: add intermediates
      settings  : Crn_settings<'s>
      name      : string
    }

type CrnExp<'s> when 's : equality = Microsoft.Research.CRNEngine.Expression.t<'s>

let rec toMathNetSymbolics (keyMap: 's -> Expression) (e:Expression.t<'s>) : Expression =
  let toSym = toMathNetSymbolics keyMap
  let toSymPair e1 e2 : Expression * Expression = 
    let e1' = toSym e1
    let e2' = toSym e2
    (e1', e2')
  match e with
  | CrnExp.Key k        -> keyMap k
  | CrnExp.Float f      -> Approximate.real f |> Expression.Approximation
  | CrnExp.Plus es      -> Sum     (es |> List.map toSym)
  | CrnExp.Times es     -> Product (es |> List.map toSym)                              
  | CrnExp.Divide 
      {div1=e1; div2=e2} -> let e1', e2' = toSymPair e1 e2
                            e1' / e2'
  | CrnExp.Power 
      { base_     = e1
      ; exponent  = e2}  -> let e1' = toSym e1                            
                            match e2 with
                            | CrnExp.Float 1.0 -> e1'
                            | CrnExp.Float 2.0 -> e1' * e1'
                            | CrnExp.Float 3.0 -> e1' * e1' * e1'
                            | CrnExp.Float 4.0 -> e1' * e1' * e1' * e1'
                            | _ -> Power (e1', toSym e2)
  | CrnExp.Minus 
      { sub1 = e1
      ; sub2 = e2 }      -> let e1', e2' = toSymPair e1 e2
                            e1' - e2'
  | CrnExp.Absolute e1  -> abs (toSym e1)
  | CrnExp.Log e1       -> Operators.ln (toSym e1 )
  | CrnExp.Ceiling _    -> failwith "Ceiling unsupported in MathNet.Symbolics"
  | CrnExp.Floor _      -> failwith "Floor unsupported in MathNet.Symbolics"
  | CrnExp.Round _      -> failwith "Round unsupported in MathNet.Symbolics"
  | CrnExp.Modulo _     -> failwith "Modulo unsupported in MathNet.Symbolics"
  | CrnExp.If _         -> failwith "Conditionals unsupported in MathNet.Symbolics"

let toReactionSpecies (crn : Crn) =
    crn.reactions
      |> List.collect (fun r-> r.reactants @ r.products)
      |> List.map (fun rElem -> rElem.element)
  
let allSpecies (crn:Crn) =
  let initialSpecies = crn.initials
                        |> List.map (fun i -> i.species)
  let reactionSpecies = toReactionSpecies crn
  initialSpecies @ reactionSpecies
    |> Seq.distinct
    |> List.ofSeq

  
//  let translateMassAction env (k:string) : Exp = 
//    Environment.find env k |> Expression.Real

let ofCrn (crn:Crn) =
  let zero = Expression.Real 0.0
  let one  = Expression.Real 1.0
    
  // find the set of constant species
  let constants = crn.initials 
                    |> List.filter (fun i -> i.constant)
                    |> List.map (fun i -> i.species.name)
                    |> Seq.distinct
                    |> List.ofSeq
  // initialize the equations to 0 for all species
  let initialGrads = allSpecies crn |> List.fold (fun (m:Map<Symbol, Expression>) x -> m.Add(Symbol x.name, zero)) Map.empty

  // Map a functional rate to a MathNet expression
  let rates = crn.settings.rates
  let rec translateFunctional (k:Key<Species>) =
    match k with
    | Key.Parameter p -> Expression.Symbol p 
    | Key.Species sp  -> Expression.Symbol sp.name
    | Key.Time        -> Expression.Symbol "time"
    | Key.Rate r      -> rates.[r] |> toMathNetSymbolics translateFunctional
  let fe (e:Functional) = toMathNetSymbolics translateFunctional e 

  // Map a parameter to a MathNet expression
  let fv (v:Microsoft.Research.CRNEngine.Value) = toMathNetSymbolics Expression.Symbol v
  let grads = 
    crn.reactions     
    |> Reaction.normalise_list                                                // expand reverse reactions
    |> List.map (fun r -> Reaction.map (fun (sp:Species) -> sp.name) fv fe r) // convert reaction rate to Symbolics
    |> List.fold                                                              // compute the deterministic rate equations
      (fun (ode:Map<Symbol, Expression>) r -> 
          let power = r.reactants 
                      |> List.map (fun m -> Power ( Expression.Symbol m.element, Expression.FromInt32 m.multiplicity))
                      |> fun ps -> match ps with [] -> one | _ -> Expression.Product ps
          r.allSpecies
          |> List.fold 
                (fun oldGradients sp ->  
                  let isConstant = constants |> List.exists (fun x -> x = sp)
                  let stoichiometry = r.getStoich isConstant sp
                  let stoichVal = stoichiometry |> Approximate.real |> Expression.Approximation
                  if stoichiometry <> 0.0
                  then 
                      let factor = 
                          match r.rate with
                          | Rate.MassAction rate -> Expression.Product [ stoichVal; rate; power ]
                          | Rate.Function fRate  -> Expression.Product [ stoichVal; fRate ]
                          
                      oldGradients.Add(Symbol sp, oldGradients.[Symbol sp] + factor) 
                  else oldGradients) ode
      ) initialGrads
  { 
    name      = crn.name
    gradients = grads
    inits     = crn.initials
                 |> List.map (Initial.mapSpecies (fun i -> Expression.Symbol i.name) 
                              >> Initial.mapValues (toMathNetSymbolics Expression.Symbol))
    settings  = crn.settings.map (toMathNetSymbolics translateFunctional)
    }


let isVariable (p:Parameter) = 
  match p.prior with
  | Some pr -> pr.variation <> Variation.Fixed
  | None    -> false

let toOdeString (odes:t<'s>) =      
    odes.gradients
    |> Map.toArray
    |> Array.map 
        (fun (Symbol k, ode) -> 
            let ode' =  
                odes.settings.parameters
                |> List.fold(fun e p -> 
                    let v = p.name |> Expression.Symbol
                    let f = 
                      if isVariable p 
                      then p.name |> Expression.Symbol                        
                      else p.value |> Approximate.real |> Expression.Approximation
                    e |> Structure.substitute v f
                    ) ode                                        
            let odeString = MathNet.Symbolics.Infix.format (Algebraic.expand ode')            
            sprintf "d(%s) = %s" k odeString)


let Stoich (crn:Crn)  = 
    let species   = crn.initials |> List.map(fun i -> i.species)
    let reactions =  crn.reactions |> Reaction.normalise_list    

    let M = reactions.Length
    let N = species.Length
    let S = Array.init N (fun i -> Array.init M (fun j -> 0))


    let sp_map = species |> List.mapi(fun i s -> (s.name,i)) |> Map.ofList
    let rxn_map = reactions |> List.mapi(fun i r -> (r,i)) |> Map.ofList
        
        
    reactions
    |> List.iter(fun r -> 
        let R = r.reactants |> Mset.to_map
        let P = r.products  |> Mset.to_map
        let j = rxn_map.[r]

        R |> Map.iter(fun s n -> S.[sp_map.[s.name]].[j] <- S.[sp_map.[s.name]].[j] - n)
        P |> Map.iter(fun s n -> S.[sp_map.[s.name]].[j] <- S.[sp_map.[s.name]].[j] + n)                                
        )

    let sp = species |> List.map (fun s -> s.name)
    
    S, sp


(* Convert a string CRN to a set of differential equations (odes), stoichiometry matrix (S), set of species (sp), map of diffusible species to rates *)   
let ofCrnString (crn_str:string) = 
    let crn = crn_str |> Microsoft.Research.CRNEngine.Crn.from_string
                
    let ode_string = crn |> ofCrn |> toOdeString
            
    let odes =                
        ode_string
        |> Array.map(fun l -> 
            let eq = l.Split('=')
            let v = eq.[0].Replace("d(","").Replace(")","").Trim()
            v, eq.[1]
        )
    
    let S, sp = Stoich crn  
    let diff = 
        crn.settings.spatial.diffusibles
        |> List.map(fun (s,r) -> 
              match s with              
              | Expression.Key (Key.Species sp) -> sp.name, r
              | _ -> failwith "Only species were expected in diffusible definition")
        |> Map.ofSeq    
        
    let env = crn.settings.parameters |> Parameters.to_env
    let pops, _ = 
        (crn.saturate_initials ()).initials
        |> Initial<Species,Value>.to_initialpops_events env crn.settings.simulation.initial
    let initials = 
        pops.index_to_species 
        |> Array.map (fun pop -> pop.species.name, pop.value) 
        |> Map.ofArray

    odes, S, sp, diff, crn.settings.parameters, initials
