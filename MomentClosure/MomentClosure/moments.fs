module Microsoft.Research.CRNEngine.Moments

open MathNet.Symbolics
open Microsoft.Research.CRNEngine

type moment_equation = int[] * Expression
type output = Row<Point> -> unit    // ND: Currently using floats as we are shipping all moments back to the GUI
type t = 
  { name           : string
  ; populations    : Populations<Species,float>
  ; equations      : moment_equation list
  ; log_approximations : (int[] * (float * Expression) list) list option
  ; monomials      : int[] list
  ; mc_settings    : Moment_closure_settings.t<Expression.t<int> * int option> 
                     (* 'int option' is used to plot the standard devition of a first order moment.
                        In particular, it has value (Some j) only when plotting a first order moment 
                        such as <X>. In this case Expression.t<int> is a single Expression.Key i, 
                        i is the index of <X> in the list of monomials, and j is the index of <X^2> *)
  ; ode_settings   : Deterministic_settings
  ; sim_settings   : Simulation_settings<Functional> }


let toSym  sp    = Expression.Symbol sp
let toApprox i   = Approximate.real i |> Expression.Approximation
let toRational (i:float) = 
  let rec f (n:float) = 
    let denominator = 10.0**n
    let product = i*denominator
    if System.Math.Round product = product 
    then product, denominator
    else f (n+1.0)    

  let num, den = f 0.0
  MathNet.Numerics.BigRational.FromIntFraction
    (num |> int, den |> int) 
    |> Expression.FromRational

let allSpecies (sys:t) = sys.populations.index_to_species 
                          |> Array.map (fun x -> x.species) 
                          |> List.ofArray  

(*
let rec to_expression_t (variableReader : string -> 's) (e:Expression) : Microsoft.Research.CRNEngine.Expression.t<'s> = 
  let f = to_expression_t  variableReader
  let complexNumberError = Infix.format >> sprintf "Cannot convert complex number %s to CRN expression." 
  let genericError       = Infix.format >> sprintf "Cannot convert \"%s\" to CRN expression."
  match e with
  | Number              n -> MathNet.Numerics.BigRational.ToDouble n
                              // TODO: is there a loss in converting double to float?
                              |> Expression.Float 
  | Approximation       n -> match n with
                              | Real    f -> Expression.Float f
                              | Complex _ -> complexNumberError e |> failwith
  | Identifier (Symbol x) -> variableReader x |> Expression.Key
  | Constant            c -> match c with
                              | Constant.E   -> Expression.Float 2.7182818285
                              | Constant.Pi  -> Expression.Float 3.1415926536
                              | Constant.I   -> complexNumberError e |> failwith
  | Power         (b,exp) -> Expression.power (f b) (f exp)
  | Product            es -> es 
                              |> List.map f
                              |> Expression.Times
  | Sum                es -> es 
                              |> List.map f
                              |> Expression.Plus
  | Function  (fn, arg)  -> match fn with 
                            | Abs   -> Microsoft.Research.CRNEngine.Expression.Absolute (f arg)
                            | Ln    -> Microsoft.Research.CRNEngine.Expression.Absolute (f arg) // TODO
                            | Exp   -> Microsoft.Research.CRNEngine.Expression.Absolute (f arg) // TODO
                            | Sin   -> genericError e |> failwith
                            | Cos   -> genericError e |> failwith
                            | Tan   -> genericError e |> failwith
                            | Cot   -> genericError e |> failwith
                            | Sec   -> genericError e |> failwith
                            | Csc   -> genericError e |> failwith
                            | Cosh  -> genericError e |> failwith
                            | Sinh  -> genericError e |> failwith
                            | Tanh  -> genericError e |> failwith
                            | Asin  -> genericError e |> failwith
                            | Acos  -> genericError e |> failwith
                            | Atan  -> genericError e |> failwith
  | FunctionN (fn, args) -> genericError e |> failwith // TODO
  | ComplexInfinity      -> genericError e |> failwith
  | PositiveInfinity     -> genericError e |> failwith
  | NegativeInfinity     -> genericError e |> failwith
  | Undefined            -> genericError e |> failwith
  *)
let rec splitCoefficient (e:Expression) = 
  match e with
  | Expression.Number            _ -> (Some e, None)
  | Expression.Approximation     _ -> (Some e, None)
  | Expression.Identifier        _ -> (None, Some e)
  | Expression.Constant          _ -> (Some e, None)
  | Expression.Power             _ -> (None, Some e)
  | Expression.Product           p -> p |> List.map splitCoefficient
                                        |> List.fold 
                                            (fun (cState, eState) (newc, newe) -> 
                                              let c = match newc with
                                                      | None    -> cState
                                                      | Some co -> match cState with
                                                                    | None     -> Some co
                                                                    | Some cst -> Some (co * cst)
                                              let ex = match newe with
                                                        | None    -> cState
                                                        | Some e1 -> match eState with
                                                                     | None     -> Some e1
                                                                     | Some e2 -> Some (e1 * e2)
                                              (c, ex)
                                            )
                                        (None, None)
  | Expression.Sum               _ -> (Some Undefined, Some Undefined)
  | Expression.Function          _ -> (Some Undefined, Some Undefined)
  | Expression.FunctionN         _ -> (Some Undefined, Some Undefined)
  | Expression.ComplexInfinity   _ -> (Some Undefined, Some Undefined)
  | Expression.PositiveInfinity  _ -> (Some Undefined, Some Undefined)
  | Expression.NegativeInfinity  _ -> (Some Undefined, Some Undefined)
  | Expression.Undefined         _ -> (Some Undefined, Some Undefined)


let zero = Expression.Zero
let one = Expression.One

let evalRate rate = match rate with
                        | Rate.MassAction f -> f
                        | Rate.Function _ -> failwith "Functional rates not supported yet"


let propensity kinetics (species:Species list) (r:Reaction<int, float, _>) = 
  let c = evalRate r.rate |> toRational
  match r.reactants with 
  | [] -> c
  | [{ element = X; multiplicity = 1}] -> 
      let x = toSym (species.[X].name)
      c * x
  | [{ element = X; multiplicity = 1}
     { element = Y; multiplicity = 1}] -> 
      let x = toSym (species.[X].name)
      let y = toSym (species.[Y].name)
      c * x * y
  | [{ element = X; multiplicity = 2}] -> 
      let x = toSym (species.[X].name)
      match kinetics with 
      | Kinetics.Deterministic -> c * x * x
      | _ -> (c/2) * x * (x-1)
  | aa -> aa 
          |> Lib.fold_left (fun acc entry -> 
            let x = toSym (species.[entry.element].name)
            match kinetics with 
            | Kinetics.Deterministic -> acc * (Expression.Pow (x,entry.multiplicity))
            | _ -> 
              let factor = [0.. entry.multiplicity-1] 
                            |> List.map (fun n -> x - n)
                            |> Expression.Product
              let coeff = Expression.One / (Lib.fac entry.multiplicity)
              acc * coeff * factor
          ) c

let toMoment kinetics (species:Species list) (rs:Reaction<int, float, _> list) (monomial:int []) =
  let momentFactor (r:Reaction<int, float, _>) spIndex (sp:Species) =
    let x = toSym sp.name
    let a = r.getStoich false spIndex
              |> int
              |> MathNet.Symbolics.Expression.FromInt32
    let m = Expression.FromInt32 monomial.[spIndex]
    (Power (x + a, m), Power (x, m))

  // calculate moment
  rs |> List.map 
          (fun r -> let h = propensity kinetics species r
                    let str4 = Infix.format <| Exponential.expand h
                    let (stoichFactors, factors)  = species
                                                      |> List.mapi (momentFactor r)
                                                      |> List.unzip
                    h * (Expression.Product stoichFactors - Expression.Product factors))
     |> Expression.Sum

let expectation expr = 
  let convert = Infix.format >> sprintf "E[%s]" >> toSym
  match splitCoefficient expr with
  | None,   Some f -> convert f
  | Some c, Some f -> c * (convert f)
  | Some c, None   -> c
  | None, None     -> Expression.Zero

let toMonomial (species:Species list) is = 
  is |> Array.mapi (fun i xi -> Power (toSym species.[i].name, Expression.FromInt32 xi) |> Exponential.expand)
                             |> Array.fold (*) Expression.One
                             |> (Infix.format >> (fun x-> "E[" + x + "]"))





let inline_monomials spIndex (monomials:int[] list) map_X1X2_locations (mc:Moment_closure_settings.monomial list) : (Expression.t<int> * int option) list =
  let inline_mon mon =
    mon 
    |> List.fold (fun (acc:int[]) (sp,count) -> acc.[spIndex sp] <- count; acc) (Array.create monomials.Head.Length 0)
    |> fun s -> List.findIndex ((=)s) monomials
  mc
  |> List.map (Expression.map inline_mon >> fun expr -> 
      match expr with 
      | Expression.Key i -> expr, Map.tryFind i map_X1X2_locations
      | _                -> expr, None
  )


// compute all moments of the given monomials
// allMoments is a list of (monomial, moment of x^monomial)
let generateMoments (crn:Crn) = 
  let crn = crn.saturate_initials()
  let env = Parameters.to_env crn.settings.parameters
  let populations,events = Initial<Species,Value>.to_initialpops_events env crn.settings.simulation.initial crn.initials
  let spIndex = populations.find_index 
  let reactions = 
    crn.reactions 
    |> Reaction.normalise_list
    |> List.map (Reaction.map spIndex (Expression.eval (Environment.find env)) id)

  // Setup moment equations and the closure approximation
  let species = populations.index_to_species |> Array.map (fun x -> x.species) |> List.ofArray
  let mc  = crn.settings.moment_closure |> Moment_closure_settings.saturate_plots species
  let monomials  = Moment_closure_settings.initAllOrders populations.index_to_species.Length mc.order
  let momentGenerator = toMoment crn.settings.simulation.kinetics
  let x = monomials |> List.map 
                        (momentGenerator species reactions 
                        >> Algebraic.expand 
                        >> Exponential.expand 
                        >> Algebraic.summands 
                        >> List.map expectation 
                        >> Expression.Sum)

  let map_X1X2_locations = 
    if crn.settings.moment_closure.order > 1
    then
      let isorder pos o (arr:int[]) = arr.[pos] = o && Array.sum arr = o
      List.init species.Length (fun i -> 
        let Ex = monomials |> List.findIndex (isorder i 1)
        let Ex2 = monomials |> List.findIndex (isorder i 2)
        Ex,Ex2
      )
      |> Map.ofList
    else
      Map.empty

  { name = crn.name
  ; equations      = List.zip monomials x
  ; log_approximations = None
  ; monomials      = monomials
  ; populations    = populations
  ; mc_settings    = { plots = mc.plots |> inline_monomials spIndex monomials map_X1X2_locations; order = mc.order; initial_minimum = mc.initial_minimum; log_evaluation=mc.log_evaluation }
  // TODO: Make a polymorphic create method to make this simpler
  ; ode_settings   = crn.settings.deterministic
  ; sim_settings   = { crn.settings.simulation with plots = monomials |> List.map (toMonomial species >> Species.create >> Key.Species >> Expression.Key) } 
  }

let c (m_upper:int[]) (m_lower:int[]) =
  let singleC (l, h) = 
    if l < h then 0
             else Lib.fac l / (Lib.fac (l - h) * Lib.fac h)
  Array.zip m_upper m_lower 
    |> Array.map singleC
    |> Array.fold (*) 1


let getCEq mbar ms (allMonomials: int[][]) = 
  // for all p \in {1 .. k}, where k is the number of monomials of order n:
  // C^{\bar m}_{m_s} = \Sum^k_{p=1} \gamma_p * C^{m_p}_{m_s}, 
  (c mbar ms |> double, allMonomials |> Array.map (fun mp -> c mp ms |> double))

let getGammas mbar (allMonomials: int[][]) =
  let eqSystem      = allMonomials |> Array.map (fun ms -> getCEq mbar ms allMonomials)
  let gammaMatrix   = Oslo.Matrix.ofArray (eqSystem |> Array.map snd)
  let gammaSolution = Oslo.Vector.ofArray (eqSystem |> Array.map fst)
  Oslo.Gauss.SolveCore gammaMatrix gammaSolution
    |> Oslo.Vector.toArray
    |> Array.map (fun x -> System.Math.Round(x))


let expressionToFloat expr = (expr |> Evaluate.evaluate (Dictionary.empty ())).RealValue

let approximateMonomial (doLog:bool) (species:Species list) (order:int) (allMonomials: int[][]) (mE:Expression) =
  
    match splitCoefficient mE with
    | _,  None -> mE, None
    | coeff, Some factor ->
        let clean_factor = 
            match factor with 
            | Identifier(Symbol x) -> x.Substring(2, x.Length-3) |> Infix.parseOrUndefined 
            | _ -> failwith "Expected variable of form E[_]"

        let deg = Polynomial.totalDegree clean_factor 
        if deg <> Expression.NegativeInfinity && expressionToFloat deg > float order
        then
            let mbar = 
                species 
                |> List.map (fun sp -> Polynomial.degree (toSym sp.name) clean_factor 
                                        |> Evaluate.evaluate (Dictionary.empty ()) 
                                        |> fun x -> x.RealValue 
                                        |> int
                )
                |> Array.ofList
            
            let gammas   = getGammas mbar allMonomials      

            let phi_mbar = Array.zip allMonomials gammas 
                            |> Array.map (fun (mu, gamma) -> Power (toMonomial species mu |> toSym, toApprox gamma) )
                            |> Array.toList
                            |> Product
                            |> Exponential.expand
            
            // Depending on whether we are
            if doLog
            then zero, Some ((match coeff with Some c -> expressionToFloat c | None -> 1.0), phi_mbar)
            else (match coeff with Some c -> c * phi_mbar | None -> phi_mbar), None

        else mE, None

let generateClosure moments = 
    let species = allSpecies moments
    let moment_array = List.toArray moments.monomials
    if moments.mc_settings.log_evaluation
    then 
        let new_equations, log_approximations = 
            moments.equations
            |> List.map (fun (mu, e) -> 
                let new_terms, log_terms = 
                    e 
                    |> Algebraic.summands
                    |> List.map (approximateMonomial true species moments.mc_settings.order moment_array)
                    |> List.unzip
                ( ( mu, new_terms |> Expression.Sum |> Algebraic.expand |> Exponential.expand )
                , ( mu, log_terms |> List.choose id)   // Don't sum up the log approximations
                )
            )
            |> List.unzip
        { moments with equations = new_equations; log_approximations = Some log_approximations }
    else
        let new_equations = 
            moments.equations
            |> List.map (fun (mu, e) -> 
                ( mu
                , e 
                  |> Algebraic.summands
                  |> List.map (approximateMonomial false species moments.mc_settings.order moment_array >> fst)
                  |> Expression.Sum 
                  |> Algebraic.expand 
                  |> Exponential.expand
                )
            )
        { moments with equations = new_equations; log_approximations = None }


let combine_equations sys = 
    match sys.log_approximations with
    | Some approxs -> approxs |> List.map2 (fun (k,eqn) (_,eqs) -> k, eqn + (eqs |> List.map (fun (a,b) -> a*b) |> Expression.Sum |> Algebraic.expand |> Exponential.expand)) sys.equations
    | None         -> sys.equations

let simulate_callback (cancel:bool ref) (output:Row<Point> -> unit) (sys:t) =
  let delta = sys.mc_settings.initial_minimum
  let species = allSpecies sys
  let species_values = sys.populations.index_to_species |> Array.map (fun sp -> match sp.value with 0.0 -> delta | _ -> sp.value)
  let values = 
    sys.monomials 
    |> List.map (fun m ->
        Array.fold2 (fun st power speciesValue -> st * speciesValue ** (float power)) 1.0 m species_values
    )
  let populations = 
    List.map2 (fun v m -> Population.create(false, v, (Species.create <| toMonomial species m))) values sys.monomials
    |> Populations.create

  let events = []
  let simulator = Simulation.create populations events sys.sim_settings 1.0
  let map_expected xs = 
    let dic = Dictionary.empty ()
    populations.index_to_species 
    |> Array.iter2 (fun x pop -> 
        Dictionary.add dic pop.species.name (FloatingPoint.Real (if x < delta then delta else x))
    ) (Oslo.Vector.toArray xs)
    dic

  let printFP fp formatter = 
      match fp with 
      | FloatingPoint.Real r -> sprintf "%f" r
      | FloatingPoint.Complex n -> n.ToString()
      | FloatingPoint.ComplexInf -> Expression.ComplexInfinity |> formatter
      | FloatingPoint.ComplexVector v -> "Complex vector"
      | FloatingPoint.ComplexMatrix m -> "Complex matrix"
      | FloatingPoint.NegInf -> Expression.NegativeInfinity |> formatter
      | FloatingPoint.PosInf -> Expression.PositiveInfinity |> formatter
      | FloatingPoint.RealVector v -> "Real vector"
      | FloatingPoint.RealMatrix m -> "Real Matrix"
      | FloatingPoint.Undef -> Expression.Undefined |> formatter

  let evaluate_equation t x eqn = 
      let dic = map_expected x
      let dxs = eqn 
                |> Algebraic.summands 
                |> List.map (fun x -> x |> Algebraic.factors
                                        |> List.map (Evaluate.evaluate dic))
      let dxi = eqn |> Evaluate.evaluate dic
      match dxi with
      | FloatingPoint.Real    f -> f
      | error -> failwith 
                    <| sprintf "Non-real value %s computed at time t = %f.\nOriginating equation:\n%s\nMoment values at time t:\n%s%s" 
                               (printFP error Infix.format)
                               t
                               (Infix.format eqn)
                               (dic |> Dictionary.toSeq
                                    |> Seq.toList
                                    |> List.map (fun (x,y) -> sprintf "%s = %s" x (printFP y Infix.format))
                                    |> String.concat "\n")
                               (if t = sys.sim_settings.initial
                                then "\n(try increasing initial minimum or avoid initialising species with value zero)"
                                else "")

  let log_approximations = 
      match sys.log_approximations with
      | Some approxs ->
          ( approxs |> List.map (fun (moment,terms) -> 
                moment, terms |> List.map (fun (coeff,expr) -> coeff, expr |> Expression.Ln |> Algebraic.expand |> Exponential.expand)
              )
          , fun t x (coeff,expr) -> coeff * (evaluate_equation t x expr |> exp)
          )
          |> Some
      | None -> None

  let f = fun (t:float) x -> 
    let exacts = sys.equations |> List.map (snd >> evaluate_equation t x) 
    match log_approximations with 
    | Some (approxs,evaluator) -> approxs |> List.map (snd >> List.sumBy (evaluator t x)) |> List.map2 (+) exacts
    | None                     -> exacts
    |> Array.ofList
    |> Oslo.Vector.ofArray

  (* Use these methods if returning floats *)
  (*let grab_data time concs = Oslo.Vector.toArray concs
  let interpolator = Row.interpolate_float*)
  
  (* Use these methods if returning Points *)
  let grab_data time (concs:Oslo.Vector) : Point[] = 
    sys.mc_settings.plots
    |> List.map (fun (expr,x2loc) -> 
      let mean = Expression.eval (fun i -> concs.[i]) expr
      let stdev = 
        match x2loc with 
        | Some j -> 
          let variance = concs.[j] - mean*mean
          if variance < 0.0
          then  // The moment closure approximation does not yield a sensible estimate of variance. 
                // As this calculation is purely for visualization, we'll fix it to zero.
                // TODO: If a WARNING mechanism is implemented, then this is a good fit, using the commented out code below.
            (*let toMonomial (species:Species list) is = 
              is 
              |> Array.mapi (fun i xi -> Power (toSym species.[i].name, Expression.FromInt32 xi) |> Exponential.expand)
              |> Array.fold (*) Expression.One
              |> Infix.format
            let species = sys.populations.index_to_species |> Array.map (fun pop -> pop.species) |> List.ofArray
            let printExpression i : string = sys.monomials.[i] |> toMonomial species
            let involved = Expression.to_string printExpression expr
            failwithf "The moment closure approximation of the variance for monomial %s has gone negative" involved*)
            0.0
          else sqrt variance
        | None -> 0.0 
      Point.create mean stdev
    ) 
    |> Array.ofList
  
  (* Initialise the simulator and then simulate *)
  let interpolator = Point.interpolate  
  let concs = values |> Array.ofList |> Oslo.Vector.ofArray
  let integrator = Oslo_integrator.create sys.name simulator grab_data interpolator sys.ode_settings concs f
  Oslo_integrator.simulate_callback cancel output integrator

let process_simulation sim_data sys = 
  let times:float list = sys.sim_settings.times
  //let plots:string list = List.map (Expression.to_string (Key.to_string Species.to_string)) sys.sim_settings.plots
  let toMonomial (species:Species list) is = 
    is 
    |> Array.mapi (fun i xi -> Power (toSym species.[i].name, Expression.FromInt32 xi) |> Exponential.expand)
    |> Array.fold (*) Expression.One
    |> (Infix.format >> (fun x-> "E[" + x + "]"))
  let species = sys.populations.index_to_species |> Array.map (fun pop -> pop.species) |> List.ofArray
  let printExpression i : string = sys.monomials.[i] |> toMonomial species
  
  let plots = sys.mc_settings.plots |> List.map (fst >> Expression.to_string printExpression)
  if List.isEmpty times then
      Table.from_rows_reverse plots sim_data
  else 
      let int_data = Row.interpolate_reverse Point.interpolate (List.rev sim_data) times
      Table.from_rows_reverse plots int_data

let simulate (sys:t) = 
  let cancel = ref false
  let result = ref []
  let output row = result := row::!result
  let final_sim = simulate_callback cancel output sys
  final_sim, process_simulation !result sys
