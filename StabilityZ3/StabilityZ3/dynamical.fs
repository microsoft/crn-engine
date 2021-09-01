// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.Biology.StabilityZ3
module MathNetWrapper = Microsoft.Research.Biology.StabilityZ3.MathNetWrapper

//open Microsoft.Research.Biology
//open IronPython.Hosting
open Python.Runtime
open FSharp.Interop.Dynamic
open Microsoft.Research.CRNEngine

(* Notes:
    [1] Removing parameter constraints: to set a parameter value, use substitution with Float x
    [2] When state variables are replaced using mass conservation laws we run into problems with multistability analysis. 
        Since the two stable states are unique, this allows for differences in substituted variables, which are no longer part of the state.
        As a temp fix, we store conservation law variables and substituted state variables in the consLaws array.
*)

module PythonWrapper = 
    
    let format_eqns eqns = eqns |> String.concat ", " |> sprintf "[%s]" 
    
    let trim_solution (sol:string) = sol.TrimStart('(').TrimEnd(')').TrimEnd(',')

    let nonlinsolve (eqns:(string*string) []) vars = 
        use gil = Py.GIL()
        let symbol = Py.Import("sympy.core.symbol")
        let solveset = Py.Import("sympy.solvers.solveset")

        let symbs = vars |> String.concat "," |> symbol?symbols |> PyTuple.AsTuple
        let t_map = vars |> List.mapi (fun i s -> s, symbs.Item(i)) |> Map.ofList
        
        let remove_var = eqns |> Array.map (fun (x,_) -> t_map.[x])    // Not sure why, but Arrays work, Lists don't.
        let remove_rhs = eqns |> Array.map snd
        let py_locals = new PyDict ()
        t_map |> Map.iter (fun s v -> py_locals.SetItem(s, v))
        let py_eqns = PythonEngine.Eval (format_eqns remove_rhs, System.Nullable(), System.Nullable(py_locals.Handle))
        
        let solution = 
            if eqns.Length > 1 
            then 
                let solutions = solveset?nonlinsolve(py_eqns,remove_var)
                let first_solution = (solutions|> PyList.AsList).Item(0)
                eqns |> Array.mapi (fun i (v,_) -> v, (first_solution.Item(i).ToString()))
            else 
                let res = solveset?nonlinsolve(py_eqns,remove_var.[0]) |> PyTuple.AsTuple
                [|fst eqns.[0], res.Item(0).Item(0).ToString()|]

        solution

type uniform = { min : float; max : float }
type normal = { mu : float; sigma : float }
type truncated_normal = { mean : float; stdev : float; min : float; max : float }
type distribution = Fixed of float | Uniform of uniform | Normal of normal | TruncatedNormal of truncated_normal | LogUniform of uniform | LogNormal of normal | LogTruncatedNormal of truncated_normal
//open Microsoft.Research.Biology.TuringZ3

type BenchmarkInfo = 
    { smt  : string
    ; time : float //sec
    }
    static member empty = {smt = ""; time = -1.0}

type AnalysisResult = 
    | SAT of Map<string, float>*BenchmarkInfo //map of variables to values (note that solution values are stored as floats and therefore the infinite precision results are approximated)
    | UNSAT of BenchmarkInfo            
    | UNKNOWN of BenchmarkInfo          
    | FAILED of string*BenchmarkInfo    //error msg
    | INCOMPLETE                //analysis has not been performed

    static member toString (r:AnalysisResult) = 
        match r with 
        | SAT (_,t) -> sprintf "%f,SAT" t.time
        | UNSAT t   -> sprintf "%f,UNSAT" t.time
        | UNKNOWN t -> sprintf "%f,UNKNOWN" t.time
        | FAILED (s,t) -> sprintf "%f,FAILED:%s" t.time s
        | INCOMPLETE   -> sprintf "-1.0, Not analyzed"

    override this.ToString() = AnalysisResult.toString this

    member this.Time = 
        match this with
        | SAT (_,t) -> t.time
        | UNSAT t   
        | UNKNOWN t -> t.time
        | FAILED (_,t) -> t.time
        | INCOMPLETE   -> -1.0

    member this.Status = 
        match this with
        | SAT _     -> "SAT"
        | UNSAT _   -> "UNSAT"
        | UNKNOWN _ -> "UNKNOWN"
        | FAILED _  -> "FAILED"
        | _         -> "OTHER"
    member this.SMT = 
        match this with
        | SAT (_,t) 
        | UNSAT t   
        | UNKNOWN t -> t.smt
        | FAILED _ 
        | _        -> ""
    member this.isSAT = 
        match this with
        | SAT _ -> true
        | _ -> false
    member this.isUNSAT = 
        match this with
        | UNSAT _ -> true
        | _ -> false

    static member from_string (str:string) =
          match str with
          | "SAT"   -> SAT (Map.empty, BenchmarkInfo.empty)
          | "UNSAT" -> UNSAT BenchmarkInfo.empty
          | "UNKNOWN" -> UNKNOWN BenchmarkInfo.empty
          | "FAILED" -> FAILED ("", BenchmarkInfo.empty)
          | s -> 
            if s.Contains "FAILED" then
                let br = s.IndexOf(":")
                FAILED (s.[br+1..], BenchmarkInfo.empty)
            else
                failwithf "Unknown result type: %s" s

type Dynamical = 
    { name          : string
    ; odes          : (string * NumExpr) [] //state variable -> ode
    ; initials      : Map<string, float>
    ; diffusion     : Map<string, NumExpr> //state variable -> diffusion rate
    ; pars          : Map<string, float>
    ; bounds        : Map<string, float option * float option>
    ; distributions : Map<string, distribution>
    ; csts          : BoolExpr []
    ; solution      : AnalysisResult
    ; consLaws      : string[]              //variables representing conservation laws (see note 2)
    ; farkas        : int[][]
    }
    override this.ToString() = this.ToText false false true
    
    static member toString (S:Dynamical) = S.ToString()

    member this.ToEquationsText simplify latex = 
        let ode = this.odes
        let ode = if simplify then ode |> Array.map(fun (x,e) -> (x, e |> MathNetWrapper.SimplifyNum)) else ode
        let ode_str = 
            if latex 
            then ode |> Array.map (fun (x,e) -> sprintf "\\frac{d%s}{dt} &= %s" x (ExpressionFunctions.ToText true e)) |> String.concat "\\\\"                
            else ode |> Array.map (fun (x,e) -> sprintf "d%s/dt = %s" x (ExpressionFunctions.ToText false e)) |> String.concat "\n"

        if latex then 
            sprintf "$$\\begin{align}%s\\end{align}$$" ode_str
        else
            ode_str

    member this.ToEquationsSplit simplify latex =
        let ode_str = this.ToEquationsText simplify latex
        let Jstr = 
            if simplify 
            then this.J () |> Matrix.map MathNetWrapper.SimplifyNum 
            else this.J ()
            |> Matrix.toText latex

        let Dstr = Expression.Key "w" |> this.D |> Matrix.toText latex

        let cst = if simplify then this.csts |> Array.map MathNetWrapper.Simplify else this.csts
        let cst_str = 
             cst
             |> Array.map (fun e -> e |> ExpressionFunctions.BToLatex |> sprintf "$$%s$$") 
             |> String.concat "\n"
        ode_str,Jstr,Dstr,cst_str

    member this.ToText simplify latex diffusion =
        let ode_str,Jstr,Dstr,cst_str = this.ToEquationsSplit simplify latex
        if latex 
        then
            if diffusion then sprintf "%s\n$$J=%s$$\n$$D=%s$$\n%s" ode_str Jstr Dstr cst_str
            else sprintf "%s\n$$J=%s$$\n$$\n%s" ode_str Jstr cst_str
        else
            if diffusion then sprintf "Equations\n%s\n\nJacobian\n%s\n\nDiffusion matrix\n%s" ode_str Jstr Dstr
            else sprintf "Equations\n%s\n\nJacobian\n%s" ode_str Jstr
    

    static member Create(ode, diff_species) = 
        { name      = ""
        ; odes      = ode
        ; initials  = Map.empty
        ; diffusion = diff_species |> List.map(fun d -> d, sprintf "D_{%s}" d |> Expression.Key) |> Map.ofList
        ; bounds    = Map.empty
        ; distributions = Map.empty
        ; pars      = Map.empty
        ; csts      = [||]
        ; solution  = INCOMPLETE 
        ; consLaws  = [||]
        ; farkas    = [||]
        }
    static member Create(ode, diff_rates) = 
        { name      = ""
        ; odes      = ode
        ; initials  = Map.empty
        ; diffusion = diff_rates
        ; bounds    = Map.empty
        ; distributions = Map.empty
        ; pars      = Map.empty
        ; csts      = [||]
        ; solution  = INCOMPLETE        
        ; consLaws  = [||]
        ; farkas    = [||]
        }   
    (*
    static member Create(ode, d) = 
        { odes       = ode |> Map.ofSeq
        ; diffusable = d |> Set.ofSeq
        ; pars      = Map.empty
        ; bounds    = Map.empty
        ; csts      = List.empty
        ; solution  = None
        ; diff_diff = true
        }

    static member Create(ode, d) = 
        { odes       = ode |> Map.map(fun _ e -> NumExpr.Parse e)
        ; diffusable = d |> Set.ofSeq
        ; pars     = Map.empty
        ; bounds    = Map.empty
        ; csts      = List.empty
        ; solution  = None
        ; diff_diff = true
        }

    static member Create(ode, d) = 
        { odes       = ode |> Seq.map(fun (v,e) -> v, NumExpr.Parse e) |> Map.ofSeq
        ; diffusable = d |> Set.ofSeq
        ; pars     = Map.empty
        ; bounds    = Map.empty
        ; csts      = List.empty
        ; solution  = None
        ; diff_diff = true
        }
    
    static member Create(ode, d, pars) = 
        { odes       = ode |> Map.map(fun _ e -> NumExpr.Parse e)
        ; diffusable = d |> Set.ofSeq
        ; pars     = pars
        ; bounds    = Map.empty
        ; csts      = List.empty
        ; solution  = None
        ; diff_diff = true
        }
    *)
//    member this.SetParam p = 
//        {this with pars = p}

    member this.Dim = this.odes.Length

    member this.State = this.odes |> Array.map fst
    
    member this.Eqns = this.odes |> Array.map snd
    
    member this.SetLB (s:string) (v:float) = 
        let (lb, ub) = 
            if this.bounds.ContainsKey s then
                this.bounds.[s]
            else None, None
        let lb' = Some v
        let bounds' = this.bounds.Remove(s).Add(s, (lb',ub))        
        {this with bounds = bounds'}
    static member setLB (s:string) (v:float) (S:Dynamical) = S.SetLB s v

    member this.SetUB (s:string) (v:float) = 
        let (lb, ub) = 
            if this.bounds.ContainsKey s then
                this.bounds.[s]
            else None, None
        let ub' = Some v
        let bounds' = this.bounds.Remove(s).Add(s, (lb,ub'))        
        {this with bounds = bounds'}
    static member setUB (s:string) (v:float) (S:Dynamical) = S.SetUB s v


    member this.SetAllUB (v:float) = 
        let vars = this.Eqns |> Array.map(fun e -> ExpressionFunctions.GetVars e) |> Array.reduce (+)
        vars
        |> Set.toArray
        |> Array.fold (fun (acc:Dynamical) var -> acc.SetUB var v) this
    static member setAllUB (v:float) (S:Dynamical) = S.SetAllUB v
    
    member this.SetAllLB (v:float) = 
        let vars = this.Eqns |> Array.map(fun e -> ExpressionFunctions.GetVars e) |> Array.reduce (+)
        vars
        |> Set.toArray
        |> Array.fold (fun (acc:Dynamical) var -> acc.SetLB var v) this
    static member setAllLB (v:float) (S:Dynamical) = S.SetAllLB v

    member this.SetName (name:string) = { this with name = name }
    static member setName (s:string) (S:Dynamical) = S.SetName s

    static member setBounds (s:string) (lb:float) (ub:float) (S:Dynamical) = 
        S
        |> Dynamical.setLB s lb
        |> Dynamical.setUB s ub

    static member setParam (s:string) (v:float) (S:Dynamical) = 
        [s, Expression.Float v]
        |> Map.ofSeq
        |> S.SubstParam       
    
    static member setDistribution (s:string, d:distribution) (S:Dynamical) = { S with distributions = S.distributions.Add (s,d) }
    static member setInitials initials S : Dynamical = { S with initials = initials }

    member this.AddCst (e:BoolExpr) = {this with csts = Array.append this.csts [|e|]}
    
    static member addCst e (S:Dynamical) = S.AddCst e
        
    //Jacobian matrix
    member this.J () = 
        let n = this.Dim
        let x, eqns = this.odes |> Array.unzip        
        Array.init n (fun i -> Array.init n (fun j -> ExpressionFunctions.differentiate eqns.[i] x.[j]))
        |> Matrix.Create

    //Diffusion matrix
    member this.D w = 
        let n = this.Dim
        let x, eqns = this.odes |> Array.unzip        
                        
        Array.init n (fun i -> 
            Array.init n (fun j -> 
                if i = j && this.diffusion.ContainsKey x.[i] then 
                    match this.diffusion.[x.[i]] with
                    | Expression.Float 1.0 -> w
                    | _ -> this.diffusion.[x.[i]] * w
                else
                    Expression.Float 0.0
            )
        )
        |> Matrix.Create
     
    //member this.D = this.D (Var "w")
        
                      
    //parameter value constraints
//    member this.ParamCst =         
//        if this.pars.Count = 0 then
//            None
//        else
//            this.pars
//            |> Map.toArray
//            |> Array.map(fun (v, v') -> Eq(Var v, Float v'))
//            |> And
//            |> Some

    member this.BoundCst =          
        this.bounds
        |> Map.toArray
        |> Array.collect (fun (v, (lb, ub)) -> 
            let lb_cst = 
                match lb with 
                | Some v' -> Some (Expression.BGeq(Expression.Key v, Expression.Float v'))
                | None -> None

            let ub_cst = 
                match ub with 
                | Some v' -> Some (Expression.BLeq(Expression.Key v, Expression.Float v'))
                | None -> None
            [|lb_cst; ub_cst|] |> Array.choose id
        )    

    member this.Div0Cst = 
        this.odes
        |> Array.map(fun (_,e) -> ExpressionFunctions.GetDenoms e )
        |> Set.unionMany        
        |> fun s -> 
            if Set.isEmpty s then   
                [||]
            else
                s
                |> Set.toArray
                |> Array.map (fun e -> Expression.BNot (Expression.BEq(e, Expression.Float 0.0)))//printfn "WARNING: Including div0 constraint for %s" (ExpressionFunctions.ToText false e)                    
                

    //system constraints: bounds, param values and div0
    member this.SysCst = Array.concat [this.csts; this.BoundCst; this.Div0Cst]

    //equilibrium constraints
    member this.EqCst =         
        let eq = this.Eqns |> Array.map(fun e -> Expression.BEq(e, Expression.Float 0.0))
        Array.append eq this.SysCst

    member this.Subst (p:Map<string,string>) = 
        let p' = p |> Map.map(fun _ x -> Expression.Key x)
        let sub x = if p.ContainsKey x then p.[x] else x
        let arraySub m = m |> Array.map(fun (x, eq:NumExpr) -> sub x, Expression.substitute p' eq)
        
        let odes = this.odes |> arraySub
        let diffusion = this.diffusion |> Map.toArray |> arraySub |> Map.ofArray
        //let pars = this.pars |> Map.toArray |> Array.map(fun (x,v) -> sub x, v) |> Map.ofArray
        let bounds = this.bounds |> Map.toArray |> Array.map(fun (x,v) -> sub x, v) |> Map.ofArray
        let cst = this.csts |> Array.map (fun eq -> Expression.substituteBool p' eq)
        
        { name       = this.name
        ; odes       = odes
        ; initials   = this.initials
        ; diffusion  = diffusion        
        ; bounds     = bounds
        ; distributions = this.distributions
        ; pars       = this.pars
        ; csts       = cst
        ; solution   = INCOMPLETE //destroying previousl solutions
        ; consLaws   = this.consLaws
        ; farkas     = Array.empty
        }


    member this.SubstParam (p:Map<string, NumExpr>) = 
        let arraySub m = m |> Array.map(fun (x, eq:NumExpr) -> x, Expression.substitute p eq)
        
        let odes = this.odes |> arraySub
        let diffusion = this.diffusion |> Map.toArray |> arraySub |> Map.ofArray
        let cst = this.csts |> Array.map (Expression.substituteBool p)
        
        let S = 
            {this with  
                  odes       = odes
                ; diffusion  = diffusion                
                ; solution   = INCOMPLETE //destroying previousl solutions
            }

        let vars = S.GetVars |> Set.ofSeq
        let bounds = 
            this.bounds
            |> Map.filter (fun k _ -> vars.Contains k)

        {S with bounds = bounds}


    member this.ReduceState (p: Map<string,NumExpr>) =
        let newOdes = 
            this.odes            
            |> Array.filter (fun (k,_) -> not (p.ContainsKey k))
            |> Array.map (fun (k,e) -> k, Expression.substitute p e)                        
        
        let newDiff = 
            this.diffusion
            |> Map.filter (fun k _ -> not (p.ContainsKey k))
        
        (*let newCst = 
            this.csts 
            |> List.map (Expression.substituteBool p)        
            |> List.append (p |> Map.toList |> List.map(fun (v,x) -> Expression.BEq(Expression.Key v, x)))*)

        //let S = Dynamical.Create(new_odes |> Map.toArray, new_diff)
        //let vars = S.GetVars |> Set.ofSeq    
        //let new_bounds = this.bounds |> Map.filter (fun k _ -> vars.Contains k)

        { name       = this.name
        ; odes       = newOdes
        ; initials   = this.initials
        ; diffusion  = newDiff
        ; bounds     = Map.empty    //this.bounds
        ; distributions = Map.empty
        ; pars       = Map.empty
        ; csts       = this.csts    //newCst
        ; solution   = INCOMPLETE //destroying previousl solutions
        ; consLaws   = Array.empty
        ; farkas     = Array.empty
        }
                
    static member reduceState (p: Map<string,NumExpr>) (S:Dynamical) = S.ReduceState p

    //do not replace variables (equation constraints need to be introduced in this case)
    static member reduceStateWR (p: Map<string,NumExpr>) (S:Dynamical) = 
        let newOdes = S.odes |> Array.filter (fun (k,_) -> not (p.ContainsKey k))                                            
        let newCst = S.csts |> Array.append (p |> Map.toArray |> Array.map (fun (v,x) -> Expression.BEq(Expression.Key v, x)))
        { S with odes = newOdes; csts = newCst; solution = INCOMPLETE } // Destroy previous solution

    member this.GetDiffVars = 
        if Map.isEmpty this.diffusion then Set.empty
        else
            this.diffusion 
            |> Map.toArray 
            |> Array.map(fun (_, e) -> ExpressionFunctions.GetVars e) 
            |> Array.reduce (+)

    //note that the wave number variable is not included
    member this.GetVars =
        //agregate all equation variables
        let main_vars = 
            this.Eqns
            |> Array.map(fun e -> ExpressionFunctions.GetVars e)
            |> Array.reduce (+)            
        
        let state_vars = this.State |> Set.ofSeq

        let cst_vars = 
            if this.csts |> Array.isEmpty then Set.empty
            else
                this.csts
                |> Array.map (fun e -> ExpressionFunctions.BGetVars e)
                |> Array.reduce (+)
                 
        let bound_vars = 
            let B = this.bounds |> Map.toArray
            if B |> Array.isEmpty then Set.empty
            else
                B
                |> Array.map fst
                |> Set.ofArray

        bound_vars + cst_vars + state_vars + main_vars + this.GetDiffVars


    //use the values from the solution to simplify the equations
    member this.MkConcrete = 
        match this.solution with        
        | SAT (s,_) -> 
            s 
            |> Map.map (fun _ v -> Expression.Float v)
            |> this.SubstParam
        | _ -> this
                                
    //use the values from the solution to simplify the equations but preserve state variables
    member this.MkConcreteParams = 
        match this.solution with        
        | SAT (s,_) -> 
            let S = this.State |> Set.ofArray
            s 
            |> Map.map(fun _ v -> Expression.Float v)
            |> Map.filter (fun k _ -> not (S.Contains k))
            |> this.SubstParam
        | _ -> this

    (* Topology (of Jacobian): 
        - 0 if no interaction 
        - 1 if there is any interaction (positive or negative)
        - NaN for remaining symbolic expressions
    *)
    member this.Topology () = 
        (this.MkConcreteParams.J ()).values
        |> Array.map(fun row -> 
            row.values
            |> Array.map (fun x -> 
                 match x with
                 | Expression.Float k -> if k<>0.0 then 1.0 else 0.0
                 | _ -> System.Double.NaN))        
                    
    (* Topology (of Jacobian): 
        - 0 if no interaction 
        - 1 if there is a positive interaction
        - -1 if there is a negative interaction
        - NaN for remaining symbolic expressions
    *)
    member this.Network () = 
        (this.MkConcreteParams.J ()).values
        |> Array.map(fun row -> 
            row.values
            |> Array.map (fun x -> 
                 match x with
                 | Expression.Float k -> 
                    if k>0.0 then 1.0 
                    elif k<0.0 then -1.0
                    else 0.0
                 | _ -> System.Double.NaN))

    
    member this.PrintSolution =
        match this.solution with
        | SAT (v,_) -> 
            v
            |> Map.toArray
            |> Array.map(fun (v,f) -> sprintf "%s\t= %.2f;" v f)
            |> String.concat "\n"
            |> printfn "\nParameters:\n%s"
        | _ -> () //printfn "No solution"

    (*
        Group similar equation terms and replace constants
        if add_cst is true, the equality constraints for the new varialbes are included
        NOTE: previous constraints on parameters could be ignored once the variables are merged and substituted
    *)
    member this.Group add_cst =     
        MathNetWrapper.var_cnt <- 0 //reset fresh variable counter
        let state = this.State |> Set.ofArray    
        let mutable cst = [||]
        let mutable newConsLaws = [||]

        let CL = this.consLaws |> Set.ofArray

        let odes = 
            this.odes 
            |> Array.map (fun (sp,e) -> 
                let e', c, vars = MathNetWrapper.Group state e

                //hanlde substituted state variables (conservation laws)
                let cl = 
                    vars
                    |> Map.toArray
                    |> Array.filter(fun (_, e) -> Set.intersect (ExpressionFunctions.GetVars e) CL |> Set.isEmpty |> not)
                    |> Array.map fst
                newConsLaws <- Array.append cl newConsLaws

                cst <- Array.append cst c
                sp, e'
            )
        let csts = 
            if add_cst 
            then Array.append this.csts cst
            else this.csts
        
        {this with odes = odes; csts = csts; consLaws = newConsLaws |> Array.append this.consLaws}

    static member group flag (S:Dynamical) = S.Group flag

    static member odesFromCrn (crn:Crn) =
        // find the set of constant species
        let constants = 
            crn.initials 
            |> List.filter (fun i -> i.constant)
            |> List.map (fun i -> i.species.name)
            |> List.distinct
        
        // initialize the equations to 0 for all species
        let initialGrads = crn.all_species () |> List.fold (fun (m:Map<string, NumExpr>) x -> m.Add(x.name, Expression.zero)) Map.empty

        // Map a functional rate to a pure expression
        let rates = crn.settings.rates
        let rec translateFunctional (k:Key<Species>) =
          match k with
          | Key.Parameter p -> Expression.Key p 
          | Key.Species sp  -> Expression.Key sp.name
          | Key.Time        -> failwith "Can't convert 'time' to a pure expression"
          | Key.Rate r      -> 
              match Map.tryFind r rates with
              | None      -> failwith <| "Rate " + r + " is undefined."
              | Some rate -> Expression.expand translateFunctional rate
      
        let fe (e:Functional) : NumExpr = Expression.expand translateFunctional e

        // Map a parameter to a MathNet expression
        let fv (v:Microsoft.Research.CRNEngine.Value) = v
        
        crn.reactions     
        |> Reaction.normalise_list                                                // expand reverse reactions
        |> List.map (fun r -> Reaction.map (fun (sp:Species) -> sp.name) fv fe r) // convert reaction rate to Symbolics
        |> List.fold (fun (ode:Map<string, NumExpr>) r -> 
              let power = r.reactants 
                          |> List.map (fun m -> Expression.Power { base_=Expression.Key m.element; exponent=Expression.Float (float m.multiplicity)})
                          |> fun ps -> match ps with [] -> Expression.one | _ -> Expression.Times ps
              (ode, r.allSpecies)
              ||> List.fold 
                    (fun oldGradients sp ->  
                      let isConstant = constants |> List.exists (fun x -> x = sp)
                      let stoichiometry = r.getStoich isConstant sp
                      let stoichVal = Expression.Float stoichiometry
                      if stoichiometry <> 0.0
                      then 
                          let factor = 
                              match r.rate with
                              | Rate.MassAction rate -> Expression.Times [ stoichVal; rate; power ]
                              | Rate.Function fRate  -> Expression.Times [ stoichVal; fRate ]
                          
                          oldGradients.Add(sp, oldGradients.[sp] + factor) 
                      else oldGradients)
              |> Map.map (fun _ v -> Expression.simplify v)
          ) initialGrads

    static member fromCRN crn =                            
        //let odes, S, species, diff, parameters, initials = crn_str |> SymbolicODEs.ofCrnString
        let parsed_odes = crn |> Dynamical.odesFromCrn |> Map.toArray
        let S, species = SymbolicODEs.Stoich crn

        // Diffusion rates
        let diff = 
            crn.settings.spatial.diffusibles
            |> List.map(fun (s,r) -> 
                  match s with              
                  | Expression.Key (Key.Species sp) -> sp.name, r
                  | _ -> failwith "Only species were expected in diffusible definition")
            |> Map.ofSeq  

        // Initial conditions
        let parameters = crn.settings.parameters
        let env = parameters |> Parameters.to_env
        let pops, _ = 
            (crn.saturate_initials ()).initials
            |> Initial<Species,Value>.to_initialpops_events env crn.settings.simulation.initial
        let initials = 
            pops.index_to_species 
            |> Array.map (fun pop -> pop.species.name, pop.value) 
            |> Map.ofArray

        if (parsed_odes |> Array.map fst |> Set.ofArray) <> (species |> Set.ofList) 
        then failwith "Set of species was not consistent when converting CRN"

        //let parsed_odes = odes |> Array.map(fun (sp,e) -> sp, e |> ExpressionFunctions.Parse)
        
        let diffusibles = 
            diff
            |> Map.filter(fun _ v -> v <> Microsoft.Research.CRNEngine.Expression.Float 0.0)
            |> Map.map (fun k v ->
                match v with 
                | Microsoft.Research.CRNEngine.Expression.Float f -> Expression.Float f
                | Microsoft.Research.CRNEngine.Expression.Key   k -> Expression.Key k
                | _ -> failwithf "Don't recognise assignment for diffusible %s" k
            )

        let range p = 
            match p.prior with
            | Some prior -> 
                let lb, ub = 
                    match prior.distribution with
                    | Distribution.Uniform nd         -> nd.min, nd.max
                    | Distribution.TruncatedNormal tn -> tn.min, tn.max
                    | Distribution.Normal n           -> n.mean - 3.0*n.stdev, n.mean + 3.0*n.stdev
                    | Distribution.LogNormal n        -> exp (n.mu - 3.0*n.sigma), exp (n.mu + 3.0*n.sigma)
                if lb<ub then 
                    Some(p.name, lb, ub)
                else
                    None
            | None -> None     

        let distribution p =
            let d = 
                match p.prior with
                | Some prior -> 
                    match prior.distribution with
                    | Distribution.Uniform nd         -> { min = nd.min; max = nd.max} |> (if prior.interval = Interval.Log then LogUniform else Uniform)
                    | Distribution.TruncatedNormal tn -> { min = tn.min; max = tn.max; mean = tn.mean; stdev = tn.stdev } |> (if prior.interval = Interval.Log then LogTruncatedNormal else TruncatedNormal)
                    | Distribution.Normal n           -> { mu = n.mean; sigma = n.stdev} |> (if prior.interval = Interval.Log then LogNormal else Normal)
                    | Distribution.LogNormal n        -> { mu = n.mu; sigma = n.sigma } |> LogNormal
                | None -> Fixed p.value
            p.name, d                                    

        let sys = Dynamical.Create(parsed_odes,diffusibles) |> Dynamical.setInitials initials
        let vars = Set.toList sys.GetDiffVars
        
        let baseSys = 
            sys
            |> Dynamical.setAllLB 0.0 
            |> List.foldBack (fun v -> Dynamical.setLB v 0.0) vars
            |> List.foldBack (fun (name,lb,ub) -> Dynamical.setBounds name lb ub) (parameters |> List.choose range)
            |> List.foldBack (distribution >> Dynamical.setDistribution) parameters
        
        { baseSys with 
            farkas = Invariants.FarkasArray S ; 
            pars   = parameters |> List.map(fun p -> p.name, p.value) |> Map.ofSeq; 
        }
        
    static member SubstituteConservation S =
        // Check that no conservation laws involve diffusible species
        S.farkas
        |> Array.iter (fun cl ->
            let clstr = Array.map2 (fun n s -> if n<>0 then sprintf "%i%s" n s |> Some else None) cl S.State |> Array.choose id |> String.concat " + "
            Array.iter2 (fun c x -> 
                if (c<>0) && (S.diffusion.ContainsKey x)
                then failwithf "Conservation law [%s] includes diffusible species %s" clstr x 
            ) cl S.State;
        )

        // New approach, based on Gauss-Jordan elimination
        // Start by removing zero columns (which should include ALL diffusible species, plus some more)
        let conLocs,conSp = 
            Array.fold (fun acc (i,sp) -> 
                if Array.exists (fun x -> x<>0) (S.farkas |> Array.map (fun cl -> cl.[i]))
                then (i,sp) :: acc
                else acc
            ) [] (Array.indexed S.State)
            |> List.rev
            |> Array.ofList
            |> Array.unzip
        let nonzeroConsLaws = 
            S.farkas 
            |> Array.map (fun cl -> Array.map (fun i -> float cl.[i]) conLocs) 
            |> MathNet.Numerics.LinearAlgebra.DenseMatrix.ofRowArrays
        let r = nonzeroConsLaws.Rank ()
        let A = nonzeroConsLaws.ToRowArrays().[0..r-1]
        let C = Array.init r (fun i -> sprintf "cons_%i" i)
        let A', b' = Invariants.GaussJordanElimination A (Array.map Expression.Key C)    // Produces A' in reduced row echelon form
        let p = Invariants.ReplacementRules conSp A' b'
                
        // Create a new system
        let newSys = S |> Dynamical.reduceState p
        let substVars = p |> Map.toArray |> Array.map fst |> Array.append C
        {newSys with consLaws = substVars} |> Dynamical.setAllLB 0.0

    static member parse_ODEs odes = Array.map (fun (k,e) -> k,e |> ExpressionFunctions.Parse) odes

    /// Reduce the model by applying equilibrium approximations to all non-specified species. 
    ///   This uses Python.Net
    member this.ModelReduceKeep remain_list = 
        
        let odes = this.odes |> Array.map (fun (a,b)->a,(ExpressionFunctions.ToText false b).Replace("^","**"))
        let remain = Set.ofList remain_list
        let species = this.State |> Set.ofArray

        let _,missing = Set.partition species.Contains remain
        if Set.count missing > 0 then failwithf "Model reduction error: Cannot keep {%s} as they do not exist in the supplied dynamical system" (String.concat "," missing)

        let odes_to_remain, odes_to_remove = Array.partition (fun (a,_)->List.exists (fun l->a=l) remain_list) odes
        let remove_arr = species - remain |> Set.toArray

        if Array.isEmpty odes_to_remove then failwith "Model reduction error: Method cannot be applied when all species are diffusible"
        
        let symbstrs = (this.GetVars - this.GetDiffVars) |> Set.toList
        System.Console.Error.Write "Performing model reduction..."
        let t = System.DateTime.Now
        let solution = PythonWrapper.nonlinsolve odes_to_remove symbstrs
        System.Console.Error.WriteLine("done ({0} sec)", ((System.DateTime.Now-t).TotalSeconds))
        
        let species_replace = solution |> Array.map (fun (x,sol) -> String.concat "" ["(";sol;")"])
        let replace_rule = Array.zip remove_arr species_replace
        let expr_replacements = replace_rule |> Map.ofArray |> Map.map (fun _ rhs -> ExpressionFunctions.Parse rhs)
        
        Dynamical.Create (Dynamical.parse_ODEs odes_to_remain, this.diffusion)
        |> Dynamical.reduceState expr_replacements
        |> Dynamical.setAllLB 0.0, expr_replacements

    /// Reduce the model by applying equilibrium approximations to non-diffusible species. 
    ///   This uses Python.Net
    member this.ModelReduce () =        
        if Map.isEmpty this.diffusion 
        then failwith "Model reduction error: default method cannot be applied to systems with no diffusibles. Instead, specify which species to keep, using ModelReduceKeep."
        let remain_list = this.diffusion |> Map.toList |> List.map fst
        this.ModelReduceKeep remain_list



(*let enumerate_bifurcation_equilibria2 eqns variables jac = 
  printfn "\nBifurcation equilibria: "      
  printfn "-----------"
  let eq = equilibrium_constraint eqns
  let stab = bifurcation_constraint2 jac
  enumerate_solutions [| eq; stab |] variables*)

(*
let stable A =   
  characteristic_polynomial A  
  |> Vector.skip 1       
  |> routh_hurwitz
  |> fst
  |> Array.map (fun a -> Gt a zero)     
 
let unstable A =   
    A
    |> stable
    |> Array.map(fun x -> z3.MkNot(x)) 
    |> z3.MkOr
*)
   

