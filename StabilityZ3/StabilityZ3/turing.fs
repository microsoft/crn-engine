// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.TuringSymbolic

module Stability = Microsoft.Research.Biology.StabilityZ3.Stability

open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.Biology.StabilityZ3
open System.Threading

type NoiseAmplificationFilter = No | Neutral | Strict | LessThan
type ModelReduction = Off | On | Specified of string list
type TuringAnalysisSettings = 
    { prevent_oscillations : bool
    ; prevent_noise_amp    : NoiseAmplificationFilter
    ; use_Lienarad_Chipart : bool
    ; group_terms          : bool option  // Some vs None: apply grouping, bool option to specify if equality constraints are included
    ; model_reduction      : ModelReduction
    ; timeout              : int option
    ; seed                 : int option
    ; print_status         : bool
    ; zero_cross_cst       : bool   //if true, then a simpler necessary condition is added as an additional constraint
    }
    static member Default = 
        { prevent_oscillations = true
        ; prevent_noise_amp    = Neutral
        ; use_Lienarad_Chipart = false
        ; group_terms          = None
        ; model_reduction      = Off
        ; timeout              = Some 10
        ; seed                 = None
        ; print_status         = true
        ; zero_cross_cst       = false //leads to more UNKNOWNs for GRNs
        }
    static member Relaxed = {TuringAnalysisSettings.Default with prevent_noise_amp = No}
    static member Strict = {TuringAnalysisSettings.Default with prevent_noise_amp = Strict}
    


//Turing instability
let Turing (opt:TuringAnalysisSettings) (S:Dynamical) =        

    if opt.zero_cross_cst then 
        failwith "Zero cross constraint is currretly only implemented for the direct encoding method"

    let vars = S.GetVars |> Set.add "wc"
    
    let is_stable = 
        if opt.use_Lienarad_Chipart then 
            Stability.Stable Stability.LienardChipart
        else
            Stability.Stable Stability.Default 
    let is_neutral = 
        if opt.use_Lienarad_Chipart then failwith ""
        else Stability.NeutrallyStable
    let is_unstable = 
        if opt.prevent_oscillations then 
            Stability.Unstable Stability.PreventOscil
        else
            Stability.Unstable Stability.Basic

    //diffusion constraints
    let diff_cst = S.GetDiffVars |> Set.toArray |> Array.map (fun v -> BGeq (Key v, Float 0.0))
    let wc = Key "wc"    
    let D = S.D wc

    let J, gcst = 
        match opt.group_terms with
        | Some flag -> 
            MathNetWrapper.var_cnt <- 0 //reset fresh variable counter
            let state = S.State |> Set.ofArray
            let mutable E = [||]
            let J = 
                S.J ()
                |> Matrix.map (fun e ->                                         
                    let e', eq, _  = MathNetWrapper.Group state e        
                    E <- Array.append E eq
                    e')

            let C = if flag then E else [||]
            J, C
        | None -> S.J (), [||]

    let base_property = 
        let core = 
            [| S.EqCst                //equilibrium
             ; J     |> is_stable     //stable without diffusion
             ; J - D |> is_unstable   //Unstable with diffusion
             ; [|BGT(wc, Float 0.0)|]
             ; diff_cst
            |]
            |> Array.concat
            //|> And

        Array.append core gcst

    let NoiseFilter filter = 
        let nd_locs = D.values |> Array.mapi (fun i Di -> i, Di.[i] = (Float 0.0)) |> Array.filter snd |> Array.map fst
        if Array.isEmpty nd_locs
        then base_property, vars
        else 
            (*[ J.Slice nd_locs nd_locs |> filter
            ; base_property
            ]
            |> And, vars*)
            Array.append base_property (filter (J.Slice nd_locs nd_locs)), vars
        
     
    match opt.prevent_noise_amp with
    | Strict   -> NoiseFilter is_stable
    | Neutral  -> NoiseFilter is_neutral
    | LessThan -> 
        let vars = vars |> Set.add "w_2"
        let w2 = Key "w_2"
        let D2 = S.D w2
        [| J - D2 |> is_stable //Stable with more diffusion
         ; [|BGT(w2, wc)|]
         ; [|BGT(w2, Float 0.0)|]
         ; base_property
        |]
        |> Array.concat, vars
    | No      -> base_property, vars

    
        
//Turing instability (direct to Z3 encoding)
let TuringZ3 (opt:TuringAnalysisSettings) (z3:Microsoft.Z3.Context) (S:Dynamical) = 
    
    let vars = 
        S.GetVars        
        |> Set.add "wc"       
        |> fun x -> if opt.zero_cross_cst then x |> Set.add "wzc" else x
        |> Set.toArray                        
        |> Array.map(fun v -> 
            let v' = z3.MkFreshConst(v, z3.RealSort) :?> Microsoft.Z3.ArithExpr
            v, v')
        |> Map.ofArray           
    
    let is_stable   = 
        if opt.use_Lienarad_Chipart then 
            Stability.StableZ3 Stability.LienardChipart z3
        else
            Stability.StableZ3 Stability.Default z3

    let is_neutral_stable = 
        if opt.use_Lienarad_Chipart 
        then failwith "Haven't allowed Lienard-Chipart for testing neutral stability (used when filtering noise-amplifying networks)"
        else Stability.NeutrallyStableZ3 z3 S.farkas.Length

    let is_unstable = 
        if opt.prevent_oscillations then 
            Stability.UnstableZ3 Stability.PreventOscil z3
        else
            Stability.UnstableZ3 Stability.Basic z3

    //diffusion constraints
    let diff_cst = S.GetDiffVars |> Set.toArray |> Array.map(fun v -> BGeq(Key v, Float 0.0))

    let J, gcst = 
        match opt.group_terms with
        | Some flag -> 
            MathNetWrapper.var_cnt <- 0 //reset fresh variable counter
            let state = S.State |> Set.ofArray
            let mutable E = [||]
            let J = 
                S.J ()
                |> Matrix.map (fun e ->                                         
                    let e', eq, _  = MathNetWrapper.Group state e        
                    E <- Array.append E eq
                    e')

            let C = if flag then E else [||]
            J, C
        | None -> S.J (), [||]


    let zero_cst = 
        if opt.zero_cross_cst then
            let detCst = BEq((S.J () - S.D (Key "wzc")).Det, Float 0.0)
            let wCst = BGT (Key "wzc",Float 0.0)         //do we need this?
            [|detCst; wCst|]
        else
            [||]

    let wc = Key "wc"
    let D = S.D wc
    let base_property = 
        let core = 
            [| S.EqCst    //equilibrium
             ; [|BGT(wc, Float 0.0)|]
             ; diff_cst
             ; zero_cst
             ; gcst
            |]
            |> Array.concat
            |> Array.map (Encoding.BoolExpr2RealRec vars z3)
        let stability = 
            [| J         |> Matrix.encode vars z3 |> is_stable   //stable without diffusion
             ; J - D     |> Matrix.encode vars z3 |> is_unstable //Unstable with diffusion
            |]
            |> Array.concat
        Array.append core stability
    
    let NoiseFilter filter = 
        let nd_locs = D.values |> Array.mapi (fun i Di -> i, Di.[i] = (Float 0.0)) |> Array.filter snd |> Array.map fst
        let extended_property = 
            if Array.isEmpty nd_locs
            then base_property
            else 
                let extra_property = J.Slice nd_locs nd_locs |> Matrix.encode vars z3 |> filter
                Array.append base_property extra_property
        extended_property, vars

    match opt.prevent_noise_amp with
    | Strict -> NoiseFilter is_stable
    | Neutral -> NoiseFilter is_neutral_stable
    | LessThan ->         
        let vars = vars.Add("w_2",  z3.MkFreshConst("w_2", z3.RealSort) :?> Microsoft.Z3.ArithExpr)
        let w2 = Key "w_2"
        let D2 = S.D w2
        [| base_property
         ; (S.J () - D2) |> Matrix.encode vars z3 |> is_stable //Stable with more diffusion
         ; [|BGT(w2, wc); BGT(w2, Float 0.0)|] |> Array.map (Encoding.BoolExpr2RealRec vars z3)
        |] |> Array.concat, vars
    | No -> base_property, vars


//Turing instability (direct to Z3 encoding) for entire regions (cst is a map to LB*UB)
let TuringRegions (cst:Map<string,float * float>) (opt:TuringAnalysisSettings) (z3:Microsoft.Z3.Context) (S:Dynamical) = 
    let expr, vars = TuringZ3 opt z3 S
    //let states = S.State |> Set.ofSeq
    //let stateVars = states |> Array.map (fun x -> vars.[x] :> Microsoft.Z3.Expr)
    //let paramVars = vars |> Map.filter (fun v _ -> not (states.Contains v))


    let Evars = cst |> Map.map(fun k _ -> vars.[k] :> Microsoft.Z3.Expr) |> Map.toArray |> Array.map snd//existential vars    
    let bounds, Avars = //universal vars and constraints
        vars 
        |> Map.filter (fun v _ -> not (cst.ContainsKey v)) 
        |> Map.toArray
        |> Array.fold(fun (C,V) (name, v) ->             
            let lb = z3.MkFreshConst(sprintf "%sLB" name, z3.RealSort) :?> Microsoft.Z3.ArithExpr
            let ub = z3.MkFreshConst(sprintf "%sUB" name, z3.RealSort) :?> Microsoft.Z3.ArithExpr
            let cst = z3.MkAnd(z3.MkGt(v,lb), z3.MkLe(v,ub))
            cst::C, lb::ub::v::V
            ) ([],[])

    let Av = Avars |> Array.ofList |> Array.map (fun v -> v :> Microsoft.Z3.Expr)
    let exists = z3.MkExists(Evars, z3.MkAnd expr)
    let forall = z3.MkForall(Av, z3.MkImplies(z3.MkAnd(bounds), exists))
    let E = forall :> Microsoft.Z3.BoolExpr        
    E, vars




(*
   member this.Z3Turing (z3:Microsoft.Z3.Context) = 
        let var_strings = 
            let main_vars = 
                this.Eqns
                |> Array.map(fun e -> e.GetVars)
                |> Array.reduce (+)
                |> Set.add "w"

            if this.diff_diff then
                let diff_vars = 
                    this.diffusable
                    |> Set.toArray
                    |> Array.map(fun x -> sprintf "Dr_%s" x)
                    |> Set.ofArray
                main_vars + diff_vars
            else    
                main_vars
                
        let vars = 
            var_strings
            |> Set.toArray                
            |> Array.map(fun v -> 
                let v' = z3.MkFreshConst(v, z3.RealSort) :?> Microsoft.Z3.ArithExpr
                v, v')
            |> Map.ofArray  
        
        let Jd = this.J - this.D //Jacobian with diffusion        
        
    

        let Z3J = EncodeMatrix this.J  //Z3 Jacobian
        let Z3Jd = EncodeMatrix Jd     //Z3 Jacobian with diffusion
        
               
        //equality constraints
        let eq       = this.EqCst |> Encoding.BoolExpr2RealRec vars z3
        let stable   = Dynamical.Z3stable z3 Z3J
        let unstable = Dynamical.Z3unstable z3 Z3Jd

        z3.MkAnd([eq; stable; unstable]), vars












            member this.RobustTuring  =         
        let Jd = this.J - this.D //Jacobian with diffusion                
        let turing_cst = And [|this.StableEq; Dynamical.unstable Jd|]

        let vars = turing_cst.GetVars
        let state_vars = this.State |> Set.ofSeq
        let param_vars = 
            vars - state_vars - (Set.singleton "w")
            |> Set.filter (fun v -> not (v.Contains("Dr")))

        let robust_turing = 
            param_vars
            |> Set.toArray
            |> Array.mapi(fun i p -> 
                let p1 = Var (sprintf "%s1" p)
                let p2 = Var (sprintf "%s2" p)
                let pv = Var p            

                let subst_map = 
                    state_vars
                    |> Set.toArray
                    |> Array.map(fun sv -> sv, Var (sprintf "%s%i" sv i))
                    |> Map.ofSeq            
            
                let subst_map1 = subst_map.Add(p, p1);
                let C1 = turing_cst.Subst subst_map1
                
                //And(C1, Eq(p1, 1.1*pv))
                
                let subst_map2 = subst_map.Add(p, p2);
                let C1 = turing_cst.Subst subst_map1
                let C2 = turing_cst.Subst subst_map2
                And [|C1; C2; Eq(p1, 1.01*pv); Eq(p2, 0.99*pv)|]
                
                )
            |> And
        And [|robust_turing; turing_cst|]

*)
