module Microsoft.Research.Biology.StabilityZ3.Stability

module LinAlg = Microsoft.Research.Biology.StabilityZ3.LinearAlgebra

open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.Biology.StabilityZ3

//let And = Array.reduce (fun a b -> BAnd (a,b))
let Or  = Array.reduce (fun a b -> BOr (a,b))

type StabilityMethod = 
    | Default
    | LienardChipart //Stability using the Lienard-Chipart criterion (see https://en.wikipedia.org/wiki/Li%C3%A9nard%E2%80%93Chipart_criterion)    

type InstabilityMethod = 
    | Basic
    | PreventOscil  //additional restriction to prevent oscillations

let internal simplify = false

let internal Gen_Default ineq alg (cs:Vector) =
    //let routh_array = LinAlg.routh_hurwitz alg cs
    let routh_array = LinAlg.routh_hurwitz alg cs
    let char_pol_csts = cs.values |> Array.map (fun a -> ineq (a, Float 0.0))
    let routh_csts = routh_array |> Array.map (fun a -> ineq (a, Float 0.0))
    char_pol_csts |> Array.append routh_csts //|> And 
        
/// Stability constraints on an arbitrary (Jacobian) matrix
let Stable alg (A:Matrix) = 
    let cs = LinAlg.characteristic_polynomial LinAlg.Faddeev A //coefficients of characteristic polynomial (leading with 0-th power)        
    let csz = 
        if simplify
        then 
            let css = cs |> Vector.map MathNetWrapper.SimplifyNum
            let n = A.Length
            let nzeros = css.values |> Array.rev |> Array.findIndex (fun c -> c <> Float 0.0)
            { css with values = css.values.[0..n-nzeros] }
        else cs
    match alg with 
    | Default -> Gen_Default BGT LinAlg.Routh csz    // We use Routh table for stability because it is more efficient than HurwitzMatrix (WARNING: currently doesn't handle special cases)
    | LienardChipart -> 
        let pm = csz |> LinAlg.routh_hurwitz LinAlg.Hurwitz //principal minors of Hurwitz matrix               
        
        //choose one of the 4 equivalent conditions (to check: does encoding all conditions help?)
        let n = A.Length
        [| for i in [0..2..n] do
            yield BGT(cs.[n-i], Float 0.0)
            if i+1 < n then
                yield BGT(pm.[i+1], Float 0.0)
        |]
        //|> And

/// Neutral stability constraints on an arbitrary (Jacobian) matrix
let NeutrallyStable (A:Matrix) = 
    let cs = LinAlg.characteristic_polynomial LinAlg.Faddeev A //coefficients of characteristic polynomial (leading with 0-th power)        
    Gen_Default BGeq LinAlg.Hurwitz cs              // We use HurwitzMatrix because the special cases of RouthTable (zero rows of Routh array) do not arise in this formulation

        
// Instability constaints of an arbitrary (Jacobian) matrix
let Unstable alg (A:Matrix)=             
    let cs = LinAlg.characteristic_polynomial LinAlg.Faddeev A
    match alg with
    | Basic ->     
        let routh_array = LinAlg.routh_hurwitz LinAlg.Routh cs          
        let char_pol_csts = cs.values |> Array.map (fun a -> BLeq(a, Float 0.0))      //CHECK IF THIS SHOULD BE LT OR LE?
        let routh_csts = routh_array |> Array.map (fun a -> BLeq(a, Float 0.0))             
        [| char_pol_csts |> Array.append routh_csts |> Or|]
    | PreventOscil ->                         
        let n = cs.Length - 1
        cs.values.[0..n-1] |> Array.map (fun a -> BGT(a, Float 0.0))        
        |> Array.append [|BLT(cs.values.[n], Float 0.0)|]                     
        //|> And


/// Stability constaints of an equilibrium        
//let StableEq alg (S:Dynamical) = And [|S.J () |> Stable alg ; S.EqCst|]
let StableEq alg (S:Dynamical) = Array.append (S.J () |> Stable alg) S.EqCst

/// Instability constaints of an equilibrium
//let UnstableEq alg (S:Dynamical) = And [|S.J () |> Unstable alg; S.EqCst|]
let UnstableEq alg (S:Dynamical) = Array.append (S.J () |> Unstable alg) S.EqCst



(********************************************************************************************
 * The following functions implement the stability constraints directly using Z3.           *
 * This way, intermediate encoding is avoided but it is harder to perform simplifications.  *
 ********************************************************************************************)

let internal GenZ3_Default ineq alg (z3:Microsoft.Z3.Context) (cs:Z3Vector) =
    let zero = z3.MkReal(0) :> Microsoft.Z3.ArithExpr
    let routh_array, div0 = cs |> LinAlg.z3_routh_hurwitz alg z3
    let char_pol_csts = cs.values |> Array.map (fun a -> ineq (a, zero))
    let routh_csts = routh_array |> Array.map (fun a -> ineq (a, zero))             
    Array.concat [| char_pol_csts; routh_csts; div0 |]
    
/// Stability constraints on an arbitrary (Jacobian) matrix
let StableZ3 alg (z3:Microsoft.Z3.Context) (A:Z3Matrix) =
    let zero = z3.MkReal(0) :> Microsoft.Z3.ArithExpr
    let cs = LinAlg.z3_characteristic_polynomial z3 A
    match alg with
    | Default -> GenZ3_Default z3.MkGt LinAlg.Routh z3 cs    // We use Routh table for stability because it is more efficient than HurwitzMatrix (WARNING: currently doesn't handle special cases)
    | LienardChipart ->  
        //failwith "The direct Z3 encoding of the Lienard-Chipart stability criterion is not currently implemented"
        let pm, _  = cs |> LinAlg.z3_routh_hurwitz LinAlg.Hurwitz z3//principal minors of Hurwitz matrix                       
        
        //choose one of the 4 equivalent conditions (to check: does encoding all conditions help?)
        let n = A.Length
        [| for i in [0..2..n] do
            yield z3.MkGt(cs.[n-i], zero)
            if i+1 < n then
                yield z3.MkGt(pm.[i+1], zero)
        |]

/// Neutral stability constraints on an arbitrary (Jacobian) matrix
let NeutrallyStableZ3 (z3:Microsoft.Z3.Context) ncons (A:Z3Matrix) =
    let zero = z3.MkReal(0) :> Microsoft.Z3.ArithExpr
    let cs = LinAlg.z3_characteristic_polynomial z3 A
    // Trim off zero eigenvalues then test the remaining factors of the char pol
    let csz = cs |> fun c -> { c with values = Array.sub c.values 0 (A.Length+1-ncons) }
    GenZ3_Default z3.MkGe LinAlg.Hurwitz z3 csz              // We use HurwitzMatrix because the special cases of RouthTable (zero rows of Routh array) do not arise in this formulation

/// Instability constraints on an arbitrary (Jacobian) matrix      
let UnstableZ3 alg (z3:Microsoft.Z3.Context)  (A:Z3Matrix)=         
    let cs = LinAlg.z3_characteristic_polynomial z3 A
    let zero = z3.MkReal(0) :> Microsoft.Z3.ArithExpr                

    match alg with
    | Basic ->    
        let routh_array, div0 = LinAlg.z3_routh_hurwitz LinAlg.Routh z3 cs            
        let char_pol_csts = cs.values |> Array.map (fun a -> z3.MkLe(a, zero))
        let routh_csts = routh_array |> Array.map (fun a -> z3.MkLe(a, zero))
                     
        let main_cst = 
            char_pol_csts
            |> Array.append routh_csts  
            |> z3.MkOr
        
        div0 |> Array.append [|main_cst|]
        //main_cst

    | PreventOscil ->             
        let n = cs.Length - 1
        cs.values.[0..n-1] |> Array.map (fun a -> z3.MkGt(a, zero))        
        |> Array.append [|z3.MkLt(cs.values.[n], zero)|]

(*
let StableEqZ3  (z3:Microsoft.Z3.Context) vars alg (S:Dynamical) =   
    let Jz = 
        let vecs = 
            S.J.values
            |> Array.map(fun vec -> 
                vec.values
                |> Array.map (fun e -> Encoding.NumExpr2RealRec vars z3 e)
                )
        Z3Matrix.Create(z3, vecs)                   

    let stable = Jz |> StableZ3 alg z3
    let eq = 
        S.Eqns
        |> Array.map(fun e -> Eq(e, Float 0.0))
        |> And
        |> Encoding.BoolExpr2RealRec vars z3
        
        
    match S.SysCst with
    | Some a -> 
        let cst = a |> Encoding.BoolExpr2RealRec vars z3
        z3.MkAnd([|stable; eq; cst|])                                        
    | None  -> z3.MkAnd(stable, eq)   
*)