// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.MathNetWrapper

open MathNet.Symbolics
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine.Expression

let rec ToMathNet (e:NumExpr) = 
    match e with      
    | Key v         -> Identifier(Symbol v)
    | Float f       -> Approximation(Real f)
    | Times terms   -> terms |> List.map ToMathNet |> List.reduce ( * )
    | Plus terms    -> terms |> List.map ToMathNet |> List.reduce ( + )
    | Power p       -> MathNet.Symbolics.Expression.Power (ToMathNet p.base_, ToMathNet p.exponent)
    | Divide d      -> (ToMathNet d.div1) / (ToMathNet d.div2)
    | Minus m       -> (ToMathNet m.sub1) - (ToMathNet m.sub2)
    | Absolute a    -> MathNet.Symbolics.Expression.Abs (ToMathNet a)
    | Log a         -> MathNet.Symbolics.Expression.Ln (ToMathNet a)
    | Ceiling a     -> failwith "Cannot convert Expression.Ceiling to MathNet.Symbolics"
    | Floor a       -> failwith "Cannot convert Expression.Floor to MathNet.Symbolics"
    | Round a       -> failwith "Cannot convert Expression.Round to MathNet.Symbolics"
    | Modulo _      -> failwith "Cannot convert Expression.Modulo to MathNet.Symbolics"
    | If (cond,a,b) -> failwith "Cannot convert Expression.If to MathNet.Symbolics"


let rec FromMathNet (e:MathNet.Symbolics.Expression) = 
    match e with      
    | Identifier(Symbol v)  -> Key v
    | Approximation(Real f) -> Float f
    | Number x              -> ExpressionFunctions.Parse (x.ToString())
    | Product terms         -> terms |> List.map FromMathNet |> Times
    | Sum terms             -> terms |> List.map FromMathNet |> Plus
    | MathNet.Symbolics.Expression.Power (a,b) -> (FromMathNet a)**(FromMathNet b)
    | _ -> failwithf "Error while decoding %s" (e.ToString())
    //| Divide (a, b) -> (ToMathNet a) / (ToMathNet b)
    //| Minus (a, b)  -> (ToMathNet a) - (ToMathNet b)             

let SimplifyNum x = x |> ToMathNet |> MathNet.Symbolics.Algebraic.expand |> FromMathNet

let rec Simplify (e:BoolExpr) =     
    match e with       
    | BGT  (a,b) -> BGT  (SimplifyNum a, SimplifyNum b)
    | BGeq (a,b) -> BGeq (SimplifyNum a, SimplifyNum b)
    | BLT  (a,b) -> BLT  (SimplifyNum a, SimplifyNum b)
    | BLeq (a,b) -> BLeq (SimplifyNum a, SimplifyNum b)
    | BEq  (a,b) -> BEq  (SimplifyNum a, SimplifyNum b)
    | BAnd (a,b) -> BAnd (Simplify a, Simplify b)
    | BOr  (a,b) -> BOr  (Simplify a, Simplify b)
    //| Const c -> Const c
    | BNot a     -> BNot (Simplify a)
    | BTrue      -> BTrue
    | BFalse     -> BFalse


(*  Group monomials and replace constants with a merged variable
    for example ax + bx + cy becomes Kx + cy where K = a + b     
    state_vars is a set of all state variables (e.g. x,y in the example above)
    x is the expression to group
*)
let mutable var_cnt = 0
let Group (state_vars:Set<string>) (x:NumExpr) =         
    let mutable vars = Map.empty

    let MkVar V = 
        let vn = sprintf "K_{%i}" var_cnt 
        let v = vn |> Key
        var_cnt <- var_cnt + 1
        vars <- vars.Add(vn, V)
        v
            
    let e = SimplifyNum x            
    let e' = 
        match e with 
        | Plus terms -> 
            terms
            |> List.groupBy (fun t -> Set.intersect (ExpressionFunctions.GetVars t) state_vars)
            |> List.map (fun (t, elist) ->     
                //TODO: Extra logic to prevent unnecessary replacements for single constants/variables (e.g. K0 = k0)                                              
                if Set.isEmpty t then   //constants only
                    elist 
                    |> NumExpr.Plus                        
                    |> SimplifyNum
                    |> MkVar                                        
                else 
                    let x = t |> Set.toList |> List.map Key |> Times            
                    let V = 
                        let v = elist |> Plus
                        v/x
                        |> SimplifyNum                    
                    let K = MkVar V
                    K * x                
                )
            |> Plus                    
        | _ ->             
            //printfn "WARNING: Expression %s was not a summation of monomials (ignoring)!"  (e.ToString())
            e


    //prepare equality constraints         
    let cst =         
        vars        
        |> Map.toArray
        |> Array.map (fun (v,e) -> BEq(Key v, e))

    e', cst, vars
