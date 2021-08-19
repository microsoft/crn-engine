module Microsoft.Research.Biology.StabilityZ3.Encoding
//open Microsoft.Research.Biology.StabilityZ3.Complex

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine.Expression

let MergeMaps (x:Map<'a,'b>) (y:Map<'a,'b>) = 
    //Array.append (y |> Map.toArray) (x |> Map.toArray)
    //|> Map.ofArray
    y
    |> Map.fold(fun (acc:Map<'a,'b>) k v -> acc.Add(k,v)) x

let Parse_Z3_value (v:string) = 
  let els = v.Split('/')
  match Array.length els with
  | 2 -> (float els.[0]) / (float els.[1])
  | 1 -> 
    if els.[0].Contains ("?") then 
        printfn "WARNING: Z3 Precission in %s" els.[0]
        els.[0].Replace("?","") |> float
    else
        float els.[0]
  | 0 -> failwith "Empty string?"
  | _ -> failwith "Multiple divisions?"



let EXPAND_POWERS = false
(*
let rec NumExpr2Fp (z3:Microsoft.Z3.Context) V (e:NumExpr) = 
    let mutable vars = V
    let mutable cst  = List.empty
    
    let FPSort = z3.MkFPSort16()
    let rounding = z3.MkFPRoundNearestTiesToAway()    
    
    let enc1 a = 
        let a', vars', cst' = NumExpr2Fp z3 vars a
        vars <- MergeMaps vars vars'
        cst <- List.append cst cst'
        a'

    let enc (a,b) = (enc1 a, enc1 b)        

    let encoded = 
        match e with 
        | Var v         ->             
            if vars.ContainsKey v then vars.[v]
            else
                let v' = z3.MkFreshConst(v, FPSort) :?> Microsoft.Z3.FPExpr
                vars <- vars.Add(v,v')
                v'
                    
        | Float f       -> z3.MkFP(f, FPSort) :> Microsoft.Z3.FPExpr
            
        | Times (a, b)  -> 
            let a', b' = enc (a,b)        
            z3.MkFPMul(rounding, a', b')
            
        | Divide (a, b) -> 
            let a', b' = enc (a,b)  
            cst <- z3.MkNot(z3.MkEq(b', z3.MkFP(0, FPSort)))::cst                  
            z3.MkFPDiv(rounding, a', b')
            
        | Power (a, b)  ->                 
            match  b.Eval with
            | Some f -> 
                if System.Math.Ceiling f = f then
                    let k  = int f
                    let a' = enc1 a
                    Array.replicate k a'
                    |> Array.reduce (fun x y -> z3.MkFPMul(rounding, x, y))
                else
                    failwith "Floating point encoding of fractional exponents is not currently supported"
            | None -> failwith "Floating point encoding of exponential expressions is not currently supported"
                                                            
        | Plus (a, b)   -> 
            let a', b' = enc (a,b)
            z3.MkFPAdd(rounding, a', b')
            
        | Minus (a, b)  -> 
            let a', b' = enc (a,b)
            z3.MkFPSub(rounding, a', b')

    encoded, vars, cst

let rec BoolExpr2FP (z3:Microsoft.Z3.Context) V (e:BoolExpr) =    
    let mutable vars = V
    let mutable cst  = List.empty

    let enc1num a = 
        let a', vars', cst' = NumExpr2Fp z3 vars a
        vars <- MergeMaps vars vars'
        cst <- List.append cst cst'
        a'

    let encNum (a,b) = (enc1num a, enc1num b)    

    let enc1bool a = 
        let a', vars', cst' = BoolExpr2FP z3 vars a
        vars <- MergeMaps vars vars'
        cst <- List.append cst cst'
        a'

    let encBool (a,b) = (enc1bool a, enc1bool b)    
    
    let e' = 
        match e with 
        | Gt (a,b) -> (a,b) |> encNum |> z3.MkFPGt
        | Ge (a,b) -> (a,b) |> encNum |> z3.MkFPGEq
        | Lt (a,b) -> (a,b) |> encNum |> z3.MkFPLt
        | Le (a,b) -> (a,b) |> encNum |> z3.MkFPLEq
        | Eq (a,b) -> (a,b) |> encNum |> z3.MkEq
        | And (a,b) -> 
            let a', b' = (a,b) |> encBool
            z3.MkAnd(a',b')
        | Or  (a,b) ->
            let a', b' = (a,b) |> encBool
            z3.MkOr(a',b')
        | Const c -> if c then z3.MkTrue() else z3.MkFalse()
        | Not a  -> z3.MkNot (enc1bool a)            
    e', vars, cst


*)



let rec NumExpr2RealRec (vars:Map<string,Microsoft.Z3.ArithExpr>) (z3:Microsoft.Z3.Context) (e:NumExpr) =                            
    let encode a = NumExpr2RealRec vars z3 a                      
    let encoded = 
        match e with 
        | Key v -> vars.[v]            

        | Float f -> z3.MkReal(f.ToString()) :> Microsoft.Z3.ArithExpr
            
        | Times terms  -> terms |> List.map encode |> z3.MkMul           
            
        | Divide div -> (encode div.div1) / (encode div.div2)            
                    
        | Power p  ->        
            if EXPAND_POWERS then 
                try 
                    let f = eval (fun _ -> nan) p.exponent
                    if System.Math.Ceiling f = f 
                    then
                        let k  = int f
                        let a' = encode p.base_
                        let Q = Array.init (abs k) (fun i -> a') |>  z3.MkMul
                        if k>0 then 
                            Q
                        else
                            z3.MkDiv(z3.MkReal(1),Q)
                    else
                        //failwith "Fractional exponents are not currently supported when expanding POW"
                        printfn "WARNING: Fractional exponent"                                      
                        z3.MkPower(encode p.base_, encode p.exponent)
                  with | _ -> //failwith "Exponential expressions are not currently supported when expanding POW"
                    printfn "WARNING: symbolic exponent"                                      
                    z3.MkPower(encode p.base_, encode p.exponent)
            else       
                match eval (fun _ -> nan) p.exponent with
                | 0.0 -> z3.MkReal(1) :> Microsoft.Z3.ArithExpr
                | 1.0 -> encode p.base_
                | 2.0 -> encode (p.base_*p.base_)
                | -1.0 -> z3.MkDiv(z3.MkReal(1), encode p.base_)                
                | _ -> z3.MkPower(encode p.base_, encode p.exponent)
                                          
        | Plus terms -> terms |> List.map encode |> z3.MkAdd
            
        //| Minus m -> z3.MkReal((m.sub1-m.sub2).ToString()) :> Microsoft.Z3.ArithExpr
        
        //| Minus (a, Float 0.0) -> encode a
        | Minus m    -> (encode m.sub1) - (encode m.sub2)

        | Absolute a -> failwith "Absolute value not supported"
        | Ceiling a  -> failwith "Ceiling not supported"
        | Floor a    -> failwith "Floor not supported"
        | Round a    -> failwith "Round not supported"
        | Log a      -> failwith "Log not supported"
        | Modulo a   -> failwith "Modulo not supported"        
        | If (_,_,_) -> failwith "If not supported"
    
    encoded


let rec BoolExpr2RealRec (vars:Map<string,Microsoft.Z3.ArithExpr>) (z3:Microsoft.Z3.Context) (e:BoolExpr) =            
    let encodeNum a = NumExpr2RealRec vars z3 a    
    let encodeBool a = BoolExpr2RealRec vars z3 a            
    
    match e with 
    | BGT (a,b)  -> (encodeNum a, encodeNum b) |> z3.MkGt
    | BGeq (a,b) -> (encodeNum a, encodeNum b) |> z3.MkGe
    | BLT (a,b)  -> (encodeNum a, encodeNum b) |> z3.MkLt
    | BLeq (a,b) -> (encodeNum a, encodeNum b) |> z3.MkLe
    | BEq (a,b)  -> (encodeNum a, encodeNum b) |> z3.MkEq
    | BAnd (a,b) -> [|a;b|] |> Array.map (BoolExpr2RealRec vars z3) |> z3.MkAnd
    | BOr  (a,b) -> [|a;b|] |> Array.map (BoolExpr2RealRec vars z3) |> z3.MkOr
    //| Const c -> if c then z3.MkTrue() else z3.MkFalse()
    | BNot a     -> z3.MkNot (encodeBool a)
    | BFalse     -> z3.MkFalse ()
    | BTrue      -> z3.MkTrue ()

    

(*
let rec BoolExpr2RealRec (vars:Map<string,Microsoft.Z3.ArithExpr>) (z3:Microsoft.Z3.Context) (e:BoolExpr) =            
    let encodeNum a = NumExpr2RealRec vars z3 a    
    let encodeBool a = BoolExpr2RealRec vars z3 a        
    
    let mutable cst = []

    let E = 
        match e with 
        | Gt (a,b) -> 
            match a, b with
            | _, Float 0.0 ->
                let v = z3.MkFreshConst("temp",z3.RealSort) :?> Microsoft.Z3.ArithExpr
                cst <- z3.MkLt(v, z3.MkReal(0))::cst
                z3.MkEq(z3.MkAdd(encodeNum a, v), z3.MkReal(0))
            | Float 0.0, _ -> Lt(b,Float 0.0) |> encodeBool
            | _ -> (encodeNum a, encodeNum b) |> z3.MkGt
        | Ge (a,b) ->                         
            match a, b with
            | _, Float 0.0 ->
                let v = z3.MkFreshConst("temp",z3.RealSort) :?> Microsoft.Z3.ArithExpr
                cst <- z3.MkLe(v, z3.MkReal(0))::cst
                z3.MkEq(z3.MkAdd(encodeNum a, v), z3.MkReal(0))
            | Float 0.0, _ -> Le(b,Float 0.0) |> encodeBool
            | _ -> (encodeNum a, encodeNum b) |> z3.MkGe
        | Lt (a,b) -> 
            match a, b with
            | _, Float 0.0 ->
                let v = z3.MkFreshConst("temp",z3.RealSort) :?> Microsoft.Z3.ArithExpr
                cst <- z3.MkGt(v, z3.MkReal(0))::cst
                z3.MkEq(z3.MkAdd(encodeNum a, v), z3.MkReal(0))
            | Float 0.0, _ -> Gt(b,Float 0.0) |> encodeBool
            | _ -> 
            (encodeNum a, encodeNum b) |> z3.MkLt
        | Le (a,b) -> 
            match a, b with
            | _, Float 0.0 ->
                let v = z3.MkFreshConst("temp",z3.RealSort) :?> Microsoft.Z3.ArithExpr
                cst <- z3.MkGe(v, z3.MkReal(0))::cst
                z3.MkEq(z3.MkAdd(encodeNum a, v), z3.MkReal(0))
            | Float 0.0, _ -> Ge(b,Float 0.0) |> encodeBool
            | _ -> (encodeNum a, encodeNum b) |> z3.MkLe
        | Eq (a,b) -> (encodeNum a, encodeNum b) |> z3.MkEq
        | And terms -> terms |> Array.map (BoolExpr2RealRec vars z3) |> z3.MkAnd
        | Or  terms -> terms |> Array.map (BoolExpr2RealRec vars z3) |> z3.MkOr
        | Const c -> if c then z3.MkTrue() else z3.MkFalse()
        | Not a  -> z3.MkNot (encodeBool a)                                        

    E::cst     
    |> Array.ofList
    |> z3.MkAnd

*)
    

let BoolExpr2RealWithVars (z3:Microsoft.Z3.Context) (vars:Set<string>) (e:BoolExpr) =                
    let vars = 
        vars
        |> Set.toArray
        |> Array.map(fun v -> 
            let v' = z3.MkFreshConst(v, z3.RealSort) :?> Microsoft.Z3.ArithExpr
            v, v')
        |> Map.ofArray    

    let cst = 
        ExpressionFunctions.BGetDenoms e
        |> Set.toArray
        |> Array.map(fun e -> BoolExpr2RealRec vars z3 (BNot (BEq(e, Float 0.0))))
        |> Array.toList  
    //let cst = List.empty

    let E = 
        (BoolExpr2RealRec vars z3 e)::cst
        |> Array.ofList
        |> z3.MkAnd
    E, vars
    
            
let BoolExpr2Real (z3:Microsoft.Z3.Context) (e:BoolExpr) =                  
    BoolExpr2RealWithVars z3 (ExpressionFunctions.BGetVars e) e






(* FP encoding *) 
(*
let FPSort = z3.MkFPSort32()

type NumExpr = FPExpr

let Simplify (a:NumExpr) = a.Simplify() :?> NumExpr 

let MkNum (x:float) =  z3.MkFP(x, FPSort) :> NumExpr

let MkVar (name:string) = z3.MkFreshConst(name,FPSort) :?> NumExpr

let rounding = z3.MkFPRoundNearestTiesToAway()

let AddNS (a:NumExpr) (b:NumExpr) = z3.MkFPAdd(rounding, a, b)  |> Simplify

let SubNS (a:NumExpr) (b:NumExpr) = z3.MkFPSub(rounding, a, b) |> Simplify

let MulNS (a:NumExpr) (b:NumExpr) = z3.MkFPMul(rounding,a,b) |> Simplify

let DivNS (a:NumExpr) (b:NumExpr) = z3.MkFPDiv(rounding, a, b) |> Simplify

let Neg (a:NumExpr) = z3.MkFPSub(rounding, MkNum 0.0 , a) |> Simplify

let Ge (a:NumExpr) (b:NumExpr) = z3.MkFPGEq(a, b)

let Gt (a:NumExpr) (b:NumExpr) = z3.MkFPGt(a, b)

let Le (a:NumExpr) (b:NumExpr) = z3.MkFPLEq(a, b)

let Lt (a:NumExpr) (b:NumExpr) = z3.MkFPLt(a, b)

let Eq (a:NumExpr) (b:NumExpr) = z3.MkEq(a, b)
*)





(* Real encoding *) 
(*

type NumExpr = ArithExpr

let Simplify (a:NumExpr) = a.Simplify() :?> NumExpr 

let MkNum (x:float) = z3.MkReal(x.ToString()) :> ArithExpr 

let MkVar (name:string) = z3.MkRealConst(name) :> ArithExpr 
     
let AddNS (a:NumExpr) (b:NumExpr) = z3.MkAdd(a, b) |> Simplify   

let SubNS (a:NumExpr) (b:NumExpr) = z3.MkSub(a, b) |> Simplify

let MulNS  (a:NumExpr) (b:NumExpr)= z3.MkMul(a,b) |> Simplify

let DivNS (a:NumExpr) (b:NumExpr) = z3.MkDiv(a, b) |> Simplify

let Neg (a:NumExpr) = z3.MkSub(MkNum 0.0 , a) 

let Ge (a:NumExpr) (b:NumExpr) = z3.MkGe(a, b)

let Gt (a:NumExpr) (b:NumExpr) = z3.MkGt(a, b)

let Le (a:NumExpr) (b:NumExpr) = z3.MkLe(a, b)

let Lt (a:NumExpr) (b:NumExpr) = z3.MkLt(a, b)

let Eq (a:NumExpr) (b:NumExpr) = z3.MkEq(a, b)

let Eval (M:Model) (a:NumExpr) = M.Eval(a,true) :?> NumExpr |> Simplify

let GetName (a:NumExpr) = a.FuncDecl.Name.ToString()
*)

(* Complex encoding *) 
(*
type NumExpr = Complex.Complex

let Simplify (a:NumExpr) = a.Simplify()

let MkNum (x:float) = Complex.Complex.Create x

let MkVar (name:string) = Complex.Complex.MkVar name
     
let AddNS (a:NumExpr) (b:NumExpr) = (a + b).Simplify()

let SubNS (a:NumExpr) (b:NumExpr) = (a - b).Simplify()

let MulNS  (a:NumExpr) (b:NumExpr)= (a * b).Simplify()

let DivNS (a:NumExpr) (b:NumExpr) = (a/b).Simplify()

let Neg (a:NumExpr) = (Complex.Complex.Create(0.0) -  a).Simplify()

let Ge (a:NumExpr) (b:NumExpr) = Complex.Complex.Ge(a,b)

let Gt (a:NumExpr) (b:NumExpr) = Complex.Complex.Gt(a,b)

let Le (a:NumExpr) (b:NumExpr) = Complex.Complex.Le(a,b)

let Lt (a:NumExpr) (b:NumExpr) = Complex.Complex.Lt(a,b)

let Eq (a:NumExpr) (b:NumExpr) = Complex.Complex.Eq(a,b)

let Eval (M:Model) (a:NumExpr) = a.Eval M |> Simplify

let GetName (a:NumExpr) = a.Name
*)



(*
type VarManager =
    { mutable vars : Map<string, NumExpr * NumExpr> //Map<name, Z3var * NumExpr>
    ; mutable csts : BoolExpr list
    }
    static member Create() = 
        { vars = Map.empty
        ; csts = List.empty
        }

    member this.MkVar NumExpr = 
        let id = this.vars.Count
        let name = sprintf "tmp_var_%i" id
        let var = MkVar(name)
        this.vars <- this.vars.Add(name, (var, NumExpr))
        var

    member this.Add NumExpr =         
        this.csts <- NumExpr::this.csts

let varManager = VarManager.Create()

   

(* Common encoding functions *)
let zero = MkNum(0.0)

let one = MkNum(1.0)

let Eq0 x = Eq x zero

let Add (a:NumExpr) (b:NumExpr) = 
    if (a |> Simplify) = zero then b |> Simplify
    elif (b |> Simplify) = zero then a |> Simplify
    else
        (AddNS a b) 

let Sub (a:NumExpr) (b:NumExpr) = 
    if (b |>Simplify) = zero then (a  |> Simplify)
    else
        SubNS a b

let Div (a:NumExpr) (b:NumExpr) =
    let a' = a |> Simplify
    let b' = b |> Simplify
    if a' = zero then zero
    elif b'= one then a'
    else 
        varManager.Add(z3.MkNot(Eq0 b))
        DivNS a b 

let And (a:BoolExpr[]) = z3.MkAnd(a)

let Or  (a:BoolExpr[]) = z3.MkOr(a)

let Mul (a:NumExpr) (b:NumExpr) = 
  let a' = a |> Simplify
  let b'= b |> Simplify
  
  if a' = zero || b' = zero then zero 
  elif a' = one then b'
  elif b' = one then a'
  else MulNS a b

let Pow (x:NumExpr) (n:int) = 
    match n with 
    | 0 -> one
    | 1 -> x
    | k -> 
        let k' = System.Math.Abs(k)
        let mul = 
            Array.replicate k' x
            |> Array.reduce Mul      
            |> Simplify              

        if k<0 then 
            Div one mul
        else
            mul


let Not = z3.MkNot


*)