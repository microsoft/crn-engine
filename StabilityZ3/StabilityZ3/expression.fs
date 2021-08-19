namespace Microsoft.Research.Biology.StabilityZ3

open Microsoft.Research.CRNEngine.Expression

type NumExpr = t<string>
type BoolExpr = bexp<string>

module ExpressionFunctions =
    let rec GetVars (exp:NumExpr) = 
        match exp with      
        | Key v         -> v |> Set.singleton
        | Float f       -> Set.empty        
        | Divide {div1=a; div2=b}
        | Power {base_ = a; exponent=b}
        | Minus {sub1=a;sub2=b}  -> GetVars a + GetVars b
        | Plus terms 
        | Times terms   -> terms |> List.fold(fun acc x -> acc + GetVars x) Set.empty
        | Modulo {modulo=m; div=d} -> GetVars m + GetVars d
        | Absolute t
        | Log t
        | Ceiling t
        | Floor t 
        | Round t     -> GetVars t
        | If (cond,a,b) -> failwith "Cannot extract variables from Expression.If" //BGetVars cond + GetVars a + GetVars b

    let rec BGetVars (exp:BoolExpr) = 
        match exp with 
        | BGT (a,b)  -> GetVars a + GetVars b
        | BGeq (a,b) -> GetVars a + GetVars b
        | BLT (a,b)  -> GetVars a + GetVars b
        | BLeq (a,b) -> GetVars a + GetVars b
        | BEq (a,b)  -> GetVars a + GetVars b
        | BAnd (a,b) -> BGetVars a + BGetVars b
        | BOr  (a,b) -> BGetVars a + BGetVars b
        | BNot a     -> BGetVars a
        | BFalse     -> Set.empty
        | BTrue      -> Set.empty

    let rec GetDenoms (exp:NumExpr) = 
        match exp with      
        | Key v                         -> Set.empty        
        | Float f                       -> Set.empty        
        | Divide {div1=a; div2=b}       -> GetDenoms a + GetDenoms b + Set.singleton b
        | Power {base_ = a; exponent=b} -> GetDenoms a + GetDenoms b
        | Minus {sub1=a;sub2=b}         -> GetDenoms a + GetDenoms b
        | Plus terms                    -> terms |> List.map GetDenoms |> Set.unionMany
        | Times terms                   -> terms |> List.map GetDenoms |> Set.unionMany
        | If (c,e1,e2)                  -> BGetDenoms c + GetDenoms e1 + GetDenoms e2
        | Absolute t
        | Ceiling t
        | Floor t
        | Round t
        | Log t                         -> GetDenoms t
        | Modulo _                      -> failwith "Modulo not supported"

    and BGetDenoms (exp:BoolExpr) = 
        match exp with 
        | BGT (a,b)  -> GetDenoms a + GetDenoms b
        | BGeq (a,b) -> GetDenoms a + GetDenoms b
        | BLT (a,b)  -> GetDenoms a + GetDenoms b
        | BLeq (a,b) -> GetDenoms a + GetDenoms b
        | BEq (a,b)  -> GetDenoms a + GetDenoms b
        | BAnd (a,b) -> BGetDenoms a + BGetDenoms b
        | BOr  (a,b) -> BGetDenoms a + BGetDenoms b
        | BNot a     -> BGetDenoms a
        | BFalse     -> Set.empty
        | BTrue      -> Set.empty
    
    
    
    let Parse str : NumExpr = Microsoft.Research.CRNEngine.Expression.from_string Parser.name str

    let rec ToTextWithPrinter use_latex (printer:float->string) str = 
        match str with
        | Key v -> v 
        | Float f -> 
            if System.Math.Ceiling(f) = f 
            then sprintf "%i" (int f)
            else printer f
        | Times terms -> 
            terms 
            |> List.map(fun x -> 
                match x with
                | Plus _ 
                | Minus _ -> x |> ToTextWithPrinter use_latex printer |> sprintf "(%s)"
                | _       -> x |> ToTextWithPrinter use_latex printer
            )
            |> String.concat (if use_latex then " \\cdot " else "*")
            |> sprintf "(%s)"
        | Divide d -> 
            let e = 
                match d.div1 with
                | Plus _  
                | Minus _ -> d.div1 |> ToTextWithPrinter use_latex printer |> sprintf "(%s)"
                | _       -> d.div1 |> ToTextWithPrinter use_latex printer
            let d =     
                match d.div2 with
                | Plus _  
                | Minus _ -> d.div2 |> ToTextWithPrinter use_latex printer |> sprintf "(%s)"
                | _       -> d.div2 |> ToTextWithPrinter use_latex printer
            if use_latex 
            then sprintf "\\frac{%s}{%s}" e d
            else sprintf "%s/%s" e d
        | Power p  -> 
            let q = 
                match p.base_ with 
                | Key _
                | Float _ -> p.base_ |> ToTextWithPrinter use_latex printer
                |  _      -> p.base_ |> ToTextWithPrinter use_latex printer |> sprintf "(%s)"
            let e =
                match p.exponent with 
                | Key _
                //| Float _ -> p.exponent |> ToText use_latex
                |  _      -> p.exponent |> ToTextWithPrinter use_latex printer //|> sprintf "(%s)"
            //if use_latex then 
            sprintf "%s^{%s}" q e
            //else
                //sprintf "%s^(%s)" q e
        | Plus terms  -> sprintf "%s" (terms |> List.map(ToTextWithPrinter use_latex printer) |> String.concat " + ")
        | Minus m -> 
            let b = m.sub2 |> ToTextWithPrinter use_latex printer
            match m.sub1 with 
            | Float 0.0 -> 
                match m.sub2 with 
                | Times _  
                | Power _ 
                | Key _
                | Float _ -> sprintf "-%s" b
                | _       -> sprintf "-(%s)" b
            | _ -> sprintf "%s - %s" (m.sub1 |> ToTextWithPrinter use_latex printer) b
        | _ -> failwith "Expression operator not supported in ExpressionFunctions.ToText"
    
    let ToText use_latex str = ToTextWithPrinter use_latex (sprintf "%f") str

    let rec BToLatex str =
        match str with 
        | BNot(BGT  (a,b)) -> sprintf "%s \\ngtr %s" (ToText true a) (ToText true b)
        | BNot(BGeq (a,b)) -> sprintf "%s \\ngeq %s" (ToText true a) (ToText true b)
        | BNot(BLT  (a,b)) -> sprintf "%s \\nless %s" (ToText true a) (ToText true b)
        | BNot(BLeq (a,b)) -> sprintf "%s \\nleq %s" (ToText true a) (ToText true b)
        | BNot(BEq  (a,b)) -> sprintf "%s \\neq %s" (ToText true a) (ToText true b)
        | BGT (a,b)  -> sprintf "%s > %s" (ToText true a) (ToText true b)
        | BGeq (a,b) -> sprintf "%s \\geq %s" (ToText true a) (ToText true b)
        | BLT (a,b)  -> sprintf "%s < %s" (ToText true a) (ToText true b)
        | BLeq (a,b) -> sprintf "%s \\leq %s" (ToText true a) (ToText true b)
        | BEq (a,b)  -> sprintf "%s = %s" (ToText true a) (ToText true b)
        | BAnd (a,b) -> sprintf "%s \\wedge %s" (BToLatex a) (BToLatex b)
        | BOr  (a,b) -> sprintf "%s \\vee %s" (BToLatex a) (BToLatex b)
        | BNot a   -> sprintf "\neg(%s)" (BToLatex a)
        | BFalse   -> "false"
        | BTrue    -> "true"
    
    //Derivative
    //NOTE 1: some optimizations might be possible by checking if sub-expressions are functions of the variable x (and treating them as constants otherwise)    
    let rec differentiate (exp:NumExpr) (x:string) = 
    
        if not ((GetVars exp).Contains x) then
            Float 0.0
        else
            match exp with
            | Float _ -> Float 0.0

            | Key y -> if x = y then Float 1.0 else Float 0.0

            | Times terms  ->  
                let n = terms.Length

                if n = 0 then 
                    failwith "ERROR: Empty multiplication term"
                elif n=1 then 
                    differentiate terms.[0] x
                else
                    let terms1 = 
                        [| for i in 1..n-2 do
                            yield (differentiate terms.[i] x) * Times (terms.[0..i-1]) * Times (terms.[i+1..]) 
                        |]
                        |> Array.append
                            [| differentiate terms.[0] x * Times (terms.[1..])
                             ; differentiate terms.[n-1] x * Times (terms.[0..n-2])
                            |]
                
                    let terms2 = 
                        terms1
                        |> Array.map(fun e -> 
                            match e with 
                            | Times terms -> 
                                if terms |> List.exists(fun x -> x = Float 0.0) 
                                then Float 0.0
                                else 
                                    terms
                                    |> List.filter (fun e' -> e'<>Float 1.0)
                                    |> fun x -> if x.Length>1 then Times x else x.[0]                            
                            | _ -> e)
                    let filtered = terms2 |> Array.filter (fun e -> eval (fun _ -> nan) e <> 0.0)
                    //let filtered = terms3 |> Array.filter (fun e -> e<>Float 0.0)
                    filtered
                    |> fun x -> 
                        match x.Length with 
                        | 0 -> Float 0.0
                        | 1 -> x.[0]
                        | _ -> Plus (List.ofArray x)
                    |> simplify
                      
            | Divide {div1 = e1; div2 = e2} -> 
                let v = GetVars e2
                if v.Contains x then
                    ((differentiate e1 x) * e2 - (differentiate e2 x) * e1)/(e2 ** 2.0)        
                else
                    (differentiate e1 x)/e2
        
            | Power {base_ = e1; exponent = e2}  ->             
                 let v = GetVars e2
                 if v.Contains x then
                    failwith "Derivative of exponential functions is not currently implemented"
                 else
                    match eval (fun _ -> nan) e2 with
                    | 1.0 -> differentiate e1 x
                    | 2.0 -> (Float 2.0) * e1 * (differentiate e1 x)
                    | _ -> e2 * (e1 ** (e2 - 1.0)) * (differentiate e1 x)
        
            | Plus terms -> 
                terms 
                |> List.map(fun t -> differentiate t x) 
                |> List.filter (fun e -> e<>Float 0.0)
                |> fun x -> 
                    match x.Length with
                    | 0 -> Float 0.0
                    | 1 -> x.[0]
                    | _ -> Plus x
        
            | Minus {sub1 = e1; sub2 = e2}  -> differentiate e1 x - differentiate e2 x

            | Log e -> differentiate e x / e

            | If (cond,vtrue,vfalse) -> If (cond, differentiate vtrue x, differentiate vfalse x)
            | Absolute _ -> failwith "Derivative of Expression.Absolute value not implemented"
            | Ceiling _  -> failwith "Expression.Ceiling not differentiable"
            | Floor _    -> failwith "Expression.Floor not differentiable"
            | Round _    -> failwith "Expression.Round not differentiable"
            | Modulo m   -> failwith "Derivative of Expression.Modulo not implemented"
    

(*
(*open Python.Runtime
open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Operators*)

type EncodingMethod = Fp | Real

//Note: Some of the optimizations using Eval when constructing expressions might introduce numerical errors?
type NumExpr = 
  | Var of string
  | Float of float
  | Divide of NumExpr * NumExpr
  | Power of NumExpr * NumExpr
  | Minus of NumExpr * NumExpr    
  | Times of NumExpr[]
  | Plus of NumExpr[] 
    static member (*) (a : NumExpr, b: NumExpr) = 
        match a, b with
        | Float 0.0, _ 
        | _, Float 0.0 -> Float 0.0
        | Float 1.0, Float k 
        | Float k, Float 1.0 -> Float k
        | Times t, Times t' -> NumExpr.MkTimes (Array.append t t')
        | Times t, t' -> NumExpr.MkTimes (Array.append t [|t'|])
        | t, Times t' -> NumExpr.MkTimes (Array.append [|t|] t')        
        | _ ->  NumExpr.MkTimes [|a; b|]        
        
//        | Float 1.0, b -> b
//        | a, Float 1.0 -> a                

//        | _ -> 
//            match a.Eval, b.Eval with 
//            | Some 0.0, _ 
//            | _, Some 0.0 -> Float 0.0
//            | Some 1.0, Some k 
//            | Some k, Some 1.0 -> Float k
//            | Some 1.0, None -> b
//            | None, Some 1.0 -> a                
//            | _ ->  Times [|a; b|]
    static member (*) (a : float, b: NumExpr) = Float a * b
    static member (*) (a : NumExpr, b: float) = a * Float b
    static member (+) (a : NumExpr, b: NumExpr) = 
        match a, b with
        | Float ka, Float kb -> Float (ka + kb)
        | Plus t, Plus t' ->  NumExpr.MkPlus (Array.append t t')
        | Plus t, t' ->  NumExpr.MkPlus (Array.append t [|t'|])
        | t, Plus t' ->  NumExpr.MkPlus (Array.append [|t|] t')        
        //| Float 0.0, b -> b
        //| a, Float 0.0 -> a
        | _ -> NumExpr.MkPlus [|a; b|]
//        | _ -> 
//            match a.Eval, b.Eval with
//            | Some ka, Some kb -> Float (ka + kb)
//            | Some 0.0, None -> b
//            | None, Some 0.0 -> a
//            | _ -> Plus [|a; b|]
    static member (+) (a : float, b: NumExpr) = Float a + b
    static member (+) (a : NumExpr, b: float) = a + Float b
    static member (-) (a : NumExpr, b: NumExpr) = 
        if b = Float 0.0 then a 
        else 
            Minus(a,b)        
//            match b.Eval with
//            | Some 0.0 -> a
//            | Some k -> Minus(a, Float k)
//            | None -> Minus(a,b)        
    static member (-) (a : float, b: NumExpr) = Minus (Float a, b)        
    static member (-) (a : NumExpr, b: float) = Minus (a, Float b)
    static member (~-) (a : NumExpr) =
        match a with 
        | Float k -> Float -k
        | Times [|Float -1.0; b|] -> b
        | _       -> (Float -1.0)*a//Minus (Float 0.0, a)
         
//        match a.Eval with 
//        | Some k -> Float -k
//        | None -> Minus (Float 0.0, a)

    static member (/) (a : NumExpr, b: NumExpr) = Divide (a, b)
    static member (/) (a : float, b: NumExpr) = Divide (Float a, b)
    static member (/) (a : NumExpr, b: float) = if b=1.0 then a else Divide (a, Float b)
    static member  Pow  (a : NumExpr, b: NumExpr) = 
        match a, b.Eval with 
        | _, Some 0.0 -> Float 1.0
        | a, Some 1.0 -> a     
        | a, Some 2.0 -> a*a
        | a, Some 3.0 -> a*a*a
        | a, Some 4.0 -> a*a*a*a
        | _ -> Power (a, b)
//        | _ -> 
//            match a.Eval, b.Eval with 
//            | _, Some 0.0 -> Float 1.0
//            | Some k, Some 1.0 -> Float k
//            | None, Some 1.0 -> a
//            | _ -> Power (a, b)
    static member  Pow  (a : float, b: NumExpr) = Power (Float a, b)
    static member  Pow  (a : NumExpr, b: float) = Power (a, Float b)
    

    static member MkTimes (e:NumExpr[]) = 
        if Array.isEmpty e then 
            failwith "ERROR: Empty multiplication term"   

        let reduced  = 
            e            
            |> Array.fold (fun (acc_k, acc_t, flag) x -> 
                match x with 
                | Float 0.0 -> acc_k, acc_t, true
                | Float 1.0 -> acc_k, acc_t, flag
                | Float k -> acc_k*k, acc_t, flag
                | Times t -> acc_k, List.append (t |> List.ofArray) acc_t, flag
                | _ -> acc_k, x::acc_t, flag) (1.0, [], false)
            |> fun (k,terms, flag) -> 
                if flag then [|Float 0.0|]
                elif k = 1.0 then terms |> Array.ofList
                else Float k::terms |> Array.ofList                    
                       
        match Array.length reduced with
        | 0 -> Float 1.0
        | 1 -> reduced.[0]
        | _ -> reduced |> Times            

    static member MkPlus (e:NumExpr[]) =    
        if Array.isEmpty e then 
            failwith "ERROR: Empty addition term"   

        let reduced = 
            e                                                  
            |> Array.fold (fun (acc_k, acc_t) x -> 
                match x with                 
                | Float k -> acc_k + k, acc_t            
                | Plus t -> acc_k, List.append (t |> Array.toList) acc_t
                | _ -> acc_k, x::acc_t) (0.0, [])
            |> fun (k,terms) -> 
                if k = 0.0 then
                    terms |> Array.ofList
                else
                    Float k::terms |> Array.ofList

        match Array.length reduced with
        | 0 -> Float 0.0
        | 1 -> reduced.[0]
        | _ -> reduced |> Plus
        
    
    static member Parse (s:string) =         
        let ( |>> ) a b c = Parser.(|>>) a b c
        let ( >>% ) a b c = Parser.(>>%) a b c
        let ( .>> ) a b c = Parser.(.>>) a b c
        let ( >>. ) a b c = Parser.(>>.) a b c
        let ( .>>. ) a b c = Parser.(.>>.) a b c
        let ( <|> ) a b = Parser.(<|>) a b
        let ( >>= ) a b = Parser.(>>=) a b

        let rec exp sp =
          let op = Parser.choice [ 
                     Parser.skw "-" >>% (fun a b -> Minus (a, b))
                     Parser.skw "+" >>% (fun a b -> Plus [|a;b|])
                   ] 
          Parser.chainl1Try 
            (Parser.choice 
              [ Parser.pTry(Parser.kw "-" >>. term sp >>= fun x -> Parser.preturn (Minus (Float 0.0, x)))
                Parser.pTry(term sp)]) op
        and term sp =
          let op = Parser.choice [
                     Parser.skw "*" >>% (fun a b -> a * b)
                     Parser.skw "/" >>% (fun a b -> Divide (a, b))                     
                   ] 
          in Parser.chainl1Try (factor sp) op
        and factor sp =
          let op = Parser.choice [ 
                     Parser.skw "^" >>% (fun (a:NumExpr) (b:NumExpr) -> NumExpr.Pow (a, b))
                     Parser.skw "**" >>% (fun (a:NumExpr) (b:NumExpr) -> NumExpr.Pow (a, b))
                   ] 
          in Parser.chainl1Try (irr sp) op
        and irr sp =
          let e  s = s |> Parser.paren (exp sp) in // inlining this causes stack overflow
          let es s = s |> Parser.paren (Parser.sepBy (exp sp) (Parser.kw ";")) in // inlining this causes stack overflow
          let t st =  Parser.choice [
                        Parser.pfloat .>> Parser.spaces |>> Float
                        Parser.pint32 .>> Parser.spaces |>> (float >> Float)                        
                        sp
                        e
                      ] st 
          in Parser.pTry t
                
        and var  = Parser.name .>> Parser.spaces |>> Var
        Parser.from_string (exp var) s
      
    member this.Eval = 
        match this with      
        | Var v         -> None           
        | Float f       -> Some f            
        | Times terms  -> 
            let evaluated = 
                terms 
                |> Array.choose(fun x -> x.Eval)
            if evaluated.Length = terms.Length then //all terms evaluate to a value
                evaluated
                |> Array.reduce (*)
                |> Some
            else
                None            
        | Divide (a, b) -> 
            match a.Eval, b.Eval with
            | Some x, Some y -> Some (x/y)
            | _ -> None            
        | Power (a, b)  -> 
            match a.Eval, b.Eval with
            | Some x, Some y -> Some (x**y)
            | _ -> None            
        | Plus terms -> 
            let evaluated = 
                terms 
                |> Array.choose(fun x -> x.Eval)
            if evaluated.Length = terms.Length then //all terms evaluate to a value
                evaluated
                |> Array.reduce (+)
                |> Some
            else
                None                
        | Minus (a, b)  -> 
            match a.Eval, b.Eval with
            | Some x, Some y -> Some (x - y)
            | _ -> None

    member this.Subst (p:Map<string,NumExpr>) =       
        match this with      
        | Var v         -> if p.ContainsKey v then p.[v] else Var v
        | Float f       -> Float f
        | Times terms   -> terms |> Array.map (fun x -> x.Subst p) |> NumExpr.MkTimes
        | Divide (a, b) -> (a.Subst p) / (b.Subst p)                        
        | Power (a, b)  -> (a.Subst p)**(b.Subst p)                        
        | Plus terms   -> terms |> Array.map (fun x -> x.Subst p) |> NumExpr.MkPlus
        | Minus (a, b)  -> (a.Subst p) - (b.Subst p)                        
            
    member this.GetVars =                  
        match this with      
        | Var v         -> v |> Set.singleton
        | Float f       -> Set.empty        
        | Divide (a, b)              
        | Power (a, b)                                    
        | Minus (a, b)  -> a.GetVars + b.GetVars
        | Plus terms 
        | Times terms -> terms |> Array.fold(fun acc x -> acc + x.GetVars) Set.empty
           
    member this.GetDenoms =                  
        match this with      
        | Var v         -> Set.empty
        | Float f       -> Set.empty        
        | Divide (a, b) -> b |> Set.singleton             
        | Power (a, b)  -> a.GetDenoms + b.GetDenoms             
        | Minus (a, b)  -> a.GetDenoms + b.GetDenoms             
        | Plus terms 
        | Times terms -> terms |> Array.fold(fun acc x ->acc + x.GetDenoms) Set.empty


    (*member this.ToPy (symbs:Map<string,PyObject>) : PyObject = 
        use gil = Py.GIL()
        match this with      
        | Var v         -> symbs.[v]
        | Float f       -> (PyObject.FromManagedObject f)
        | Divide (a, b) -> (a.ToPy symbs) ?/? (b.ToPy symbs)         
        //| Power (a, b)  -> (a.ToPy symbs) ?^? b                        
        | Power (a, b)  -> failwith "Power not implemented"
        | Minus (a, b)  -> (a.ToPy symbs) ?-? (b.ToPy symbs)
        | Plus terms    -> terms |> Array.fold (fun acc t -> acc ?+? t.ToPy symbs) (PyObject.FromManagedObject 0.0) 
        | Times terms   -> terms |> Array.fold (fun acc t -> acc ?*? t.ToPy symbs) (PyObject.FromManagedObject 1.0)*)

    //Derivative
    //NOTE 1: some optimizations might be possible by checking if sub-expressions are functions of the variable x (and treating them as constants otherwise)    
    member this.d (x:string) =         
        if not (this.GetVars.Contains x) then
            Float 0.0
        else
            match this with
            | Float _ -> Float 0.0

            | Var y -> if x = y then Float 1.0 else Float 0.0

            | Times terms  ->  
                let n = terms.Length

                if n = 0 then 
                    failwith "ERROR: Empty multiplication term"
                elif n=1 then 
                    terms.[0].d x
                else
                    [| for i in 1..n-2 do
                        yield (terms.[i].d x) * NumExpr.MkTimes (terms.[0..i-1]) * NumExpr.MkTimes (terms.[i+1..]) 
                    |]
                    |> Array.append
                        [| terms.[0].d x * NumExpr.MkTimes (terms.[1..])
                        ; terms.[n-1].d x * NumExpr.MkTimes (terms.[0..n-2])
                        |]
                
                    |> Array.map(fun e -> 
                        match e with 
                        | Times terms -> 
                            if terms |> Array.exists(fun e' -> e'.Eval = Some 0.0) then
                                Float 0.0
                            else
                                terms
                                |> Array.filter (fun e' -> e'<>Float 1.0)
                                |> fun x -> if x.Length>1 then NumExpr.MkTimes x else x.[0]                            
                        | _ -> e)
                    |> Array.choose(fun e -> 
                        match e.Eval with
                        | Some 0.0 -> None
                        | Some k   -> Some (Float k)                    
                        | None  -> Some e)
                    |> Array.filter (fun e -> e<>Float 0.0)
                    |> fun x -> if x.Length>1 then Plus x else x.[0]
                      
            | Divide (e1, e2) -> 
                let v = e2.GetVars
                if v.Contains x then
                    ((e1.d x) * e2 - (e2.d x) * e1)/(e2 ** 2.0)        
                else
                    (e1.d x)/e2
        
            | Power (e1, e2)  ->             
                 let v = e2.GetVars
                 if v.Contains x then
                    failwith "Derivative of exponential functions is not currently implemented"
                 else
                    match e2.Eval with
                    | Some 1.0 -> e2 * e1.d x               
                    | Some 2.0 -> e2 * e1 * e1.d x               
                    | _ -> e2 * (e1 ** (e2 - 1.0)) * e1.d x               
        
            | Plus terms -> 
                terms 
                |> Array.map(fun t -> t.d x) 
                |> Array.filter (fun e -> e<>Float 0.0)
                |> fun x -> if x.Length>1 then NumExpr.MkPlus x else x.[0]
        
            | Minus (e1, e2)  -> e1.d x - e2.d x



    member this.ToText use_latex = 
        match this with      
        | Var v         -> v 
        | Float f       -> 
            if System.Math.Ceiling(f) = f then
                sprintf "%i" (int f)
            else
                sprintf "%s" (f.ToString())
        | Times terms  -> 
            terms 
            |> Array.map(fun x -> 
                match x with
                | Plus _ 
                | Minus _ -> sprintf "(%s)" (x.ToText use_latex)
                | _ -> x.ToText use_latex)
            |> String.concat (if use_latex then " \\cdot " else "*")
            |> sprintf "(%s)"
        | Divide (a, b) -> 
            let e = 
                match a with
                | Plus _  
                | Minus _ -> sprintf "(%s)" (a.ToText use_latex)
                | _ -> sprintf "%s" (a.ToText use_latex)
            let d =     
                match b with
                | Plus _  
                | Minus _ -> sprintf "(%s)" (b.ToText use_latex)
                | _ -> sprintf "%s" (b.ToText use_latex)
            if use_latex then 
                sprintf "\\frac{%s}{%s}" e d
            else
                sprintf "%s/%s" e d
        | Power (a, b)  -> 
            let q = 
                match a with 
                | Var _
                | Float _ -> sprintf "%s" (a.ToText use_latex)
                |  _ -> sprintf "(%s)" (a.ToText use_latex)
            let e =
                match b with 
                | Var _
                | Float _ -> sprintf "%s" (b.ToText use_latex)
                |  _ -> sprintf "(%s)" (b.ToText use_latex)
            if use_latex then 
                sprintf "%s^{%s}" q e
            else
                sprintf "%s^(%s)" q e
        | Plus terms  -> sprintf "%s" (terms |> Array.map(fun x -> x.ToText use_latex) |> String.concat " + ")
        | Minus (Float 0.0, b)  -> 
            match b with 
            | Times _  
            | Power _ 
            | Var _
            | Float _ -> sprintf "-%s" (b.ToText use_latex)
            | _       -> sprintf "-(%s)" (b.ToText use_latex)

        | Minus (a, b)  -> sprintf "%s - %s" (a.ToText use_latex) (b.ToText use_latex)
    

    override this.ToString() = this.ToText false




type BoolExpr = 
    | Gt of NumExpr * NumExpr
    | Ge of NumExpr * NumExpr
    | Lt of NumExpr * NumExpr
    | Le of NumExpr * NumExpr
    | Eq of NumExpr * NumExpr                
    | And of BoolExpr[]
    | Or  of BoolExpr[]
    | Const of bool
    | Not of BoolExpr

    override this.ToString() =    
        match this with 
        | Not(Gt (a,b))  -> sprintf "%s \\ngtr %s" (a.ToString()) (b.ToString())
        | Not(Ge (a,b))  -> sprintf "%s \\ngeq %s" (a.ToString()) (b.ToString())
        | Not(Lt (a,b))  -> sprintf "%s \\nless %s" (a.ToString()) (b.ToString())
        | Not(Le (a,b))  -> sprintf "%s \\nleq %s" (a.ToString()) (b.ToString())
        | Not(Eq (a,b))  -> sprintf "%s  \\neq %s" (a.ToString()) (b.ToString())                
        | Gt (a,b)  -> sprintf "%s > %s" (a.ToString()) (b.ToString())
        | Ge (a,b)  -> sprintf "%s \\geq %s" (a.ToString()) (b.ToString())
        | Lt (a,b)  -> sprintf "%s < %s" (a.ToString()) (b.ToString())
        | Le (a,b)  -> sprintf "%s \\leq %s" (a.ToString()) (b.ToString())
        | Eq (a,b)  -> sprintf "%s = %s" (a.ToString()) (b.ToString())
        | And terms  -> sprintf "%s" (terms |> Array.map(fun e -> e.ToString()) |> String.concat " \\wedge ")
        | Or  terms  -> sprintf "%s" (terms |> Array.map(fun e -> e.ToString()) |> String.concat " \\vee ")
        | Const c -> if c then "\\top" else "\\bot"
        | Not a -> sprintf "\neg(%s)" (a.ToString())




    member this.Subst (p:Map<string,NumExpr>) =                 
        match this with 
        | Gt (a,b)  -> Gt (a.Subst p, b.Subst p)
        | Ge (a,b)  -> Ge (a.Subst p, b.Subst p)
        | Lt (a,b)  -> Lt (a.Subst p, b.Subst p)
        | Le (a,b)  -> Le (a.Subst p, b.Subst p)
        | Eq (a,b)  -> Eq (a.Subst p, b.Subst p)
        | And terms  -> And (terms |> Array.map(fun e -> e.Subst p))
        | Or  terms  -> Or (terms |> Array.map(fun e -> e.Subst p))
        | Const c -> Const c
        | Not a -> Not (a.Subst p)
                
    member this.GetVars =                        
        match this with 
        | Gt (a,b)  -> a.GetVars + b.GetVars
        | Ge (a,b)  -> a.GetVars + b.GetVars
        | Lt (a,b)  -> a.GetVars + b.GetVars
        | Le (a,b)  -> a.GetVars + b.GetVars
        | Eq (a,b)  -> a.GetVars + b.GetVars
        | And terms -> terms |> Array.fold(fun acc e -> acc + e.GetVars) Set.empty
        | Or  terms -> terms |> Array.fold(fun acc e -> acc + e.GetVars) Set.empty
        | Const c -> Set.empty
        | Not a -> a.GetVars


    member this.GetDenoms =                        
        match this with 
        | Gt (a,b)  -> a.GetDenoms + b.GetDenoms
        | Ge (a,b)  -> a.GetDenoms + b.GetDenoms
        | Lt (a,b)  -> a.GetDenoms + b.GetDenoms
        | Le (a,b)  -> a.GetDenoms + b.GetDenoms
        | Eq (a,b)  -> a.GetDenoms + b.GetDenoms
        | And terms -> terms |> Array.fold(fun acc e -> acc + e.GetDenoms) Set.empty
        | Or  terms -> terms |> Array.fold(fun acc e -> acc + e.GetDenoms) Set.empty
        | Const c -> Set.empty
        | Not a -> a.GetDenoms*)