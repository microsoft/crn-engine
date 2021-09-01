// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.Complex

//open Microsoft.Research.Biology.TuringZ3.Encoding
open Microsoft.Z3

//Microsoft.Z3.Global.SetParameter("verbose","10")
let z3 = new Context()


type Complex = 
    { r : ArithExpr
    ; i : ArithExpr
    ; name : string option
    }
    static member MkNum (x:float) = z3.MkReal(x.ToString()) :> ArithExpr 
    static member Zero  = z3.MkReal("0.0") :> ArithExpr 


    static member MkVar (name:string) = 
        { r = z3.MkRealConst(sprintf "%s_r" name) :> ArithExpr 
        ; i = z3.MkRealConst(sprintf "%s_i" name) :> ArithExpr 
        ; name = Some name
        }

    static member Create(a,b) = 
        { r = a
        ; i = b
        ; name = None
        }

    static member Create(a:float) = 
        { r = Complex.MkNum a
        ; i = Complex.Zero
        ; name = None
        }

    static member (*) (a : Complex, b:Complex) =
         Complex.Create(a.r*b.r - a.i*b.i, a.r*b.i + a.i*b.r)

//    static member (*) (a : float, b:Complex) =
//        let a' = Complex.MkNum a
//        Complex.Create(a' * b.r, a' * b.i)

    static member (+) (a : Complex, b:Complex) =
        Complex.Create(a.r + b.r, a.i + b.i)

//    static member (+) (a : Complex, b:float) =
//        let b' = Complex.MkNum b
//        Complex.Create(a.r + b', a.i)

    static member (-) (a : Complex, b:Complex) =
         Complex.Create(a.r - b.r, a.i - b.i)

    member this.Eq (x : Complex) =         
         z3.MkAnd(z3.MkEq(this.r, x.r), z3.MkEq(this.i, x.i))

    member this.Simplify() = 
        { r = this.r.Simplify() :?> ArithExpr
        ; i = this.i.Simplify() :?> ArithExpr
        ; name = this.name
        }    
        
    static member (/) (a : Complex, b:Complex) =
         { r = (a.r*b.r + a.i*b.i)/(b.r*b.r + b.i*b.i)
         ; i = (a.i*b.r - a.r*b.i)/(b.r*b.r + b.i*b.i)
         ; name = None
         }



    //compare operations should be checked
    static member Gt (a : Complex, b:Complex) =
        z3.MkAnd(z3.MkGt(a.r, b.r), z3.MkGe(a.i, b.i))

    static member Ge (a : Complex, b:Complex) =
        z3.MkAnd(z3.MkGe(a.r, b.r), z3.MkGe(a.i, b.i))

    static member Lt (a : Complex, b:Complex) =
        z3.MkAnd(z3.MkLt(a.r, b.r), z3.MkLe(a.i, b.i))

    static member Le (a : Complex, b:Complex) =
        z3.MkAnd(z3.MkLe(a.r, b.r), z3.MkLe(a.i, b.i))

    static member Eq (a : Complex, b:Complex) =
        z3.MkAnd(z3.MkEq(a.r, b.r), z3.MkEq(a.i, b.i))

    member this.Eval (M:Model) = 
        { r = M.Eval(this.r, true) :?> ArithExpr       
        ; i = M.Eval(this.i, true) :?> ArithExpr
        ; name = this.name
        }

    member this.Name = 
        match this.name with
        | Some n -> n
        | None  -> "Complex"


    override this.ToString() = 
        let num2str (x:ArithExpr) = 
            let precision = 5u
            if x.IsNumeral then
                sprintf "%s" (x.ToString())
            elif x.IsAlgebraicNumber then
                let x' = x :?> AlgebraicNum            
                sprintf "%s" (x'.ToDecimal(precision))
            elif x.IsRatNum then
                let x' = x :?> RatNum            
                sprintf "%s" (x'.ToDecimalString(precision))
            else
                sprintf "[CE]"
        
        sprintf "(%s, %si)" (num2str this.r) (num2str this.i)    
