namespace Microsoft.Research.Biology.StabilityZ3

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Z3

type Z3Vector = 
    { values : ArithExpr[]
    ; z3     : Context
    }        

    static member exprSum (z3:Context) (e:ArithExpr[]) =                 
        let nonzeros = e |> Array.filter (fun aij -> aij <> (z3.MkReal(0) :> ArithExpr))
        match Array.length nonzeros with
        | 0 -> z3.MkReal(0) :> ArithExpr
        | 1 -> nonzeros.[0]
        | _ -> nonzeros |> z3.MkAdd
                

    member this.Sum = this.values |> Z3Vector.exprSum this.z3

    static member sum (v:Z3Vector) = v.Sum

    static member Times (z3:Context, v:Z3Vector, v':Z3Vector) =    
        Array.map2 (fun r c -> r * c) v.values v'.values
        |> Z3Vector.exprSum z3

    static member Plus (z3:Context, v:Z3Vector, v':Z3Vector) =    
        Array.map2 (fun r c -> r + c) v.values v'.values
        |> Z3Vector.Create z3

    static member Minus (z3:Context, v:Z3Vector, v':Z3Vector) =    
        Array.map2 (fun r c -> r - c) v.values v'.values
        |> Z3Vector.Create z3

    static member Create (z3:Context) (e:ArithExpr[]) = {values = e; z3 = z3}

    member this.Item i = this.values.[i]

    member this.Slice elems = 
        elems 
        |> Array.map (Array.get this.values) 
        |> Z3Vector.Create this.z3

    member this.Skip n = 
        {values = this.values |> Seq.skip n |> Array.ofSeq; z3 = this.z3}

    static member skip n (v:Z3Vector) = v.Skip n

    member this.Length = this.values.Length   
  

type Z3Matrix = 
    { values : Z3Vector[] //row vectors
    ; z3     : Context
    }
     

    static member Create (z3:Context, E:ArithExpr[][]) = 
        {values = E |> Array.map(fun e -> e |> Z3Vector.Create z3)
        ; z3 = z3
        }

    static member Create (z3:Context, E:Z3Vector[]) = 
        {values = E; z3 = z3}
    
    member this.Length = this.values.Length

    member this.Item i = this.values.[i] 

    member this.Col i = this.values |> Array.map (fun x -> x.[i]) |> Z3Vector.Create this.z3

    static member Times (z3:Context, A : Z3Matrix, B:Z3Matrix) =    
        let n = A.Length
        let vec = 
            Array.init n (fun i -> 
                Array.init n (fun j ->
                    Z3Vector.Times (z3, A.[i], (B.Col j))
                )
            )
        Z3Matrix.Create (z3, vec)
    
    static member Plus (z3:Context, A : Z3Matrix, B:Z3Matrix) =    
        let n = A.Length
        let vec = Array.init n (fun i -> Z3Vector.Plus (z3, A.[i], B.[i]))
        Z3Matrix.Create (z3, vec)

    static member Minus (z3:Context, A:Z3Matrix, B:Z3Matrix) =    
        let n = A.Length
        let vec = Array.init n (fun i -> Z3Vector.Minus(z3,A.[i],B.[i]))
        Z3Matrix.Create (z3, vec)
    
    member this.Diagonal = 
          this.values
          |> Array.mapi (fun i Ai -> Ai.[i])
          |> Z3Vector.Create this.z3

    member this.Trace =  this.Diagonal.Sum
          
    static member sI (z3:Context) n (c:ArithExpr) = 
        let vec = Array.init n (fun i -> Array.init n (fun j -> if i=j then c else z3.MkReal(0) :> ArithExpr))
        Z3Matrix.Create (z3, vec)

    static member I z3 n  = Z3Matrix.sI z3 n (z3.MkReal(1) :> ArithExpr)        

    static member Zero (z3:Context) n = 
        let vec = Array.create n (Array.create n (z3.MkReal(0) :> ArithExpr)) 
        Z3Matrix.Create (z3, vec)

    member this.Pow (k:int) =    
        if k = 0 then 
            Z3Matrix.I this.z3 this.Length
        else
            Array.init k (fun i -> this)
            |> Array.reduce (fun a b -> Z3Matrix.Times(this.z3, a, b))

    member this.Slice rows cols = 
        //let cols'= cols |> Array.sort
        let vec = 
            rows
            |> Array.map this.Item
            |> Array.map (fun x -> x.Slice cols)        
        Z3Matrix.Create(this.z3, vec)

    member this.Det  = 
      let n = this.Length
      match n with 
      | 1 -> this.[0].[0]      
      | _ -> 
        [|0..n-1|] 
        |> Array.map (fun i -> 
            let cols = Array.init (n-1) (fun j -> (i+j+1)%n)
            let A'  = this.Slice [|1..n-1|] cols
            let det = this.[0].[i] * A'.Det                           //NOTE: Maybe replace this NumExpr with a variable?            
            if i%2 = 0 then 
                det
            else
                -det
            )
        |> Z3Vector.Create this.z3
        |> Z3Vector.sum            




open Microsoft.Research.CRNEngine.Expression

type Vector = 
    { values : NumExpr[]
    }                        

    member this.Sum = this.values |> List.ofArray |> NumExpr.Plus

    static member sum (v:Vector) = v.Sum

    static member (*) (v:Vector, v':Vector) =    
        Array.map2 (fun r c -> r * c) v.values v'.values
        |> List.ofArray
        |> NumExpr.Plus

    static member (+) (v:Vector, v':Vector) =    
        Array.map2 (fun r c -> r + c) v.values v'.values
        |> Vector.Create        

    static member (-) (v:Vector, v':Vector) =    
        Array.map2 (fun r c -> r - c) v.values v'.values
        |> Vector.Create  

    static member Create(e:NumExpr[]) = {values = e}

    member this.Item i = this.values.[i]

    member this.Slice elems = 
        elems 
        |> Array.map (Array.get this.values) 
        |> Vector.Create

    member this.Skip n = 
        {values = this.values |> Seq.skip n |> Array.ofSeq}

    static member skip n (v:Vector) = v.Skip n

    member this.Length = this.values.Length

    static member rev (v:Vector) = v.values |> Array.rev |> Vector.Create
    
//    member this.Eval (M:Microsoft.Z3.Model) = 
//        this.values
//        |> Array.map(fun v -> v |> Eval M)
//        |> Vector.Create
//        
//    static member eval M (v:Vector) = v.Eval M

    static member map (f:NumExpr -> NumExpr) (v:Vector) = 
        v.values 
        |> Array.map f 
        |> Vector.Create

    override this.ToString() = 
        this.values
        |> Array.map (ExpressionFunctions.ToText false)
        |> String.concat ", "

    static member toString (v:Vector) = v.ToString()

    member this.Transpose = 
        this.values
        |> Array.map(fun x -> [|x|] |> Vector.Create)
        |> Matrix.Create
    
    static member transpose (v:Vector) = v.Transpose

    static member Eq (v:Vector) (v':Vector) =   
        Array.map2  (fun a b -> BEq(a,b)) v.values v'.values
        |> Array.reduce (fun a b -> BAnd (a,b))
 

and Matrix = 
    { values : Vector[] //row vectors
    }
    static member Create (E:NumExpr[][]) = 
        {values = E |> Array.map(fun e -> e |> Vector.Create)}

    static member Create (E:Vector[]) = 
        {values = E}
    
    member this.Length = this.values.Length
    member this.RowCount = this.values.Length
    member this.ColumnCount = this.values.[0].Length

    member this.Item i = this.values.[i] 

    member this.Col i = this.values |> Array.map (fun x -> x.[i]) |> Vector.Create

    static member (*) (A : Matrix, B:Matrix) =    
        let n = A.Length
        Array.init n (fun i -> 
            Array.init n (fun j ->
                A.[i] * (B.Col j)                 
            )
        )
        |> Matrix.Create
    
    static member (*) (A : Matrix, v:Vector) =    
        let n = v.Length
        Array.init n (fun i ->A.[i] * v)        
        |> Vector.Create

    static member (+) (A : Matrix, B:Matrix) =    
        let n = A.Length
        Array.init n (fun i -> A.[i] + B.[i])           
        |> Matrix.Create               

    static member (-) (A : Matrix, B:Matrix) =    
        let n = A.Length
        Array.init n (fun i -> A.[i] - B.[i])           
        |> Matrix.Create      
    
    member this.Diagonal = 
          this.values
          |> Array.mapi (fun i Ai -> Ai.[i])
          |> Vector.Create

    member this.Trace =  this.Diagonal.Sum
          
    static member sI n (c:NumExpr) = 
        Array.init n (fun i -> Array.init n (fun j -> if i=j then c else Float 0.0))
        |> Matrix.Create

    static member I n  = Matrix.sI n (Float 1.0)        

    static member Zero n = 
        Array.create n (Array.create n (Float 0.0)) 
        |> Matrix.Create

    member this.Pow (k:int) =    
        if k = 0 then 
            Matrix.I this.Length
        else
            Array.init k (fun i -> this)
            |> Array.reduce (*)           

    member this.Slice rows cols =         
        rows
        |> Array.map this.Item
        |> Array.map (fun x -> x.Slice cols)
        |> Matrix.Create    

    member this.Det  = 
      let n = this.Length
      match n with 
      | 1 -> this.[0].[0]      
      | _ -> 
        [|0..n-1|] 
        |> Array.map (fun i -> 
            let cols = Array.init (n-1) (fun j -> (i+j+1)%n)
            let A'  = this.Slice [|1..n-1|] cols
            let det = this.[0].[i] * A'.Det                           //NOTE: Maybe replace this NumExpr with a variable?            
            if i%2 = 0 then 
                det
            else
                Float 0.0 - det
            )
        |> Vector.Create
        |> Vector.sum        
                        
    member this.Transpose =     
        [|0..this.Length|]
        |> Array.map (fun i -> this.values |> Array.map (fun x -> x.[i]))        
        |> Matrix.Create

    static member transpose (A:Matrix) = A.Transpose

//    member this.Eval (M:Microsoft.Z3.Model) = 
//        this.values     
//        |> Array.map (fun r -> r.Eval M)
//        |> Matrix.Create
//
//    static member eval M (m:Matrix) = m.Eval M

    static member map (f:NumExpr -> NumExpr) (m:Matrix) = 
        m.values
        |> Array.map (fun r -> r |>  Vector.map f)
        |> Matrix.Create

    member this.ToLatex() = 
        this.values     
        |> Array.map (fun r -> 
            r.values 
            |> Array.map (ExpressionFunctions.ToText true)
            |> String.concat " & "
            )
        |> String.concat "\\\\"
        |> sprintf "\\begin{bmatrix}%s\\end{bmatrix}"

    static member toLatex (m:Matrix) = m.ToLatex()

    override this.ToString() =     
        this.values
        |> Array.map(fun v -> 
            v.values             
            //|> Array.map(fun x -> eval (fun _ -> nan) x).ToString())
            |> Array.map (ExpressionFunctions.ToText false)
            |> String.concat ", "        
            )
        |> String.concat "; "    
    
    static member toString (m:Matrix) = m.ToString()

    static member toText latex (m:Matrix) = if latex then m.ToLatex() else m.ToString()
        
    member this.ToString name =  sprintf "%s=[%s];" name (this.ToString())
           
    member this.Encode vars z3 = 
        let vecs = 
            this.values
            |> Array.map(fun vec -> vec.values |> Array.map (fun e -> Encoding.NumExpr2RealRec vars z3 e))
        Z3Matrix.Create(z3, vecs)  

    static member encode vars z3 (A:Matrix) = A.Encode vars z3
            
    static member Eq (A:Matrix) (A':Matrix) =     
        Array.map2 Vector.Eq A.values A'.values
        |> Array.reduce (fun a b -> BAnd (a,b))



//
//    member this.Print() = 
//        this.values |> Array.map(fun v -> v.values |> Array.map (fun e -> e.Eval.Value.ToString()) |> String.concat ",\t") |> String.concat ";\n"    
//        |> sprintf "%s"
//
//    member this.Print p = 
//        this.values |> Array.map(fun v -> v.values |> Array.map (fun e -> e.Subst(p).Eval.Value.ToString()) |> String.concat ",\t") |> String.concat ";\n"    
//        |> sprintf "%s"
