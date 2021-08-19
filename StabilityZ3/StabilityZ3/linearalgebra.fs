module Microsoft.Research.Biology.StabilityZ3.LinearAlgebra


open Microsoft.Research.Biology.StabilityZ3
//open Microsoft.Research.Biology.StabilityZ3.Matrix

open Microsoft.Research.CRNEngine.Expression

type CharPolMethod = BasicCharPol | Faddeev | FaddeevTest
type StabilityMethod  = Routh | Hurwitz //| RouthArray

(* Generic transpose method *)
let transpose (matrix:'a list list) =
  if matrix.Length = 0 then failwith "Invalid matrix"  
  List.init matrix.[0].Length (fun i -> 
      List.init matrix.Length (fun j -> 
          matrix.[j].[i]))  

(* Faddeev-LeVerrier algorithm: https://en.wikipedia.org/wiki/Faddeev%E2%80%93LeVerrier_algorithm
   For possible alternative methods see: 
    - http://www4.ncsu.edu/~ipsen/ps/charpoly3.pdf 
    - https://www.emis.de/journals/ELA/ela-articles/articles/vol9_pp42-54.pdf

    \Sum_{k=0}^n c_k*\lambda^k where c_n = 1
    returns [c_0; c_1;..;c_{n-1}; c_n] //n+1 elements where n is the size of the matrix (c_n = 1.0)
*)
let characteristic_polynomial procedure (A:Matrix) =         
    let n  = A.Length    
        
    match procedure with
    | BasicCharPol    ->        
        //failwith "This procedure produces different results from the Faddeev one and must be tested further"

        let Ak = Array.init n (fun i -> A.Pow (i+1))
        let t  = Ak |> Array.map(fun x -> x.Trace)

        [|1..n|]
        |> Array.fold (fun (C:NumExpr list) k ->              
            let C' = C |> Array.ofList
            let c' = 
                Array.init k (fun j -> C'.[j] * t.[j])                
                |> Array.reduce (+)
            let c = Float 0.0 - (c' / Float (float k))        
            c::C   
        ) [Float 1.0]        
        |> Array.ofList    
        |> Array.rev
        |> Vector.Create 

    | FaddeevTest  ->             
        let c1 = Float 0.0 - A.Trace
        let B1 = A + Matrix.sI n c1

        List.fold (fun state k ->
            let (Am, Bm, cm) = List.head state            
            let An:Matrix = A * Bm
            let cn = Float 0.0 - (An.Trace / (Float (float k)))
            let Bn = An + Matrix.sI n cn                                    
            (An, Bn, cn)::state
        ) [A, B1, c1] [1..n]
        |> List.unzip3
        |> fun (_,_,C) -> C
        |> Array.ofList                                 
        |> Array.rev
        |> Array.append [|Float 1.0|]
        |> Vector.Create   


    | Faddeev ->     
        let Z = Matrix.Zero n
        List.fold (fun state k ->
            let (M, c) = List.head state
            let I = Matrix.sI n c
            let M' = A * M + I            
            let trace = Float 0.0 - (A * M').Trace        
            let c' = if k > 1 then trace / (Float (float k)) else trace
            (M', c') :: state
        ) [(Z, Float 1.0)] [1..n]
        |> Array.ofList
        |> Array.unzip
        |> snd        
        |> Array.rev
        |> Vector.Create   
        

(*  For the Routh-Hurwitz table method see http://www.chem.mtu.edu/~tbco/cm416/routh.html
*)
let routh_hurwitz procedure (cs:Vector) =
    let n = cs.Length
    if n <= 1
    then [||]
    else
        match procedure with 
        | Hurwitz ->         
            //1) Construct the Hurwitz matrix from the polynomial coefficients
            let R = Array.init (n-1) (fun i -> Array.init (n-1) (fun j -> Float 0.0))            
            for i in [0..n-2] do        
                R.[i].[i] <- cs.[i+1]
                for j in [1..i+1] do 
                    if i+j<n-1 then
                        R.[i+j].[i] <- cs.[i+1-j]
            
                    if i-j>=0 && i+j<n-1 then
                        R.[i-j].[i] <- cs.[i+j+1]          
            let Hurwitz = R |> Matrix.Create
            
            //2) Compute the Hurwitz determinants (determinants of principal minors)        
            [|for i in [0..n-2] do 
                let ids = [|0..i|]
                let pm = Hurwitz.Slice ids ids
                yield pm.Det
            |]

        (*| RouthArray -> 
            Array.init (n-1) (fun k ->
                let mk = 
                    Array.init (k+1) (fun i ->
                        Array.init (k+1) (fun j ->
                            let idx = 2*i-j
                            if (idx < -1 || idx >= n) then Float 0.0
                            elif idx = -1 then Float 1.0
                            else cs.[idx]
                        )
                    )
                    |> Matrix.Create
                mk.Det//, mk                            
            )
            |> Array.append [| cs.[n-1] |]
            //|> Array.append [| cs.[n-1], Matrix.Create [|[|cs.[n-1]|]|] |]
            //|> Array.unzip 
        *)
        | Routh -> 
            let h = System.Math.Ceiling(float n / 2.0) |> int    
            let RArray = Array.init n (fun i -> Array.init h (fun j -> Float 0.0))    
            let GetC i = if i<n then cs.[i] else Float 0.0
    
            for i in [0..h-1] do              
                RArray.[0].[i] <- GetC (i*2)
    
            for i in [0..h-1] do            
                RArray.[1].[i] <- GetC (i*2 + 1)
    
            for k in [2..n-1] do
                for i in [0..h-1] do
                    let x1 = RArray.[k-2].[0]
                    let y1 = RArray.[k-1].[0]
                    let x, y = 
                        if i < h-1 then
                            RArray.[k-2].[i+1], RArray.[k-1].[i+1]
                        else
                            Float 0.0, Float 0.0                                                  
                    let expr = (y1*x - y*x1) / y1                                  
                    RArray.[k].[i] <- expr
                                    
            RArray
            |> Array.map( fun row -> row.[0])

            
(*    
let eigenvalues (A:Matrix) = 
    let n = A.Length
    let eig = Array.init n (fun i -> MkVar(sprintf "eig_%i" i))

    for i in [0..n-2] do
        for j in [i+1..n-1] do
            varManager.Add( Not(Eq eig.[i] eig.[j]))

    //eigenvalue constraints
    eig
    |> Array.map(fun l -> (A - Matrix.sI n l).Det |> Eq0)
    |> Array.iter(fun d -> varManager.Add(d))   

    eig
*)


open Microsoft.Z3
//only Faddeev
let z3_characteristic_polynomial (z3:Context) (A:Z3Matrix) =         
    let one = z3.MkReal(1):>ArithExpr
    let n  = A.Length
    if n > 1
    then 
        let Z = Z3Matrix.Zero z3 n
        List.fold (fun state (k:int) ->
            let (M, c) = List.head state
            let I = Z3Matrix.sI z3 n c
            let M' = Z3Matrix.Plus(z3, Z3Matrix.Times(z3, A, M), I)
            let trace = -(Z3Matrix.Times(z3, A, M').Trace)
            let K = z3.MkReal(k) :> ArithExpr               //k<>0
            let c' = if k > 1 then trace / K else trace
            (M', c') :: state
        ) [(Z, one)] [1..n]
        |> Array.ofList
        |> Array.unzip
        |> snd        
        |> Array.rev
    else
        [| one; -A.[0].[0] |]
    |> Z3Vector.Create z3
        

//only the table methods
let z3_routh_hurwitz procedure (z3:Context) (cs:Z3Vector) =
    let n = cs.Length
    if n <= 1
    then [||], [||]
    else
        let zero = z3.MkReal(0) :> ArithExpr
    
        match procedure with 
        | Routh ->           
            let h = System.Math.Ceiling(float n / 2.0) |> int    
            let RArray = Array.init n (fun i -> Array.init h (fun j -> zero))    
            let GetC i = if i<n then cs.[i] else zero
    
            for i in [0..h-1] do              
                RArray.[0].[i] <- GetC (i*2)
    
            for i in [0..h-1] do            
                RArray.[1].[i] <- GetC (i*2 + 1)
    
            let mutable cst = List.empty

            for k in [2..n-1] do
                for i in [0..h-1] do
                    let x1 = RArray.[k-2].[0]
                    let y1 = RArray.[k-1].[0]
                    let x, y = 
                        if i < h-1 then
                            RArray.[k-2].[i+1], RArray.[k-1].[i+1]
                        else
                            zero, zero                                             
                    cst <- z3.MkNot(z3.MkEq(y1,zero))::cst //prevent division by 0
                    let expr = (y1*x - y*x1) / y1                                  
                    RArray.[k].[i] <- expr
                                    
            let col1 = RArray |> Array.map( fun row -> row.[0])

            col1, Array.ofList cst

        | Hurwitz -> 
            //1) Construct the Hurwitz matrix from the polynomial coefficients
            let R = Array.init (n-1) (fun i -> Array.init (n-1) (fun j -> zero))            
            for i in [0..n-2] do        
                R.[i].[i] <- cs.[i+1]
                for j in [1..i+1] do 
                    if i+j<n-1 then
                        R.[i+j].[i] <- cs.[i+1-j]
            
                    if i-j>=0 && i+j<n-1 then
                        R.[i-j].[i] <- cs.[i+j+1]          
            let Hurwitz = Z3Matrix.Create(z3, R)
            
            //2) Compute the Hurwitz determinents (determinants of principal minors)        
            let result = 
                [|for i in [0..n-2] do 
                    let ids = [|0..i|]
                    let pm = Hurwitz.Slice ids ids
                    yield pm.Det
                |]
        
            result, [||]

        //| RouthArray ->   failwith "Not implemented"
    
