namespace Microsoft.Research.CRNEngine

//This matrix class is used only for the older RKF54 solver. It's from the PowerPack, don't use for anything else. 

//Neither JavaScript or WebSharper currently support 2d arrays - CDG

[<JavaScript>]
[<Sealed>]
type internal MatrixFS(values : float[], rows : int, columns : int) = 

    member m.Values = values

    /// Get the number of rows in a matrix
    member m.NumRows = rows

    /// Get the number of columns in a matrix
    member m.NumCols = columns

    member m.Item
          with get (i,j) = values.[i * columns + j]
          and  set (i,j) x = values.[i * columns + j] <- x

[<JavaScript>]
[<Sealed>]
type internal VectorFS(values : float[]) =

    member x.Values = values

    member v.Length = values.Length
    
    member m.NumRows = values.Length

    member v.Item
          with get i = values.[i]
          and  set i x = values.[i] <- x

/// Implementations of operations that will work for any type
[<JavaScript>]
module Impl = 
    
    let inline mul (x:float) (y:float) = x * y

    let mulDenseMatrixVec (a:MatrixFS) (b:VectorFS) =
        let nA = a.NumCols 
        let mA = a.NumRows
        let mB = b.NumRows 
        //if nA<>mB then invalidArg "b" "the two inputs do not have compatible dimensions"
        if nA<>mB then failwith "the two inputs do not have compatible dimensions"
        let arr = Array.zeroCreate mA 
        let arrA = a.Values 
        let arrB = b.Values 
        for i = 0 to mA - 1 do 
            let mutable r = 0.0 
            for k = 0 to nA - 1 do 
                //r <- r + mul (getArray2D arrA i k) arrB.[k]
                r <- r + (arrA.[i*nA + k]) * arrB.[k]
            arr.[i] <- r
        VectorFS(arr)

type internal matrixFS = MatrixFS
type internal vectorFS = VectorFS

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
[<JavaScript>]
module internal MatrixFS =

    let create m n x  =
        let values = Array.create (m*n) x
        MatrixFS(values, m, n)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
[<JavaScript>]
module internal VectorFS =

    let get (a:vectorFS) j   = a.[j]

    let toArray (v : vectorFS) = Array.init v.Length (get v)

    let ofList xs = VectorFS(Array.ofList xs)

    let create count x =
        let values = Array.create count x
        VectorFS(values)

    let fold  f z (a:vectorFS) = Array.fold f z a.Values
    (*
        let mutable acc = z
        for i = 0 to a.NumRows-1 do acc <- f acc a.[i]
        acc
     *)
//[<JavaScript>]
type internal MatrixFS with
    static member ( * )(a: MatrixFS,b : VectorFS) = Impl.mulDenseMatrixVec a b
    static member Multi (a: MatrixFS,b : VectorFS) = Impl.mulDenseMatrixVec a b

//[<JavaScript>]
type internal VectorFS with 
    member x.ToArray() = VectorFS.toArray x
    static member Multiply (k,m: VectorFS) = VectorFS(Array.map (fun v -> k*v) m.Values)
    static member Multiply (m: VectorFS,k) = VectorFS(Array.map (fun v -> v*k) m.Values)
    static member Add (k,m: VectorFS) = VectorFS(Array.map (fun v -> k+v) m.Values)
    static member Add (m: VectorFS,k) = VectorFS(Array.map (fun v -> v+k) m.Values)
    static member Add (m: VectorFS,k: VectorFS) = VectorFS(Array.map2 (fun v1 v2 -> v1+v2) m.Values k.Values)
    static member Subtract (k,m: VectorFS) = VectorFS(Array.map (fun v -> k-v) m.Values)
    static member Subtract (m: VectorFS,k) = VectorFS(Array.map (fun v -> v-k) m.Values)
    static member Subtract (m: VectorFS,k: VectorFS) = VectorFS(Array.map2 (fun v1 v2 -> v1-v2) m.Values k.Values)