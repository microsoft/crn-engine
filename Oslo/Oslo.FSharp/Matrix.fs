// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo

[<JavaScript>]
type Matrix =
    {
        /// NOTE: currently this representaiton is much faster on CLR than a flattened
        /// vector/array for large matrices, probably due to better cache locality.
        /// In JS, both this representation and a flattened packed Float64Array do well.
        MatrixData : Vector []
    }

    member mx.Item
        with [<InlineJavaScript>] get (i, j) = mx.MatrixData.[i].[j]
        and [<InlineJavaScript>] set (i, j) d = mx.MatrixData.[i].[j] <- d

    member mx.Item
        with get i = mx.MatrixData.[i]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<JavaScript>]
module Matrix =
    type M = Matrix
    type V = Vector

    let zeros rows cols =
        { MatrixData = Array.init rows (fun _ -> Vector.zeros cols) }

    let numRows m =
        m.MatrixData.Length

    let numCols m =
        Vector.length m.MatrixData.[0]

    let copy mx =
        { mx with MatrixData = Array.map Vector.copy mx.MatrixData }

    let ofArray (xs: double[][]) =
        { MatrixData = Array.map Vector.ofArray xs }

    let ofRows (rows: V []) =
        { MatrixData = Array.map Vector.copy rows }

    let copyCol (x: M) (j: int) =
        let n = numRows x
        let v = Vector.zeros n
        for i in 0 .. n - 1 do
            v.[i] <- x.[i,j]
        v

    let copyRow (x: M) (i: int) =
        Vector.copy x.[i]

    let identity (rows: int) (n: int) =
        let r = zeros rows n
        for i in 0 .. min (rows - 1) (n - 1) do
            r.[i, i] <- 1.0
        r

    let sameSizeCheck (a: M) (b: M) =
        let m1 = numRows a
        let n1 = numCols a
        let m2 = numRows b
        let n2 = numCols b
        if m1 <> m2 || n1 <> n2 then
            failwith "Matrix dimensions do not match"

    let copyTo x y =
        sameSizeCheck x y
        Array.iter2 Vector.copyTo x.MatrixData y.MatrixData

    let vectorProduct v1 v2 =
        let m = Vector.length v1
        let n = Vector.length v2
        let res = zeros m n 
        for i = 0 to m - 1 do
            for j = 0 to n - 1 do
                res.[i, j] <- v1.[i] * v2.[j]
        res

    ///Implement matrix per-element mapping
    /// r.[i].[j] = f(A.[i].[j]) where
    /// (1) A is m x n
    /// (2) f is doble -> double
    /// (3) r is m x n
    [<JavaScript>]
    let matrMap (f : double -> double) (A : M) =
        let dim1 = numRows A
        let dim2 = numCols A
        let r = zeros dim1 dim2
        for i = 0 to dim1 - 1 do
                let r_i = r.[i]
                for j = 0 to dim2 - 1 do
                    r_i.[j] <- A.[i].[j] |> f
        r

    /// r = A * v where
    /// (1) A is m x n
    /// (2) v is n x 1
    /// (3) r is mutated in place
    let matrMultVecStore (A: M) (v: V) (r: double[]) =
        for i in 0 .. numRows A - 1 do
            let mutable s = 0.0
            let row = A.[i]
            for j in 0 .. numCols A - 1 do
                s <- s + row.[j] * v.[j]
            r.[i] <- s

    let matrMultVec (A: M) (v: V) =
        let store = (Vector.fastCreate (numRows A))
        matrMultVecStore A v store
        store |> Vector.ofArray

    //Implements matrix addition
    //result is C = A + B,
    //A is m x n matrix,
    //B is m x n matrix.
    let add (A: M) (B: M) : M =
        sameSizeCheck A B
        { MatrixData = Array.map2 Vector.add A.MatrixData B.MatrixData }

    //Implements matrix subtraction
    //result is C = A - B,
    //A is m x n matrix,
    //B is m x n matrix.
    let subtract (A: M) (B: M) : M =
        sameSizeCheck A B
        { MatrixData = Array.map2 Vector.subtract A.MatrixData B.MatrixData }

    //Implements scalar by matrix multiplication
    //result is C = s * A ,
    //s is scalar,
    //A is m x n matrix.
    let scalMultMatr (s: double) (A: M) : M =
        { MatrixData = Array.map (Vector.scale s) A.MatrixData }

    //Implements scalar by matrix multiplication
    //result is C = A * s ,
    //A is m x n matrix,
    //s is scalar.
    let matrMultScal (A: M) (s: double) : M =
        scalMultMatr s A

    let times (A: M) (s: double) : M =
        scalMultMatr s A

    (*let areEqual v1 v2 =
        numRows v1 = numRows v2
        && Array.forall2 Vector.areEqual v1.MatrixData v2.MatrixData*)

    let areEqualEps (v1: M) (v2: M) (eps: double) : bool =
        numRows v1 = numRows v2
        && Array.forall2 (fun x y -> Vector.areEqualEps x y eps) v1.MatrixData v2.MatrixData

    let toArray m =
        Array.init (numRows m) (fun i -> Vector.toArray (copyRow m i))

    let transpose m =
        let rows = numRows m
        let cols = numCols m
        let r = zeros cols rows
        for i in 0..(rows-1) do
            for j in 0..(cols-1) do
                r.[j].[i] <- m.[i].[j]
        r

    //Implements matrix multiplication by matrix
    //result is C = A * B,
    //A is m x n matrix,
    //B is n x k matrix.
    [<JavaScript>]
    let matrMultMatr A B =
        let dim1 = numRows A
        let dim2 = numCols A
        let dim3 = numCols B
        if dim2 <> numRows B then failwith "Matrix dimensions do not match"
        let r = zeros dim1 dim3
        for i = 0 to dim1 - 1 do
            let A_i = A.[i]
            let r_i = r.[i]
            for j = 0 to dim3 - 1 do
                let mutable sum = 0.0
                for k in 0 .. dim2 - 1 do
                    sum <- sum + A_i.[k] * B.[k, j]
                r_i.[j] <- sum
        r

[<AutoOpen>]
[<JavaScript>]
module MatrixNotation =

    [<InlineJavaScript>]
    let ( $ ) a b = Matrix.vectorProduct a b
