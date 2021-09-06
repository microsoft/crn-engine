// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo
open System
open System.Windows
open System.Numerics
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.NumericLiterals
open Microsoft.FSharp.Core.Operators
open Oslo.Matrix 
open Oslo.SparseVector

[<JavaScript>]
module SparseMatrix =
    [<Literal>]
    let Delta : int = 1
    [<AllowNullLiteral>]
    type SparseMatrix(rows0 : int, cols0 : int, matrixItems : double[][],  matrixIndices : int[][], _count : int[]) = 
            
        //Many of these mutables are for compatilbility with WebSharper which otherwise generates "Failed to translate property access"
        //As I understand it may be actually the quotations system that's causing the limitation
        let mutable rows : int = rows0
        let mutable cols : int = cols0

        let mutable items : double[][] = matrixItems
        let mutable indices : int[][] = matrixIndices
        let mutable count : int[] = _count
           
        member this.SetItems(_items:double[][]) =
          items <- _items

        (*let mutable this.items : double[][] = if matrixItems = null then failwith("argument is null") else matrixItems//The nonzero elements of the sparse matrix
        let mutable this.indices : int[][] = if matrixIndices = null then failwith("argument is null") else matrixIndices  //The this.indices of the nonzero elements in the sparse matrix
        let mutable this.count : int[] = _count //Number of initialized elements*)
            
        //new(rows0 : int, cols0 : int, matrixItems : double[][],  matrixIndices : int[][], _count : int[]) = {rows = rows0; cols = cols0; items = matrixItems; indices = matrixIndices; count = _count}

        new(rows0 : int, cols0 : int) = SparseMatrix(rows0, cols0, Array.init rows0 (fun _ -> Array.empty), Array.init rows0 (fun _ -> Array.empty), Array.zeroCreate rows0)
            
            
            
        member this.Count with get() = count
                          and set value = count <- value

        member this.CountElem with get(i) = count.[i]
                              and set i value = count.[i] <- value
            
        /// <summary>Get row dimension.</summary>
        /// <returns>this.m, the number of rows</returns>
        member this.RowDimension with get() = rows

        /// <summary>Get column dimension.</summary>
        /// <returns>this.n, the number of columns.</returns>
        member this.ColumnDimension with get() = cols
            
        member this.Copy() = 
            let A = SparseMatrix(this.RowDimension, this.ColumnDimension, (Array.copy items), (Array.copy indices), (Array.copy count))
            A

        member this.Items with get(i) = items.[i]
                          and set(i) value = items.[i] <- value

        member this.Items with get(i, j) = items.[i].[j]
                          and set(i, j) value = items.[i].[j] <- value
                                    
        member this.Indices with get(i) = indices.[i]
                            and set(i) value = indices.[i] <- value
            
        member this.Indices with get(i, j) = indices.[i].[j]
                            and set(i, j) value = indices.[i].[j] <- value

        /// <summary>Dense version of a sparse matrix</summary>
        /// <returns>A matrix equivalent</returns>
        member this.DenseMatrix() = 

            let DM = Array.init (rows) (fun _ -> Array.zeroCreate cols)

            for i in 0..this.RowDimension - 1 do
                //This is faulty
                for j in 0..this.ColumnDimension - 1 do
                    Array.set DM.[i] indices.[i].[j] items.[i].[j]
            DM
            
        /// <summary>Accessor method for (i,j)th element</summary>
        member this.Item 
          /// <param name="i">Row index</param>
          /// <param name="j">Column index</param>
          /// <returns>(i,j)th element</returns>
          with get(i : int, j : int) =
            if (i < 0 || j < 0 || i >= rows || j >= cols) 
              then
                failwith "Element index is out of range"
              else
                if (indices.[i] = null) 
                  then
                    0.0
                  else
                    let jidx : int = 
                      match Array.tryFindIndex (fun x -> x = j) indices.[i].[0..(count.[i] - 1)] with
                      | Some x -> x
                      | None -> if Array.forall (fun x -> x < j) (indices.[i].[0..(count.[i] - 1)]) then ~~~(count.[i]) else match (Array.tryFindIndex (fun x -> x > j) (indices.[i].[0..(count.[i] - 1)])) with | Some x -> ~~~x | None -> -1 
                    if (jidx < 0) then
                      0.0
                    else
                      items.[i].[jidx]
            
          /// <param name="i">Row index</param>
          /// <param name="j">Column index</param>
          /// <param name="value">Value to assign</param>
          and set(i, j) value =
            if (i < 0 || j < 0 || i >= rows || j >= cols) then
              failwith "Element index is out of range"
            else
              if (Array.isEmpty indices.[i] || Array.length indices.[i]<1)
              then
                indices.[i] <- Array.zeroCreate Delta
                items.[i] <- Array.zeroCreate Delta
                indices.[i].[0] <- j
                items.[i].[0] <- value
                count.[i] <- 1                
              else
                let jidx : int = 
                  match Array.tryFindIndex (fun x -> x = j) indices.[i].[0..(count.[i] - 1)] with
                  | Some x -> x
                  | None -> if Array.forall (fun x -> x < j) (indices.[i].[0..(count.[i] - 1)]) then ~~~(count.[i]) else match (Array.tryFindIndex (fun x -> x > j) (indices.[i].[0..(count.[i] - 1)])) with | Some x -> ~~~x | None -> -1 
                if (jidx >= 0) 
                then
                  items.[i].[jidx] <- value
                else                                    
                  let indexToAdd = ~~~jidx;
                                                
                  if (count.[i] >= items.[i].Length) then                                                            
                    let delta = Math.Min(IncrementSize, cols - items.Length)
                    let newIndices : int[] = Array.zeroCreate ((Array.length indices) + delta)
                    let newItems : double[] = Array.zeroCreate ((Array.length items) + delta)
                    for k in 0..indexToAdd - 1 do Array.set newIndices k indices.[i].[k]
                    for k in 0..indexToAdd - 1 do Array.set newItems k items.[i].[k]
                                      
                    for k in 0..count.[i] - indexToAdd - 1 do
                      newIndices.[count.[i] - k] <- indices.[i].[count.[i] - k - 1]
                      newItems.[count.[i] - k] <- items.[i].[count.[i] - k - 1]
                    items.[i] <- newItems
                    indices.[i] <- newIndices

                  else
                    for k in 0..count.[i] - indexToAdd - 1 do
                      indices.[i].[count.[i] - k] <- indices.[i].[count.[i] - k - 1]
                      items.[i].[count.[i] - k] <- items.[i].[count.[i] - k - 1]
                                                
                  count.[i] <- count.[i] + 1
                  Array.set indices.[i] indexToAdd j
                  Array.set items.[i] indexToAdd value           
            
            
        /// <summary>Accessor method for ith row</summary>
        member this.Item  
          /// <param name="i">Row index</param>
          /// <returns>The ith row as a SparseVector</returns>
          with get(i) = SparseVector(items.[i], indices.[i], cols)
          /// <param name="i">Row index</param>
          /// <param name="value">Value to assign</param>
          and set i (value : SparseVector) = 
            indices.[i] <- value.Indices
            items.[i] <- value.Items
            count.[i] <- value.Length
                        
        /// <summary>Tranpose</summary>
        /// <returns></returns>
        member this.transpose() =         
          let At = SparseMatrix(this.ColumnDimension, this.RowDimension)

          for i in 0..this.RowDimension - 1 do
            for j in 0..(this.[i].Count) - 1 do
              At.[this.[i].Indices.[j]].[i] <- this.[i].Items.[j];

          At

        /// <summary>Method to rescale a row of a Sparse Matrix</summary>
        /// <param name="i">Index of the row to be scaled</param>
        /// <param name="j1">Lowest column entry to scale</param>
        /// <param name="j2">High column entry to scale</param>
        /// <param name="sf">Scale factor</param>
        member this.ScaleRow(i, j1, j2, sf : double) =            
          for k in 0..count.[i] - 1 do
            if ((indices.[i].[k] >= j1) && (indices.[i].[k] <= j2)) then
              items.[i].[k] <- items.[i].[k] * sf
            

        /// <summary>Switch rows of a sparse matrix</summary>
        member this.SwitchRows(i, j) =             
          let tempItems = items.[i]
          let tempIndices = indices.[i]
          let tempCount = count.[i]

          items.[i] <- items.[j]
          indices.[i] <- indices.[j]
          count.[i] <- count.[j]

          items.[j] <- tempItems
          indices.[j] <- tempIndices
          count.[j] <- tempCount
            
        /// <summary>Matrix addition for a sparse matrix</summary>
        /// <param name="B">The matrix to add</param>
        /// <returns>The result A + B</returns>
        member this.plus(B : SparseMatrix ) = 
          let C = SparseMatrix(rows, cols)

          for r in 0..rows - 1 do
            let v = (this.Item r).plus (B.Item r)
            let it = v.Items
            C.Items(r) <- it
            C.Indices(r) <- v.Indices
            C.CountElem(r) <- v.Count
          C
            
        /// <summary>Matrix subtraction for a sparse matrix</summary>
        /// <param name="B">The matrix to subtract</param>
        /// <returns>The result A - B</returns>
        member this.minus(B : SparseMatrix ) =
          let C = SparseMatrix(rows, cols)
          for r in 0..rows-1 do
            let v = (this.Item r).minus (B.Item r)
            C.Items(r) <- v.Items
            C.Indices(r) <- v.Indices
            C.CountElem(r) <- v.Count
          C

        /// <summary>Matrix multiplication</summary>
        /// <param name="v">Vector</param>
        /// <returns></returns>
        member this.times(v : double[]) = 
        
          let result = Array.zeroCreate rows
              
          for i in 0..rows - 1 do                        
            if (Array.isEmpty indices.[i] = false)
            then
              let mutable s = 0.0
              for k in 0..count.[i] - 1 do
                  s <- s +  items.[i].[k] * v.[indices.[i].[k]]
              Array.set result i s                      
                
          result
                        
        /// <summary>Matrix multiplication, y = v * A</summary>
        /// <param name="v">Vector</param>
        /// <returns>y</returns>
        member this.timesRight(v : double[]) =
          let v_ = v
          let mutable result = Array.zeroCreate cols
          for i in 0..rows - 1 do
            //if (not (Array.isEmpty this.indices.[i])) then
            let indices_i = indices.[i]
            let v_i = v_.[i]
            let items_i = items.[i]
                        
            for k in 0..indices_i.Length - 1 do
                let indicies_ik = indices_i.[k]
                result.[indicies_ik] <- result.[indicies_ik] + v_i * items_i.[k]
                
          result
            
        /// <summary>Matrix multiplication by a scalar</summary>
        /// <param name="s">Scalar</param>
        /// <returns>Scaled sparse matrix</returns>
        member this.times(s : double) = 
          let B = SparseMatrix(rows, cols)

          for i in 0..rows - 1 do
            if (Array.isEmpty indices.[i] = false)
            then
              B.Indices(i) <- Array.zeroCreate count.[i]
              B.Items(i) <- Array.zeroCreate count.[i]
              for j in 0..count.[i] - 1 do
                  B.Indices(i, j) <- indices.[i].[j]
                  B.Items(i, j) <- s * items.[i].[j]
              B.CountElem(i) <- count.[i]
                
          B


        /// <summary>Matrix multiplication by a scalar</summary>
        /// <param name="s">Scaling factor</param>
        /// <returns>Scaled sparse matrix</returns>
        member this.Mul(s : double) =
          for i in 0..rows - 1 do
            for j in 0..count.[i] - 1 do                          
              items.[i].[j] <- items.[i].[j] * s                    
                
          this

            
        /// <summary>Matrix right multiplication by a matrix</summary>
        /// <param name="B">Scaling factor</param>
        /// <returns>A * B where A is current sparce matrix</returns>
        member this.times(B : SparseMatrix) =
            
          //if (B = null) then failwith "Argument is zero"
          if (B.RowDimension <> cols) then failwith "Sparse matrix inner dimensions must agree."
        
          let C = SparseMatrix(rows, B.ColumnDimension)

          for i in 0..rows - 1 do
            if not (Array.isEmpty indices.[i]) then
              for j in 0..B.ColumnDimension - 1 do
                let mutable accumulator = 0.0
                for jj in 0..count.[i] - 1 do
                  let ii = indices.[i].[jj]
                  let b_ii = B.Indices(ii)
                  if (not (Array.isEmpty b_ii)) then
                    let idx = Array.BinarySearch(b_ii,0,B.CountElem(ii),j)
                    if (idx >= 0) then accumulator <- accumulator + items.[i].[jj] * B.Items(ii).[idx]

                    if accumulator <> 0.0 then C.Item(i, j) <- accumulator
                                        
          C

    /// <summary>Identity matrix in sparse form</summary>
    /// <param name="m">Row dimension</param>
    /// <param name="n">Column dimension</param>
    /// <returns>An m x n sparse identity matrix</returns>
    let inline Identity(m : int, n : int) = 
        let o =  Math.Min(m, n)
        let A = SparseMatrix(m, n)
        for i in 0..o - 1 do
            A.[i].[i] <- 1.0

        A

    let inline areEqual (A : SparseMatrix) (B : SparseMatrix) =
        
      let rec compare (a : SparseMatrix) (b : SparseMatrix) =
        let mutable s = 0.0
        for i in 0..a.RowDimension - 1 do
          for j in 0..a.ColumnDimension - 1 do
            s <- s + Math.Abs(a.[i].[j] - b.[i].[j])
        s = 0.0
        
      ((A.ColumnDimension) = (B.ColumnDimension)) &&
      ((A.RowDimension) = (B.RowDimension)) && 
      (compare A B)

    let inline areEqualEps (A : SparseMatrix) (B : SparseMatrix) eps = 
      let rec compare (a : SparseMatrix) (b : SparseMatrix) =
        let mutable s = 0.0
        for i in 0..a.RowDimension - 1 do
          for j in 0..a.ColumnDimension - 1 do
            s <- s + Math.Abs(a.[i].[j] - b.[i].[j])
        s < eps
        
      ((A.ColumnDimension) = (B.ColumnDimension)) &&
      ((A.RowDimension) = (B.RowDimension)) && 
      (compare A B)

                                            
        
