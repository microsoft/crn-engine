// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo

open System
open SparseMatrix

//Provides implementation of Gaussian elimination with partial pivoting
[<JavaScript>]
module Gauss =

    module private SolveCoreUtility =

        let inline flipValues (v: Vector) (i: int) (j: int) =
            let temp = v.[i]
            v.[i] <- v.[j]
            v.[j] <- temp

        let inline flipRows (A: Matrix) (r1: int) (r2: int) =
            let r1 = A.[r1]
            let r2 = A.[r2]
            for k = 0 to Vector.length r1 - 1 do
                let x = r1.[k]
                r1.[k] <- r2.[k]
                r2.[k] <- x

        // NOTE: this is often the performance bottleneck, therefore we use
        // internal representations of Vector and Matrix, and we specialize code
        // for JavaScript separately.
        [<DirectJavaScript "
            var data = $A1.MatrixData,
                jR = (data.length - $j - 1) | 0,
                mult = +$b1[jR],
                b = data[jR];
            for (var i = 0; i < jR; i++) {
                var a = data[i], aj = +a[$j];
                if (aj !== 0) {
                    for (var k = $j|0 + 1; k < a.length; k++) {
                        a[k] = +a[k] - aj * +b[k];
                    }
                    $b1[i] = +$b1[i] - aj * mult;
                    a[$j] = 0.0;
                }
            }">]
        let processOtherRows (A1: Matrix) (b1: Vector) (j: int) =
            let jR = Matrix.numRows A1 - j - 1
            let b = A1.[jR]
            for i = 0 to jR - 1 do
                let a = A1.[i]
                let aj = a.[j]
                if aj <> 0.0 then
                    for k = j + 1 to Vector.length a - 1 do
                        a.[k] <- a.[k] - aj * b.[k]
                    b1.[i] <- b1.[i] - aj * b1.[jR]
                    a.[j] <- 0.0

        let divideFrom (v: Vector) (r: double) (i: int) =
            for k in i .. Vector.length v - 1 do
                v.[k] <- v.[k] / r

        let solveCore (A1: Matrix) (b1: Vector) =
            let mutable maxIdx = 0
            for j = 0 to Matrix.numRows A1 - 1 do
                let jR = Matrix.numRows A1 - j - 1
                // Find row with largest absolute value of j-st element
                maxIdx <- 0
                for i = 0 to jR do
                    if abs A1.[i].[j] > abs A1.[maxIdx].[j] then
                        maxIdx <- i
                if abs A1.[maxIdx].[j] < 1e-12 then
                    failwith "Cannot apply Gauss method"
                // Divide this row by max value
                do  let row = A1.[maxIdx]
                    let v = row.[j]
                    divideFrom row v (j + 1)
                    b1.[maxIdx] <- b1.[maxIdx] / v
                    row.[j] <- 1.0
                // Move this row to bottom
                if maxIdx <> jR then
                    flipRows A1 maxIdx jR
                    flipValues b1 maxIdx jR
                processOtherRows A1 b1 j

    /// Solves system of linear equations A * x = b
    /// using Gaussian elimination with partial pivoting.
    let SolveCore (A: Matrix) (b: Vector) : Vector =
        let n = Matrix.numRows A
        let A1 = Matrix.copy A
        let b1 = Vector.copy b
        SolveCoreUtility.solveCore A1 b1
        // Build answer
        let x = Vector.zeros n
        for i = 0 to n - 1 do
            let mutable s = b1.[i]
            let a = A1.[i]
            for j = n - i to n - 1 do
                s <- s - x.[j] * a.[j]
            x.[n - i - 1] <- s
        x

    let SparseSolveCore (A: SparseMatrix) (b: Vector) : Vector =

            let n = A.RowDimension
            let mutable b1 = [| for i in 0..n - 1 -> b.[i] |] 

            let mutable map = [| for i in 0..n - 1 -> i |]
            let mutable x = Array.zeroCreate n
            for j in 0..n - 1 do
                // Find row with largest absolute value of j-st element
                let mutable maxIdx = 0
                let mutable maxVal = A.Items(maxIdx, j)
                let mutable Aij = 0.0
                for i in 0..n - j - 1 do
                    Aij <- A.Items(i, j)
                    if (Math.Abs(Aij) > Math.Abs(maxVal))
                            then
                                maxIdx <- i
                                maxVal <- Aij
                    
                

                if (Math.Abs(maxVal) < 1e-12)
                        then
                            failwith "Cannot apply Gauss method"

                // Divide this row by max value
                A.ScaleRow(maxIdx, j + 1, n - 1, 1.0 / maxVal);
                b1.[maxIdx] <- b1.[maxIdx] / maxVal;
                A.Items(maxIdx, j) <- 1.0


                // Move this row to bottom
                if (maxIdx <> n - j - 1) then
                    
                    A.SwitchRows(maxIdx, n - j - 1)

                    let temp3 = b1.[n - j - 1]
                    b1.[n - j - 1] <- b1.[maxIdx]
                    b1.[maxIdx] <- temp3

                    let temp2 = map.[n - j - 1]
                    map.[n - j - 1] <- map.[maxIdx]
                    map.[maxIdx] <- temp2
                
                // Process all other rows
                for i = 0 to n - j - 2 do
                    Aij <- A.Item(i, j)
                    if (Aij <> 0.0) then
                        for k = j + 1 to n - 1 do
                            A.Item(i, k) <- (Aij - A.Item(n - j - 1, k))
                        b1.[i] <- (b1.[i] - (Aij * b1.[n - j - 1]));
                        A.Item(i, j) <- 0.0

            


            // Build answer
            for i = 0 to n - 1 do
                let mutable s = b1.[i]
                let Ai = A.[i]
                
                for k in 0..Ai.Count - 1 do
                    if (Ai.Indices.[k] >= n - i)
                            then
                                s <- s - x.[Ai.Indices.[k]] * A.[i].Items.[k]
                Array.set x (n - i - 1)  s
                
            Vector.ofArray x


