// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

module SparseMatrixTests =

    open Expecto
    open Oslo.SparseMatrix

    let tests =
        testList "Sparse Matrices" [
            test "Plus" {
                let N = 50

                // Make matrix diagonal 6.0, 1.0 elsewhere
                let A = SparseMatrix(N, N)
                for i in 0..5..N-1 do A.Item(i,i) <- 6.0;
        
                // Make matrix diagonal -1.0, 1.0 elsewhere
                let B = SparseMatrix(N, N)
                for i in 0..5..N-1 do B.Item(i,i) <- -1.0;

                let expected = SparseMatrix(N, N)
                for i in 0..5..N-1 do expected.Item(i,i) <- 5.0;

                let C = A.plus(B)
                Expect.isTrue (Oslo.SparseMatrix.areEqual C expected) ""
            }
            test "Minus" {
                let N = 50

                let A = Oslo.SparseMatrix.Identity(N, N)
                for i in 0..N - 1 do A.Item(i,i) <- if i % 5 = 0 then 5.0 else 3.0

                let B = Oslo.SparseMatrix.Identity(N, N)
                for i in 0..N - 1 do B.Item(i,i) <- if i % 5 = 0 then 2.0 else 1.0

                let expected = Oslo.SparseMatrix.Identity(N, N)
                for i in 0..N - 1 do expected.Item(i,i) <- if i % 5 = 0 then 3.0 else 2.0

                let C = A.minus(B)
                Expect.isTrue (Oslo.SparseMatrix.areEqual C expected) ""
            }
            test "Minus with mismatched" {
                let N = 50

                let A = Oslo.SparseMatrix.Identity(N, N)
                for i in 0..N - 1 do A.Item(i,i) <- if i % 5 = 0 then 5.0 else 3.0

                let B = Oslo.SparseMatrix.Identity(N, N)
                for i in 0..N - 1 do B.Item(i,i) <- if i % 5 = 0 then 2.0 else 1.0

                let expected = Oslo.SparseMatrix.Identity(N, N)
                for i in 0..N - 1 do expected.Item(i,i) <- if i % 5 = 0 then 3.0 else 2.0

                let C = A.minus(B)
                Expect.isTrue (Oslo.SparseMatrix.areEqual C expected) ""
            }
            //TODO: times
        ]
