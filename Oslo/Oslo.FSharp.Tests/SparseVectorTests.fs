// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

module SparseVectorTests =

    open Expecto
    open Oslo.SparseVector

    let tests =
        testList "Sparse Vectors" [
            test "Access test" {
                let sv = SparseVector([|1.0; 2.0; 3.0|], [| 0; 3; 6|], 10)
                Expect.equal sv.[0] 1.0 ""
                Expect.equal sv.[1] 0.0 ""
                Expect.equal sv.[2] 0.0 ""
                Expect.equal sv.[3] 2.0 ""
                Expect.equal sv.[4] 0.0 ""
                Expect.equal sv.[5] 0.0 ""
                Expect.equal sv.[6] 3.0 ""
                Expect.equal sv.[7] 0.0 ""
            }
        ]
