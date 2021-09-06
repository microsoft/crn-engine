// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

open Expecto

module Program =
    open Oslo

    [<Tests>]
    let allTests =
        testList "All" [
            VectorTests.tests
            MatrixTests.tests
            SparseVectorTests.tests
            SparseMatrixTests.tests
            GuassianEliminationTests.tests
            GearTests.tests
        ]

    [<EntryPoint>]
    let main args =
        runTestsWithArgs defaultConfig args allTests
