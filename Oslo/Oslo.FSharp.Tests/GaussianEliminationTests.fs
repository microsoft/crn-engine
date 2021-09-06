// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

module GuassianEliminationTests =

    open Expecto
    open Oslo
    open Oslo.Gauss

    let private VectorsEqual(a: Vector, b: Vector) =
        Vector.subtract a b
        |> Vector.sum
        |> abs < 0.001
        |> Expect.isTrue

    let tests =
        testList "Gaussian Elimination" [
            test "Solve core" {
                let A = !*[|[|2.0;1.0;-1.0|];[|-3.0;-1.0;2.0|];[|-2.0;1.0;2.0|]|]
                let b = !^[|8.0;-11.0;-3.0|]
                let x = (SolveCore A b)
                VectorsEqual(x, !^[|2.0;3.0;-1.0|]) "Vectors must equal"
            }
        ]
