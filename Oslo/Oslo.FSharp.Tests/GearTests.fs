// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

module GearTests =

    open Expecto
    open System
    open Oslo
    open Oslo.GearBDF

    let tests =
        testList "Gaussian Elimination" [
            test "Exp" {
                let t0 = 0.0
                let x0 = !^[|1000.0|]
                let f t (x : Vector) = !^[|- 0.01 * x.[0]|]
                let ts, xs = solve t0 2000.0 x0 f {defaults() with MaxStep = 5.0} |> List.unzip

                let max = List.map2 (fun t (x:Vector) -> Math.Abs(x.[0] - 1000.0 * Math.Exp(-0.01 * t))) ts xs |> List.max
                Expect.isLessThan max 1e-1 ""
            }
        ]
