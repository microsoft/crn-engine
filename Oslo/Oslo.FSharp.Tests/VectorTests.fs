// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

module VectorTests =

    open Expecto
    open Oslo

    let tests =
        testList "Vectors" [
            test "Adding" {
                let x = !^[|1.0; 0.0|]
                let y = !^[|-1.0; 1.0|]
                let z = x +^ y

                Expect.isTrue(Vector.areEqual z !^[|0.0; 1.0|]) "Should equal"
                Expect.isFalse(Vector.areEqual z !^[|0.0; 1.000001|]) "Shouldn't equal"
            }
            test "Subtraction (eps)" {
                let x = !^[|1.0; 0.0|]
                let y = !^[|-1.0; 1.000000000000000001|]
                let z = x +^ y

                Expect.isTrue(Vector.areEqual z !^[|0.0; 1.0|]) "Should equal"
                Expect.isFalse(Vector.areEqual z !^[|0.0; 1.01|]) "Shouldn't equal"
            }
            test "Multiply vector" {
                let x = !^[|1.0; 0.0|]
                let s = 2.0
                let y = x *> s
                Expect.isTrue(Vector.areEqual y !^[|2.0; 0.0|]) "Equal"
            }
            test "Scale vector" {
                let x = !^[|1.0; 0.0|]
                let s = 0.5
                let y = s *< x
                Expect.isTrue(Vector.areEqual y !^[|0.5; 0.0|]) "Equal"
            }
            test "dot" {
                let x = !^[|1.0; 0.0|]
                let y = !^[|-1.0; 1.0|]
                let z = Vector.dot x y
                Expect.equal z -1.0 "equal"
            }
            test "Dollar" {
                let x = !^[|1.0; 0.0|]
                let y = !^[|-1.0; 1.0|]
                let z = x $ y

                Expect.isTrue(Matrix.areEqualEps z !*[| [|-1.0; 1.0|]; [|0.0; 0.0|]|] 0.001) "Equal"
            }
        ]
