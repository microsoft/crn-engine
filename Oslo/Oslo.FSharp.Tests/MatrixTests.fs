// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

module MatrixTests =

    open Expecto
    open Oslo

    let tests =
        testList "Matrix" [
            test "Add matrices" {
                let A = 
                    !*[| [| 1.0; 0.0 |]
                         [| 0.0; 1.0 |] |]
        
                let B = 
                    !*[| [| -1.0; 0.0 |]
                         [| 0.0; -0.9 |] |]

                let result = Matrix.add A B
                let expectation =
                    !*[| [| 0.0; 0.0 |]
                         [| 0.0; 0.1 |] |]
        
                Expect.isTrue(Matrix.areEqualEps result expectation 0.001) ""
            }
            test "Subtract matrices" {
                let A = 
                    !*[| [| 1.0; 0.0 |]
                         [| 0.0; 1.0 |] |]
        
                let B = 
                    !*[| [| -1.0; 0.0 |]
                         [| 0.0; -0.9 |] |]

                let result = Matrix.subtract A B
                let expectation =
                    !*[| [| 2.0; 0.0 |]
                         [| 0.0; 1.9 |] |]

                Expect.isTrue(Matrix.areEqualEps result expectation 0.001) ""
            }
            test "Multiply by scalar" {
                let A = 
                    !*[| [| 1.0; 0.0 |]
                         [| 0.0; 1.1 |] |]

                let result = Matrix.scalMultMatr 2.0 A
                let expectation =
                    !*[| [| 2.0; 0.0 |]
                         [| 0.0; 2.2 |] |]

                Expect.isTrue(Matrix.areEqualEps result expectation 0.001) ""
            }
            test "Multiply matrices" {
                let A = 
                    !*[| [| 0.0; 2.0 |]
                         [| 1.0; 1.2 |] |]
        
                let B = 
                    !*[| [| 3.0; 0.0 |]
                         [| 0.0; -2.0 |] |]

                let result = Matrix.matrMultMatr A B
                let expectation =
                    !*[| [| 0.0; -4.0 |]
                         [| 3.0; -2.4 |] |]

                Expect.isTrue(Matrix.areEqualEps result expectation 0.001) ""
            }
            test "Multiply by vector" {
                let A = 
                    !*[| [| 0.0; 2.0 |]
                         [| 1.0; 1.2 |] |]
        
                let v = !^[| 1.0; 2.0 |]

                let result = Matrix.matrMultVec A v
                let expectation = !^[| 4.0; 3.4 |]

                Expect.isTrue(Vector.areEqualEps result expectation 0.001) ""
            }
            test "Transpose square" {
                let A = 
                    !*[| [| 0.0; 2.0 |]
                         [| 1.0; 1.2 |] |]
                let result = Matrix.transpose A
                let expectation =
                    !*[| [| 0.0; 1.0 |]
                         [| 2.0; 1.2 |] |]

                Expect.isTrue(Matrix.areEqualEps result expectation 0.001) ""
            }
            test "Transpose non-square" {
                let A = 
                    !*[| [| 0.0; 2.0; -2.3 |] |]
                let result = Matrix.transpose A
                let expectation =
                    !*[| [| 0.0 |]
                         [| 2.0 |]
                         [| -2.3 |] |]

                Expect.isTrue(Matrix.areEqualEps result expectation 0.001) ""
            }
            test "Map" {
                let A = 
                    !*[| [| 1.0; 2.0 |]
                         [| 3.0; 4.0 |] |]
        
                let result = Matrix.matrMap (fun x -> x * x) A
                let resultAsPiping = A |> Matrix.matrMap (fun x -> x * x) //Not sure why we're testing piping...
                let expectation =
                    !*[| [| 1.0; 4.0 |]
                         [| 9.0; 16.0 |] |]

                Expect.isTrue(Matrix.areEqualEps result expectation 0.001) ""
                Expect.isTrue(Matrix.areEqualEps resultAsPiping expectation 0.001) ""
            }
        ]
