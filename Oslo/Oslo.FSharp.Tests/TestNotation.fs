// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.Tests

[<AutoOpen>]
module TestsNotation =

    open Oslo

    let ( !^ ) xs = Vector.ofArray xs
    let ( !* ) xs = Matrix.ofArray xs
