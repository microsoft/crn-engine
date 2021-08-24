// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
[<JavaScript>] 
[<CustomEquality; NoComparison>]
type Lambda<'key> when 'key:equality = 
  Lambda of (('key -> float) -> float)
    override x.GetHashCode() = hash (x)
    override x.Equals(yobj) = 
        match yobj with
        | :? Lambda<'key> as y -> System.Object.ReferenceEquals (x,y)
        | _ -> false
    member inline this.key = match this with Lambda y -> y
    //This member is provided to allow for allocation-free calling, which is valuable for performance
    member inline this.keyref(key:byref<_>) = match this with Lambda y -> y key
    static member map f (Lambda a_lbd : Lambda<'key>) b_to_float =
        let a_to_float a = b_to_float (f a) in
        a_lbd a_to_float

    static member reset a_lbd a_to_key = Lambda.map a_to_key a_lbd