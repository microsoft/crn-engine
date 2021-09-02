// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo

open System

type Vector =
    private {
        Data : double []
    }

    member v.Item
        with [<InlineJavaScript "$0[$1]">] get (i: int) : double = v.Data.[i]
        and [<InlineJavaScript "void($0[$1]=$2)">] set (i: int) (d: double) = v.Data.[i] <- d

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<JavaScript>]
module Vector =

    type T = Vector

    [<InlineJavaScript "new Float64Array($0)">]
    let fastCreate (n: int) =
        Array.CreateInstance(typeof<double>, n) :?> double []

    [<InlineJavaScript "new Float64Array($0)">]
    let zeros n = { Data = fastCreate n }

    [<InlineJavaScript "$0.length">]
    let length (v: Vector) = v.Data.Length

    [<InlineJavaScript "$1.set($0)">]
    let copyTo x y =
        if x.Data.Length <> y.Data.Length then
            failwith "Invalid vector lengths"
        Array.Copy(x.Data, y.Data, x.Data.Length)

    [<DirectJavaScript "$t.set($s.subarray($i,$i+$n),$j)">]
    let blit (s: T) (i: int) (t: T) (j: int) (n: int) =
        Array.Copy(s.Data, i, t.Data, j, n)

    [<DirectJavaScript "new Float64Array($arr)">]
    let ofArray (arr: double[]) =
        let n = Array.length arr
        let r = zeros n
        Array.Copy(arr, r.Data, n)
        r

    [<DirectJavaScript "Array.prototype.slice.call($v)">]
    let toArray v =
        let n = length v
        let r = fastCreate n
        Array.Copy(v.Data, r, n)
        r

    let copy v =
        let r = zeros (length v)
        copyTo v r
        r

    let max v =
        let mutable s = 0.0
        for i in 0 .. length v - 1 do
            s <- max s v.[i]
        s

    let update f v =
        for i in 0 .. length v - 1 do
            v.[i] <- f v.[i]

    let updatei f v =
        for i in 0 .. length v - 1 do
            v.[i] <- f i v.[i]

    let map f v =
        let r = copy v
        update f r
        r

    let mapi f v =
        let r = copy v
        updatei f r
        r

    let sum v =
        let mutable s = 0.0
        for i in 0 .. length v - 1 do
            s <- s + v.[i]
        s

    let checkSize a b =
        if length a <> length b then
            failwith "Vector dimensions do not match"

    [<InlineJavaScript "$0">]
    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let inline adapt2 (f: double -> double -> double) =
        OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)

    [<InlineJavaScript "$0">]
    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let inline adapt3 (f: int -> double -> double -> double) =
        OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)

    [<InlineJavaScript "$0($1)($2)">]
    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let inline invoke2 (f: OptimizedClosures.FSharpFunc<double,double,double>) x y =
        f.Invoke(x, y)

    [<InlineJavaScript "$0($1)($2)($3)">]
    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let inline invoke3 (f: OptimizedClosures.FSharpFunc<int,double,double,double>) x y z =
        f.Invoke(x, y, z)

    let mapi2 f v1 v2 =
        checkSize v1 v2
        let g = adapt3 f
        let n = length v1
        let r = zeros n
        for i in 0 .. n - 1 do
            r.[i] <- invoke3 g i v1.[i] v2.[i]
        r

    let map2 f v1 v2 =
        checkSize v1 v2
        let g = adapt2 f
        let n = length v1
        let r = zeros n
        for i in 0 .. n - 1 do
            r.[i] <- invoke2 g v1.[i] v2.[i]
        r

    let dot v1 v2 =
        checkSize v1 v2
        let mutable s = 0.0
        for i in 0 .. length v1 - 1 do
            s <- s + v1.[i] * v2.[i]
        s

    let areEqual (v1:T) (v2: T) =
        let n = length v1
        if n = length v2 then
            v1.Data = v2.Data
        else false

    let areEqualEps v1 v2 eps : bool =
        let n = length v1
        if n = length v2 then
            (v1.Data, v2.Data)
            ||> Seq.forall2 (fun i j -> Math.Abs(i - j) < eps)
        else false

    let add v1 v2 =
        checkSize v1 v2
        let n = length v1
        let r = zeros n
        for i in 0 .. n - 1 do
            r.[i] <- v1.[i] + v2.[i]
        r

    let setAdded v1 v2 =
        checkSize v1 v2
        let n = length v1
        for i in 0 .. n - 1 do
            v1.[i] <- v1.[i] + v2.[i]

    let subtract v1 v2 =
        checkSize v1 v2
        let n = length v1
        let r = zeros n
        for i in 0 .. n - 1 do
            r.[i] <- v1.[i] - v2.[i]
        r

    let scale k v =
        let r = copy v
        for i in 0 .. length r - 1 do
            r.[i] <- k * r.[i]
        r

    let setScaled k v =
        let n = length v
        for i in 0 .. n - 1 do
            v.[i] <- k * v.[i]

    let setAddedScaled x y k =
        checkSize x y
        let n = length x
        for i in 0 .. n - 1 do
            x.[i] <- x.[i] + k * y.[i]

[<AutoOpen>]
[<JavaScript>]
module VectorNotation =

    [<InlineJavaScript>]
    let ( *< ) a v = Vector.scale a v

    [<InlineJavaScript>]
    let ( *> ) v a = Vector.scale a v

    [<InlineJavaScript>]
    let ( +^ ) x y = Vector.add x y

    [<InlineJavaScript>]
    let ( -^ ) x y = Vector.subtract x y
