// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace proxies

open WebSharper
open WebSharper.JavaScript

(* minimal implementation of StringBuilder for compatiblity *not* performance *)

[<Name "StringBuilder">]
//[<Proxy(typeof<Microsoft.Research.CRNEngine.Stringbuilder.t>)>]
module private StringBuilderProxy =

    [<JavaScript>]
    [<Name "StringBuilder">]
    [<Proxy(typeof<System.Text.StringBuilder>)>]
    type StringBuilderProxy(str : string) =

        let mutable _str = ""

        do
            _str <- str

        new () = StringBuilderProxy("")

        member this.Append (str : string) =
            _str <- _str + str
            As<System.Text.StringBuilder> this

        (* used by JavaScript string() *)
        member this.toString () =
            _str 
