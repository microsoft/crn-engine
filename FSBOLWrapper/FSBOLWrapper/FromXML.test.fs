// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.FSBOL.FromXMLTest

open Microsoft.Research.FSBOL.Sequence


open Xunit
open FsUnit.Xunit
open System
open System.Diagnostics
open System.Xml
open System.IO
open System.Text

[<Fact>]
let ``fromSequenceXML``() = 
    let uriPrefix = "www.microsoft.com/gec"
    let version = "1"
    let name = "pTet"
    let seq = "atcgga"
    let encoding = Terms.dnasequence
    let displayId = name;
    let s:Sequence = new Sequence(name,uriPrefix,displayId,version, seq,encoding)
    
    Debug.WriteLine("End of test")

