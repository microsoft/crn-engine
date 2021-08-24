// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScriptExport>]
module Microsoft.Research.GEC.JSONAPI

open WebSharper
open Microsoft.Research.CRNEngine

let compile (code:string) (parts:string) (reactions:string) : obj =
  let result = JSAPI.compile code parts reactions
  WebSharper.Json.Encode result

let get_solution (o:obj) (idx:int) : obj =
  let o = WebSharper.Json.Decode<JSAPI.solve_result> o
  let result = JSAPI.get_solution o idx
  WebSharper.Json.Encode result