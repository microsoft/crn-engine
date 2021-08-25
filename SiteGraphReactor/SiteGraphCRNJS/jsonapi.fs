// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScriptExport>]
module SiteGraphReactor.JSONAPI
open WebSharper
open SiteGraphReactor.JSAPI
open SiteGraphReactor.Compile

type ParseResult =
  { bundle     : bundle
  ; unexpanded : obj }

let user_parse (code:string) : ParseResult =
  let pr = JSAPI.user_parse code in
  let ret = WebSharper.Json.Encode pr.unexpanded in
  { bundle = pr.bundle ; unexpanded = ret }

let user_expand (bundle:bundle) : obj =
  let gui = JSAPI.user_expand bundle in
  let ret = WebSharper.Json.Encode gui in
  ret

let user_compile (code:string) : obj =
  let gui = JSAPI.user_compile code in
  let ret = WebSharper.Json.Encode gui in
  ret