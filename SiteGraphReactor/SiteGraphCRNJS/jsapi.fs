// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScriptExport>]
module SiteGraphReactor.JSAPI
open WebSharper
open Microsoft.Research.CRNEngine
open SiteGraphReactor.Compile

type ParseResult =
  { bundle     : bundle
  ; unexpanded : Gui }

let user_parse (code:string) : ParseResult =
  let (settings,sitegraphs) = parse_with_settings code in
  let bundle, crn = convert_unexpanded sitegraphs settings in
  let crn = match settings.inference with 
            | None -> crn 
            | Some i -> { crn with settings = {crn.settings with inference = i} }
  let crn = crn.initialise()
  let gui = Gui.from_crn crn
  { bundle     = bundle
  ; unexpanded = gui }

let user_expand (bundle:bundle) : Gui =
  let crn = expand 0 bundle in
  let crn = crn.initialise()
  let gui = Gui.from_crn crn
  gui

let user_compile (code:string) : Gui =
  let (settings,sitegraphs) = parse_with_settings code in
  let no_limit = 0
  let crn = convert_expand no_limit sitegraphs settings.render_mode
  let crn = match settings.inference with 
            | None -> crn 
            | Some i -> { crn with settings = {crn.settings with inference = i} }
  let crn = crn.initialise()
  let gui = Gui.from_crn crn
  gui