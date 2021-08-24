// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScriptExport>]
module Microsoft.Research.DNA.JSAPI
open Microsoft.Research.CRNEngine
open Microsoft.Research.DNA.Dsd
open Microsoft.Research.CRNEngine.JSAPI

type ParseObject =
  { code : string
  ; toeholds: string
  ; specificities: string }

type ParseResult =
  { bundle     : bundle
  ; settings   : Options.t
  ; unexpanded : GuiIG }

let set_options result settings = 
  let bundle = Dsd.set_options result.bundle settings
  { result with bundle = bundle; settings = settings }

/// This invokes the DSD parser. It consumes the toeholds text, the specificities text, and the code, and produces a Dsd.bundle. Note that the type of the bundle is opaque here, so it can be changed arbitrarily, provided that the API is preserved.
let parser:string->string->string->(bundle*Task option) = parse_extended

/// Parses code and converts the unexpanded model into a CRN.
let user_parse (code:ParseObject) : ParseResult =
  let (bundle,task) = parser code.toeholds code.specificities code.code in
  let crn = convert_unexpanded bundle in
  let gui = Gui.from_crn crn in
  let model:GuiModel = { top = gui; systems = [] }
  let ig:GuiIG = {task = task; nodes = Map.empty |> Map.add "" model; edges = Map.empty; expanded = false}
  let options = get_options bundle
  // I'm removing the rules program from the options, because it doesn't serialise properly and isn't needed anyway.
  let settings = Options.set_rules_program None options
  { bundle = bundle
  ; settings = settings
  ; unexpanded = ig }

/// Expands the bundle into a CRN, unless it's JIT. If it's JIT, it just returns the unexpanded CRN.
let user_expand (bundle:bundle) : GuiModel =
  let options = get_options bundle
  let crn = if Options.get_is_jit options then convert_unexpanded bundle else convert_expand bundle
  let gui = Gui.from_crn crn in
  let model:GuiModel = { top = gui; systems = [] }
  model

/// Parses code and expands the model into a CRN, unless it's JIT. If it's JIT, it just returns the unexpanded CRN.
let user_compile (code:ParseObject) : GuiModel =
  let (bundle,task) = parser code.toeholds code.specificities code.code in
  let options = get_options bundle
  let crn = if Options.get_is_jit options then convert_unexpanded bundle else convert_expand bundle
  let gui = Gui.from_crn crn in
  let model:GuiModel = { top = gui; systems = [] }
  model

let user_parse_oldsyntax (code:ParseObject) : ParseResult = { code with code = code.code |> SLConversion.convertSL } |> user_parse
let user_compile_oldsyntax (code:ParseObject) : GuiModel = { code with code = code.code |> SLConversion.convertSL } |> user_compile

let get_jit_classic (bundle:bundle) : Microsoft.Research.CRNEngine.JSAPI.jit<Microsoft.Research.DNA.Species.t> =
  { jit = match Dsd.to_jit bundle with
          | Choice1Of2 x -> x
          | _ -> failwith "Internal error: expecting Classic DSD JIT."
    calculus = Dsd.makeDsdCalculus bundle }

let get_jit_rules (bundle:bundle) : Microsoft.Research.CRNEngine.JSAPI.jit<RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>> =
  { jit = match Dsd.to_jit bundle with
          | Choice2Of2 x -> x
          | _ -> failwith "Internal error: expecting Rules DSD JIT."
    calculus = Dsd.to_rules_calculus bundle }

let is_jit (bundle:bundle) = Dsd.is_jit bundle

let is_classic (bundle:bundle) = match bundle with ClassicDSD _ -> true | _ -> false