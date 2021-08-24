// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.ClassicDSDServer.Messages

open Microsoft.Research.CRNEngine
open Microsoft.Research.Filzbach
open Microsoft.Research
open Microsoft.Research.CRNEngine.JSAPI

type Request_ParseDSD =
    { mtype : string
      code : DNA.JSAPI.ParseObject
      oldSyntax : bool }

type Request_CompileDSD =
    { mtype : string
      code : DNA.JSAPI.ParseObject
      oldSyntax : bool }

type Request_ExpandDSD =
    { mtype : string
      code : DNA.JSAPI.ParseObject
      model: GuiIG
      settings : Microsoft.Research.DNA.Options.t
      oldSyntax : bool }

type Response_IsJIT =
    { mtype : string
      isJIT : bool }

type ParseResult = { model : GuiIG; settings: Microsoft.Research.DNA.Options.t; code : string }

type Response_ParseResult =
    { mtype : string
      result : ParseResult }