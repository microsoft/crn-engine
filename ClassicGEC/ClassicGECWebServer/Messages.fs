// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.ClassicGECWebServer.Messages

open Microsoft.Research.CRNEngine
open Microsoft.Research.Filzbach
open Microsoft.Research
open Microsoft.Research.CRNEngine.JSAPI

type Request_GECCompile =
    { mtype : string
      code : string
      parts : string
      reactions : string }

type Request_GECGetSolution =
    { mtype : string
      idx : int }

type Response_GECSolutions =
    { mtype : string
      count : int }

type Response_SBOL =
    { mtype : string
      document : FSBOL.JsonSerializer.rSBOLDocument }

type GECSolution =
    { model : GuiIG
      code : string }

type Response_GECSolution =
    { mtype : string
      solution : GECSolution }