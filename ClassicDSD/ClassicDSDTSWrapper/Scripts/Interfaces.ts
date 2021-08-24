// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as CRN from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import * as Interfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";

export type DsdSettings = WGI.Microsoft.Research.DNA.Options.t;
export type ParseResult = { model: Interfaces.IG, settings: DsdSettings, code: string }

export type ParseDSDObservables = { result: Rx.Observable<ParseResult> }

export type ParseObject = WGI.Microsoft.Research.DNA.JSAPI.ParseObject;


export type WorkerRequest_Compile = { mtype: "dsd.compile", code: ParseObject, oldSyntax: boolean }
export type WorkerRequest_Parse = { mtype: "dsd.parse", code: ParseObject, oldSyntax: boolean }
export type WorkerRequest_Expand = { mtype: "dsd.expand", code: ParseObject, model: Interfaces.IG, settings: DsdSettings, oldSyntax: boolean }

export type WorkerRequest = WorkerRequest_Compile | WorkerRequest_Parse | WorkerRequest_Expand | CRN.WorkerRequest;

export type WorkerResponse_IsJIT = { mtype: "dsd.isjit", isJIT: boolean }
export type WorkerResponse_ParseResult = { mtype: "dsd.parseresult", result: ParseResult }

export type WorkerResponse = WorkerResponse_IsJIT | WorkerResponse_ParseResult | CRN.WorkerResponse;