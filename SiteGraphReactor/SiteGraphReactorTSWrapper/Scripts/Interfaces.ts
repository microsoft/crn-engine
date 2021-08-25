// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as CRN from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import * as Interfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';

export type ParseResult = { model: Interfaces.IG, settings: void, options: void, code: string }

export type ParseSGObservables = { result: Rx.Observable<ParseResult> }

export type WorkerRequest_Compile = { mtype: "sg.compile"; code: string }
export type WorkerRequest_Parse = { mtype: "sg.parse"; code: string }
export type WorkerRequest_Expand = { mtype: "sg.expand"; code: string }

export type WorkerRequest = WorkerRequest_Compile | WorkerRequest_Expand | WorkerRequest_Parse | CRN.WorkerRequest

export type WorkerResponse_ParseResult = { mtype: "sg.parseresult", result: ParseResult }

export type WorkerResponse = WorkerResponse_ParseResult | CRN.WorkerResponse