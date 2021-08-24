// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as CRN from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';

export type jSBOLDocument = WGI.FSBOL.JsonSerializer.rSBOLDocument;

export type CompileGECObservables = {
    solution_count: Rx.Observable<number>,
    jsbol: Rx.Observable<jSBOLDocument>,
    exports: Rx.Observable<Interfaces.ExportDef>
}

export type GECSolution = { model: Interfaces.IG, code: string }

export type GetSolutionObservables = {
    solution: Rx.Observable<GECSolution>,
    jsbol: Rx.Observable<jSBOLDocument>,
    exports: Rx.Observable<Interfaces.ExportDef>
}

export type WorkerRequest_GECCompile = { mtype: "gec.compile", code: string, parts: string, reactions: string }
export type WorkerRequest_GECGetSolution = { mtype: "gec.getsolution", idx: number }

export type WorkerRequest = WorkerRequest_GECCompile | WorkerRequest_GECGetSolution | CRN.WorkerRequest;

export type WorkerResponse_GECSolutions = { mtype: "gec.solutions", count: number }
export type WorkerResponse_JSBOL = { mtype: "gec.jsbol", document: jSBOLDocument }
export type WorkerResponse_GECSolution = { mtype: "gec.solution", solution: GECSolution }

export type WorkerResponse = WorkerResponse_GECSolutions | WorkerResponse_JSBOL | WorkerResponse_GECSolution | CRN.WorkerResponse;