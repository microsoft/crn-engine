// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as CRN from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';

// The public FSBOL (github.com/SynBioDex/fSBOL) doesn't support TypesTSFS, so the auto-generated interfaces don't work
// TODO: Update FSBOL and re-plumb SBOL components
//export type jSBOLDocument = WGI.FSBOL.JsonSerializer.rSBOLDocument;

// Workaround: define the structure that is used, to prevent build errors
export type rRange = {
    name: string;
    displayId: string;
    persistentIdentity: string;
    version: string;
    uri: string;
    startIndex: number;
    endIndex: number;
    orientation: string;
 }
export type rSequenceAnnotation = {
    name: string;
    displayId: string;
    persistentIdentity: string;
    version: string;
    uri: string;
    ranges: Array<rRange>;
    roles: Array<string>;
}
export type rComponent = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    definition: string;
    access: string;
}
export type rComponentDefinition = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    components: Array<rComponent>;
    sequenceAnnotations: Array<rSequenceAnnotation>;
    sequences: Array<string>;
    types: Array<string>;
    roles: Array<string>;
}
export type rSBOLDocument = {
    componentDefinitions: Array<rComponentDefinition>;
}
export type jSBOLDocument = rSBOLDocument


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