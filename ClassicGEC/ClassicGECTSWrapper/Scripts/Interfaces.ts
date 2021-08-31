// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as CRN from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';

// The public FSBOL (github.com/SynBioDex/fSBOL) doesn't support TypesTSFS, so the auto-generated interfaces don't work
// TODO: Update FSBOL and re-plumb SBOL components
//export type jSBOLDocument = WGI.FSBOL.JsonSerializer.rSBOLDocument;

/*
export type rAttachment = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    source: string;
    format: string;
    size: number;
    hash: string;
}
export type rCollection = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    members: Array<string>;
}
export type rCombinatorialDerivation = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    strategy: string;
    template: string;
    variableComponents: Array<rVariableComponent>;
}
export type rFunctionalComponent = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    definition: string;
    access: string;
    mapsTos: Array<rMapsTo>;
    direction: string;
}
export type rImplementation = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    built: string;
}
export type rInteraction = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    types: Array<string>;
    participations: Array<rParticipation>;
}
export type rModel = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    source: string;
    language: string;
    framework: string;
}
export type rModule = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    definition: string;
    mapsTos: Array<rMapsTo>;
}
export type rModuleDefinition = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    roles: Array<string>;
    functionalComponents: Array<rFunctionalComponent>;
    interactions: Array<rInteraction>;
    modules: Array<rModule>;
    models: Array<string>;
}
export type rParticipation = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    roles: Array<string>;
    participant: string;
}
export type rSequence = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    elements: string;
    encoding: string;
}
export type rVariableComponent = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    operator: string;
    variants: Array<string>;
    variantCollections: Array<string>;
    variantDerivations: Array<string>;
    variable: string;
}
*/

////////////////////////
export type rCut = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    orientation: string;
    at: number;
}
export type rGenericLocation = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    orientation: string;
}
export type rRange = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    orientation: string;
    startIndex: number;
    endIndex: number;
}
export type rLiteral = {
    literalType: string;
    string: string;
    int: number;
    int64: number;
    double: number;
    bool: boolean;
}
export type rQName = {
    qNameType: string;
    name: string;
    prefix: string;
    nameSpaceUri: string;
}
export type rMapsTo = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    local: string;
    remote: string;
    refinment: string;
}
export type rSequenceAnnotation = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    ranges: Array<rRange>;
    cuts: Array<rCut>;
    genericLocations: Array<rGenericLocation>;
    roles: Array<string>;
    componentObj: string;
}
export type rAnnotation = {
    qName: rQName;
    valueType: string;
    literal: rLiteral;
    uri: string;
    nestedQName: rQName;
    annotations: Array<rAnnotation>;
}
export type rSequenceConstraint = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    subject: string;
    object: string;
    restriction: string;
}
export type rComponent = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    description: string;
    annotations: Array<rAnnotation>;
    definition: string;
    access: string;
    mapsTos: Array<rMapsTo>;
    roles: Array<string>;
    roleIntegrations: Array<string>;
}
export type rComponentDefinition = {
    uri: string;
    version: string;
    name: string;
    displayId: string;
    persistentIdentity: string;
    attachments: Array<string>;
    description: string;
    annotations: Array<rAnnotation>;
    components: Array<rComponent>;
    sequenceAnnotations: Array<rSequenceAnnotation>;
    sequenceConstraints: Array<rSequenceConstraint>;
    sequences: Array<string>;
    types: Array<string>;
    roles: Array<string>;
}
export type rSBOLDocument = {
    //attachments: Array<rAttachment>;
    //sequences: Array<rSequence>;
    componentDefinitions: Array<rComponentDefinition>;
    //moduleDefinitions: Array<rModuleDefinition>;
    //models: Array<rModel>;
    //implementations: Array<rImplementation>;
    //collections: Array<rCollection>;
    //CombinatorialDerivation: Array<rCombinatorialDerivation>;
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