// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import "jquery";
import * as Rx from 'rx';
import * as InternalInterfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import * as CRNInterfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as Interfaces from './Interfaces';
import CRNEngine from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import DefaultDatabase from './DefaultDatabase';
import ClassicDSDWorker from "worker-loader?filename=./ClassicDSD.worker.[hash].js!./ClassicDSD.worker.js";
import { WebSharperGeneratedInterfaces } from "./WebSharperGeneratedInterfaces";
import * as CRN from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";

/** Provides asynchronous access to DSD F# methods. */
class ClassicDSD extends CRNEngine {
    constructor(server: boolean) {
        super(server, ClassicDSDWorker);
    }

    private DoDSDRequestResponse(task: Interfaces.WorkerRequest) {
        return this.DoRequestResponse(<InternalInterfaces.WorkerRequest><any>task);
    }

    private currentBundleIsJIT = false;

    protected isJIT(model: CRNInterfaces.Model) {
        return this.currentBundleIsJIT;
    }

    private ToParseCodeObservables(messages: Rx.Observable<Interfaces.WorkerResponse>): Interfaces.ParseDSDObservables {
        var that = this;
        // The first message will tell whether this is a JIT model.
        messages.take(1).subscribe(r => that.currentBundleIsJIT = (<Interfaces.WorkerResponse_IsJIT>r).isJIT, error => { }, () => { });
        // Don't need to return it.
        messages = messages.skip(1);
        var ret = {
            result: messages.where(v => v.mtype == "dsd.parseresult").select(v => (<Interfaces.WorkerResponse_ParseResult>v).result),
        };
        return ret;
    }

    private toeholds: string = DefaultDatabase.toeholds;
    get Toeholds(): string { return this.toeholds; }
    set Toeholds(value: string) { this.toeholds = value; }

    private specificities: string = DefaultDatabase.specificities;
    get Specificities(): string { return this.specificities; }
    set Specificities(value: string) { this.specificities = value; }

    /** Starts a compile operation. This will result in a fully-expanded CRN. This will always parse the code. */
    UserCompile(code: string, oldSyntax: boolean): Interfaces.ParseDSDObservables {
        if (code == null) code = "";
        var parseObject: Interfaces.ParseObject = { code: code, toeholds: this.toeholds, specificities: this.specificities };
        var task: Interfaces.WorkerRequest_Compile = { mtype: "dsd.compile", code: parseObject, oldSyntax: oldSyntax };
        var messages = this.DoDSDRequestResponse(task);
        return this.ToParseCodeObservables(messages);
    }

    /** Starts a parse operation. This will result in an unexpanded CRN. */
    UserParse(code: string, oldSyntax: boolean): Interfaces.ParseDSDObservables {
        if (code == null) code = "";
        var parseObject: Interfaces.ParseObject = { code: code, toeholds: this.toeholds, specificities: this.specificities };
        var task: Interfaces.WorkerRequest_Parse = { mtype: "dsd.parse", code: parseObject, oldSyntax: oldSyntax };
        var messages = this.DoDSDRequestResponse(task);
        return this.ToParseCodeObservables(messages);
    }

    /** Starts an expand operation. This will result in a fully-expanded CRN. If you call this after a UserParse call on the same code, it will not parse again; instead, it will just expand the previous parse result. */
    UserExpand(code: string, oldSyntax: boolean, model: CRN.IG, options: Interfaces.DsdSettings): Interfaces.ParseDSDObservables {
        if (code == null) code = "";
        var parseObject: Interfaces.ParseObject = { code: code, toeholds: this.toeholds, specificities: this.specificities };
        var task: Interfaces.WorkerRequest_Expand = { mtype: "dsd.expand", code: parseObject, model: model, settings: options, oldSyntax: oldSyntax };
        var messages = this.DoDSDRequestResponse(task);
        return this.ToParseCodeObservables(messages);
    }
}

export default ClassicDSD