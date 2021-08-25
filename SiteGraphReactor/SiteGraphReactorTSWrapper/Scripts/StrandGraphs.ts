// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import "jquery";
import * as Rx from 'rx';
import * as InternalInterfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import * as CRNInterfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as Interfaces from './Interfaces';
import CRNEngine from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import StrandGraphsWorker from "worker-loader?name=./StrandGraphs.worker.[hash].js!./StrandGraphs.worker.js";

/** Provides asynchronous access to SiteGraphReactor F# methods. */
class StrandGraphs extends CRNEngine {
    constructor(server: boolean) {
        super(server, StrandGraphsWorker);
    }

    private StartNewStrandGraphsTask(task: Interfaces.WorkerRequest): Rx.Observable<Interfaces.WorkerResponse> {
        return this.StartNewTask(<InternalInterfaces.WorkerRequest><any>task);
    }

    private ToParseSGObservables(messages: Rx.Observable<Interfaces.WorkerResponse>): Interfaces.ParseSGObservables {
        var ret = {
            result: messages.where(v => v.mtype == "sg.parseresult").select(v => (<Interfaces.WorkerResponse_ParseResult>v).result),
        };
        return ret;
    }

    /** Starts a compile operation. This will result in a fully-expanded CRN. This will always parse the code. */
    UserCompile(code: string): Interfaces.ParseSGObservables {
        var task: Interfaces.WorkerRequest_Compile = { mtype: "sg.compile", code: code };
        var messages = this.StartNewStrandGraphsTask(task);
        return this.ToParseSGObservables(messages);
    }

    /** Starts a parse operation. This will result in an unexpanded CRN. */
    UserParse(code: string): Interfaces.ParseSGObservables {
        var task: Interfaces.WorkerRequest_Parse = { mtype: "sg.parse", code: code };
        var messages = this.StartNewStrandGraphsTask(task);
        return this.ToParseSGObservables(messages);
    }

    /** Starts an expand operation. This will result in a fully-expanded CRN. If you call this after a UserParse call on the
    same code, it will not parse again; instead, it will just expand the previous parse result. */
    UserExpand(code: string): Interfaces.ParseSGObservables {
        var task: Interfaces.WorkerRequest_Expand = { mtype: "sg.expand", code: code };
        var messages = this.StartNewStrandGraphsTask(task);
        return this.ToParseSGObservables(messages);
    }
}

export default StrandGraphs