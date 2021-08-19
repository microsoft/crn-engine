import "jquery";
import * as Rx from 'rx';
import * as InternalInterfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/InternalInterfaces';
import * as CRNInterfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as Interfaces from './Interfaces';
import CRNEngine from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import DefaultDatabase from './DefaultDatabase';
import ClassicGECWorker from "worker-loader?filename=./ClassicGEC.worker.[hash].js!./ClassicGEC.worker.js";

/** Provides asynchronous access to GEC F# methods. */
class ClassicGEC extends CRNEngine {
    constructor(server: boolean) {
        super(server, ClassicGECWorker);
    }

    private DoGECRequestResponse(task: Interfaces.WorkerRequest) {
        return this.DoRequestResponse(<InternalInterfaces.WorkerRequest><any>task);
    }

    private parts: string = DefaultDatabase.parts;
    get Parts(): string { return this.parts; }
    set Parts(value: string) { this.parts = value; }

    private reactions: string = DefaultDatabase.reactions;
    get Reactions(): string { return this.reactions; }
    set Reactions(value: string) { this.reactions = value; }

    UserCompile(code: string): Interfaces.CompileGECObservables {
        var task: Interfaces.WorkerRequest_GECCompile = { mtype: "gec.compile", code: code, parts: this.parts, reactions: this.reactions };
        var messages = <Rx.Observable<Interfaces.WorkerResponse>>this.DoGECRequestResponse(task);
        var ret = {
            crn: messages.where(v => v.mtype == "model").select(v => (<InternalInterfaces.WorkerResponse_Model>v).model),
            exports: messages.where(v => v.mtype == "export").select(v => (<InternalInterfaces.WorkerResponse_Export>v).export),
            solution_count: messages.where(v => v.mtype == "gec.solutions").select(v => (<Interfaces.WorkerResponse_GECSolutions>v).count),
            jsbol: messages.where(v => v.mtype == "gec.jsbol").select(v => (<Interfaces.WorkerResponse_JSBOL>v).document),
        };
        return ret;
    }

    UserGetSolution(idx: number): Interfaces.GetSolutionObservables {
        var task: Interfaces.WorkerRequest_GECGetSolution = { mtype: "gec.getsolution", idx: idx };
        let messages = <Rx.Observable<Interfaces.WorkerResponse>>this.DoGECRequestResponse(task);
        let ret = {
            solution: messages.where(v => v.mtype == "gec.solution").select(v => (<Interfaces.WorkerResponse_GECSolution>v).solution),
            jsbol: messages.where(v => v.mtype == "gec.jsbol").select(v => (<Interfaces.WorkerResponse_JSBOL>v).document),
            exports: messages.where(v => v.mtype == "export").select(v => (<InternalInterfaces.WorkerResponse_Export>v).export)
        };
        return ret;
    }
}

export default ClassicGEC