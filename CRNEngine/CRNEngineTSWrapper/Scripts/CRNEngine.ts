// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import { WebSharperGeneratedInterfaces as WGI } from "./WebSharperGeneratedInterfaces";
import * as Interfaces from './Interfaces';
import * as Internals from './InternalInterfaces';
import * as Rx from 'rx';
import CRNEngineWorker from "worker-loader?filename=./CRNEngine.worker.[hash].js!./CRNEngine.worker.js";
import FastTasksWorker from "worker-loader?filename=./FastTasks.worker.[hash].js!./FastTasks.worker.js";
import { WorkerRequest_ParseCode } from "./InternalInterfaces";

/** This class represents a worker from the threadpool (see below). It wraps a web worker, providing a task-based interface to it. */
class ThreadpoolWorker {
    constructor(worker: Worker) {
        var that = this;
        this.worker = worker;
        this.task = null;
        this.worker.addEventListener("message", e => that.HandleWorkerMessage(e));
    }

    /** The Worker this instance is wrapping. */
    private worker: Worker;
    /** The task currently being executed. */
    private task: any;
    private messages: Rx.Subject<Internals.WorkerResponse>;

    /** Returns true if this worker does not have a task running. */
    public IsIdle(): boolean { return this.task == null; }
    public OnIdle: () => void = () => { };

    /** Aborts the current operation. This terminates the worker. */
    public Abort() {
        this.worker.terminate();
        if (this.messages != null)
            this.messages.onCompleted();
    }

    /** Perform operations consequent to the end of a task. */
    private Finalize() {
        this.task = null;
        this.messages = null;
        this.OnIdle();
    }

    /** Handles a message coming from the worker, invoking the appropriate callbacks. Cleans up the current task if the message indicates its completion. */
    private HandleWorkerMessage(d: any) {
        var msg = <Internals.WorkerResponse>d.data;
        if (msg.mtype == "error") {
            try {
                this.messages.onError((<Internals.WorkerResponse_Error>msg).error);
            } catch (e) {
                console.log("threadpool worker onError handler threw " + JSON.stringify(e));
            };
            this.Finalize();
        }
        else if (msg.mtype == "finished" || msg.mtype == "aborted") {
            try { this.messages.onCompleted(); } catch (e) { console.log("worker onCompleted handler threw " + JSON.stringify(e)); };
            this.Finalize();
        }
        else
            try { this.messages.onNext(msg); } catch (e) { console.log("worker onNext handler threw " + JSON.stringify(e)); };
    }

    /** Starts a new task, posting it to the underlying Worker. Fails if there is already a task running. Returns an observable with the worker's messages for this task. */
    public StartTask(task: any): Rx.Observable<Internals.WorkerResponse> {
        if (!this.IsIdle())
            throw "invalid operation";
        this.task = task;
        this.messages = new Rx.Subject<Internals.WorkerResponse>();
        this.worker.postMessage(task);
        return this.messages;
    }
}

/** This class represents a pool of web workers that can be used and reused to complete tasks. As more tasks are assigned to it, it will spawn more workers, until reaching a limit. When the limit is reached, any further task will be enqueued, and assigned to the first available worker. This class will collect messages coming from the worker, and expose them as an Observable. */
class Threadpool {
    /** The single instance of this class. */
    public static Instance: Threadpool;
    /** The maximum number of workers. This number should be picked with some care, as browsers have limits on the total number of workers that a page can run, and they may fail ungracefully if the limit is reached. The Threadpool should not hog too many workers for this reason. */
    private static workerLimit = 4;

    /** The threadpool needs to know how to make a new worker. */
    constructor(private workerConstructor: { new(): Worker }) { }

    /** The actual Workers. These are ThreadpoolWorker wrappers; see above. */
    private workers: ThreadpoolWorker[] = [];
    /** The queue of tasks that have not been assigned to a worker yet. */
    private taskQueue: { task: any, subject: Rx.Subject<Internals.WorkerResponse> }[] = [];

    /** This function will look at the queue and start a new task if a slot is available. It gets called every time a worker completes its task, so there is no need to attempt to start more than one task (there will never be more than one free slot). If there are no more tasks and every worker is idle, it fires the task queue cleared event. */
    private CheckQueue(): void {
        // Look for an idle worker to post a new task to.
        if (this.taskQueue.length > 0)
            for (var i = 0; i < this.workers.length; i++) {
                if (this.workers[i].IsIdle()) {
                    var taskQ = this.taskQueue.shift();
                    var task = taskQ.task;
                    var subject = taskQ.subject;
                    var messages = this.workers[i].StartTask(task);
                    // Pipe the messages to the previously-created subject.
                    messages.subscribe(r => subject.onNext(r), e => subject.onError(e), () => subject.onCompleted());
                    break;
                }
            }
    }

    /** Creates a new Worker and sets it up as a threadpool worker. */
    private SpawnNewWorker(): ThreadpoolWorker {
        console.log("CRNEngine is spawning a new worker");
        var that = this;
        var newWorker = new this.workerConstructor();
        var tWorker = new ThreadpoolWorker(newWorker);
        // When it completes a task, check the queue for more tasks.
        tWorker.OnIdle = () => that.CheckQueue();
        this.workers.push(tWorker);
        return tWorker;
    }

    /** Attempts to start a new task. If there is an idle worker, the task will be dispatched to that worker. If no workers are idle, but the total number of worker is lower than the limit, it will spawn a new worker and dispatch the task to that worker. If all possible workers are busy, it will put the task in a queue. */
    public StartParallelTask(task: Internals.WorkerRequest): Rx.Observable<Internals.WorkerResponse> {
        // If there are no workers, or all workers are busy but I can spawn a new one, spawn a new one.
        if ((this.workers.length == 0 || this.workers.every(w => !w.IsIdle())) && this.workers.length < Threadpool.workerLimit)
            this.SpawnNewWorker();
        // Create a Subject to expose the messages coming from the Worker.
        var subject = new Rx.Subject<Internals.WorkerResponse>();
        var taskQ = { task: task, subject: subject };
        // Put the task in the queue.
        this.taskQueue.push(taskQ);
        // Check the queue. If I have just spawned a new worker, the task will be assigned to it.
        this.CheckQueue();
        return subject;
    }

    public Abort() {
        while (this.workers.length > 0)
            this.workers.pop().Abort();
    }
}

/** Provides asynchronous access to CRNEngine F# methods, based on Rx. The user program invokes a function on this class and gets a bunch of Observables in return.

A web worker is maintained, which executes all F# code. The web worker is launched at first request, and then executes one task at a time sequentially. Tasks can be aborted, and attempting to start a new task before the current one is done will cause the current task to be aborted. When a task is aborted, the web worker is terminated. If a new task is started after an abort, a new web worker is launched.

Communication between the main thread and the web worker is based on the interfaces defined in Interfaces.ts. The main thread sends a single message that begins a task, and the worker responds with one or more messages. Messages end up in a callback function that passes them to an internal Rx observer, which splits them into the final Observables. */
class ModellingEngine {
    public static GetWebSocketURL() {
        return "ws://" + document.location.host + "/websocket";
    }

    constructor(public ServerMode: boolean, private workerConstructor?: { new(): Worker }) {
        if (this.workerConstructor == null)
            this.workerConstructor = CRNEngineWorker;
        if (Threadpool.Instance == null)
            Threadpool.Instance = new Threadpool(this.workerConstructor);
    }

    /** This should return true if the current CRN is targeting JIT simulation. */
    protected isJIT(model: Interfaces.Model) {
        return false;
    }

    /** Call this to release any resources associated with this object. In practice, this will shut down workers and cause all current operations to terminate (synchronously). */
    Dispose() {
        this.Abort();
        this.AbortFastTasks();
    }

    /** The Worker used for long tasks (i.e. parsing, simulating, and so on). This Worker will process one task at a time. If the main thread performs a request while another one is still being processed, the Worker will be terminated. If the Worker is terminated, then this variable gets set to null. */
    private currentWorker: Worker = null;

    /** The internal observer for the current task. This observer will know how to split up the messages in order to feed the final Observables. The observer is set before starting a new task. Note that there is only one of these at any given time, because only one task can be active at any given time. */
    private currentWorkerObserver: Rx.Observer<Internals.WorkerResponse>;
    private observerQueue: Rx.Observer<Internals.WorkerResponse>[] = [];

    private nextObserver() {
        if (this.observerQueue.length > 0)
            this.currentWorkerObserver = this.observerQueue.shift();
        else
            this.currentWorkerObserver = null;
    }

    /** This function is the callback for a worker operation. It will pass the message to the internal observer. */
    private WorkerCallback(d: any) {
        var msg = <Internals.WorkerResponse>d.data;
        if (msg.mtype == "error") {
            try {
                this.currentWorkerObserver.onError((<Internals.WorkerResponse_Error>msg).error);
            } catch (e) {
                console.log("worker onError handler threw " + JSON.stringify(e));
            };
            this.nextObserver();
        }
        else if (msg.mtype == "finished" || msg.mtype == "aborted") {
            this.currentWorkerObserver.onCompleted();
            this.nextObserver();
        }
        else
            this.currentWorkerObserver.onNext(msg);
    }

    /** This function is the error callback for worker operations. It will convert it into an IError message for the internal observer. Note that this handles cases where something goes unexpectedly wrong inside the worker, not cases such as e.g. parsing errors, which are part of the normal functioning of the program. */
    private WorkerErrorCallback(ev: ErrorEvent) {
        // These errors are not normal. I'll abort the worker, just in case its internal state has somehow become corrupted.
        this.Abort();
        console.error("WebWorker encountered error in" + ev.filename + ' lineno: ' + ev.lineno + ' error: ' + ev.message);
        if (this.currentWorkerObserver) {
            this.currentWorkerObserver.onError({ message: ev.message });
        }
        this.nextObserver();
    }

    /** Aborts the current operation, whatever it is. If the current task is null, it means that the worker is idle. Therefore, there is no need to terminate it. If the current task is not null, then the worker must be terminated. The following request to start a task will start a new one. If multiple operations are queued, they are all aborted. */
    public Abort() {
        Threadpool.Instance.Abort();
        while (this.currentSocketObservers.length > 0)
            // Note that this will cause the dispose function of the observer to be invoked, which will remove itself from the list.
            this.currentSocketObservers[0].onCompleted();
        if (this.currentWorkerObserver != null) {
            this.currentWorker.terminate();
            this.currentWorker = null;
            while (this.currentWorkerObserver != null) {
                this.currentWorkerObserver.onCompleted();
                this.nextObserver();
            }
        }
    }

    public IsIdle(): boolean {
        return this.currentSocketObservers.length == 0 && this.currentWorkerObserver == null;
    }

    /** Makes sure that there is a Worker ready to accept a new task. If there is need to spawn a new Worker, it hooks up the message handlers. */
    private PrepareWorker(): void {
        var that = this;
        if (this.currentWorker == null) {
            this.currentWorker = new this.workerConstructor();
            this.currentWorker.addEventListener("message", e => {
                that.WorkerCallback(e);
            });
            this.currentWorker.addEventListener("error", e => {
                that.WorkerErrorCallback(e);
            });
        }
    }

    /** Starts a new Task, and returns the internal Observable of the Worker's messages. The Observable is shared, so that the caller function can split its messages into multiple Observables for the user program to subscribe to. */
    protected StartNewTask(task: Internals.WorkerRequest): Rx.Observable<Internals.WorkerResponse> {
        var that = this;
        this.PrepareWorker();
        // Create the internal observable.
        var messages = Rx.Observable.create<Internals.WorkerResponse>((observer: Rx.Observer<Internals.WorkerResponse>) => {
            // This function gets called when the user code subscribes to any of the Observables that it has received from the caller function.
            // Note that this function will only be called once, because this Observable will be shared (see below).

            // Take a reference of the observer, so that the worker callback knows where to send the messages.
            if (that.currentWorkerObserver == null)
                that.currentWorkerObserver = observer;
            else
                that.observerQueue.push(observer);
            // Post the message to the worker.
            that.currentWorker.postMessage(task);
        });
        // "Share" the observable. This means that the caller function can create several Observables out of its messages, but there will be
        // only one observer underneath. This is necessary so that all of the final Observables are actually observing the same task!
        var published = messages.share();
        return published;
    }

    public UserParseCode(code: string): Interfaces.ParseCodeObservables {
        if (code == null)
            code = "";
        var messages = this.DoRequestResponse({ mtype: "parsecode", code: code });
        return {
            model: messages.where(v => v.mtype == "model").select(v => (<Internals.WorkerResponse_Model>v).model),
        };
    }

    public UserGenerateExports(ig: Interfaces.IG, nodeId: string): Interfaces.ExportObservables {
        var messages = this.DoRequestResponse({ mtype: "generateexports", nodeId: nodeId, model: ig });
        return {
            exports: messages.where(v => v.mtype == "export").select(v => (<Internals.WorkerResponse_Export>v).export)
        };
    }

    public UserGenerateExport(ig: Interfaces.IG, nodeId: string, id: string, instance: string): Interfaces.ExportObservables {
        var messages = this.DoRequestResponse({ mtype: "generateexport", nodeId: nodeId, model: ig, id: id, instance: instance });
        return {
            exports: messages.where(v => v.mtype == "export").select(v => (<Internals.WorkerResponse_Export>v).export)
        };
    }

    /** The WebSocket used for the current server-side task. Todo: this should allow for multiple operations. */
    private currentSocketObservers: Rx.Observer<Internals.WorkerResponse>[] = [];

    protected FromWebSocket(request: Internals.WorkerRequest): Rx.Observable<Internals.WorkerResponse> {
        var ws = new WebSocket(ModellingEngine.GetWebSocketURL());

        //Into socket
        var observer = Rx.Observer.create((data) => {
            if (ws.readyState === WebSocket.OPEN) { ws.send(<any>data); }
            //?throw/buffer etc on socket close?
        });

        // Prepare to send the request.
        ws.onopen = (e) => {
            var req = JSON.stringify(request);
            ws.send(req);
        };

        //Out of socket
        var observable = Rx.Observable.create<Internals.WorkerResponse>((obs) => {
            // Add observer to the list of observers.
            this.currentSocketObservers.push(obs);
            // bind observable to web socket
            ws.onmessage = (m) => {
                var msg = JSON.parse(m.data);
                if (msg.mtype == "error") {
                    try {
                        obs.onError((<Internals.WorkerResponse_Error>msg).error);
                    } catch (e) {
                        console.log("socket onError handler threw " + JSON.stringify(e));
                    };
                }
                else if (msg.mtype == "finished" || msg.mtype == "aborted") {
                    obs.onCompleted();
                }
                else
                    obs.onNext(msg);
            };
            ws.onerror = obs.onError.bind(obs);
            ws.onclose = obs.onCompleted.bind(obs);

            // disposable to close socket
            return () => {
                // Remove the observer from the list of observers.
                this.currentSocketObservers = this.currentSocketObservers.filter(o => o != obs);
                ws.close();
            };
        });

        return observable.share();
    }

    protected DoRequestResponse(request: Internals.WorkerRequest): Rx.Observable<Internals.WorkerResponse> {
        return this.ServerMode ? this.FromWebSocket(request) : this.StartNewTask(request);
    }

    public UserInferGui(ig: Interfaces.IG): Interfaces.InferenceObservables {
        var messages = this.DoRequestResponse({ mtype: "infergui", model: ig, pool: this.pool });
        return {
            exports: messages.where(v => v.mtype == "export").select(v => (<Internals.WorkerResponse_Export>v).export),
            progress: messages.where(v => v.mtype == "inferenceprogress").select(v => {
                let msg = <Internals.WorkerResponse_InferenceProgress>v;
                return { nodeId: msg.nodeId, iteration: msg.progress };
            }),
            instances: messages.where(v => v.mtype == "instancedefinitions").select(v => {
                let msg = <Internals.WorkerResponse_SimInstanceDefinitions>v;
                return { nodeId: msg.nodeId, definitions: msg.definitions };
            }),
            inferencechainupdate: messages.where(v => v.mtype == "inferencechainupdate").select(v => {
                let msg = <Internals.WorkerResponse_InferenceChain>v;
                return { nodeId: msg.nodeId, values: msg.update };
            }),
            parameterdefinitions: messages.where(v => v.mtype == "parameterdefinitions").select(v => {
                let msg = <Internals.WorkerResponse_ParameterDefinitions>v;
                return { nodeId: msg.nodeId, parameters: msg.parameters };
            }),
            parameterresults: messages.where(v => v.mtype == "parameterresult").select(v => {
                let msg = <Internals.WorkerResponse_ParameterResult>v;
                return { nodeId: msg.nodeId, values: msg.values };
            }),
            summary: messages.where(v => v.mtype == "summary").select(v => {
                let msg = <Internals.WorkerResponse_Summary>v;
                return { nodeId: msg.nodeId, summary: msg.summary };
            }),
            inferenceresults: messages.where(v => v.mtype == "inferenceresult").select(v => {
                let msg = <Internals.WorkerResponse_InferenceResult>v;
                return { nodeId: msg.nodeId, result: msg.result };
            })
        };
    }

    /** Runs a single simulation in a secondary worker, and returns only the non-initial messages (i.e. the sim results and probabilities). */
    private SimulateSingle(model: Interfaces.IG, nodeId: string, definition: Interfaces.SimulationInstanceDefinition): Rx.Observable<Internals.WorkerResponse> {
        return Threadpool.Instance.StartParallelTask({ mtype: "simulatesingle", model: model, nodeId: nodeId, definition: definition });
    }

    /** This function retrieves the first model in an inference graph. The purpose of this function is to serve as a bridge to allow using functions that work on models only. Eventually, the objective is to obsolete this function when every method runs on inference graphs natively. */
    private GetFirstModel(ig: Interfaces.IG) {
        for (let nodeid in ig.nodes)
            return ig.nodes[nodeid];
        return null;
    }

    /** Note that the messages received here are the result of a "getsimruns..." request to the worker. This is a request to prepare the CRN and the instances, without running a simulation. Therefore, I need to get those instances and then call SimulateSingle on each of them, using the results in the SimulatorObservables. This is used in the parallel simulation case. */
    private RunSimulationParallel(ig: Interfaces.IG, nodeId: string, instancesMessages: Rx.Observable<Internals.WorkerResponse>): Interfaces.SimulatorObservables {
        var that = this;
        // I'm using Subjects. While this may not be the most elegant solution, it's efficient and easy to follow.
        var ret = {
            node: new Rx.Subject<Interfaces.Model>(),
            exports: new Rx.Subject<Interfaces.ExportDef>(),
            instances: new Rx.Subject<Interfaces.SimulationInstanceDefinition[]>(),
            statespace: new Rx.Subject<Interfaces.StateSpace>(),
            probabilities: new Rx.Subject<Interfaces.InstanceProbabilities>(),
            simrun: new Rx.Subject<Interfaces.SimRunDefinition>()
        };
        // I'll need to make a note of the sim type, in order to run the simulations.
        var t: Interfaces.SimResultType = null;
        // If there is any error in the initial messages, then all of the results are in error.
        instancesMessages.subscribeOnError(e => {
            ret.node.onError(e);
            ret.exports.onError(e);
            ret.instances.onError(e);
            ret.statespace.onError(e);
            ret.probabilities.onError(e);
            ret.simrun.onError(e);
        });
        // Listen to the sim type and store it.
        instancesMessages.where(v => v.mtype == "simtype").subscribeOnNext(v => { t = (<Internals.WorkerResponse_SimType>v).simtype; });
        // Listen to the exports. Pipe them to the final results directly.
        instancesMessages.where(v => v.mtype == "export").subscribeOnNext(v => {
            var e = (<Internals.WorkerResponse_Export>v).export;
            ret.exports.onNext(e);
        });
        // Listen to the sim instance definitions. Note that I will only receive one of these, which will contain all of the definitions.
        instancesMessages.where(v => v.mtype == "instancedefinitions").subscribeOnNext(v => {
            var model = ig.nodes[nodeId];
            // Send the program.
            ret.node.onNext(model);
            // Finalize the initial observables (if non-JIT). Note that JIT simulations may output CRNs after results.
            if (!this.isJIT(model))
                ret.node.onCompleted();

            // Here are the sim instance definitions.
            var definitions = (<Internals.WorkerResponse_SimInstanceDefinitions>v).definitions;
            // Pipe them to the final results and finalize the Subject.
            ret.instances.onNext(definitions);
            ret.instances.onCompleted();

            // I'll call SimulateSingle on each instance, therefore generating an array of incoming result streams. These are the messages coming from the secondary workers. Note that the messages here can be both sim results and probabilities.
            var results: Rx.Observable<Internals.WorkerResponse>[] = definitions.map(def => that.SimulateSingle(ig, nodeId, def));
            // I'll combine the result streams into one stream.
            var resultsStream = Rx.Observable.merge(results);
            // Then, I'll split it again according to message type.
            var groupedResults = resultsStream.groupBy(m => m.mtype).share();
            // Separate the stream in sim results and probabilities (groupBy does this efficiently). Also use select to turn them into streams of the actual response objects.
            var simResults = groupedResults.where(g => g.key == "simresult").selectMany(g => g).select(m => (<Internals.WorkerResponse_SimResult>m).row);
            var simTables = groupedResults.where(g => g.key == "simtable").selectMany(g => g).select(m => (<Internals.WorkerResponse_SimTable>m).table);
            var newPlottables = groupedResults.where(g => g.key == "newplottable").selectMany(g => g).select(m => (<Internals.WorkerResponse_NewPlottable>m).plottable);

            // Pipe the sim results stream to the final results.
            ret.simrun.onNext({ simtype: t, values: simResults, tables: simTables, newplottables: newPlottables });
            // Finalize the other standing observables.
            ret.simrun.onCompleted();
            // Pipe the state space stream to the final results.
            var ssResults = groupedResults.where(g => g.key == "statespace").selectMany(g => g).select(m => (<Internals.WorkerResponse_StateSpace>m).statespace);
            ssResults.subscribe(ss => ret.statespace.onNext(ss), error => ret.statespace.onError(error), () => ret.statespace.onCompleted());
            // Pipe the probabilities stream to the final results.
            var probResults = groupedResults.where(g => g.key == "probabilities").selectMany(g => g).select(m => (<Internals.WorkerResponse_Probabilities>m).probabilities);
            probResults.subscribe(prob => ret.probabilities.onNext(prob), error => ret.probabilities.onError(error), () => ret.probabilities.onCompleted());
            // If the simulation is JIT...
            if (this.isJIT(model)) {
                // Pipe the final Model.
                var jitModels = groupedResults.where(g => g.key == "node").selectMany(g => g).select(m => (<Internals.WorkerResponse_Node>m).node);
                jitModels.subscribe(node => ret.node.onNext(node), error => ret.node.onError(error), () => { });
            }
            // Pipe the final exports.
            var jitExports = groupedResults.where(g => g.key == "export").selectMany(g => g).select(m => (<Internals.WorkerResponse_Export>m).export);
            jitExports.subscribe(exports => ret.exports.onNext(exports), error => ret.exports.onError(error), () => { });
            // When everything is done, signal completion of Models and exports.
            resultsStream.subscribeOnCompleted(() => { ret.node.onCompleted(); ret.exports.onCompleted(); });
        });

        return ret;
    }

    /** This is the function that converts a stream of messages from the main worker into an Interfaces.SimulatorObservables. It is used in the non-parallel simulation case. */
    private MakeSimulationResultsSet(source: Rx.Observable<Internals.WorkerResponse>): Interfaces.SimulatorObservables {
        // The source is producing a stream of undifferentiated messages. I need to sort these into a number of separate Observables.
        var groups = source.groupBy(m => m.mtype).share();

        var node: Rx.Observable<Interfaces.Model> = groups.where(g => g.key == "node")
            .selectMany(g => g).select(m => (<Internals.WorkerResponse_Node>m).node);
        var exp: Rx.Observable<Interfaces.ExportDef> = groups.where(g => g.key == "export")
            .selectMany(g => g).select(m => {
                var e = (<Internals.WorkerResponse_Export>m).export;
                return e;
            });
        var instances: Rx.Observable<Interfaces.SimulationInstanceDefinition[]> = groups.where(g => g.key == "instancedefinitions")
            .selectMany(g => g).select(m => (<Internals.WorkerResponse_SimInstanceDefinitions>m).definitions);

        var results: Rx.Observable<Interfaces.SimResult<Interfaces.SimResultValue>> = groups.where(g => g.key == "simresult")
            .selectMany(g => g).select(m => (<Internals.WorkerResponse_SimResult>m).row);
        var tables: Rx.Observable<Interfaces.SimTable<Interfaces.SimResultValue>> = groups.where(g => g.key == "simtable")
            .selectMany(g => g).select(m => (<Internals.WorkerResponse_SimTable>m).table);
        var newplottables: Rx.Observable<Interfaces.NewPlottableDefinition> = groups.where(g => g.key == "newplottable").selectMany(g => g).select(m => (<Internals.WorkerResponse_NewPlottable>m).plottable);
        var statespace: Rx.Observable<Interfaces.StateSpace> = groups.where(g => g.key == "statespace")
            .selectMany(g => g).select(m => (<Internals.WorkerResponse_StateSpace>m).statespace);
        var probabilities: Rx.Observable<Interfaces.InstanceProbabilities> = groups.where(g => g.key == "probabilities")
            .selectMany(g => g).select(m => (<Internals.WorkerResponse_Probabilities>m).probabilities);
        var simrun: Rx.Observable<Interfaces.SimRunDefinition> = groups.where(g => g.key == "simtype")
            .selectMany(g => g).select(m => {
                return {
                    simtype: (<Internals.WorkerResponse_SimType>m).simtype,
                    values: results,
                    tables: tables,
                    newplottables: newplottables
                }
            });
        return {
            node: node,
            exports: exp,
            instances: instances,
            statespace: statespace,
            probabilities: probabilities,
            simrun: simrun
        };
    }

    /** Executes a simulation of the specified CRN. Runs multiple sweeps simultaneously. */
    public UserSimulateGui(ig: Interfaces.IG, nodeId: string): Interfaces.SimulatorObservables {
        // JIT sims in general may rely on local unserialisable data generated by previous calls, and cannot be run in parallel.
        let model = this.GetFirstModel(ig);
        if (!this.isJIT(model) && this.ServerMode == false) {
            var sweepMessages = this.StartNewTask({ mtype: "getsimrunsgui", model: ig, nodeId: nodeId });
            return this.RunSimulationParallel(ig, nodeId, sweepMessages);
        } else {
            var messages = this.DoRequestResponse({ mtype: "simulategui", model: ig, nodeId: nodeId, pool: this.pool });
            return this.MakeSimulationResultsSet(messages);
        }
    }

    /** Computes the state space for a given CRN. Note that state spaces may be infinite, so this operation may not complete. */
    public UserStateSpace(ig: Interfaces.IG, jit: boolean): Rx.Observable<Interfaces.StateSpace> {
        var messages = this.DoRequestResponse({ mtype: "statespace", jit: jit, model: ig });
        return messages.where(v => v.mtype == "statespace").select(v => (<Internals.WorkerResponse_StateSpace>v).statespace);
    }

    /** Generates the probability map for a specific species. */
    public GetProbabilityMap(probabilities: Interfaces.Probabilities, species: string, lowerBound: number): Rx.Observable<Interfaces.ProbabilityMap> {
        var probabilityMapMessages = this.DoRequestResponse({ mtype: "getprobabilitymap", probabilities: probabilities, species: species, lowerBound: lowerBound });
        var ret = probabilityMapMessages.where(v => v.mtype == "probabilitymap").select(v => (<Internals.WorkerResponse_ProbabilityMap>v).map);
        return ret;
    }

    public UserSynthesis(ig: Interfaces.IG, nodeId: string, crnId: string): Rx.Observable<Interfaces.SynthesisResult> {
        var messages = this.DoRequestResponse({ mtype: "synthesis", model: ig, nodeId: nodeId, crnId: crnId });
        return messages.where(v => v.mtype == "synthesis").select(v => (<Internals.WorkerResponse_Synthesis>v).result);
    }

    public GetBistabilityPlot(crn: Interfaces.CRN, solution: { [name: string]: number }, spX: string, spY: string, numPoints: number): Rx.Observable<Interfaces.BistabilityPlot> {
        var messages = this.DoRequestResponse({ mtype: "bistability", crn:crn, solution: solution, spX: spX, spY: spY, numPoints: numPoints });
        return messages.where(v => v.mtype == "bistability").select(v => (<Internals.WorkerResponse_Bistability>v).plot);
    }


    /* JOB MANAGEMENT. These functions are related to the job manager, for Azure batch tasks. They can only be used in server mode. */

    /** The pool where tasks will be executed. An empty string indicates local execution; anything else indicates cloud execution. */
    public pool: string = "";

    public GetCloudCapabilities(): Rx.Observable<Interfaces.CloudCapabilities> {
        var capMessages = this.DoRequestResponse({ mtype: "getcloudcapabilities" });
        return capMessages.where(v => v.mtype == "pools").select(v => {
            var c = (<Internals.JobsResponse_CloudCapabilities>v);
            return { account: c.account, pools: c.pools };
        });
    }

    public GetJobs(allFiles: boolean): Rx.Observable<Interfaces.JobDescriptor[]> {
        var jobsMessages = this.DoRequestResponse({ mtype: "getjobs", allFiles: allFiles });
        var ret = jobsMessages.where(v => v.mtype == "jobs").select(v => (<Internals.JobsResponse_Jobs>v).jobs);
        return ret;
    }

    public StopJob(id: string): Rx.Observable<{}> {
        var jobsMessages = this.DoRequestResponse({ mtype: "stopjob", id: id });
        var ret = jobsMessages.select(v => { return {}; });
        return ret;
    }

    public DeleteJob(id: string): Rx.Observable<{}> {
        var jobsMessages = this.DoRequestResponse({ mtype: "deletejob", id: id });
        var ret = jobsMessages.select(v => { return {}; });
        return ret;
    }


    /* FAST TASKS. The functions in this section are expected to complete in real-time. They run on a separate worker. This worker is never aborted, as fast tasks are supposed to always complete in a timely manner. Therefore, it is possible for a request to be made while another task is being served (although this should be rare, because the tasks are fast). For this reason, a queue of tasks is maintained, rather than having a single task. Also, fast tasks are supposed to only have a single answer, which is either completion or failure. They don't have a stream of updates.Each time a fast task is queued, an Observable is created. Subscribing to that Observable causes the task to be started, with the observer being put in a queue. When the fast tasks worker returns a message, the next observer in the queue is dequeued, invoked, and terminated. */

    /**The Worker for fast tasks. */
    private fastTasksWorker: Worker = null;

    /**The queue of observers. */
    private fastTasksObservers: Rx.Observer<Internals.FastWorkerResponse>[] = [];

    /**The Worker callback. It will remove the first observer from the queue, send it the message, and complete it. Tasks are processed sequentially.*/
    private FastTasksWorkerCallback(d: any) {
        var msg = <Internals.FastWorkerResponse>d.data;
        var observer = this.fastTasksObservers.shift();
        if (msg.mtype == "error")
            observer.onError((<Internals.FastWorkerResponse_Error>msg).error);
        else {
            observer.onNext(msg);
            observer.onCompleted();
        }
    }

    /** This function is the error callback for worker operations. It will convert it into an IError message for the observer. Note that this handles cases where something goes unexpectedly wrong inside the worker, not cases such as e.g. parsing errors, which are part of the normal functioning of the program. */
    private FastTasksWorkerErrorCallback(ev: ErrorEvent) {
        // These errors are not normal. I'll abort the worker, just in case its internal state has somehow become corrupted.
        console.error("WebWorker encountered error in" + ev.filename + ' lineno: ' + ev.lineno + ' error: ' + ev.message);
        // Ship the error to all observers.
        while (this.fastTasksObservers.length > 0)
            this.fastTasksObservers.shift().onError({ message: ev.message });
        this.AbortFastTasks();
    }

    /** This function will abort all current fast tasks, and terminate the Worker. This should normally never be invoked, and is private because the user program should never need to invoke it. */
    private AbortFastTasks() {
        if (this.fastTasksWorker != null) {
            this.fastTasksWorker.terminate();
            this.fastTasksWorker = null;
        }
        while (this.fastTasksObservers.length > 0)
            this.fastTasksObservers.shift().onCompleted();
    }

    /** Enqueues a new task for the fast tasks worker. Returns an Observable of the task's return type. Note that this assumes the task will send back exactly one message. */
    private StartNewFastTask(task: Internals.FastWorkerRequest): Rx.Observable<Internals.FastWorkerResponse> {
        var that = this;

        if (this.fastTasksWorker == null) {
            this.fastTasksWorker = new FastTasksWorker();
            this.fastTasksWorker.addEventListener("message", e => that.FastTasksWorkerCallback(e));
            this.fastTasksWorker.addEventListener("error", e => that.FastTasksWorkerErrorCallback(e));
        }

        var observable = Rx.Observable.create<Internals.FastWorkerResponse>(function (observer: Rx.Observer<Internals.FastWorkerResponse>) {
            that.fastTasksObservers.push(observer);
            that.fastTasksWorker.postMessage(task);
        });

        return observable;
    }

    public ParseExpression(str: string): Rx.Observable<Interfaces.Expression> {
        var messages = this.StartNewFastTask({ mtype: "parseexpression", expression: str });
        return messages.select(res => (<Internals.FastWorkerResponse_Expression>res).expression);
    }

    public ParseTime(str: string): Rx.Observable<Interfaces.TimeUnit> {
        var messages = this.StartNewFastTask({ mtype: "parsetimeunit", unittext: str });
        return messages.select(res => (<Internals.FastWorkerResponse_TimeUnit>res).unit);
    }

    public ParseSpace(str: string): Rx.Observable<Interfaces.SpaceUnit> {
        var messages = this.StartNewFastTask({ mtype: "parsespaceunit", unittext: str });
        return messages.select(res => (<Internals.FastWorkerResponse_SpaceUnit>res).unit);
    }

    public ParseConcentration(str: string): Rx.Observable<Interfaces.ConcentrationUnit> {
        var messages = this.StartNewFastTask({ mtype: "parseconcentrationunit", unittext: str });
        return messages.select(res => (<Internals.FastWorkerResponse_ConcentrationUnit>res).unit);
    }

    public StringifyTime(time: Interfaces.TimeUnit): Rx.Observable<string> {
        var messages = this.StartNewFastTask({ mtype: "stringifytimeunit", unit: time });
        return messages.select(res => (<Internals.FastWorkerResponse_String>res).str);
    }

    public StringifySpace(Space: Interfaces.SpaceUnit): Rx.Observable<string> {
        var messages = this.StartNewFastTask({ mtype: "stringifyspaceunit", unit: Space });
        return messages.select(res => (<Internals.FastWorkerResponse_String>res).str);
    }

    public StringifyConcentration(concentration: Interfaces.ConcentrationUnit): Rx.Observable<string> {
        var messages = this.StartNewFastTask({ mtype: "stringifyconcentrationunit", unit: concentration });
        return messages.select(res => (<Internals.FastWorkerResponse_String>res).str);
    }
}

export default ModellingEngine