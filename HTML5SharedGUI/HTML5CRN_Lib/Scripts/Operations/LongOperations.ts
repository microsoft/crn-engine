// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

/** The interface for an operation that can be processed by the state machine. */
export interface IOperation {
    Initiate(): JQueryPromise<any>;
    Abort(): void;
    GetName(): string;
    /** After the operation has completed, this function should return an appropriate exit message. */
    GetExitMessage(): string;
}

export enum State {
    /** The state machine is currently not processing a queue. */
    Idle,
    /** The state machine is currently processing a queue, but no operation is executing. */
    Ready,
    /** The state machine is currently processing an operation. */
    InProgress
};

/** This class implements a state machine that manages a sequence of operations, each of which is an asynchronous procedure. It will execute the operations in the queue sequentially. The queue is added in a single batch, and the state machine cannot receive more operations while the queue is being processed. */
export class StateMachine {
    /** The exit message from the last completed operation. */
    private lastExitMessage = "";
    /** This flag is true of the last operation terminated with an error. */
    private lastIsError = false;
    /** A counter of state transitions, for debugging purposes. */
    private stateNum = 0;
    /** The current state of the state machine. */
    private state: State = State.Idle;
    /** The current queue of operations. This contains all operations that have not been started yet. */
    private queue: Array<IOperation> = [];
    /** The operation that is currently executing, if any. */
    private ongoingOperation: IOperation = null;
    /** The Promise for the opreation that is currently executing, if any. */
    private ongoingPromise: JQueryPromise<any> = null;
    /** A callback that will be invoked any time the state machine performs a transition. */
    private changeStateCallback: (State: any) => void = (newState) => { };

    /** Pass true if you want all transitions and debug information to be logged. */
    constructor(private Verbose = false) {
    }

    private LogVerbose(message: string) {
        if (this.Verbose)
            console.log("LongOpsStateMachine:" + this.stateNum + ":" + State[this.state] + ": " + message);
    }

    private EnqueueIteration() {
        setTimeout(() => { this.Iteration(); });
    }

    /** Executes the state machine logic. Determines whether to perform a transition. Executed every time an operation completes, or a queue is added, or right after a transition has completed. */
    private Iteration() {
        this.LogVerbose("Evaluating transition possibility.");
        var prevState: State = this.state;
        var nextState: State = this.state;
        switch (this.state) {
            case State.Idle:
                // State where the machine is waiting for the signal that tasks have been enqueued.
                if (this.queue != null) {
                    // The machine has been signaled. Transition to Ready state.
                    this.LogVerbose("Queue was filled. Activation requested. Transition to Ready state.");
                    nextState = State.Ready;
                }
                else
                    this.LogVerbose("No operations were enqueed.");
                break;
            case State.Ready:
                // State where the machine is checking whether the queue is empty.
                if (this.queue.length == 0) {
                    // The queue is empty. Transition to Idle state.
                    this.LogVerbose("Queue is empty. Transition to Idle state.");
                    this.queue = null;
                    nextState = State.Idle;
                }
                else {
                    // The queue is not empty. Dequeue an operation and transition to InProgress state.
                    nextState = State.InProgress;
                    this.ongoingOperation = this.queue.shift();
                    var name = this.ongoingOperation.GetName();
                    this.LogVerbose("Queue is not empty. Launching async operation \"" + name + "\".");
                    // Initiate the operation and store the Promise.
                    this.ongoingPromise = this.ongoingOperation.Initiate();
                    if (this.ongoingPromise == null)
                        console.log("ATTN: Initiate returned null for operation \"" + name + "\".");
                    this.LogVerbose("Operation initiated successfully. Transition to InProgress state.");
                }
                break;
            case State.InProgress:
                // State where the machine is waiting for an operation to complete.
                if (this.ongoingPromise == null) {
                    // Paranoia check.
                    console.log("ATTN: this.ongoingPromise null in state machine; this should never happen. Stopping.");
                    this.ongoingOperation = null;
                    nextState = State.Ready;
                }
                else {
                    // Look at the state of the Promise.
                    switch (this.ongoingPromise.state()) {
                        case "pending":
                            // The operation is in execution. I should already have a subscription set to trigger an iteration when it finishes, so I don't need to do anything right now.
                            this.LogVerbose("There is an ongoing operation. iteration will be triggered as the operation finishes.");
                            break;
                        case "resolved":
                            // The operation has been completed successfully. Retrieve the exit state, clean up my internal variables, and transition to Ready.
                            this.lastExitMessage = this.ongoingOperation.GetExitMessage() ? this.ongoingOperation.GetName() + ': ' + this.ongoingOperation.GetExitMessage() : "";
                            this.lastIsError = false;
                            this.LogVerbose("Ongoing operation promise is resolved. Transition to Ready state");
                            this.ongoingOperation = null;
                            this.ongoingPromise = null;
                            nextState = State.Ready;
                            break;
                        case "rejected":
                            // The operation has been completed unsuccessfully. Retrieve the exit state, clean up my internal variables, discard the queue, and transition to Idle.
                            this.lastExitMessage = this.ongoingOperation.GetExitMessage() ? this.ongoingOperation.GetName() + ': ' + this.ongoingOperation.GetExitMessage() : "";
                            this.lastIsError = true;
                            this.LogVerbose("Ongoing operation promise is rejected. halting queue processing. Transition to Idle state.");
                            this.ongoingOperation = null;
                            this.ongoingPromise = null;
                            nextState = State.Idle;
                            this.queue = [];
                            break;
                        default:
                            throw "Can not happen according to JQuery docs. see deferred.state() API docs"
                    }
                }
                break;
            default:
                throw "Not implemented yet";
        }

        if (prevState != nextState) {
            // Execute the transition. Change the state, and enqueue another iteration immediately (so I can evaluate whether a further transition should take place).
            this.state = nextState;
            this.stateNum++;
            this.LogVerbose("State changed from " + State[prevState] + " to " + State[nextState] + ".");
            this.EnqueueIteration();
            this.changeStateCallback(nextState);
        }
        else
            this.LogVerbose("State remained in " + State[prevState] + "; no further iteration requested.");

        // If I'm transitioning from Ready to InProgress, subscribe an iteration to the completion of the current Promise.
        if ((prevState == State.Ready) && this.ongoingPromise) {
            this.ongoingPromise.always((res) => {
                this.Iteration();
            });
        }
    }

    /** Enqueues a batch of operations. This can only be called in Idle state. */
    public EnqueueOperations(operations: Array<IOperation>) {
        // Reset the exit states.
        this.lastExitMessage = "";
        this.lastIsError = false;
        if (this.state != State.Idle)
            throw "Invalid operation: You can enqueue only in Idle state.";
        // Store the queue.
        this.queue = operations.slice();
        this.LogVerbose("Enqueued " + this.queue.length + " operations");
        // Request an iteration.
        this.EnqueueIteration();
    }

    /** Requests to stop the current execution. */
    public Stop() {
        if (this.state == State.Idle)
            throw "Invalid operation: Can not execute abort as the machine is already in Idle state.";
        this.LogVerbose("Stop operation request received");
        if (this.ongoingPromise != null)
            this.ongoingOperation.Abort();
    }

    /** Retrieves a status message for the current operation. */
    public GetStatus() {
        switch (this.state) {
            case State.Idle:
                return this.lastExitMessage;
            case State.Ready:
                return "";
            case State.InProgress:
                var currentOpName = this.ongoingOperation.GetName();
                return currentOpName + " is under way...";
            default:
                throw "Unknown state " + this.state;
        }
    }

    public GetIsError() {
        return this.lastIsError;
    }

    public SetChangeStateCallback(callback: (State: any) => void) {
        this.changeStateCallback = callback;
    }
}
