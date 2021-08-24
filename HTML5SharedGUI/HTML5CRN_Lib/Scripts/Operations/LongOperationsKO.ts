import * as $ from 'jquery';
import * as ko from 'knockout';
import * as LongOperations from './LongOperations';

//exposes KO observables of Long Operations State machine
export class LongOperationsKO {
    public CanStartNewAction = ko.observable<boolean>(true);
    public CanStopOngoingAction = ko.observable<boolean>(false);
    public ExecutionStatus = ko.observable<string>("");
    public IsError = ko.observable<boolean>(false);
    private stateMachineInitialised = ko.observable<boolean>(false);

    private stateMachine: LongOperations.StateMachine;
    private stateChangeCallback = (newState: LongOperations.State) => {
        var status = this.stateMachine.GetStatus();
        // Normalise to ascii cr/lf.
        status = status.replace(new RegExp("[\\\\]n", "g"), "\n").replace(new RegExp("[\\\\]r", "g"), "\r");
        // Replace with breaks.
        status = status.replace(new RegExp("[\r\n|\n\r|\n|\r]", "g"), "<br>");
        this.ExecutionStatus(status);
        var isError = this.stateMachine.GetIsError();
        this.IsError(isError);
        switch (newState) {
            case LongOperations.State.Idle:
                this.CanStartNewAction(true);
                this.CanStopOngoingAction(false);
                break;
            case LongOperations.State.InProgress:
            case LongOperations.State.Ready:
                this.CanStartNewAction(false);
                this.CanStopOngoingAction(true);
                break;
            default:
                throw "Not implemented yet";
        }
    };

    constructor() {
        this.stateMachine = new LongOperations.StateMachine(false);
        this.stateMachineInitialised(true);
        this.stateMachine.SetChangeStateCallback(this.stateChangeCallback);
    }

    public EnqueueOperations(operations: Array<LongOperations.IOperation>) {
        this.stateMachine.EnqueueOperations(operations);
    }

    public Stop() {
        this.stateMachine.Stop();
    }

    public Bind(elem: HTMLElement) {
        ko.applyBindings(this, elem);
    }
}
