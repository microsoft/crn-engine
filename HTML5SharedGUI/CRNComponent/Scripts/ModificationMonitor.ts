import * as CrnVM from "./crnVM";
import * as rx from 'rx';
import * as ko from 'knockout';

export interface IModificationMonitor {
    StartMonitoring(modelVM: CrnVM.InferenceGraph): void;
    Modified: rx.Observable<void>;
}

// (FP) This class monitors a CRN VM for changes. It triggers an event (in the form of an Observable<void>) whenever something changes.
export class JsonSnapshotMonitor implements IModificationMonitor {
    private subject = new rx.Subject<void>();

    public StartMonitoring(modelVM: CrnVM.InferenceGraph) {
        var prev = "";
        // Note that I'll have to rate-limit this, otherwise the subject will get multiple redundant notifications any time multiple fields within the model get updated at the same time. In particular, this happens any time a parsing operation completes. The number of changes in that case can easily freeze the GUI.
        ko.computed(() => {
            // If the model is currently being deserialized (the deserializing flag is true), then I will not consider any changes until it's done (which happens when the deserializing flag goes to false). This allows me not to fire a large number of useless notifications during deserialization.
            if (modelVM.deserializing())
                return prev;
            var start = performance.now();
            // I'll build a JS object with the relevant fields, and call ko.toJSON on it. The effect is that this will be re-evaluated any time any of the fields change. The clearinitials and clearreaction functions serve the purpose of excluding some fields that are very expensive to evaluate and should not be considered for this purpose.
            var clearinitials = function (i: CrnVM.Initial) {
                var ret = (<any>Object).assign({}, i);
                delete ret.crn;
                delete ret.plot;
                delete ret.cachedsvg;
                return ret;
            }
            var clearreaction = function (r: CrnVM.Reaction) {
                var ret = (<any>Object).assign({}, r);
                delete ret.crn;
                return ret;
            }
            var o = modelVM.Nodes().map(node => node.AllCRNs().map(crn => {
                return {
                    inference: crn.settings.Inference,
                    ode: crn.settings.ODE,
                    parameters: crn.settings.Parameters,
                    pde: crn.settings.PDE,
                    sim: crn.settings.Sim,
                    ssa: crn.settings.SSA,
                    sweeps: crn.settings.Sweeps,
                    units: crn.settings.Units,
                    chosenSimulation: crn.settings.ChosenSimulation,
                    name: crn.name,
                    initials: crn.initials().map(clearinitials),
                    reactions: crn.reactions().map(clearreaction)
                };
            }));
            var str = ko.toJSON(o);
            return str;
        }).subscribe(curr => this.subject.onNext(null));
    }

    public Modified: rx.Observable<void> = this.subject;
}