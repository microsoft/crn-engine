import * as CRN from './crnVM';
import * as ko from 'knockout';

class ModelsVM {
    constructor(private igs: CRN.InferenceGraph[]) { }

    public Nodes: KnockoutComputed<CRN.Model[]> = ko.pureComputed(() => this.igs[0].Nodes());
    public SelectedNode: KnockoutComputed<CRN.Model> = ko.pureComputed({
        read: () => this.igs[0].SelectedNode(),
        write: selected => {
            let name = selected.Top().name();
            for (let ig of this.igs)
                for (let model of ig.Nodes())
                    if (model.Top().name() == name) {
                        ig.SelectedNode(model);
                        break;
                    }
        }
    });

    public CRNs: KnockoutComputed<CRN.CRN[]> = ko.pureComputed(() => this.SelectedNode().AllCRNs());
    public SelectedCRN: KnockoutObservable<CRN.CRN> = ko.pureComputed({
        read: () => this.igs[0].SelectedCRN(),
        write: selected => {
            let name = selected == null ? null : selected.name();
            for (let ig of this.igs)
                for (let model of ig.Nodes())
                    for (let crn of model.AllCRNs())
                        if (crn.name() == name) {
                            model.SelectedCRN(crn);
                            break;
                        }
        }
    });
}

class CRNSelector {
    constructor(...models: CRN.InferenceGraph[]) {
        this.VM = new ModelsVM(models);
    }

    private VM: ModelsVM;

    // Binds a <crnselector> element to a set of Models. The selector will select the same CRN in all of the bound Models. It is assumed that all Models have the same set of CRNs.
    public Bind(div: HTMLElement, ) {
        // Clear existing bindings. This is necessary in case there's another viewmodel further up within the hierarchy.
        (<any>ko.utils.domNodeDisposal).cleanExternalData = function () { }; // prevent non-ko handlers from being cleaned by cleanNode
        ko.cleanNode(div);
        ko.applyBindings(this.VM, div);
    }

    public getSelectedNode(): string {
        return this.VM.SelectedNode().Top().name();
    }

    public SelectedNode: KnockoutObservable<CRN.Model> = ko.pureComputed(() => this.VM.SelectedNode());
    public SelectedNodeID: KnockoutComputed<string> = ko.pureComputed(() => this.SelectedNode().NodeID());
}

export default CRNSelector