import * as StateSpaceViewer from "../../../../HTML5SharedGUI/CRNComponent/Scripts/crnStateSpaceViewer";
import * as SSA from "../Operations/StateSpaceAnalysis";
import * as CRN from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as CRNvm from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';
import * as CRNKO from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnKO';

class SSAGraphAdapter implements SSA.ISSAViewer {
    constructor(ig: CRNvm.InferenceGraph) {
        var model = null;
        for (let n in ig.Nodes()) {
            model = ig.Nodes()[n];
            break;
        }
        this.vm = new StateSpaceViewer.StateSpaceGraphVM(model.Top());
    }

    private vm: StateSpaceViewer.StateSpaceGraphVM;

    public Bind(elem: HTMLElement) {
        CRNKO.bindStateSpace(elem, this.vm);
    }

    Show(stateSpace: CRN.StateSpace): void {
        this.vm.stateSpace(stateSpace);
    }
}

export default SSAGraphAdapter