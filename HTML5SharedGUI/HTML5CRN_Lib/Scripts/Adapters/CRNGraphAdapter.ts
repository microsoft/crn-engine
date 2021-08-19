import * as ParseOperation from '../Operations/ParseCodeFillCRN';
import * as serialization from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as ReactionGraphViewer from "../../../../HTML5SharedGUI/CRNComponent/Scripts/crnGraphViewer";
import * as CRN from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as CRNvm from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';
import * as CRNKO from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnKO';

class CRNGraphAdapter<TCustomSettings> implements ParseOperation.IModelViewer<TCustomSettings> {
    constructor(ig: CRNvm.InferenceGraph) {
        this.vm = new ReactionGraphViewer.CRNGraphVM(ig);
    }

    public vm: ReactionGraphViewer.CRNGraphVM;

    public Bind(elem: HTMLElement) {
        CRNKO.bindGraph(elem, this.vm);
    }

    UpdateValuesWith(update: serialization.IG, customSettings: TCustomSettings, fromJIT: boolean): void {
        //if (fromJIT)
          //  return;
        this.vm.model.fromSerializableForm(update);
    }
}

export default CRNGraphAdapter