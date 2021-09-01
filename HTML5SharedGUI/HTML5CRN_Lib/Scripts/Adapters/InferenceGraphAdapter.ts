// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ParseOperation from '../Operations/ParseCodeFillCRN';
import * as serialization from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as InferenceGraphViewer from "../../../../HTML5SharedGUI/CRNComponent/Scripts/inferenceGraphViewer";
import * as CRN from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as CRNvm from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';
import * as CRNKO from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnKO';

class InferenceGraphAdapter<TCustomSettings> implements ParseOperation.IModelViewer<TCustomSettings> {
    constructor(ig: CRNvm.InferenceGraph) {
        this.vm = new InferenceGraphViewer.InferenceGraphViewerVM(ig);
    }

    public vm: InferenceGraphViewer.InferenceGraphViewerVM;

    public Bind(elem: HTMLElement) {
        CRNKO.bindInferenceGraph(elem, this.vm);
    }

    UpdateValuesWith(update: serialization.IG, customSettings: TCustomSettings, fromJIT: boolean): void {
        //if (fromJIT)
        //  return;
        this.vm.ig.fromSerializableForm(update);
    }
}

export default InferenceGraphAdapter
