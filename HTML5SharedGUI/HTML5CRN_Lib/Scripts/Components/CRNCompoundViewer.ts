// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Interfaces from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as Operation from '../Operations/ParseCodeFillCRN';
import * as serialization from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";

class Viewer<TCustomSettings> implements Operation.IModelViewer<TCustomSettings> {
    private viewers: Operation.IModelViewer<TCustomSettings>[];
    public constructor(...viewers: Operation.IModelViewer<TCustomSettings>[]) {
        this.viewers = viewers;
    }

    UpdateValuesWith(update: serialization.IG, customSettings: TCustomSettings, fromJIT: boolean): void {
        for (let viewer of this.viewers)
            viewer.UpdateValuesWith(update, customSettings, fromJIT);
    }
}

export default Viewer
