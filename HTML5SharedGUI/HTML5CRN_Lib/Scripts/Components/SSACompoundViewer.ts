// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Interfaces from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as Operation from '../Operations/StateSpaceAnalysis';

export class Viewer implements Operation.ISSAViewer {
    private viewers: Operation.ISSAViewer[];
    public constructor(...viewers: Operation.ISSAViewer[]) {
        this.viewers = viewers;
    }

    //Operation.ISSAViewer implementation
    Show(stateSpace: Interfaces.StateSpace) {
        for (let viewer of this.viewers) {
            viewer.Show(stateSpace);
        }
    }
}
