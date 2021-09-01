// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as SSAOperation from '../Operations/StateSpaceAnalysis';
import ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as MeInterfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as $ from 'jquery';
import * as Rx from 'rx';

/**
 * Adapts CRNEngine to SSAOperation.ISSAEngine
 */
export class Adapter implements SSAOperation.ISSAEngine {
    constructor(private me: ME) { }

    public Analyse(model: MeInterfaces.IG, jit: boolean): JQueryPromise<MeInterfaces.StateSpace> {
        var result = jQuery.Deferred<MeInterfaces.StateSpace>();
        var analysis = this.me.UserStateSpace(model, jit);
        analysis.subscribe(update => {
            if (result.state() == "pending") {
                result.resolve(update);
            }
            else {
                console.warn("result promise is already \"" + result.state() + "\". Has ME SSA behavior changed?");
            }
        }, error => {
            console.error(JSON.stringify(error));
            result.reject(JSON.stringify(error));
        },
            () => { });

        return result;
    }

    public Abort(): void {
        this.me.Abort();
    }
}
