// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as $ from 'jquery';
import * as Operations from "./LongOperations";

//Tab selection as long operation.
//Selects jQuery tab by id of div containing it 
export class Operation implements Operations.IOperation {

    private initiatedDeferred: JQueryDeferred<any> = undefined; //operation promise

    constructor(private tabWidget: JQuery, private tabSelector: string) {
    }

    GetName(): string {
        return "Tab selection";
    };

    GetExitMessage() {
        return "";
    }

    Initiate(): JQueryPromise<any> {
        this.initiatedDeferred = jQuery.Deferred<any>();

        var index = this.tabWidget.find(this.tabSelector).index() - 1;

        // Note: if at this point jQuery throws an exception complaining that the widget has not been initialized, it means that tabWidget has not received a tabs() call. You could call tabs on it now, and that might seem to work (because jQuery is capable of going through children to find tabs). However, this would mask the fundamental issue of the widget not having been initialized at the correct point, which may result in the tool missing initialization steps. It's best to perform setup in the correct places and order.
        this.tabWidget.tabs("option", "active", index);

        this.initiatedDeferred.resolve(true);

        return this.initiatedDeferred;
    };

    Abort(): void {
    };
}
