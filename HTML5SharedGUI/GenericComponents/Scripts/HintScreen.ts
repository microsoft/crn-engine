// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as I from "./Interfaces";
import * as ko from 'knockout';

class HintScreenVM {
    public IsHintVisible = ko.observable(true);
}

export interface IHintRemoveNotifier {
    SubscribeRemoveHint(callback: () => void): void; //indicates, that hint must be removed
}

export class CombinedHintRemoveNotifier implements IHintRemoveNotifier {
    constructor(notifiers: IHintRemoveNotifier[]) {
        notifiers.forEach(n => n.SubscribeRemoveHint(() =>
            this.notificationCallbacks.forEach(c => { c(); }))
        );
    }

    private notificationCallbacks: Array<() => void> = [];
    public SubscribeRemoveHint(callback: () => void) {
        this.notificationCallbacks.push(callback);
    }
}

//The class rely on knockout binding of "IsHintVisible" boolean property to passed DOM element descendents
export class HintScreen implements I.IUIBindable {
    private vm = new HintScreenVM();
    constructor(notifier: IHintRemoveNotifier) {
        notifier.SubscribeRemoveHint(() => { this.vm.IsHintVisible(false) });
    }

    //Features.IUIBindable implementation
    public Bind(elem: HTMLElement) {
        ko.applyBindings(this.vm, elem);
    }
}
