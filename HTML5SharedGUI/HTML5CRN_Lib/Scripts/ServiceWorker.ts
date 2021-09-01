// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from 'knockout';

class ServiceWorkerVM {
    offlineStatus = ko.observable(false);
    hint = ko.observable("Application can be used without internet connection");
}

class ServiceWorker {
    private vm: ServiceWorkerVM = new ServiceWorkerVM();

    constructor() {
        if ('serviceWorker' in navigator) {
            window.addEventListener('load', () => {
                navigator.serviceWorker.register('./service-worker.js').then(registration => {
                    console.log('SW registered: ', registration);
                    this.vm.offlineStatus(true);
                }).catch(registrationError => {
                    console.log('SW registration failed: ', registrationError);
                });
            });
        }
    }

    Bind(div: HTMLElement) {
        if (this.vm) {
            ko.cleanNode(div);
            ko.applyBindings(this.vm, div);
        }
    }
}

export default ServiceWorker;
