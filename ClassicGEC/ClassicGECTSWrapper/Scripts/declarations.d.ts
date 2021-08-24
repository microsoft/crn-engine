// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

declare module "worker-loader*" {
    class WebpackWorker extends Worker {
        constructor();
    }

    export default WebpackWorker;
}

declare module "*" // Effectively disables checking that an imported file is a module. This is brutal, but saves a ton of headaches. See https://github.com/Microsoft/TypeScript/issues/15031. If at any point TS provides a better way to allow using a js file as an untyped module, we should switch to that.