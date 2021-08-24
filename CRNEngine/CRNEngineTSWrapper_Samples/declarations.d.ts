// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

declare module "raw-loader*"
declare module "worker-loader*" {
    class WebpackWorker extends Worker {
        constructor();
    }
    export default WebpackWorker;
}
