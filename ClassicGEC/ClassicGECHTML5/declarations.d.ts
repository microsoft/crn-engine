// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

declare module "*.html"
declare module "idd" {
    var InteractiveDataDisplay: any;
    export = InteractiveDataDisplay;
}
declare module "tinycolor" {
    var tinycolor: any;
    export = tinycolor;
}
declare module "worker-loader*" {
    class WebpackWorker extends Worker {
        constructor();
    }
    export default WebpackWorker;
}
declare module "*index.js"
declare module "*main.js"
declare module "sboljs"