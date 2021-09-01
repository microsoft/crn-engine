// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

var strandgraphs_worker_url = "/Scripts/src/WorkerBoot.js";

require.config({
    baseUrl: "/",
    paths: {
        "jquery": "https://code.jquery.com/jquery-2.1.4.min",
        //require plugins
        text: "https://cdnjs.cloudflare.com/ajax/libs/require-text/2.0.12/text.min",
        "rx": "https://cdnjs.cloudflare.com/ajax/libs/rxjs/3.1.2/rx.lite.min",
    },
    shim: {
        "jquery": {
            exports: '$'
        },
    }
});