// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from 'knockout';
import * as CRN from './../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as CRNExport from "./../../CRNComponent/Scripts/crnExport";
import "../samples.css";

var data: CRN.ExportDef[] = [
    { content_type: "text/plain", id: "matlab", display_name: "Matlab", content: null },
    { content_type: "text/plain", id: "sbml", display_name: "SBML", content: null },
    { content_type: "image/svg+xml", id: "initials", display_name: "Initials", content: ["<svg width='100' height= '100'><circle cx='50' cy= '50' r='40' stroke='green' stroke-width='4' fill= 'yellow' /></svg>"] },
    { content_type: "image/svg+xml", id: "reactions", display_name: "Reactions", content: ["A->B", "B->C", "C->D", "D->E", "E->F", "F->G", "G->H"] },
    { content_type: "text/plain", id: "code", display_name: "Code", content: ["(code here)"] },
    { content_type: "application/x-latex", id: "latex", display_name: "LaTeX", content: null }
];

function getExportContent(id: string): JQueryPromise<{ content: string[], save_content?: string }> {
    var ret = $.Deferred<{ content: string[], save_content?: string }>();
    switch (id) {
        case "matlab":
            ret.resolve({ content: ["Here is Matlab code"] });
            break;
        case "sbml":
            ret.resolve({ content: ["Here is SBML code"] });
            break;
        case "reactions":
            ret.resolve({ content: ["A->B", "B->C", "C->D", "D->E", "E->F", "F->G", "G->H"] });
            break;
        case "code":
            ret.resolve({ content: ["CRN Code"] });
            break;
        case "latex":
            ret.resolve({ content: ["c = \\pm\\sqrt{a^2 + b^2}"] });
            break;
        default:
            throw "cannot generate export " + id;
    }
    return ret.promise();
}

var crnExport = new CRNExport.CRNExportViewer(getExportContent);
crnExport.Bind(document.getElementById("crnexport"));
for (let exportDef of data)
    crnExport.showExport(exportDef);
