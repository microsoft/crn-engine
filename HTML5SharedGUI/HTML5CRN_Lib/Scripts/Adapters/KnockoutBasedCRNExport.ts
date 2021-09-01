// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ParseOperation from '../Operations/ParseCodeFillCRN';
import * as I from '../../../GenericComponents/Scripts/Interfaces';
import * as CRN from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import CRNEngine from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine";
import * as CRNExportViewer from "../../../../HTML5SharedGUI/CRNComponent/Scripts/crnExport";
import * as MEExporter from "../Operations/ModellingEngineExporter";
import CRNSelector from "../../../CRNComponent/Scripts/CRNSelector";

//Adapts ko based CRN to be crnSerializationInterfaces.ICRNGUI viewer  source
export class CRNExport implements MEExporter.IExportsViewer, I.IUIAutoBindable {
    private exportsVM: CRNExportViewer.CRNExportViewer;

    constructor(private crnEngine: CRNEngine) {
        this.exportsVM = new CRNExportViewer.CRNExportViewer((id, instance) => this.getExportContent(id, instance));
    }

    private model: CRN.IG;
    private nodeId: string;

    private getExportContent(exportId: string, instance: string): JQueryPromise<{ content: string[], save_content?: string }> {
        var ret = $.Deferred<{ content: string[], save_content: string }>();
        var ob = this.crnEngine.UserGenerateExport(this.model, this.nodeId, exportId, instance);
        ob.exports.subscribe(def => {
            ret.resolve({ content: def.content, save_content: def.save_content });
        }, err => { ret.resolve({ content: [JSON.stringify(err)], save_content: null }); }, () => { });
        return ret;
    }

    //Features.IUIAutoBindable implementation
    public AutoBind() {
        var crnExport = <HTMLDivElement>document.getElementById('crn-export');
        this.exportsVM.Bind(crnExport);
    }

    public Bind(elem: HTMLElement) {
        this.exportsVM.Bind(elem);
    }
    public BindToTabs(tabsContainer: HTMLElement) {
        this.exportsVM.BindToTabs(tabsContainer);
    }

    public SetModel(model: CRN.IG): void {
        this.model = model;
    }
    public SetNodeID(nodeId: string): void {
        if (this.nodeId != nodeId) {
            this.nodeId = nodeId;
            this.exportsVM.onNodeChanged();
        }
    }

    //ParseOperation.IExportsViewer implementation
    public ShowExport(update: CRN.ExportDef): void {
        this.exportsVM.showExport(update);
    }

    public Reset(): void {
        this.exportsVM.clear();
    }
}
