// (FP) This file contains the code for the export viewer component, which is just a tabbed view of several text areas.

import * as $ from "jquery";
import * as ko from "knockout";
import * as CRN from "./../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as katex from "katex";
import { saveAs } from "file-saver";
import * as template from 'raw-loader!../html/crn-export.html';

var parser = new DOMParser();

ko.components.register('crn-export', {
    viewModel: {
        createViewModel: (params, componentInfo) => {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context == null ? {} : context.$data;
        }
    },
    template: template
});

class CRNExportViewModel {
    constructor(public ID: string, public Generator: (instance: string) => JQueryPromise<{ content: string[]; save_content?: string }>) {
        this.ExportDefs = ko.observable<CRN.ExportDef[]>([]);
        this.DisplayName = ko.pureComputed(() => {
            return this.ExportDefs()[0].display_name;
        });
        this.Content = ko.pureComputed(() => {
            return this.GetFullContent();
        });
        this.JoinedContent = ko.pureComputed(() => {
            var c = this.Content();
            if (c == null)
                return null;
            return c.join("");
        });
        this.PageCount = ko.pureComputed(() => {
            var c = this.Content();
            if (c == null || c.length == 0)
                return 1;
            return Math.floor((c.length - 1) / this.PageSize()) + 1;
        });
        this.CurrentPage = ko.pureComputed(() => {
            var c = this.Content();
            if (c == null || c.length == 0)
                return [];
            var pn = this.CurrentPageNumber();
            var ps = this.PageSize();
            if (c.length < pn * ps)
                return [];
            if (c.length >= (pn + 1) * ps)
                return c.slice(pn * ps, (pn + 1) * ps - 1);
            else
                return c.slice(pn * ps);
        });
        this.Pages = ko.pureComputed(() => {
            var ret = [];
            for (var i = 0; i < this.PageCount(); i++)
                ret.push(i + 1);
            return ret;
        });

        this.ContentType = ko.pureComputed(() => {
            var defs = this.ExportDefs();
            return defs[0].content_type;
        });
        this.CanSave = ko.pureComputed(() => {
            var defs = this.ExportDefs();
            for (let def of defs)
                if (this.GetSaveContent(def) != null)
                    return true;
            return false;
        });

        // Retrieve the content when I get focus.
        this.IsActive.subscribe((v: boolean) => {
            if (v)
                this.RetrieveContent();
        });
        // Retrieve the content when there is an update, if I have focus.
        this.ExportDefs.subscribe((v: CRN.ExportDef[]) => {
            if (this.IsActive())
                this.RetrieveContent();
        });
        // Reset to first page any time the page number changes.
        this.Pages.subscribe(p => this.CurrentPageNumber(0));
    }

    public ExportDefs: KnockoutObservable<CRN.ExportDef[]>;

    public RetrieveContent() {
        var defs = this.ExportDefs();
        for (var idx = 0; idx < defs.length; idx++) {
            let curr = defs[idx];
            if (curr.content == null) {
                this.Generator(curr.instance).then(res => {
                    curr.content = res.content;
                    curr.save_content = res.save_content;
                    this.ExportDefs(defs);
                });
            }
        }
    }

    public DisplayName: KnockoutComputed<string>

    public IsActive = ko.observable<boolean>(false);

    public SetDef(def: CRN.ExportDef) {
        var defs = this.ExportDefs();
        for (var idx = 0; idx < defs.length; idx++) {
            let curr = defs[idx];
            if (curr.instance == def.instance) {
                defs[idx] = def;
                this.ExportDefs(defs);
                return;
            }
        }
        defs.push(def);
        this.ExportDefs(defs);
        return;
    }

    private GetContent(def: CRN.ExportDef): string[] {
        if (def.content_type == "application/x-tex" && def.content != null) {
            try {
                var hack = def.content.map(c => c.replace(/align\*/g, "aligned"));
                var hacks = hack.join("");
                return [katex.renderToString(hacks)];
            }
            catch (e) {
                return e.toString();
            }
        }
        return def.content;
    }

    public Content: KnockoutComputed<string[]>;
    public ContentType: KnockoutComputed<string>;
    public JoinedContent: KnockoutComputed<string>;

    public PageSize: KnockoutObservable<number> = ko.observable(25);
    public CurrentPageNumber: KnockoutObservable<number> = ko.observable(0);
    public PageCount: KnockoutComputed<number>;
    public CurrentPage: KnockoutComputed<string[]>;
    public Pages: KnockoutComputed<number[]>;

    public SetPage(p: number) { this.CurrentPageNumber(p - 1); }

    private GetSaveContent(def: CRN.ExportDef): string {
        if (def.save_content == null)
            return def.content == null ? null : def.content.join("");
        if (def.save_content == "")
            return null;
        return def.save_content;
    }

    private StackSVG(blocks: string[]): string {
        if (blocks == null || blocks.length == 0)
            return null;
        var hblocks: { content: string, height: number, width: number }[] = [];
        var totalwidth = 0.0, totalheight = 0.0;
        for (let block of blocks) {
            var width = 0, height = 0;
            if (block != null && block != "") {
                var doc = parser.parseFromString(block, "image/svg+xml");
                var root = doc.getElementsByTagName("svg")[0];
                width = parseFloat(root.attributes.getNamedItem("width").value);
                height = parseFloat(root.attributes.getNamedItem("height").value);
            }
            hblocks.push({ content: block, height: height, width: width });
            if (totalwidth < width)
                totalwidth = width;
            totalheight += height;
        }
        var ret = "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"" + totalheight + "\" width=\"" + totalwidth + "\">\r\n";
        for (let hblock of hblocks) {
            if (hblock.content == null || hblock.content == "")
                continue;
            ret += "<g transform=\"translate(0," + totalheight + ")\" height=\"" + hblock.height + "\" width=\"" + hblock.width + "\">\r\n";
            if (hblock.content != null && hblock.content != "")
                ret += hblock.content + "\r\n";
            ret += "</g>";
            totalheight += hblock.height;
        }
        ret += "</svg>";
        return ret;
    }

    private CombineSVG(chunks: { header: string, content: string[] }[]): string {
        var hchunks: { header: string, content: string, height: number, width: number }[] = [];
        var totalwidth = 0.0, totalheight = 0.0, headerheight = 36.0;
        for (let chunk of chunks) {
            var width = 0, height = 0;
            var content = this.StackSVG(chunk.content)
            if (content != null && content != "") {
                var doc = parser.parseFromString(content, "image/svg+xml");
                var root = doc.getElementsByTagName("svg")[0];
                width = parseFloat(root.attributes.getNamedItem("width").value);
                height = parseFloat(root.attributes.getNamedItem("height").value);
            }
            hchunks.push({ header: chunk.header, content: content, height: height, width: width });
            if (totalwidth < width)
                totalwidth = width;
            totalheight += (headerheight + height);
        }
        var ret = "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"" + totalheight + "\" width=\"" + totalwidth + "\">\r\n";
        ret += "<style>.export_instance_header { margin-top: 50px; font-size: x-large; }</style>";
        totalheight = headerheight;
        for (let chunk of hchunks) {
            if ((chunk.header == null || chunk.header == "") && (chunk.content == null || chunk.content == ""))
                continue;
            ret += "<g transform=\"translate(0," + totalheight + ")\" height=\"" + (headerheight + chunk.height) + "\" width=\"" + chunk.width + "\">\r\n";
            if (chunk.header != null && chunk.header != "")
                ret += "<text class=\"export_instance_header\">" + chunk.header + "</text>\r\n";
            if (chunk.content != null && chunk.content != "")
                ret += chunk.content + "\r\n";
            ret += "</g>";
            totalheight += (headerheight + chunk.height);
        }
        ret += "</svg>";
        return ret;
    }

    private GetFullContent(): string[] {
        var defs = this.ExportDefs();
        if (defs == null || defs.length == 0)
            return null;
        var baseDef = defs[0];
        if (defs.length == 1)
            return this.GetContent(baseDef);
        else if (baseDef.content_type == "image/svg+xml")
            return [this.CombineSVG(defs.map(def => { return { header: def.instance, content: this.GetContent(def) }; }))];
        else
            return ["multi-instance export not yet implemented for " + baseDef.content_type];
    }

    private GetFullSaveContent(): string {
        var baseDef = this.ExportDefs()[0];
        var defs = this.ExportDefs();
        if (defs.length == 1)
            return this.GetSaveContent(defs[0]);
        else if (baseDef.content_type == "image/svg+xml")
            return this.CombineSVG(defs.map(def => { return { header: def.instance, content: [this.GetSaveContent(def)] }; }));
        else
            return "multi-instance save not yet implemented for " + baseDef.content_type;
    }

    public CanSave: KnockoutComputed<boolean>;

    public saveAs() {
        var baseDef = this.ExportDefs()[0];
        var extension = "txt";
        switch (baseDef.content_type) {
            case "image/svg+xml":
                extension = "svg";
                break;
            case "application/x-tex":
                extension = "tex";
                break;
            case "text/html":
                extension = "html";
                break;
            default: break;
        }
        var fileName = baseDef.id + "." + extension;
        var defs = this.ExportDefs();
        var content = this.GetFullSaveContent();
        var blob = new Blob([content], { type: baseDef.content_type });
        saveAs(blob, fileName);
    }
}

class CRNExportsViewModel {
    constructor(private Generator: (id: string, instance: string) => JQueryPromise<{ content: string[]; save_content?: string }>) { }
    public Definitions = ko.observableArray<CRNExportViewModel>();

    public byID(id: string): CRNExportViewModel {
        for (var def of this.Definitions())
            if (def.ID == id)
                return def;
        return null;
    }

    public showExport(exportDef: CRN.ExportDef, initiallyActive: boolean) {
        if (exportDef == null)
            return;
        var definitions = this.Definitions();
        for (let def of definitions)
            if (def.ID == exportDef.id) {
                def.SetDef(exportDef);
                return;
            }
        var self = this;
        function generator(instance: string) {
            return self.Generator(exportDef.id, instance);
        }
        var def = new CRNExportViewModel(exportDef.id, generator);
        def.IsActive(initiallyActive);
        def.SetDef(exportDef);
        this.Definitions.push(def);
    }

    public reload(id: string) {
        var definitions = this.Definitions();
        var definition: CRNExportViewModel = null;
        for (let def of definitions) {
            if (def.ID == id) {
                definition = def;
                break;
            }
        }
        var self = this;
        function generator(instance: string) {
            return self.Generator(definition.ID, instance);
        }
        definition.Generator = generator;
        definition.RetrieveContent();
    }

    public onNodeChanged() {
        var definitions = this.Definitions();
        for (let def of definitions) {
            var all_nodes = false;
            for (let d of def.ExportDefs())
                if (d.node_id == null)
                    all_nodes = true;
            if (!all_nodes) {
                for (let d of def.ExportDefs()) {
                    d.content = null;
                    if (!all_nodes && def.IsActive())
                        this.reload(def.ID);
                }
            }
        }
    }

    public clear() {
        this.Definitions.removeAll();
    }
}

// This class represents the component. It's basically a wrapper around the VM, plus component registration and binding. Note that a common case is presenting all available exports as tabs. This could in theory be achieved with appropriate KO foreach bindings; however, this implies setting the exports VM as the VM for the entire tab container. This is not always feasible, e.g. in the case where the tab container is supposed to also hold other tabs in addition to exports. In this scenario, use the BindToTabs function of this class.
export class CRNExportViewer {
    private exportVM: CRNExportsViewModel;

    constructor(private Generator: (id: string, instance: string) => JQueryPromise<{ content: string[]; save_content?: string }>) {
        this.exportVM = new CRNExportsViewModel(Generator);
    }

    public Bind(div: HTMLElement) {
        // Apply the bindings by provieding the viewmodel directly.
        ko.applyBindings(this.exportVM, div);
    }

    // I'll maintain the ID of the currently active panel in the tabs system. This will allow me to properly mark a newly created VM as active, in the case where the user selects a tab before running the corresponding export.
    private activePanel: string;

    // This function should be invoked on an element that is set as a tab container, or that directly contains one. It will add/remove tabs to match available exports, without removing bindings on other tabs.
    public BindToTabs(tabsContainer: HTMLElement) {
        var self = this;
        // Find the tabs container.
        if (!$(tabsContainer).hasClass("j-has-tabs"))
            tabsContainer = $(".j-has-tabs", tabsContainer)[0];
        // When a tab is activated, I need to mark the corresponding VM as active.
        $(tabsContainer).on("tabsactivate", function (event, ui) {
            // Find the panel that has just been abandoned, and the panel that has just been reached. Note that I'm only interested in panels that contain a crn-export element. If they don't, I don't care. If they do, I expect them to have a CRNExportViewModel associated with them.
            var oldPanel = ui.oldPanel.children("crn-export");
            if (oldPanel.length > 0) {
                var oldvm: CRNExportViewModel = ko.dataFor(oldPanel[0]);
                if (oldvm == null)
                    console.log("Warning: no vm for an export tab.");
                else if (oldvm.IsActive != null)
                    oldvm.IsActive(false);
            }
            self.activePanel = ui.newPanel[0].id;
            var newPanel = ui.newPanel.children("crn-export");
            if (newPanel.length > 0) {
                var newvm: CRNExportViewModel = ko.dataFor(newPanel[0]);
                if (newvm == null)
                    console.log("Warning: no vm for an export tab.");
                else if (newvm.IsActive != null)
                    newvm.IsActive(true);
            }
        });
        var headers = $(tabsContainer).children("ul");
        // The host may have already setup some tabs for some exports. So here I get the list of export tabs that are already present. They won't be added or removed; they'll always be present, even if empty.
        var sticky = headers.children("li").children("a").toArray().map(e => e.getAttribute("href"));
        this.exportVM.Definitions.subscribe(changes => {
            for (let change of changes) {
                let id = change.value.ID;
                var divID = "export-" + id;
                var anchorID = "export-anchor-" + id;
                var headerID = "export-header-" + id;
                if (sticky.some(s => s == "#" + divID))
                    continue;
                if (change.status == "added") {
                    var div = $("<div></div>");
                    div.attr("id", divID);
                    div.addClass("c-export");
                    div.attr("data-bind", "with: byID('" + id + "')");
                    var ex = $("<crn-export></crn-export>");
                    div.append(ex);
                    $(tabsContainer).append(div);
                    var a = $("<a></a>");
                    a.text(change.value.DisplayName());
                    a.attr("id", anchorID);
                    a.attr("href", "#" + divID);
                    var li = $("<li></li>");
                    li.attr("id", headerID);
                    li.append(a);
                    headers.append(li);
                    ko.applyBindings(this.exportVM, div[0]);
                    $(tabsContainer).tabs("refresh");
                    (<any>$(tabsContainer)[0]).fresh = false;
                }
                else if (change.status == "deleted") {
                    headers.children("#" + headerID).remove();
                    $(tabsContainer).children("#" + divID).remove();
                    $(tabsContainer).tabs("refresh");
                    (<any>$(tabsContainer)[0]).fresh = false;
                }
            }
        }, null, "arrayChange");
    }

    public showExport(exportDef: CRN.ExportDef) {
        this.exportVM.showExport(exportDef, this.activePanel == "export-" + exportDef.id);
    }

    public onNodeChanged() {
        this.exportVM.onNodeChanged();
    }

    public clear() {
        this.exportVM.clear();
    }
}