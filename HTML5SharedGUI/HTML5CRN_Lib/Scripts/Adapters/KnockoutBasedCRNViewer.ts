import * as ParseOperation from '../Operations/ParseCodeFillCRN';
import * as SimulateOperation from '../Operations/SimulateParsedCRN';
import * as InferOperation from '../Operations/Inference';
import * as serialization from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as CRNSettings from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnSettings';
import * as CRNKO from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnKO';
import * as CRNvm from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';
import * as CrnModificationMon from '../../../../HTML5SharedGUI/CRNComponent/Scripts/modificationMonitor';
import * as ModificationIndicator from '../../../GenericComponents/Scripts/ModificationIndicators';
import * as ko from 'knockout';
import * as Files from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnDataSets';
import * as Utils from "../Utils";

export class CustomSettingsConverter<TCustomSettings>{
    convert: (crn: CRNvm.CRN, settings: TCustomSettings) => CRNvm.ExternalSetting[];
    convertBack: (crn: CRNvm.CRN) => TCustomSettings;
}

/** Adapts MVVM designed subsystem to different operations */
export class CRNEditor<TCustomSettings> implements ParseOperation.IModelViewer<TCustomSettings>, SimulateOperation.IModelSource, InferOperation.IModelSource {
    private modelVM: CRNvm.InferenceGraph;
    public GetVM(): CRNvm.InferenceGraph { return this.modelVM; }
    private mon = new CrnModificationMon.JsonSnapshotMonitor();
    private isModified = false;

    constructor(files: Files.KnockoutBasedDataSetsList, private modificationIndicator: ModificationIndicator.IModificationIndicator, private customSettingsConverter: CustomSettingsConverter<TCustomSettings> = null) {
        this.modelVM = new CRNvm.InferenceGraph(files);
        this.mon.Modified.subscribeOnNext(() => {
            this.isModified = true;
            modificationIndicator.SetModified();
        });
    }

    public Bind(elem: HTMLElement) {
        var directivesArea = <HTMLDivElement>($("crnsettings", $(elem))[0]);
        if (directivesArea) CRNKO.bind(directivesArea, this.modelVM);
        var speciesArea = <HTMLDivElement>($("species-viewer", $(elem))[0]);
        if (speciesArea) CRNKO.bind(speciesArea, this.modelVM);
        var reactionsArea = <HTMLDivElement>($("reactions-viewer", $(elem))[0]);
        if (reactionsArea) CRNKO.bind(reactionsArea, this.modelVM);
        var parametersArea = <HTMLDivElement>($("parameters-viewer", $(elem))[0]);
        if (parametersArea) CRNKO.bind(parametersArea, this.modelVM);
        this.mon.StartMonitoring(this.modelVM);
    }

    public GetConcentrationUnits() {
        return this.modelVM.SelectedCRN().settings.Units.Concentration();
    }
    public GetPlotSettings() {
        let settings = this.modelVM.SelectedCRN().settings;
        return {
            NodeID: this.modelVM.SelectedNode().NodeID(),
            XLabel: settings.PlotSettings.XLabel(),
            YLabel: settings.PlotSettings.YLabel(),
            Title: settings.PlotSettings.Title(),
            LabelFontSize: settings.PlotSettings.LabelFontSize(),
            TickFontSize: settings.PlotSettings.TickFontSize(),
            XTicks: settings.PlotSettings.XTicks(),
            YTicks: settings.PlotSettings.YTicks(),
            BurnInThin: settings.Inference.Thin(),
            MaxTime: settings.Sim.Final(),
            XMin: settings.PlotSettings.XMin(),
            XMax: settings.PlotSettings.XMax(),
            YMin: settings.PlotSettings.YMin(),
            YMax: settings.PlotSettings.YMax(),
            VBoundaries: settings.PlotSettings.VBoundaries().map(Utils.parseFloatExp),
            HBoundaries: settings.PlotSettings.HBoundaries().map(Utils.parseFloatExp)
        }
    }

    //ParseOperation.ICRNViewer implementation. For updates coming from JIT, only update the graph.
    public UpdateValuesWith(update: serialization.IG, customSettings: TCustomSettings, fromJIT: boolean): void {
        if (!fromJIT) {
            this.modelVM.fromSerializableForm(update);
            this.modelVM.setDefaultOptions();
            if (this.customSettingsConverter != null && customSettings != null) {
                for (let model of this.modelVM.Nodes()) {
                    for (let crn of model.AllCRNs()) {
                        var externalSettings = this.customSettingsConverter.convert(crn, customSettings);
                        crn.externalSettings.removeAll();
                        for (let externalSetting of externalSettings)
                            crn.externalSettings.push(externalSetting);
                    }
                }
            }
            this.modificationIndicator.Clear();
        }
    }

    /** Returns true if the data in the viewer has been modified since the last UpdateValuesWith. */
    public GetIsModified() {
        return this.isModified;
    }

    // Implementations of operation interfaces.
    public getModel(): serialization.IG {
        this.isModified = false;
        this.modificationIndicator.Clear();
        return this.modelVM.getSerializableForm();
    }
    public getNodeID(): string { return this.modelVM.SelectedNode().NodeID(); }
    public getCRNID(): string { return this.modelVM.SelectedCRN().name(); }

    public getSettings(): TCustomSettings {
        var crn = this.modelVM.SelectedCRN();
        return this.customSettingsConverter.convertBack(crn);
    }
}