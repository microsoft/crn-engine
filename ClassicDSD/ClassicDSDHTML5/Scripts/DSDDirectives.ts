import { ExternalSetting as ExternalSetting, CRN as CRN } from '../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';
import { DsdSettings as DsdSettings } from '../../ClassicDSDTSWrapper/Scripts/Interfaces';
import * as DsdInterfaces from '../../ClassicDSDTSWrapper/Scripts/WebSharperGeneratedInterfaces';
import * as ko from "knockout";
import * as template from 'raw-loader!../Templates/dsddirectives_template.html';

class DSDDirectivesVM extends ExternalSetting {
    constructor(owner: CRN) {
        super(owner);
        this.template(template);
        this.Rules.subscribe(v => { if (this.source != null) this.source.rules = v; });
        this.Leaks.subscribe(v => { if (this.source != null) this.source.leaks = v; });
        this.PinLeaks.subscribe(v => { if (this.source != null) this.source.pin_leaks = v; });
        this.Polymers.subscribe(v => { if (this.source != null) this.source.polymers = v; });
        this.Unproductive.subscribe(v => { if (this.source != null) this.source.unproductive = v; });
        this.SequenceRates.subscribe(v => { if (this.source != null) this.source.sequence_rates = v; });
        this.Temperature.subscribe(v => { if (this.source != null) this.source.temperature = v; });
        this.StabilityCorrection.subscribe(v => { if (this.source != null) this.source.stabilityCorrection = v; });
        this.CoaxialDangle.subscribe(v => { if (this.source != null) this.source.coaxialDangle = v; });
        this.DoubleCoaxialDangle.subscribe(v => { if (this.source != null) this.source.doubleCoaxialDangle = v; });
        this.TerminalDangleFactor.subscribe(v => { if (this.source != null) this.source.terminalDangleFactor = v; });
        this.DeclareDomains.subscribe(v => { if (this.source != null) this.source.declare_domains = v; });
        this.CheckDNA.subscribe(v => { if (this.source != null) this.source.check_dna = v; });
        this.ColourToeholds.subscribe(v => { if (this.source != null) this.source.colour_toeholds = v; });
        this.PlotNames.subscribe(v => { if (this.source != null) this.source.plot_names = v; });
        this.ToeholdBindRate.subscribe(v => { if (this.source != null) this.source.toehold_bind_rate = v; });
        this.ToeholdUnbindRate.subscribe(v => { if (this.source != null) this.source.toehold_unbind_rate = v; });
        this.ToeholdLength.subscribe(v => { if (this.source != null) this.source.toehold_length = v; });
        this.SpecificityLength.subscribe(v => { if (this.source != null) this.source.specificity_length = v; });
        this.GeneratePredicates.subscribe(v => { if (this.source != null) this.source.generate_predicates = v; });
        this.IsJIT.subscribe(v => { if (this.source != null) this.source.is_jit = v; });
    }
    private _source: DsdSettings;
    public get source(): DsdSettings { return this._source; }
    public set source(source: DsdSettings) {
        this._source = source;
        this.Rules(source.rules);
        this.Leaks(source.leaks);
        this.PinLeaks(source.pin_leaks);
        this.Polymers(source.polymers);
        this.Unproductive(source.unproductive);
        this.SequenceRates(source.sequence_rates);
        this.Temperature(source.temperature);
        this.StabilityCorrection(source.stabilityCorrection);
        this.CoaxialDangle(source.coaxialDangle);
        this.DoubleCoaxialDangle(source.doubleCoaxialDangle);
        this.TerminalDangleFactor(source.terminalDangleFactor);
        this.DeclareDomains(source.declare_domains);
        this.CheckDNA(source.check_dna);
        this.ColourToeholds(source.colour_toeholds);
        this.PlotNames(source.plot_names);
        this.ToeholdBindRate(source.toehold_bind_rate);
        this.ToeholdUnbindRate(source.toehold_unbind_rate);
        this.ToeholdLength(source.toehold_length);
        this.SpecificityLength(source.specificity_length);
        this.GeneratePredicates(source.generate_predicates);
        this.IsJIT(source.is_jit);
    }
    public Rules = ko.observable<DsdInterfaces.WebSharperGeneratedInterfaces.Microsoft.Research.DNA.Options.semantics>("Infinite");
    public RulesSelect = DsdInterfaces.WebSharperGeneratedInterfaces.Microsoft.Research.DNA.Options.semanticsSelect;
    public Leaks = ko.observable<boolean>(false);
    public PinLeaks = ko.observable<boolean>(false);
    public Polymers = ko.observable<boolean>(false);
    public Unproductive = ko.observable<boolean>(false);
    public SequenceRates = ko.observable<boolean>(false);
    public Temperature = ko.observable<number>(0);
    public StabilityCorrection = ko.observable<number>(0);
    public CoaxialDangle = ko.observable<number>(0);
    public DoubleCoaxialDangle = ko.observable<number>(0);
    public TerminalDangleFactor = ko.observable<number>(0);
    public DeclareDomains = ko.observable<boolean>(false);
    public CheckDNA = ko.observable<boolean>(false);
    public ColourToeholds = ko.observable<boolean>(false);
    public PlotNames = ko.observable<boolean>(false);
    public ToeholdBindRate = ko.observable<number>(0);
    public ToeholdUnbindRate = ko.observable<number>(0);
    public ToeholdLength = ko.observable<number>(0);
    public SpecificityLength = ko.observable<number>(0);
    public GeneratePredicates = ko.observable<DsdInterfaces.WebSharperGeneratedInterfaces.Microsoft.Research.DNA.Options.generate_predicates>("All_predicates");
    public GeneratePredicatesSelect = DsdInterfaces.WebSharperGeneratedInterfaces.Microsoft.Research.DNA.Options.generate_predicatesSelect;
    public IsJIT = ko.observable<boolean>(false);
}

export default DSDDirectivesVM