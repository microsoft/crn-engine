// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Data structures that represent a CRN that comes from Crn.gui.
import * as $ from 'jquery';
import * as ko from 'knockout';
import * as CRNSettings from './crnSettings';
import { WebSharperGeneratedInterfaces as WGI } from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces"; // https://github.com/Microsoft/TypeScript/issues/5711
import * as CRNSerializable from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as Utils from './Utils';
import * as Papa from "papaparse";
import * as DataSets from './crnDataSets';
import * as Files from './SortableFileNamesList';

var parser = new DOMParser();

function parseRate(str: string): CRNSerializable.Rate {
    if (str == null || str == "")
        return undefined;
    if (str.length > 2 && str[0] == '[' && str[str.length - 1] == ']')
        return { Function: str.substr(1, str.length - 2) };
    else
        return { MassAction: str };
}

function stringRate(r: CRNSerializable.Rate): string {
    if ((<any>r).MassAction)
        return (<{ MassAction: string }>r).MassAction;
    else
        return "[" + (<{ Function: string }>r).Function + "]";
}

function parseMultiset(str: string): CRNSerializable.MultiSetElement[] {
    if (!str)
        return null;
    var ret: CRNSerializable.MultiSetElement[] = [];
    var c = 0;
    var isEnd = function () { return c >= str.length; }
    var tokenize = function () {
        var ret = str.substr(c);
        // Get rid of whitespace.
        c += ret.match(/[\s]*/)[0].length;
        ret = str.substr(c);
        var m = ret.match(/([\x2a]|[\x2c]|[^\x2a\x2c]+)/);
        if (m == null)
            return null;
        c += m[0].length;
        return m[0];
    };
    var parseMsetElement = function (): CRNSerializable.MultiSetElement {
        var token1 = tokenize();
        if (token1 == ',' || token1 == '*')
            throw "malformed multiset string";
        if (isEnd())
            return { element: token1, multiplicity: 1 };
        var token2 = tokenize();
        if (token2 == ',')
            return { element: token1, multiplicity: 1 };
        if (token2 != '*' || isEnd())
            throw "malformed multiset string";
        var token3 = tokenize();
        return { element: token3, multiplicity: parseInt(token1) };
    }
    while (!isEnd()) {
        var el = parseMsetElement();
        ret.push(el);
    }
    return ret;
}

export class Reaction {
    reactants = ko.observableArray<CRNSerializable.MultiSetElement>();
    products = ko.observableArray<CRNSerializable.MultiSetElement>();
    catalysts = ko.observableArray<CRNSerializable.MultiSetElement>();
    rate = ko.observable<string>();
    reverseRate = ko.observable<string>();

    rateWidth = ko.pureComputed<number>(() => 10 * Math.max(10, Math.max(this.rate() == null ? 0 : this.rate().length, this.reverseRate() == null ? 0 : this.reverseRate().length)));

    ErrorInputReactants = ko.observable<boolean>(false);
    ErrorInputCatalysts = ko.observable<boolean>(false);
    ErrorInputProducts = ko.observable<boolean>(false);
    private loadObjectToArray(value: any, a: KnockoutObservableArray<CRNSerializable.MultiSetElement>, crn: CRN) {
        var res = new Array<CRNSerializable.MultiSetElement>();
        if (!value)
            return;
        var arr: Array<CRNSerializable.MultiSetElement>;
        if (typeof value == "string")
            arr = parseMultiset(<string>value);
        else
            arr = value;
        for (var i = 0; i < arr.length; i++) {
            var val = arr[i];
            if (crn.getInitial(val.element) == null) {
                if (a == this.reactants)
                    this.ErrorInputReactants(true);
                if (a == this.catalysts)
                    this.ErrorInputCatalysts(true);
                if (a == this.products)
                    this.ErrorInputProducts(true);
                return;
            }
            res.push(val);
        }
        if (a == this.reactants)
            this.ErrorInputReactants(false);
        if (a == this.catalysts)
            this.ErrorInputCatalysts(false);
        if (a == this.products)
            this.ErrorInputProducts(false);
        a.removeAll();
        ko.utils.arrayPushAll<CRNSerializable.MultiSetElement>(a, res);
    }

    getSpeciesListAsNames(model: Model, speciesIDs: CRNSerializable.MultiSetElement[]): string {
        if (model.deserializing())
            return "";
        return speciesIDs.map(el => {
            if (el.multiplicity == 1)
                return el.element;
            return el.multiplicity + " * " + el.element;
        }).join(', ');
    }

    getSpeciesListAsStructural(model: Model, speciesIDs: CRNSerializable.MultiSetElement[]): Text {
        if (model.deserializing())
            return document.createTextNode("");
        var crn = model.SelectedCRN();
        var text = speciesIDs.map(el => {
            if (el.multiplicity == 1)
                return crn.getInitial(el.element).structural();
            return el.multiplicity + " * " + crn.getInitial(el.element).structural();
        }).join(', ');
        return document.createTextNode(text);
    }

    // TODO: handle multisets properly
    getSpeciesListAsSVG(model: Model, speciesIDs: CRNSerializable.MultiSetElement[]): HTMLSpanElement {
        var retDiv = document.createElement('div');
        if (model.deserializing())
            return retDiv;
        var crn = model.SelectedCRN();
        retDiv.setAttribute('class', 'crn c-species-list');
        // For each species...
        for (var i = 0; i < speciesIDs.length; i++) {
            var species = crn.getInitial(speciesIDs[i].element);
            if (!species)
                continue;
            // Unless this is the first one, append the '+' separator.
            if (i > 0)
                retDiv.appendChild(document.createTextNode('+'));
            var svgContent = species.getSVG();
            if (svgContent != null) {
                svgContent.setAttribute('class', 'crn c-species-list__svg-species');

                if (speciesIDs[i].multiplicity > 1) {
                    var countDisplay = document.createElement('span');
                    countDisplay.textContent = speciesIDs[i].multiplicity + " *";
                    retDiv.appendChild(countDisplay);
                }
                retDiv.appendChild(svgContent);
            }
        }
        var retSpan = document.createElement('span');
        retSpan.appendChild(retDiv);
        return retSpan;
    }

    getSpeciesList(model: InferenceGraph, speciesIDs: CRNSerializable.MultiSetElement[]): HTMLSpanElement {
        var retSpan = document.createElement('span');
        if (model.deserializing())
            return retSpan;
        var crn = model.SelectedCRN();
        retSpan.setAttribute('class', 'crn c-species-list');
        for (var i = 0; i < speciesIDs.length; i++) {
            if (i > 0) {
                var plusDisplay = document.createElement('span');
                plusDisplay.textContent = '+';
                retSpan.appendChild(plusDisplay);
            }
            var name = speciesIDs[i].element;
            var species = crn.getInitial(name);
            if (speciesIDs[i].multiplicity > 1) {
                var countDisplay = document.createElement('span');
                countDisplay.textContent = speciesIDs[i].multiplicity + " *";
                retSpan.appendChild(countDisplay);
            }

            var speciesDiv = document.createElement('div');
            if (species != null && model.reactionsShowGraphic()) {
                var svgContent = species.getSVG();
                if (svgContent != null) {
                    svgContent.setAttribute('class', 'crn c-species-list__svg-species');
                    speciesDiv.appendChild(svgContent);
                }
            }
            if (species != null && model.reactionsShowStructural()) {
                var p = document.createElement('p');
                p.appendChild(document.createTextNode(species.structural()));
                speciesDiv.appendChild(p);
            }
            if (model.reactionsShowNames()) {
                var p = document.createElement('p');
                p.appendChild(document.createTextNode(name));
                speciesDiv.appendChild(p);
            }

            retSpan.appendChild(speciesDiv);
        }
        return retSpan;
    }
}

export class Initial {
    constructor(private crn: CRN) {
        this.svg.subscribe(v => this.cachedsvg = null);
    }

    species = ko.observable<string>();
    structural = ko.observable<string>();
    svg = ko.observable<string>();
    value = ko.observable<string>();
    constant = ko.observable<boolean>();
    time = ko.observable<string>();
    spatial = ko.observable<CRNSerializable.SpatialInitial>();

    plot = ko.pureComputed({
        read: function () {
            var self = <Initial><any>this;
            return self.crn.settings.Sim.Plots().indexOf(self.species()) >= 0;
        },
        write: function (value: boolean) {
            var self = <Initial><any>this;
            var species = self.species();
            var plots = self.crn.settings.Sim.Plots;
            if (value) {
                if (plots().indexOf(self.species()) < 0)
                    plots.push(self.species());
            }
            else {
                while (true) {
                    var idx = plots().indexOf(species);
                    if (idx < 0)
                        break;
                    plots.splice(idx, 1);
                }
            }
        },
        owner: this
    });

    /** This function is invoked by the JS engine when serialization is requested. It ensures that serialization can succeed despite the circular reference to the owner CRN. */
    toJSON = function (this: Initial) {
        var copy = ko.toJS(this);
        delete copy.plot;
        delete copy.crn;
        return copy;
    };

    getSVGWidth(): number {
        var svg = this.svg();
        if (svg == null || svg == "")
            return null;
        return parseFloat(this.cacheSVG().getAttribute('width'));
    }

    getSVGHeight(): number {
        var svg = this.svg();
        if (svg == null || svg == "")
            return null;
        return parseFloat(this.cacheSVG().getAttribute('height'));
    }

    private cachedsvg?: Element;

    private cacheSVG(): Element {
        var svg = this.svg();
        if (svg == null || svg == "")
            return null;
        if (this.cachedsvg == null) {
            var doc = parser.parseFromString(this.svg(), "image/svg+xml");
            this.cachedsvg = doc.documentElement;
        }
        return this.cachedsvg;
    }

    /** Returns an SVG element (parsed from the string). */
    getSVG(): Element {
        var svg = this.svg();
        if (svg == null || svg == "")
            return null;
        return <Element>this.cacheSVG().cloneNode(true);
    }
}

export class ExternalSetting {
    constructor(public owner: CRN) { }
    template = ko.observable<string>("");

    /** This function is invoked by the JS engine when serialization is requested. It ensures that serialization can succeed despite the circular reference to the owner CRN. */
    toJSON = function (this: ExternalSetting) {
        var copy = ko.toJS(this);
        delete copy.crn;
        return copy;
    };
}

export class CRN {
    deserializing: KnockoutObservable<boolean> = ko.observable(false);

    name = ko.observable<string>("");
    attributes: { [idx: string]: CRNSerializable.SpeciesAttributes } = {};
    settings = new CRNSettings.CRNSettingsVM();
    externalSettings = ko.observableArray<ExternalSetting>([]);
    reactions = ko.observableArray<Reaction>([]);
    initials = ko.observableArray<Initial>([]);

    svgStyle = ko.observable<string>("text { stroke: black; fill: black; stroke-width: 0; font-size: 15px; font-family: Verdana, Arial, sans-serif }");

    selectedSpecies = ko.observable<Initial>(null);
    selectedReaction = ko.observable<Reaction>(null);

    getInitial(name: string) {
        for (var i = 0; i < this.initials().length; i++) {
            if (this.initials()[i].species() == name)
                return this.initials()[i];
        }
        return null;
    }

    AddSpecies() {
        var name = 'usersp';
        var flag = true;
        var exists = false;
        for (var idx = 1; this.getInitial(name) != null; idx++)
            name = 'usersp_' + idx;
        var sp = new Initial(this);
        sp.species(name);
        this.initials.push(sp);
    }

    RemoveSpecies = () => {
        var data = this.selectedSpecies();
        if (data == null)
            return;
        this.reactions(this.reactions().map((val) => {
            val.reactants.remove(val.reactants().filter((a) => {
                return a.element == data.species()
            })[0]);
            val.catalysts.remove(val.catalysts().filter((a) => {
                return a.element == data.species()
            })[0]);
            val.products.remove(val.products().filter((a) => {
                return a.element == data.species()
            })[0]);
            return val;
        }));
        this.initials.remove(data);
    }

    AddReaction = () => {
        this.reactions.push(new Reaction());
    }

    RemoveReaction = () => {
        var data = this.selectedReaction();
        if (data == null)
            return;
        this.reactions.remove(this.selectedReaction());
    }

    getSerializableForm(): CRNSerializable.CRN {
        // Note: order of fields may be important for some serializers. Do not reorder fields except to make it match the output of serializers.
        var crn: CRNSerializable.CRN = {
            name: this.name(),
            settings: this.settings.source,
            reactions: this.reactions().map((r: Reaction): CRNSerializable.Reaction => {
                var ret: CRNSerializable.Reaction = {
                    catalysts: r.catalysts().map(el => { return { element: el.element, multiplicity: el.multiplicity } }),
                    reactants: r.reactants().map(el => { return { element: el.element, multiplicity: el.multiplicity } }),
                    reverse: parseRate(r.reverseRate()),
                    rate: parseRate(r.rate()),
                    products: r.products().map(el => { return { element: el.element, multiplicity: el.multiplicity } }),
                }
                if (r.reverseRate() == null || r.reverseRate() == "")
                    delete ret.reverse;
                return ret;
            }),
            initials: this.initials().map((i: Initial) => {
                var sp: CRNSerializable.Initial = { constant: i.constant(), value: i.value(), species: i.species() }
                if (i.time() != null)
                    sp.time = i.time();
                if (i.spatial() != null)
                    sp.spatial = i.spatial();
                return sp;
            }),
            attributes: this.attributes
        };

        return crn;
    }

    reset(): void {
        this.name("");
        this.initials([]);
        this.reactions([]);
        this.settings.reset();
        this.externalSettings.removeAll();
    }

    fromSerializableForm(crn: CRNSerializable.CRN): void {
        if (crn == null) {
            this.reset();
            return;
        }

        this.deserializing(true);

        this.name(crn.name);
        this.attributes = crn.attributes;
        this.settings.source = crn.settings;

        this.initials(crn.initials.map((i: CRNSerializable.Initial) => {
            var init = new Initial(this);
            init.species(i.species);
            init.value(i.value);
            init.constant(i.constant);
            //init.plot(crn.settings.simulation.plots.some(exp => exp == i.species));
            init.time(i.time);
            init.spatial(i.spatial);
            var attr = crn.attributes[i.species];
            init.structural(attr == null ? null : attr.structure);
            init.svg(attr == null ? null : attr.svg);
            return init;
        }));

        this.reactions(crn.reactions.map((r: CRNSerializable.Reaction) => {
            var reac = new Reaction();
            reac.reactants(r.reactants);
            reac.products(r.products);
            reac.catalysts(r.catalysts);
            reac.rate(stringRate(r.rate));
            reac.reverseRate(r.reverse ? stringRate(r.reverse) : "");
            return reac;
        }));

        // Temporary place to create the axes labels, unless they are overridden by directives already. This will be eventually done on the F# side.
        var candidateXLabel: string = "";
        var candidateYLabel: string = "";
        if (crn.settings.simulator == "PDE") {
            if (crn.settings.spatial.dimensions == 1) {
                candidateXLabel = "Space";
                candidateYLabel = "Time";
            }
            else {
                candidateXLabel = "Space (x)";
                candidateYLabel = "Space (y)";
            }
        }
        else {
            candidateXLabel = "Time";
            if (crn.settings.simulator == "SSA")
                candidateYLabel = "Species count";
            else
                candidateYLabel = "Concentration (nM)";
        }
        if (this.settings.PlotSettings.XLabel() == "" || this.settings.PlotSettings.XLabel() == null)
            this.settings.PlotSettings.XLabel(candidateXLabel);
        if (this.settings.PlotSettings.YLabel() == "" || this.settings.PlotSettings.YLabel() == null)
            this.settings.PlotSettings.YLabel(candidateYLabel);

        this.deserializing(false);
    }
}

export class Model {
    deserializing: KnockoutObservable<boolean> = ko.observable(false);

    public Top = ko.observable<CRN>(new CRN());
    public NodeID = ko.pureComputed(() => this.Top().name());
    public Systems = ko.observableArray<CRN>();

    public SelectedCRN = ko.observable<CRN>(this.Top());
    public AllCRNs = ko.computed<CRN[]>(() => {
        if (this.deserializing())
            return [];
        var ret = [this.Top()];
        for (let crn of this.Systems())
            ret.push(crn);
        return ret;
    });

    reset() {
        this.SelectedCRN(this.Top());
        this.Top().reset();
        this.Systems([]);
    }

    allInitials: KnockoutComputed<Initial[]> = ko.pureComputed(() => {
        if (this.deserializing())
            return [];
        var ret = this.Top().initials();
        for (let crn of this.Systems())
            ret = ret.concat(crn.initials());
        return ret;
    });

    getSerializableForm(): CRNSerializable.Model {
        var model = {
            top: this.Top().getSerializableForm(),
            systems: this.Systems().map(crn => crn.getSerializableForm()),
        };
        return model;
    }

    fromSerializableForm(model: CRNSerializable.Model): void {
        if (model == null) {
            this.reset();
            return;
        }

        this.deserializing(true);

        this.Top().fromSerializableForm(model.top);

        for (var c = 0; c < model.systems.length; c++) {
            if (this.Systems().length > c)
                this.Systems()[c].fromSerializableForm(model.systems[c]);
            else {
                var vm = new CRN();
                vm.fromSerializableForm(model.systems[c]);
                this.Systems.push(vm);
            }
        }
        if (this.Systems().length > model.systems.length)
            this.Systems(this.Systems().slice(0, model.systems.length));

        if (this.SelectedCRN() != null) {
            // Set the selected CRN to the CRN in the new model that has the same name as the previously selected CRN. Failing that, set it to the top.
            let selectedCRNName = this.SelectedCRN().name();
            let selected = null;
            for (let crn of this.Systems())
                if (crn.name() == selectedCRNName) {
                    selected = crn;
                    break;
                }
            if (selected == null)
                selected = this.Top();
            this.SelectedCRN(selected);
        }

        this.deserializing(false);
    }
}

export class InferenceGraph {
    constructor(public Data: DataSets.KnockoutBasedDataSetsList) {
        if (Data != null)
            this.Files = new Files.SortableFilenamesList(Data.getFullList(), this);
    }

    deserializing: KnockoutObservable<boolean> = ko.observable(false);

    public Task: WGI.Microsoft.Research.CRNEngine.Task;
    public Nodes = ko.observable<Model[]>([new Model()]);
    public Edges = ko.observable<{ [key: string]: WGI.Microsoft.Research.CRNEngine.GuiIGEdge[] }>({});
    public SelectedNode = ko.observable<Model>(this.Nodes()[0]);
    public SelectedCRN = ko.pureComputed<CRN>(() => this.SelectedNode().SelectedCRN());

    public Files: Files.SortableFilenamesList;

    reset() {
        this.Nodes([new Model()]);
        this.SelectedNode(this.Nodes()[0]);
    }
    public AllCRNNames = ko.computed<string[]>(() => {
        var ret: string[] = [];
        for (let model of this.Nodes())
            for (let crn of model.AllCRNs())
                if (ret.indexOf(crn.name()) == -1)
                    ret.push(crn.name());
        return ret;
    });

    fromSerializableForm(ig: CRNSerializable.IG): void {
        if (ig == null) {
            this.reset();
            return;
        }

        this.deserializing(true);

        this.Task = ig.task;
        // I'm going to treat the first model differently. Instead of replacing the entire array, I'm going to make it so that the first model in the array is always the same object, and mutate that. This way, any component that is bound to the Model directly will correctly preserve the binding. Note that components that bind to nodes other than the first are assumed to be aware of IGs and able to adjust.
        var firstModel: CRNSerializable.Model = null;
        var models: CRNSerializable.Model[] = [];
        for (let n in ig.nodes) {
            let model = ig.nodes[n];
            if (firstModel == null)
                firstModel = model;
            else
                models.push(model);
        }
        if (firstModel == null) {
            this.reset();
            return;
        }
        var firstNode = this.Nodes()[0];

        firstNode.fromSerializableForm(firstModel);

        var nodes: Model[] = [firstNode];
        for (let model of models) {
            var node = new Model();
            node.fromSerializableForm(model);
            nodes.push(node);
        }
        var currentlySelectedNode = this.SelectedNode();

        this.Nodes(nodes);

        var newSelectedNode = firstNode;
        for (var node of nodes)
            if (node.Top().name() == currentlySelectedNode.Top().name())
                newSelectedNode = node;

        this.SelectedNode(newSelectedNode);
        this.Edges(ig.edges);

        this.deserializing(false);
    }

    getSerializableForm(): CRNSerializable.IG {
        var ret: CRNSerializable.IG = { nodes: {}, edges: {}, expanded: false };
        if (this.Task != null)
            ret.task = this.Task;
        for (let node of this.Nodes()) {
            var model = node.getSerializableForm();
            ret.nodes[node.Top().name()] = model;
        }
        ret.edges = this.Edges();
        return ret;
    }

    speciesShowStructural: KnockoutObservable<boolean> = ko.observable(true);
    speciesShowGraphic: KnockoutObservable<boolean> = ko.observable(true);
    reactionsViewType: KnockoutObservable<string> = ko.observable("name"); // "name", "structural", "svg"

    private reactionsShowNamesInternal: KnockoutObservable<boolean> = ko.observable(true);
    private reactionsShowGraphicInternal: KnockoutObservable<boolean> = ko.observable(true);
    private reactionsShowStructuralInternal: KnockoutObservable<boolean> = ko.observable(false);
    reactionsShowOnlyNames: KnockoutComputed<boolean> = ko.pureComputed(() => this.reactionsShowNamesInternal() && !this.reactionsShowGraphicInternal() && !this.reactionsShowStructuralInternal());
    reactionsShowNames: KnockoutComputed<boolean> = <any>ko.pureComputed({
        read: function (this: InferenceGraph) { return this.reactionsShowNamesInternal() },
        write: function (this: InferenceGraph, value: boolean) {
            this.reactionsShowNamesInternal(value);
            if (!(this.reactionsShowNames() || this.reactionsShowGraphic() || this.reactionsShowStructuralInternal()))
                this.reactionsShowNamesInternal(true);
        }, owner: this
    });
    reactionsShowGraphic: KnockoutComputed<boolean> = <any>ko.pureComputed({
        read: function (this: InferenceGraph) { return this.reactionsShowGraphicInternal() },
        write: function (this: InferenceGraph, value: boolean) {
            this.reactionsShowGraphicInternal(value);
            if (!(this.reactionsShowNames() || this.reactionsShowGraphic() || this.reactionsShowStructuralInternal()))
                this.reactionsShowGraphicInternal(true);
        }, owner: this
    });
    reactionsShowStructural: KnockoutComputed<boolean> = <any>ko.pureComputed({
        read: function (this: InferenceGraph) { return this.reactionsShowStructuralInternal() },
        write: function (this: InferenceGraph, value: boolean) {
            this.reactionsShowStructuralInternal(value);
            if (!(this.reactionsShowNames() || this.reactionsShowGraphic() || this.reactionsShowStructuralInternal()))
                this.reactionsShowStructuralInternal(true);
        }, owner: this
    });

    showRates: KnockoutObservable<boolean> = ko.observable(true);

    allInitials: KnockoutComputed<Initial[]> = ko.pureComputed(() => {
        var ret = this.Nodes().map(node => node.allInitials());
        return [].concat.apply([], ret);
    });

    isGraphicAvailable: KnockoutComputed<boolean> = ko.pureComputed(() => {
        var initials = this.allInitials();
        return initials.length > 0 && !initials.every(initial => initial.svg() == null || initial.svg() == "");
    });
    isStructuralAvailable: KnockoutComputed<boolean> = ko.pureComputed(() => {
        var initials = this.allInitials();
        return initials.length > 0 && !initials.every(initial => initial.structural() == null || initial.structural() == "");
    });

    parametersConfig: any = {
        data: ko.pureComputed(() => this.SelectedCRN().settings.Parameters()),
        showAll: true,
        headerTemplate: 'parameters-header',
        columnTemplate: 'parameters-template',
        ViewModel: this,
        selected: ko.pureComputed({
            read: () => this.SelectedCRN().settings.selectedParameter(),
            write: v => this.SelectedCRN().settings.selectedParameter(v),
            owner: this
        }),
    };

    speciesConfig: any = {
        data: ko.pureComputed(() => this.SelectedCRN().initials(), this),
        //showAll: true,
        headerTemplate: 'species-header',
        columnTemplate: 'species-template',
        ViewModel: this,
        selected: ko.pureComputed({
            read: () => this.SelectedCRN().selectedSpecies(),
            write: v => this.SelectedCRN().selectedSpecies(v),
            owner: this
        }),
    };
    reactionsConfig: any = {
        data: ko.pureComputed(() => this.SelectedCRN().reactions(), this),
        //showAll: true,
        headerTemplate: 'reactions-header',
        columnTemplate: 'reactions-template',
        ViewModel: this,
        selected: ko.pureComputed({
            read: () => this.SelectedCRN().selectedReaction(),
            write: v => this.SelectedCRN().selectedReaction(v),
            owner: this
        }),
    };

    AddParameter = () => this.SelectedCRN().settings.AddParameter();
    RemoveParameter = () => this.SelectedCRN().settings.RemoveParameter();
    AddReaction = () => this.SelectedCRN().AddReaction();
    RemoveReaction = () => this.SelectedCRN().RemoveReaction();
    AddSpecies = () => this.SelectedCRN().AddSpecies();
    RemoveSpecies = () => this.SelectedCRN().RemoveSpecies();

    setDefaultOptions() {
        if (this.isGraphicAvailable())
            this.reactionsViewType("svg");
        else
            this.reactionsViewType("name");
        this.showRates(true);
        this.speciesShowGraphic(true);
        this.speciesShowStructural(true);
    }
}
