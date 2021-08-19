// Implements a reaction graph viewer that has a MSAGL/JS graph plus some controls to operate on it.

import * as $ from 'jquery';
import * as CRNvm from './crnVM';
import * as MSAGL from './../../../HTML5SharedGUI/MSAGL_JS/Scripts/msagl';
import * as Utils from './Utils';
import * as ko from 'knockout';
import * as rx from 'rx';
import "../../GenericComponents/Scripts/Dropdown";
import ZoomSlider from './../../GenericComponents/Scripts/ZoomSlider';

// Graph node that represents a reaction.
class ReactionNode extends MSAGL.GNode {
    reaction: CRNvm.Reaction;
    constructor(id: string, reaction: CRNvm.Reaction) {
        super({ id: id, label: '', shape: 'rect' });
        this.reaction = reaction;
        this.fill = "white";
    }
    toJSON() {
        var clone = (<any>Object).assign({}, this);
        delete clone.reaction;
        return clone;
    }
}

// Graph node that represents a species.
class SpeciesNode extends MSAGL.GNode {
    species: CRNvm.Initial;
    constructor(species: CRNvm.Initial) {
        super({ id: species.species(), label: '', shape: 'roundedrect', thickness: species.value() == "0" ? 1 : 4 });
        this.species = species;
    }
    public addValue(v: string) {
        if (v != '0')
            this.thickness = 4;
    }
    toJSON() {
        var clone = (<any>Object).assign({}, this);
        delete clone.species;
        return clone;
    }
}

// Design note: maybe I should bring the code that performs the binding out of crnKO and into this file.
export class CRNGraphVM {
    constructor(public model: CRNvm.InferenceGraph) {
        var that = this;

        // Prepare a callback for performing reconfiguration when something changes.
        var reconfigure = function () {
            var crn = that.model.SelectedCRN();
            // Check whether species have SVG or structural strings. If there are no species, that counts as false.
            var supportsGraphics = that.showGraphics();
            var supportsStructural = that.showStructural();
            for (var i = 0; i < crn.initials().length; i++) {
                var species: CRNvm.Initial = crn.initials()[i];
                supportsGraphics = supportsGraphics || species.svg() != null;
                supportsStructural = supportsStructural || species.structural() != null;
            }
            // If they don't have SVG and/or structural, reconfigure the check boxes accordingly.
            if (!supportsGraphics) {
                that.showGraphics(false);
                if (!that.showStructural())
                    that.showNames(true);
            }
            if (!supportsStructural) {
                that.showStructural(false);
                if (!that.showGraphics())
                    that.showNames(true);
            }
            that.supportsGraphics(supportsGraphics);
            that.supportsStructural(supportsStructural);
        };
        reconfigure();
        this.propertyChanged.subscribeOnNext(reconfigure);

        // Send a notification when a property changes.
        var firePropertyChanged: (name: string) => (() => void) = function (name: string): (() => void) {
            return function () {
                that.propertyChangedStream.onNext(name);
            };
        }

        // Hook up the KO properties to the RX observable.
        ko.computed(() => this.model.SelectedCRN().initials()).subscribe(firePropertyChanged("crn.initials"));
        ko.computed(() => this.model.SelectedCRN().reactions()).subscribe(firePropertyChanged("crn.reactions"));
        // Explicitly declaring the indexer, to satisfy --noImplicitAny.
        var indexed = <{ [key: string]: any }>this;
        for (var i in indexed)
            if (ko.isObservable(indexed[i]))
                indexed[i].subscribe(firePropertyChanged(i));
    }

    private propertyChangedStream: Rx.Subject<string> = new Rx.Subject<string>();
    // A Rx Observable you can use to be notified when a property changes.
    propertyChanged = this.propertyChangedStream.asObservable();

    // UI states.
    supportsGraphics = ko.observable<boolean>(false);
    supportsStructural = ko.observable<boolean>(false);
    showNames = ko.observable<boolean>(true);
    showStructural = ko.observable<boolean>(false);
    showGraphics = ko.observable<boolean>(true);
    showRates = ko.observable<boolean>(true);
    layeredLayout = ko.observable<boolean>(true);
    horizontalLayout = ko.observable<boolean>(false);
    aspectRatio = ko.observable<boolean>(false);
    groupInitials = ko.observable<boolean>(false);
    editMode = ko.observable<boolean>(false);
    routing = ko.observable<string>(MSAGL.GSettings.sugiyamaSplinesRouting);

    public saveAsSVG = () => { };

    public zoom: ZoomSlider = new ZoomSlider();
}

export class CRNGraphViewer {
    // Instantiate this with a div that will contain the graph. The div needs to have an ID; I'll use it later. I should probably come up with a
    // design that doesn't require this.
    constructor(container: HTMLElement) {
        var that = this;
        this.container = container;
        this.iddgraph = new MSAGL.IDDSVGGraph(container);
        this.iddgraph.allowEditing = false;
        // Hook it up with a custom label draw function (for SVG rendering).
        this.iddgraph.customDrawLabel = function (svg, parent, label, owner) {
            return that.customDrawLabel(svg, parent, label, owner);
        };
    }

    vm: CRNGraphVM;

    private zoom_noRecurse = false;

    // Call to set the source data.
    setVM(vm: CRNGraphVM) {
        if (this.vm !== vm) {
            if (this.vm != null) {
                console.log("Warning: VM changed in CRNGraphViewer. This will leak KO subscriptions. If this is happening for a good reason, dispose logic needs to be implemented.");
            }
            var that = this;
            this.vm = vm;
            vm.saveAsSVG = () => that.iddgraph.saveAsSVG();

            vm.zoom.Value.subscribe(function (p) {
                if (!that.zoom_noRecurse)
                    that.iddgraph.setZoomLevel(p);
            });
            that.iddgraph.zoomLevelChangeCallback = function (v) {
                // Prevent recursion.
                that.zoom_noRecurse = true;
                vm.zoom.Value(v);
                that.zoom_noRecurse = false;
            }

            // I'm calling the update in a timeout so that if multiple changes happen at the same time (a common case), render() is only called once.
            var prepareUpdate = function (name: string) {
                if (name == "editMode") {
                    that.iddgraph.allowEditing = that.vm.editMode();
                }
                else {
                    if (!that.vm.showStructural() && !that.vm.showGraphics())
                        that.vm.showNames(true);
                    if (that.timeoutKey != null)
                        clearTimeout(that.timeoutKey);
                    that.timeoutKey = setTimeout(function () { that.timeoutKey = null; that.render(); });
                }
            };
            this.vm.propertyChanged.subscribeOnNext(prepareUpdate);

            // Begin a redraw.
            this.render();
        }
    }
    // This is the ID of the timeout that's currently being used to delay the call to render().
    private timeoutKey: number;

    container: HTMLElement;
    iddgraph: MSAGL.IDDSVGGraph;

    private customDrawReactionNode(svg: Element, parent: Element, label: MSAGL.GLabel, owner: ReactionNode): boolean {
        if (!this.vm.showRates())
            return false;
        var reaction: CRNvm.Reaction = owner.reaction;
        var rateText = document.createElementNS("http://www.w3.org/2000/svg", 'text');
        rateText.textContent = reaction.rate();
        rateText.setAttribute('x', (label.bounds.x + label.bounds.width / 2).toString());
        rateText.setAttribute('y', (label.bounds.y + 12).toString());
        if (owner.stroke == null)
            rateText.setAttribute('style', 'text-anchor: middle');
        else
            rateText.setAttribute('style', 'text-anchor: middle; stroke: ' + owner.stroke);
        parent.appendChild(rateText);
        if (reaction.reverseRate() == null)
            return true;
        var reverseRateText = document.createElementNS("http://www.w3.org/2000/svg", 'text');
        reverseRateText.textContent = reaction.reverseRate();
        reverseRateText.setAttribute('x', (label.bounds.x + label.bounds.width / 2).toString());
        reverseRateText.setAttribute('y', (label.bounds.y + label.bounds.height / 2 + 15).toString());
        if (owner.stroke == null)
            reverseRateText.setAttribute('style', 'text-anchor: middle');
        else
            reverseRateText.setAttribute('style', 'text-anchor: middle; stroke: ' + owner.stroke);
        parent.appendChild(reverseRateText);
        return true;
    }

    private customDrawSpeciesNode(svg: Element, parent: Element, label: MSAGL.GLabel, owner: SpeciesNode): boolean {
        // Note: this is not a good place to attempt to get the size of a text block, because getBBox will fail on Firefox, see https://bugzilla.mozilla.org/show_bug.cgi?id=612118
        var species: CRNvm.Initial = owner.species;
        // Start building the label.
        var y = label.bounds.y + 15;
        if (this.vm.showGraphics()) {
            // Add the SVG graphics, if available.
            var content: Element = species.getSVG();
            if (content != null) {
                var g = document.createElementNS("http://www.w3.org/2000/svg", "g");
                // Move the content to the label position by creating a group and setting its transform.
                var contentWidth = parseFloat(content.getAttribute('width'));
                var contentHeight = parseFloat(content.getAttribute('height'));
                g.setAttribute('transform', 'translate(' + (label.bounds.x + (label.bounds.width - contentWidth) / 2) + ' ' + label.bounds.y + ')');
                g.setAttribute('width', contentWidth.toString());
                g.setAttribute('height', contentHeight.toString());
                parent.appendChild(g);

                // This bit used to require Utils.copyTree, because using appendChild directly wouldn't work. This appears to no longer be an issue.
                g.appendChild(content);
                //Utils.copyTree(content, g);

                y += contentHeight;
            }
        }
        if (this.vm.showStructural()) {
            // Add the structural species string.
            var text = document.createElementNS("http://www.w3.org/2000/svg", 'text');
            var structural = species.structural();
            text.textContent = structural ? structural : "";
            text.setAttribute('x', (label.bounds.x + label.bounds.width / 2).toString());
            text.setAttribute('y', y.toString());
            text.setAttribute('style', 'fill: black; text-anchor: middle');
            parent.appendChild(text);
            y += 20;
        }
        if (this.vm.showNames()) {
            // Add the name string.
            var text = document.createElementNS("http://www.w3.org/2000/svg", 'text');
            text.textContent = species.species();
            text.setAttribute('x', (label.bounds.x + label.bounds.width / 2).toString());
            text.setAttribute('y', y.toString());
            text.setAttribute('style', 'fill: black; text-anchor: middle');
            parent.appendChild(text);
        }

        return true;
    }

    // The custom draw label function. Will be invoked by the graph viewer.
    private customDrawLabel(svg: Element, parent: Element, label: MSAGL.GLabel, owner: MSAGL.IElement): boolean {
        if ((<any>owner).reaction != null)
            return this.customDrawReactionNode(svg, parent, label, <ReactionNode>owner);
        else if ((<any>owner).species != null)
            return this.customDrawSpeciesNode(svg, parent, label, <SpeciesNode>owner);
        return false;
    }

    private customReactionNodeSizer(svg: Element, label: MSAGL.GLabel, owner: ReactionNode): MSAGL.IPoint {
        if (!this.vm.showRates())
            return MSAGL.GGraph.SVGSizer(svg, label);
        var reaction: CRNvm.Reaction = owner.reaction;
        var rateSize = MSAGL.GGraph.SVGSizer(svg, new MSAGL.GLabel(reaction.rate()));
        if (reaction.reverseRate() == null)
            return rateSize;
        var reverseRateSize = MSAGL.GGraph.SVGSizer(svg, new MSAGL.GLabel(reaction.reverseRate()));
        return { x: Math.max(rateSize.x, reverseRateSize.x), y: rateSize.y + 4 + reverseRateSize.y };
    }

    private customSpeciesNodeSizer(svg: Element, label: MSAGL.GLabel, owner: SpeciesNode): MSAGL.IPoint {
        var species: CRNvm.Initial = owner.species;
        // I'm starting with a size of zero, and I'm expanding it as I add elements to the label.
        var ret = { x: 0, y: 0 };
        if (this.vm.showNames()) {
            // Add room for the name.
            var box = MSAGL.GGraph.SVGSizer(svg, new MSAGL.GLabel(species.species()));
            ret = { x: Math.max(ret.x, box.x), y: ret.y + 20 };
        }
        if (this.vm.showStructural()) {
            // Add room for the structural string.
            var structural = species.structural();
            var box = MSAGL.GGraph.SVGSizer(svg, new MSAGL.GLabel(structural ? structural : ""));
            ret = { x: Math.max(ret.x, box.x), y: ret.y + 20 };
        }
        if (this.vm.showGraphics()) {
            // Add room for the SVG, if present.
            if (species.svg() != null)
                ret = { x: Math.max(ret.x, species.getSVGWidth()), y: ret.y + species.getSVGHeight() };
        }
        return ret;
    }

    // The custom node sizer. This needs to make enough room for the label to be rendered.
    private customSizer(svg: Element, label: MSAGL.GLabel, owner: MSAGL.IElement): MSAGL.IPoint {
        if ((<any>owner).reaction != null)
            return this.customReactionNodeSizer(svg, label, <ReactionNode>owner);
        else if ((<any>owner).species != null)
            return this.customSpeciesNodeSizer(svg, label, <SpeciesNode>owner);
        return MSAGL.GGraph.SVGSizer(svg, label);
    }

    // This function turns the CRNTable into a GGraph. TODO: handle multisets properly
    private makeGraph(): MSAGL.GGraph {
        var crn = this.vm.model.SelectedCRN();
        var ret: MSAGL.GGraph = new MSAGL.GGraph();
        // For each initial, add a node.
        for (var i = 0; i < crn.initials().length; i++) {
            var species = crn.initials()[i];
            // Initials may have duplicates. If I see a species twice, I may have to upgrade its value.
            var existing = <SpeciesNode>ret.getNode(species.species());
            if (existing != null)
                existing.addValue(species.value());
            else {
                var spNode = new SpeciesNode(species);
                ret.addNode(spNode);
            }
        }
        // For each reaction...
        for (var i = 0; i < crn.reactions().length; i++) {
            var reaction = crn.reactions()[i];
            var isReversible: boolean = reaction.reverseRate() == null || reaction.reverseRate() == "";
            // Add a reaction node.
            var rnode = new ReactionNode('_reaction_' + i, reaction);
            var frate = parseFloat(reaction.rate());
            // Hack to display leak reactions in gray
            if (frate != 0 && frate <= 1E-9)
                rnode.stroke = "#AAAAAA";
            ret.addNode(rnode);
            // For each reactant...
            var reactants = reaction.reactants();
            for (var j = 0; j < reactants.length; j++) {
                // Get the species node.
                var inNode = ret.getNode(reactants[j].element);
                // Add an edge. It might need an arrowhead at source, if the reaction is reversible.
                var inEdge = new MSAGL.GEdge({
                    id: rnode.id + '_in_' + j, source: inNode.id, target: rnode.id, arrowHeadAtTarget: null,
                    arrowHeadAtSource: isReversible ? null : MSAGL.GArrowHead.filled
                });
                if (rnode.stroke != null)
                    inEdge.stroke = rnode.stroke;
                ret.addEdge(inEdge);
            }
            // For each product...
            var products = reaction.products();
            for (var j = 0; j < products.length; j++) {
                // Get the species node.
                var outNode = ret.getNode(products[j].element);
                // Add an edge.
                var outEdge = new MSAGL.GEdge({ id: rnode.id + '_out_' + j, source: rnode.id, target: outNode.id, arrowHeadAtTarget: MSAGL.GArrowHead.closed });
                if (rnode.stroke != null)
                    outEdge.stroke = rnode.stroke;
                ret.addEdge(outEdge);
            }
            // For each catalyst...
            var catalysts = reaction.catalysts();
            for (var j = 0; j < catalysts.length; j++) {
                // Get the species node.
                var inNode = ret.getNode(catalysts[j].element);
                // Add an edge.
                var inEdge = new MSAGL.GEdge({ id: rnode.id + '_cat_' + j, source: inNode.id, target: rnode.id, arrowHeadAtTarget: MSAGL.GArrowHead.diamondFilled });
                if (rnode.stroke != null)
                    inEdge.stroke = rnode.stroke;
                ret.addEdge(inEdge);
            }
        }

        // Deal with settings.
        if (this.vm.layeredLayout()) {
            // Deal with settings that only apply to layered layouts.
            // Plane transformation.
            ret.settings.transformation = this.vm.horizontalLayout() ? MSAGL.GPlaneTransformation.ninetyDegreesTransformation : MSAGL.GPlaneTransformation.defaultTransformation;
            // Enforcing aspect ratio.
            if (this.vm.aspectRatio()) {
                var w = this.container.offsetWidth;
                var h = this.container.offsetHeight;
                if (w != null && w > 0 && h != null && h > 0)
                    ret.settings.aspectRatio = this.vm.horizontalLayout() ? h / w : w / h;
            }
            // Grouping initials.
            if (this.vm.groupInitials()) {
                for (var i = 0; i < crn.initials().length; i++) {
                    var initial = crn.initials()[i];
                    if (initial.value() != "0") {
                        var upNode = initial.species();
                        for (var j = 0; j < ret.nodes.length; j++) {
                            if ((<SpeciesNode>ret.nodes[j]).species == null || (<SpeciesNode>ret.nodes[j]).species.value() == "0") {
                                var downNode = ret.nodes[j].id;
                                ret.settings.upDownConstraints.push(new MSAGL.GUpDownConstraint({ upNode: upNode, downNode: downNode }));
                            }
                        }
                    }
                }
            }
        }
        else
            ret.settings.layout = MSAGL.GSettings.mdsLayout;
        ret.settings.routing = this.vm.routing();

        // Temporarily append an SVG element to the DOM, so that I can use it for sizing.
        var style = window.getComputedStyle(this.container);
        var svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        // Try to give it the same font style, so the text can be accurately sized.
        if (style !== undefined) {
            (<any>svg).style.font = style.font;
            (<any>svg).style.fontFamily = style.fontFamily;
            (<any>svg).style.fontFeatureSettings = style.fontFeatureSettings;
            (<any>svg).style.fontSize = style.fontSize;
            (<any>svg).style.fontSizeAdjust = style.fontSizeAdjust;
            (<any>svg).style.fontStretch = style.fontStretch;
            (<any>svg).style.fontStyle = style.fontStyle;
            (<any>svg).style.fontVariant = style.fontVariant;
            (<any>svg).style.fontWeight = style.fontWeight;
        }
        document.body.appendChild(svg);

        var that = this;
        // Create the node boundaries (using the custom sizer).
        ret.createNodeBoundaries(function (label: MSAGL.GLabel, owner: MSAGL.IElement) { return that.customSizer(svg, label, owner); });

        // Throw away the temporary SVG element.
        document.body.removeChild(svg);

        return ret;
    }

    // The graph currently being laid out.
    private graphInLayout: MSAGL.GGraph = null;

    // Update the graph.
    private render() {
        // If a previous graph is still being laid out, abort it.
        if (this.graphInLayout != null)
            this.graphInLayout.stopLayoutGraph();

        // Make the GGraph.
        this.graphInLayout = this.makeGraph();
        // Put the graph in the IDDSVGGraph instance.
        this.iddgraph.setGraph(this.graphInLayout);

        // Prepare a layout callback.
        var that = this;
        var graphLayoutCallback = function () {
            // I'm keeping the graphInLayout around because I want subsequent calls to render() to always call stopLayoutGraph on it. This because Chrome does not seem to garbage collect workers that don't get terminated. If not for that issue, the graphInLayout could safely be set to null here.
            //that.graphInLayout = null;
            // Give it the style specified in the source data.
            that.iddgraph.styleString = that.vm.model.SelectedCRN().svgStyle();
            // Draw it.
            that.iddgraph.drawGraph();
        }
        this.graphInLayout.layoutCallbacks.add(graphLayoutCallback);

        // Begin layout.
        this.graphInLayout.beginLayoutGraph();
    }
}