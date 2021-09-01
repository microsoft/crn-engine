// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as MSAGL from '../../../HTML5SharedGUI/MSAGL_JS/Scripts/msagl';
import { WebSharperGeneratedInterfaces as WGI } from "../../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces"; // https://github.com/Microsoft/TypeScript/issues/5711
import * as Interfaces from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as CRN from './crnVM';
import * as ko from 'knockout';
import * as rx from 'rx';
import "../../GenericComponents/Scripts/Dropdown";
import ZoomSlider from './../../GenericComponents/Scripts/ZoomSlider';

var parser = new DOMParser();

/** This class represents a state space VM. Mostly, it holds a CRN (the one from which the state space was calculated), and the
state space itself. It also has some UI-related flags. */
export class StateSpaceGraphVM {
    constructor(crn: CRN.CRN) {
        this.crn = crn;

        var that = this;

        // Prepare a callback for performing reconfiguration when something changes.
        var reconfigure = function () {
            // Check whether species have SVG or structural strings. If there are no species, that counts as false.
            var supportsGraphics = false;
            var supportsStructural = false;
            for (var i = 0; i < that.crn.initials().length; i++) {
                var species: CRN.Initial = that.crn.initials()[i];
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
        crn.svgStyle.subscribe(firePropertyChanged("crn.svgStyle"));
        // Explicitly declaring the indexer, to satisfy --noImplicitAny.
        var indexed = <{ [key: string]: any }>this;
        for (var i in indexed)
            if (ko.isObservable(indexed[i]))
                indexed[i].subscribe(firePropertyChanged(i));

        this.stateSpace.subscribe(ss => {
            this.attributes = {};
            for (let att of ss.attributes)
                this.attributes[att.name] = att;
        });
    }

    // The source data.
    crn: CRN.CRN;
    stateSpace = ko.observable<Interfaces.StateSpace>();
    attributes: { [name: string]: Interfaces.SpeciesAttributes } = {};

    private propertyChangedStream: Rx.Subject<string> = new Rx.Subject<string>();
    // A Rx Observable you can use to be notified when a property changes.
    propertyChanged = this.propertyChangedStream.asObservable();

    // UI states.
    supportsGraphics = ko.observable<boolean>(false);
    supportsStructural = ko.observable<boolean>(false);
    showNames = ko.observable<boolean>(true);
    showStructural = ko.observable<boolean>(false);
    showGraphics = ko.observable<boolean>(false);
    showPropensities = ko.observable<boolean>(true);
    layeredLayout = ko.observable<boolean>(true);
    horizontalLayout = ko.observable<boolean>(true);
    aspectRatio = ko.observable<boolean>(false);
    editMode = ko.observable<boolean>(false);
    routing = ko.observable<string>(MSAGL.GSettings.splinesRouting);

    public saveAsSVG = () => { };

    public zoom: ZoomSlider = new ZoomSlider();
}

/** Describes precalculated sizes for parts of a node's content, to help in layout. */
interface SSNodeSize {
    name: MSAGL.IPoint;
    structural: MSAGL.IPoint;
    graphics: MSAGL.IPoint;
    pop: MSAGL.IPoint;
}

/** Extends a GNode with some additional information: the state this node represents, and some internal layout information. */
class StateSpaceNode extends MSAGL.GNode {
    constructor(node: any, state: Interfaces.State) {
        super(node);
        this.state = state;
        this.sizes = {};
    }
    public state: Interfaces.State;
    public sizes: { [idx: string]: SSNodeSize }; // name -> SSNodeSize
}

/** This class implements the StateSpaceViewer. Give it a container element and a StateSpaceGraphVM, and it will put a state space
graph on the page. */
export class StateSpaceViewer {
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

    container: HTMLElement;
    vm: StateSpaceGraphVM;

    private zoom_noRecurse = false;

    // Call to set the source data.
    setVM(vm: StateSpaceGraphVM) {
        if (this.vm !== vm) {
            if (this.vm != null) {
                console.log("Warning: VM changed in StateSpaceViewer. This will leak KO subscriptions. If this is happening for a good reason, dispose logic needs to be implemented.");
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

            var prepareUpdate = function (name: string) {
                if ((name == "zoomLevel") || (name == "zoomValue")) {
                    // zoom change should not trigger graph rerendering
                    return;
                }
                if (name == "editMode")
                    that.iddgraph.allowEditing = that.vm.editMode();
                else {
                    if (!that.vm.showStructural() && !that.vm.showGraphics())
                        that.vm.showNames(true);
                    that.render();
                }
            };
            this.vm.propertyChanged.subscribeOnNext(prepareUpdate);

            // Begin a redraw.
            this.render();
        }
    }

    private getStructural(species: string): string {
        var attr = this.vm.attributes[species];
        if (attr != null)
            return attr.structure;
        var initial = this.vm.crn.getInitial(species);
        if (initial != null)
            return initial.structural();
        return null;
    }

    private getSVG(species: string): string {
        var attr = this.vm.attributes[species];
        if (attr != null)
            return attr.svg;
        var initial = this.vm.crn.getInitial(species);
        if (initial != null)
            return initial.svg();
        return null;
    }

    private getSpeciesBlock(name: string, sizes: SSNodeSize): SVGElement {
        var showNames = this.vm.showNames() && sizes.name.x > 0;
        var showStructural = this.vm.showStructural() && sizes.structural.x > 0;
        var showGraphics = this.vm.showGraphics() && sizes.graphics.x > 0;
        var totalWidth = Math.max(sizes.name.x, Math.max(sizes.graphics.x, sizes.structural.x));
        var g = document.createElementNS("http://www.w3.org/2000/svg", 'g');
        if (showNames) {
            var text = document.createElementNS("http://www.w3.org/2000/svg", 'text');
            text.textContent = name;
            text.setAttribute('style', 'fill: black; text-anchor: middle');
            text.setAttribute('x', (totalWidth / 2).toString());
            text.setAttribute('y', (sizes.graphics.y + sizes.structural.y).toString());
            g.appendChild(text);
        }
        if (showStructural) {
            var text = document.createElementNS("http://www.w3.org/2000/svg", 'text');
            text.textContent = this.getStructural(name);
            text.setAttribute('style', 'fill: black; text-anchor: middle');
            text.setAttribute('x', (totalWidth / 2).toString());
            text.setAttribute('y', sizes.graphics.y.toString());
            g.appendChild(text);
        }
        if (showGraphics) {
            var doc = parser.parseFromString(this.getSVG(name), "image/svg+xml");
            var svg = doc.documentElement;
            svg.setAttribute('transform', 'translate(' + ((totalWidth - sizes.graphics.x) / 2) + ' ' + 0 + ')');
            g.appendChild(svg);
        }
        g.setAttribute('width', totalWidth.toString());
        g.setAttribute('height', (sizes.name.y + sizes.graphics.y + sizes.structural.y).toString());
        return g;
    }

    private customDrawLabel(svg: Element, parent: Element, label: MSAGL.GLabel, owner: MSAGL.IElement): boolean {
        if ((<StateSpaceNode>owner).state == null)
            return false;
        var node = <StateSpaceNode>owner;
        var state = node.state;
        // Start building the label.
        var x = label.bounds.x;
        var y = label.bounds.y + 10;
        var popw = 0;
        for (var name in state.species) {
            var sizes: SSNodeSize = node.sizes[name];
            popw = Math.max(popw, sizes.pop.x);
        }
        for (var name in state.species) {
            var sizes: SSNodeSize = node.sizes[name];
            var speciesBlock = this.getSpeciesBlock(name, sizes);
            var spWidth = parseFloat(speciesBlock.getAttribute('width'));
            var spHeight = parseFloat(speciesBlock.getAttribute('height'));
            speciesBlock.setAttribute('transform', 'translate(' + (x + popw + 5) + ' ' + y + ')');
            parent.appendChild(speciesBlock);

            var popBlock = document.createElementNS("http://www.w3.org/2000/svg", 'text');
            popBlock.textContent = state.species[name].toString();
            popBlock.setAttribute('x', x.toString());
            popBlock.setAttribute('y', (y + (spHeight - sizes.pop.y) / 2).toString());
            // Note: it is important for text-anchor to be set as an inline style, rather than an attribute, because attributes
            // are low-priority. In particular, DNA strand renderings from Species.fs declare their styles in a <style> tag, which
            // has a larger-than-expected scoping and can affect this text element. If text-anchor is in an attribute, it can
            // get ignored because of this.
            popBlock.setAttribute('style', 'fill: black; text-anchor: start');
            parent.appendChild(popBlock);

            y += spHeight + 10;
        }

        return true;
    }

    private customSizer(svg: Element, label: MSAGL.GLabel, owner: MSAGL.IElement): MSAGL.IPoint {
        if ((<StateSpaceNode>owner).state != null) {
            var showNames = this.vm.showNames();
            var showStructural = this.vm.showStructural();
            var showGraphics = this.vm.showGraphics();
            var ssnode = <StateSpaceNode>owner;
            var ret = { x: 0, y: 0 };
            var popw = 0;
            for (var name in ssnode.state.species) {
                var pop: number = ssnode.state.species[name];
                var structural: string = this.getStructural(name);
                var svgs: string = this.getSVG(name);
                var svgSize = { x: 0, y: 0 };
                if (showGraphics && svg != null) {
                    var doc = parser.parseFromString(svgs, "image/svg+xml");
                    var svgBlock = doc.documentElement;
                    svgSize.x = parseFloat(svgBlock.getAttribute('width'));
                    svgSize.y = parseFloat(svgBlock.getAttribute('height')) + 5;
                    if (isNaN(svgSize.x))
                        svgSize.x = 0;
                    if (isNaN(svgSize.y))
                        svgSize.y = 0;
                    // I'm adding the block to the svg sizer because the block may contain <style> tags that influence text sizinMSAGL.
                    svg.appendChild(svgBlock);
                }
                var textSizes: SSNodeSize = {
                    name: showNames ? MSAGL.GGraph.SVGSizer(svg, new MSAGL.GLabel(name)) : { x: 0, y: 0 },
                    structural: showStructural ? structural == null ? { x: 0, y: 0 } : MSAGL.GGraph.SVGSizer(svg, new MSAGL.GLabel(structural)) : { x: 0, y: 0 },
                    graphics: showGraphics ? svgSize : { x: 0, y: 0 },
                    pop: MSAGL.GGraph.SVGSizer(svg, new MSAGL.GLabel(pop.toString()))
                };
                ssnode.sizes[name] = textSizes;
                popw = Math.max(popw, textSizes.pop.x);
                ret.y += textSizes.name.y + textSizes.structural.y + textSizes.graphics.y + 10;
                ret.x = Math.max(ret.x, Math.max(textSizes.name.x, Math.max(textSizes.structural.x, textSizes.graphics.x)) + popw);
                while (svg.childNodes.length > 0)
                    svg.removeChild(svg.firstChild);
            }
            ret.y -= 14;
            return ret;
        }
        return MSAGL.GGraph.SVGSizer(svg, label);
    }

    private makeGraph(): MSAGL.GGraph {
        var ret: MSAGL.GGraph = new MSAGL.GGraph();
        var ss: Interfaces.StateSpace = this.vm.stateSpace();
        if (ss != null) {
            // First pass: generate nodes.
            for (var i = 0; i < ss.states.length; i++) {
                var state = ss.states[i];
                var node = new StateSpaceNode({ id: i, label: '', shape: 'rect' }, state);
                ret.addNode(node);
            }
            // Second pass: generate edges.
            for (var i = 0; i < ss.states.length; i++) {
                var state = ss.states[i];
                for (var j in state.transitions) {
                    var transition = state.transitions[j];
                    var propText = this.vm.showPropensities() ? transition.propensity : null;
                    // Ensure no duplicate IDs.
                    var id = i + '-' + transition.target;
                    while (ret.getEdge(id) != null)
                        id += 'i';
                    var edge = new MSAGL.GEdge({ id: id, source: i, target: transition.target.toString(), label: propText });
                    ret.addEdge(edge);
                }
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
        }
        else
            ret.settings.layout = MSAGL.GSettings.mdsLayout;
        ret.settings.routing = this.vm.routing();

        // Temporarily append an SVG element to the DOM, so that I can use it for sizinMSAGL.
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

    iddgraph: MSAGL.IDDSVGGraph;
    // The graph currently being laid out.
    graphInLayout: MSAGL.GGraph;

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
            that.iddgraph.styleString = that.vm.crn.svgStyle();
            // Draw it.
            that.iddgraph.drawGraph();
        }
        this.graphInLayout.layoutCallbacks.add(graphLayoutCallback);

        // Begin layout.
        this.graphInLayout.beginLayoutGraph();
    }
}
