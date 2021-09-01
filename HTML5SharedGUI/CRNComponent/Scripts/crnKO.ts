// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import "../../KnockoutGrid/knockoutgrid";
import * as $ from 'jquery';
import * as ko from 'knockout';
import * as CRNSettings from './crnSettings';
import * as CRNgraph from './crnGraphViewer';
import * as StateSpaceGraph from './crnStateSpaceViewer';
import * as InferenceGraphViewer from './inferenceGraphViewer';
import * as CRNvm from './crnVM';
import * as Serializable from './../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import "../../GenericComponents/Scripts/KOBindings";

// This file handles KO registration and binding for the CRN components. Available components include: crnsettings, crnsettings-readonly, reactions-viewer, species-viewer, parameters-viewer, crnreactiongraph, statespacegraph.

var parser = new DOMParser();

// Set up a ViewModel factory that returns the current context. This is for when the factory is not supposed to create a new VM, because the VM is supplied from the outside (i.e. as a parameter to applyBindings).
var vmp = {
    createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
        var context = ko.contextFor(componentInfo.element);
        return context === null ? {} : context.$data;
    }
};

if ((<any>ko.bindingHandlers).speciesarray == null)
    (<any>ko.bindingHandlers).speciesarray = {
        init: function (element: HTMLInputElement, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            var value = ko.unwrap(valueAccessor());
            element.value = bindingContext.$data.getSpeciesListAsNames(bindingContext.$parent.ViewModel, value);
            $(element).change(() => {
                var newVal = element.value;
                bindingContext.$data.loadObjectToArray(newVal, valueAccessor(), bindingContext.$parent.ViewModel);
            });
        },
        update: function (element: HTMLInputElement, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            var value = ko.unwrap(valueAccessor());
            element.value = bindingContext.$data.getSpeciesListAsNames(bindingContext.$parent.ViewModel, value);
        }
    }

// Register the crn settings component (as editable and read-only).
import * as crnSettingsTemplate from 'raw-loader!../html/crn-settings-component.html';
ko.components.register("crnsettings", {
    viewModel: vmp,
    template: crnSettingsTemplate
});
import * as reactionsTemplate from 'raw-loader!../html/reactions-table.html';
ko.components.register("reactions-viewer", {
    viewModel: vmp,
    template: reactionsTemplate
});
import * as speciesTemplate from 'raw-loader!../html/species-table.html';
ko.components.register("species-viewer", {
    viewModel: vmp,
    template: speciesTemplate
});
import * as parametersTemplate from 'raw-loader!../html/parameters-table.html';
ko.components.register("parameters-viewer", {
    viewModel: vmp,
    template: parametersTemplate
});

// Register a custom binding that turns a CRN object (the data) into a CRNGraphViewer (the UI).
if ((<any>ko.bindingHandlers).crngraph == null)
    (<any>ko.bindingHandlers).crngraph = {
        init: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            // Initialisation: create an instance of the CRNGraphViewer.
            element.CRNGraphViewer = new CRNgraph.CRNGraphViewer(element);
            element.CRNGraphViewer.setVM(viewModel);
        },
        update: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            // Update: get the CRN, tell the CRNGraphViewer to render.
            //var t: CRN.CRNTable = viewModel.table();
            element.CRNGraphViewer.setVM(viewModel);
        }
    };

// Register a custom binding that turns an IG object (the data) into a InferenceGraphViewer (the UI).
if ((<any>ko.bindingHandlers).inferencegraph == null) {
    (<any>ko.bindingHandlers).inferencegraph = {
        init: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            // Initialisation: create an instance of the InferenceGraphViewer.
            element.InferenceGraphViewer = new InferenceGraphViewer.InferenceGraphViewer(element);
            element.InferenceGraphViewer.setVM(viewModel);
        },
        update: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            element.InferenceGraphViewer.setVM(viewModel);
        }
    };
}

// Register a custom binding that turns a State Space object into a StateSpaceGraphViewer.
if ((<any>ko.bindingHandlers).statespacegraph == null)
    (<any>ko.bindingHandlers).statespacegraph = {
        init: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            element.StateSpaceGraphViewer = new StateSpaceGraph.StateSpaceViewer(element);
            element.StateSpaceGraphViewer.setVM(viewModel);
        },
        update: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            element.StateSpaceGraphViewer.setVM(viewModel);
        }
    };

// Register a custom binding that allows to change the set of to_be_plotted species by specifying names.
if ((<any>ko.bindingHandlers).plottedSpecies == null)
    (<any>ko.bindingHandlers).plottedSpecies = {
        init: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            $(element).on("change", function () {
                var species: KnockoutObservableArray<string> = valueAccessor();
                var speciesUnwrapped = ko.unwrap(species);
                var names: string[] = $(element).prop("value").split("\n");
                names = names.map(s => s.trim()).filter(s => s != "");
                species(names);
            })
        },
        update: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            var species: KnockoutObservableArray<string> = valueAccessor();
            var speciesUnwrapped = ko.unwrap(species);
            $(element).prop("value", speciesUnwrapped == null ? null : speciesUnwrapped.join("\n"));
        }
    };

// Register a custom binding to split variable names in sweep control
if ((<any>ko.bindingHandlers).sweepVariableNames == null)
    (<any>ko.bindingHandlers).sweepVariableNames = {
        init: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            $(element).on("change", function () {
                var names: string[] = $(element).prop("value").split(",");
                var nameList: string[] = valueAccessor();
                nameList.length = 0;
                for (let i = 0; i < names.length; i++)
                    nameList[i] = names[i];
            })
        },
        update: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            var nameList: KnockoutObservableArray<string> = valueAccessor();
            var nameListUnwrapped: string[] = ko.unwrap(nameList);
            var concatNames: string = nameList.length == 0 ? "" : nameListUnwrapped.reduce((prev, cur, curindex) => prev + "," + cur);
            $(element).prop("value", concatNames);
        }
    };

// Register a custom binding to split variable values in sweep control
if ((<any>ko.bindingHandlers).sweepVariableValues == null)
    (<any>ko.bindingHandlers).sweepVariableValues = {
        init: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            $(element).on("change", function () {
                var valueBlocks: string[] = $(element).prop("value").split(";");

                var values: Serializable.Expression[][] =
                    valueBlocks.map((valueBlock) => valueBlock.split(",")
                        .map((str) => <Serializable.Expression>{ Float: +str }));

                var valuesTransposed: Serializable.Expression[][] = new Array<Array<Serializable.Expression>>();
                var temp: Serializable.Expression[];
                for (var i = 0; i < values[0].length; ++i) {
                    temp = [];
                    for (var j = 0; j < values.length; ++j)
                        temp.push(values[j][i]);
                    valuesTransposed.push(temp);
                }

                var valueList: Serializable.Expression[][] = valueAccessor();
                valueList.length = 0;
                for (let i = 0; i < valuesTransposed.length; ++i)
                    valueList[i] = valuesTransposed[i];
            })
        },
        update: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            var valueList: KnockoutObservableArray<Serializable.Expression[]> = valueAccessor();
            var valueListUnwrapped: Serializable.Expression[][] = ko.unwrap(valueList);

            var valueListUnwrappedToString: string[][] = valueListUnwrapped.map((vl) => vl.map((v) => CRNSettings.expressionToString(v)));
            var valueListUnwrappedToStringTransposed: string[][] = new Array<Array<string>>();
            var temp: string[];
            for (var i = 0; i < valueListUnwrappedToString[0].length; ++i) {
                temp = [];
                for (var j = 0; j < valueListUnwrappedToString.length; ++j)
                    temp.push(valueListUnwrappedToString[j][i]);
                valueListUnwrappedToStringTransposed.push(temp);
            }

            var concatValues: string =
                valueListUnwrappedToStringTransposed.map((vl) => vl.reduce((prev, cur) => prev + "," + cur))
                    .reduce((prev, cur) => prev + ";" + cur);
            if (!concatValues)
                concatValues = "";
            $(element).prop("value", concatValues);
        }
    };

import * as reactionGraphTemplate from 'raw-loader!../html/crngraphview.html';
if (!ko.components.isRegistered('crnreactiongraph'))
    ko.components.register('crnreactiongraph', {
        viewModel: vmp,
        template: reactionGraphTemplate
    });
import * as inferenceGraphTemplate from 'raw-loader!../html/inference-graph.html';
if (!ko.components.isRegistered('inferencegraph'))
    ko.components.register('inferencegraph', {
        viewModel: vmp,
        template: inferenceGraphTemplate
    });
import * as stateSpaceTemplate from 'raw-loader!../html/statespacegraphview.html';
ko.components.register('statespacegraph', {
    viewModel: vmp,
    template: stateSpaceTemplate
});

// Register the CRN selector component.
import * as CRNSelectorTemplate from 'raw-loader!../html/crnselector.html';
if (!ko.components.isRegistered('crnselector'))
    ko.components.register('crnselector', {
        viewModel: vmp,
        template: CRNSelectorTemplate
    });

import * as speciesRowsTemplate from 'raw-loader!../html/species-rows.html';
import * as reactionsRowsTemplate from 'raw-loader!../html/reactions-rows.html';
import * as parametersRowsTemplate from 'raw-loader!../html/parameters-rows.html';

// Binds all of the following components that are found under the specified div to the specified CRN: crnsettings, crnsettings-readonly, reactions-viewer, species-viewer and parameters-viewer.
export function bind(div: HTMLElement, model: CRNvm.InferenceGraph) {
    // Clear existing bindings. This is necessary in case there's another viewmodel further up within the hierarchy.
    (<any>ko.utils.domNodeDisposal).cleanExternalData = function () { }; // prevent non-ko handlers from being cleaned by cleanNode
    ko.cleanNode(div);
    // Apply the viewmodel to the div.
    if (!$('#species-template').length) $('head').append(speciesRowsTemplate);
    if (!$('#reactions-template').length) $('head').append(reactionsRowsTemplate);
    if (!$('#parameters-template').length) $('head').append(parametersRowsTemplate);
    ko.applyBindings(model, div);
}

// Binds all crnreactiongraph components found in the specified div to the specified CRN graph VM.
export function bindGraph(div: HTMLElement, crnG: CRNgraph.CRNGraphVM) {
    // Clear existing bindings. This is necessary in case there's another viewmodel further up within the hierarchy.
    (<any>ko.utils.domNodeDisposal).cleanExternalData = function () { }; // prevent non-ko handlers from being cleaned by cleanNode
    ko.cleanNode(div);
    // Apply the viewmodel to the div.
    ko.applyBindings(crnG, div);
}

// Binds all inferencegraph components found in the specified div to the specified CRN graph VM.
export function bindInferenceGraph(div: HTMLElement, model: InferenceGraphViewer.InferenceGraphViewerVM) {
    // Clear existing bindings. This is necessary in case there's another viewmodel further up within the hierarchy.
    (<any>ko.utils.domNodeDisposal).cleanExternalData = function () { }; // prevent non-ko handlers from being cleaned by cleanNode
    ko.cleanNode(div);
    // Apply the viewmodel to the div.
    ko.applyBindings(model, div);
}

// Binds all statespacegraph components found in the specified div to the specified state space graph VM.
export function bindStateSpace(div: HTMLElement, crnSS: StateSpaceGraph.StateSpaceGraphVM) {
    (<any>ko.utils.domNodeDisposal).cleanExternalData = function () { };
    ko.cleanNode(div);
    ko.applyBindings(crnSS, div);
}
