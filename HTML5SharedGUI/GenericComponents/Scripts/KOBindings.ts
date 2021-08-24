// This file contains general-purpose additional KO bindings.

import * as ko from 'knockout';

var parser = new DOMParser();

// Set up a ViewModel factory that returns the current context. This is for when the factory is not supposed to create a new VM, because the VM is supplied from the outside (i.e. as a parameter to applyBindings).
var InheritedVMFactory = {
    createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
        var context = ko.contextFor(componentInfo.element);
        return context === null ? {} : context.$data;
    }
};

// Register an "element" binding that returns the bound value as a DOM element.
if ((<any>ko.bindingHandlers).element == null)
    (<any>ko.bindingHandlers).element = {
        update: function (element: any, valueAccessor: () => any) {
            var elem = ko.utils.unwrapObservable(valueAccessor());
            $(element).empty();
            $(element).append(elem);
        }
    }

// Register a custom binding that sets the color of the element to red.
if ((<any>ko.bindingHandlers).error == null)
    (<any>ko.bindingHandlers).error = {
        update: function (element: any, valueAccessor: () => any) {
            var err = valueAccessor()();
            if (err)
                $(element).css('background-color', '#FF9999');
            else
                $(element).css('background-color', 'transparent');
        }
    }

// Register a custom binding, named 'svg', that is able to turn a string into actual SVG elements.
if ((<any>ko.bindingHandlers).svg == null)
    (<any>ko.bindingHandlers).svg = {
        init: function (element: any, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
        },
        update: function (element: HTMLElement, valueAccessor: () => any, allBindings?: KnockoutAllBindingsAccessor, viewModel?: any, bindingContext?: KnockoutBindingContext) {
            if (bindingContext.$data.svg() == null || bindingContext.$data.svg() == "")
                return;
            // Parse the string into a document.
            var doc = parser.parseFromString(bindingContext.$data.svg(), "image/svg+xml");

            // Clear out anything that's already in the element.
            while (element.childElementCount > 1)
                element.removeChild(element.lastChild);

            // Make a new SVG object.
            var svg = document.createElementNS("http://www.w3.org/2000/svg", 'svg');
            // Set the size of the SVG element (from the content).
            svg.setAttribute('width', doc.documentElement.getAttribute('width'));
            svg.setAttribute('height', doc.documentElement.getAttribute('height'));
            // Put it in the DOM.
            element.appendChild(svg);

            // Copy the element tree to the SVG tree. Direct assignment of the element does not work. I don't know why.
            function copyTree(source: any, dest: Element) {
                if (source.tagName === undefined) {
                    // It's a text node. Copy it.
                    var txt = document.createTextNode(source.textContent);
                    dest.appendChild(txt);
                }
                else {
                    // Create a new tag with the same type.
                    var el = document.createElementNS("http://www.w3.org/2000/svg", source.tagName);
                    // Copy all attributes.
                    for (var i = 0; i < source.attributes.length; i++)
                        el.setAttribute(source.attributes[i].name, source.attributes[i].value);
                    // Recurse on the children.
                    for (var i = 0; i < source.childNodes.length; i++) {
                        var child = <Element>source.childNodes[i];
                        copyTree(child, el);
                    }
                    dest.appendChild(el);
                }
            }
            copyTree(doc.documentElement, svg);
        }
    };

// Registers a custom binding that sets HTML content (like the standard html binding), and also applies KO bindings to the newly inserted HTML. The context for these bindings is the same.
if ((<any>ko.bindingHandlers).bindHTML == null)
    ko.bindingHandlers.bindHTML = {
        init: function () {
            return { controlsDescendantBindings: true };
        },
        update: function (element, valueAccessor, allBindings, viewModel, bindingContext) {
            var value = ko.utils.unwrapObservable(valueAccessor());
            ko.applyBindingsToNode(element, { html: value });
            ko.applyBindingsToDescendants(bindingContext, element);
        }
    };

// Registers an extender that forces a value to be numeric. Pass true in the extender constructor to specify that an empty string must be interpreted as null (rather than zero).
if ((<any>ko.extenders).numeric == null)
    (<any>ko.extenders).numeric = function (target: any, allowNull: boolean) {
        //create a writable computed observable to intercept writes to our observable
        var result = ko.pureComputed<number>({
            read: target,  //always return the original observables value
            write: function (newValue) {
                var current = target();
                var newValueAsNum = ((newValue == null || (<string><any>newValue) == "") && allowNull) ? undefined : +newValue;

                //only write if it changed
                if (newValueAsNum !== current) {
                    target(newValueAsNum);
                } else {
                    //if the rounded value is the same, but a different value was written, force a notification for the current field
                    if (newValue !== current) {
                        target.notifySubscribers(newValueAsNum);
                    }
                }
            }
        }).extend({ notify: 'always' });
        //initialize with current value to make sure it is rounded appropriately
        result(target());
        //return the new computed observable
        return result;
    };

// Registers an extender that converts a value between a semicolon-separated string an an array of strings. Pass true in the extender constructor to specify that values are numbers (rather than strings).
if ((<any>ko.extenders).array == null)
    (<any>ko.extenders).array = function (target: any, numeric: boolean) {
        //create a writable computed observable to intercept writes to our observable
        var result = ko.pureComputed<any>({
            read: target,
            write: function (newValue) {
                if (typeof newValue == "string") {
                    var s = <string>newValue;
                    var tokens = s.split(',').map(t => t.trim()).filter(t => t != "");
                    if (numeric)
                        target(tokens.map(t => parseFloat(t)));
                    else
                        target(tokens);
                }
                else
                    target(newValue);
            }
        }).extend({ notify: 'always' });
        //return the new computed observable
        return result;
    };
