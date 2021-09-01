// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from 'knockout';
import * as template from 'raw-loader!../Templates/dropdown.html';

var allowCloseTypes = ["button", "checkbox", "hidden", "radio", "reset", "search", "submit"];

ko.components.register("dropdown", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var $_elem = $(componentInfo.element);
            // I need to wait until the descendants are loaded, before setting up the drop down menu.
            (<any>ko).bindingEvent.subscribe(componentInfo.element, 'descendantsComplete', function (node: any) {
                // Hide sub-menus.
                $('dropdowncontent', $_elem).hide();
                // Slide out the drop down any time the mouse is over it. Slide it up when it's not.
                var $li = $('dropdowncontent', $_elem);
                $_elem.focusout(function (this: JQuery) {
                    $('dropdowncontent', this).stop().slideUp(200);
                });
                $_elem.focusin(function (this: JQuery) {
                    $('dropdowncontent', this).stop().slideDown(200);
                });
                $_elem.hover(function (this: JQuery) {
                    $('dropdowncontent', this).stop().slideDown(200);
                },
                    function (this: JQuery) {
                        // If there are any text inputs within the control, then I don't want to close it just because the user moves the mouse away. Generally speaking, there's a set of input types for which I will allow closing on hover out, and I'll leave it open if an input of any other type has focus. In that case, it will close on focusout, i.e. when the user clicks outside the drop down.
                        var focused = $("*:focus", this).toArray();
                        var canClose = !focused.some(el => {
                            var type: string = (<HTMLInputElement>el).type;
                            if (type == null)
                                return false;
                            return allowCloseTypes.indexOf(type) < 0;
                        });
                        if (canClose)
                            $('dropdowncontent', this).stop().slideUp(200);
                    });
            });

            // Finally, pass the parent VM as this component's VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});
