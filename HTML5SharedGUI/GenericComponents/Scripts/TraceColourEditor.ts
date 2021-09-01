// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as tinycolor from "tinycolor2";
import * as ko from 'knockout';
import * as template from 'raw-loader!../Templates/tracecoloureditor.html';

ko.components.register("tracecoloureditor", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var $_elem = $(componentInfo.element);
            var context = ko.contextFor(componentInfo.element);
            return context == null ? {} : context.$data;
        }
    }
});

class TraceColourEditor {
    public Name: KnockoutComputed<string>;
    public Text: KnockoutObservable<string>;
    public Hex: KnockoutComputed<string>;
    public TransparentRGBA: KnockoutComputed<string>;

    public Regenerate() {
        this.Text("#" + tinycolor.random().toHex());
    }

    constructor(public DisplayName: KnockoutObservable<string>, initialColour: string) {
        this.Text = ko.observable(initialColour);
        this.Hex = ko.computed(() => {
            var parsed = tinycolor(this.Text());
            if (parsed.isValid())
                return "#" + parsed.toHex();
            else
                return "#" + tinycolor("black").toHex();
        });
        // The color for an area is the same as the line color, except it's partially transparent. This is made through a computed obsrevable.
        this.TransparentRGBA = ko.computed(() => {
            var c = tinycolor(this.Hex());
            c.setAlpha(0.2);
            return c.toRgbString();
        });
    }
}

export default TraceColourEditor;
