// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as tinycolor from "tinycolor2";
import * as ko from 'knockout';
import * as template from 'raw-loader!../Templates/paletteeditor.html';
import "idd";
declare var InteractiveDataDisplay: any;

ko.components.register("paletteeditor", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var $_elem = $(componentInfo.element);
            var context = ko.contextFor(componentInfo.element);
            if (context == null)
                return {}
            var editor: PaletteEditor = context.$data;
            editor.bind($_elem);
            return editor;
        }
    }
});

class PaletteEditor {
    public IDDPalette: KnockoutObservable<any> = ko.observable(InteractiveDataDisplay.ColorPalette.parse("black,green"));
    public LogColors: KnockoutObservable<boolean> = ko.observable(false);

    public bind($_elem: JQuery) {
        var $btnEditPalette = $(".j-edit-palette", $_elem);
        $btnEditPalette.click(function () {
            var $editor = $(".c-heatmap-palette", $_elem);
            if ($editor.is(":visible")) {
                $editor.toggle("slide");
                $btnEditPalette.text("Edit palette");
            } else {
                $editor.toggle("slide");
                $btnEditPalette.text("Hide editor");
            }

        });
        var paletteEditor = new InteractiveDataDisplay.ColorPaletteEditor($(".c-heatmap-palette", $_elem), this.IDDPalette());
    }

    public onPaletteChanged(self: PaletteEditor, event: Event, arg: any) {
        self.IDDPalette(arg);
    }
}

export default PaletteEditor;
