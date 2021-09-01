// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from "knockout";
import "idd";
declare var InteractiveDataDisplay: any;
import * as $ from "jquery";

export class Settings {
    // This is an idd color palette.
    public colorPalette: KnockoutObservable<any>;
    public isLogPalette: KnockoutObservable<boolean>;

    constructor(colorPalette: string, isLogPalette: boolean) {
        this.colorPalette = ko.observable(InteractiveDataDisplay.ColorPalette.parse(colorPalette));      
        this.isLogPalette = ko.observable(isLogPalette);          
    }
}

export class StoredSettings extends Settings {
    private static storage = window.localStorage;

    constructor(defaultColorPalette: string) {
        var initialPalette = StoredSettings.storage.getItem("SpatialViewerColorPalette");
        if (initialPalette == null) {
            initialPalette = defaultColorPalette;
        }

        var isLogPalette = StoredSettings.storage.getItem("SpatialViewerIsLogPalette") == "true";

        super(initialPalette, isLogPalette);

        this.colorPalette.subscribe(function (palette:any) {
            var paletteStr = palette.gradientString();
            StoredSettings.storage.setItem("SpatialViewerColorPalette", paletteStr);
        });

        this.isLogPalette.subscribe(function (isLog: boolean) {
            StoredSettings.storage.setItem("SpatialViewerIsLogPalette", isLog ? "true" : "false");
        });
    }
}
