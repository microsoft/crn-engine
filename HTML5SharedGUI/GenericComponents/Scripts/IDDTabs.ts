// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This module uses jQuery's tab functionality to set up any element with the j-has-tabs class as a set of tabs. It also ensures that any IDD content is properly resized when the tab size changes.
import * as $ from "jquery"
import "idd";
declare var InteractiveDataDisplay: any;

function setup(context?: Element | JQuery, onActivate?: (panel: JQuery) => void) {
    $(".j-has-tabs", context).tabs({
        heightStyle: "fill",
        activate: (event: any, ui: any) => {
            var newPanel = ui.newPanel;
            var oldPanel = ui.oldPanel;

            if (oldPanel) {
                oldPanel.trigger("deactivate");
            }
            newPanel.trigger("activate");

            if (ui.newPanel.length > 0 && !(<any>ui.newPanel[0]).fresh) {
                if (ui.newPanel.is(".ui-tabs")) {
                    ui.newPanel.tabs("refresh");
                    (<any>ui.newPanel[0]).fresh = true;
                }
                else
                    $(".ui-tabs", ui.newPanel.has(".ui-tabs")).tabs("refresh");
                var subTabs = newPanel.find(".ui-tabs");
                subTabs.tabs("refresh");
            }

            InteractiveDataDisplay.updateLayouts($(newPanel));
            if (onActivate != null)
                onActivate($(newPanel))
        }
    });
}

// This function performs some layout operations that need to happen when the window changes size. When the window size changes, tab controls that are currently visible need to have their height recalculated. Tab controls that are NOT currently visible also need to have their height recalculated, but this cannot be done now (they are not laid out), so I'm setting a flag on them to remind me to recalculate their height the next time they become visible. This is a bit hacky and I did it as a proof-of-concept that tabbing can be optimised. If it is confirmed to be working properly, we can engineer a less hacky solution. For reference, the previous solution was to always recalculate the height of tabs as they get selected; this was wasteful because in the majority of cases the window was not resized, therefore the height would be the same as it was before anyway. This is an issue because the height calculation can take a very long time with models that have lots of elements in the Export tab.
var refreshTabs = function () {
    $(".ui-tabs").each((i, e: HTMLElement) => {
        // Note that offsetParent is null any time the element or any of its parents have display: none.
        if (e.offsetParent == null)
            (<any>e).fresh = false;
        else {
            $(e).tabs("refresh");
            InteractiveDataDisplay.updateLayouts($(e));
            (<any>e).fresh = true;
        }
    });
}
$(window).resize(refreshTabs);
refreshTabs();

export default setup;
