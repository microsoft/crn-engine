// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// (FP) This file contains code that binds together the plotter, bar chart and table view into a single component that shows
// all three viewers in tabs.

import * as $ from "jquery";
import * as Rx from "rx";
import * as Knockout from "knockout";
import * as Framework from "./SimulationViewerFramework";
import * as TimeseriesView from "./TimeseriesView";
import * as BarChartView from "./BarChartView";
import * as TableView from "./TableView";
import * as ProgressView from "./ProgressView";
import "jqueryui";
import * as template from 'raw-loader!../html/Template.html';
declare var InteractiveDataDisplay: any;
import IDDTabs from "../../GenericComponents/Scripts/IDDTabs";

export class Viewer extends Framework.FilteredViewCollection {
    private timeseries = new TimeseriesView.View();
    private barcharts = new BarChartView.View();
    private table = new TableView.View();
    private progress = new ProgressView.ProgressView();

    constructor() {
        super();
    }

    private AreViewsSubscribedToFilters = false;
    private SubscribeViewsToFilters() {
        if (this.AreViewsSubscribedToFilters)
            return;
        this.AreViewsSubscribedToFilters = true;

        //Adding viewers so they will receive updates
        this.AddView(this.timeseries);
        this.AddView(this.barcharts);
        this.AddView(this.table);
        this.AddView(this.progress);
    }

    // (FP) "bind" constructs the control (tabs and all) within the given Element.
    public bind(elem: HTMLElement) {
        if (elem == null)
            throw "Attempt to bind Viewer to null";
        this.SubscribeViewsToFilters();
        var $_elem = $(elem);

        $_elem.empty(); //clearing all children
        $_elem.append(template); //setting up basic simulation viewer layout.
        IDDTabs($_elem);

        //binding views to the corresponding DOM elements//binding views to the corresponding DOM elements
        this.timeseries.bind($("species-timeseries-view.c-sim-viewer__timeseries", $_elem).get(0));
        this.barcharts.bind($("species-barchart-view.c-sim-viewer__bars", $_elem).get(0));
        this.table.bind($("species-table-view.c-sim-viewer__table", $_elem).get(0));

        var progressDisplays = $("sim-progress-view", $_elem);
        for (var i = 0; i < progressDisplays.length; i++)
            this.progress.bind(progressDisplays.get(i));
    }

    // (FP) "autoBind" constructs the three individual control within a tabbed area that has already been provided (and can be found under the given Element).
    // Design note: this does not seem to be used outside the samples. We should consider removing it.
    public autoBind(elem: HTMLElement) {

        if ($("species-barchart-view.c-sim-viewer__bars", $(elem)).length !== 0) {
            this.barcharts = new BarChartView.View();
            this.AddView(this.barcharts);
            this.barcharts.bind($("species-barchart-view.c-sim-viewer__bars", $(elem)).get(0));
        }

        if ($("species-timeseries-view.c-sim-viewer__timeseries", $(elem)).length !== 0) {
            this.timeseries = new TimeseriesView.View();
            this.AddView(this.timeseries);
            this.timeseries.bind($("species-timeseries-view.c-sim-viewer__timeseries", $(elem)).get(0));
        }

        if ($("div.c-sim-viewer__table", $(elem)).length !== 0) {
            this.table = new TableView.View();
            this.AddView(this.table);
            this.table.bind($("div.c-sim-viewer__table", $(elem)).get(0));
        }
    }
}
