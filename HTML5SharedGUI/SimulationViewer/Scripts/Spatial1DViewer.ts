import * as $ from "jquery";
import * as Rx from "rx";
import * as Knockout from "knockout";
import * as Framework from "./SimulationViewerFramework";
import * as TimeseriesView from "./Spatial1DTimeseriesView";
import * as TableView from "./Spatial1DTableView";
import * as Settings from "./SpatialViewerSettings";
import "jqueryui";
import * as template from 'raw-loader!../html/Spatial1DTemplate.html';
declare var InteractiveDataDisplay: any;
import IDDTabs from "../../GenericComponents/Scripts/IDDTabs";

class Viewer extends Framework.FilteredViewCollection {
    private timeseries: TimeseriesView.Viewer;
    private table = new TableView.View();

    constructor(settings: Framework.ISpatialViewerSettings) {
        super();
        this.timeseries = new TimeseriesView.Viewer(settings);
    }

    private AreViewsSubscribedToFilters = false;
    private SubscribeViewsToFilters() {
        if (this.AreViewsSubscribedToFilters)
            return;
        this.AreViewsSubscribedToFilters = true;

        //Adding viewes so they will receive updates
        this.AddView(this.timeseries);
        this.AddView(this.table);
    }

    // (FP) "bind" constructs the control (tabs and all) within the given Element.
    public bind(elem: HTMLElement) {
        if (elem == null)
            throw "Attempt to bind Spatial1DViewer to null";
        this.SubscribeViewsToFilters();
        var $_elem = $(elem);

        $_elem.empty(); //clearing all children
        $_elem.append(template); //setting up basic simulation viewer layout.
        IDDTabs($_elem);

        //binding views to the corresponding DOM elements//binding views to the corresponding DOM elements
        this.timeseries.Bind($("spatial-simulation-1d", $_elem).get(0));
        this.table.Bind($("spatial-1d-table-view", $_elem).get(0));
    }
}

export default Viewer;