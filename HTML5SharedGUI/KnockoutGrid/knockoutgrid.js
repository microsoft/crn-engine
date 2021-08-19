define(['jquery', 'knockout'], function ($, ko) {
    // Private function
    function tableViewModel(params) {
        this.gridViewModel = new ko.simpleGrid.viewModel(params.config);
    };

    if (!ko.components.isRegistered('table-viewer')) {
        ko.components.register('table-viewer', { viewModel: tableViewModel, template: '<div class="tableViewerDiv" data-bind="simpleGrid: gridViewModel"></div>' })
    }

    ko.simpleGrid = {
        // Defines a view model class you can use to populate a grid
        viewModel: function (configuration) {
            var self = this;

            this.ViewModel = configuration.ViewModel || {};
            this.data = configuration.data;
            this.currentPageIndex = ko.observable(0);
            this.pageSize = ko.observable(0);
            this.showAll = configuration.showAll || false;
            if (!this.showAll)
                this.pageSize(25);

            this.columnTemplate = configuration.columnTemplate || "";
            this.headerTemplate = configuration.headerTemplate || "";
            this.lastPageOnChange = configuration.last || false;

            this.selectedRow = configuration.selected || ko.observable();

            // Note that when adding rows to the table, each row gets a focusin event. Each focusin event causes a style change in the row, which in turn causes layout. If many rows are being added at the same time, this can cause performance issues. In order to resolve this, I'm running the effects of selection in a timeout that gets canceled if selection changes again. This way, selection will only actually change when control returns to the page.
            this.selectRowTimeoutHandle = 0;
            this.selectRow = function (data) {
                if (this.selectRowTimeoutHandle !== 0)
                    clearTimeout(this.selectRowTimeoutHandle);
                this.selectRowTimeoutHandle = setTimeout(() => {
                    if (this.selectedRow() !== data)
                        this.selectedRow(data);
                    this.selectRowTimeoutHandle = 0;
                });
            };

            this.setLastPage = function () {
                self.currentPageIndex(Math.ceil(ko.unwrap(this.data).length / this.pageSize()) - 1);
            };

            this.itemsOnCurrentPage = ko.computed(function () {
                if (this.showAll)
                    return this.data();
                var startIndex = this.pageSize() * this.currentPageIndex();
                return ko.unwrap(this.data).slice(startIndex, startIndex + this.pageSize());
            }, this);

            this.maxPageIndex = ko.computed(function () {
                return Math.ceil(ko.unwrap(this.data).length / this.pageSize()) - 1;
            }, this);

            this.startRange = ko.computed(function () {
                if (this.currentPageIndex() === 0) {
                    return this.currentPageIndex() + 1;
                } else {
                    return (this.currentPageIndex() * this.pageSize()) + 1;
                }
            }, this);

            this.endRange = ko.computed(function () {
                if (this.currentPageIndex() === 0) {
                    if (this.pageSize() < ko.unwrap(this.data).length) {
                        return this.pageSize();
                    } else {
                        return ko.unwrap(this.data).length;
                    }
                } else if (this.currentPageIndex() === this.maxPageIndex()) {
                    return ko.unwrap(this.data).length;
                } else {
                    return (this.currentPageIndex() + 1) * this.pageSize();
                }
            }, this);

            this.data.subscribe(function (newVal) {
                if (self.lastPageOnChange) {
                    self.setLastPage();
                } else {
                    if (self.itemsOnCurrentPage().length === 0)
                        self.setLastPage();
                }
                self.selectRow(null);
            });

            this.sizes = ko.observableArray([10, 25, 50, 100]);
        }
    };

    // Templates used to render the grid
    var templateEngine = new ko.nativeTemplateEngine();

    templateEngine.addTemplate = function (templateName, templateMarkup) {
        $('head').append("<script type='text/html' id='" + templateName + "'>" + templateMarkup + "<" + "/script>");
    };

    templateEngine.addTemplate("ko_simpleGrid_grid", "\
                    <table class=\"table table-bordered\" cellspacing=\"0\">\
                        <thead>\
                            <tr class='grid-header'>\
								<!-- ko if: headerTemplate !== '' -->\
									<!-- ko with: ViewModel -->\
									<!-- ko template: $parent.headerTemplate--><!-- /ko-->\
									<!-- /ko -->\
								<!-- /ko-->\
                            </tr>\
                        </thead>\
                        <tbody>\
                            <!-- ko foreach: itemsOnCurrentPage -->\
                            <tr tabindex='0' data-bind='event: {focusin: $parent.selectRow($data)}, css: {rowSelected: $parent.selectedRow() === $data}'>\
								<!-- ko template: $parent.columnTemplate -->\
								<!-- /ko -->\
                            </tr>\
                            <!-- /ko--> \
                        </tbody>\
                    </table>");

    templateEngine.addTemplate("ko_simpleGrid_pageLinks", "\
                    <div class=\"ko-grid-pageLinks\">\
						<span>Items on page:\
						<select data-bind=\"options: sizes, value: pageSize, event: {change: setLastPage}\"></select>\
						</span>\
                        <span>Page:</span>\
                        <!-- ko foreach: ko.utils.range(0, maxPageIndex) -->\
                               <a href=\"#\" data-bind=\"text: $data + 1, click: function() { $root.currentPageIndex($data) }, css: { selected: $data == $root.currentPageIndex() }\">\
                            </a>\
                        <!-- /ko -->\
                    </div>");

    // The "simpleGrid" binding
    ko.bindingHandlers.simpleGrid = {
        init: function () {
            return { 'controlsDescendantBindings': true };
        },
        // This method is called to initialize the node, and will also be called again if you change what the grid is bound to
        update: function (element, viewModelAccessor, allBindings) {
            var viewModel = viewModelAccessor();

            // Empty the element
            while (element.firstChild)
                ko.removeNode(element.firstChild);

            // Allow the default templates to be overridden
            var gridTemplateName = allBindings.get("simpleGridTemplate") || "ko_simpleGrid_grid",
                pageLinksTemplateName = allBindings.get("simpleGridPagerTemplate") || "ko_simpleGrid_pageLinks";


            // Render the page links
            if (!viewModel.showAll) {
                var pageLinksContainer = element.appendChild(document.createElement("DIV"));
                ko.renderTemplate(pageLinksTemplateName, viewModel, { templateEngine: templateEngine }, pageLinksContainer, "replaceNode");
            }

            // Render the main grid
            var gridContainer = element.appendChild(document.createElement("DIV"));
            ko.renderTemplate(gridTemplateName, viewModel, { templateEngine: templateEngine }, gridContainer, "replaceNode");

        }
    };
});
