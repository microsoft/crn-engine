import * as $ from 'jquery';
import * as ko from 'knockout';
import * as CRNvm from './crnVM';
import * as CRNsettings from './crnSettings';

class ViewModel {
    public sortedList = ko.observableArray<string>([]);
    public selected = ko.observable<string>('');

    public Add = () => {
        this.sortedList.push(this.selected());
    }

    public Remove = (data: string) => {
        this.sortedList.remove(data);
    }

    public available = ko.pureComputed<string[]>(() => {
        let list = this.fullList().filter((val) => { return this.sortedList().indexOf(val) === -1 });
        return list;
    });

    constructor(private fullList: KnockoutObservableArray<string>) {
    }
}

import * as sortableFilenamesListTemplate from 'raw-loader!../HTML/sortable-filenames-list.html';

/** Control that enables the user to compose a list of file names and to sort them. */
export class SortableFilenamesList {
    private viewModel: ViewModel;

    constructor(fullList: KnockoutObservableArray<string>, private model: CRNvm.InferenceGraph) {
        this.viewModel = new ViewModel(fullList);

        // When the list is changed by the user, I want to update the currently selected CRN with the new file list.
        this.viewModel.sortedList.subscribe(names => {
            var ds = names.map(n => new CRNsettings.DataSet({ file: n, data: [] }));
            if (names.length != this.model.SelectedCRN().settings.Data().length || !names.every((n, i) => n == this.model.SelectedCRN().settings.Data()[i].name))
                this.model.SelectedCRN().settings.Data(ds);
        });
        // When the file list changes in the currently selected CRN (or when a different CRN is selected), I want to update the visible list.
        ko.computed(() => {
            var names = this.model.SelectedCRN().settings.Data().map(d => d.name);
            if (names.length != this.viewModel.sortedList().length || !names.every((n, i) => n == this.viewModel.sortedList()[i]))
                this.viewModel.sortedList(names);
        });

        if (!ko.components.isRegistered("sortable-data-set"))
            ko.components.register("sortable-data-set", {
                viewModel: {
                    createViewModel: (params, componentInfo) => {
                        var context = ko.contextFor(componentInfo.element);
                        return context === null ? {} : context.$data;
                    }
                },
                template: sortableFilenamesListTemplate
            });

        ko.bindingHandlers['uiSortableList'] = {
            init: function (element, valueAccessor, allBindingsAccesor, context) {
                var $element = $(element),
                    list = valueAccessor();

                $element.sortable({
                    update: function (event, ui) {
                        var item = ko.dataFor(ui.item[0]),
                            newIndex = ko.utils.arrayIndexOf(<any>(ui.item.parent().children()), ui.item[0]);
                        if (newIndex >= list().length) newIndex = list().length - 1;
                        if (newIndex < 0) newIndex = 0;

                        ui.item.remove();
                        list.remove(item);
                        list.splice(newIndex, 0, item);
                    }
                });
            }
        };
    }

    Bind(div: HTMLElement) {
        ko.cleanNode(div);
        ko.applyBindings(this.viewModel, div);
    }
}