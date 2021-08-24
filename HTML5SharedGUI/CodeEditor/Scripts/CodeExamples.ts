///<reference path="./Interfaces.ts"/>

// (FP) The code in this file handles the logic related to examples (i.e. the files you can load via the drop-down menu).

import * as ko from "knockout";
import * as Rx from "rx";

// The HTML template for the drop box.
import * as examplesTemplate from "raw-loader!./../Templates/code-examples-template.html";

// (FP) This class handles the binding with the drop box. KO takes care of selecting the example (as KO supports drop boxes), but we have to add extra logic to cause that action to change the text in the editor.
class CodeExamplesViewModel {
    customValue = "Examples";
    optionValues = [this.customValue];
    examples = ko.observable<ExamplesGroup[]>([]);
    selectedOptionText: KnockoutObservable<SelectedExample | string> = ko.observable(this.customValue);
    selectedOptionValue: KnockoutComputed<SelectedExample | string>;
    // (FP) The code editor text can be changed because the user typed something, or because we're loading an example. In the former case, I need to  reset the example selection. So I need a flag to remember which direction the current change is coming from.
    textLoading: boolean = false;
    constructor(examples: ExamplesGroup[], text: KnockoutObservable<string>, loadEvents: Rx.IObserver<string>) {
        // (FP) Load the examples into the options.
        var i: number = 1;
        this.examples(examples);
        var arr = examples;
        for (var k in arr)
            this.optionValues[i++] = k;
        var that = this;
        // (FP) Subscribe to the text changing from the outside, in order to reset the selection.
        text.subscribe(function (value) {
            if (!that.textLoading) {
                that.selectedOptionValue(that.customValue);
            }
        });
        // (FP) Insert extra logic in the drop box binding to load the example text.
        // Design note: this is done by having the binding be a Computed, and mapping it to a regular binding. Another way would be to use a regular binding directly, subscribe to it, and run the logic in the subscription handler. This is also fine.
        this.selectedOptionValue = ko.computed({
            read: function (this: CodeExamplesViewModel) {
                return this.selectedOptionText();
            },
            write: function (this: CodeExamplesViewModel, value: SelectedExample | string) {
                this.selectedOptionText(value);
                if (value && (value != that.customValue)) {
                    if (typeof value !== "string") {
                        var example = <SelectedExample>value;
                        that.textLoading = true;
                        // (FP) Load the example into the text; note that this is an asynchronous operation.
                        // Note that fetch does not work on IE.
                        /*fetch("../../" + (<SelectedExample>value).Group.Correspondence[(<SelectedExample>value).Name]).then(response => response.text()).then(result => {
                            text(result); that.textLoading = false;
                        });*/
                        var oReq = new XMLHttpRequest();
                        oReq.onload = () => {
                            text(oReq.responseText);
                            that.textLoading = false;
                            loadEvents.onNext(example.Name);
                        };
                        // NOTE: the URLs here are supposed to be resolved relativly to the HTML page that runs the current script. Usually it is ($app_root)/index.html
                        oReq.open("GET", example.Group.Correspondence[example.Name]);
                        oReq.send();
                    }
                }
            },
            // "owner" ensures that "this" has the right meaning inside the handlers.
            owner: this
        });
    }
}

// (FP) This tells KO how to make a CodeExamples.
class ExamplesViewerConfig implements KnockoutComponentTypes.Config {
    get template(): string {
        return examplesTemplate;
    }
    get viewModel(): KnockoutComponentTypes.ViewModelFactoryFunction {
        var that = this;
        return {
            createViewModel: function (params, componentInfo) {
                // (FP) params.text contains the Observable that's bound to the code editor text (so that the examples box can use it to set the text). params.correspondence contains the actual examples. I'm not sure why it has that name.
                var vm = new CodeExamplesViewModel(params.correspondence, params.text, params.loadEvents);
                return vm;
            }
        }
    }
}

if (!ko.components.isRegistered("code-examples"))
    ko.components.register("code-examples", new ExamplesViewerConfig());