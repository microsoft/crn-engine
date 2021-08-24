import * as ko from 'knockout';
import * as template from 'raw-loader!../Templates/zoomslider.html';

ko.components.register("zoomslider", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

class ZoomSlider {
    constructor() {
        var that = this;
        this.Value = ko.computed({
            read: function () {
                var fValue = that.Step();
                if (fValue) {
                    var v = Math.exp(Math.log(that.StepSize()) * fValue);
                    return v;
                }
                else {
                    return 1.0;
                }
            },
            write: function (value) {
                var fValue = value;
                if (fValue) {
                    var v = Math.log(fValue) / Math.log(that.StepSize());
                    that.Step(v);
                }
            }
        });
        this.Percentage = ko.computed({
            read: function () {
                return (that.Value() * 100.0).toFixed(1);
            },
            write: function (value) {
                var fValue = parseFloat(value);
                that.Value(fValue / 100.0);
            }
        })
    }

    public Value: KnockoutComputed<number>;
    public Percentage: KnockoutComputed<string>;

    public Step: KnockoutObservable<number> = ko.observable(0);
    public StepSize: KnockoutObservable<number> = ko.observable(1.1);
}

export default ZoomSlider