import * as $ from 'jquery';
import * as ko from 'knockout';
import * as LongOperationsKO from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Operations/LongOperationsKO';

//extends LongOpsKO so it aware of CRN tab. IsCRNButtonEnabled is true only if DSD tab is active
export class SgOperationButtonsAvailability extends LongOperationsKO.LongOperationsKO {
    private isCalculusTabSelected = ko.observable(true);
    private isCrnTabSelected = ko.observable(false);

    public IsCRNButtonEnabled = ko.computed(() => {
        var canStartNewOp = this.CanStartNewAction();
        var isCalculusSelected = this.isCalculusTabSelected();
        return canStartNewOp && isCalculusSelected;
    });

    public IsExportButtonEnabled = ko.computed(() => {
        var canStartNewOp = this.CanStartNewAction();
        var isCrnTabSelected = this.isCrnTabSelected();
        return canStartNewOp && isCrnTabSelected;
    });

    public AreActionButtonsEnabled = ko.computed(() => {
        var canStartNewOp = this.CanStartNewAction();
        var isCrnOrCalculusTabSelected = this.isCrnTabSelected() || this.isCalculusTabSelected();
        return canStartNewOp && isCrnOrCalculusTabSelected;
    })

    public CRNTabActiveChanged(isCalculusTabSelected: boolean, isCrnTabSelected: boolean) {
        this.isCalculusTabSelected(isCalculusTabSelected);
        this.isCrnTabSelected(isCrnTabSelected);
    }
}