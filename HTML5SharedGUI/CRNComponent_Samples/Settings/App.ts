import * as crnKO from "../../CRNComponent/Scripts/crnKO";
import * as crnVM from "../../CRNComponent/Scripts/crnVM";
import sampleCRN from "../SampleCRN";
import * as ko from "knockout";
import * as externalSettingTemplate from 'raw-loader!./externalSettingTemplate.html';
import "../samples.css";
import "../../CRNComponent/Styles/crn.css";

class MyExternalSetting extends crnVM.ExternalSetting {
    constructor(owner: crnVM.CRN) {
        super(owner);
        this.template(externalSettingTemplate);
    }

    numberSetting = ko.observable<number>(1);
    possibleOptions = ["First", "Second"];
    chosenValue = ko.observable<string>(this.possibleOptions[0]);
}

var crn = sampleCRN.SelectedCRN();
var externalSetting = new MyExternalSetting(crn);
crn.externalSettings.push(externalSetting);

crnKO.bind(<HTMLDivElement>document.getElementById("crnsettings"), sampleCRN);