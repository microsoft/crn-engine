import * as crnKO from "../../CRNComponent/Scripts/crnKO";
import * as crnVM from "../../CRNComponent/Scripts/crnVM";
import * as crnGraph from "../../CRNComponent/Scripts/crnGraphViewer";
import "../samples.css";
import "./styles.css";
import sampleCRN from "../SampleCRN";

$(window).on('load', function () {
    var model = sampleCRN.getSerializableForm();
    var vm = new crnVM.InferenceGraph(null);
    vm.fromSerializableForm(model);
    var graphVM = new crnGraph.CRNGraphVM(vm);

    crnKO.bindGraph(<HTMLDivElement>document.getElementById("graph"), graphVM);
});