import * as crnKO from "../../CRNComponent/Scripts/crnKO";
import * as crnVM from "../../CRNComponent/Scripts/crnVM";
import sampleCRN from "../SampleCRN";
import "../samples.css";

var svgs: string[] = [];

var graph: crnVM.InferenceGraph = sampleCRN;
var model = graph.SelectedNode();
var crn = model.Top();
for (var i = 0; i < crn.initials().length; i++) {
    svgs.push(crn.initials()[i].svg());
    crn.initials()[i].svg(null);
}
graph.setDefaultOptions();

crnKO.bind(<HTMLDivElement>document.getElementById("reactions"), graph);

for (var i = 0; i < crn.initials().length; i++)
    crn.initials()[i].svg(svgs[i]);