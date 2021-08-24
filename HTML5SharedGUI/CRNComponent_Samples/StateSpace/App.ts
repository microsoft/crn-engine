import * as crnKO from "../../CRNComponent/Scripts/crnKO";
import * as crnVM from "../../CRNComponent/Scripts/crnVM";
import sampleCRN from "../SampleCRN";
import * as crnStateSpace from "../../CRNComponent/Scripts/crnStateSpaceViewer";
import * as Interfaces from "./../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import "../samples.css";
import "./styles.css";

var stateSpace: Interfaces.StateSpace =
    {
        states:
        [
            { species: { "Species with a very long name": 100, "SP2": 1 }, transitions: [{ target: 1, propensity: "1.01234" }, { target: 2, propensity: "1.0" }] },
            { species: { "Species with a very long name": 2 }, transitions: [{ target: 0, propensity: "0.1234" }] },
            { species: { "SP2": 200 }, transitions: [{ target: 0, propensity: "0.1234" }] },
        ],
        start_index: 0,
        attributes: []
    };

let crn = sampleCRN.SelectedCRN();
var graphVM = new crnStateSpace.StateSpaceGraphVM(crn);
graphVM.stateSpace(stateSpace);

crnKO.bindStateSpace(<HTMLDivElement>document.getElementById("graph"), graphVM);