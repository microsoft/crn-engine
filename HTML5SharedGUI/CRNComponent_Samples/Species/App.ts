import * as crnKO from "../../CRNComponent/Scripts/crnKO";
import * as crnVM from "../../CRNComponent/Scripts/crnVM";
import sampleCRN from "../SampleCRN";
import "../samples.css";

crnKO.bind(<HTMLDivElement>document.getElementById("species"), sampleCRN);