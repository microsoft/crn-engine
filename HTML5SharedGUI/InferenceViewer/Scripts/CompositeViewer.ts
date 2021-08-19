// (FP) This class is not actually a component (in the sense of implementing the behavior of a specific bit of GUI). It's just an object that routes a set of inference results to multiple other objects that want to make use of it (by implementing an interface). Then, in the app, you build one of these with all of your actual viewer, and you pass the results to this object. This is a much simpler version of the concept used in the simulation viewer, except that in that case there is also an actual GUI component (that aggregates the viewers).

import * as ko from "knockout";
import * as $ from "jquery";
import * as Rx from "rx";

import * as Inference from "./InferenceViewer";

// Represents a collection of IInferenceViewer as single IInferenceViewer
export class Viewer implements Inference.IInferenceViewer {

    constructor(private viewers: Inference.IInferenceViewer[]) { }
    
    show(run: Inference.IInferenceRun) {        
        this.viewers.forEach(v => v.show(run));
    }
}