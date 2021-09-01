// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import CRNEditor from "./CRNEditor";
import * as ko from "knockout";
import "../CodeEditor/Styles/Codepad.css";

class TestVM {
    errors = ko.observableArray();
    constructor(){
        var errorTexts = [{ text: "some error 1" }, { text: "some error 2" }, { text: "another one error" }];
        this.errors(errorTexts);

        setInterval(() => {
            this.errors.push({ text: "another one" });
        }, 1000);
    }
}

var editor = new CRNEditor();
ko.applyBindings(new TestVM());
