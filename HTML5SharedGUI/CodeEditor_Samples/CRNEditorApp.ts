// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import CRNEditor from "./CRNEditor";
import * as ko from "knockout";
import "../CodeEditor/Styles/Codepad.css";

var editor = new CRNEditor();
ko.applyBindings();
