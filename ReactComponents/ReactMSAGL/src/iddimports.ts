// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This file forces some IDD prerequisites to be loaded as globals, even though TypeScript loads them as modules. IDD wants them as globals.
import jQuery from 'jquery';
import Rx from 'rx';
import { saveAs } from 'file-saver';
import "jquery-mousewheel";
(window as any)["$"] = jQuery;
(window as any)["jQuery"] = jQuery;
(window as any)["Rx"] = Rx;
(window as any)["filesaver"] = saveAs;
