// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import { Launch as Bootstrap } from '../../../HTML5SharedGUI/HTML5DSD_Generic/Scripts/GenericApp';
import { Parser as NotExpandingParser } from './Adapters/SGCRNUnexpandedParser';
import { Parser as ExpandingParser } from './Adapters/SGCRNExpandedParser';
import StrandGraphsLanguage from './StrandGraphsLang';
import StrandGraphs from '../../../SiteGraphReactor/SiteGraphReactorTSWrapper/Scripts/StrandGraphs';
import Options from '../../../HTML5SharedGUI/GenericComponents/Scripts/Options';

var examples: ExamplesGroup[] = [
    {
        // NOTE: the URLs here are supposed to be resolved relativly to the HTML page that runs the CodeEditor/CodePad.js script. Usually it is ($app_root)/index.html.
        // As the path to the code is $app_root/CodeExamples/* the relatice url here is CodeExamples/*
        Name: "Group 1",
        Correspondence: {
            "Catalytic": "CodeExamples/Catalytic.txt",
            "3-initiated, 4-way strand displacement": "CodeExamples/4-wayStrandReplacement.txt"
        }
    }
];

var engine = new StrandGraphs(Options.Server());
var options = new Options(engine);
var expandingParser = new ExpandingParser(engine);
var notExpandingParser = new NotExpandingParser(engine);

Bootstrap(<any>StrandGraphsLanguage, examples, notExpandingParser, expandingParser, engine, options, null);