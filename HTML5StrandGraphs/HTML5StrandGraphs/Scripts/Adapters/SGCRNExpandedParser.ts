// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as GenericExpandingParser from '../../../../HTML5SharedGUI/HTML5DSD_Generic/Scripts/Adapters/GenericExpandingParser';
import SG from '../../../../SiteGraphReactor/SiteGraphReactorTSWrapper/Scripts/StrandGraphs';
import Options from '../../../../HTML5SharedGUI/GenericComponents/Scripts/Options';

//Uses ModellingEngine.ts layer to aquire parsed CRN code
export class Parser extends GenericExpandingParser.ExpandingParser<void, Options> {
    constructor(private sg: SG) {
        super();
    };

    failureHandler(failure: any) {
        this.sg.Abort();
    };

    getCRNStreams(code: string): GenericExpandingParser.IResultStreams<void> {
        return this.sg.UserExpand(code);
    };
}