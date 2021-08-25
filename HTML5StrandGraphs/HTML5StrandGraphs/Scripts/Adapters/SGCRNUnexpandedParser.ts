// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as GenericDSDParser from '../../../../HTML5SharedGUI/HTML5DSD_Generic/Scripts/Adapters/GenericDSDParser';
import SG from '../../../../SiteGraphReactor/SiteGraphReactorTSWrapper/Scripts/StrandGraphs';
import Options from '../../../../HTML5SharedGUI/GenericComponents/Scripts/Options';

//Uses ModellingEngine.ts layer to aquire parsed CRN code
export class Parser extends GenericDSDParser.Parser<void, Options> {
    constructor(private sg: SG) {
        super();
    };

    failureHandler(failure: any) {
        this.sg.Abort();
    };

    getCRNStreams(code: string): GenericDSDParser.IResultStreams<void> {
        return this.sg.UserParse(code);
    };
}