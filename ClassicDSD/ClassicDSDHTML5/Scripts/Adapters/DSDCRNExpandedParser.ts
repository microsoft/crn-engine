// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as GenericExpandingParser from '../../../../HTML5SharedGUI/HTML5DSD_Generic/Scripts/Adapters/GenericExpandingParser';
import DSD from '../../../ClassicDSDTSWrapper/Scripts/ClassicDSD';
import * as Interfaces from '../../../ClassicDSDTSWrapper/Scripts/Interfaces';
import * as CrnInterfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import DSDParsingOptions from '../DSDParsingOptions';
import DSDDirectives from '../DSDDirectives';
import * as CrnEditor from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNViewer';
import * as CRNVM from '../../../../HTML5SharedGUI/CRNComponent/Scripts/crnVM';

//Uses ModellingEngine.ts layer to aquire parsed CRN code
export class Parser extends GenericExpandingParser.ExpandingParser<Interfaces.DsdSettings, DSDParsingOptions> {
    constructor(private dsd: DSD) {
        super();
    };

    failureHandler(failure: any) {
        this.dsd.Abort();
    };

    getCRNStreams(code: string, options: DSDParsingOptions, model: CrnInterfaces.IG, settings: Interfaces.DsdSettings): GenericExpandingParser.IResultStreams<Interfaces.DsdSettings> {
        return this.dsd.UserExpand(code, options.oldSyntax, model, settings);
    };
}