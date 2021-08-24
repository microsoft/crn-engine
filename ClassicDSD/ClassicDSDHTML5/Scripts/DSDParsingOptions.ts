// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import ClassicDSD from '../../ClassicDSDTSWrapper/Scripts/ClassicDSD';
import Options from '../../../HTML5SharedGUI/GenericComponents/Scripts/Options';

class DSDParsingOptions extends Options {
    public constructor(private dsd: ClassicDSD) {
        super(dsd);
    }
    oldSyntax: boolean = false;
}

export default DSDParsingOptions