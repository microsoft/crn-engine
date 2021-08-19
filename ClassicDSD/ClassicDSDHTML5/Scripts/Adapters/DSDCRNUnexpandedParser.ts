import * as GenericDSDParser from '../../../../HTML5SharedGUI/HTML5DSD_Generic/Scripts/Adapters/GenericDSDParser';
import DSD from '../../../ClassicDSDTSWrapper/Scripts/ClassicDSD';
import * as Interfaces from '../../../ClassicDSDTSWrapper/Scripts/Interfaces';
import DSDParsingOptions from '../DSDParsingOptions';

//Uses ModellingEngine.ts layer to aquire parsed CRN code
export class Parser extends GenericDSDParser.Parser<Interfaces.DsdSettings, DSDParsingOptions> {
    constructor(private dsd: DSD) {
        super();
    };

    failureHandler(failure: any) {
        this.dsd.Abort();
    };

    getCRNStreams(code: string, options: DSDParsingOptions): GenericDSDParser.IResultStreams<Interfaces.DsdSettings> {
        return this.dsd.UserParse(code, options.oldSyntax);
    };
}