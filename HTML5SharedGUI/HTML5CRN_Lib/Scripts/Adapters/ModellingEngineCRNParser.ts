import * as GenericCRNParser from '../Adapters/GenericCRNParser';
import ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as jQuery from 'jquery';

/** Uses ModellingEngine.ts layer to acquire a parsed CRN. */
export class Parser extends GenericCRNParser.Parser<void, void> {
    constructor(private me: ME) {
        super();
    };

    failureHandler() {
        this.me.Abort();
    };

    getModelStreams(code: string, options: void): GenericCRNParser.IModelStreams<void> {
        var meStreams = this.me.UserParseCode(code);
        // Coercing format to GenericCRNParser.ICRNStreams.
        return { model: meStreams.model };
    };
}