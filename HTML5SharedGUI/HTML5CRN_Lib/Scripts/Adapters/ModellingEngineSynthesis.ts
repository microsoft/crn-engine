import * as SynthesisOperation from '../Operations/Synthesis';
import ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import * as MeInterfaces from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as $ from 'jquery';
import * as Rx from 'rx';

/**
 * Adapts CRNEngine to SynthesisOperation.ISynthesisEngine
 */
class SynthesisAdapter implements SynthesisOperation.ISynthesisEngine {
    public constructor(private me: ME) { }

    public Synthesize(ig: MeInterfaces.IG, nodeId: string, crnId: string): JQueryPromise<MeInterfaces.SynthesisResult> {
        var result = jQuery.Deferred<MeInterfaces.SynthesisResult>();
        var analysis = this.me.UserSynthesis(ig, nodeId, crnId);
        analysis.subscribe(update => {
            if (result.state() == "pending") {
                result.resolve(update);
            }
            else {
                console.warn("result promise is already \"" + result.state() + "\". Has ME synthesis behavior changed?");
            }
        }, error => {
            console.error(JSON.stringify(error));
            result.reject(JSON.stringify(error));
        },
            () => { });
        
        return result;
    }

    public Abort(): void {
        this.me.Abort();
    }
}

export default SynthesisAdapter