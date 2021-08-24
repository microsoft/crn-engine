import * as CRNViewer from '../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Adapters/KnockoutBasedCRNViewer';
import * as Cache from './SVGStructuralCache';
import * as serialization from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';

/**
 * Caches all SVG and structural representations using species names as a cache entry key.
 * For UpdateValuesWith call: Adds previously saved SVG and structural representations in case of their absence.
 */
export class CachingDecorator<TCustomSettings> {
    constructor(private cache: Cache.SVGStructuralCache, private viewer: CRNViewer.CRNEditor<TCustomSettings>) {
    }
    
    GetIsModified(): boolean {
        return this.viewer.GetIsModified();
    }

    UpdateValuesWith(update: serialization.IG, customSettings: TCustomSettings, fromJIT: boolean): void {
        if (update != null) {
            for (let m in update.nodes) {
                let model = update.nodes[m];
                let species = model.top.attributes;
                for (let key in species) {
                    let specie = this.cache.tryGet(species[key].name);
                    if (specie != undefined)
                        species[key] = specie;
                }
            }
        }

        this.viewer.UpdateValuesWith(update, customSettings, fromJIT);
    }
}