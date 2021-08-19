import * as serialization from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";

export class SVGStructuralCache  {
    private speciesCache: { [key: string]: serialization.SpeciesAttributes } = {}; 
    constructor() {
    }

    add(name: string, structure: string, svg: string): void {
        if (this.speciesCache[name] == undefined) {
            this.speciesCache[name] = {
                name: name,
                structure: structure,
                svg: svg
            };
        } else {
            this.speciesCache[name].structure = structure;
            this.speciesCache[name].svg = svg;
        } 
    }

    tryGet(name: string): serialization.SpeciesAttributes {
        return this.speciesCache[name];
    }
}