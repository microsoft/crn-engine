// (FP) This file implements a control that allows selection of a species from a list of species.

import * as ko from "knockout";
import * as I from "./Interfaces";
import * as template from 'raw-loader!../Templates/singlefilter.html';
ko.components.register("single-species-filter-view", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

// This object is the VM for one simulation plottable. It only contains three implicit fields, representing the display name, the instance id and the plottable index within the instance. Note that they are simple fields, not observables; this because the plottable index and ID of an instance never change (when the list is updated, the entire object is discarded and replaced).
class SpeciesVM {
    constructor(public Instance: number, public Species: number, public Name: string) { }
}

// This object is the VM for the list of plottables. It contains an observable array of species VMs (see above), and an observable for the selected species. The other fields are computed observables that depend on them.
class SpeciesFilterVM {
    // This is where the list of SpeciesVMs gets stored.
    public Species: KnockoutObservableArray<SpeciesVM> = ko.observableArray([]);
    // This computed maps the list above to their names (handy for binding in HTML).
    public SpeciesNames: KnockoutComputed<string[]> = ko.pureComputed(() => {
        return this.Species().map(val => val.Name);
    });

    // This is where the currently selected plottable is stored.
    public EffectiveSpeciesName: KnockoutObservable<string> = ko.observable("");
    // This computed maps the selected species to its instance ID.
    public EffectiveInstance: KnockoutComputed<number>;
    // This computed maps the selected species to its index within the instance.
    public EffectiveSpecies: KnockoutComputed<number>;

    constructor() {
        this.EffectiveInstance = ko.pureComputed(() => {
            var name = this.EffectiveSpeciesName();
            var allSpecies = this.Species();
            for (let species of allSpecies)
                if (species.Name == name)
                    return species.Instance;
            return -1;
        });
        this.EffectiveSpecies = ko.pureComputed(() => {
            var name = this.EffectiveSpeciesName();
            var allSpecies = this.Species();
            for (let species of allSpecies)
                if (species.Name == name)
                    return species.Species;
            return -1;
        });
    }
}

export class View implements I.ITracesFilter {
    private vm: SpeciesFilterVM;

    constructor() {
        this.vm = new SpeciesFilterVM();
    }

    // (FP) This function constructs the control in the given Element.
    bind(elementToBindTo: HTMLElement) {
        if (elementToBindTo == null)
            throw "Attempt to bind SingleSpeciesFilter to null";
        ko.applyBindings(this.vm, elementToBindTo);
    }

    public GetSelectedInstance(): KnockoutComputed<number> {
        return this.vm.EffectiveInstance;
    }
    public GetSelectedSpecies(): KnockoutComputed<number> {
        return this.vm.EffectiveSpecies;
    }

    // The outside world can call this to set the list of species (in practice, the overall Viewer will receive instances from the source of simulation events, and use this to pass them to this control).
    public SetAvailableTraces(data: I.ITraceDefinitions) {
        var vms = [];
        for (let crn of data.CRNs) {
            for (let settings of crn.Settings) {
                for (let sweep of settings.Sweeps) {
                    for (let instance of sweep.Instances) {
                        for (let spIdx = 0; spIdx < instance.Plottables.length; spIdx++) {
                            var species = instance.Plottables[spIdx];
                            var spName = species.Name;
                            if (instance.Name != null && instance.Name != "")
                                spName = instance.Name + "." + spName;
                            if (sweep.Name != null && sweep.Name != "")
                                spName = sweep.Name + "." + spName;
                            if (settings.Name != null && settings.Name != "")
                                spName = settings.Name + "." + spName;
                            if (crn.Name != null && crn.Name != "")
                                spName = crn.Name + "." + spName;
                            var vm = new SpeciesVM(instance.ID, spIdx, spName);
                            vms.push(vm);
                        }
                    }
                }
            }
        }
        this.vm.Species(vms);
    }

    public AddSpeciesToInstance(data: I.IAdditionalPlottable) {
        var vm = this.vm;
        var maxSpecies: SpeciesVM = null;
        for (let svm of vm.Species())
            if (svm.Instance == data.Instance)
                if (maxSpecies == null || maxSpecies.Species < svm.Species)
                    maxSpecies = svm;
        vm.Species.push(new SpeciesVM(data.Instance, maxSpecies == null ? 0 : maxSpecies.Species + 1, data.Name));
    }
}