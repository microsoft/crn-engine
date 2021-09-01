// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This file implements a control that allows selecting traces. Traces are organised in a hierarchical structure of Model(CRN), Settings, Instance and Species. In this control, all such objects with the same name are collapsed in a single VM, which the user can select. For example, if we have two instances A and B each of which has species S1 and S2, then there will only be two species VMs, rather than 4. This means that the relationship between VMs and traces is many-to-many (i.e. an instance VM can be connected to multiple instances with the same name, and any given trace will be connected to a species, an instance, a settings and a model).

import * as ko from "knockout";
import * as $ from "jquery";
import "./Dropdown";
import * as I from "./Interfaces";
import * as template from 'raw-loader!../Templates/multifilter.html';
ko.components.register("multi-instances-filter-view", {
    template: template,
    viewModel: {
        createViewModel: function (params: any, componentInfo: KnockoutComponentTypes.ComponentInfo) {
            // Return the parent VM.
            var context = ko.contextFor(componentInfo.element);
            return context === null ? {} : context.$data;
        }
    }
});

function getOriginalTarget(event: any): Element {
    if (event.originalEvent.srcElement != null)
        return event.originalEvent.srcElement;
    return event.originalEvent.originalTarget;
}

class SystemVM {
    public IsChosen: KnockoutObservable<boolean>;

    // Single-select this item.
    public Clicked(data: any, event: JQueryEventObject) {
        // Don't act if the user clicked on the check box. Returning true will allow the default click action to proceed.
        if (getOriginalTarget(event).tagName.toLowerCase() == "input")
            return true;
        for (let other of this.Owner.Systems())
            if (other != this)
                other.IsChosen(false);
        this.IsChosen(true);
        // Select all settings for this system.
        this.Owner.SelectedAllSettings(true);
        return false;
    }

    constructor(public Owner: InstanceFilterVM, public Name: string, public Settings: string[], chosen: boolean) {
        this.IsChosen = ko.observable(chosen);
    }
}

class SettingsVM {
    public IsChosen: KnockoutObservable<boolean>;

    // Single-select this item.
    public Clicked(data: any, event: JQueryEventObject) {
        // Don't act if the user clicked on the check box. Returning true will allow the default click action to proceed.
        if (getOriginalTarget(event).tagName.toLowerCase() == "input")
            return true;
        for (let other of this.Owner.Settings())
            if (other != this)
                other.IsChosen(false);
        this.IsChosen(true);
        // Select all sweeps for this settings.
        this.Owner.SelectedAllSweeps(true);
        return false;
    }

    constructor(public Owner: InstanceFilterVM, public Name: string, public Sweeps: string[], chosen: boolean) {
        this.IsChosen = ko.observable(chosen);
    }
}

class SweepVM {
    public IsChosen: KnockoutObservable<boolean>;

    // Single-select this item.
    public Clicked(data: any, event: JQueryEventObject) {
        // Don't act if the user clicked on the check box. Returning true will allow the default click action to proceed.
        if (getOriginalTarget(event).tagName.toLowerCase() == "input")
            return true;
        for (let other of this.Owner.Sweeps())
            if (other != this)
                other.IsChosen(false);
        this.IsChosen(true);
        // Select all instances for this sweep.
        this.Owner.SelectedAllInstances(true);
        return false;
    }

    constructor(public Owner: InstanceFilterVM, public Name: string, public Instances: string[], chosen: boolean) {
        this.IsChosen = ko.observable(chosen);
    }
}

// (FP) This object is the VM for one simulation instance. It contains two implicit fields, representing the display name and the instance id. Note that they are simple fields, not observables; this because the name and ID of a instance never change (when the list is updated, the entire object is discarded and replaced). In addition, it contains a boolean that tells whether this specific instance has been selected by the user.
class InstanceVM {
    public IsChosen: KnockoutObservable<boolean>;

    // Single-select this item.
    public Clicked(data: any, event: JQueryEventObject) {
        // Don't act if the user clicked on the check box. Returning true will allow the default click action to proceed.
        if (getOriginalTarget(event).tagName.toLowerCase() == "input")
            return true;
        for (let other of this.Owner.Instances())
            if (other != this)
                other.IsChosen(false);
        this.IsChosen(true);
        return false;
    }

    constructor(public Owner: InstanceFilterVM, public Name: string, public IDs: number[], public Species: KnockoutObservableArray<string>, chosen: boolean) {
        this.IsChosen = ko.observable(chosen);
    }
}

// (FP) This object is the VM for one species. It works much like the one for instances above.
class SpeciesVM {
    public IsChosen: KnockoutObservable<boolean>;

    // Single-select this item.
    public Clicked(data: any, event: JQueryEventObject) {
        // Don't act if the user clicked on the check box. Returning true will allow the default click action to proceed.
        if (getOriginalTarget(event).tagName.toLowerCase() == "input")
            return true;
        for (let other of this.Owner.Species())
            if (other != this)
                other.IsChosen(false);
        this.IsChosen(true);
        return false;
    }

    constructor(public Owner: InstanceFilterVM, public Name: string, chosen: boolean) {
        this.IsChosen = ko.observable(chosen);
    }
}

// (FP) This is the VM for the control. It contains a set of available instances and a set of available species (each of these will contain a flag if it is selected). This information is made available in an easier format for binding through a few computed observables.
class InstanceFilterVM {
    // The list of all known systems.
    public Systems: KnockoutObservableArray<SystemVM>;
    // The list of all known settings.
    public Settings: KnockoutObservableArray<SettingsVM>;
    // The list of all known sweeps.
    public Sweeps: KnockoutObservableArray<SweepVM>;
    // The list of all known instances.
    public Instances: KnockoutObservableArray<InstanceVM>;
    // The list of all known species.
    public Species: KnockoutObservableArray<SpeciesVM>;

    // The set of systems that the user can choose from.
    public SelectableSystems: KnockoutComputed<SystemVM[]>;
    // The set of settings that the user can choose from.
    public SelectableSettings: KnockoutComputed<SettingsVM[]>;
    // The set of sweeps that the user can choose from.
    public SelectableSweeps: KnockoutComputed<SweepVM[]>;
    // The set of instances that the user can choose from.
    public SelectableInstances: KnockoutComputed<InstanceVM[]>;
    // The set of species that the user can choose from.
    public SelectableSpecies: KnockoutComputed<SpeciesVM[]>;

    // True if all systems are selected.
    public SelectedAllSystems: KnockoutComputed<boolean>;
    // True if all settings are selected.
    public SelectedAllSettings: KnockoutComputed<boolean>;
    // True if all sweeps are selected.
    public SelectedAllSweeps: KnockoutComputed<boolean>;
    // True if all instances are selected.
    public SelectedAllInstances: KnockoutComputed<boolean>;
    // True if all species are selected.
    public SelectedAllSpecies: KnockoutComputed<boolean>;

    // The set of selected systems.
    public EffectiveSystems: KnockoutComputed<Array<string>>;
    // The set of selected settings.
    public EffectiveSettings: KnockoutComputed<Array<string>>;
    // The set of selected sweeps.
    public EffectiveSweeps: KnockoutComputed<Array<string>>;
    // The set of selected instances.
    public EffectiveInstances: KnockoutComputed<Array<string>>;
    // The set of selected species.
    public EffectiveSpeciesNames: KnockoutComputed<Array<string>>;

    public IsSystemsVisible: KnockoutComputed<boolean>;
    public IsSettingsVisible: KnockoutComputed<boolean>;
    public IsSweepsVisible: KnockoutComputed<boolean>;
    public IsInstancesVisible: KnockoutComputed<boolean>;
    public IsSpeciesVisible: KnockoutComputed<boolean>;
    public IsAnyVisible: KnockoutComputed<boolean>;

    constructor() {
        this.Systems = ko.observableArray([]);
        this.Settings = ko.observableArray([]);
        this.Sweeps = ko.observableArray([]);
        this.Instances = ko.observableArray([]);
        this.Species = ko.observableArray([]);

        this.SelectableSystems = ko.pureComputed(() => this.Systems());
        this.SelectableSettings = ko.pureComputed(() => {
            return this.Settings().filter(i => this.SelectableSystems().some(system => system.IsChosen() && system.Settings.indexOf(i.Name) >= 0));
        });
        this.SelectableSweeps = ko.pureComputed(() => {
            return this.Sweeps().filter(i => this.SelectableSettings().some(setting => setting.IsChosen() && setting.Sweeps.indexOf(i.Name) >= 0));
        });
        this.SelectableInstances = ko.pureComputed(() => {
            return this.Instances().filter(i => this.SelectableSweeps().some(sweep => sweep.IsChosen() && sweep.Instances.indexOf(i.Name) >= 0));
        });
        this.SelectableSpecies = ko.pureComputed(() => {
            return this.Species().filter(s => this.SelectableInstances().some(instance => instance.IsChosen() && instance.Species.indexOf(s.Name) >= 0));
        });

        this.IsSystemsVisible = ko.pureComputed(() => {
            return this.Systems().length > 1;
        });
        this.IsSettingsVisible = ko.pureComputed(() => {
            return this.Settings().length > 1;
        });
        this.IsSweepsVisible = ko.pureComputed(() => {
            return this.Sweeps().length > 1;
        });
        this.IsInstancesVisible = ko.pureComputed(() => {
            return this.Instances().length > 1;
        });
        this.IsSpeciesVisible = ko.pureComputed(() => {
            return this.Instances().length > 1 || this.Settings().length > 1;
        });
        this.IsAnyVisible = ko.pureComputed(() => this.IsSystemsVisible() || this.IsSettingsVisible() || this.IsSweepsVisible() || this.IsInstancesVisible() || this.IsSpeciesVisible());

        this.EffectiveSystems = ko.pureComputed(() => {
            var systems = this.SelectableSystems().filter(s => s.IsChosen()).map(s => s.Name);
            return systems;
        }).extend({ rateLimit: 100, method: "notifyWhenChangesStop" });
        this.EffectiveSettings = ko.pureComputed(() => {
            var settings = this.SelectableSettings().filter(s => s.IsChosen()).map(s => s.Name);
            return settings;
        }).extend({ rateLimit: 100, method: "notifyWhenChangesStop" });
        this.EffectiveSweeps = ko.pureComputed(() => {
            var sweeps = this.SelectableSweeps().filter(s => s.IsChosen()).map(s => s.Name);
            return sweeps;
        }).extend({ rateLimit: 100, method: "notifyWhenChangesStop" });
        this.EffectiveInstances = ko.pureComputed(() => {
            var instances = this.SelectableInstances().filter(s => s.IsChosen()).map(s => s.Name);
            return instances;
        }).extend({ rateLimit: 100, method: "notifyWhenChangesStop" });
        this.EffectiveSpeciesNames = ko.computed(() => {
            var names = this.SelectableSpecies().filter(s => s.IsChosen()).map(s => s.Name);
            return names;
        }).extend({ rateLimit: 100, method: "notifyWhenChangesStop" });

        this.SelectedAllSystems = ko.pureComputed<boolean>({
            read: () => {
                return this.SelectableSystems().every((system) => { return system.IsChosen(); });
            },
            write: (value) => {
                this.SelectableSystems().forEach((system) => { system.IsChosen(value); });
            }
        });
        this.SelectedAllSettings = ko.pureComputed<boolean>({
            read: () => {
                return this.SelectableSettings().every((setting) => { return setting.IsChosen(); });
            },
            write: (value) => {
                this.SelectableSettings().forEach((setting) => { setting.IsChosen(value); });
            }
        });
        this.SelectedAllSweeps = ko.pureComputed<boolean>({
            read: () => {
                return this.SelectableSweeps().every((sweeps) => { return sweeps.IsChosen(); });
            },
            write: (value) => {
                this.SelectableSweeps().forEach((sweep) => { sweep.IsChosen(value); });
            }
        })
        this.SelectedAllInstances = ko.pureComputed<boolean>({
            read: () => {
                return this.SelectableInstances().every((id) => { return id.IsChosen(); });
            },
            write: (value) => {
                this.SelectableInstances().forEach((sp) => { sp.IsChosen(value); });
            }
        });
        this.SelectedAllSpecies = ko.pureComputed<boolean>({
            read: () => {
                return this.SelectableSpecies().every((sp) => { return sp.IsChosen(); });
            },
            write: (value) => {
                this.SelectableSpecies().forEach((sp) => { sp.IsChosen(value); });
            }
        });
    }
}

// (FP) This is the implementation of the control. Note that it is an INamesFilter and an IInstancesFilter, so it can be given data from the Viewer.
export class View implements I.ITracesFilter {
    private vm: InstanceFilterVM;

    constructor() {
        this.vm = new InstanceFilterVM();
    }

    // (FP) This function constructs the control in the given Element.
    bind(elementToBindTo: HTMLElement) {
        if (elementToBindTo == null)
            throw "Attempt to bind MultiInstancesFilter to null";
        ko.applyBindings(this.vm, elementToBindTo);
    }

    public GetSelectedSystems() {
        return this.vm.EffectiveSystems;
    }

    public GetSelectedSettings() {
        return this.vm.EffectiveSettings;
    }

    public GetSelectedSweeps() {
        return this.vm.EffectiveSweeps;
    }

    public GetSelectedInstances() {
        return this.vm.EffectiveInstances;
    }

    public GetSelectedSpecies() {
        return this.vm.EffectiveSpeciesNames;
    }

    public SetAvailableTraces(data: I.ITraceDefinitions) {
        var systemsVMs: SystemVM[] = [];
        var settingsVMs: SettingsVM[] = [];
        var sweepVMs: SweepVM[] = [];
        var instanceVMs: InstanceVM[] = [];
        var speciesVMs: SpeciesVM[] = [];
        var that = this;

        function addSystemVM(crn: I.ITraceDefinitionsCRN) {
            let settingsNames = crn.Settings.map(i => i.Name);
            for (let vm of systemsVMs) {
                if (vm.Name == crn.Name) {
                    for (let name of settingsNames)
                        if (vm.Settings.indexOf(name) < 0)
                            vm.Settings.push(name);
                    return;
                }
            }
            systemsVMs.push(new SystemVM(that.vm, crn.Name, settingsNames, systemsVMs.length == 0));
        }
        function addSettingsVM(settings: I.ITraceDefinitionsSetting) {
            let sweepNames = settings.Sweeps.map(i => i.Name);
            for (let vm of settingsVMs) {
                if (vm.Name == settings.Name) {
                    for (let name of sweepNames)
                        if (vm.Sweeps.indexOf(name) < 0)
                            vm.Sweeps.push(name);
                    return;
                }
            }
            settingsVMs.push(new SettingsVM(that.vm, settings.Name, sweepNames, settingsVMs.length == 0));
        }
        function addSweepVM(sweep: I.ITraceDefinitionsSweep) {
            let instanceNames = sweep.Instances.map(i => i.Name);
            for (let vm of sweepVMs) {
                if (vm.Name == sweep.Name) {
                    for (let name of instanceNames)
                        if (vm.Instances.indexOf(name) < 0)
                            vm.Instances.push(name);
                    return;
                }
            }
            sweepVMs.push(new SweepVM(that.vm, sweep.Name, instanceNames, sweepVMs.length == 0));
        }
        function addInstanceVM(instance: I.ITraceDefinitionsInstance) {
            let plotNames = instance.Plottables.map(i => i.Name);
            for (let vm of instanceVMs) {
                if (vm.Name == instance.Name) {
                    vm.IDs.push(instance.ID);
                    for (let name of plotNames)
                        if (vm.Species.indexOf(name) < 0)
                            vm.Species.push(name);
                    return;
                }
            }
            instanceVMs.push(new InstanceVM(that.vm, instance.Name, [instance.ID], ko.observableArray(instance.Plottables.map(i => i.Name)), true));
        }
        function addSpeciesVM(species: I.ITraceDefinitionsPlottable) {
            if (speciesVMs.every(vm => vm.Name != species.Name))
                speciesVMs.push(new SpeciesVM(that.vm, species.Name, true));
        }

        for (let crn of data.CRNs) {
            addSystemVM(crn);
            for (let settings of crn.Settings) {
                addSettingsVM(settings);
                for (let sweep of settings.Sweeps) {
                    addSweepVM(sweep);
                    for (let instance of sweep.Instances) {
                        addInstanceVM(instance);
                        for (let species of instance.Plottables)
                            addSpeciesVM(species);
                    }
                }
            }
        }
        this.vm.Systems(systemsVMs);
        this.vm.Settings(settingsVMs);
        this.vm.Sweeps(sweepVMs);
        this.vm.Instances(instanceVMs);
        this.vm.Species(speciesVMs);
    }

    public AddSpeciesToInstance(newPlottable: I.IAdditionalPlottable) {
        // Find the instance VM that represents the plottable's instance, and add the name to it.
        for (let inst of this.vm.Instances())
            if (inst.IDs.indexOf(newPlottable.Instance) >= 0) {
                inst.Species.push(newPlottable.Name);
                break;
            }
        // If there is no species VM for this name, add it.
        if (this.vm.Species().every(vm => vm.Name != newPlottable.Name))
            this.vm.Species.push(new SpeciesVM(this.vm, newPlottable.Name, true));
    }

    public ResetNames() {
        this.vm.Species([]);
    }
}
