// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import "./SequencesDatabase.css"
import * as ko from 'knockout';
import DSD from '../../../ClassicDSDTSWrapper/Scripts/ClassicDSD';
import * as fileSaver from "file-saver";
import * as template from "raw-loader!./template.html";
import * as toeholds from "raw-loader!./toeholds.txt";
import * as specificities from "raw-loader!./specificities.txt";

class SequencesDatabaseVM {
    constructor(engine: DSD) {
        this.Toeholds.subscribe(t => {
            engine.Toeholds = t;
            window.localStorage.setItem("ToeholdsPersisted", t);
        });
        var prevToeholds = window.localStorage.getItem("ToeholdsPersisted");
        if (prevToeholds == null || prevToeholds == "")
            prevToeholds = engine.Toeholds;
        this.Toeholds(prevToeholds);

        this.Specificities.subscribe(s => {
            engine.Specificities = s;
            window.localStorage.setItem("SpecificitiesPersisted", s);
        });
        var prevSpecificities = window.localStorage.getItem("SpecificitiesPersisted");
        if (prevSpecificities == null || prevSpecificities == "")
            prevSpecificities = engine.Specificities;
        this.Specificities(prevSpecificities);
    }

    public Toeholds = ko.observable<string>("");
    public Specificities = ko.observable<string>("");

    private Save(ob: KnockoutObservable<string>, name: string) {
        var txt = ob();
        var blob = new Blob([txt], { type: "text/txt" });
        fileSaver.saveAs(blob, name);
    }

    private Load(ob: KnockoutObservable<string>) {
        var that = this;
        var filesInput = document.createElement("input");
        filesInput.type = "file";
        filesInput.onchange = function () {
            var file = filesInput.files[0];
            var reader = new FileReader();
            reader.onload = function (event) {
                var text = (<any>event.target).result;
                ob(text);
            }
            reader.readAsText(file);
        };
        filesInput.click();
    }

    public SaveToeholds() {
        this.Save(this.Toeholds, "toeholds.txt");
    }

    public LoadToeholds() {
        this.Load(this.Toeholds);
    }

    public ResetToeholds() {
        this.Toeholds(toeholds);
    }

    public SaveSpecificities() {
        this.Save(this.Specificities, "specificities.txt");
    }

    public LoadSpecificities() {
        this.Load(this.Specificities);
    }

    public ResetSpecificities() {
        this.Toeholds(specificities);
    }
}

function build(div: HTMLDivElement, engine: DSD) {
    div.innerHTML = template;
    var vm = new SequencesDatabaseVM(engine);
    ko.applyBindings(vm, div);
}

export default build