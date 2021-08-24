// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from "knockout";
import { Observable } from "rx";
import CRNEngine from './../../CRNEngineTSWrapper/Scripts/CRNEngine';
import { JobDescriptor, JobFile } from "../../CRNEngineTSWrapper/Scripts/Interfaces";
import "../Styles/app.css";

ko.bindingHandlers.nullableChecked = {
    init: function (element, valueAccessor) {
        element.onclick = function () {
            var value = valueAccessor();
            var unwrappedValue = ko.unwrap(value);
            if (unwrappedValue)
                value(null);
            else if (unwrappedValue === false)
                value(true);
            else
                value(false);
        };
    },
    update: function (element, valueAccessor) {
        var value = ko.utils.unwrapObservable(valueAccessor());
        element.checked = value;
        element.indeterminate = (value == null);
    }
};

class JobVM {
    constructor(job: JobDescriptor) {
        this.Job = ko.observable(job);
        this.ZipFile = ko.pureComputed(() => this.Job().zipFile);
        this.Files = ko.pureComputed(() => this.Job().files);
        this.Selected = ko.observable(false);
        this.Stopping = ko.observable(false);
        this.Deleting = ko.observable(false);
        this.State = ko.pureComputed(() => {
            if (this.Deleting())
                return "Deleting";
            if (this.Stopping())
                return "Stopping";
            return this.Job().state;
        });
    }

    public Job: KnockoutObservable<JobDescriptor>;
    public State: KnockoutObservable<string>;
    public ZipFile: KnockoutObservable<JobFile>;
    public Files: KnockoutObservable<JobFile[]>;
    public Selected: KnockoutObservable<boolean>;
    public Stopping: KnockoutObservable<boolean>;
    public Deleting: KnockoutObservable<boolean>;
}

class JobsVM {
    constructor(private engine: CRNEngine) {
        engine.GetCloudCapabilities().subscribe(cc => {
            this.Account(cc.account);
        }, error => { }, () => { });
        this.BeginLoad();
    }

    private reloadInterval = 60000;
    private timeoutKey: number;

    public BeginLoad() {
        var that = this;
        if (this.timeoutKey != null)
            clearTimeout(this.timeoutKey);
        this.timeoutKey = null;
        this.Loading(true);
        this.Error(null);
        engine.GetJobs(false).subscribe((jobs: JobDescriptor[]) => {
            var currvms: JobVM[] = this.Jobs();
            var newvms = jobs.map(j => {
                for (let c of currvms)
                    if (c.Job().id == j.id) {
                        c.Job(j);
                        return c;
                    }
                return new JobVM(j);
            });
            newvms.sort((a, b) => Date.parse(a.Job().start) - Date.parse(b.Job().start));
            this.Jobs(newvms);
            this.Loading(false);
            this.Error(null);
        }, (error:any) => this.onError(error), () => {
            this.timeoutKey = setTimeout(() => that.BeginLoad(), this.reloadInterval);
        });
    }

    private onError(error: any) {
        var errorText = JSON.stringify(error);
        console.error(errorText);
        this.Loading(false);
        this.Error(errorText);
    }

    public Account = ko.observable("");
    public Jobs = ko.observableArray<JobVM>([]);

    public AllSelected = ko.pureComputed({
        read: () => this.Jobs().every(job => job.Selected()),
        write: v => this.Jobs().forEach(job => job.Selected(v))
    });

    public Loading = ko.observable(false);
    public StatusText = ko.pureComputed(() => (this.Jobs().length == 0 ? "No" : this.Jobs().length.toString()) + " job" + (this.Jobs().length == 1 ? "" : "s") + (this.Loading() ? " (loading...)" : ""));
    public Error: KnockoutObservable<string> = ko.observable(null);

    public reload() {
        this.BeginLoad();
    }

    public stopJob(job: JobVM) {
        var that = this;
        job.Stopping(true);
        engine.StopJob(job.Job().id).subscribe(() => { }, (error:any) => this.onError(error), () => that.BeginLoad());
    }

    public stopSelected() {
        var that = this;
        var obs = this.Jobs().filter(j => j.Selected()).map(j => {
            j.Stopping(true);
            return engine.StopJob(j.Job().id);
        });
        if (obs.length > 0)
            Observable.forkJoin(obs).subscribe(() => { }, error => this.onError(error), () => that.BeginLoad());
    }

    public deleteJob(job: JobVM) {
        var that = this;
        job.Deleting(true);
        engine.DeleteJob(job.Job().id).subscribe(() => { }, (error:any) => this.onError(error), () => that.BeginLoad());
    }

    public deleteSelected() {
        var that = this;
        var obs = this.Jobs().filter(j => j.Selected()).map(j => {
            j.Deleting(true);
            return engine.DeleteJob(j.Job().id);
        });
        if (obs.length > 0)
            Observable.forkJoin(obs).subscribe(() => { }, error => this.onError(error), () => that.BeginLoad());
    }
}

var engine = new CRNEngine(true);
var vm = new JobsVM(engine);

ko.applyBindings(vm, document.getElementById("app"))
console.log("CRNJobsManager loaded");