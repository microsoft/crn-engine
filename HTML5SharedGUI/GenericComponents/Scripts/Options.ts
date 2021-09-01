// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import CRNEngine from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import "./Dropdown";

class Options {
    public static Server(): boolean { return window.location.search.indexOf("server=true") !== -1; }

    public constructor(private engine: CRNEngine) {
        engine.GetCloudCapabilities().subscribe(cap => {
            this.Account(cap.account);
            this.Pools(cap.pools);
            this.Pool(cap.pools.length == 0 ? "" : cap.pools[0]);
        }, error => { }, () => { });
        this.ActivePool.subscribe(p => engine.pool = p);
    }

    public Pools: KnockoutObservable<string[]> = ko.observable([]);
    public Pool: KnockoutObservable<string> = ko.observable("");

    public IsCloudCapable = ko.pureComputed(() => this.Pools().length > 0);
    public Cloud = ko.observable(false);
    public Account = ko.observable("");

    public ActivePool = ko.pureComputed(() => this.Cloud() ? this.Pool() : "");

    public bind(container: HTMLElement) {
        ko.applyBindings(this, container);
    }

    public openJobsManager() {
        var url = window.location.origin + window.location.pathname + "jobs";
        window.open(url, 'crn_jobs').focus();
    }
}

export default Options;
