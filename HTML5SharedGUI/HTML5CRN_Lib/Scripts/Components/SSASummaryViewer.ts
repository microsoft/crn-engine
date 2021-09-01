// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from "knockout";

export interface ISpecies {
    graphics: string;
    name: string;
    value: number;
}

export type IState = Array<ISpecies>;

export interface SSASummaryViewerParams {
    initialState: KnockoutObservable<IState>;
    terminalStates: KnockoutObservable<IState[]>;
}

export class SSASummaryViewModel{
    private initialState: KnockoutObservable<IState>;
    private terminalStates: KnockoutObservable<IState[]>;
    private render = ko.observable(true);
    constructor(params: SSASummaryViewerParams) {
        this.initialState = params.initialState? params.initialState:ko.observable<IState>();
        this.terminalStates = params.terminalStates? params.terminalStates:ko.observable<IState[]>();
    }
}

import * as ssaSummaryViewerTemplate from 'raw-loader!../../Templates/ssa-summary-template.html';

export class SSASummaryViewer {
    constructor() {
        if (!ko.components.isRegistered("ssa-summary-viewer"))
            ko.components.register("ssa-summary-viewer", {
                viewModel: params => new SSASummaryViewModel(params),
                template: ssaSummaryViewerTemplate
            });
    }
}
