// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as Interfaces from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as Operation from '../Operations/StateSpaceAnalysis';
import * as SummaryViewer from '../Components/SSASummaryViewer';
import * as ko from 'knockout';

export interface ISSASummary {
    InitialState: SummaryViewer.IState;
    TerminalStates: Array<SummaryViewer.IState>;
}

/**
 * Finds and returns initial and terminal states
 * @param states States to sift
 */
export function SiftStates(state_space: Interfaces.StateSpace): ISSASummary {
    let statesCount = state_space.states.length;

    let attributes: { [key: string]: Interfaces.SpeciesAttributes } = {}
    for (let attribute of state_space.attributes)
        attributes[attribute.name] = attribute;

    let initState: SummaryViewer.IState = [];
    let initSpecs = state_space.states[state_space.start_index].species;
    for (let species in initSpecs) {
        initState.push({
            graphics: attributes[species].svg,
            name: species,
            value: initSpecs[species]
        });
    }
    let finalStatesIndices: Array<number> = [];
    for (let i = 0; i < statesCount; i++) {
        let currentState = state_space.states[i];
        let transitions = currentState.transitions;
        if (transitions.length == 0)
            finalStatesIndices.push(i)
    }
    let finalStatesSpecs = finalStatesIndices.map(idx => state_space.states[idx].species);
    let finalStates: SummaryViewer.IState[] = [];
    for (let species of finalStatesSpecs) {
        let stateToShow: SummaryViewer.IState = [];
        $.map(species, (value, index) => {
            stateToShow.push({
                graphics: attributes[index].svg,
                name: index,
                value: value
            });
        });
        finalStates.push(stateToShow);
    }
    return {
        InitialState: initState,
        TerminalStates: finalStates
    }
}

export class SSASummaryVM {
    initialState = ko.observable();
    terminalStates = ko.observable();
}

/**
 * Shows space state analysis initial and terminal states
 */
export class Viewer implements Operation.ISSAViewer {

    private ssaVM = new SSASummaryVM();
    private ssaViewer = new SummaryViewer.SSASummaryViewer();

    public constructor() {
    }

    public Bind(elem: HTMLElement) {
        ko.applyBindings(this.ssaVM, elem);
    }

    //Operation.ISSAViewer implementation
    Show(stateSpace: Interfaces.StateSpace) {
        var states = SiftStates(stateSpace);
        this.ssaVM.initialState(states.InitialState);
        this.ssaVM.terminalStates(states.TerminalStates);
    }
}
