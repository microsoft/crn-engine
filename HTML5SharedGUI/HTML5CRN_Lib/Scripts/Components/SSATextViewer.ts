import * as Interfaces from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces";
import * as Operation from '../Operations/StateSpaceAnalysis';

let parser = new DOMParser();

export interface ISSASummary {
    TerminalStatesCount: number;
    StatesCount: number;
    TransitionsCount: number;
    InitialStatePopulation: Interfaces.Population;
    FinalStatesPopulations: Array<Interfaces.Population>;
    MaximumPopulations: Interfaces.Population;
    MinimumPopulations: Interfaces.Population;
}

/**
 * Calculates summary of the State Space Analysis graph
 * @param state State to summrize
 */
export function CalculateSummary(state_space: Interfaces.StateSpace): ISSASummary {
    var statesCount = state_space.states.length;
    var transitionsCount = 0;
    var finalStatesIndices: Array<number> = [];
    var minPopulations: Interfaces.Population = {};
    var maxPopulations: Interfaces.Population = {};   
    for (var i = 0; i < statesCount; i++) {
        var currentState = state_space.states[i];
        var transitions = currentState.transitions;
        if (transitions.length == 0)
            finalStatesIndices.push(i);
        else
            for (var j = 0; j < transitions.length; j++) {
                //counting overall transitions
                transitionsCount++;
            }
        var species = currentState.species;
        for (var key in species) {
            if (species.hasOwnProperty(key)) {
                var population = species[key];
                if ((!minPopulations[key]) || (minPopulations[key] > population))
                    minPopulations[key] = population;
                if ((!maxPopulations[key]) || (maxPopulations[key] < population))
                    maxPopulations[key] = population;
            }
        }
    }

    var finalStates = finalStatesIndices.map(idx => state_space.states[idx].species);
    var result: ISSASummary = {
        StatesCount: statesCount,
        TransitionsCount: transitionsCount,
        InitialStatePopulation: state_space.states[state_space.start_index].species,
        TerminalStatesCount: finalStates.length,
        MinimumPopulations: minPopulations,
        MaximumPopulations: maxPopulations,
        FinalStatesPopulations: finalStates
    };
    return result;
}

/**
 * Shows space state analysis as text information. Puts textural information inside element passed with Bind method
 */
export class Viewer implements Operation.ISSAViewer {
    private elem: HTMLElement = undefined;
    private htmlText: string = undefined;
    public constructor() {

    }

    public Bind(elem: HTMLElement) {
        this.elem = elem;
        if (this.htmlText) {
            let doc = parser.parseFromString(this.htmlText, "text/html");
            this.elem.textContent = "";
            this.elem.appendChild(doc.getElementsByTagName("body")[0].firstChild);
        }
    }

    private GenerateInnerHtml(summary: ISSASummary): string {
        var populationsToString = function (populations: Interfaces.Population) {
            var str = "";
            var first = true;
            for (var key in populations) {
                if (populations.hasOwnProperty(key)) {
                    if (first)
                        first = false;
                    else
                        str += "<br>";
                    str += key + " => " + populations[key];

                }
            }
            return str;
        }

        var resultStr = "<p>" +
            "Number of terminal states: " + summary.TerminalStatesCount + "<br>" +
            "Number of states: " + summary.StatesCount + " <br>" +
            "Number of transitions: " + summary.TransitionsCount + "</p>" +
            "<h3>Initial State Population</h3>";
        resultStr += "<p>" + populationsToString(summary.InitialStatePopulation) + "</p>";
        resultStr+="<h3>Terminal State Populations</h3>";
        var finalStates = summary.FinalStatesPopulations;
        if (finalStates.length == 0)
            resultStr += "<p>No terminal states identified</p>";
        else
            for (var i = 0; i < finalStates.length; i++) {
                resultStr += "<h4>Final state " + (i + 1) + "</h4>";
                resultStr += "<p>" + populationsToString(finalStates[i]) + "</p>";
            }
        resultStr += "<h3>Maximum Populations of Species</h3>" +
            "<p>" + populationsToString(summary.MaximumPopulations) + "</p>" +
            "<h3>Minimum Populations of Species</h3>" +
            "<p>" + populationsToString(summary.MinimumPopulations) + "</p>";
        return "<div>" + resultStr + "</div>";
    }

    //Operation.ISSAViewer implementation
    Show(stateSpace: Interfaces.StateSpace) {
        var generated = this.GenerateInnerHtml(CalculateSummary(stateSpace));
        if (this.elem) {
            let doc = parser.parseFromString(generated, "text/html");
            this.elem.textContent = "";
            this.elem.appendChild(doc.getElementsByTagName("body")[0].firstChild);
        } else {
            this.htmlText = generated;
        }
    }
}