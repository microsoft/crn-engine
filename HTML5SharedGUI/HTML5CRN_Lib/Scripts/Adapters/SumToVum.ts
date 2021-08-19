import * as SimRunner from './ModellingEngineSimulationRunner';
import * as SVF from '../../../../HTML5SharedGUI/SimulationViewer/Scripts/SimulationViewerFramework';
import * as ME from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/CRNEngine';
import { WebSharperGeneratedInterfaces as WGI } from "../../../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces";
import * as serialization from '../../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';
import * as I from "../../../GenericComponents/Scripts/Interfaces";

export function getModelfromSim(simMsg: SimRunner.ISimulationMessage) {
    if (simMsg.Type == SimRunner.SimulationMessageType.JITModel)
        return <serialization.Model>simMsg.Payload;
    return null;
}
export function getExportsFromSim(simMsg: SimRunner.ISimulationMessage) {
    if (simMsg.Type == SimRunner.SimulationMessageType.Export)
        return <serialization.ExportDef>simMsg.Payload;
    return null;
}
export function getStateSpaceFromSim(simMsg: SimRunner.ISimulationMessage) {
    if (simMsg.Type == SimRunner.SimulationMessageType.StateSpace)
        return <serialization.StateSpace>simMsg.Payload;
    return null;
}

/** Converts SimulationUpdateMessage (SUM) to VisualizationUpdateMessage(VUM) */
export function Convert(sum: SimRunner.ISimulationMessage) {
    var result: SVF.IVisualizationUpdateMessage = undefined;
    switch (sum.Type) {
        case SimRunner.SimulationMessageType.TracesInfo:
            var instances = <I.ITraceDefinitions>sum.Payload;
            result = {
                MessageType: SVF.VisualizationUpdateMessageType.TraceDefinitions,
                EncapsulatedUpdate: instances
            };
            break;
        case SimRunner.SimulationMessageType.NewPlottable:
            var newPlottable = <I.IAdditionalPlottable>sum.Payload;
            result = {
                MessageType: SVF.VisualizationUpdateMessageType.AdditionalPlottable,
                EncapsulatedUpdate: newPlottable
            };
            break;
        case SimRunner.SimulationMessageType.SimulationStep:
            var step = <SVF.ISimStepData>sum.Payload;
            result = {
                MessageType: SVF.VisualizationUpdateMessageType.SimulationStep,
                EncapsulatedUpdate: step
            };
            break;
        case SimRunner.SimulationMessageType.SimulationTable:
            var table = <SVF.ISimTable>sum.Payload;
            result = {
                MessageType: SVF.VisualizationUpdateMessageType.SimulationTable,
                EncapsulatedUpdate: table
            };
            break;
        case SimRunner.SimulationMessageType.Probabilities:
            var meprobs = <serialization.InstanceProbabilities>sum.Payload;
            var probs: SVF.IProbabilitiesInfo = {
                InstanceName: Object.getOwnPropertyNames(meprobs.instance.environment).map(n => n + "=" + meprobs.instance.environment[n]).join(", "),
                Names: Object.getOwnPropertyNames(meprobs.probabilities.species),
                ProbabilitiesToken: meprobs.probabilities
            };
            result = {
                MessageType: SVF.VisualizationUpdateMessageType.Probabilities,
                EncapsulatedUpdate: probs
            };
            break;
        default:
            break;
    }
    return result;
};