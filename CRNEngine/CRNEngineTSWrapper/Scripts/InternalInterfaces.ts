// This file contains the interfaces for the messages exchanged by the background threads and the main thread.
import * as I from "./Interfaces";

export type WorkerResponse_Error = { mtype: "error", error: I.Error }
export type WorkerResponse_Finished = { mtype: "finished" }
export type WorkerResponse_Aborted = { mtype: "aborted" }
export type WorkerResponse_Model = { mtype: "model"; model: I.IG }
export type WorkerResponse_Node = { mtype: "node"; node: I.Model }
export type WorkerResponse_Export = { mtype: "export"; export: I.ExportDef }
export type WorkerResponse_InferenceChain = { mtype: "inferencechainupdate"; nodeId: string; update: I.EvaluatedValues }
export type WorkerResponse_ParameterResult = { mtype: "parameterresult"; nodeId: string; values: I.EvaluatedValues }
export type WorkerResponse_ParameterDefinitions = { mtype: "parameterdefinitions"; nodeId: string; parameters: I.InferenceParameter[] }
export type WorkerResponse_InferenceResult = { mtype: "inferenceresult"; nodeId: string; result: I.InferenceResult }
export type WorkerResponse_SimType = { mtype: "simtype"; simtype: I.SimResultType }
export type WorkerResponse_SimInstanceDefinitions = { mtype: "instancedefinitions"; nodeId: string; definitions: I.SimulationInstanceDefinition[] }
export type WorkerResponse_InferenceProgress = { mtype: "inferenceprogress"; nodeId: string; progress: number }
export type WorkerResponse_NewPlottable = { mtype: "newplottable"; plottable: I.NewPlottableDefinition }
export type WorkerResponse_SimResult = { mtype: "simresult"; row: I.SimResult<number> | I.SimResult<I.SimResultMeanStdev> | I.SimResult<number[]> }
export type WorkerResponse_SimTable = { mtype: "simtable"; table: I.SimTable<number> | I.SimTable<I.SimResultMeanStdev> }
export type WorkerResponse_StateSpace = { mtype: "statespace"; statespace: I.StateSpace }
export type WorkerResponse_Summary = { mtype: "summary"; nodeId: string; summary: string }
export type WorkerResponse_Probabilities = { mtype: "probabilities"; probabilities: I.InstanceProbabilities }
export type WorkerResponse_ProbabilityMap = { mtype: "probabilitymap"; map: I.ProbabilityMap }
export type WorkerResponse_Synthesis = { mtype: "synthesis"; result: I.SynthesisResult }
export type WorkerResponse_Bistability = { mtype: "bistability"; plot: I.BistabilityPlot }

export type JobsResponse_CloudCapabilities = { mtype: "pools", account: string, pools: string[] }
export type JobsResponse_Jobs = { mtype: "jobs", jobs: I.JobDescriptor[] }

export type WorkerResponse = WorkerResponse_Error | WorkerResponse_Finished | WorkerResponse_Aborted | WorkerResponse_Model | WorkerResponse_Node | WorkerResponse_Export | WorkerResponse_InferenceChain | WorkerResponse_InferenceResult | WorkerResponse_InferenceProgress | WorkerResponse_ParameterDefinitions | WorkerResponse_ParameterResult | WorkerResponse_SimType | WorkerResponse_SimInstanceDefinitions | WorkerResponse_NewPlottable | WorkerResponse_SimResult | WorkerResponse_SimTable | WorkerResponse_StateSpace | WorkerResponse_Summary | WorkerResponse_ProbabilityMap | WorkerResponse_Probabilities | WorkerResponse_Synthesis | WorkerResponse_Bistability | JobsResponse_CloudCapabilities | JobsResponse_Jobs;

export type WorkerRequest_ParseCode = { mtype: "parsecode"; code: string }
export type WorkerRequest_GenerateExports = { mtype: "generateexports"; model: I.IG; nodeId: string }
export type WorkerRequest_GenerateExport = { mtype: "generateexport"; model: I.IG; nodeId: string; id: string; instance: string }
export type WorkerRequest_SimulateGUI = { mtype: "simulategui"; model: I.IG, nodeId: string, pool: string }
export type WorkerRequest_SimulateSingle = { mtype: "simulatesingle"; model: I.IG; nodeId: string; definition: I.SimulationInstanceDefinition }
export type WorkerRequest_InferCode = { mtype: "infercode"; code: string; datasets: I.Dataset[], pool: string }
export type WorkerRequest_InferGUI = { mtype: "infergui"; model: I.IG, pool: string }
export type WorkerRequest_GetSimRunsGUI = { mtype: "getsimrunsgui"; model: I.IG; nodeId: string }
export type WorkerRequest_StateSpace = { mtype: "statespace"; jit: boolean; model: I.IG }
export type WorkerRequest_GetProbabilityMap = { mtype: "getprobabilitymap"; probabilities: I.Probabilities; species: string; lowerBound: number }
export type WorkerRequest_Synthesis = { mtype: "synthesis"; model: I.IG; nodeId: string; crnId: string }
export type WorkerRequest_Bistability = { mtype: "bistability"; crn: I.CRN; solution: { [name: string]: number }; spX: string; spY: string; numPoints: number }

export type JobsRequest_GetCloudCapabilities = { mtype: "getcloudcapabilities" }
export type JobsRequest_GetJobs = { mtype: "getjobs"; allFiles: boolean }
export type JobsRequest_StopJob = { mtype: "stopjob"; id: string }
export type JobsRequest_DeleteJob = { mtype: "deletejob"; id: string }

export type WorkerRequest = WorkerRequest_ParseCode | WorkerRequest_GenerateExports | WorkerRequest_GenerateExport | WorkerRequest_SimulateGUI | WorkerRequest_SimulateSingle | WorkerRequest_InferGUI | WorkerRequest_InferCode | WorkerRequest_GetSimRunsGUI | WorkerRequest_StateSpace | WorkerRequest_GetProbabilityMap | WorkerRequest_Synthesis | WorkerRequest_Bistability | JobsRequest_GetCloudCapabilities | JobsRequest_GetJobs | JobsRequest_StopJob | JobsRequest_DeleteJob;


export type FastWorkerRequest_ParseExpression = { mtype: "parseexpression"; expression: string }
export type FastWorkerRequest_ParseTimeUnit = { mtype: "parsetimeunit"; unittext: string }
export type FastWorkerRequest_ParseSpaceUnit = { mtype: "parsespaceunit"; unittext: string }
export type FastWorkerRequest_ParseConcentrationUnit = { mtype: "parseconcentrationunit"; unittext: string }
export type FastWorkerRequest_StringifyTimeUnit = { mtype: "stringifytimeunit"; unit: I.TimeUnit }
export type FastWorkerRequest_StringifySpaceUnit = { mtype: "stringifyspaceunit"; unit: I.SpaceUnit }
export type FastWorkerRequest_StringifyConcentrationUnit = { mtype: "stringifyconcentrationunit"; unit: I.ConcentrationUnit }

export type FastWorkerRequest = FastWorkerRequest_ParseExpression | FastWorkerRequest_ParseTimeUnit | FastWorkerRequest_ParseSpaceUnit | FastWorkerRequest_ParseConcentrationUnit | FastWorkerRequest_StringifyTimeUnit | FastWorkerRequest_StringifySpaceUnit | FastWorkerRequest_StringifyConcentrationUnit;

export type FastWorkerResponse_Error = { mtype: "error", error: I.Error }
export type FastWorkerResponse_Expression = { mtype: "expression", expression: I.Expression }
export type FastWorkerResponse_String = { mtype: "string", str: string }
export type FastWorkerResponse_TimeUnit = { mtype: "timeunit", unit: I.TimeUnit }
export type FastWorkerResponse_SpaceUnit = { mtype: "spaceunit", unit: I.SpaceUnit }
export type FastWorkerResponse_ConcentrationUnit = { mtype: "concentrationunit", unit: I.ConcentrationUnit }

export type FastWorkerResponse = FastWorkerResponse_Error | FastWorkerResponse_Expression | FastWorkerResponse_SpaceUnit | FastWorkerResponse_TimeUnit | FastWorkerResponse_ConcentrationUnit | FastWorkerResponse_String;