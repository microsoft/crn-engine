module Microsoft.Research.CRNEngineServerLib.Messages

open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI
open Microsoft.Research.CRNEngineCloudLib.DataStructures

type SimulationInstanceDefinition = 
    { id : int
      instance : Instance<string> }

type InstanceProbabilities =
    { instance : Instance<string>
      probabilities : Microsoft.Research.CRNEngine.Probabilities }

type Result = 
    { sim_id : int
      result : Microsoft.Research.CRNEngine.Result<float> }

type GeneralMessage = 
    { mtype : string }

type Request_InferCode = 
    { mtype : string
      code : string
      datasets : Dataset []
      pool : string }

type Request_InferGui = 
    { mtype : string
      model : GuiIG
      pool : string }

type Request_SimulateGui = 
    { mtype : string
      model : GuiIG
      nodeId : string
      pool : string }

type Request_ParseCode = 
    { mtype : string
      code : string }

type Request_GenerateExports =
    { mtype : string
      model : GuiIG
      nodeId : string}

type Request_GenerateExport =
    { mtype : string
      model : GuiIG
      nodeId : string
      id : string
      instance : string option }

type Request_StateSpace =
    { mtype : string
      jit : bool
      model : GuiIG }

type Request_GetProbabilityMap =
    { mtype : string
      probabilities : Probabilities
      species : string
      lowerBound : float }

type Request_Synthesis =
    { mtype : string
      model : GuiIG
      nodeId : string
      crnId : string }

type Request_Bistability =
    { mtype : string
      crn : Gui
      solution : Map<string,float>
      spX : string
      spY : string
      numPoints : int }

type Request_GetJobs =
    { mtype : string
      allFiles : bool }

type Request_StopJob =
    { mtype : string
      id : string }

type Request_DeleteJob =
    { mtype : string
      id : string }

type Request_GetCloudCapabilities =
    { mtype : string }

type Response_CloudCapabilities =
    { mtype : string
      account : string
      pools : string[] }

type Response_Jobs =
    { mtype : string
      jobs : JobDescriptor[] }

type Response_Synthesis =
    { mtype : string 
      result: SynthesisResult }

type Response_StateSpace =
    { mtype : string
      statespace : state_space }

type Response_Export = 
    { mtype : string
      export : export_def }

type Response_SimType = 
    { mtype : string
      simtype : string }

type ResponsePlottable =
    { instance : int; plottable : Jit.newplottable }
type Response_NewPlottable =
    { mtype : string
      plottable : ResponsePlottable }

type Response_Plottables =
    { mtype : string
      plottables : string[] }

type ResponseRow =
    { instance : int; time : float; values: obj}
type Response_SimResult = 
    { mtype : string
      row : ResponseRow }

type ResponseTable =
    { instance : int; time : float[]; values: obj}
type Response_SimTable =
    { mtype : string
      table : ResponseTable }

type Response_Probabilities = 
    { mtype : string
      probabilities : InstanceProbabilities }

type Response_ProbabilityMap =
    { mtype : string
      map : probabilityMap }

type Response_InstanceDefinitions = 
    { mtype : string
      nodeId : string
      definitions : SimulationInstanceDefinition[] }

type Response_ParameterDefinitions = 
    { mtype : string
      nodeId : string
      parameters : InferenceParameter[] }

type Response_InferenceSummary = 
    { mtype : string
      nodeId : string
      summary : string }

type Response_InferenceProgress = 
    { mtype : string
      nodeId : string
      progress : int }

type Response_InferenceChainUpdate = 
    { mtype : string
      nodeId : string
      update : inference_evaluated_values }

type Response_ParameterResult = 
    { mtype : string
      nodeId : string
      values : inference_evaluated_values }

type Response_InferenceResult = 
    { mtype : string
      nodeId : string
      result : Result }

type Response_Program = 
    { mtype : string
      model : GuiIG }

type Response_Node = 
    { mtype : string
      node : GuiModel }

type Response_Bistability =
    { mtype : string
      plot : BistabilityPlot }

type CRNEngineError = { message : string
                        positions : Parser.error[] option }

type Response_Error =
    { mtype : string
      error : CRNEngineError }