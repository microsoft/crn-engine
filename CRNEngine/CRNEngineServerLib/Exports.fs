module Microsoft.Research.CRNEngineServerLib.Exports

open Microsoft.Research.CRNEngineServerLib.Serialisation
open System.Net.WebSockets
open Messages
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.JSAPI
open Parser
open InferenceSiteGraph
open System.Threading.Tasks

let make_exports (ig:IGraph) (nodeId:string)=
    let model = ig.nodes |> Map.toSeq |> Seq.head |> snd
    let exports = model_to_export false ig nodeId
    let all_systems = model.top::model.systems
    let mc_systems = List.filter (fun (sys:Crn) -> sys.settings.simulator = Simulator.MC) all_systems
    match mc_systems with
    | [] -> exports
    | mc_systems ->
      let get_latex (crn:Crn) =
        let moments = Microsoft.Research.CRNEngine.Moments.generateMoments crn
        let momentsLatex = Microsoft.Research.CRNEngine.MC_Utils.to_latex moments
        let closure = Microsoft.Research.CRNEngine.Moments.generateClosure moments
        let closureLatex = Microsoft.Research.CRNEngine.MC_Utils.to_latex closure
        let matlab = Microsoft.Research.CRNEngine.MC_Utils.to_matlab closure
        let (momentsLatex, closureLatex) = match crn.name with
                                           | "" -> (momentsLatex,closureLatex)
                                           | name -> (name + "\\\\ \n\n" + momentsLatex, name + "\\\\ \n\n" + closureLatex)
        (momentsLatex, closureLatex, matlab)
      let all_latex = List.map get_latex mc_systems
      let (all_moments_latex, all_closure_latex, all_matlab) = List.unzip3 all_latex
      let exports = Array.map (fun (exp:export_def) -> if exp.id = "matlab" then { exp with content = [|(String.concat "\n\n\n" all_matlab)|] |> Some } else exp) exports
      Array.append exports [| { content_type = "application/x-tex"; id = "moments"; display_name = "Moments"; node_id=Some nodeId; instance = None ;content = [|(String.concat "\\\\ \n\n" all_moments_latex)|] |> Some; save_content = None }
                              { content_type = "application/x-tex"; id = "closure"; display_name = "Closure"; node_id=Some nodeId; instance = None ;content = [|(String.concat "\\\\ \n\n" all_closure_latex)|] |> Some; save_content = None } |]

let processGenerateExportsRequest (gui:GuiIG) (nodeId:string) (webSocket:WebSocket) = 
    let sendObject = sendObject webSocket
    let ig = gui.to_ig()
    let exports = make_exports ig nodeId
    for export in exports do
        sendObject { Response_Export.mtype = "export"
                     Response_Export.export = export }

let processGenerateExportRequest (gui:GuiIG) (nodeId:string) (id:string) (instance:string option) (webSocket:WebSocket) =
    let sendObject = sendObject webSocket
    let ig = gui.to_ig()
    let export = model_to_single_export ig nodeId id
    sendObject { Response_Export.mtype = "export"
                 Response_Export.export = export }