// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.GUITests

open Xunit
open FsUnit
open System.IO;
open Microsoft.Research.CRNEngine

let parse = Crn.parse |> Parser.from_string

[<Fact(DisplayName="GUI - simulate via GUI calls")>]
let simulate_via_GUI_calls () =
  let t = "directive simulation { final=20.0; points=1000; plots=[x] }
directive simulator deterministic
directive parameters [ c0 = 0.001; r = 1.0; K = 2.0; tlag=0.0 ]

directive rates [
  growth =  [grow]*r*(1 - [x] / K);
  scaled = 2.0*[growth]
]

init x c0 | init grow 1 @ tlag |

// Cell growth
->[[scaled]*[x]] x"
  let gui = JSAPI.user_parse_code t
  let model = gui.nodes |> Map.toSeq |> Seq.head |> snd
  let simruns = JSAPI.user_get_sim_runs gui ""
  JSAPI.simulateFloat gui "" simruns.instances.Head (fun _ -> ()) (fun _ -> ()) (ref false)
  ()

[<Fact(DisplayName="GUI - Plot rate expressions")>]
let plot_rates () = 
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture;
  let t = 
    "directive simulation { final=1.0; points=100; plots=[[low]]}
directive rates [ low = [X]+[Y] ]
init X 1 | init Y 1 |
X + Y ->{0.1} Z" 
  let gui = JSAPI.user_parse_code t
  let model = gui.nodes |> Map.toSeq |> Seq.head |> snd
  let simruns = JSAPI.user_get_sim_runs gui ""
  JSAPI.simulateFloat gui "" simruns.instances.Head (fun _ -> ()) (fun _ -> ()) (ref false)
  ()

[<Trait("Category", "Slow")>]
[<Fact(DisplayName="GUI - run inference graph")>]
let gui_inference_graph() =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture;
  let projectDir = Directory.GetParent(System.IO.Directory.GetCurrentDirectory()).Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
  let code = File.ReadAllText(Path.Combine(projectDir, "Models/GoalReceivers.crn"));
  let dataDir = Path.Combine(projectDir, "testData\inference");
  // Parse the IG in the back-end.
  let ig = Parser.from_string InferenceSiteGraph.parse code
  let loadedGraph = InferenceSiteGraph.load_data dataDir ig
  // Ship the IG to the front-end.
  let guiig = GuiIG.from_ig loadedGraph
  // Run the inference with the GUI functions. Ignore all results; I just want it to complete.
  let start = System.DateTime.Now;
  JSAPI.user_infer_gui guiig (fun export -> ()) (fun parameters -> ()) (fun inference_result -> ()) (ref false)
  let duration = System.DateTime.Now - start;
  ()
  
[<Trait("Category","Slow")>]
[<Fact(DisplayName="GUI - infer and infer_seq produce the same result")>]
let gui_infer_infer_seq() =
    let text = "
    directive simulation {
  final=600;
  plots=[Input1; Input2; Output; Signal]
}
directive simulator deterministic
directive parameters [
  k = 0.003, {distribution=Uniform(0.0001,1)}
]
directive inference {burnin=8; samples=5; thin=1; seed=0}
directive data [Join_data]

| 100 Reporter
| 100 Join
| 10 Input2
| 10 Input1
| Reporter + Output ->{k} sp8 + Signal
| Join + Input1 <->{k}{k} sp10 + sp9
| sp10 + Input2 <->{k}{k} sp7 + Output"
    let model = Model.from_string text
    let projectDir = Directory.GetParent(System.IO.Directory.GetCurrentDirectory()).Parent.Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
    let dataDir = Path.Combine(projectDir,"Examples","Observations")
    let settings = Io.load_data dataDir model.top.settings
    let model = {model with top = {model.top with settings=settings}}
    let mutable result = None
    let results,parameters = model.infer_seq (fun f -> result <- Some f)
    List.ofSeq results |> ignore
    let summary = result.Value.to_summary()

    Assert.Equal (8, result.Value.burnin.Length)
    Assert.Equal (5, result.Value.posterior.Length)
        
    let result2 = model.infer()
    let summary2 = result2.to_summary()

    Assert.Equal(summary,summary2)
