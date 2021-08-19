module Microsoft.Research.CRNEngine.Tests.ModelTests

open Xunit
open FsUnit
open FsUnit.Xunit
open Microsoft.Research.Filzbach
open System.Diagnostics
open System.IO
open Microsoft.Research.CRNEngine
    
[<Fact(DisplayName="Model - Basic CRN Model Parser Test")>]
let moduleBasicParseTest() = 
    let crn = "directive simulation { final=1.0; points=10000 }
directive parameters [r = 1.0]
directive simulator cme

X + Y ->{r} X + B |
Y + X ->{r} Y + B |
X + B ->{r} X + X |
Y + B ->{r} Y + Y |

init X 30 |
init Y 20 "
    let from_string (s:string) = Parser.from_string Model.parse s
    let mt = from_string crn
    Debug.WriteLine("End of test")

[<Fact(DisplayName="Model - Systems sim test")>]
let moduleSystemsSimTest() =
   let projectDir = Directory.GetParent(System.IO.Directory.GetCurrentDirectory()).Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
   let code = File.ReadAllText(Path.Combine(projectDir, "Models/GoalReceivers.crn"));
   let inferencegraph = InferenceSiteGraph.from_string code
   let expanded = InferenceSiteGraph.expandAndLift inferencegraph
   expanded.nodes |> Map.toSeq |> Seq.map (fun (_,n) -> n) |> Seq.iter (fun m -> m.simulate() |> ignore)
   ()