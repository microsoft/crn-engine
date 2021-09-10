// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScriptExport>]
module Microsoft.Research.GEC.JSAPI

open Microsoft.Research.GEC.GECEngine

#if JavaScript
open WebSharper
#endif

open Microsoft.Research.CRNEngine

// ND: SBOL support currently disabled. Awaiting on two developments:
//  1. Updates to sboljs that pass component governance checks
//  2. Updates to FSBOL that enable TypesTSFS to complete successfully
//open FSBOL
//open FSBOL.SBOLDocument
//open FSBOL.JsonSerializer


type ClassicResult = { solution : t
                     ; solutionCount: int
                     ; model : GuiIG }
                     //; jsbol : rSBOLDocument
                     //; sbol : SBOLDocument }

type LogicResult = { solution : t
                   ; solutionCount: int
                   ; model : GuiIG }
                   //; jsbol : rSBOLDocument
                   //; sbol : SBOLDocument }

type solve_result = ClassicGEC of ClassicResult | LogicGEC of LogicResult

let compile (program:string) (dbParts:string) (dbReactions:string) : solve_result =
  let output = GECEngine.solveGEC (ref false) program dbParts dbReactions
  let graph = GuiIG.from_ig output.graph
  //let jsbol = JsonSerializer.sbolToJson output.sbol
  let scount = 
    match output.solution.solution with 
    | Some(_,sol,_,_,_) -> sol.numSolutions 
    | None -> failwith "Output of solution is null" 
  ClassicGEC { model = graph; solution = output.solution; solutionCount= scount(*; jsbol = jsbol; sbol = output.sbol*) }

type solution_result = { model : GuiIG
                       //; jsbol : rSBOLDocument
                       //; sbol : SBOLDocument 
                       ; crnstring : string}

let get_solution (so:solve_result) (i:int) : solution_result =
  match so with 
  | ClassicGEC o ->
    let model = o.model.to_ig()   

    //let model = o.model.nodes |> Map.toSeq |> Seq.head |> snd
    //let gmodel = model.to_model()
    let result = GECEngine.getCrnAssignment model o.solution (i-1)
    let model = GuiIG.from_ig result.model
    //let jsbol = JsonSerializer.sbolToJson result.sbol
    { model = model 
    //; jsbol = jsbol 
    //; sbol = result.sbol 
    ; crnstring = result.model.to_string()}
  | LogicGEC o -> failwith "Logic GEC solution selection not implemented yet."