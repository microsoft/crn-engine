// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CliLibrary.Synthesis

open Microsoft.Research.CRNEngine
open Microsoft.Research.Biology.StabilityZ3

let run (model:Model) = 
  model.top :: model.systems
  |> List.iter (fun crn ->
    let dyn = Dynamical.fromCRN crn
    let timeout = crn.settings.synthesis.timeout
    let solver = 
      match crn.settings.synthesis.solver with
      | Z3Solver.NLSat -> match timeout with Some t -> Solver.NlsatTO ((uint32 t)*1000u) | None -> Solver.Nlsat
      | Z3Solver.Portfolio -> match timeout with Some t -> Solver.PortfolioTO ((uint32 t)*1000u) | None -> Solver.Portfolio
    let res = 
      match crn.settings.synthesis.mode with
      | Synthesis_mode.Multistability ->
        printfn "Synthesizing parameters for multistability..."
        dyn |> Solver.CheckBistability true solver false crn.settings.synthesis.seed
      | Synthesis_mode.Turing ->
        printfn "Synthesizing parameters for Turing instability..."
        let turingSettings = TuringSymbolic.TuringAnalysisSettings.Default
        dyn |> Solver.CheckTuring true solver { turingSettings with print_status = false; seed = crn.settings.synthesis.seed }
    match res.solution with
    | AnalysisResult.UNSAT _ -> printfn "Unsatisfiable"
    | AnalysisResult.UNKNOWN b -> printfn "No solution found after %1.1f seconds" b.time
    | AnalysisResult.FAILED _  -> printfn "Synthesis procedure failed"
    | AnalysisResult.INCOMPLETE _ -> printfn "Synthesis procedure did not complete"
    | AnalysisResult.SAT (p, b) -> 
        printfn "Solution found after %1.1f seconds" b.time
        printfn "\n================"
        printfn "Variable | Value"
        printfn "----------------"
        Map.iter (printfn "%8s | %1.1f") p
        printfn "----------------"
  )
