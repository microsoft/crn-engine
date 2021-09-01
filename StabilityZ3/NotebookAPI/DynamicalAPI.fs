// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.NotebookAPI.DynamicalAPI

open Microsoft.Research.Biology.StabilityZ3
open FSharp.Plotly

let ENCODE_TO_Z3 = true //for larger systems, it is more efficient to avoid the intermediate constraints
let default_solver = Solver.PortfolioTO (uint32 10000)

//Plot the dispersion relation of a dynamical system that has been solved for Turing-ness
let PlotWaves (S:Dynamical) = 
    match S.solution with
    | SAT (v,_) -> 
      let X = MathNet.Numerics.Generate.LogSpaced(101,-2.0,2.0)
      let Y = TuringNumerical.dispersion X S
      
      let x0, y0 = X.[0],Y.[0]
      let x1, y1 = Array.zip X Y |> Array.maxBy snd
      let charts = 
        [ Chart.Scatter (X, Y, StyleParam.Mode.Lines)
        ; Chart.Scatter ([x0; x1], [y0; y1], StyleParam.Mode.Markers)
        ]
      Chart.Combine charts
      |> Chart.withX_Axis (Axis.LinearAxis.init(StyleParam.AxisType.Log))
      |> Chart.withX_AxisStyle ("Wavenumber, w")
      |> Chart.withY_AxisStyle ("max(Re(l)")
      |> Chart.withLegend false
      |> Chart.withMarginSize (Top=5, Bottom=40, Right=15)
      |> Chart.withSize (450.0, 250.0)
    | _   -> failwith "No solution"
            
let PrintResult disp S = 
  match S.solution with
  | SAT (v,t) -> 
    v
    |> Map.toArray
    |> Array.map(fun (v,f) -> sprintf "%s\t= %g;" v f)
    |> String.concat "\n"
    |> printfn "A satisfying solution was synthesized with the following parameters (%f sec) :\n\n%s" t.time
    |> disp   
  | UNSAT t   -> printfn "No solution exists (UNSAT). Done in %f sec" t.time
  | UNKNOWN t -> printfn "No solution was identified (UNKNOWN). Done in %f sec" t.time
  | FAILED (msg,t) -> printfn "Analysis failed [%s] Done in %f sec" msg t.time
  | _         -> printfn "No solution was available"        



let TuringAnalysis (s:TuringSymbolic.TuringAnalysisSettings) S = 
    let solver = 
        match s.timeout with
        | Some timeout -> Solver.PortfolioTO (uint32 (1000*timeout))
        | None -> Solver.Portfolio
    S |> Solver.CheckTuring ENCODE_TO_Z3 solver s

let TuringAnalysisDefault S = S |> Solver.CheckTuring ENCODE_TO_Z3 default_solver TuringSymbolic.TuringAnalysisSettings.Default
    
let ToSVG S = Visualization.ToSVG 300.0 S


let DisplayEquations htmlView (S:Dynamical) = 
    S.ToText true true true
    //|> Dynamical.ToText true
    |> sprintf "<center><h3>System equations:</h3>%s</center>"
    |> htmlView
    S

let DisplayJacobian htmlView S =
    match S.solution with
    | SAT _ -> 
        S
        |> Visualization.ToSVG 300.0
        |> sprintf "<center><h3>Jacobian visualization:</h3>%s</center>"
        |> htmlView
        S
    | _ -> S
    
let DisplayParameters htmlView S = 
    match S.solution with
    | SAT (v,_) ->     
        v
        |> Map.toArray
        |> Array.map(fun (v,f) -> sprintf "%s\t= %.2g;" v f)
        |> String.concat "\n"
        |> sprintf "<center><h3>Parameters:</h3></center>%s"
        |> htmlView
        S
    | _ -> S

let DisplayWavenumbers disp S =
    match S.solution with
    | SAT _ -> PlotWaves S |> disp; S//; FSChart.PlotWavesToSVG S; S
    | _     -> S
    
let PrintTuringCst dispLatex S = 
    let str = 
        S
        |> TuringSymbolic.Turing TuringSymbolic.TuringAnalysisSettings.Default  //settings used to be {settings with prevent_noise_amp = No; prevent_oscillations = false}?
        |> fst
        |> Array.map (MathNetWrapper.Simplify)
        |> Array.distinct
        |> Array.map (Microsoft.Research.CRNEngine.Expression.to_string_bool id)
        |> String.concat "\\\\"
        |> sprintf "\\begin{align}%s\\end{align}" 
    str.Replace("&&", "\quad\wedge\\\\").Replace(">=", "&\geq").Replace("<=", "&\leq").Replace("=", "&=").Replace("<", "&<")
    |> dispLatex
    //S
        


let GetWMax S = 
    S
    |> TuringNumerical.check                
    |> snd
    ||> Array.zip
    |> Array.maxBy snd

//let EnumerateEqulibria mx S =
//    S 
//    |> Solver.EnumerateEquilibria ENCODE_TO_Z3 solver mx        
    
let DisplayTabulatedTuring htmlView (S:Dynamical) = 
    let equations = S.ToText true true true
    let jacobian = match S.solution with SAT _ -> S |> Visualization.ToSVG 300.0 | _ -> ""
    let dispersion = PlotWaves S |> FSharp.Plotly.GenericChart.toChartHtmlWithSize 450 250
    let parameters = 
        match S.solution with 
        | SAT (v,_) -> 
            v
            |> Map.toList
            |> List.map(fun (v,f) -> sprintf "<tr><td>%s</td><td>%.2g</td>" v f)
            |> List.append ["<tr><th>Name</th><th>Value</th></tr>"]
            |> String.concat "\n"
            |> sprintf "<center><table>%s</table></center>"
        | _ -> ""    
    sprintf "<center><table>
    <tr><th>Equations</th><th>Solution</th></tr>
    <tr><td style=\"width:600px\">%s</td><td>%s</td></tr>
    <tr><th>Dispersion relation</th><th>Jacobian</th></tr>
    <tr><td>%s</td><td>%s</td></tr>
    </table></center>
    " equations parameters dispersion jacobian
    |> htmlView
    S


let DisplayTabulatedBistability htmlView (S:Dynamical) = 
    let equations = S.ToEquationsText true true
    //let jacobian = match S.solution with SAT _ -> S |> Visualization.ToSVG 300.0 | _ -> ""
    //let dispersion = PlotWaves S |> FSharp.Plotly.GenericChart.toChartHtmlWithSize 450 250
    let parameters = 
        match S.solution with 
        | SAT (v,_) -> 
            v
            |> Map.toList
            |> List.map(fun (v,f) -> sprintf "<tr><td>%s</td><td>%.2g</td>" v f)
            |> List.append ["<tr><th>Name</th><th>Value</th></tr>"]
            |> String.concat "\n"
            |> sprintf "<center><table>%s</table></center>"
        | _ -> ""    
    sprintf "<center><table>
    <tr><th>Equations</th><th>Solution</th></tr>
    <tr><td style=\"width:600px\">%s</td><td>%s</td></tr>
    </table></center>
    " equations parameters
    |> htmlView
    S

let CheckEquilibriumAndReturn htmlView S = 
  let S' = S |> Solver.CheckEquilibrium ENCODE_TO_Z3 default_solver        
  if S'.solution.isSAT
  then DisplayTabulatedBistability htmlView S' |> ignore
  else printfn "%s (%f sec)" S'.solution.Status S'.solution.Time
  S'
let CheckEquilibrium htmlView = CheckEquilibriumAndReturn htmlView >> ignore

let CheckTuringAndReturn htmlView settings S = 
    let S' = TuringAnalysis settings S
    if S'.solution.isSAT 
    then DisplayTabulatedTuring htmlView S' |> ignore
    else printfn "%s (%f sec)" S'.solution.Status S'.solution.Time
    S'

//let CheckTuringAndReturn = TuringAnalysis settings >> fst3 >>  DisplayWavenumbers
let CheckTuring htmlView = CheckTuringAndReturn htmlView TuringSymbolic.TuringAnalysisSettings.Default >> ignore
let CheckTuringRelaxed htmlView = CheckTuringAndReturn htmlView TuringSymbolic.TuringAnalysisSettings.Relaxed >> ignore
let CheckTuringStrict htmlView = CheckTuringAndReturn htmlView TuringSymbolic.TuringAnalysisSettings.Strict >> ignore
let CheckTuringReduced htmlView = CheckTuringAndReturn htmlView {TuringSymbolic.TuringAnalysisSettings.Default with model_reduction=TuringSymbolic.On} >> ignore
let CheckCrn Dr htmlView = Dynamical.fromCRN >> Dynamical.setAllLB 0.0 >> Dynamical.setUB Dr 1.0 >> CheckTuring htmlView
let EnumerateTuring unique S = Solver.EnumerateTuring ENCODE_TO_Z3 default_solver TuringSymbolic.TuringAnalysisSettings.Default unique S

let CheckBistabilityAndReturn htmlView S =    
  let S' = Solver.CheckBistability ENCODE_TO_Z3 default_solver true None S
  if S'.solution.isSAT
  then DisplayTabulatedBistability htmlView S' |> ignore
  else printfn "%s (%f sec)" S'.solution.Status S'.solution.Time
  S'
let CheckBistability htmlView = CheckBistabilityAndReturn htmlView >> ignore
