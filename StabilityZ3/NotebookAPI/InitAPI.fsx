// Load script for stability-Z3 API

// CRN engine
#r @"../NotebookAPI/bin/Release/net472/ParserCombinators.dll"
#r @"../NotebookAPI/bin/Release/net472/Oslo.FSharp.dll"
#r @"../NotebookAPI/bin/Release/net472/ReactionDiffusion.dll"
#r @"../NotebookAPI/bin/Release/net472/ParserCombinators.dll"
#r @"../NotebookAPI/bin/Release/net472/Filzbach.FSharp.Portable.dll"
#r @"../NotebookAPI/bin/Release/net472/WebSharper.Core.dll"
#r @"../NotebookAPI/bin/Release/net472/CRNEngineDotNet.dll"

// Z3
#r @"../NotebookAPI/bin/Release/net472/Microsoft.Z3.dll"

// MSAGL and charting dependencies
#r @"../NotebookAPI/bin/Release/net472/Microsoft.Msagl.dll"
#r @"../NotebookAPI/bin/Release/net472/Microsoft.Msagl.Drawing.dll"
#r @"../NotebookAPI/bin/Release/net472/FSharp.Plotly.dll"
#r @"../NotebookAPI/bin/Release/net472/XPlot.Plotly.dll"

// StabilityZ3 build
#r @"../NotebookAPI/bin/Release/net472/StabilityZ3.dll"
#r @"../NotebookAPI/bin/Release/net472/NetworkVisualization.dll"
#r @"../NotebookAPI/bin/Release/net472/GRNs.dll"
#r @"../NotebookAPI/bin/Release/net472/CRNs.dll"
#r @"../NotebookAPI/bin/Release/net472/NotebookAPI.dll"

// Temporary link to IfSharp DLL (for debugging)
//#r @"../../../IfSharp/src/IfSharp/bin/Debug/IfSharp.Kernel.dll"
//#r @"../../../IfSharp/src/IfSharp/bin/Debug/IfSharp.exe"

namespace API

open Microsoft.Research.Biology
open Microsoft.Research.Biology.StabilityZ3.NotebookAPI
open Microsoft.Research.CRNEngine

module Init = 
  printfn "Loading the SMT-based Dynamics Analysis Library (SDAL)..."

module LinearCrn =  

  let htmlView s = IfSharp.Kernel.Util.Html s |> IfSharp.Kernel.App.Display
  let Encode = StabilityZ3.LinearCRN.Encode
  let CrnsToSvgTable = LinearCrnAPI.CrnsToSvgTable htmlView
  let RDNetsCompare = LinearCrnAPI.RDNetsCompare htmlView
  let Compute = LinearCrnAPI.Compute htmlView IfSharp.Kernel.App.Display
  let Solve = LinearCrnAPI.Solve htmlView

module Dynamical =
  
  let htmlView s = IfSharp.Kernel.Util.Html s |> IfSharp.Kernel.App.Display
  let latexView s = IfSharp.Kernel.Util.Latex s |> IfSharp.Kernel.App.Display

  let FromCRN = StabilityZ3.Dynamical.fromCRN
  let Create (odes,diff:string list) = StabilityZ3.Dynamical.Create (odes,diff)
  let SetAllLB = StabilityZ3.Dynamical.setAllLB

  let ToLatex (d:StabilityZ3.Dynamical) = d |> StabilityZ3.Dynamical.toString |> IfSharp.Kernel.Util.Latex
  let DisplayEquations = DynamicalAPI.DisplayEquations htmlView
  let DisplayJacobian S = DynamicalAPI.DisplayJacobian htmlView S
  let DisplayParameters S = DynamicalAPI.DisplayParameters htmlView S
  let DisplayWavenumbers S = DynamicalAPI.DisplayWavenumbers IfSharp.Kernel.App.Display S  
  let PrintTuringCst S = DynamicalAPI.PrintTuringCst latexView S

  let CheckEquilibrium = DynamicalAPI.CheckEquilibrium htmlView

  let CheckBistabilityAndReturn S = DynamicalAPI.CheckBistabilityAndReturn htmlView S
  let CheckBistability S = DynamicalAPI.CheckBistability htmlView S

  let CheckTuringSettings = DynamicalAPI.TuringAnalysis
  let TuringAnalysisDefault = DynamicalAPI.TuringAnalysisDefault
  let CheckTuringAndReturn = DynamicalAPI.CheckTuringAndReturn htmlView
  let CheckTuring S = DynamicalAPI.CheckTuring htmlView S
  let CheckTuringRelaxed = DynamicalAPI.CheckTuringRelaxed htmlView
  let CheckTuringStrict = DynamicalAPI.CheckTuringStrict htmlView
  let CheckTuringReduced = DynamicalAPI.CheckTuringReduced htmlView
  let CheckCrn = DynamicalAPI.CheckCrn
  let EnumerateTuring = DynamicalAPI.EnumerateTuring

module Crn = 

  let htmlView s = IfSharp.Kernel.Util.Html s |> IfSharp.Kernel.App.Display
  let SimCRN = CrnAPI.SimCRN IfSharp.Kernel.App.Display 
  let SimCRNStr = Crn.from_string >> SimCRN
  let SimPattern = CrnAPI.SimPattern htmlView
  //let SimPatternMovie = NotebookAPI.CrnAPI.SimPatternMovie htmlView
  let Parse = Crn.from_string
  let CheckEquilibrium = Dynamical.FromCRN >> Dynamical.CheckEquilibrium
  let CheckTuring = Dynamical.FromCRN >> Dynamical.CheckTuring
  let CheckBistability = Dynamical.FromCRN >> Dynamical.CheckBistability
  let PlotBistability = CrnAPI.PlotBistability htmlView