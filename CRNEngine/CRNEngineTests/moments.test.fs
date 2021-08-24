// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.MomentClosureTests

open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

(* Define some models to use in tests *)

let simple_crn () = 
  ( Crn.from_string """
directive simulation { final=1.0; points=100 }

init B1 1 |
init B2 1 |

B1 ->{1.0} B1 + B1 |
B2 ->{1.0} B2 + B2 |
B1 + B2 ->{1.0}
  """
  , "B1", "B2")

let am () = 
  ( Crn.from_string """
directive simulator mc
directive simulation { final=3.0; points=1000 }
directive moments {order = 3; initial_minimum = 1e-8}
directive parameters [ k = 1.0 ]

init X 6 |
init Y 4 |
 
X + Y ->{k} Y + B |
Y + X ->{k} X + B |
B + X ->{k} X + X |
B + Y ->{k} Y + Y 
"""
  , "X", "Y" )

let kaiC order = Crn.from_string (sprintf """directive simulator mc
directive simulation { final=38.0; points=1000 }
directive moments {order = %d; initial_minimum = 1e-16}
directive parameters [k=1]

init OO 200 |
init OP 0 |
init PO 0 |
init PP 100 |
 
PP ~ OO ->{0.012} OP |
PP ~ OP ->{0.012} PP |

OO ~ PP ->{0.01} PO |
OO ~ PO ->{0.01} OO |

PP ->{0.001} PO |
OO ->{0.001} OP""" order)

let kaiC_covariance () = Crn.from_string """
directive simulator mc
directive simulation { final=38.0; points=1000 }
directive moments { order = 3
                  ; initial_minimum = 1e-16
                  ; plots = [<OO*PP> - <OO>*<PP>] // plots covariance
                  } 
directive parameters [k=1]

init OO 200 |
init OP 0 |
init PO 0 |
init PP 100 |
 
PP ~ OO ->{0.012} OP |
PP ~ OP ->{0.012} PP |

OO ~ PP ->{0.01} PO |
OO ~ PO ->{0.01} OO |

PP ->{0.001} PO |
OO ->{0.001} OP"""

/// Return some string representations for testing
let print allMoments momentClosure = 
  printfn "Before:"
  printfn "%s" (allMoments |> MC_Utils.to_string)
  printfn "\n================================\n"
  printfn "After:"
  printfn "%s" (momentClosure |> MC_Utils.to_string)

let simulate print_moments crn = 
  let allMoments = Moments.generateMoments crn
  let momentClosure = Moments.generateClosure allMoments
  if print_moments then print allMoments momentClosure   
  // Numerically integrate the closure equations
  Moments.simulate momentClosure |> snd

[<Fact(DisplayName="Moment Closure - AM")>]
let am_test() = 

  // Specify CRN
  //let crn, sp1, sp2 = simple_crn () // ND: Not a great test, because it is an unstable system!
  let crn, sp1, sp2 = am ()
  
  // Setup moment equations and the closure approximation
  let allMoments = Moments.generateMoments crn
  let momentClosure = Moments.generateClosure allMoments
  print allMoments momentClosure
  
  // Numerically integrate the closure equations
  let _,solution = Moments.simulate momentClosure
  Assert.NotEmpty solution.columns.Head.values


[<Fact(DisplayName="Moment Closure - KaiC multimolecular")>]
let kaiC_multimolecular () =

  let crn = Crn.from_string """
directive simulator mc
directive simulation { final=10.0; points=1000 }
directive moments {order = 3; initial_minimum = 1e-8}
init a 2 |
init b 2 |
init c 0 |

b + a + a ->{1.0} a + a + a |
c + b + b ->{1.0} b + b + b |
a + c + c ->{1.0} c + c + c |

a ->{1.0} b |
b ->{1.0} c |
c ->{1.0} a
  """

  (* Setup moment equations and the closure approximation.
     Using a slightly higher initial_minimum to avoid denominator-zero infinity evaluations. *)
  let solution = simulate true crn
  Assert.NotEmpty solution.columns.Head.values


[<Fact(DisplayName="Moment Closure - KaiC")>]
let kaiC2 () = 

  let crn = kaiC 3
  let solution = simulate true crn
  Assert.NotEmpty solution.columns.Head.values


[<Fact(DisplayName="Moment Closure - KaiC (via GUI functions)")>]
let kaiC2gui () = 
  System.Globalization.CultureInfo.DefaultThreadCurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let crn = kaiC 3
  let gui = Gui.from_crn crn
  let model = { GuiModel.top = gui; GuiModel.systems = [] }
  let ig = { task = None; nodes = Map.ofList ["", model]; edges=Map.empty; expanded=false}
  let runs = JSAPI.user_get_sim_runs ig ""
  for id, instance in runs.instances |> Seq.indexed do
    let crn = JSAPI.prepare_CRN_for_sim ig "" instance
    let allMoments = Moments.generateMoments crn
    let momentClosure = Moments.generateClosure allMoments 
    let plottablesCount = crn.settings.moment_closure.plots.Length
    let result = ref []  
    let output (data:Row<Point>) : unit =
      if data.values.Length <> plottablesCount 
      then failwith "unexpected data count"
      result := data::!result
    let _ = Moments.simulate_callback (ref false) output momentClosure
    let solution = Moments.process_simulation !result momentClosure
    Assert.NotEmpty solution.columns.Head.values
  ()

[<Fact(DisplayName="Moment Closure - Catalysts")>]
let mc_catalysts () = 
  let crn = kaiC 1
  let times = List.init 38 float
  let new_settings = crn.settings.update_times times
  let crn = (crn.update_settings new_settings)
  let lna = (crn.update_settings {crn.settings with simulator = LNA}).saturate_plots().to_lna()
  let resMC1 = simulate false crn
  let mc1X = (resMC1.columns |> List.find (fun c -> c.name = "E[OP]")).values |> List.map (fun p -> p.mean)
  let resLNA = lna.simulate()
  let lnaX = (resLNA.columns |> List.find (fun c -> c.name = "OP")).values |> List.map (fun p -> p.mean)
  List.iter2 (fun mc lna -> Assert.InRange (abs(mc-lna), 0.0, 0.1)) mc1X lnaX   // The variation in this example is O(100)

[<Fact(DisplayName="Moment Closure - Plots")>]
let mc_plots () = 
  let crn, X, Y = am ()
  let mc_plots = [Expression.Key [(Species.create X, 1)]]
  let crn = { crn with settings = { crn.settings with moment_closure = { crn.settings.moment_closure with plots = mc_plots } } }
  let solution = simulate false crn
  Assert.NotEmpty solution.columns.Head.values

[<Fact(DisplayName="Moment Closure - Covariance")>]
let mc_plots_covariance () = 
  let crn = kaiC_covariance ()
  let solution = simulate false crn
  Assert.NotEmpty solution.columns.Head.values

[<Fact(DisplayName="Moment Closure - Matlab export")>]
let mc_matlab_export () = 
  let crn = kaiC_covariance ()
  let allMoments = Moments.generateMoments crn
  let momentClosure = Moments.generateClosure allMoments
  let matlabCode = MC_Utils.to_matlab momentClosure  
  Assert.NotEqual<string>("", matlabCode)

[<Fact(DisplayName="Moment Closure - Latex/Matlab export")>]
let mc_latex_matlab2 () = 
  let code = "directive simulator mc
directive simulation { final=2.5; points=1000; plots = [x1; x2] }
directive parameters [r=1.0]
directive moments {order = 2; initial_minimum = 1e-16; plots = [<x1>; <x2>]}
 
-> T |
T -> |
 
x2 + T -> T + x1 |
x1 + T ->  T + x0 |
x0 + x0 ->  x0 + x1 |
x1 + x0 ->  x0 + x2
"   
  let crn = Crn.from_string code
  let allMoments = Moments.generateMoments crn
  let momentClosure = Moments.generateClosure allMoments
  let latexCode = MC_Utils.to_latex momentClosure  
  Assert.NotEqual<string>("", latexCode)
  let matlabCode = MC_Utils.to_matlab momentClosure  
  Assert.NotEqual<string>("", matlabCode)
