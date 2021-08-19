namespace Microsoft.Research.CRNEngine

open Operators
open System.Diagnostics

[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Simulator = 
  | [<WebSharper.Constant "Oslo">] Oslo
  | [<WebSharper.Constant "Sundials">] Sundials
  | [<WebSharper.Constant "SSA">] SSA
  | [<WebSharper.Constant "CME">] CME
  | [<WebSharper.Constant "CMESundials">] CMESundials
  | [<WebSharper.Constant "LNA">] LNA
  | [<WebSharper.Constant "PDE">] PDE
  | [<WebSharper.Constant "MC">] MC
  static member defaults = SSA
  member simulator.to_string =
    match simulator with
    | Oslo -> "deterministic"
    | Sundials -> "sundials"
    | SSA -> "stochastic"
    | CME -> "cme"
    | CMESundials -> "cmesundials"
    | LNA -> "lna"
    | PDE -> "pde"
    | MC -> "mc" 
  member s.plot_label (dimensions:int) (u:Units) = 
    let concentration:string = "Concentration (" + u.concentration.to_string + ")"
    let individuals:string = "Individuals"
    let space:string = "Distance (" + u.space.to_string + ")"
    let time:string = "Time (" + u.time.to_string + ")"
    match s with
    | Oslo -> time,concentration
    | Sundials -> time,concentration
    | SSA -> time,individuals
    | CME | CMESundials -> time,individuals
    | LNA -> time,individuals
    | PDE -> 
       if dimensions = 1 
       then space,time
       else space,space
    | MC -> time,individuals 
  static member parse : Parser.t<Simulator> =
    Parser.choice [ 
      Parser.kw "deterministic" >>% Oslo;
      Parser.kw "stochastic" >>% SSA;
      Parser.kw "lna" >>% LNA;
      Parser.kw "cme" >>% CME;
      Parser.kw "cmesundials" >>% CMESundials;
      Parser.kw "sundials" >>% Sundials;
      Parser.kw "pde" >>% PDE;
      Parser.kw "mc" >>% MC;
    ]
  ///TODO: should these be moved somewhere else? 
  ///Function to decide whether to output a data point to the UI.
  static member shouldPrint (end_time:float) (next_currenttime:float) (next_printtime:float) (final_step:int option) (first_loop:bool) ((*currenttime*)_:float) : bool =
    //if first_loop then currenttime=0.0 else (next_currenttime >= next_printtime) || (next_currenttime >= end_time) || (match final_step with Some n -> true | _ -> false)
    if first_loop 
    then true 
    else (next_currenttime >= next_printtime) || (next_currenttime >= end_time) || (Option.isSome final_step)
  ///Function to decide whether to stop running the simulator.
  static member shouldStop (end_time:float) (currenttime:float) (stepsdone:int) (cancel_flag:bool ref) (final_step:int option) : bool =
    (currenttime >= end_time) || (!cancel_flag) || (match final_step with None -> false | Some n -> stepsdone >= n)