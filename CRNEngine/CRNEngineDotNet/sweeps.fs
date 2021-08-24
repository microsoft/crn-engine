// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Sweeps = 
  {list:Sweep list} //Dummy type
  static member create(l:(string * ((string*float list) list)) list) = 
    List.map (fun (name:string,assignments:(string*float list) list) -> Sweep.create(name,assignments)) l
  static member create(l:(string * ((string*Value list) list)) list) = 
    List.map (fun (name:string,assignments:(string*Value list) list) -> Sweep.create(name,assignments)) l
  static member create(l:(string * ((string list*float list list) list)) list) = 
    List.map (fun (name:string,assignments:(string list*float list list) list) -> Sweep.create(name,assignments)) l
  static member create(l:(string * ((string list*Value list list) list)) list) = 
    List.map (fun (name:string,assignments:(string list*Value list list) list) -> Sweep.create(name,assignments)) l
  static member get_variable_dependencies (s:Sweep list) = 
    s |> List.map (fun sw -> sw.merge)
    |> List.collect (fun mer -> mer.values |> List.map (fun vals -> vals |> List.choose (fun value -> match value with Expression.Key k -> Some k | _ -> None)))
  static member eval (e:Environment.t) (s:Sweep list) = 
    let f (s:Sweep) = List.map (fun (e:Environment.t) -> s.name,e) (s.eval e)
    if s=[] then ["",Environment.empty] else List.collect f s
  static member eval_label (e:Environment.t) (sweeps:Sweep list) = 
    let f (s:Sweep) = 
      let simplified = (s.simplify_assignments ()).eval_label e
      let full = s.eval_label e
      List.map2 (fun (assignment:string,e:Environment.t) (name,_) -> s.name,assignment,e,name) full simplified
    if sweeps=[] then ["","",Environment.empty,""] else sweeps |> List.collect f
  (*static member float_label (sweeps:Sweep list) = 
    let f (s:Sweep) = 
      let simplified = (s.simplify_assignments ()).float_label ()
      let full = s.float_label ()
      List.map2 (fun (assignment:string,e:Environment.t) (name,_) -> s.name,assignment,e,name) full simplified
    if sweeps=[] then ["","",Environment.empty,""] else sweeps |> List.collect f*)
  static member to_string (s:Sweep list) = 
    sprintf "[\n  %s;\n]" (String.concat ";\n  " <|  List.map (fun (s:Sweep) -> s.string) s )
  static member get_variables (s:Sweep list) = 
    s |> List.collect (fun s -> s.variables) |> Lib.remove_duplicates (=)
  static member add_multiples (counter:int ref) (multiples:string list) (s:Sweep list) = 
    List.unzip <| List.map (fun (s:Sweep) -> s.add_multiples counter multiples) s

