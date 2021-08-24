// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
open System.Diagnostics
[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Sweep =
  {name:string; assignments:Assignment list}
  static member create() = {name = ""; assignments = []}
  static member create(name:string, assignments:Assignment list) = {name = name; assignments = assignments}
  static member create(assignments:Assignment list) = Sweep.create("",assignments)
  static member create(name:string, l:(string*float list) list) = 
    Sweep.create(name, List.map (fun (a:string,b:float list) -> Assignment.create(a,b)) l)
  static member create(name:string, l:(string*Value list) list) = 
    Sweep.create(name, List.map (fun (a:string,b:Value list) -> Assignment.create(a,b)) l)
  static member create(name:string, l:(string list*float list list) list) = 
    Sweep.create(name, List.map (fun (a:string list,b:float list list) -> Assignment.create(a,b)) l)
  static member create(name:string, l:(string list*Value list list) list) = 
    Sweep.create(name, List.map (fun (a:string list,b:Value list list) -> Assignment.create(a,b)) l)
  static member create(text:string) = Parser.from_string Sweep.parse text
  member s.variables:string list = s.assignments |> List.collect (fun a -> a.variables) |> Lib.remove_duplicates (=)
  member s.string:string = s.name + " = [" + (String.concat "; " <| List.map (fun (a:Assignment) -> a.string) s.assignments) + "]"
  member s.simplify_assignments () = { s with assignments = s.assignments |> List.filter (fun a -> a.values.Length > 1) }
  member s.merge:Assignment= 
    let f1 (result:Assignment) (a:Assignment) =
      let f2 (vss:Value list list) (vs':Value list) = 
        let f3 (vs:Value list) = 
          let m:Map<string,Value> = Map.ofList (List.zip result.variables vs)
          let vs':Value list = List.map (Expression.substitute m) vs'
          vs@vs'
        List.map f3 vss
      Assignment.create(result.variables @ a.variables , List.concat (List.map (f2 result.values) a.values))
    List.fold f1 (Assignment.create()) s.assignments
  member s.inline_env (env:Environment.t) = { s with assignments = s.assignments |> List.map (fun a -> a.partial_inline env)}
  member s.eval (e:Environment.t) : Environment.t list = (s.merge).eval e
  member s.eval_label (e:Environment.t) : (string*Environment.t) list = (s.merge).eval_label e
  //member s.float_label () : (string*Environment.t) list = (s.merge).float_label ()
  member s.add_multiples (counter:int ref) (multiples:string list) : string list list * Sweep = 
    let (fresh_names_list:string list list),(a:Assignment) = s.merge.add_multiples counter multiples
    in fresh_names_list,Sweep.create(s.name, [a])
  member private s.merge_eval_combined (global_env:Environment.t) : Environment.t list = 
    let f1 (rs:Environment.t list) (a:Assignment) = 
      let f2 (r:Environment.t) = 
        let global_env:Environment.t = Environment.extend r global_env
        List.map (Environment.extend r) (a.eval global_env)
      List.concat (List.map f2 rs)
    List.fold f1 [Environment.empty] s.assignments
  member s.mentions = s.assignments |> List.collect (fun ass -> ass.mentions |> List.distinct)
  static member parse:Parser.t<Sweep> = 
    Parser.name .>> Parser.skw "=" .>>. (Parser.list_of Assignment.parse) |>> fun (x,y) -> Sweep.create(x,y)
  static member list_to_string (l:Sweep list) = 
    sprintf "[\n  %s;\n]" (String.concat ";\n  " <|  List.map (fun (s:Sweep) -> s.string) l )
