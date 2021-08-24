// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//[<JavaScript>]
namespace Microsoft.Research.CRNEngine
open Top

type Assignments(list:Assignment list) =
  new () = Assignments(list = []) 
  new (l:(string*float list) list) = 
    Assignments(List.map (fun (a:string,b:float list) -> Assignment(a,b)) l)
  new (l:(string list*float list list) list) = 
    Assignments(List.map (fun (a:string list,b:float list list) -> Assignment(a,b)) l)
  new (l:(string list*Value list list) list) = 
    Assignments(List.map (fun (a:string list,b:Value list list) -> Assignment(a,b)) l)
  new (text:string) = 
    let a:Assignments = Parser.from_string Assignments.parse text
    Assignments(list = a.list)
  member a.list:Assignment list = list
  member a.string:string = "[" + (String.concat "; " <| List.map (fun (a:Assignment) -> a.string) a.list) + "]"
  member a.variables:string list = a.list |> List.collect (fun a -> a.variables) |> Lib.remove_duplicates (=)
  member a.merge:Assignment = 
    let f1 (result:Assignment) (a:Assignment) =
      let f2 (vss:Value list list) (vs':Value list) = 
        let f3 (vs:Value list) = 
          let m:Map<string,Value> = Map.ofList (List.zip result.variables vs)
          let vs':Value list = List.map (Expression.substitute m) vs'
          vs@vs'
        List.map f3 vss
      Assignment(result.variables @ a.variables , List.concat (List.map (f2 result.values) a.values))
    List.fold f1 (Assignment()) a.list
  member a.merge_eval (e:Environment.t) : Environment.t list = (a.merge).eval e
  member a.add_multiples (counter:int ref) (multiples:string list) : string list list * Assignments = 
    let (fresh_names_list:string list list),(a:Assignment) = a.merge.add_multiples counter multiples
    in fresh_names_list,Assignments([a])
  member private a.merge_eval_combined (global_env:Environment.t) : Environment.t list = 
    let f1 (rs:Environment.t list) (a:Assignment) = 
      let f2 (r:Environment.t) = 
        let global_env:Environment.t = Environment.extend r global_env
        List.map (Environment.extend r) (a.eval global_env)
      List.concat (List.map f2 rs)
    List.fold f1 [Environment.empty] a.list
  static member parse:Parser.t<Assignments> = 
    (Parser.list_of Assignment.parse) |>> fun l -> Assignments(l) 