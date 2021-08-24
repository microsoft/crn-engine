// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Assignment = 
  ///variables: Tuple of variables being assigned.
  ///values: List of value tuples to assign. 
  ///Each value tuple represents an assignment to the variable tuple.
  ///The value tuple and variable tuple are assumed to be the same length 
  {variables:string list; values:Value list list}
  static member create(variables:string list, values:Value list list) = {variables = variables; values = values}
  static member create() = {variables=[]; values=[[]]} 
  static member create(variable:string, floats:float list) = Assignment.create([variable],List.map (fun v -> [Expression.Float v]) floats)
  static member create(variable:string, values:Value list) = Assignment.create([variable], [values])
  static member create(variables:string list, floats:float list list) = Assignment.create(variables, List.map (fun l -> List.map (fun v -> Expression.Float v) l) floats)
  static member create(text:string) = Parser.from_string Assignment.parse text
  ///String representation.
  member a.string:string =
    let varsTuple = Lib.tuple_to_string a.variables
    let valuesTuples = 
      a.values 
      |> List.map (List.map (Expression.to_string id) >> Lib.tuple_to_string) 
      |> String.concat "; "
    sprintf "%s = [%s]" varsTuple valuesTuples
  ///List of strings, each representing an assignment of a value tuple to the variable tuple.
  member a.to_string_bindings:string list = 
    let varsTuple = Lib.tuple_to_string a.variables
    let valTuples = a.values |> List.map (List.map (Expression.to_string id) >> Lib.tuple_to_string) 
    List.map (fun tup -> varsTuple + " = " + tup) valTuples
  ///List of strings, each representing an assignment of values to variables
  member a.to_string_separated:string list = 
    let values:string list list = List.map (List.map (Expression.to_string id)) a.values 
    List.map (fun vs -> List.map2 (sprintf "%s = %s") a.variables vs |> String.concat "; ") values
  member a.partial_inline (e:Environment.t) : Assignment = 
    let emap = Map.map (fun _ v -> Expression.Float v) e
    { a with values = a.values |> List.map (List.map (Expression.substitute emap))}
  ///Uses environment 'e' to evaluate all values to floats.
  ///Returns a list of environments, each representing an assignment of a float tuple to the variable tuple.
  member a.eval (e:Environment.t) : Environment.t list = 
    let f (values:Value list) = 
      let floats:float list = List.map (Expression.eval (Environment.find e)) values 
      Map.ofList (List.zip a.variables floats)
    List.map f a.values
  ///Returns a list of string*environment, the string represents the assignment of values to variables
  ///The environment represents the assignment of floats to variables. 
  member a.eval_label (e:Environment.t) : (string*Environment.t) list = 
    List.zip (List.map (sprintf "[%s]") a.to_string_separated) (a.eval e)
  (*member a.float_label () : (string * Environment.t) list =
    let f (values:Value list) = 
      let float_vars = List.zip a.variables values |> List.choose (fun (k,v) -> match v with Value.Float f -> Some (k,f) | _ -> None)
      let label = float_vars |> List.map (fun (k,v) -> sprintf "%s = %s" k (v.ToString())) |> String.concat "; "
      label, Map.ofList float_vars
    List.map f a.values*)
  ///Adds 'multiples' to the variables being assigned.
  ///Adds a fresh name for each member of 'multiples' to each value tuple.
  ///Returns a list of fresh name tuples, together with the updated assignment.
  member a.add_multiples (counter:int ref) (multiples:string list) : string list list * Assignment = 
    let f (vs:Value list) = 
      counter := !counter + 1; 
      let f (name:string) = name + "_" + string !counter 
      let fresh_names:string list = List.map f multiples 
      (fresh_names, vs @ (List.map Expression.Key fresh_names)) 
    let fresh_names_list,values_list = List.unzip (List.map f a.values)
    fresh_names_list, Assignment.create(a.variables@multiples,values_list)
  member a.mentions = a.variables @ (a.values |> List.concat |> List.collect Expression.mentions)
  ///Parser
  static member parse:Parser.t<Assignment> =
    let parse_variables = Parser.tuple_of Parser.name
    let parse_values = Parser.list_of <| Parser.tuple_of (Expression.parse Parser.name)
    (parse_variables .>> Parser.skw "=") .>>. parse_values |>> fun (vars, vals) -> Assignment.create(vars,vals)

