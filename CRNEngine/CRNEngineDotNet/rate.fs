// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

open Operators

[<JavaScript>] 
[<WebSharper.NamedUnionCases>]
type Rate<'v,'e> = 
  | MassAction of MassAction:'v
  | Function of Function:'e
  member r.map (fv:'v -> 'v2) (fe:'e -> 'e2) =
    match r with
    | MassAction(v) -> MassAction(fv v)
    | Function(e) -> Function(fe e) 
  member r.fold (fv:'v -> 'output) (fe:'e -> 'output) =
    match r with
    | MassAction(v) -> fv v
    | Function(e) -> fe e 
  member r.to_string (fv:'v -> string) (fe:'e -> string) = r.fold (fun v -> "{" + fv v  + "}") (fun e -> "[" + fe e + "]") 
  member r.to_string_bare (fv:'v -> string) (fe:'e -> string) = r.fold fv fe
  static member from_gui (pv:string -> 'v) (pe:string -> 'e) (g:Rate<string,string>) = g.map pv pe
  static member parse (pv:Parser.t<'v>) (pe:Parser.t<'e>) = 
    (Parser.bracket "{" "}" pv |>> Rate.MassAction) <|> 
    (Parser.bracket "[" "]" pe |>> Rate.Function )
  static member map2 (fv:'v -> 'v2) (fe:'e -> 'e2) (r:Rate<'v,'e>) = r.map fv fe
