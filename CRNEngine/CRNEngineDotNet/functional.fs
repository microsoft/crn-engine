// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Functional = Expression.t<Key<Species>>
[<JavaScript>]
module Functional2 =
  let create (s:Species) = Expression.Key (Key.Species s)
  let rec all_species (rates:Map<string,Functional>) (f:Functional) = 
    Expression.mentions f
    |> List.collect (fun k -> 
      match k with
      | Key.Species s -> [s]
      | Key.Rate r    -> all_species rates rates.[r]
      | _             -> []
    )
  let zero_absent_species positive_species rates = 
    let species_in_rates = rates |> Map.toList |> List.collect (snd >> all_species rates)
    let zeros = (Set.ofList species_in_rates) - (Set.ofList positive_species) |> Set.toList
    let zero_map = zeros |> List.map (fun s -> Key.Species s, Expression.Float 0.0) |> Map.ofList
    rates |> Map.map (fun _ v -> Expression.substitute zero_map v)
  let parse:Parser.t<Functional> = Expression.parse (Key.parse Species.parse)
  ///Special case: a single name x is interpreted as a species instead of a parameter
  ///e.g. "plots = [x]"
  let parse_plot:Parser.t<Functional> = 
    Expression.parse (Key.parse Species.parse)
    |>> fun exp -> 
          match exp with
          | Expression.Key (Key.Parameter s) -> Expression.Key (Key.Species (Species.from_string s))
          | _ -> exp
  let to_string (e:Functional) = Expression.to_string (Key.to_string Species.to_string) e
  let to_string_plot (e:Functional) = 
    match e with
    | Expression.Key (Key.Species s) -> Species.to_string s
    | _ -> to_string e
  let to_matlab (e:Functional) = Expression.to_string (Key.to_matlab Species.to_string "time")
  let from_string (s:string) = Parser.from_string parse s
  let from_string_plot (s:string) = Parser.from_string parse_plot s
  let map (f:Species -> Species) (e:Functional) = Expression.map (Key.map f) e