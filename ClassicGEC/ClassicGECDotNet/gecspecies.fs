// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.GEC.GecSpecies

open Parser
open Microsoft.Research.CRNEngine

type species = string list

let species_to_gecAbstractComplex (sp:species) = 
    sp |> List.map (fun x -> 
      match x with 
      | "_" -> Ast.WildCardVal
      | _ -> Ast.IdVal(x))

type t = {
  compartment: string option
  species:species
}
with
  static member empty_Species = 
    {species = []; compartment=None}
  member s.to_string() = 
    match s.compartment with 
    | Some(comp) -> comp + "[" + (Lib.string_of_list (fun x -> x)  "::" s.species) + "]" 
    | None -> (Lib.string_of_list (fun x -> x)  "::" s.species)
  member s.to_crn_string() = 
    match s.compartment with 
    | Some(comp) -> comp + "_" + (Lib.string_of_list (fun x -> x)  "_" s.species)
    | None -> (Lib.string_of_list (fun x -> x)  "_" s.species)
  member s.to_crn_species() = Species.create(s.to_crn_string())
  member s.to_ast_gecSpecies() = 
    match s.compartment with 
    | Some(x) ->  Ast.CompartmentSpecies(x,species_to_gecAbstractComplex s.species)
    | None -> Ast.SimpleSpecies(species_to_gecAbstractComplex s.species)

let SPECIES_SEP = Parser.pstring "::"

let pName = Parser.name_kw Keywords.kwList

let parse_species_ns = Parser.sepBy (pName) SPECIES_SEP
let parse_species = parse_species_ns .>> Parser.spaces

let parse_kw (keywords:string list) = 
  Parser.plookAheadWith(
    Parser.choice[
      Parser.pTry((Parser.name_kw keywords) .>> Parser.pstring "[" >>= fun _ -> Parser.preturn true)
      Parser.preturn false
    ])
    >>= 
    fun (hasCompartment) -> 
      if hasCompartment then
        pName .>> Parser.pstring "[" .>>. parse_species_ns .>> Parser.pstring "]" .>> Parser.spaces |>> fun(x,y) -> {species=y;compartment=Some(x)}
      else 
        parse_species |>> fun y -> {species=y;compartment=None}
  
let parse = parse_kw Keywords.kwList
  
  
let parse_crn_species = parse |>> fun x -> x.to_crn_string() |> Species.create 
let parse_gec_to_crn_species = parse_species |>> fun x -> Lib.string_of_list (fun x -> x)  "_" x |> Species.create

