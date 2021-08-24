// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

[<JavaScript>]
//'s for species, 'v for value
type Target<'s,'v> when 's:equality =
  | Species of Species: Populations<'s,'v>
  | [<WebSharper.Constant "OutputPoint">] OutputPoint
  member t.to_string namer str_of_val separator =
    let string_of_pop_info (pi:Population<'s,'v>) = 
      " \r\n|" + str_of_val pi.value + " " + namer pi.species 
    match t with
    | Species pop -> pop.to_string_ext string_of_pop_info separator
    | OutputPoint -> ""
  member t.map_species f = 
    match t with
    | Species pop -> Species (pop.map_species f)
    | OutputPoint -> OutputPoint
  member t.map_value f = 
    match t with
    | Species pop -> Species (pop.map_population f)
    | OutputPoint -> OutputPoint