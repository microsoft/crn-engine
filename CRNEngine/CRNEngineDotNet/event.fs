// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
[<JavaScript>]
//'s for species, 'v for value
type Event<'s,'time, 'v> when 's:equality = 
  { time:'time; target:Target<'s,'v> }
  static member create (ps:Populations<'s,'v>) (t:'time) = 
    { time = t; target = Species ps }
  ///TODO: Check that this is equivalent to the definition below
  member e.to_string namer str_of_time str_of_val =
    e.target.to_string namer str_of_val (" @ " + (str_of_time e.time))
  member e.map_species f = { time = e.time; target = e.target.map_species f }
  member e.map_time f = { time = f e.time; target = e.target }
  member e.map_value f = { time = e.time; target = e.target.map_value f }

  (*
    member e.to_string namer str_of_time str_of_val =
    let string_of_pop_info (pi:Population<'s,'v>) =
      "event " + namer pi.species + " " + str_of_val pi.value + " @ " + (str_of_time e.time) in
    let target_str =
      match e.target with
      | Species pop -> pop.to_string_ext string_of_pop_info " |\r\n"
      | OutputPoint -> "" in
    target_str
  *)
  (*let inline_env_target f = function
    | Species pop -> Species (Populations.eval f pop)
    | OutputPoint -> OutputPoint

  let inline_env f ev = 
    { time = f ev.time
    ; target = inline_env_target f ev.target }

  let eval env ev = inline_env env ev*)

  (* Use this when we have a syntax for populations in events
     The below code just splits a population into multiple events
  let to_string namer str_of_val (e: ('s, 'v) t) =
    let string_of_pop_info (pi: ('s, 'v) Populations.pop_info) =
      namer pi.calculus_species + " " + str_of_val pi.population in
    let target_str = match e.target with
    | Parameter (p, v) -> p.name + " " + (str_of_val v)
    | Species pop -> Populations.to_string_ext pop string_of_pop_info ", " in
    "event " + target_str + " @ " + (str_of_val e.time)
  *)