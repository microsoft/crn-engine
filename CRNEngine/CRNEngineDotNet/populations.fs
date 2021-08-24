// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
///'s for species, 'v for value
[<JavaScript>] 
type Populations<'s,'v> when 's:equality =
  { index_to_species:Population<'s,'v> array; species_to_index:Hashtable.t<'s,int> }
  static member empty () : Populations<'s,'v> = 
    { index_to_species = [||] ; species_to_index = Hashtable.empty () } 
  ///RLP: This function seems to expect that [infos] contain no duplicates
  static member create (infos:Population<'s,'v> list) =
    let index = List.toArray infos
    let map = Hashtable.empty ()
    List.iteri (fun i info -> (Array.set index i info; Hashtable.add map info.species i)) infos;
    { index_to_species = index; species_to_index = map }
  member pops.contains_species (s:'s) = 
    match Hashtable.tryFind pops.species_to_index s with 
    | None -> false 
    | Some _ -> true
  member pops.species_present (f:'v -> float) (s:'s) =
    match Hashtable.tryFind pops.species_to_index s with
    | None -> false
    | Some i -> f (Array.get pops.index_to_species i).value <> 0.0
  member pops.add_pop_info (info:Population<'s,'v>) =
    match Hashtable.tryFind pops.species_to_index info.species with
    | None ->
        let id = Array.length pops.index_to_species in
        let new_index = Array.append pops.index_to_species ([|info|]) in
        let new_map = Hashtable.copy pops.species_to_index in
        Hashtable.add new_map info.species id;
        { index_to_species = new_index; species_to_index = new_map }
    | Some _ -> failwith "Adding pop_info for species twice"
  member pops.get_pop_info_list = Array.toList pops.index_to_species
  member pops.get_pop_array = Array.map (fun (p:Population<'s,'v>) -> p.value) pops.index_to_species
  member pops.get_initial_species_list = 
    List.map (fun (p:Population<'s,'v>) -> p.species) (List.filter (fun (p:Population<'s,'v>) -> p.initial) pops.get_pop_info_list)
  member pops.get_input_species_list = 
    List.map (fun (p:Population<'s,'v>) -> p.species) (List.filter (fun (p:Population<'s,'v>) -> p.input) pops.get_pop_info_list)
  member pops.get_calculus_species_list = List.map (fun (si:Population<'s,'v>) -> si.species) pops.get_pop_info_list
  member pops.get_pop_info id = Array.get pops.index_to_species id
  member pops.find_index species = Hashtable.find pops.species_to_index species
  member pops.tryFind_index species = Hashtable.tryFind pops.species_to_index species
  member pops.get_calculus_species id = pops.index_to_species.[id].species
  member pops.get_population id = pops.index_to_species.[id].value
  member pops.set_population id v = pops.index_to_species.[id] <- { pops.index_to_species.[id] with value = v }
  member pops.is_constant id = pops.index_to_species.[id].constant
  member pops.set_constant id b = pops.index_to_species.[id] <- { pops.index_to_species.[id] with constant = b }
  member pops.get_count = Array.length pops.index_to_species
  member pops.set_max id m = pops.index_to_species.[id] <- { pops.index_to_species.[id] with max = m }
  member pops.map_population f = 
    { index_to_species = Array.map (fun (p:Population<'s,'v>) -> 
        Population.create(p.constant,(f p.value),p.species,p.initial,p.input,p.max)) pops.index_to_species
    ; species_to_index = Hashtable.copy pops.species_to_index }
  static member test_integer_or_constant (pops:Populations<'s,float>) =
     pops.index_to_species
     |> Array.fold (fun s p -> 
                     let pval = p.value in
                     ((round pval) - pval = 0.0 || p.constant) && s
                     ) true 
  static member round (pops:Populations<'s,float>)= pops.map_population round 
  member pops.map_species f = 
    let newmap = Hashtable.empty () in
    Hashtable.iter (fun sp idx -> Hashtable.add newmap (f sp) idx) pops.species_to_index;
    { index_to_species = Array.map (fun (pi:Population<'s,'v>) -> { species = f pi.species
                                                                  ; value = pi.value
                                                                  ; constant = pi.constant
                                                                  ; initial = pi.initial
                                                                  ; input = pi.input
                                                                  ; spatial_initial = pi.spatial_initial
                                                                  ; max = pi.max }) pops.index_to_species
    ; species_to_index = newmap }
  member pops.eval (f:'v -> float) = pops.map_population f
  static member scale (vpops:Populations<'s,float>) scale = vpops.map_population (fun p -> scale * p) 
  member pops.tryFind_species (s:'s) =
    match Hashtable.tryFind pops.species_to_index s with
      | None -> None
      | Some id -> Some (Array.get pops.index_to_species id)
  member pops.clone =
    { index_to_species = Array.copy pops.index_to_species
    ; species_to_index = Hashtable.copy pops.species_to_index }
  member pops.to_string_ext string_of_pop_info sep =
    Lib.string_of_list string_of_pop_info sep (Array.toList pops.index_to_species)
  member pops.to_string display_species display_value =
    pops.to_string_ext (fun p -> p.to_string display_species display_value) (" |" + Lib.newline)
  static member to_string_float display_species (pops:Populations<'s,float>) = pops.to_string display_species Lib.display_float
  member pops.to_string_nonzero display_species display_value =
    let specs = pops.index_to_species |> Array.filter (fun pi -> display_value pi.value <> "0.0") |> List.ofArray in
    Lib.string_of_list (fun (p:Population<'s,'v>) -> p.to_string display_species display_value) (" |" + Lib.newline) specs
  member pops.to_key_plottables p = Expression.map (pops.find_index) p
  member pops.to_species_plottables p = Expression.map (pops.get_calculus_species) p
  ///Merges two populations, disregarding constancy
  ///The result has as species the union of species in [pop1] and [pop2]
  ///populations are added with [plus]
  static member merge plus pop1 (pop2:Populations<'s,'v>) =
    let combine acc (pi2:Population<'s,'v>) =
      match Hashtable.tryFind acc.species_to_index pi2.species with
      | None -> acc.add_pop_info pi2
      | Some i -> 
        let pi1 = Array.get acc.index_to_species i in
        (* RLP: here we should also consider merging constancy and even initial/input *)
        let pi = {pi1 with value = plus pi1.value pi2.value} in
        Array.set acc.index_to_species i pi;
        acc in
    Array.fold combine pop1 pop2.index_to_species

    (* Check if this can be removed 
  let extend smallpops (largepops:t<'s,float>) =
    if smallpops.index_to_species.Length >= largepops.index_to_species.Length then smallpops else
    let extras = Array.sub largepops.index_to_species smallpops.index_to_species.Length (largepops.index_to_species.Length-smallpops.index_to_species.Length) in
    Array.fold (fun pops sp -> add_pop_info (empty_pop_info_float sp) pops) smallpops (Array.map (fun pi -> pi.calculus_species) extras)
  *)
  