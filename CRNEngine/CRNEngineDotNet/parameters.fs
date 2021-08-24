// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open System.Diagnostics
[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Parameters = 
  {list:Parameter list} //Dummy type
  static member create (l:((string*float)*Prior option) list) = 
    List.map (fun ((n,v),prior) -> Parameter.create((n,v),prior)) l
  static member get name ps = match ps |> List.tryFind (fun p -> p.name = name) with Some p -> p | None -> failwithf "Unknown parameter %s" name
  static member add p (ps:Parameter list) = 
    match List.tryFind (fun psi -> psi.name = p.name) ps with
    | Some psi -> failwithf "Cannot add another parameter with name %s" psi.name
    | None     -> List.append ps [p]
  // Would be better if we had some kind of order-preserving map
  static member overwrite (ps:Parameter list) pnew = 
    if List.exists (fun p -> p.name = pnew.name) ps 
    then List.map (fun p -> if p.name = pnew.name then pnew else p) ps
    else List.append ps [pnew]
  static member remove (names:string list) (ps:Parameter list) = 
    let f (ps:Parameter list) (name:string) = List.filter (fun (p:Parameter) -> p.name <> name) ps in
      List.fold f ps names
  static member to_env (ps:Parameter list) : Environment.t = ps |> List.map (fun v -> v.name, v.value) |> Map.ofList
  static member fix (ps:Parameter list) = ps |> List.map (fun (p:Parameter) -> match p.prior with Some pr -> { p with prior = Some (Prior.fix pr) } | None -> p)
  static member substitute (e:Environment.t) (ps:Parameter list) = ps |> List.map (fun p -> Parameter.substitute e p)
  static member get_names (ps:Parameter list) = Lib.remove_duplicates (=) (List.map Parameter.getname ps)
  static member create_multiples (fresh:string list list list) (multiples:Parameter list) = 
    let f (fresh_names:string list) = 
      let f2 (p:Parameter) (name:string) = Parameter.create(name,p.value,Option.map Prior.vary p.prior)
      List.map2 f2 multiples fresh_names 
    List.map (List.map f) fresh
  static member merge (existing:Parameter list) (overwriters:Parameter list) =
    List.fold Parameters.overwrite existing overwriters
  ///Flattens 'sweeps' to a single sweep 
  ///For each assignment in the flattened sweep, 
  ///creates a fresh name for each parameter in 'ps' labelled as Multiple
  ///Assumes that each parameter is only defined once in ps
  static member expand_multiples (sweeps:Sweep list list) (ps:Parameter list) = 
    let multiples,singles = ps |> List.partition Parameter.is_multiple
    let multiple_names = List.map Parameter.getname multiples
    let counter = ref 0
    let fresh_names, sweeps = List.map (fun (s:Sweep list) -> Sweeps.add_multiples counter multiple_names s) (*(get_names ps)*) sweeps |> List.unzip
    let tmp_multiples = Parameters.create_multiples (List.concat fresh_names) multiples 
                        |> List.concat 
    // Create a map from a parameter to a list of its expanded parameters
    let oldToNewNamesMap = 
      tmp_multiples
      |> List.fold (fun (acc:Map<string, string list>) xxs ->
          xxs 
          |> List.zip multiples
          |> List.fold (fun (acc:Map<string, string list>) (p, newP) ->
            if acc.ContainsKey(p.name)
              then acc.Add(p.name, acc.[p.name] @ [newP.name])
              else acc.Add(p.name, [newP.name])
          ) acc) Map.empty
      // add non-multiple parameters as well
      |> fun m -> List.fold (fun (acc:Map<string, string list>) p -> acc.Add(p.name, [p.name])) m singles 
    let fresh_parameters = tmp_multiples |> List.concat 
    sweeps,List.concat [singles; List.map (fun (p:Parameter) -> Parameter.create(p.name,p.value,None) ) multiples; fresh_parameters], oldToNewNamesMap
  static member sample_from_prior rng (ps:Parameter list) =    
    ps |> List.map (fun p -> match p.prior with None -> p | Some pr -> { p with value = pr.distribution.sample rng })
  static member to_string (parameters:Parameter list) = 
    sprintf "[\n  %s;\n]" (parameters |> List.map Parameter.to_string |> String.concat ";\n  ")
  static member to_string_inline (parameters:Parameter list) = 
    parameters |> List.map (fun p -> sprintf "%s = %s" p.name (p.value.ToString())) |> String.concat "; "
  
  (*
  {map:Map<string,Parameter>} //Dummy type
  member ps.to_list() = ps.map |> Map.toList |> List.map snd
  static member ofList ps = 
    let names = ps |> List.map (fun p -> p.name)
    if (names = List.distinct names)
    then { map = ps |> List.map (fun p -> p.name, p) |> Map.ofList }
    else failwith "Attemped to work with a list of parameters that don't have unique names"
  static member empty = { map = Map.empty }
  static member create (l:((string*float)*Prior option) list) = 
    List.map (fun ((n,v),prior) -> Parameter.create((n,v),prior)) l |> Parameters.ofList
  static member add p (ps:Parameters) = { map = Map.add p.name p ps.map }
  static member remove (names:string list) (ps:Parameter list) = 
    let f (ps:Parameter list) (name:string) = List.filter (fun (p:Parameter) -> p.name <> name) ps in
      List.fold f ps names
  static member to_env (ps:Parameters) = ps.map |> Map.map (fun _ v -> v.value)
  static member fix (ps:Parameters) = { map = Map.map (fun k (p:Parameter) -> Parameter.create(p.name,p.value,Option.map Prior.fix p.prior)) ps.map }
  static member substitute (e:Environment.t) (ps:Parameters) = { map = Map.map (fun k p -> Parameter.substitute e p) ps.map }
  static member get_names (ps:Parameter list) = Lib.remove_duplicates (=) (List.map Parameter.getname ps)
  static member create_multiples (fresh:string list list list) (multiples:Parameter list) = 
    let f (fresh_names:string list) = 
      let f2 (p:Parameter) (name:string) = Parameter.create(name,p.value,Option.map Prior.vary p.prior)
      List.map2 f2 multiples fresh_names 
    List.map (List.map f) fresh
  static member merge (existing:Parameters) (overwriters:Parameter list) =
    { map = List.fold (fun ps pnew -> Map.add pnew.name pnew ps) existing.map overwriters }
  ///Flattens 'sweeps' to a single sweep 
  ///For each assignment in the flattened sweep, 
  ///creates a fresh name for each parameter in 'ps' labelled as Multiple
  ///Assumes that each parameter is only defined once in ps
  static member expand_multiples (sweeps:Sweep list list) (ps:Parameters) = 
    let multiples,singles = ps.to_list() |> List.partition Parameter.is_multiple
    let multiple_names = List.map Parameter.getname multiples
    let counter = ref 0
    let fresh_names, sweeps = List.map (fun (s:Sweep list) -> Sweeps.add_multiples counter multiple_names s) (*(get_names ps)*) sweeps |> List.unzip
    let fresh_parameters = 
      Parameters.create_multiples (List.concat fresh_names) multiples |> List.concat |> List.concat 
    sweeps,List.concat [singles; List.map (fun (p:Parameter) -> Parameter.create(p.name,p.value,None) ) multiples; fresh_parameters]
  static member sample_from_prior rng (ps:Parameters) =    
    { map = ps.map |> Map.map (fun k p -> match p.prior with None -> p | Some pr -> { p with value = pr.distribution.sample rng }) }
  static member to_string (parameters:Parameters) = 
    sprintf "[\n  %s;\n]" (parameters.to_list() |> List.map Parameter.to_string |> String.concat ";\n  ")
  static member to_string_inline (parameters:Parameters) = 
    parameters.map |> Map.toList |> List.map (fun (k,p) -> sprintf "%s = %s" k (p.value.ToString())) |> String.concat "; "
  *)
