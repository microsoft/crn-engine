// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Stringmap
open Microsoft.Research.CRNEngine

type 'a t = Map<string,'a>

(* The empty map. *)
let empty : 'a t = Map.empty

(* Try to find a mapping (returns an option type). *)
let tryFind (k:string) (m:'a t) : 'a option = Map.tryFind k m 

(* Try to find a mapping (throws an exception). *)
let find key table = Map.find key table

(* Remove a mapping. *)
let remove key table = Map.remove key table

(* Add a mapping. *)
let add key value table = Map.add key value table

(* Map across a map. *)
let map key m = Map.map key m

(* Construct a mapping from a list.
 * We fold from the right so that earlier bindings take precedence! *)
let of_list (kas:(string * 'a) list) : 'a t = Lib.fold_right (fun (k,a) m -> add k a m) kas empty

(* Get all of the keys from a string map. *)
let getKeys (m:'a t) = Map.fold (fun acc key _ -> acc@[key]) [] m

(* Is a given key in the map? *)
let inDomain (m:'a t) (k:string) = match tryFind k m with None -> false | Some _ -> true

(* Fold across a string map. *)
let fold (f:'b -> string -> 'a -> 'b) (init:'b) (m:'a t) = Map.fold f init m

let iter f m = Map.iter f m

(* Apply a filter to a map. *)
let filter = Map.filter
