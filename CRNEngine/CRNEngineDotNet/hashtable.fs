[<JavaScript>]
module Microsoft.Research.CRNEngine.Hashtable

open System.Collections.Generic

type t<'a,'b> = Dictionary<'a,'b>
#if JavaScript
//How does JavaScript behave in this case, structural semantics?
let empty () = new Dictionary<_,_>()
#else
let empty () = new Dictionary<_,_>(HashIdentity.Structural)
#endif
let find (ht:t<'a,'b>) (key:'a) = 
  ht.[key]
let add (ht:t<'a,'b>) (key:'a) (value:'b) = ht.[key] <- value
let remove (ht:t<'a,'b>) (key:'a) = ht.Remove key
let fold (f:'a -> 'b -> 'c -> 'c) (ht:t<'a,'b>) (init:'c) =
  let mutable res = init in
  for kvp in (ht :> seq<_>) do
    res <- f kvp.Key kvp.Value res
  done;
  res
let iter (f:'a -> 'b -> unit) (ht:t<'a,'b>) =
  for kvp in (ht :> seq<_>) do
    f kvp.Key kvp.Value
  done
let tryFind (ht:t<'a,'b>) (key:'a) =
  if ht.ContainsKey(key) then
    Some(ht.[key])
  else
    None
    //byref values are not permitted in current F# quotations, implement a custom Dictionary? Try WebSharper 4 when released. Colin
  (*match ht.TryGetValue(key) with
    | (false, _) -> None
    | (true, value) -> Some(value)*)
let contains_key (ht:t<'a,'b>) (key:'a) =
  ht.ContainsKey(key)
let map f ht =
  let m = empty () in
  iter (fun k v -> add m k (f v)) ht;
  m
let map_key f ht =
  let m = empty () in
  iter (fun k v -> add m (f k) v) ht;
  m

let to_list ht = fold (fun k v l -> (k, v)::l) ht []

let copy t =
  let c = empty () in
  iter (fun k v -> add c k v) t;
  c

let count (ht:t<'a,'b>) = ht.Count

let toSeq (ht:t<'a,'b>) = ht :> seq<KeyValuePair<'a,'b>>

let ofMap (m:Map<'a, 'b>) = 
  let newTable = empty ()
  Map.iter (add newTable) m
  newTable