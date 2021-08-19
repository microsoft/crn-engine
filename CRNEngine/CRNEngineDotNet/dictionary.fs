[<JavaScript>]
module Microsoft.Research.CRNEngine.Dictionary

#if JavaScript
open Microsoft.FSharp.Collections
type t<'a,'b> when 'a:comparison = { mutable map : Map<'a,'b> }
let empty () = { map = Map.empty }
let containsKey t k = Map.containsKey k t.map
let toSeq t = t.map |> Map.toSeq
let values t = t.map |> Map.toSeq |> Seq.map (fun (k,v) -> v)
let keys t = t.map |> Map.toSeq |> Seq.map (fun (k,v) -> k)
let add t key value = t.map <- Map.add key value t.map
let find t key = Map.find key t.map
let count t = t.map.Count
#else
open System.Collections.Generic
type t<'a,'b> when 'a:comparison = Dictionary<'a,'b>
let empty () = new Dictionary<_,_>(HashIdentity.Structural)
let containsKey (t:t<'a,'b>) k = t.ContainsKey k
let toSeq (t:t<'a,'b>) = Seq.map (fun (kv:KeyValuePair<'a,'b>) -> (kv.Key,kv.Value)) t
let values (t:t<'a,'b>) = t.Values
let keys (t:t<'a,'b>) = t.Keys
let add (t:t<'a,'b>) key value = t.[key] <- value
let find (t:t<'a,'b>) key = t.[key]
let count (t:t<'a,'b>) = t.Count
#endif