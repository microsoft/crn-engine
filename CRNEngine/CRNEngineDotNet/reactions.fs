// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
[<JavaScript>]
type Reactions<'s ,'v,'e> when 's:equality and 'v:equality and 'e:equality =
  {list:Reaction<'s,'v,'e> list} //Dummy type
  static member create(l:('s list * 's list * (Rate<'v,'e> option * Rate<'v,'e>) * 's list) list) = 
    List.map (fun (c,r,(rev,f),p) -> Reaction.create(c,r,(rev,f),p)) l
  static member create(l:('s list * (Rate<'v,'e> option * Rate<'v,'e>) * 's list) list) = 
    List.map (fun (r,(rev,f),p) -> Reaction.create(r,(rev,f),p)) l
