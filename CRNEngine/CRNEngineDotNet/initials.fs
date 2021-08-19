namespace Microsoft.Research.CRNEngine
[<JavaScript>]
type Initials<'s ,'v> when 's:equality and 'v:equality =
  {list:Initial<'s,'v> list} //Dummy type
  static member create (l:(bool * 'v * 's * 'v option * Spatial_initial.t option) list) = 
    List.map (fun (c,v,s,t,sp) -> Initial.create(c,v,s,t,sp)) l
  static member create (l:(bool * 'v * 's * 'v option) list) = 
    List.map (fun (c,v,s,t) -> Initial.create(c,v,s,t,None)) l
  static member create (l:('v * 's * 'v option * Spatial_initial.t option) list) = 
    List.map (fun (v,s,t,sp) -> Initial.create(false,v,s,t,sp)) l
  static member create (l:('v * 's * 'v option) list) = 
    List.map (fun (v,s,t) -> Initial.create(false,v,s,t,None)) l
  static member create (l:('v * 's) list) = 
    List.map (fun (v,s) -> Initial.create(false,v,s,None,None)) l
