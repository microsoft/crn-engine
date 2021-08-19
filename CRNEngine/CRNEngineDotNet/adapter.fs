namespace Microsoft.Research.CRNEngine
[<JavaScript>] 
type Adapter<'s> when 's : comparison  =
 { init     : 's list -> int list
 ; react_st : int list -> int -> Reaction<int,Value,Expression.t<Key<int>>>  list
 ; index    : Index<'s> }
