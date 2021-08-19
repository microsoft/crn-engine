namespace Microsoft.Research.CRNEngine
[<JavaScript>] 
type Inlined<'s> = 
  | Species of 's
  | Time
  static member to_string (f:'s -> string) (k:Inlined<'s>) =
    match k with
    | Species(s) -> "[" + f s + "]"
    | Time       -> "[time]"
  static member map (f:'s -> 's2) (k:Inlined<'s>) =
    match k with
    | Species(s) -> Species(f s)
    | Time       -> Time