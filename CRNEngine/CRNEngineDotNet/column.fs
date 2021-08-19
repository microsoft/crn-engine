namespace Microsoft.Research.CRNEngine
[<JavaScript>]
type Column<'v> = 
  { name: string; values: 'v list }
  static member create (values:'v list) (name:string) = { name = name; values = values}
  static member reverse (c:Column<'v>) = {c with values = List.rev c.values}
