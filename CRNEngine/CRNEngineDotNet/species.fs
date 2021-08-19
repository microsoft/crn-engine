namespace Microsoft.Research.CRNEngine
[<JavaScript>]
/// Do not add more cases or fields to this type. This is supposed to remain a primitive type.
type Species = 
  { name: string }
  static member create (s:string) = {name = s}
  static member to_string (s:Species) = s.name
  static member parse = Parser.(|>>) (Parser.name_kw Keywords.kwList) Species.create
  static member from_string = Parser.from_string Species.parse 