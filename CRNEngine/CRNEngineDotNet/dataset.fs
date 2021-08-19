namespace Microsoft.Research.CRNEngine
open System.Diagnostics
[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Dataset = 
  { file:string; data:Table<float> list }
  static member empty name = { file = name; data = [] }
  static member create name data = { file = name; data = data }