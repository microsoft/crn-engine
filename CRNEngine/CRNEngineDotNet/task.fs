// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

[<JavaScript>]
type TaskType =
| [<WebSharper.ConstantAttribute "Parse">] Parse
| [<WebSharper.ConstantAttribute "Simulate">] Simulate
| [<WebSharper.ConstantAttribute "Infer">] Infer

[<JavaScript>]
type Task =
 {
   task_type: TaskType option
   /// The number of execution runs requested.
   copies: int
   /// The ID for the first execution run, used to identify output.
   copy_id: int
   /// The number of nodes in which this task may be split.
   nodes: int }
   member x.to_string() =
    let tokens = seq {
        match x.task_type with None -> () | Some t -> yield sprintf "task=%s" (t.ToString().ToLower())
        if x.copies <> 1 then yield sprintf "copies=%d" x.copies
        if x.copy_id <> 1 then yield sprintf "copy_id=%d" x.copy_id
        if x.nodes <> 1 then yield sprintf "nodes=%d" x.nodes
    }
    let ret = sprintf "directive task {%s}" (System.String.Join(";",tokens))
    ret