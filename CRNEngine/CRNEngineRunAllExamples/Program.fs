// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.RunAllExamples.Program
open Microsoft.Research.CRNEngine
open Microsoft.Research.CliLibrary.Testing

[<EntryPoint>]
let main(args) =
    System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    System.Threading.Thread.CurrentThread.CurrentUICulture <- System.Globalization.CultureInfo.InvariantCulture
    testValidate (fun _ -> None) InferenceSiteGraph.from_string args