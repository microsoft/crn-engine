module Microsoft.Research.CliCRN

open Microsoft.Research.CRNEngine
open Microsoft.Research.CliLibrary

[<EntryPoint>]
let main args =
    // Enforce invariant culture (prevents issues with decimal separators on international systems).
    System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    let ret = Program.main InferenceSiteGraph.from_string args
    ret