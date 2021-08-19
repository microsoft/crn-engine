module Microsoft.Research.CRNEngineCloudLib.DataStructures

type JobState =
| [<WebSharper.Constant("Waiting")>] Waiting
| [<WebSharper.Constant("Active")>] Active
| [<WebSharper.Constant("Completed")>] Completed
| [<WebSharper.Constant("NoJob")>] NoJob

type JobFile = {
    name: string
    uri: string
}

type JobDescriptor = {
    id: string
    state: JobState
    verb: string
    start: System.DateTime
    zipFile: JobFile option
    files: JobFile[]
}
